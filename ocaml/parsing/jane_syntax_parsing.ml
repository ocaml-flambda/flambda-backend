(** As mentioned in the .mli file, there are some gory details around the
    particular translation scheme we adopt for moving to and from OCaml ASTs
    ([Parsetree.expression], etc.).  The general idea is that we adopt a scheme
    where each novel piece of syntax is represented using one of two embeddings:

    1. As an AST item carrying an attribute. The AST item serves as the "body"
      of the syntax indicated by the attribute.
    2. As a pair of an extension node and an AST item that serves as the "body".
       Here, the "pair" is embedded as a pair-like construct in the relevant AST
       category, e.g. [include sig [%jane.ERASABILITY.EXTNAME];; BODY end] for
       signature items.

    In particular, for an language extension named [EXTNAME] (i.e., one that is
    enabled by [-extension EXTNAME] on the command line), the attribute (if
    used) must be [[@jane.ERASABILITY.EXTNAME]], and the extension node (if
    used) must be [[%jane.ERASABILITY.EXTNAME]]. For built-in syntax, we use
    [_builtin] instead of an language extension name.

    The [ERASABILITY] component indicates to tools such as ocamlformat and
    ppxlib whether or not the attribute is erasable. See the documentation of
    [Erasability] for more information on how tools make use of this
    information.

    In the below example, we use attributes an examples, but it applies equally
    to extensions. We also provide utilities for further desugaring similar
    applications where the embeddings have the longer form
    [[@jane.ERASABILITY.FEATNAME.ID1.ID2.….IDn]] (with the outermost one being
    the [n = 0] case), as these might be used inside the [EXPR]. (For example,
    within the outermost [[@jane.non_erasable.comprehensions]] term for list and
    array comprehensions, we can also use
    [[@jane.non_erasable.comprehensions.list]],
    [[@jane.non_erasable.comprehensions.array]],
    [[@jane.non_erasable.comprehensions.for.in]], etc.).

    As mentioned, we represent terms as a "pair" and don't use the extension
    node or attribute payload; this is so that ppxen can see inside these
    extension nodes or attributes. If we put the subexpressions inside the
    payload, then we couldn't write something like [[[%string "Hello, %{x}!"]
    for x in names]], as [ppx_string] wouldn't traverse inside the payload to
    find the [[%string]] extension node.

    Our novel syntactic features are of course allowed to impose extra
    constraints on what legal bodies are; we're also happy for this translation
    to error in various ways on malformed input, since nobody should ever be
    writing these forms directly.  They're just an implementation detail.

    See modules of type AST below to see how different syntactic categories
    are represented. For example, expressions are encoded using an attribute.

    We provide one module per syntactic category (e.g., [Expression]), of module
    type [AST].  They also provide some simple machinery for working with the
    general [@jane.ERASABILITY.FEATNAME.ID1.ID2.….IDn] wrapped forms. To
    construct one, we provide [make_jane_syntax]; to destructure one, we provide
    [match_jane_syntax] (which we expose via [make_of_ast]). Users of this
    module still have to write the transformations in both directions for all
    new syntax, lowering it to extension nodes or attributes and then lifting it
    back out. *)

(** How did we choose between using the attribute embedding and the extension
    node embedding for a particular syntactic category?

    Generally, we prefer the attribute embedding: it's more compatible with
    ppxes that aren't aware of Jane Syntax. (E.g., if a type looks like a tuple,
    it truly is a tuple and not an extension node embedding.)

    We can't apply the attribute embedding everywhere because some syntactic
    categories, like structure items, don't carry attributes. For these, we
    use extension nodes.

    However, the attribute embedding is more inconvenient in some ways than
    the extension node embedding. For example, the attribute embedding requires
    callers to strip out Jane Syntax-related attributes from the attribute list
    before processing it. We've tried to make this obvious from the signature
    of, say, [Jane_syntax.Expression.of_ast], but this is somewhat more
    inconvenient than just operating on the [expr_desc]. Nonetheless, because
    of the advantages with ppxlib interoperability, we've opted for the
    attribute embedding where possible.
*)

open Parsetree

(** We carefully regulate which bindings we import from [Language_extension]
    to ensure that we can import this file into the Jane Street internal
    repo with no changes.
*)
module Language_extension = struct
  include Language_extension_kernel

  include (
    Language_extension :
      Language_extension_kernel.Language_extension_for_jane_syntax)
end

(******************************************************************************)

module Feature : sig
  type t =
    | Language_extension : _ Language_extension.t -> t
    | Builtin

  type error =
    | Disabled_extension : _ Language_extension.t -> error
    | Unknown_extension of string

  val describe_uppercase : t -> string

  val extension_component : t -> string

  val of_component : string -> (t, error) result

  val is_erasable : t -> bool
end = struct
  type t =
    | Language_extension : _ Language_extension.t -> t
    | Builtin

  type error =
    | Disabled_extension : _ Language_extension.t -> error
    | Unknown_extension of string

  let builtin_component = "_builtin"

  let describe_uppercase = function
    | Language_extension ext ->
      "The extension \"" ^ Language_extension.to_string ext ^ "\""
    | Builtin -> "Built-in syntax"

  let extension_component = function
    | Language_extension ext -> Language_extension.to_string ext
    | Builtin -> builtin_component

  let of_component str =
    if String.equal str builtin_component
    then Ok Builtin
    else
      match Language_extension.of_string str with
      | Some (Pack ext) ->
        if Language_extension.is_enabled ext
        then Ok (Language_extension ext)
        else Error (Disabled_extension ext)
      | None -> Error (Unknown_extension str)

  let is_erasable = function
    | Language_extension ext -> Language_extension.is_erasable ext
    (* Builtin syntax changes don't involve additions or changes to concrete
       syntax and are always erasable.
    *)
    | Builtin -> true
end

(** Was this embedded as an [[%extension_node]] or an [[@attribute]]?  Not
    exported. Used only for error messages. *)
module Embedding_syntax = struct
  type t =
    | Extension_node
    | Attribute

  let name = function
    | Extension_node -> "extension node"
    | Attribute -> "attribute"

  let name_indefinite = function
    | Extension_node -> "an extension node"
    | Attribute -> "an attribute"

  let name_plural = function
    | Extension_node -> "extension nodes"
    | Attribute -> "attributes"

  let pp ppf (t, name) =
    let sigil = match t with Extension_node -> "%" | Attribute -> "@" in
    Format.fprintf ppf "[%s%s]" sigil name
end

(******************************************************************************)

module Misnamed_embedding_error = struct
  type t =
    | No_erasability
    | No_feature
    | Unknown_erasability of string

  let to_string = function
    | No_erasability -> "Missing erasability and feature components"
    | No_feature -> "Missing a feature component"
    | Unknown_erasability str ->
      Printf.sprintf
        "Unrecognized component where erasability was expected: `%s'" str
end

(** The component of an attribute or extension name that identifies whether or
    not the embedded syntax is *erasable*; that is, whether or not the
    upstream OCaml compiler can safely interpret the AST while ignoring the
    attribute or extension.  (This means that syntax encoded as extension
    nodes should always be non-erasable.)  Tools that consume the parse tree
    we generate can make use of this information; for instance, ocamlformat
    will use it to guide how we present code that can be run with both our
    compiler and the upstream compiler, and ppxlib can use it to decide
    whether it's ok to allow ppxes to construct syntax that uses this
    emedding.  In particular, the upstream version of ppxlib will allow ppxes
    to produce [[@jane.erasable.*]] attributes, but will report an error if a
    ppx produces a [[@jane.non_erasable.*]] attribute.

    As mentioned above, unlike for attributes, the erasable/non-erasable
    distinction is not meaningful for extension nodes, as the compiler will
    always error if it sees an uninterpreted extension node. So, for purposes
    of tools in the wider OCaml ecosystem, it is irrelevant whether embeddings
    that use extension nodes indicate [Erasable] or [Non_erasable] for this
    component, but the semantically correct choice and the one we've settled
    on is to use [Non_erasable]. *)
module Erasability = struct
  type t =
    | Erasable
    | Non_erasable

  let to_string = function
    | Erasable -> "erasable"
    | Non_erasable -> "non_erasable"

  let of_string = function
    | "erasable" -> Ok Erasable
    | "non_erasable" -> Ok Non_erasable
    | _ -> Error ()
end

(** An AST-style representation of the names used when generating extension
    nodes or attributes for modular syntax; see the .mli file for more
    details. *)
module Embedded_name : sig
  (** A nonempty list of name components, without the first two components.
      (That is, without the leading root component that identifies it as part of
      the modular syntax mechanism, and without the next component that
      identifies the erasability.) See the .mli file for more details. *)
  type components = ( :: ) of string * string list

  type t =
    { erasability : Erasability.t;
      components : components
    }

  (** See the mli. *)
  val of_feature : Feature.t -> string list -> t

  val components : t -> components

  (** Convert one of these Jane syntax names to the embedded string form used in
      the OCaml AST as the name of an extension node or an attribute; exposed
      for extensions that only uses [Embedded_name] instead of the whole
      infrastructure in this module, such as the dummy argument extension *)
  val to_string : t -> string

  (** Parse a Jane syntax name from the OCaml AST, either as the name of an
      extension node or an attribute:
        - [Some (Ok _)] if it's a legal Jane-syntax name;
        - [Some (Error _)] if the root is present, but the name has fewer than 3
          components or the erasability component is malformed; and
        - [None] if it doesn't start with the leading root name and isn't part
          of our Jane-syntax machinery.
      Not exposed. *)
  val of_string : string -> (t, Misnamed_embedding_error.t) result option

  (** Print out the embedded form of a Jane-syntax name, in quotes; for use in
      error messages. *)
  val pp_quoted_name : Format.formatter -> t -> unit

  (** Print out an empty extension node or attribute with a Jane-syntax name,
      accompanied by an indefinite article; for use in error messages.  Not
      exposed. *)
  val pp_a_term : Format.formatter -> Embedding_syntax.t * t -> unit
end = struct
  (** The three parameters that control how we encode Jane-syntax extension node
      names.  When updating these, update comments that refer to them by their
      contents! *)
  module Config = struct
    (** The separator between name components *)
    let separator = '.'

    (** The leading namespace that identifies this extension node or attribute
        as reserved for a piece of modular syntax *)
    let root = "jane"

    (** For printing purposes, the appropriate indefinite article for [root] *)
    let article = "a"
  end

  include Config

  let separator_str = String.make 1 separator

  type components = ( :: ) of string * string list

  type t =
    { erasability : Erasability.t;
      components : components
    }

  let of_feature feature trailing_components =
    let feature_component = Feature.extension_component feature in
    let erasability : Erasability.t =
      if Feature.is_erasable feature then Erasable else Non_erasable
    in
    { erasability; components = feature_component :: trailing_components }

  let components t = t.components

  let to_string { erasability; components = feat :: subparts } =
    String.concat separator_str
      (root :: Erasability.to_string erasability :: feat :: subparts)

  let of_string str : (t, Misnamed_embedding_error.t) result option =
    match String.split_on_char separator str with
    | root' :: parts when String.equal root root' -> (
      match parts with
      | [] -> Some (Error No_erasability)
      | [_] -> Some (Error No_feature)
      | erasability :: feat :: subparts -> (
        match Erasability.of_string erasability with
        | Ok erasability ->
          Some (Ok { erasability; components = feat :: subparts })
        | Error () -> Some (Error (Unknown_erasability erasability))))
    | _ :: _ | [] -> None

  let pp_quoted_name ppf t = Format.fprintf ppf "\"%s\"" (to_string t)

  let pp_a_term ppf (esyn, t) =
    Format.fprintf ppf "%s %a" article Embedding_syntax.pp (esyn, to_string t)
end

(******************************************************************************)
module Error = struct
  (** An error triggered when desugaring a language extension from an OCaml
      AST; should always be fatal *)
  type error =
    | Introduction_has_payload of Embedding_syntax.t * Embedded_name.t * payload
    | Unknown_extension of Embedding_syntax.t * Erasability.t * string
    | Disabled_extension :
        { ext : _ Language_extension.t;
          maturity : Language_extension.maturity option
        }
        -> error
    | Wrong_syntactic_category of Feature.t * string
    | Misnamed_embedding of
        Misnamed_embedding_error.t * string * Embedding_syntax.t
    | Bad_introduction of Embedding_syntax.t * Embedded_name.t

  (** The exception type thrown when desugaring a piece of modular syntax from
      an OCaml AST *)
  exception Error of Location.t * error
end

open Error

let assert_extension_enabled (type a) ~loc (ext : a Language_extension.t)
    (setting : a) =
  if not (Language_extension.is_at_least ext setting)
  then
    let maturity : Language_extension.maturity option =
      match ext with
      | Layouts -> Some (setting : Language_extension.maturity)
      | _ -> None
    in
    raise (Error (loc, Disabled_extension { ext; maturity }))

let report_error ~loc = function
  | Introduction_has_payload (what, name, _payload) ->
    Location.errorf ~loc
      "@[Modular syntax %s are not allowed to have a payload,@ but %a does@]"
      (Embedding_syntax.name_plural what)
      Embedded_name.pp_quoted_name name
  | Unknown_extension (what, erasability, name) ->
    let embedded_name = { Embedded_name.erasability; components = [name] } in
    Location.errorf ~loc "@[Unknown extension \"%s\" referenced via@ %a %s@]"
      name Embedded_name.pp_a_term (what, embedded_name)
      (Embedding_syntax.name what)
  | Disabled_extension { ext; maturity } -> (
    (* CR layouts: The [maturity] special case is a bit ad-hoc, but the
       layouts error message would be much worse without it. It also
       would be nice to mention the language construct in the error message.
    *)
    match maturity with
    | None ->
      Location.errorf ~loc "The extension \"%s\" is disabled and cannot be used"
        (Language_extension.to_string ext)
    | Some maturity ->
      Location.errorf ~loc
        "This construct requires the %s version of the extension \"%s\", which \
         is disabled and cannot be used"
        (Language_extension.maturity_to_string maturity)
        (Language_extension.to_string ext))
  | Wrong_syntactic_category (feat, cat) ->
    Location.errorf ~loc "%s cannot appear in %s"
      (Feature.describe_uppercase feat)
      cat
  | Misnamed_embedding (err, name, what) ->
    Location.errorf ~loc "Cannot have %s named %a: %s"
      (Embedding_syntax.name_indefinite what)
      Embedding_syntax.pp (what, name)
      (Misnamed_embedding_error.to_string err)
  | Bad_introduction (what, ({ components = ext :: _; _ } as name)) ->
    Location.errorf ~loc
      "@[The extension \"%s\" was referenced improperly; it started with@ %a \
       %s,@ not %a one@]"
      ext Embedded_name.pp_a_term (what, name)
      (Embedding_syntax.name what)
      Embedded_name.pp_a_term
      (what, { name with components = [ext] })

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)

(******************************************************************************)
(** Generically find and create the OCaml AST syntax used to encode one of our
    novel syntactic features.  One module per variety of AST (expressions,
    patterns, etc.). *)

(** The parameters that define how to look for [[%jane.*.FEATNAME]] and
    [[@jane.*.FEATNAME]] inside ASTs of a certain syntactic category. This
    module type describes the input to the [Make_with_attribute] and
    [Make_with_extension_node] functors (though they stipulate additional
    requirements for their inputs).
*)
module type AST_syntactic_category = sig
  (** The AST type (e.g., [Parsetree.expression]) *)
  type ast

  (** The name for this syntactic category in the plural form; used for error
      messages (e.g., "expressions") *)
  val plural : string

  (** How to get the location attached to an AST node.  Should just be
      [fun tm -> tm.pCAT_loc] for the appropriate syntactic category [CAT]. *)
  val location : ast -> Location.t

  (** Set the location of an AST node. *)
  val with_location : ast -> Location.t -> ast
end

module type AST_internal = sig
  include AST_syntactic_category

  val embedding_syntax : Embedding_syntax.t

  val make_jane_syntax : Embedded_name.t -> ?payload:payload -> ast -> ast

  (** Given an AST node, check if it's a representation of a term from one of
      our novel syntactic features; if it is, split it back up into its name,
      the location of the extension/attribute, any payload, and the body.  If
      the embedded term is malformed in any way, raises an error; if the input
      isn't an embedding of one of our novel syntactic features, returns [None].
      Partial inverse of [make_jane_syntax]. *)
  val match_jane_syntax :
    ast -> (Embedded_name.t * Location.t * Parsetree.payload * ast) option
end

(* Parses the embedded name from an embedding, raising if
    the embedding is malformed. Malformed means that
    NAME is missing; e.g. the attribute is just [[@jane]].
*)
let parse_embedding_exn ~loc ~name ~embedding_syntax =
  let raise_error err = raise (Error (loc, err)) in
  match Embedded_name.of_string name with
  | Some (Ok name) -> Some name
  | Some (Error err) ->
    raise_error (Misnamed_embedding (err, name, embedding_syntax))
  | None -> None

let find_and_remove_jane_syntax_attribute =
  (* Recurs on [rev_prefix] *)
  let rec loop ~rev_prefix ~suffix =
    match rev_prefix with
    | [] -> None
    | attr :: rev_prefix -> (
      let { attr_name = { txt = name; loc = attr_loc }; attr_payload } = attr in
      match
        parse_embedding_exn ~loc:attr_loc ~name ~embedding_syntax:Attribute
      with
      | None -> loop ~rev_prefix ~suffix:(attr :: suffix)
      | Some name ->
        let unconsumed_attributes = List.rev_append rev_prefix suffix in
        Some (name, attr_loc, attr_payload, unconsumed_attributes))
  in
  fun attributes -> loop ~rev_prefix:(List.rev attributes) ~suffix:[]

let make_jane_syntax_attribute name payload =
  { attr_name =
      { txt = Embedded_name.to_string name; loc = !Ast_helper.default_loc };
    attr_loc = !Ast_helper.default_loc;
    attr_payload = payload
  }

(** For a syntactic category, produce translations into and out of
    our novel syntax, using parsetree attributes as the encoding.
*)
module Make_with_attribute (AST_syntactic_category : sig
  include AST_syntactic_category

  val attributes : ast -> attributes

  val with_attributes : ast -> attributes -> ast
end) : AST_internal with type ast = AST_syntactic_category.ast = struct
  include AST_syntactic_category

  let embedding_syntax = Embedding_syntax.Attribute

  let make_jane_syntax name ?(payload = PStr []) ast =
    let attr = make_jane_syntax_attribute name payload in
    (* See Note [Outer attributes at end] in jane_syntax.ml *)
    with_attributes ast (attributes ast @ [attr])

  let match_jane_syntax ast =
    match find_and_remove_jane_syntax_attribute (attributes ast) with
    | None -> None
    | Some (name, loc, payload, attrs) ->
      Some (name, loc, payload, with_attributes ast attrs)
end

(** For a syntactic category, produce translations into and out of
    our novel syntax, using extension nodes as the encoding.
*)
module Make_with_extension_node (AST_syntactic_category : sig
  include AST_syntactic_category

  (** How to construct an extension node for this AST (something of the
          shape [[%name]]). Should just be [Ast_helper.CAT.extension] for the
          appropriate syntactic category [CAT]. (This means that [?loc] should
          default to [!Ast_helper.default_loc.].) *)
  val make_extension_node :
    ?loc:Location.t -> ?attrs:attributes -> extension -> ast

  (** Given an extension node (as created by [make_extension_node]) with an
          appropriately-formed name and a body, combine them into the special
          syntactic form we use for novel syntactic features in this syntactic
          category. Partial inverse of [match_extension_use]. *)
  val make_extension_use : extension_node:ast -> ast -> ast

  (** Given an AST node, check if it's of the special syntactic form
          indicating that this is one of our novel syntactic features (as
          created by [make_extension_node]), split it back up into the extension
          node and the possible body. Doesn't do any checking about the
          name/format of the extension or the possible body terms (for which see
          [AST.match_extension]). Partial inverse of [make_extension_use]. *)
  val match_extension_use : ast -> (extension * ast) option
end) : AST_internal with type ast = AST_syntactic_category.ast = struct
  include AST_syntactic_category

  let embedding_syntax = Embedding_syntax.Extension_node

  let make_jane_syntax name ?(payload = PStr []) ast =
    make_extension_use ast
      ~extension_node:
        (make_extension_node
           ( { txt = Embedded_name.to_string name;
               loc = !Ast_helper.default_loc
             },
             payload ))

  let match_jane_syntax ast =
    match match_extension_use ast with
    | None -> None
    | Some (({ txt = name; loc = ext_loc }, ext_payload), body) -> (
      match parse_embedding_exn ~loc:ext_loc ~name ~embedding_syntax with
      | None -> None
      | Some name -> Some (name, ext_loc, ext_payload, body))
end

(********************************************************)
(* Modules representing individual syntactic categories *)

(* Note [Hiding internal details]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Each such module is first written with a '0' suffix. These '0'
   modules are used internally as arguments to [Make_ast] to produce
   non-'0' modules which are exported. This approach allows us to
   hide details of these modules necessary for [Make_ast] but
   unnecessary for external uses.
*)

(** The AST parameters for every subset of types; embedded with attributes. *)
module Type_AST_syntactic_category = struct
  type ast = core_type

  (* Missing [plural] *)

  let location typ = typ.ptyp_loc

  let with_location typ l = { typ with ptyp_loc = l }

  let attributes typ = typ.ptyp_attributes

  let with_attributes typ ptyp_attributes = { typ with ptyp_attributes }
end

(** Types; embedded with attributes. *)
module Core_type0 = Make_with_attribute (struct
  include Type_AST_syntactic_category

  let plural = "types"
end)

(** Constructor arguments; the same as types, but used in fewer places *)
module Constructor_argument0 = Make_with_attribute (struct
  include Type_AST_syntactic_category

  let plural = "constructor arguments"
end)

(** Expressions; embedded using an attribute on the expression. *)
module Expression0 = Make_with_attribute (struct
  type ast = expression

  let plural = "expressions"

  let location expr = expr.pexp_loc

  let with_location expr l = { expr with pexp_loc = l }

  let attributes expr = expr.pexp_attributes

  let with_attributes expr pexp_attributes = { expr with pexp_attributes }
end)

(** Patterns; embedded using an attribute on the pattern. *)
module Pattern0 = Make_with_attribute (struct
  type ast = pattern

  let plural = "patterns"

  let location pat = pat.ppat_loc

  let with_location pat l = { pat with ppat_loc = l }

  let attributes pat = pat.ppat_attributes

  let with_attributes pat ppat_attributes = { pat with ppat_attributes }
end)

(** Module types; embedded using an attribute on the module type. *)
module Module_type0 = Make_with_attribute (struct
  type ast = module_type

  let plural = "module types"

  let location mty = mty.pmty_loc

  let with_location mty l = { mty with pmty_loc = l }

  let attributes mty = mty.pmty_attributes

  let with_attributes mty pmty_attributes = { mty with pmty_attributes }
end)

(** Extension constructors; embedded using an attribute. *)
module Extension_constructor0 = Make_with_attribute (struct
  type ast = extension_constructor

  let plural = "extension constructors"

  let location ext = ext.pext_loc

  let with_location ext l = { ext with pext_loc = l }

  let attributes ext = ext.pext_attributes

  let with_attributes ext pext_attributes = { ext with pext_attributes }
end)

(** Signature items; embedded as
    [include sig [%%extension.EXTNAME];; BODY end]. Signature items don't have
    attributes or we'd use them instead.
*)
module Signature_item0 = Make_with_extension_node (struct
  type ast = signature_item

  let plural = "signature items"

  let location sigi = sigi.psig_loc

  let with_location sigi l = { sigi with psig_loc = l }

  let make_extension_node = Ast_helper.Sig.extension

  let make_extension_use ~extension_node sigi =
    Ast_helper.Sig.include_
      { pincl_mod = Ast_helper.Mty.signature [extension_node; sigi];
        pincl_loc = !Ast_helper.default_loc;
        pincl_attributes = []
      }

  let match_extension_use sigi =
    match sigi.psig_desc with
    | Psig_include
        { pincl_mod =
            { pmty_desc =
                Pmty_signature
                  [{ psig_desc = Psig_extension (ext, []); _ }; sigi];
              _
            };
          _
        } ->
      Some (ext, sigi)
    | _ -> None
end)

(** Structure items; embedded as
    [include struct [%%extension.EXTNAME];; BODY end]. Structure items don't
    have attributes or we'd use them instead.
*)
module Structure_item0 = Make_with_extension_node (struct
  type ast = structure_item

  let plural = "structure items"

  let location stri = stri.pstr_loc

  let with_location stri l = { stri with pstr_loc = l }

  let make_extension_node = Ast_helper.Str.extension

  let make_extension_use ~extension_node stri =
    Ast_helper.Str.include_
      { pincl_mod = Ast_helper.Mod.structure [extension_node; stri];
        pincl_loc = !Ast_helper.default_loc;
        pincl_attributes = []
      }

  let match_extension_use stri =
    match stri.pstr_desc with
    | Pstr_include
        { pincl_mod =
            { pmod_desc =
                Pmod_structure
                  [{ pstr_desc = Pstr_extension (ext, []); _ }; stri];
              _
            };
          _
        } ->
      Some (ext, stri)
    | _ -> None
end)

(** Constructor declarations; embedded with attributes. *)
module Constructor_declaration0 = Make_with_attribute (struct
  type ast = Parsetree.constructor_declaration

  let plural = "constructor declarations"

  let location pcd = pcd.pcd_loc

  let with_location pcd loc = { pcd with pcd_loc = loc }

  let attributes pcd = pcd.pcd_attributes

  let with_attributes pcd pcd_attributes = { pcd with pcd_attributes }
end)

(** Type declarations; embedded with attributes. *)
module Type_declaration0 = Make_with_attribute (struct
  type ast = Parsetree.type_declaration

  let plural = "type declarations"

  let location ptype = ptype.ptype_loc

  let with_location ptype loc = { ptype with ptype_loc = loc }

  let attributes ptype = ptype.ptype_attributes

  let with_attributes ptype ptype_attributes = { ptype with ptype_attributes }
end)

(******************************************************************************)
(* Main exports *)

module type AST = sig
  type ast

  val make_jane_syntax :
    Feature.t -> string list -> ?payload:payload -> ast -> ast

  val make_entire_jane_syntax :
    loc:Location.t -> Feature.t -> (unit -> ast) -> ast

  val make_of_ast :
    of_ast_internal:(Feature.t -> ast -> 'a option) -> ast -> 'a option
end

let needs_extra_checks = function
  | Feature.Language_extension Mode -> false
  | _ -> true

(* See Note [Hiding internal details] *)
module Make_ast (AST : AST_internal) : AST with type ast = AST.ast = struct
  include AST

  let make_jane_syntax feature trailing_components ?payload ast =
    AST.make_jane_syntax
      (Embedded_name.of_feature feature trailing_components)
      ?payload ast

  let make_entire_jane_syntax ~loc feature ast =
    AST.with_location
      (* We can't call [Location.ghostify] here, as we need
         [jane_syntax_parsing.ml] to build with the upstream compiler; see
         Note [Buildable with upstream] in jane_syntax.mli for details. *)
      (Ast_helper.with_default_loc { loc with loc_ghost = true } (fun () ->
           make_jane_syntax feature [] (ast ())))
      loc

  (** Generically lift our custom ASTs for our novel syntax from OCaml ASTs. *)
  let make_of_ast ~of_ast_internal =
    let of_ast ast =
      let loc = AST.location ast in
      let raise_error loc err = raise (Error (loc, err)) in
      match AST.match_jane_syntax ast with
      | Some
          ( ({ erasability; components = [name] } as embedded_name),
            syntax_loc,
            payload,
            ast ) -> (
        match Feature.of_component name with
        | Ok feat -> (
          (if needs_extra_checks feat
          then
            match payload with
            | PStr [] -> ()
            | _ ->
              raise_error syntax_loc
                (Introduction_has_payload
                   (AST.embedding_syntax, embedded_name, payload)));
          match of_ast_internal feat ast with
          | Some ext_ast -> Some ext_ast
          | None ->
            if needs_extra_checks feat
            then raise_error loc (Wrong_syntactic_category (feat, AST.plural))
            else None)
        | Error err ->
          raise_error loc
            (match err with
            | Disabled_extension ext ->
              Disabled_extension { ext; maturity = None }
            | Unknown_extension name ->
              Unknown_extension (AST.embedding_syntax, erasability, name)))
      | Some (({ components = _ :: _ :: _; _ } as name), _, _, _) ->
        raise_error loc (Bad_introduction (AST.embedding_syntax, name))
      | None -> None
    in
    of_ast
end

let make_jane_syntax_attribute feature trailing_components payload =
  make_jane_syntax_attribute
    (Embedded_name.of_feature feature trailing_components)
    payload

(* See Note [Hiding internal details] *)
module Expression = Make_ast (Expression0)
module Pattern = Make_ast (Pattern0)
module Module_type = Make_ast (Module_type0)
module Signature_item = Make_ast (Signature_item0)
module Structure_item = Make_ast (Structure_item0)
module Core_type = Make_ast (Core_type0)
module Constructor_argument = Make_ast (Constructor_argument0)
module Extension_constructor = Make_ast (Extension_constructor0)
module Constructor_declaration = Make_ast (Constructor_declaration0)
module Type_declaration = Make_ast (Type_declaration0)
