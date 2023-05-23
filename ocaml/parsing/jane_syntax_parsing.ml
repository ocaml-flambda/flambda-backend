(** As mentioned in the .mli file, there are some gory details around the
    particular translation scheme we adopt for moving to and from OCaml ASTs
    ([Parsetree.expression], etc.).  The general idea is that we adopt a scheme
    where each novel piece of syntax is represented using one of two embeddings:

    1. As an AST item carrying an attribute. The AST item serves as the "body"
      of the syntax indicated by the attribute.
    2. As a pair of an extension node and an AST item that serves as the "body".
       Here, the "pair" is embedded as a pair-like construct in the relevant AST
       category, e.g. [include sig [%%extension.EXTNAME];; BODY end] for
       signature items.

    In particular, for an extension named [EXTNAME] (i.e., one
    that is enabled by [-extension EXTNAME] on the command line), the attribute
    (if used) must be [[@jane.EXTNAME]], and the extension (if used) must be
    [[%jane.EXTNAME]]. For built-in syntax, we use [_builtin]
    instead of an extension name.

    In the below example, we use attributes an examples, but it applies equally
    to extensions. We also provide utilities for further desugaring similar
    applications where the embeddings have the longer form
    [[@jane.FEATNAME.ID1.ID2.….IDn]] (with the outermost one being the [n = 0]
    case), as these might be used inside the [EXPR]. (For example, within the
    outermost [[@jane.comprehensions]] term for list and array comprehensions,
    we can also use [[@jane.comprehensions.list]],
    [[@jane.comprehensions.array]], [[@jane.comprehensions.for.in]], etc.).

    As mentioned, we represent terms as a "pair" and don't use the extension
    node or attribute payload; this is so that ppxen can see inside these
    extension nodes. If we put the subexpressions inside the payload, then we
    couldn't write something like [[[%string "Hello, %{x}!"] for x in names]],
    as [ppx_string] wouldn't traverse inside the payload to find the [[%string]]
    extension node.

    Our novel syntactic features are of course allowed to impose extra
    constraints on what legal bodies are; we're also happy for this translation
    to error in various ways on malformed input, since nobody should ever be
    writing these forms directly.  They're just an implementation detail.

    See modules of type AST below to see how different syntactic categories
    are represented. For example, expressions are rendered using an attribute.

    We provide one module per syntactic category (e.g., [Expression]), of module
    type [AST].  They also provide some simple machinery for working with the
    general [@jane.FEATNAME.ID1.ID2.….IDn] wrapped forms.  To construct one, we
    provide [make_jane_syntax]; to destructure one, we provide
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

(******************************************************************************)

module Feature : sig
  type t =
    | Language_extension of Language_extension.t
    | Builtin

  type error =
    | Disabled_extension of Language_extension.t
    | Unknown_extension of string

  val describe_uppercase : t -> string

  val extension_component : t -> string

  val of_component : string -> (t, error) result
end = struct
  type t = Language_extension of Language_extension.t
         | Builtin

  type error =
    | Disabled_extension of Language_extension.t
    | Unknown_extension of string

  let builtin_component = "_builtin"

  let describe_uppercase = function
    | Language_extension ext ->
        "The extension \"" ^ Language_extension.to_string ext ^ "\""
    | Builtin ->
        "Built-in syntax"

  let extension_component = function
    | Language_extension ext -> Language_extension.to_string ext
    | Builtin -> builtin_component

  let of_component str =
    if String.equal str builtin_component then
      Ok Builtin
    else
      match Language_extension.of_string str with
      | Some ext when Language_extension.is_enabled ext ->
          Ok (Language_extension ext)
      | Some ext ->
          Error (Disabled_extension ext)
      | None ->
          Error (Unknown_extension str)
end

(* FUTURE-PROOFING: We're about to add builtin stuff; delete this ignore-only
   binding when we do. *)
let _ = Feature.extension_component

(** Was this embedded as an [[%extension_node]] or an [[@attribute]]?  Not
    exported. *)
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
    let sigil = match t with
      | Extension_node -> "%"
      | Attribute -> "@"
    in
    Format.fprintf ppf "[%s%s]" sigil name
end

(******************************************************************************)

(** An AST-style representation of the names used when generating extension
    nodes or attributes for modular syntax; see the .mli file for more
    details. *)
module Embedded_name : sig
  (** A nonempty list of name components, without the leading root component
      that identifies it as part of the modular syntax mechanism; see the .mli
      file for more details. *)
  type t = ( :: ) of string * string list

  (** Convert one of these Jane syntax names to the embedded string form used in
      the OCaml AST as the name of an extension node or an attribute; not
      exposed. *)
  val to_string : t -> string

  (** Parse a Jane syntax name from the OCaml AST, either as the name of an
      extension node or an attribute:
        - [Some (Ok _)] if it's a legal Jane-syntax name;
        - [Some (Error ())] if it's the bare root name; and
        - [None] if it doesn't start with the leading root name and isn't part
          of our Jane-syntax machinery.
      Not exposed. *)
  val of_string : string -> (t, unit) result option

  (** Print out the embedded form of a Jane-syntax name, in quotes; for use in
      error messages. *)
  val pp_quoted_name : Format.formatter -> t -> unit

  (** Print out an empty extension node or attribute with a Jane-syntax name,
      accompanied by an indefinite article; for use in error messages.  Not
      exposed. *)
  val pp_a_term : Format.formatter -> Embedding_syntax.t * t -> unit

  (** Print out the illegal empty quasi-Jane-syntax extension node or attribute
      with no name beyond the leading root component; for use in error messages.
      Not exposed. *)
  val pp_bad_empty_term : Format.formatter -> Embedding_syntax.t -> unit
end = struct
  (** The three parameters that control how we encode Jane-syntax extension node
      names.  When updating these, update comments that refer to them by their
      contents! *)
  module Config = struct
    (** The separator between name components *)
    let separator = '.'

    (** The leading namespace that identifies this extension node as reserved
        for a modular extension *)
    let root = "jane"

    (** For printing purposes, the appropriate indefinite article for [root] *)
    let article = "a"
  end

  include Config
  let separator_str = String.make 1 separator

  type t = ( :: ) of string * string list

  let to_string (feat :: subparts) =
    String.concat separator_str (root :: feat :: subparts)

  let of_string str = match String.split_on_char separator str with
    | root' :: parts when String.equal root root' -> begin
        match parts with
        | feat :: subparts -> Some (Ok (feat :: subparts))
        | []               -> Some (Error ())
      end
    | _ :: _ | [] -> None

  let pp_quoted_name ppf t = Format.fprintf ppf "\"%s\"" (to_string t)

  let pp_a_term ppf (esyn, t) =
    Format.fprintf ppf "%s %a" article Embedding_syntax.pp (esyn, to_string t)

  let pp_bad_empty_term ppf esyn = Embedding_syntax.pp ppf (esyn, root)
end

(******************************************************************************)
module Error = struct
  (** Someone used [[%jane.FEATNAME]]/[[@jane.FEATNAME]] wrong *)
  type malformed_embedding =
    | Has_payload of payload

  (** An error triggered when desugaring a language extension from an OCaml
      AST; should always be fatal *)
  type error =
    | Malformed_embedding of
        Embedding_syntax.t * Embedded_name.t * malformed_embedding
    | Unknown_extension of Embedding_syntax.t * string
    | Disabled_extension of Language_extension.t
    | Wrong_syntactic_category of Feature.t * string
    | Unnamed_embedding of Embedding_syntax.t
    | Bad_introduction of Embedding_syntax.t * Embedded_name.t

  (** The exception type thrown when desugaring a piece of modular syntax from
      an OCaml AST *)
  exception Error of Location.t * error
end

open Error

let assert_extension_enabled ~loc ext =
  if not (Language_extension.is_enabled ext) then
    raise (Error(loc, Disabled_extension ext))
;;

let report_error ~loc = function
  | Malformed_embedding(what, name, malformed) -> begin
      match malformed with
      | Has_payload _payload ->
          Location.errorf
            ~loc
            "@[Modular syntax %s are not allowed to have a payload,@ \
             but %a does@]"
          (Embedding_syntax.name_plural what)
          Embedded_name.pp_quoted_name name
    end
  | Unknown_extension (what, name) ->
      Location.errorf
        ~loc
        "@[Unknown extension \"%s\" referenced via@ %a %s@]"
        name
        Embedded_name.pp_a_term (what, Embedded_name.[name])
        (Embedding_syntax.name what)
  | Disabled_extension ext ->
      Location.errorf
        ~loc
        "The extension \"%s\" is disabled and cannot be used"
        (Language_extension.to_string ext)
  | Wrong_syntactic_category(feat, cat) ->
      Location.errorf
        ~loc
        "%s cannot appear in %s"
        (Feature.describe_uppercase feat)
        cat
  | Unnamed_embedding what ->
      Location.errorf
        ~loc
        "Cannot have %s named %a"
        (Embedding_syntax.name_indefinite what)
        Embedded_name.pp_bad_empty_term what
  | Bad_introduction(what, (ext :: _ as name)) ->
      Location.errorf
        ~loc
        "@[The extension \"%s\" was referenced improperly; it started with@ \
         %a %s,@ not %a one@]"
        ext
        Embedded_name.pp_a_term (what, name)
        (Embedding_syntax.name what)
        Embedded_name.pp_a_term (what, Embedded_name.[ext])

let () =
  Location.register_error_of_exn
    (function
      | Error(loc, err) -> Some (report_error ~loc err)
      | _ -> None)

(******************************************************************************)
(** Generically find and create the OCaml AST syntax used to encode one of our
    novel syntactic features.  One module per variety of AST (expressions,
    patterns, etc.). *)

(** The parameters that define how to look for [[%jane.FEATNAME]] and
    [[@jane.FEATNAME]] inside ASTs of a certain syntactic category. This module
    type describes the input to the [Make_with_attribute] and
    [Make_with_extension_node] functors (though they stipulate additional
    requirements for their inputs).
*)
module type AST_syntactic_category = sig
  (** The AST type (e.g., [Parsetree.expression]) *)
  type ast

  (** The "AST description" type, without the location and attributes (e.g.,
      [Parsetree.expression_desc]) *)
  type ast_desc

  (** The name for this syntactic category in the plural form; used for error
      messages (e.g., "expressions") *)
  val plural : string

  (** How to get the location attached to an AST node.  Should just be
      [fun tm -> tm.pCAT_loc] for the appropriate syntactic category [CAT]. *)
  val location : ast -> Location.t

  (** Turn an [ast_desc] into an [ast] by adding the appropriate metadata.  When
      creating [ast] nodes afresh to embed our novel syntax, the location should
      be omitted; in this case, it will default to [!Ast_helper.default_loc],
      which should be [ghost]. *)
  val wrap_desc :
    ?loc:Location.t -> attrs:Parsetree.attributes -> ast_desc -> ast
end

module type AST = sig
  include AST_syntactic_category

  val embedding_syntax : Embedding_syntax.t

  val make_jane_syntax : Embedded_name.t -> ast -> ast_desc

  (** Given an AST node, check if it's a representation of a term from one of
      our novel syntactic features; if it is, split it back up into its name and
      the body.  If the embedded term is malformed in any way, raises an error;
      if the input isn't an embedding of one of our novel syntactic features,
      returns [None].  Partial inverse of [make_jane_syntax]. *)
  val match_jane_syntax : ast -> (Embedded_name.t * ast) option
end

(* Some extensions written before this file existed are handled in their own
   way; this function filters them out. *)
let uniformly_handled_extension name =
  match name with
  | "local"|"global"|"nonlocal"|"escape"|"curry" -> false
  | _ -> true

(* Parses the embedded name from an embedding, raising if
    the embedding is malformed. Malformed means either:

    1. The embedding has a payload; attribute payloads must
    be empty, so other ppxes can traverse "into" them.

    2. NAME is missing; e.g. the attribute is just [[@jane]].
*)
let parse_embedding_exn ~loc ~payload ~name ~embedding_syntax =
  let raise_error err = raise (Error (loc, err)) in
  match Embedded_name.of_string name with
  | Some (Ok (feat :: _ as name))
    when uniformly_handled_extension feat -> begin
      let raise_malformed err =
        raise_error (Malformed_embedding (embedding_syntax, name, err))
      in
      match payload with
      | PStr [] -> Some name
      | _ -> raise_malformed (Has_payload payload)
    end
  | Some (Error ()) -> raise_error (Unnamed_embedding embedding_syntax)
  | Some (Ok (_ :: _)) | None -> None

module With_attributes = struct
  type 'desc t =
    { jane_syntax_attributes : Parsetree.attributes
    ; desc : 'desc
    }
end

let find_and_remove_jane_syntax_attribute =
  let rec loop rest ~rev_prefix =
    match rest with
    | [] -> None
    | attr :: rest ->
      let { attr_name = { txt = name; loc = attr_loc }; attr_payload } =
        attr
      in
      begin
        match
         parse_embedding_exn
           ~loc:attr_loc
           ~payload:attr_payload
           ~name
           ~embedding_syntax:Attribute
        with
        | None -> loop rest ~rev_prefix:(attr :: rev_prefix)
        | Some name -> Some (name, List.rev_append rev_prefix rest)
      end
  in
  fun attributes -> loop attributes ~rev_prefix:[]
;;

(** For a syntactic category, produce translations into and out of
    our novel syntax, using parsetree attributes as the encoding.
*)
module Make_with_attribute
    (AST_syntactic_category : sig
       include AST_syntactic_category

       val desc : ast -> ast_desc
       val attributes : ast -> Parsetree.attributes
       val with_attributes : ast -> Parsetree.attributes -> ast
     end) :
    AST with type ast      = AST_syntactic_category.ast
         and type ast_desc =
               AST_syntactic_category.ast_desc With_attributes.t
= struct
    include AST_syntactic_category

    let embedding_syntax = Embedding_syntax.Attribute

    type nonrec ast_desc = ast_desc With_attributes.t

    let wrap_desc ?loc ~attrs:extra_attrs with_attributes =
      let { desc; jane_syntax_attributes } : _ With_attributes.t =
        with_attributes
      in
      wrap_desc ?loc ~attrs:(jane_syntax_attributes @ extra_attrs) desc

    let make_jane_syntax name ast : _ With_attributes.t =
      let original_attributes = attributes ast in
      let attr =
        { attr_name =
            { txt = Embedded_name.to_string name
            ; loc = !Ast_helper.default_loc
            }
        ; attr_loc = !Ast_helper.default_loc
        ; attr_payload = PStr []
        }
      in
      { desc = desc ast; jane_syntax_attributes = attr :: original_attributes }

    let match_jane_syntax ast =
      match find_and_remove_jane_syntax_attribute (attributes ast) with
      | None -> None
      | Some (name, attrs) -> Some (name, with_attributes ast attrs)
end

(** For a syntactic category, produce translations into and out of
    our novel syntax, using extension nodes as the encoding.
*)
module Make_with_extension_node
    (AST_syntactic_category : sig
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
      val make_extension_use  : extension_node:ast -> ast -> ast_desc

      (** Given an AST node, check if it's of the special syntactic form
          indicating that this is one of our novel syntactic features (as
          created by [make_extension_node]), split it back up into the extension
          node and the possible body. Doesn't do any checking about the
          name/format of the extension or the possible body terms (for which see
          [AST.match_extension]). Partial inverse of [make_extension_use]. *)
      val match_extension_use : ast -> (extension * ast) option
     end) :
    AST with type ast      = AST_syntactic_category.ast
         and type ast_desc = AST_syntactic_category.ast_desc =
  struct
    include AST_syntactic_category

    let embedding_syntax = Embedding_syntax.Extension_node

    let make_jane_syntax name ast =
      make_extension_use
        ast
        ~extension_node:
          (make_extension_node
            ({ txt = Embedded_name.to_string name
              ; loc = !Ast_helper.default_loc },
              PStr []))

    let match_jane_syntax ast =
      match match_extension_use ast with
      | None -> None
      | Some (({txt = name; loc = ext_loc}, ext_payload), body) ->
        match
          parse_embedding_exn
            ~loc:ext_loc
            ~payload:ext_payload
            ~name
            ~embedding_syntax
        with
        | None -> None
        | Some name -> Some (name, body)
end

(** The AST parameters for every subset of types; embedded as
    [[[%jane.FEATNAME] * BODY]]. *)
module Type_AST_syntactic_category = struct
  type ast = core_type
  type ast_desc = core_type_desc

  (* Missing [plural] *)

  let location typ = typ.ptyp_loc

  let wrap_desc ?loc ~attrs = Ast_helper.Typ.mk ?loc ~attrs

  let attributes typ = typ.ptyp_attributes
  let with_attributes typ ptyp_attributes = { typ with ptyp_attributes }
  let desc typ = typ.ptyp_desc
end

(** Types; embedded as [[[%jane.FEATNAME] * BODY]]. *)
module Core_type = Make_with_attribute (struct
    include Type_AST_syntactic_category

    let plural = "types"
end)

(** Constructor arguments; the same as types, but used in fewer places *)
module Constructor_argument = Make_with_attribute (struct
  include Type_AST_syntactic_category

  let plural = "constructor arguments"
end)

(** Expressions; embedded using an attribute on the expression. *)
module Expression = Make_with_attribute (struct
  type ast = expression
  type ast_desc = expression_desc

  let plural = "expressions"

  let location expr = expr.pexp_loc

  let wrap_desc ?loc ~attrs = Ast_helper.Exp.mk ?loc ~attrs

  let desc expr = expr.pexp_desc
  let attributes expr = expr.pexp_attributes
  let with_attributes expr pexp_attributes = { expr with pexp_attributes }
end)

(** Patterns; embedded using an attribute on the pattern. *)
module Pattern = Make_with_attribute (struct
  type ast = pattern
  type ast_desc = pattern_desc

  let plural = "patterns"

  let location pat = pat.ppat_loc

  let wrap_desc ?loc ~attrs = Ast_helper.Pat.mk ?loc ~attrs

  let desc pat = pat.ppat_desc
  let attributes pat = pat.ppat_attributes
  let with_attributes pat ppat_attributes = { pat with ppat_attributes }
end)

(** Module types; embedded using an attribute on the module type. *)
module Module_type = Make_with_attribute (struct
    type ast = module_type
    type ast_desc = module_type_desc

    let plural = "module types"

    let location mty = mty.pmty_loc

    let wrap_desc ?loc ~attrs = Ast_helper.Mty.mk ?loc ~attrs

    let desc mty = mty.pmty_desc
    let attributes mty = mty.pmty_attributes
    let with_attributes mty pmty_attributes = { mty with pmty_attributes }
end)

(** Signature items; embedded as
    [include sig [%%extension.EXTNAME];; BODY end]. Signature items don't have
    attributes or we'd use them instead.
*)
module Signature_item = Make_with_extension_node (struct
    type ast = signature_item
    type ast_desc = signature_item_desc

    let plural = "signature items"

    let location sigi = sigi.psig_loc

    (* The attributes are only set in [ast_mapper], so requiring them to be
       empty here is fine, as there won't be any to set in that case. *)
    let wrap_desc ?loc ~attrs =
      match attrs with
      | [] -> Ast_helper.Sig.mk ?loc
      | _ :: _ ->
          Misc.fatal_errorf
            "Jane syntax: Cannot put attributes on a signature item"

    let make_extension_node = Ast_helper.Sig.extension

    let make_extension_use ~extension_node sigi =
      Psig_include { pincl_mod = Ast_helper.Mty.signature [extension_node; sigi]
                   ; pincl_loc = !Ast_helper.default_loc
                   ; pincl_attributes = [] }

    let match_extension_use sigi =
      match sigi.psig_desc with
      | Psig_include
          { pincl_mod =
              { pmty_desc =
                  Pmty_signature
                    [ { psig_desc = Psig_extension (ext, []); _ }
                    ; sigi ]
              ; _}
          ; _}
        ->
          Some (ext, sigi)
      | _ -> None
end)

(** Structure items; embedded as
    [include struct [%%extension.EXTNAME];; BODY end]. Structure items don't
    have attributes or we'd use them instead.
*)
module Structure_item = Make_with_extension_node (struct
    type ast = structure_item
    type ast_desc = structure_item_desc

    let plural = "structure items"

    let location stri = stri.pstr_loc

    (* The attributes are only set in [ast_mapper], so requiring them to be
       empty here is fine, as there won't be any to set in that case. *)
    let wrap_desc ?loc ~attrs =
      match attrs with
      | [] -> Ast_helper.Str.mk ?loc
      | _ :: _ ->
          Misc.fatal_errorf
            "Jane syntax: Cannot put attributes on a structure item"

    let make_extension_node = Ast_helper.Str.extension

    let make_extension_use ~extension_node stri =
      Pstr_include { pincl_mod = Ast_helper.Mod.structure [extension_node; stri]
                   ; pincl_loc = !Ast_helper.default_loc
                   ; pincl_attributes = [] }

    let match_extension_use stri =
      match stri.pstr_desc with
      | Pstr_include
          { pincl_mod =
              { pmod_desc =
                  Pmod_structure
                    [ { pstr_desc = Pstr_extension (ext, []); _ }
                    ; stri ]
              ; _}
          ; _}
        ->
          Some (ext, stri)
      | _ -> None
end)

(******************************************************************************)

module AST = struct
  type (_, _) t =
    | Expression :
        (Parsetree.expression, Parsetree.expression_desc With_attributes.t) t
    | Pattern : (Parsetree.pattern, Parsetree.pattern_desc With_attributes.t) t
    | Module_type :
        (Parsetree.module_type, Parsetree.module_type_desc With_attributes.t) t
    | Signature_item :
        (Parsetree.signature_item, Parsetree.signature_item_desc) t
    | Structure_item :
        (Parsetree.structure_item, Parsetree.structure_item_desc) t
    | Core_type :
        (Parsetree.core_type, Parsetree.core_type_desc With_attributes.t) t
    | Constructor_argument :
        (Parsetree.core_type, Parsetree.core_type_desc With_attributes.t) t

  let to_module (type ast ast_desc) (t : (ast, ast_desc) t) :
    (module AST with type ast = ast and type ast_desc = ast_desc) =
    match t with
    | Expression -> (module Expression)
    | Pattern -> (module Pattern)
    | Module_type -> (module Module_type)
    | Signature_item -> (module Signature_item)
    | Structure_item -> (module Structure_item)
    | Core_type -> (module Core_type)
    | Constructor_argument -> (module Constructor_argument)

  let wrap_desc (type ast ast_desc) (t : (ast, ast_desc) t) =
    let (module AST) = to_module t in
    AST.wrap_desc

  let make_jane_syntax (type ast ast_desc) (t : (ast, ast_desc) t) =
    let (module AST) = to_module t in
    AST.make_jane_syntax

  let make_entire_jane_syntax t ~loc name ast =
    make_jane_syntax t [name]
      (Ast_helper.with_default_loc (Location.ghostify loc) ast)

  (** Generically lift our custom ASTs for our novel syntax from OCaml ASTs. *)
  let make_of_ast (type ast ast_desc) (t : (ast, ast_desc) t) ~of_ast_internal =
    let (module AST) = to_module t in
    let of_ast ast =
      let loc = AST.location ast in
      let raise_error err = raise (Error (loc, err)) in
      match AST.match_jane_syntax ast with
      | Some ([name], ast) -> begin
          match Feature.of_component name with
          | Ok feat -> begin
              match of_ast_internal feat ast with
              | Some ext_ast -> Some ext_ast
              | None ->
                  raise_error (Wrong_syntactic_category(feat, AST.plural))
            end
          | Error err -> raise_error begin match err with
            | Disabled_extension ext -> Disabled_extension ext
            | Unknown_extension name ->
                Unknown_extension (AST.embedding_syntax, name)
          end
        end
      | Some (_ :: _ :: _ as name, _) ->
          raise_error (Bad_introduction(AST.embedding_syntax, name))
      | None -> None
    in
    of_ast
end
