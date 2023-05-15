(** As mentioned in the .mli file, there are some gory details around the
    particular translation scheme we adopt for moving to and from OCaml ASTs
    ([Parsetree.expression], etc.).  The general idea is that we adopt a scheme
    where a language extension is represented as a pair of an extension node and
    an AST item that serves as the "body".  In particular, for an extension
    named [EXTNAME] (i.e., one that is enabled by [-extension EXTNAME] on the
    command line), the extension node used must be [[%extension.EXTNAME]].  We
    also provide utilities for further desugaring similar applications where the
    extension nodes have the longer form [[%extension.EXTNAME.ID1.ID2.….IDn]]
    (with the outermost one being the [n = 0] case), as these might be used
    inside the [EXPR].  (For example, within the outermost
    [[%extension.comprehensions]] term for list and array comprehensions, we can
    also use [[%extension.comprehensions.list]],
    [[%extension.comprehensions.array]], [[%extensions.comprehensions.for.in]],
    etc.).

    As mentioned, we represent terms as a "pair" and don't use the extension
    node payload; this is so that ppxen can see inside these extension nodes.
    If we put the subexpressions inside the extension node payload, then we
    couldn't write something like [[[%string "Hello, %{x}!"] for x in names]],
    as [ppx_string] wouldn't traverse inside the payload to find the [[%string]]
    extension point.

    Language extensions are of course allowed to impose extra constraints
    constraints on what legal bodies are; we're also happy for this translation
    to error in various ways on malformed input, since nobody should ever be
    writing these forms directly.  They're just an implementation detail.

    See modules of type AST below to see how different syntactic categories
    are represented. For example, expressions are rendered as an application
    of the extension node to the body, i.e. [([%extension.EXTNAME] EXPR)].

    We provide one module per syntactic category (e.g., [Expression]), of module
    type [AST].  They also provide some simple machinery for working with the
    general [%extension.EXTNAME.ID1.ID2.….IDn] wrapped forms.  To construct
    one, we provide [extension_expr]; to destructure one, we provide
    [match_extension] in the various AST modules; to construct one, we provide
    [make_extension] in the same places..  We still have to write the
    transformations in both directions for all new syntax, lowering it to
    extension nodes and then lifting it back out. *)

open Parsetree

(******************************************************************************)
(** Collect all the extension-node-name-building machinery in one place so that
    it can be changed all at once. *)

(** An AST-style representation of the names used when generating extension
    nodes for modular extensions; see the .mli file for more details. *)
module Extension_node_name : sig
  (** A nonempty list of name components, without the leading root [extension.];
      see the .mli file for more details. *)
  type t = ( :: ) of string * string list

  (** Convert a modular extension extension node's name to the string form used
      in the OCaml AST; not exposed. *)
  val to_string : t -> string

  (** Parse an OCaml extension node's name:
        - [Some (Ok _)] if it's a legal modular extension name;
        - [Some (Error ())] if it's the bare [extension]; and
        - [None] if it doesn't start with the leading [extension].
      Not exposed. *)
  val of_string : string -> (t, unit) result option

  (** Print out a modular extension extension node name, in quotes; for use in
      error messages. *)
  val pp_quoted_name : Format.formatter -> t -> unit

  (** Print out an empty extension node with a modular extension name,
      accompanied by an indefinite article; for use in error messages.  Not
      exposed. *)
  val pp_a_node : Format.formatter -> t -> unit

  (** Print out the illegal empty quasi-modular extension extension node with no
      name beyond [extension]; for use in error messages.  Not exposed. *)
  val pp_bad_empty_node : Format.formatter -> unit -> unit
end = struct
  (** The three parameters that control how we encode modular extension
      extension node names.  When updating these, update comments that refer to
      them! *)
  module Config = struct
    (** The separator between name components *)
    let separator = '.'

    (** The leading namespace that identifies this extension point as reserved
        for a modular extension *)
    let root = "extension"

    (** For printing purposes, the appropriate indefinite article for [root] *)
    let article = "an"
  end

  include Config
  let separator_str = String.make 1 separator

  type t = ( :: ) of string * string list

  let to_string (ext :: subparts) =
    String.concat separator_str (root :: ext :: subparts)

  let of_string str = match String.split_on_char separator str with
    | root' :: parts when String.equal root root' -> begin
        match parts with
        | ext :: subparts -> Some (Ok (ext :: subparts))
        | []              -> Some (Error ())
      end
    | _ :: _ | [] -> None

  let pp_quoted_name ppf t = Format.fprintf ppf "\"%s\"" (to_string t)

  let pp_extension_node ppf id = Format.fprintf ppf "[%%%s]" id

  let pp_a_node ppf t =
    Format.fprintf ppf "%s %a" article pp_extension_node (to_string t)

  let pp_bad_empty_node ppf () = pp_extension_node ppf root
end

(******************************************************************************)
module Error = struct
  (** Someone used [[%extension.EXTNAME]] wrong *)
  type malformed_extension =
    | Has_payload of payload

  (** An error triggered when desugaring a language extension from an OCaml
      AST; should always be fatal *)
  type error =
    | Malformed_extension of Extension_node_name.t * malformed_extension
    | Unknown_extension of string
    | Disabled_extension of Language_extension.t
    | Wrong_syntactic_category of Language_extension.t * string
    | Unnamed_extension
    | Bad_introduction of Extension_node_name.t

  (** The exception type thrown when desugaring a language extension from an
      OCaml AST *)
  exception Error of Location.t * error
end

open Error

let assert_extension_enabled ~loc ext =
  if not (Language_extension.is_enabled ext) then
    raise (Error(loc, Disabled_extension ext))
;;

let report_error ~loc = function
  | Malformed_extension(ext_name, malformed) -> begin
      match malformed with
      | Has_payload _payload ->
          Location.errorf
            ~loc
            "@[Modular extension nodes are not allowed to have a payload,@ \
             but %a does@]"
          Extension_node_name.pp_quoted_name ext_name
    end
  | Unknown_extension name ->
      Location.errorf
        ~loc
        "@[Unknown extension \"%s\" referenced via@ %a extension node@]"
        name
        Extension_node_name.pp_a_node Extension_node_name.[name]
  | Disabled_extension ext ->
      Location.errorf
        ~loc
        "The extension \"%s\" is disabled and cannot be used"
        (Language_extension.to_string ext)
  | Wrong_syntactic_category(ext, cat) ->
      Location.errorf
        ~loc
        "The extension \"%s\" cannot appear in %s"
        (Language_extension.to_string ext)
        cat
  | Unnamed_extension ->
      Location.errorf
        ~loc
        "Cannot have an extension node named %a"
        Extension_node_name.pp_bad_empty_node ()
  | Bad_introduction(ext :: _ as ext_name) ->
      Location.errorf
        ~loc
        "@[The extension \"%s\" was referenced improperly; it started with@ %a \
         extension node,@ not %a one@]"
        ext
        Extension_node_name.pp_a_node ext_name
        Extension_node_name.pp_a_node Extension_node_name.[ext]

let () =
  Location.register_error_of_exn
    (function
      | Error(loc, err) -> Some (report_error ~loc err)
      | _ -> None)

(******************************************************************************)
(** Generically find and create the OCaml AST syntax used to encode one of our
    language extensions.  One module per variety of AST (expressions, patterns,
    etc.). *)

(** The parameters that define how to look for [[%extension.EXTNAME]] inside
    ASTs of a certain syntactic category.  See also the [Make_AST] functor,
    which uses these definitions to make the e.g. [Expression] module. *)
module type AST_parameters = sig
  (** The AST type (e.g., [Parsetree.expression]) *)
  type ast

  (** The "AST description" type, without the location and attributes (e.g.,
      [Parsetree.expression_desc]) *)
  type ast_desc

  (** The name for this syntactic category in the plural form; used for error
      messages *)
  val plural : string

  (** How to get the location attached to an AST node.  Should just be
      [fun tm -> tm.pCAT_loc] for the appropriate syntactic category [CAT]. *)
  val location : ast -> Location.t

  (** Turn an [ast_desc] into an [ast] by adding the appropriate metadata.  When
      creating [ast] nodes afresh for an extension, the location should be
      omitted; in this case, it will default to [!Ast_helper.default_loc], which
      should be [ghost]. *)
  val wrap_desc :
    ?loc:Location.t -> attrs:Parsetree.attributes -> ast_desc -> ast

  (** How to construct an extension node for this AST (something of the shape
      [[%name]] or [[%%name]], depending on the AST).  Should just be
      [Ast_helper.CAT.extension] for the appropriate syntactic category [CAT].
      (This means that [?loc] should default to [!Ast_helper.default_loc.].) *)
  val make_extension_node :
    ?loc:Location.t -> ?attrs:attributes -> extension -> ast

  (** Given an extension node (as created by [make_extension_node]) with an
      appropriately-formed name and a body, combine them into the special
      syntactic form we use for language extensions for this syntactic
      category.  Partial inverse of [match_extension_use]. *)
  val make_extension_use  : extension_node:ast -> ast -> ast_desc

  (** Given an AST node, check if it's of the special syntactic form indicating
      that this is a language extension (as created by [make_extension_node]),
      split it back up into the extension node and the possible body.
      Doesn't do any checking about the name/format of the extension or the
      possible body terms (see [AST.match_extension]).  Partial inverse of
      [make_extension_use]. *)
  val match_extension_use : ast -> (extension * ast) option
end

module type AST = sig
  type ast

  type ast_desc

  val plural : string

  val location : ast -> Location.t

  val wrap_desc :
    ?loc:Location.t -> attrs:Parsetree.attributes -> ast_desc -> ast

  val make_extension : Extension_node_name.t -> ast -> ast_desc

  val make_entire_extension :
    loc:Location.t -> string -> (unit -> ast) -> ast_desc

  val match_extension : ast -> (Extension_node_name.t * ast) option
end

(* Some extensions written before this file existed are handled in their own
   way; this function filters them out. *)
let uniformly_handled_extension name =
  match name with
  | "local"|"global"|"nonlocal"|"escape"|"include_functor"|"curry" -> false
  | _ -> true

(** Given the [AST_parameters] for a syntactic category, produce the
    corresponding module, of type [AST], for lowering and lifting language
    extension syntax from and to it. *)
module Make_AST (AST_parameters : AST_parameters) :
    AST with type ast      = AST_parameters.ast
         and type ast_desc = AST_parameters.ast_desc =
  struct
    include AST_parameters

    let make_extension ext_name =
      make_extension_use
        ~extension_node:
          (make_extension_node
             ({ txt = Extension_node_name.to_string ext_name
              ; loc = !Ast_helper.default_loc },
              PStr []))

    let make_entire_extension ~loc name ast =
      make_extension [name]
        (Ast_helper.with_default_loc (Location.ghostify loc) ast)

    (* This raises an error if the language extension node is malformed.
       Malformed means either:

       1. The [[%extension.NAME]] extension point has a payload; extensions must
          be empty, so other ppxes can traverse "into" them.

       2. The [[%extension.NAME]] extension point contains
          body forms that are "shaped" incorrectly. *)
    let match_extension ast =
      match match_extension_use ast with
      | Some (({txt = ext_name; loc = ext_loc}, ext_payload), body) ->
        begin
          let raise_error err = raise (Error(ext_loc, err)) in
          match Extension_node_name.of_string ext_name with
          | Some (Ok (ext :: _ as ext_name))
            when uniformly_handled_extension ext -> begin
              let raise_malformed err =
                raise_error (Malformed_extension(ext_name, err))
              in
              match ext_payload with
              | PStr [] -> Some (ext_name, body)
              | _ -> raise_malformed (Has_payload ext_payload)
            end
          | Some (Error ()) -> raise_error Unnamed_extension
          | Some (Ok (_ :: _)) | None -> None
        end
      | None -> None
end

(** Expressions; embedded as [([%extension.EXTNAME] BODY)]. *)
module Expression = Make_AST(struct
  type ast = expression
  type ast_desc = expression_desc

  let plural = "expressions"

  let location expr = expr.pexp_loc

  let wrap_desc ?loc ~attrs = Ast_helper.Exp.mk ?loc ~attrs

  let make_extension_node = Ast_helper.Exp.extension

  let make_extension_use ~extension_node expr =
    Pexp_apply(extension_node, [Nolabel, expr])

  let match_extension_use expr =
    match expr.pexp_desc with
    | Pexp_apply({pexp_desc = Pexp_extension ext; _},
                 [Asttypes.Nolabel, body]) ->
       Some (ext, body)
    | _ ->
       None
end)

(** Patterns; embedded as [[%extension.EXTNAME], BODY]. *)
module Pattern = Make_AST(struct
  type ast = pattern
  type ast_desc = pattern_desc

  let plural = "patterns"

  let location pat = pat.ppat_loc

  let wrap_desc ?loc ~attrs = Ast_helper.Pat.mk ?loc ~attrs

  let make_extension_node = Ast_helper.Pat.extension

  let make_extension_use ~extension_node pat =
    Ppat_tuple [extension_node; pat]

  let match_extension_use pat =
    match pat.ppat_desc with
    | Ppat_tuple([{ppat_desc = Ppat_extension ext; _}; pattern]) ->
        Some (ext, pattern)
    | _ ->
       None
end)

(** Module types; embedded as [functor (_ : [%extension.EXTNAME]) -> BODY]. *)
module Module_type = Make_AST(struct
    type ast = module_type
    type ast_desc = module_type_desc

    let plural = "module types"

    let location mty = mty.pmty_loc

    let wrap_desc ?loc ~attrs = Ast_helper.Mty.mk ?loc ~attrs

    let make_extension_node = Ast_helper.Mty.extension

    let make_extension_use ~extension_node mty =
      Pmty_functor(Named(Location.mknoloc None, extension_node), mty)

    let match_extension_use mty =
      match mty.pmty_desc with
      | Pmty_functor(Named({txt = None},
                           {pmty_desc = Pmty_extension ext}), mty) ->
        Some (ext, mty)
      | _ -> None
end)

(******************************************************************************)
(** Generically lift and lower our custom language extension ASTs from/to OCaml
    ASTs. *)

module type Of_ast_parameters = sig
  module AST : AST
  type t
  val of_ast_internal : Language_extension.t -> AST.ast -> t option
end

module Make_of_ast (Params : Of_ast_parameters) : sig
  val of_ast : Params.AST.ast -> Params.t option
end = struct
  let of_ast ast =
    let loc = Params.AST.location ast in
    let raise_error err = raise (Error (loc, err)) in
    match Params.AST.match_extension ast with
    | None -> None
    | Some ([name], ast) -> begin
        match Language_extension.of_string name with
        | Some ext -> begin
            assert_extension_enabled ~loc ext;
            match Params.of_ast_internal ext ast with
            | Some ext_ast -> Some ext_ast
            | None ->
                raise_error (Wrong_syntactic_category(ext, Params.AST.plural))
          end
        | None -> raise_error (Unknown_extension name)
      end
    | Some (_ :: _ :: _ as ext_name, _) ->
        raise_error (Bad_introduction(ext_name))
end
