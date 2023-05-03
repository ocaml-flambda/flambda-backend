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
module Error = struct
  type malformed_extension =
    | Has_payload of payload

  type error =
    | Malformed_extension of string list * malformed_extension
    | Unknown_extension of string
    | Disabled_extension of Language_extension.t
    | Wrong_syntactic_category of Language_extension.t * string
    | Unnamed_extension
    | Bad_introduction of string * string list

  exception Error of Location.t * error
end

open Error

let assert_extension_enabled ~loc ext =
  if not (Language_extension.is_enabled ext) then
    raise (Error(loc, Disabled_extension ext))
;;

let report_error ~loc = function
  | Malformed_extension(name, malformed) -> begin
      let name = String.concat "." ("extension" :: name) in
      match malformed with
      | Has_payload _payload ->
          Location.errorf
            ~loc
            "@[Modular extension nodes are not allowed to have a payload,@ \
             but \"%s\" does@]"
          name
    end
  | Unknown_extension name ->
      Location.errorf
        ~loc
        "@[Unknown extension \"%s\" referenced via an@ [%%extension.%s] \
         extension node@]"
        name
        name
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
        "Cannot have an extension node named [%%extension]"
  | Bad_introduction(name, subnames) ->
      Location.errorf
        ~loc
        "@[The extension \"%s\" was referenced improperly; it started with an@ \
         [%%extension.%s] extension node,@ not an [%%extension.%s] one@]"
        name
        (String.concat "." (name :: subnames))
        name

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

  (** Turn an [ast_desc] into an [ast] by adding the appropriate metadata *)
  val wrap_desc :
    loc:Location.t -> attrs:Parsetree.attributes -> ast_desc -> ast

  (** How to construct an extension node for this AST (something of the shape
      [[%name]] or [[%%name]], depending on the AST).  Should just be
      [Ast_helper.CAT.extension] for the appropriate syntactic category
      [CAT]. *)
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
    loc:Location.t -> attrs:Parsetree.attributes -> ast_desc -> ast

  val make_extension : loc:Location.t -> string list -> ast -> ast_desc

  val match_extension : ast -> (string list * ast) option
end

(* Some extensions written before this file existed are handled in their own
   way; this function filters them out. *)
let uniformly_handled_extension names =
  match names with
  | [("local"|"global"|"nonlocal"|"escape"|"include_functor"|"curry")] -> false
  | _ -> true

(** Given the [AST_parameters] for a syntactic category, produce the
    corresponding module, of type [AST], for lowering and lifting language
    extension syntax from and to it. *)
module Make_AST (AST_parameters : AST_parameters) :
    AST with type ast      = AST_parameters.ast
         and type ast_desc = AST_parameters.ast_desc =
  struct
    include AST_parameters

    let make_extension ~loc names =
      let loc = Location.ghostify loc in
      make_extension_use
        ~extension_node:
          (make_extension_node
             ~loc
             ({ txt = String.concat "." ("extension" :: names); loc }, PStr []))

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
          match String.split_on_char '.' ext_name with
          | "extension" :: names when uniformly_handled_extension names -> begin
              let raise_malformed err =
                raise (Error(ext_loc, Malformed_extension(names, err)))
              in
              match ext_payload with
              | PStr [] -> Some (names, body)
              | _ -> raise_malformed (Has_payload ext_payload)
            end
          | _ -> None
        end
      | None -> None
end

(** Expressions; embedded as [([%extension.EXTNAME] BODY)]. *)
module Expression = Make_AST(struct
  type ast = expression
  type ast_desc = expression_desc

  let plural = "expressions"

  let location expr = expr.pexp_loc

  let wrap_desc ~loc ~attrs = Ast_helper.Exp.mk ~loc ~attrs

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

  let wrap_desc ~loc ~attrs = Ast_helper.Pat.mk ~loc ~attrs

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

    let wrap_desc ~loc ~attrs = Ast_helper.Mty.mk ~loc ~attrs

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
    | Some ([], _) ->
        raise_error Unnamed_extension
    | Some (name :: subnames, _) ->
        raise_error (Bad_introduction(name, subnames))
end
