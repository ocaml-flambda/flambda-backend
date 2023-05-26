open Asttypes
open Parsetree
open Jane_syntax_parsing

(******************************************************************************)
(** Individual language extension modules *)

(* Note [Check for immutable extension in comprehensions code]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   When we spot a comprehension for an immutable array, we need to make sure
   that both [comprehensions] and [immutable_arrays] are enabled.  But our
   general mechanism for checking for enabled extensions (in [of_ast]) won't
   work well here: it triggers when converting from
   e.g. [[%jane.comprehensions.array] ...]  to the comprehensions-specific
   AST. But if we spot a [[%jane.comprehensions.immutable]], there is no
   expression to translate.  So we just check for the immutable arrays extension
   when processing a comprehension expression for an immutable array.

   Note [Wrapping with make_entire_jane_syntax]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The topmost node in the encoded AST must always look like e.g.
   [%jane.comprehensions]. This allows the decoding machinery to know what
   extension is being used and what function to call to do the decoding.
   Accordingly, during encoding, after doing the hard work of converting the
   extension syntax tree into e.g. Parsetree.expression, we need to make a final
   step of wrapping the result in an [%jane.xyz] node. Ideally, this step would
   be done by part of our general structure, like we separate [of_ast] and
   [of_ast_internal] in the decode structure; this design would make it
   structurally impossible/hard to forget taking this final step.

   However, the final step is only one line of code (a call to
   [make_entire_jane_syntax]), but yet the name of the feature varies, as does
   the type of the payload. It would thus take several lines of code to execute
   this command otherwise, along with dozens of lines to create the structure in
   the first place. And so instead we just manually call
   [make_entire_jane_syntax] and refer to this Note as a reminder to authors of
   future syntax features to remember to do this wrapping.
*)

module With_attributes = With_attributes

let namespace language_extension
  : Jane_syntax_parsing.Embedded_name.Namespace.t
  =
  if Language_extension.is_erasable language_extension
  then Erasable
  else Non_erasable

(** List and array comprehensions *)
module Comprehensions = struct
  let extension : Language_extension.t = Comprehensions
  let extension_string = Language_extension.to_string extension
  let namespace = namespace extension

  type iterator =
    | Range of { start     : expression
               ; stop      : expression
               ; direction : direction_flag }
    | In of expression

  type clause_binding =
    { pattern    : pattern
    ; iterator   : iterator
    ; attributes : attribute list }

  type clause =
    | For of clause_binding list
    | When of expression

  type comprehension =
    { body    : expression
    ; clauses : clause list
    }

  type expression =
    | Cexp_list_comprehension  of comprehension
    | Cexp_array_comprehension of mutable_flag * comprehension

  (* The desugared-to-OCaml version of comprehensions is described by the
     following BNF, where [{% '...' | expr %}] refers to the result of
     [Expression.make_jane_syntax] (via [comprehension_expr]) as described at
     the top of [jane_syntax_parsing.mli].

     {v
         comprehension ::=
           | {% 'comprehension.list' | '[' clauses ']' %}
           | {% 'comprehension.array' | '[|' clauses '|]' %}

         clauses ::=
           | {% 'comprehension.for' | 'let' iterator+ 'in' clauses %}
           | {% 'comprehension.when' | expr ';' clauses %}
           | {% 'comprehension.body' | expr %}

         iterator ::=
           | pattern '=' {% 'comprehension.for.range.upto' | expr ',' expr %}
           | pattern '=' {% 'comprehension.for.range.downto' | expr ',' expr %}
           | pattern '=' {% 'comprehension.for.in' | expr %}
     v}
  *)

  let comprehension_expr names x =
    AST.wrap_desc Expression ~attrs:[] ~loc:x.pexp_loc @@
    AST.make_jane_syntax
      Expression
      { namespace; components = extension_string :: names }
      x

  (** First, we define how to go from the nice AST to the OCaml AST; this is
      the [expr_of_...] family of expressions, culminating in
      [expr_of_comprehension_expr]. *)

  let expr_of_iterator = function
    | Range { start; stop; direction } ->
        comprehension_expr
          [ "for"
          ; "range"
          ; match direction with
            | Upto   -> "upto"
            | Downto -> "downto" ]
          (Ast_helper.Exp.tuple [start; stop])
    | In seq ->
        comprehension_expr ["for"; "in"] seq

  let expr_of_clause_binding { pattern; iterator; attributes } =
    Ast_helper.Vb.mk ~attrs:attributes pattern (expr_of_iterator iterator)

  let expr_of_clause clause rest = match clause with
    | For iterators ->
        comprehension_expr
          ["for"]
          (Ast_helper.Exp.let_
             Nonrecursive (List.map expr_of_clause_binding iterators)
             rest)
    | When cond ->
        comprehension_expr ["when"] (Ast_helper.Exp.sequence cond rest)

  let expr_of_comprehension ~type_ { body; clauses } =
    (* We elect to wrap the body in a new AST node (here, [Pexp_lazy])
       because it makes it so there is no AST node that can carry multiple Jane
       Syntax-related attributes in addition to user-written attributes. This
       choice simplifies the definition of [comprehension_expr_of_expr], as
       part of its contract is threading through the user-written attributes
       on the outermost node.
    *)
    comprehension_expr
      type_
      (Ast_helper.Exp.lazy_
        (List.fold_right
          expr_of_clause
          clauses
          (comprehension_expr ["body"] body)))

  let expr_of ~loc cexpr =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    AST.make_entire_jane_syntax Expression ~loc namespace extension_string
      (fun () ->
        match cexpr with
        | Cexp_list_comprehension comp ->
            expr_of_comprehension ~type_:["list"] comp
        | Cexp_array_comprehension (amut, comp) ->
            expr_of_comprehension
              ~type_:[ "array"
                    ; match amut with
                      | Mutable   -> "mutable"
                      | Immutable -> "immutable"
                    ]
              comp)

  (** Then, we define how to go from the OCaml AST to the nice AST; this is
      the [..._of_expr] family of expressions, culminating in
      [comprehension_expr_of_expr]. *)

  module Desugaring_error = struct
    type error =
      | Non_comprehension_embedding of Embedded_name.t
      | Non_embedding
      | Bad_comprehension_embedding of string list
      | No_clauses

    let report_error ~loc = function
      | Non_comprehension_embedding name ->
          Location.errorf ~loc
            "Tried to desugar the non-comprehension embedded term %a@ \
             as part of a comprehension expression"
            Embedded_name.pp_quoted_name name
      | Non_embedding ->
          Location.errorf ~loc
            "Tried to desugar a non-embedded expression@ \
             as part of a comprehension expression"
      | Bad_comprehension_embedding subparts ->
          Location.errorf ~loc
            "Unknown, unexpected, or malformed@ comprehension embedded term %a"
            Embedded_name.pp_quoted_name
            Embedded_name.
              { namespace; components = extension_string :: subparts }
      | No_clauses ->
          Location.errorf ~loc
            "Tried to desugar a comprehension with no clauses"

    exception Error of Location.t * error

    let () =
      Location.register_error_of_exn
        (function
          | Error(loc, err) -> Some (report_error ~loc err)
          | _ -> None)

    let raise expr err = raise (Error(expr.pexp_loc, err))
  end

  (* Returns the expression node with the outermost Jane Syntax-related
     attribute removed. *)
  let expand_comprehension_extension_expr expr =
    match find_and_remove_jane_syntax_attribute expr.pexp_attributes with
    | Some
        ({ namespace = _; components = comprehensions :: names }, attributes)
      when String.equal comprehensions extension_string ->
        names, { expr with pexp_attributes = attributes }
    | Some (ext_name, _) ->
        Desugaring_error.raise expr (Non_comprehension_embedding ext_name)
    | None ->
        Desugaring_error.raise expr Non_embedding

  let iterator_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["for"; "range"; "upto"],
      { pexp_desc = Pexp_tuple [start; stop]; _ } ->
        Range { start; stop; direction = Upto }
    | ["for"; "range"; "downto"],
      { pexp_desc = Pexp_tuple [start; stop]; _ } ->
        Range { start; stop; direction = Downto }
    | ["for"; "in"], seq ->
        In seq
    | bad, _ ->
        Desugaring_error.raise expr (Bad_comprehension_embedding bad)

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat
    ; iterator = iterator_of_expr pvb_expr
    ; attributes = pvb_attributes }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

  let comprehension_of_expr =
    let rec raw_comprehension_of_expr expr =
      match expand_comprehension_extension_expr expr with
      | ["for"], { pexp_desc = Pexp_let(Nonrecursive, iterators, rest); _ } ->
          add_clause
            (For (List.map clause_binding_of_vb iterators))
            (raw_comprehension_of_expr rest)
      | ["when"], { pexp_desc = Pexp_sequence(cond, rest); _ } ->
          add_clause
            (When cond)
            (raw_comprehension_of_expr rest)
      | ["body"], body ->
          { body; clauses = [] }
      | bad, _ ->
          Desugaring_error.raise expr (Bad_comprehension_embedding bad)
    in
    fun expr ->
      match raw_comprehension_of_expr expr with
      | { body = _; clauses = [] } ->
          Desugaring_error.raise expr No_clauses
      | comp -> comp

  (* Returns remaining unconsumed attributes on outermost expression *)
  let comprehension_expr_of_expr expr =
    let name, wrapper = expand_comprehension_extension_expr expr in
    let comp =
      match name, wrapper.pexp_desc with
      | ["list"], Pexp_lazy comp ->
          Cexp_list_comprehension (comprehension_of_expr comp)
      | ["array"; "mutable"], Pexp_lazy comp ->
          Cexp_array_comprehension (Mutable, comprehension_of_expr comp)
      | ["array"; "immutable"], Pexp_lazy comp ->
          (* assert_extension_enabled:
            See Note [Check for immutable extension in comprehensions code] *)
          assert_extension_enabled ~loc:expr.pexp_loc Immutable_arrays;
          Cexp_array_comprehension (Immutable, comprehension_of_expr comp)
      | bad, _ ->
          Desugaring_error.raise expr (Bad_comprehension_embedding bad)
    in
    comp, wrapper.pexp_attributes
end

(** Immutable arrays *)
module Immutable_arrays = struct
  type nonrec expression =
    | Iaexp_immutable_array of expression list

  type nonrec pattern =
    | Iapat_immutable_array of pattern list

  let language_extension : Language_extension.t = Immutable_arrays
  let extension_string = Language_extension.to_string language_extension
  let namespace = namespace language_extension

  let expr_of ~loc = function
    | Iaexp_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      AST.make_entire_jane_syntax Expression ~loc namespace extension_string
        (fun () -> Ast_helper.Exp.array elts)

  (* Returns remaining unconsumed attributes *)
  let of_expr expr = match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts, expr.pexp_attributes
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc = function
    | Iapat_immutable_array elts ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      AST.make_entire_jane_syntax Pattern ~loc namespace extension_string
        (fun () -> Ast_helper.Pat.array elts)

  (* Returns remaining unconsumed attributes *)
  let of_pat pat = match pat.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts, pat.ppat_attributes
    | _ -> failwith "Malformed immutable array pattern"
end

(** [include functor] *)
module Include_functor = struct
  type signature_item =
    | Ifsig_include_functor of include_description

  type structure_item =
    | Ifstr_include_functor of include_declaration

  let extension : Language_extension.t = Include_functor
  let extension_string = Language_extension.to_string extension
  let namespace = namespace extension

  let sig_item_of ~loc = function
    | Ifsig_include_functor incl ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      AST.make_entire_jane_syntax Signature_item ~loc namespace extension_string
        (fun () -> Ast_helper.Sig.include_ incl)

  let of_sig_item sigi = match sigi.psig_desc with
    | Psig_include incl -> Ifsig_include_functor incl
    | _ -> failwith "Malformed [include functor] in signature"

  let str_item_of ~loc = function
    | Ifstr_include_functor incl ->
      (* See Note [Wrapping with make_entire_jane_syntax] *)
      AST.make_entire_jane_syntax Structure_item ~loc namespace extension_string
        (fun () -> Ast_helper.Str.include_ incl)

  let of_str_item stri = match stri.pstr_desc with
    | Pstr_include incl -> Ifstr_include_functor incl
    | _ -> failwith "Malformed [include functor] in structure"
end

(** Module strengthening *)
module Strengthen = struct
  type nonrec module_type =
    { mty : Parsetree.module_type; mod_id : Longident.t Location.loc }

  let extension : Language_extension.t = Module_strengthening
  let extension_string = Language_extension.to_string extension
  let namespace = namespace extension

  (* Encoding: [S with M] becomes [functor (_ : S) -> (module M)], where
     the [(module M)] is a [Pmty_alias].  This isn't syntax we can write, but
     [(module M)] can be the inferred type for [M], so this should be fine. *)

  let mty_of ~loc { mty; mod_id } =
    (* See Note [Wrapping with make_entire_jane_syntax] *)
    AST.make_entire_jane_syntax Module_type ~loc namespace extension_string
      (fun () -> Ast_helper.Mty.functor_ (Named (Location.mknoloc None, mty))
        (Ast_helper.Mty.alias mod_id))

  (* Returns remaining unconsumed attributes *)
  let of_mty mty = match mty.pmty_desc with
    | Pmty_functor(Named(_, mty), {pmty_desc = Pmty_alias mod_id}) ->
       { mty; mod_id }, mty.pmty_attributes
    | _ -> failwith "Malformed strengthened module type"
end

(******************************************************************************)
(** The interface to our novel syntax, which we export *)

module type AST = sig
  type t
  type ast

  val of_ast : ast -> t option
end

module Core_type = struct
  type t = |

  let of_ast_internal (feat : Feature.t) _typ = match feat with
    | _ -> None

  let of_ast = AST.make_of_ast Core_type ~of_ast_internal
end

module Constructor_argument = struct
  type t = |

  let of_ast_internal (feat : Feature.t) _carg = match feat with
    | _ -> None

  let of_ast = AST.make_of_ast Constructor_argument ~of_ast_internal
end

module Expression = struct
  type t =
    | Jexp_comprehension   of Comprehensions.expression
    | Jexp_immutable_array of Immutable_arrays.expression

  let of_ast_internal (feat : Feature.t) expr = match feat with
    | Language_extension Comprehensions ->
      let expr, attrs = Comprehensions.comprehension_expr_of_expr expr in
      Some (Jexp_comprehension expr, attrs)
    | Language_extension Immutable_arrays ->
      let expr, attrs = Immutable_arrays.of_expr expr in
      Some (Jexp_immutable_array expr, attrs)
    | _ -> None

  let of_ast = AST.make_of_ast Expression ~of_ast_internal
end

module Pattern = struct
  type t =
    | Jpat_immutable_array of Immutable_arrays.pattern

  let of_ast_internal (feat : Feature.t) pat = match feat with
    | Language_extension Immutable_arrays ->
      let expr, attrs = Immutable_arrays.of_pat pat in
      Some (Jpat_immutable_array expr, attrs)
    | _ -> None

  let of_ast = AST.make_of_ast Pattern ~of_ast_internal
end

module Module_type = struct
  type t =
    | Jmty_strengthen of Strengthen.module_type

  let of_ast_internal (feat : Feature.t) mty = match feat with
    | Language_extension Module_strengthening ->
      let mty, attrs = Strengthen.of_mty mty in
      Some (Jmty_strengthen mty, attrs)
    | _ -> None

  let of_ast = AST.make_of_ast Module_type ~of_ast_internal
end

module Signature_item = struct
  type t =
    | Jsig_include_functor of Include_functor.signature_item

  let of_ast_internal (feat : Feature.t) sigi =
    match feat with
    | Language_extension Include_functor ->
      Some (Jsig_include_functor (Include_functor.of_sig_item sigi))
    | _ -> None

  let of_ast = AST.make_of_ast Signature_item ~of_ast_internal
end

module Structure_item = struct
  type t =
    | Jstr_include_functor of Include_functor.structure_item

  let of_ast_internal (feat : Feature.t) stri =
    match feat with
    | Language_extension Include_functor ->
      Some (Jstr_include_functor (Include_functor.of_str_item stri))
    | _ -> None

  let of_ast = AST.make_of_ast Structure_item ~of_ast_internal
end
