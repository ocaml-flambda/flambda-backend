open Asttypes
open Parsetree
open Extensions_parsing

(******************************************************************************)
(** Individual language extension modules *)

(* Note [Check for immutable extension in comprehensions code]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   When we spot a comprehension for an immutable array, we need to make sure
   that both [comprehensions] and [immutable_arrays] are enabled.  But our
   general mechanism for checking for enabled extensions (in
   Extensions_parsing.Translate(...).of_ast) won't work well here: it triggers
   when converting from e.g. [[%extensions.comprehensions.array] ...]  to the
   comprehensions-specific AST. But if we spot a
   [[%extensions.comprehensions.immutable]], there is no expression to
   translate.  So we just check for the immutable arrays extension when
   processing a comprehension expression for an immutable array.

   Note [Wrapping with make_extension]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The topmost node in the encoded AST must always look like e.g.
   [%extension.comprehensions]. This allows the decoding machinery to know
   what extension is being used and what function to call to do the decoding.
   Accordingly, during encoding, after doing the hard work of converting the
   extension syntax tree into e.g. Parsetree.expression, we need to make a final
   step of wrapping the result in an [%extension.xyz] node. Ideally, this step
   would be done by part of our general structure, like we separate [of_ast]
   and [of_ast_internal] in the decode structure; this design would make it
   structurally impossible/hard to forget taking this final step.

   However, the final step is only one line of code (a call to
   [make_extension]), but yet the name of the extension varies, as does the type
   of the payload. It would thus take several lines of code to execute this
   command otherwise, along with dozens of lines to create the structure in the
   first place. And so instead we just manually call [make_extension] and refer
   to this Note as a reminder to authors of future extensions to remember to do
   this wrapping.
*)

(** List and array comprehensions *)
module Comprehensions = struct
  let extension_string = Language_extension.to_string Comprehensions

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
     [Expression.make_extension] (via [comprehension_expr]) as described at the
     top of [extensions_parsing.mli].

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

  let comprehension_expr ~loc names =
    Expression.make_extension ~loc (extension_string :: names)

  (** First, we define how to go from the nice AST to the OCaml AST; this is
      the [expr_of_...] family of expressions, culminating in
      [expr_of_comprehension_expr]. *)

  let expr_of_iterator ~loc = function
    | Range { start; stop; direction } ->
        comprehension_expr
          ~loc
          [ "for"
          ; "range"
          ; match direction with
            | Upto   -> "upto"
            | Downto -> "downto" ]
          (Ast_helper.Exp.tuple [start; stop])
    | In seq ->
        comprehension_expr ~loc ["for"; "in"] seq

  let expr_of_clause_binding ~loc { pattern; iterator; attributes } =
    Ast_helper.Vb.mk
      ~loc
      ~attrs:attributes
      pattern
      (expr_of_iterator ~loc iterator)

  let expr_of_clause ~loc clause rest = match clause with
    | For iterators ->
        comprehension_expr
          ~loc
          ["for"]
          (Ast_helper.Exp.let_
             Nonrecursive
             (List.map (expr_of_clause_binding ~loc) iterators)
             rest)
    | When cond ->
        comprehension_expr
          ~loc
          ["when"]
          (Ast_helper.Exp.sequence cond rest)

  let expr_of_comprehension ~loc ~type_ { body; clauses } =
    comprehension_expr
      ~loc
      type_
      (List.fold_right
         (expr_of_clause ~loc)
         clauses
         (comprehension_expr ~loc ["body"] body))

  let expr_of ~loc eexpr =
    let ghost_loc = { loc with Location.loc_ghost = true } in
    let expr_of_comprehension_type type_ =
      expr_of_comprehension ~loc:ghost_loc ~type_
    in
    (* See Note [Wrapping with make_extension] *)
    Expression.make_extension ~loc [extension_string] @@
    match eexpr with
    | Cexp_list_comprehension comp ->
        expr_of_comprehension_type ["list"]  comp
    | Cexp_array_comprehension (amut, comp) ->
        expr_of_comprehension_type
          [ "array"
          ; match amut with
            | Mutable   ->
                "mutable"
            | Immutable ->
                "immutable"
          ]
          comp

  (** Then, we define how to go from the OCaml AST to the nice AST; this is
      the [..._of_expr] family of expressions, culminating in
      [comprehension_expr_of_expr]. *)

  module Desugaring_error = struct
    type error =
      | Non_comprehension_extension_point of string list
      | Non_extension
      | Bad_comprehension_extension_point of string list
      | No_clauses

    let report_error ~loc = function
      | Non_comprehension_extension_point name ->
          Location.errorf ~loc
            "Tried to desugar the non-comprehension extension point \
             \"extension.%s\" as part of a comprehension expression"
            (String.concat "." name)
      | Non_extension ->
          Location.errorf ~loc
            "Tried to desugar a non-extension expression as part of a \
             comprehension expression"
      | Bad_comprehension_extension_point name ->
          Location.errorf ~loc
            "Unknown, unexpected, or malformed comprehension extension point \
             \"extension.comprehension.%s\""
            (String.concat "." name)
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

  let expand_comprehension_extension_expr expr =
    match Expression.match_extension expr with
    | Some (comprehensions :: name, expr)
      when String.equal comprehensions extension_string ->
        name, expr
    | Some (name, _) ->
        Desugaring_error.raise expr (Non_comprehension_extension_point name)
    | None ->
        Desugaring_error.raise expr Non_extension

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
        Desugaring_error.raise expr (Bad_comprehension_extension_point bad)

  let clause_binding_of_vb { pvb_pat; pvb_expr; pvb_attributes; pvb_loc = _ } =
    { pattern = pvb_pat
    ; iterator = iterator_of_expr pvb_expr
    ; attributes = pvb_attributes }

  let add_clause clause comp = { comp with clauses = clause :: comp.clauses }

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
        Desugaring_error.raise expr (Bad_comprehension_extension_point bad)

  let comprehension_of_expr expr =
    match raw_comprehension_of_expr expr with
    | { body = _; clauses = [] } ->
        Desugaring_error.raise expr No_clauses
    | comp -> comp

  let comprehension_expr_of_expr expr =
    match expand_comprehension_extension_expr expr with
    | ["list"], comp ->
        Cexp_list_comprehension (comprehension_of_expr comp)
    | ["array"; "mutable"], comp ->
        Cexp_array_comprehension (Mutable, comprehension_of_expr comp)
    | ["array"; "immutable"], comp ->
        (* assert_extension_enabled:
           See Note [Check for immutable extension in comprehensions code] *)
        assert_extension_enabled ~loc:expr.pexp_loc Immutable_arrays;
        Cexp_array_comprehension (Immutable, comprehension_of_expr comp)
    | bad, _ ->
        Desugaring_error.raise expr (Bad_comprehension_extension_point bad)
end

(** Immutable arrays *)
module Immutable_arrays = struct
  type nonrec expression =
    | Iaexp_immutable_array of expression list

  type nonrec pattern =
    | Iapat_immutable_array of pattern list

  let extension_string = Language_extension.to_string Immutable_arrays

  let expr_of ~loc = function
    | Iaexp_immutable_array elts ->
      (* See Note [Wrapping with make_extension] *)
      Expression.make_extension ~loc [extension_string] @@
      Ast_helper.Exp.array ~loc elts

  let of_expr expr = match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc = function
    | Iapat_immutable_array elts ->
      (* See Note [Wrapping with make_extension] *)
      Pattern.make_extension ~loc [extension_string] @@
      Ast_helper.Pat.array ~loc elts

  let of_pat expr = match expr.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"
end

(******************************************************************************)
(** The interface to language extensions, which we export *)

module type AST = sig
  type t
  type ast

  val of_ast : ast -> t option
end

module Expression = struct
  module M = struct
    module AST = Extensions_parsing.Expression

    type t =
      | Eexp_comprehension   of Comprehensions.expression
      | Eexp_immutable_array of Immutable_arrays.expression

    let of_ast_internal (ext : Language_extension.t) expr = match ext with
      | Comprehensions ->
        Some (Eexp_comprehension (Comprehensions.comprehension_expr_of_expr expr))
      | Immutable_arrays ->
        Some (Eexp_immutable_array (Immutable_arrays.of_expr expr))
      | _ -> None
  end

  include M
  include Make_of_ast(M)
end

module Pattern = struct
  module M = struct
    module AST = Extensions_parsing.Pattern

    type t =
      | Epat_immutable_array of Immutable_arrays.pattern

    let of_ast_internal (ext : Language_extension.t) pat = match ext with
      | Immutable_arrays ->
        Some (Epat_immutable_array (Immutable_arrays.of_pat pat))
      | _ -> None
  end

  include M
  include Make_of_ast(M)
end
