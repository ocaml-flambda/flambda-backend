open Asttypes
open Parsetree
open Extensions_parsing

(******************************************************************************)
(** Individual language extension modules *)

(** List and array comprehensions *)
module Comprehensions = struct
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

  type comprehension_expr =
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

  (** Because we construct a lot of subexpressions, we save the name here *)
  let extension_name = Clflags.Extension.to_string Comprehensions

  let comprehension_expr ~loc names =
    Expression.make_extension ~loc (extension_name :: names)

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

  let expr_of_comprehension_expr ~loc eexpr =
    let ghost_loc = { loc with Location.loc_ghost = true } in
    let expr_of_comprehension_type type_ =
      expr_of_comprehension ~loc:ghost_loc ~type_
    in
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
                assert_extension_enabled ~loc Immutable_arrays;
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
      when String.equal comprehensions extension_name ->
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

  let expr_of ~loc = function
    | Iaexp_immutable_array elts -> Ast_helper.Exp.array ~loc elts

  let of_expr expr = match expr.pexp_desc with
    | Pexp_array elts -> Iaexp_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"

  let pat_of ~loc = function
    | Iapat_immutable_array elts -> Ast_helper.Pat.array ~loc elts

  let of_pat expr = match expr.ppat_desc with
    | Ppat_array elts -> Iapat_immutable_array elts
    | _ -> failwith "Malformed immutable array expression"
end

(** We put our grouped ASTs in modules so that we can export them later;
    however, we need to extend these modules later, so we have to give these
    modules backup names, and we drop the [Ext] for export. *)

module Ext_expression = struct
  type t =
    | Eexp_comprehension   of Comprehensions.comprehension_expr
    | Eexp_immutable_array of Immutable_arrays.expression
end

module Ext_pattern = struct
  type t =
    | Epat_immutable_array of Immutable_arrays.pattern
end

(** How a single extension lifts and lowers its terms from and to the
    corresponding OCaml AST type, for every syntactic category at once; at least
    one of the fields should be [Supported].  We're adding fields (syntactic
    categories) to this type as needed. *)
type extension =
  { expression : (expression, Ext_expression.t) optional_ast_extension
  ; pattern    : (pattern,    Ext_pattern.t)    optional_ast_extension
  }

(** Construct an [extension].  We use optional arguments here because there are
   (or have the potential to be) a lot of syntactic categories and we really
   don't want to have to say [Unsupported] for all of them. *)
(* CR aspectorzabusky: Don't love this function name *)
let extension_embeddings ?expression ?pattern () =
  let of_option = function
    | Some ae -> Supported ae
    | None    -> Unsupported
  in
  { expression = of_option expression
  ; pattern    = of_option pattern }

(** Map a language extension name to its parsing [extension].  There are some
    extensions that we handle separately; these will raise an error if supplied
    here. *)
let extension : Clflags.Extension.t -> extension = function
  | Comprehensions ->
      extension_embeddings
        ~expression:{ ast_of = Comprehensions.expr_of_comprehension_expr
                    ; of_ast = Comprehensions.comprehension_expr_of_expr
                    ; wrap   = (fun cexp -> Eexp_comprehension cexp)
                    ; unwrap = (function | Eexp_comprehension cexp -> Some cexp
                                         | _                       -> None) }
        ()
  | Immutable_arrays ->
      extension_embeddings
        ~expression:{ ast_of = Immutable_arrays.expr_of
                    ; of_ast = Immutable_arrays.of_expr
                    ; wrap   = (fun iaexp -> Eexp_immutable_array iaexp)
                    ; unwrap = (function | Eexp_immutable_array iaexp -> Some iaexp
                                         | _                          -> None) }
        ~pattern:{ ast_of = Immutable_arrays.pat_of
                 ; of_ast = Immutable_arrays.of_pat
                 ; wrap   = (fun iapat -> Epat_immutable_array iapat)
                 ; unwrap = (function | Epat_immutable_array iapat -> Some iapat) }
        ()
  | (Local | Include_functor | Polymorphic_parameters) as ext ->
      (* These are the extensions that were written before this modular
         extensions machinery landed. *)
      Misc.fatal_errorf
        "The extension \"%s\" should be handled through its own mechanism, not \
         this uniform one."
        (Clflags.Extension.to_string ext)

(******************************************************************************)
(** Moving to and from OCaml ASTs *)

(** A type-indexed enumeration for selecting a specific syntactic category; see
    the argument to the [Translate] functor for details. *)
module Syntactic_category = struct
  (* One constructor per field of [extension] *)
  type ('ast, 'ext_ast) t =
    | Expression : (expression, Ext_expression.t) t
    | Pattern    : (pattern,    Ext_pattern.t)    t

  let ast_module (type ast ext_ast) (cat : (ast, ext_ast) t)
      : (module AST with type ast = ast) =
    match cat with
    | Expression -> (module Expression)
    | Pattern    -> (module Pattern)

  let ast_extension (type ast ext_ast) (cat : (ast, ext_ast) t) ext
      : (ast, ext_ast) optional_ast_extension =
    let ext = extension ext in
    match cat with
    | Expression -> ext.expression
    | Pattern    -> ext.pattern
end

(** See the [Translate] functor *)
include Translate(Syntactic_category)

(** Both translations at once *)
let extension_translations cat = extension_ast_of_ast cat, ast_of_extension_ast cat

(******************************************************************************)
(** The interface to language extensions, which we export; at this point we're
    willing to shadow the module (types) imported from [Extensions_parsing]. *)

module type AST = sig
  type t
  type ast

  val of_ast : ast -> t option
  val ast_of : loc:Location.t -> Clflags.Extension.t -> t -> ast
end

module Expression = struct
  include Ext_expression

  type ast = Parsetree.expression

  let of_ast, ast_of = extension_translations Expression
end

module Pattern = struct
  include Ext_pattern

  type ast = Parsetree.pattern

  let of_ast, ast_of = extension_translations Pattern
end
