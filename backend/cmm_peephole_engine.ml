(* Variable kinds *)

type _ pattern_kind =
| Expr : Cmm.expression pattern_kind
| Int : int pattern_kind
| Natint : Nativeint.t pattern_kind

type 'a pattern_var =
{ id : int;
  name : string;
  kind : 'a pattern_kind;
}

let var_counter = ref 0

let create_var kind name =
  incr var_counter;
  { id = !var_counter;
    name;
    kind;
  }

module IM = Numbers.Int.Map

module Env : sig
  type t
  val empty : t
  val add : t -> 'a pattern_var -> 'a -> t
  val find_exn : t -> 'a pattern_var -> 'a
  val register_phantom_let :
    t ->
    phantom_var: Backend_var.With_provenance.t ->
    defining_expr: Cmm.phantom_defining_expr option ->
    t
  val place_phantom_lets :
    t ->
    Cmm.expression ->
    Cmm.expression
end = struct
  type t = {
    exprs : Cmm.expression IM.t;
    ints : int IM.t;
    natints : Nativeint.t IM.t;
    phantom_lets_rev :
      (Backend_var.With_provenance.t
       * Cmm.phantom_defining_expr option) list;
  }
  let empty = {
    exprs = IM.empty;
    ints = IM.empty;
    natints = IM.empty;
    phantom_lets_rev = []
  }
  let add (type a) env (var : a pattern_var) (expr : a) =
    match var.kind with
    | Expr ->
        if IM.mem var.id env.exprs
        then Misc.fatal_errorf "Duplicate binding for var %s" var.name
        else { env with exprs = IM.add var.id expr env.exprs }
    | Int ->
        if IM.mem var.id env.ints
        then Misc.fatal_errorf "Duplicate binding for var %s" var.name
        else { env with ints = IM.add var.id expr env.ints }
    | Natint ->
        if IM.mem var.id env.natints
        then Misc.fatal_errorf "Duplicate binding for var %s" var.name
        else { env with natints = IM.add var.id expr env.natints }
  let find_exn (type a) env (var : a pattern_var) : a =
    match var.kind with
    | Expr -> IM.find var.id env.exprs
    | Int -> IM.find var.id env.ints
    | Natint -> IM.find var.id env.natints
  let register_phantom_let env ~phantom_var ~defining_expr =
    { env with phantom_lets_rev = (phantom_var, defining_expr) :: env.phantom_lets_rev }
  let place_phantom_lets env expr =
    List.fold_left (fun expr (phantom_var, defining_expr) ->
        Cmm.Cphantom_let (phantom_var, defining_expr, expr))
      expr env.phantom_lets_rev
end

type binop =
  | Add

type cmm_pattern =
  | Any of Cmm.expression pattern_var
  | Const_int_fixed of int
  | Const_int_var of int pattern_var
  | Const_natint_fixed of Nativeint.t
  | Const_natint_var of Nativeint.t pattern_var
  | Binop of binop * cmm_pattern * cmm_pattern
  | When of cmm_pattern * (Env.t -> bool)

type 'a clause =
  cmm_pattern * (Env.t -> 'a)

let matches_binop (binop : binop) (cop : Cmm.operation) =
  match binop, cop with
  | Add, Caddi -> true
  | _, _ -> false

let match_clauses_in_order clauses expr =
  let (let*) = Option.bind in
  let rec match_one_pattern env pat (expr : Cmm.expression) =
    match expr with
    | Cphantom_let (phantom_var, defining_expr, expr) ->
        let env = Env.register_phantom_let env ~phantom_var ~defining_expr in
        match_one_pattern env pat expr
    | _ -> begin
    match pat, expr with
    | Any v, expr -> Some (Env.add env v expr)
    | Const_int_fixed n1, Cconst_int (n2, _) ->
        if Int.equal n1 n2 then Some env else None
    | Const_int_var v, Cconst_int (n, _) ->
        Some (Env.add env v n)
    | Const_natint_fixed n1, Cconst_natint (n2, _) ->
        if Nativeint.equal n1 n2 then Some env else None
    | Const_natint_var v, Cconst_natint (n, _) ->
        Some (Env.add env v n)
    | Binop (binop, pat1, pat2), Cop (cop, [expr1; expr2], _) ->
        if matches_binop binop cop then
          let* env = match_one_pattern env pat1 expr1 in
          match_one_pattern env pat2 expr2
        else None
    | When (pat, guard), expr ->
        let* env = match_one_pattern env pat expr in
        if guard env then Some env else None
    | _, _ -> None
  end
  in
  let rec find_matching_clause expr = function
    | [] -> expr
    | (pat, f) :: clauses -> begin
        match match_one_pattern Env.empty pat expr with
        | Some env -> Env.place_phantom_lets env (f env)
        | None -> find_matching_clause expr clauses
      end
  in
  find_matching_clause expr clauses

let run expr clauses =
  match_clauses_in_order clauses expr

module Syntax = struct
  let (=>) lhs rhs = lhs, rhs
  let (#.) = Env.find_exn
end

module Default_variables = struct
  let c = create_var Expr "c"
  let c1 = create_var Expr "c1"
  let c2 = create_var Expr "c2"
  let i = create_var Int "i"
  let i1 = create_var Int "i1"
  let i2 = create_var Int "i2"
  let n = create_var Natint "n"
  let n1 = create_var Natint "n1"
  let n2 = create_var Natint "n2"
end
