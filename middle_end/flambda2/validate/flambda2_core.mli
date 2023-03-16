module P = Flambda_primitive

(** Simplified core of [flambda2] terms **)
(* (1) Simple.t -> core_exp for [Apply*] expressions
   (2) Ignore [Num_occurrences] (which is used for making inlining decisions)
   (3) Ignored traps for now *)

type core_exp =
  | Named of named
  | Let of let_expr
  | Let_cont of let_cont_expr
  | Apply of apply_expr
  | Apply_cont of apply_cont_expr
  | Lambda of lambda_expr
  | Switch of switch_expr
  | Invalid of { message : string }

and lambda_expr =
  (Bound_for_lambda.t, core_exp) Name_abstraction.t

and 'a id_or_exp =
  | Id of 'a
  | Exp of core_exp

and 'a id_or_cont =
  | Cont_id of 'a
  | Handler of continuation_handler

(** Let expressions [let x = e1 in e2]

   [fun x -> e2] = let_abst
   [e1] = body **)
and let_expr =
  { let_abst : (Bound_for_let.t, core_exp) Name_abstraction.t;
    expr_body : core_exp; }

and named =
  | Simple of Simple.t
  | Prim of primitive
  | Slot of (Variable.t * slot)
  | Closure_expr of (Variable.t * Function_slot.t * set_of_closures)
  | Set_of_closures of set_of_closures
  | Static_consts of static_const_group
  | Rec_info of Rec_info_expr.t

and slot =
  | Function_slot of Function_slot.t
  | Value_slot of Value_slot.t

and set_of_closures =
  { function_decls : function_declarations;
    value_slots : value_expr Value_slot.Map.t;
    alloc_mode : Alloc_mode.For_allocations.t }

and function_declarations =
  { funs : function_expr Function_slot.Map.t;
    in_order : function_expr Function_slot.Lmap.t}

and value_expr = Simple.t id_or_exp

and function_expr = Code_id.t id_or_exp

and primitive =
  | Nullary of P.nullary_primitive
  | Unary of P.unary_primitive * core_exp
  | Binary of P.binary_primitive * core_exp * core_exp
  | Ternary of P.ternary_primitive * core_exp * core_exp * core_exp
  | Variadic of P.variadic_primitive * core_exp list

(* Named lambda expression that has an additional closure parameter *)
and function_params_and_body =
  { expr: (Bound_var.t, lambda_expr) Name_abstraction.t;
    anon: bool }

and static_const_or_code =
  | Code of function_params_and_body
  | Deleted_code
  | Static_const of static_const

and static_const_group = static_const_or_code list

and static_const =
  | Static_set_of_closures of set_of_closures
  | Block of Tag.Scannable.t * Mutability.t * core_exp list
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Immutable_float_block of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_value_array of Field_of_static_block.t list
  | Empty_array
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

and let_cont_expr =
  (* Non-recursive case [e1 where k x = e2]

     [fun x -> e2] = handler
     bound variable [k] = Bound_continuation.t
     [e1] = body (has bound variable [k] in scope) *)
  | Non_recursive of
    { handler : continuation_handler;
      body : (Bound_continuation.t, core_exp) Name_abstraction.t;}

  (* Recursive case, we have a set of (mutually recursive) continuations
     [let rec K x in e] where [K] is a map of continuations
     [x] is the set of invariant parameters
     bound variable [K] is in the scope of [e]

     [x] = invariant_params (Bound_parameters.t)
     [K] = continuation_map
     [e] = body *)
  | Recursive of
      (Bound_continuations.t, recursive_let_expr) Name_abstraction.t

and recursive_let_expr =
  { continuation_map :
      (Bound_parameters.t, continuation_handler_map) Name_abstraction.t;
    body : core_exp; }

and continuation_handler_map =
  continuation_handler Continuation.Map.t

and continuation_handler =
  (Bound_parameters.t, core_exp) Name_abstraction.t

and apply_expr =
  { callee: core_exp;
    continuation: continuation_expr;
    exn_continuation: exn_continuation_expr;
    apply_args: core_exp list; }

and continuation_expr = Apply_expr.Result_continuation.t id_or_cont

and exn_continuation_expr = Continuation.t id_or_cont

and apply_cont_expr =
  { k : Continuation.t;
    args : core_exp list }

and switch_expr =
  { scrutinee : core_exp;
    arms : core_exp Targetint_31_63.Map.t }

type simple_type =
  | Var of Variable.t
  | Symbol of Symbol.t
  | Naked_immediate of Targetint_31_63.t
  | Tagged_immediate of Targetint_31_63.t
  | Naked_float of Numeric_types.Float_by_bit_pattern.t
  | Naked_int32 of Int32.t
  | Naked_int64 of Int64.t
  | Naked_nativeint of Targetint_32_64.t

val simple_with_type : Simple.t -> simple_type

val is_static_set_of_closures : static_const_or_code -> bool

val is_code : static_const_or_code -> bool

val must_be_named : core_exp -> named option

val must_be_static_consts : core_exp -> static_const_group option

val must_be_code : core_exp -> function_params_and_body option

val must_be_simple : core_exp -> Simple.t option

val must_be_simple_or_immediate : core_exp -> Simple.t option

val must_be_tagged_immediate : core_exp -> named option

val must_be_untagged_immediate : core_exp -> named option

val must_be_function_slot_expr :
  core_exp -> (Variable.t * Function_slot.t) option

val must_be_set_of_closures : core_exp -> set_of_closures option

val must_be_static_set_of_closures : static_const -> set_of_closures option

val must_have_closure : core_exp -> set_of_closures option

val fmap_id_or_exp :
  'a id_or_exp -> ('a -> 'b) -> (core_exp -> core_exp) -> 'b id_or_exp

val merge_id_or_exp : 'a id_or_exp -> ('a -> 'b) -> (core_exp -> 'b) -> 'b

module T0 : sig
  type t = core_exp

  val apply_renaming : t -> Renaming.t -> t
  val ids_for_export : t -> Ids_for_export.t
end

module ContMap : sig
  type t = continuation_handler_map
  val apply_renaming : t -> Renaming.t -> t
  val ids_for_export : t -> Ids_for_export.t
end

module RecursiveLetExpr : sig
  type t = recursive_let_expr
  val apply_renaming : t -> Renaming.t -> t
  val ids_for_export : t -> Ids_for_export.t
end

module Core_let : sig
  val create : x:Bound_for_let.t -> e1:core_exp -> e2 :core_exp -> core_exp

  type t = let_expr

  module Pattern_match_pair_error : sig
    type t = Mismatched_let_bindings
  end

  val pattern_match :
    let_expr -> f:(x:Bound_for_let.t -> e1:core_exp -> e2:core_exp -> 'a) -> 'a

  val pattern_match_pair :
    t -> t -> (Bound_for_let.t -> core_exp -> core_exp -> 'a) ->
    (Bound_codelike.t -> Bound_codelike.t -> core_exp -> core_exp -> 'a) ->
    ('a, Pattern_match_pair_error.t) Result.t
end

module Core_continuation_handler : sig
  type t = continuation_handler
  val create : Bound_parameters.t -> core_exp -> t

  val pattern_match : t -> (Bound_parameters.t -> core_exp -> 'a) -> 'a
  val pattern_match_pair :
    t -> t -> (Bound_parameters.t -> core_exp -> core_exp -> 'a) -> 'a
end

module Core_letcont_body : sig
  type t = (Bound_continuation.t, core_exp) Name_abstraction.t

  val create : Bound_continuation.t -> core_exp -> t
  val pattern_match : t -> (Bound_continuation.t -> core_exp -> 'a) -> 'a
  val pattern_match_pair :
    t -> t -> (Bound_continuation.t -> core_exp -> core_exp -> 'a) -> 'a
end

module Core_recursive : sig

  type t = (Bound_continuations.t, recursive_let_expr) Name_abstraction.t

  val create : Bound_continuations.t -> recursive_let_expr -> t

  val pattern_match :
    t ->
    f:(Bound_continuations.t -> recursive_let_expr -> 'a) -> 'a

  val pattern_match_pair :
    t -> t ->
    (Bound_parameters.t ->
     core_exp ->
     core_exp -> continuation_handler_map -> continuation_handler_map -> 'a) -> 'a
end

module Core_lambda : sig
  type t = lambda_expr

  val create : Bound_for_lambda.t -> T0.t -> t

  val pattern_match :
    t -> f:(Bound_for_lambda.t -> T0.t -> 'a) -> 'a

  val pattern_match_pair:
    t -> t ->
    f:(return_continuation:Continuation.t ->
       exn_continuation:Continuation.t ->
       Bound_parameters.t -> core_exp -> core_exp -> 'a) -> 'a
end

module Core_function_params_and_body : sig
  type t = (Bound_var.t, Core_lambda.t) Name_abstraction.t

  val create : Bound_var.t -> Core_lambda.t -> t

  val my_closure : t -> Bound_var.t

  val lambda_expr : t -> Core_lambda.t

  val pattern_match :
    t -> f:(Bound_var.t -> Core_lambda.t -> 'a) -> 'a

  val pattern_match_pair :
    t -> t -> f:(return_continuation:Continuation.t ->
      exn_continuation:Continuation.t ->
      Bound_parameters.t ->
      body1:core_exp -> body2:core_exp ->
      my_closure:Bound_var.t -> 'a)
    -> 'a
end

module Core_continuation_map : sig
  type t = (Bound_parameters.t, continuation_handler_map) Name_abstraction.t
  val create : Bound_parameters.t -> continuation_handler_map -> t
  val pattern_match :
    t -> f:(Bound_parameters.t -> continuation_handler_map -> 'a) -> 'a
end

val print : Format.formatter -> core_exp -> unit
val print_static_pattern : Format.formatter -> Bound_codelike.Pattern.t -> unit
val print_prim : Format.formatter -> primitive -> unit
val print_bound_pattern : Format.formatter -> Bound_for_let.t -> unit

val apply_renaming : core_exp -> Renaming.t -> core_exp

val function_decl_create : function_expr Function_slot.Lmap.t -> function_declarations

val core_fmap :
  ('a -> Simple.t -> core_exp) ->
  (Apply_expr.Result_continuation.t -> continuation_expr) ->
  (Continuation.t -> exn_continuation_expr) ->
  'a -> core_exp -> core_exp

(* Fixpoint functions for core expressions *)
val let_fix : (core_exp -> core_exp) -> let_expr -> core_exp
val let_cont_fix : (core_exp -> core_exp) -> let_cont_expr -> core_exp
val apply_fix : (core_exp -> core_exp) ->
  (Apply_expr.Result_continuation.t -> continuation_expr) ->
  (Continuation.t -> exn_continuation_expr) ->
  apply_expr -> core_exp
val apply_cont_fix : (core_exp -> core_exp) -> apply_cont_expr -> core_exp
val apply_cont_fix' :
  (core_exp -> core_exp) ->
  (Continuation.t -> Continuation.t) ->
  apply_cont_expr -> core_exp
val lambda_fix : (core_exp -> core_exp) -> lambda_expr -> core_exp
val switch_fix : (core_exp -> core_exp) -> switch_expr -> core_exp

val named_fix :
  (core_exp -> core_exp) ->
  ('a -> Simple.t -> core_exp) -> 'a -> named -> core_exp
val set_of_closures_fix :
  (core_exp -> core_exp) ->
  ('a -> Simple.t -> core_exp) -> 'a -> set_of_closures -> set_of_closures
val prim_fix : (core_exp -> core_exp) -> primitive -> core_exp
val static_const_group_fix :
  (core_exp -> core_exp) ->
  ('a -> Simple.t -> core_exp) -> 'a -> static_const_group -> core_exp
val static_const_group_fix' :
  (core_exp -> core_exp) ->
  ('a -> Simple.t -> core_exp) ->
  ('a -> Code_id.t -> function_expr) ->
  'a -> static_const_group -> core_exp
