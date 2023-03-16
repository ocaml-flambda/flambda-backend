open! Flambda2_core

let debug = ref true

let unequal (e1 : core_exp) (e2 : core_exp) =
  if !debug then
    Format.fprintf Format.std_formatter "Unequal:@ %a <>@ %a@."
      print e1
      print e2
  else ();
  false

(** Simple program context **)
(* LATER: Same structure used as [compare/compare.ml],
   might be useful to refactor the structure out of the file *)
module Env = struct
  type t =
    { mutable symbols : Symbol.t Symbol.Map.t;
      mutable code_ids : Code_id.t Code_id.Map.t;
      mutable function_slots : Function_slot.t Function_slot.Map.t;
      mutable function_slots_rev : Function_slot.t Function_slot.Map.t;
      mutable value_slots : Value_slot.t Value_slot.Map.t;
      mutable value_slots_rev : Value_slot.t Value_slot.Map.t
    }

  let create () =
    { symbols = Symbol.Map.empty;
      code_ids = Code_id.Map.empty;
      function_slots = Function_slot.Map.empty;
      function_slots_rev = Function_slot.Map.empty;
      value_slots = Value_slot.Map.empty;
      value_slots_rev = Value_slot.Map.empty }

  let add_symbol t symbol1 symbol2 =
    t.symbols <- Symbol.Map.add symbol1 symbol2 t.symbols

  let add_code_id t code_id1 code_id2 =
    t.code_ids <- Code_id.Map.add code_id1 code_id2 t.code_ids

  let add_function_slot t function_slot1 function_slot2 =
    t.function_slots
      <- Function_slot.Map.add function_slot1 function_slot2 t.function_slots;
    t.function_slots_rev
      <- Function_slot.Map.add function_slot2 function_slot1
           t.function_slots_rev

  let add_value_slot t value_slot1 value_slot2 =
    t.value_slots <- Value_slot.Map.add value_slot1 value_slot2 t.value_slots;
    t.value_slots_rev
      <- Value_slot.Map.add value_slot2 value_slot1 t.value_slots_rev

  let find_symbol t sym = Symbol.Map.find_opt sym t.symbols

  let find_code_id t code_id = Code_id.Map.find_opt code_id t.code_ids

  let find_function_slot t function_slot =
    Function_slot.Map.find_opt function_slot t.function_slots

  let find_function_slot_rev t function_slot =
    Function_slot.Map.find_opt function_slot t.function_slots_rev

  let find_value_slot t value_slot =
    Value_slot.Map.find_opt value_slot t.value_slots

  let find_value_slot_rev t value_slot =
    Value_slot.Map.find_opt value_slot t.value_slots_rev
end

(* Used for unification of environments while comparing function and value slots in
  [set_of_closures]. This is necessary because function and value slots do not have
  an explicit binding site. *)
let subst_symbol (env : Env.t) symbol =
  Env.find_symbol env symbol |> Option.value ~default:symbol

let subst_function_slot (env : Env.t) slot =
  Env.find_function_slot env slot |> Option.value ~default:slot

let subst_name env n =
  Name.pattern_match n
    ~var:(fun _ -> n)
    ~symbol:(fun s -> Name.symbol (subst_symbol env s))

let subst_simple env s =
  match s with
  | Id s ->
    Id (Simple.pattern_match s
      ~const:(fun _ -> s)
      ~name:(fun n ~coercion:_ -> Simple.name (subst_name env n)))
  | Exp _ -> s

let subst_code_id env code_id =
  Env.find_code_id env code_id |> Option.value ~default:code_id

let subst_function_expr env (fn_expr : function_expr) =
  match fn_expr with
  | Id id -> Id (subst_code_id env id)
  | Exp _ -> fn_expr

(** Equality between two programs given a context **)
(* For now, following a naive alpha-equivalence equality from [compare/compare]
    (without the discriminant) *)

(* The most naive equality type, a boolean *)
type eq = bool

let eq_string = string_of_bool

let equiv_symbols env sym1 sym2 : eq =
  let sym1 = subst_symbol env sym1 in
  Symbol.equal sym1 sym2

let equiv_names env name1 name2 : eq =
  let result =
    Name.pattern_match name1
      ~var:(fun var1 ->
        Name.pattern_match name2
          ~var:(fun var2 -> Variable.equal var1 var2)
          ~symbol:(fun _ -> false))
      ~symbol:(fun symbol1 ->
        Name.pattern_match name2
          ~var:(fun _ ->
            unequal
              (Named (Simple (Simple.name name1)))
              (Named (Simple (Simple.name name2))))
          ~symbol:(fun symbol2 -> equiv_symbols env symbol1 symbol2))
  in
  if result then result
  else
    (Format.fprintf Format.std_formatter "@.%a <> %a@."
      Name.print name1
      Name.print name2;
     false)

let equiv_value_slots env value_slot1 value_slot2 : eq =
  match Env.find_value_slot env value_slot1 with
  | Some value_slot ->
    Value_slot.equal value_slot value_slot2
  | None ->
      match Env.find_value_slot_rev env value_slot2 with
      | Some _ ->
        unequal
          (Named (Slot (Variable.create "", Value_slot value_slot1)))
          (Named (Slot (Variable.create "", Value_slot value_slot2)))
      | None -> Env.add_value_slot env value_slot1 value_slot2;
        unequal
          (Named (Slot (Variable.create "", Value_slot value_slot1)))
          (Named (Slot (Variable.create "", Value_slot value_slot2)))

let zip_fold l1 l2 ~f ~acc =
  List.combine l1 l2 |> List.fold_left f acc

let zip_sort_fold l1 l2 ~compare ~f ~acc =
  let l1 = List.sort compare l1 in
  let l2 = List.sort compare l2 in
  zip_fold l1 l2 ~f ~acc

(* Ignore code ids, phi slots, and rec info *)
let equiv_code_ids _ _ _ = true

let rec equiv (env:Env.t) e1 e2 : eq =
  match e1, e2 with
  | Named (Closure_expr (_, slot1, {function_decls;_})), Lambda _ ->
    let e1 =
      Function_slot.Lmap.find slot1 function_decls.in_order
    in
    (match e1 with
    | Id _ -> false
    | Exp e1 -> equiv env e1 e2)
  | Lambda _, Named (Closure_expr (_, slot1, {function_decls;_}))  ->
    let e2 =
      Function_slot.Lmap.find slot1 function_decls.in_order
    in
    (match e2 with
     | Id _ -> false
     | Exp e2 -> equiv env e1 e2)
  | Named v1, Named v2 -> equiv_named env v1 v2
  | Let e1, Let e2 -> equiv_let env e1 e2
  | Let_cont e1, Let_cont e2 -> equiv_let_cont env e1 e2
  | Apply e1, Apply e2 -> equiv_apply env e1 e2
  | Apply_cont e1, Apply_cont e2 -> equiv_apply_cont env e1 e2
  | Lambda e1, Lambda e2 -> equiv_lambda env e1 e2
  | Switch e1, Switch e2 -> equiv_switch env e1 e2
  | Invalid _, Invalid _ -> true
  | (Named (Simple _ | Prim _ | Slot _ | Closure_expr _ | Set_of_closures _
           | Static_consts _ | Rec_info _) | Let _ | Let_cont _ | Apply _ |
     Apply_cont _ | Switch _ | Invalid _ | Lambda _), _ ->
    unequal e1 e2

(* [let_expr] *)
and equiv_let env e1 e2 : eq =
  Core_let.pattern_match_pair e1 e2
    (fun _bound let_bound1 let_bound2 ->
       equiv env let_bound1 let_bound2 && equiv env e1.expr_body e2.expr_body)
    (fun bound1 bound2 let_bound1 let_bound2 ->
         equiv_let_symbol_exprs env
           (bound1, e1.expr_body, let_bound1)
           (bound2, e2.expr_body, let_bound2))
  |> function | Ok comp -> comp | Error _ ->
    unequal (Let e1) (Let e2)

and equiv_let_symbol_exprs env
      (static1, const1, body1) (static2, const2, body2) : eq =
  equiv_bound_static env static1 static2 &&
  equiv env const1 const2 &&
  equiv env body1 body2

and equiv_static_consts env
      (const1 : Flambda2_core.static_const_or_code)
      (const2 : Flambda2_core.static_const_or_code) : eq =
  match const1, const2 with
  | Code code1, Code code2 -> equiv_code env code1 code2
  | Static_const (Block (tag1, mut1, fields1)),
    Static_const (Block (tag2, mut2, fields2)) ->
    equiv_block env (tag1, mut1, fields1) (tag2, mut2, fields2)
  | Static_const (Static_set_of_closures set1),
    Static_const (Static_set_of_closures set2) ->
    equiv_set_of_closures env set1 set2
  | Deleted_code, Deleted_code -> true
  (* IY: Is it fine to treat all the other static constants uniformly? *)
  | (Static_const (Static_set_of_closures _) |
     Static_const (Block _) |
     Static_const (Boxed_float _) |
     Static_const (Boxed_int32 _) |
     Static_const (Boxed_int64 _) |
     Static_const (Boxed_nativeint _) |
     Static_const (Immutable_float_block _) |
     Static_const (Immutable_float_array _) |
     Static_const (Immutable_value_array _) |
     Static_const Empty_array |
     Static_const (Mutable_string _)|
     Static_const (Immutable_string _)|
     Code _ | Deleted_code), _ -> compare const1 const2 = 0

and equiv_code env {expr=expr1;anon=_} {expr=expr2;anon=_} =
  Core_function_params_and_body.pattern_match_pair expr1 expr2
    ~f:(fun
         ~return_continuation:_
         ~exn_continuation:_
         _params
         ~body1
         ~body2
         ~my_closure:_ ->
         equiv env body1 body2)

and equiv_block env (tag1, mut1, fields1) (tag2, mut2, fields2) =
  Tag.Scannable.equal tag1 tag2 &&
  Mutability.compare mut1 mut2 = 0 &&
  (List.combine fields1 fields2 |>
   List.fold_left (fun x (e1, e2) -> x && equiv env e1 e2)
     true)

and equiv_bound_static env static1 static2 : eq =
  let static1 = Bound_codelike.to_list static1 in
  let static2 = Bound_codelike.to_list static2 in
  List.combine static1 static2 |>
  List.fold_left (fun x (e1, e2) -> x && equiv_pattern env e1 e2) true

(* Compare equal patterns and add variables to environment *)
and equiv_pattern env
      (pat1 : Bound_codelike.Pattern.t) (pat2 : Bound_codelike.Pattern.t) : eq =
  match pat1, pat2 with
  | Code id1, Code id2 ->
    Env.add_code_id env id1 id2; true
  | Block_like sym1, Block_like sym2 ->
    Env.add_symbol env sym1 sym2; true
  | Set_of_closures clo1, Set_of_closures clo2 ->
    equiv_simple env
      (Simple.var (Bound_var.var clo1)) (Simple.var (Bound_var.var clo2))
  | (Code _ | Block_like _ | Set_of_closures _), _ -> false

and equiv_function_slots env slot1 slot2 =
  let _ =  subst_function_slot env slot1 in
  match Env.find_function_slot env slot1 with
  | Some function_slot ->
    Function_slot.equal function_slot slot2
  | None ->
    match Env.find_function_slot_rev env slot2 with
    | Some _ -> false
    | None -> Env.add_function_slot env slot1 slot2; true

and equiv_function_decl env exp1 exp2 =
  match exp1, exp2 with
  | Id id1, Id id2 -> equiv_code_ids env id1 id2
  | Exp exp1, Exp exp2 -> equiv env exp1 exp2
  | (Id _ | Exp _), _ -> false

and equiv_set_of_closures env
  (set1 : set_of_closures) (set2 : set_of_closures) : eq =
  (* Unify value and function slots *)
  (* Comparing value slots *)
  let value_slots_by_value set =
    Value_slot.Map.bindings (set.value_slots)
    |> List.map (fun (var, value) ->
      subst_simple env value, var)
  in
  let compare (value1, _var1) (value2, _var2) =
    (match value1, value2 with
      | Id value1, Id value2 ->
        Simple.compare value1 value2
      | Exp exp1, Exp exp2 ->
        if equiv env exp1 exp2 then 0 else 1
      | (Id _ | Exp _), _ -> 1)
  in
  let value_slots_eq =
    zip_sort_fold (value_slots_by_value set1) (value_slots_by_value set2)
      ~compare
      ~f:(fun x ((_, var1), (_, var2)) ->
            x && equiv_value_slots env var1 var2)
      ~acc:true
  in
  (* Comparing function slots *)
  let function_slots_and_fun_decls_by_code_id (set : set_of_closures)
      : (function_expr * (Function_slot.t * function_expr)) list =
    let map = (set.function_decls).funs in
    Function_slot.Map.bindings map
    |> List.map (fun (function_slot, code_id) ->
      subst_function_expr env code_id, (function_slot, code_id))
  in
  let function_slots_eq =
    zip_fold
      (function_slots_and_fun_decls_by_code_id set1)
      (function_slots_and_fun_decls_by_code_id set2)
      ~f:(fun acc ((_, (slot1, decl1)), (_, (slot2, decl2))) ->
        acc &&
        equiv_function_slots env slot1 slot2 &&
        equiv_function_decl env decl1 decl2)
      ~acc: true
  in
  value_slots_eq && function_slots_eq

(* N.B. ignore Phi slots assigned *)
and equiv_named env named1 named2 : eq =
  match named1, named2 with
  | Simple simple1, Simple simple2 ->
    equiv_simple env simple1 simple2
  | Prim prim1, Prim prim2 ->
    equiv_primitives env prim1 prim2
  | Slot (_, Function_slot slot1), Slot (_, Function_slot slot2) ->
    equiv_function_slots env slot1 slot2
  | Slot (_, Value_slot slot1), Slot (_, Value_slot slot2) ->
    equiv_value_slots env slot1 slot2
  | Closure_expr (_, slot1, set1), Closure_expr (_, slot2, set2) ->
    equiv_function_slots env slot1 slot2 &&
    equiv_set_of_closures env set1 set2
  | Set_of_closures set1, Set_of_closures set2 ->
    equiv_set_of_closures env set1 set2
  | Rec_info _, Rec_info _ -> true
  | Static_consts const1, Static_consts const2 ->
    (List.combine const1 const2 |>
     List.fold_left (fun x (e1, e2) -> x && equiv_static_consts env e1 e2) true)
  | (Simple _ | Prim _ | Slot (_, Function_slot _ | _, Value_slot _) |
     Set_of_closures _ | Rec_info _ | Static_consts _ | Closure_expr _), _ ->
    unequal (Named named1) (Named named2)

and equiv_simple env simple1 simple2 : eq =
  Simple.pattern_match simple1
    ~name:(fun name1 ~coercion:_ ->
      Simple.pattern_match simple2
        ~name:(fun name2 ~coercion:_ -> equiv_names env name1 name2)
        ~const:(fun _ -> false))
    ~const:(fun const1 ->
      Simple.pattern_match simple2
        ~name:(fun _ ~coercion:_ -> false)
        ~const:(fun const2 -> Reg_width_const.equal const1 const2))

and equiv_primitives env prim1 prim2 : eq =
  match (prim1:primitive), (prim2:primitive) with
  | Unary (op1, arg1), Unary (op2, arg2) ->
    P.equal_unary_primitive op1 op2 &&
    equiv env arg1 arg2
  | Binary (op1, arg1_1, arg1_2), Binary (op2, arg2_1, arg2_2) ->
    P.equal_binary_primitive op1 op2 &&
    equiv env arg1_1 arg2_1 &&
    equiv env arg1_2 arg2_2
  | Ternary (op1, arg1_1, arg1_2, arg1_3),
    Ternary (op2, arg2_1, arg2_2, arg2_3) ->
    P.equal_ternary_primitive op1 op2 &&
    equiv env arg1_1 arg2_1 &&
    equiv env arg1_2 arg2_2 &&
    equiv env arg1_3 arg2_3
  | Variadic (op1, args1), Variadic (op2, args2) ->
    P.equal_variadic_primitive op1 op2 &&
    (List.combine args1 args2 |>
     List.fold_left (fun x (e1, e2) -> x && equiv env e1 e2) true)
  | Nullary (Invalid _), Nullary (Invalid _) -> true
  | Nullary (Optimised_out _), Nullary (Optimised_out _) -> true
  | Nullary (Probe_is_enabled _), Nullary (Probe_is_enabled _) -> true
  | Nullary Begin_region, Nullary Begin_region -> true
  | (Nullary (Invalid _) | Nullary (Optimised_out _ ) | Nullary (Probe_is_enabled _)
    | Nullary (Begin_region) | Nullary (Enter_inlined_apply _)
    | Unary _ | Binary _ | Ternary _ | Variadic _), _ ->
    false

(* [let_cont_expr] *)
and equiv_let_cont env let_cont1 let_cont2 : eq =
  match let_cont1, let_cont2 with
  | Non_recursive {handler = handler1; body = body1},
    Non_recursive {handler = handler2; body = body2} ->
    equiv_cont_handler env handler1 handler2 &&
    Core_letcont_body.pattern_match_pair body1 body2
      (fun _bound b1 b2 -> equiv env b1 b2)
  | Recursive handlers1, Recursive handlers2 ->
    Core_recursive.pattern_match_pair handlers1 handlers2
      (fun (_params : Bound_parameters.t)
        (body1 : core_exp) (body2 : core_exp)
        (map1 : continuation_handler_map) (map2 : continuation_handler_map) ->
        equiv env body1 body2 &&
        equiv_cont_handler_map env map1 map2)
  | (Non_recursive _ | Recursive _), _ -> false

and equiv_cont_handler env handler1 handler2 =
  Core_continuation_handler.pattern_match_pair handler1 handler2
    (fun _bound h1 h2 -> equiv env h1 h2)

and equiv_cont_handler_map env
      (map1 : continuation_handler_map) (map2 : continuation_handler_map) =
  Continuation.Map.equal (equiv_cont_handler env) map1 map2

and equiv_continuation_expr env (e1 : continuation_expr) (e2 : continuation_expr) : eq =
  match e1, e2 with
  | Cont_id e1, Cont_id e2 ->
    Apply_expr.Result_continuation.equal e1 e2
  | Handler e1, Handler e2 -> equiv_cont_handler env e1 e2
  | (Cont_id _ | Handler _), (Cont_id _ | Handler _) -> false

and equiv_exn_continuation_expr env
      (e1 : exn_continuation_expr) (e2 : exn_continuation_expr) : eq =
  match e1, e2 with
  | Cont_id e1, Cont_id e2 -> Continuation.equal e1 e2
  | Handler e1, Handler e2 -> equiv_cont_handler env e1 e2
  | (Cont_id _ | Handler _), (Cont_id _ | Handler _) -> false

(* [apply] *)
and equiv_apply env (e1 : apply_expr) (e2 : apply_expr) : eq =
  let equiv_conts =
    equiv_continuation_expr env (e1.continuation) (e2.continuation) &&
    equiv_exn_continuation_expr env (e1.exn_continuation) (e2.exn_continuation) in
  let callee = equiv env (e1.callee) (e2.callee) in
  let args =
    zip_fold (e1.apply_args) (e2.apply_args)
      ~f:(fun x (e1, e2) -> x && equiv env e1 e2) ~acc:true in
  equiv_conts && callee && args

(* [apply_cont] *)
and equiv_apply_cont env
      ({k = k1; args = args1} : apply_cont_expr)
      ({k = k2; args = args2} : apply_cont_expr) : eq =
  equiv_cont env k1 k2 &&
  zip_fold args1 args2 ~f:(fun x (e1, e2) -> x && equiv env e1 e2) ~acc:true

and equiv_cont _env (e1 : Continuation.t) (e2 : Continuation.t) : eq =
  match Continuation.sort e1, Continuation.sort e2 with
  | Toplevel_return, Toplevel_return -> true
  | Normal_or_exn, Normal_or_exn
  | Return, Return
  | Define_root_symbol, Define_root_symbol -> Continuation.equal e1 e2
  | (Normal_or_exn | Return | Define_root_symbol | Toplevel_return), _ -> false

and equiv_lambda env (e1 : lambda_expr) (e2 : lambda_expr) : eq =
  Core_lambda.pattern_match_pair e1 e2
    ~f:(fun
         ~return_continuation:_ ~exn_continuation:_ _params e1 e2 ->
         equiv env e1 e2)

(* [switch] *)
and equiv_switch env
      ({scrutinee = scrutinee1; arms = arms1} : switch_expr)
      ({scrutinee = scrutinee2; arms = arms2} : switch_expr) : eq =
  equiv env scrutinee1 scrutinee2 &&
  Targetint_31_63.Map.equal (equiv env) arms1 arms2

let core_eq e1 e2 =
  try (equiv (Env.create ()) e1 e2) with
  Invalid_argument _ -> unequal e1 e2
