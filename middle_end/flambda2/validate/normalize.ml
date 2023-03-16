open! Flambda
open! Flambda2_core
open! Translate

(** Normalization

    - CBV-style reduction for [let] and [letcont] expressions
    - Assumes that the [typeopt/value_kind] flag is [false] *)

(* Substitution funtions for β-reduction *)
let rec apply_directed_renaming
      (cont_id: (Continuation.t * Continuation.t) list) (exp : core_exp): core_exp =
  let f_res : Apply_expr.Result_continuation.t -> continuation_expr =
    (fun x ->
       match x with
       | Return id ->
         (match List.assoc_opt id cont_id with
          | Some id -> Cont_id (Return id)
          | None -> Cont_id x)
       | Never_returns -> Cont_id x)
  in
  let f_exn =
    (fun id ->
       match List.assoc_opt id cont_id with
       | Some id -> Cont_id id
       | _ -> Cont_id id)
  in
  let f_cont =
    (fun id ->
       match List.assoc_opt id cont_id with
       | Some id -> id
       | _ -> id)
  in
  match exp with
  | Named e ->
    named_fix (apply_directed_renaming cont_id)
      (fun _ x -> Named (Simple x)) () e
  | Let e ->
    let_fix (apply_directed_renaming cont_id) e
  | Let_cont e ->
    let_cont_fix (apply_directed_renaming cont_id) e
  | Apply e ->
    apply_fix
      (apply_directed_renaming cont_id)
      f_res
      f_exn
      e
  | Apply_cont e ->
    apply_cont_fix' (apply_directed_renaming cont_id) f_cont e
  | Lambda e ->
    lambda_fix (apply_directed_renaming cont_id) e
  | Switch e ->
    switch_fix (apply_directed_renaming cont_id) e
  | Invalid _ -> exp

(* [Let-β]
      e[bound\let_body] *)
let rec subst_pattern ~(bound : Bound_for_let.t) ~(let_body : core_exp)
          (e : core_exp) : core_exp =
  match bound with
  | Singleton bound ->
    (match must_be_set_of_closures let_body with
     | Some clo ->
       subst_singleton_set_of_closures ~bound ~clo e
     | None ->
        core_fmap
          (fun (bound, let_body) s ->
            let bound = Simple.var (Bound_var.var bound) in
            if (Simple.equal s bound) then let_body else Named (Simple s))
          (fun x -> Cont_id x)
          (fun x -> Cont_id x)
          (bound, let_body) e)
  | Static bound ->
    subst_static_list ~bound ~let_body e

and subst_singleton_set_of_closures ~(bound: Bound_var.t)
      ~(clo : set_of_closures) (e : core_exp) : core_exp =
  match e with
  | Named e -> subst_singleton_set_of_closures_named ~bound ~clo e
  | Let e ->
    let_fix (subst_singleton_set_of_closures ~bound ~clo) e
  | Let_cont e ->
    let_cont_fix (subst_singleton_set_of_closures ~bound ~clo) e
  | Apply e ->
    apply_fix
      (subst_singleton_set_of_closures ~bound ~clo)
      (fun x -> Cont_id x)
      (fun x -> Cont_id x)
      e
  | Apply_cont e ->
    apply_cont_fix (subst_singleton_set_of_closures ~bound ~clo) e
  | Lambda e ->
    lambda_fix (subst_singleton_set_of_closures ~bound ~clo) e
  | Switch e ->
    switch_fix (subst_singleton_set_of_closures ~bound ~clo) e
  | Invalid _ -> e

and subst_singleton_set_of_closures_named ~bound ~clo (e : named) : core_exp =
  let f bound v =
    (if Simple.same v (Simple.var (Bound_var.var bound)) then
        Named (Set_of_closures clo)
     else
       Named (Simple v))
  in
  match e with
  | Simple v -> f bound v
  | Prim e -> prim_fix (subst_singleton_set_of_closures ~bound ~clo) e
  | Closure_expr (phi, slot, set) ->
    let set =
      set_of_closures_fix (subst_singleton_set_of_closures ~bound ~clo)
        f bound set
    in
    Named (Closure_expr (phi, slot, set))
  | Set_of_closures set ->
    let set =
      set_of_closures_fix (subst_singleton_set_of_closures ~bound ~clo)
        f bound set
    in
    Named (Set_of_closures set)
  | Static_consts group ->
    static_const_group_fix (subst_singleton_set_of_closures ~bound ~clo)
      f bound group
  | Slot (phi, Function_slot slot) ->
    (let bound = Function_slot.Lmap.bindings clo.function_decls.in_order
    in
    (* try to find if any of the symbols being bound is the same as the variable v *)
    let bound_closure =
      List.find_opt (fun (x, _) -> x = slot) bound
    in
    (match bound_closure with
     | None -> Named e
     | Some (k, _) -> Named (Closure_expr (phi, k, clo))))
  | Slot (_, Value_slot _) | Rec_info _ -> Named e

and subst_static_list ~(bound : Bound_codelike.t) ~let_body e : core_exp =
  let rec subst_static_list_ bound body e =
    (match bound, body with
     | [], [] -> e
     | hd :: tl, let_body :: body ->
       subst_static_list_ tl body
         (subst_pattern_static ~bound:hd ~let_body e)
     | _, _ ->
      Misc.fatal_error "Mismatched static binder and let body length")
  in
  match must_be_static_consts let_body with
  | Some consts_list ->
    let (body : core_exp list) =
      List.map (fun x -> Named (Static_consts [x])) consts_list
    in
    subst_static_list_ (Bound_codelike.to_list bound) body e
  | None -> Misc.fatal_error "Expected name static constants in let body"

and subst_pattern_static
      ~(bound : Bound_codelike.Pattern.t) ~(let_body : core_exp) (e : core_exp)
  : core_exp =
  match e with
  | Let e ->
    let_fix (subst_pattern_static ~bound ~let_body) e
  | Let_cont e ->
    let_cont_fix (subst_pattern_static ~bound ~let_body) e
  | Apply e ->
    apply_fix
      (subst_pattern_static ~bound ~let_body)
      (fun x -> Cont_id x)
      (fun x -> Cont_id x)
      e
  | Apply_cont e ->
    apply_cont_fix (subst_pattern_static ~bound ~let_body) e
  | Lambda e ->
    lambda_fix (subst_pattern_static ~bound ~let_body) e
  | Switch e ->
    switch_fix (subst_pattern_static ~bound ~let_body) e
  | Named named ->
    (match bound with
     | Block_like bound ->
       subst_block_like ~bound ~let_body named
     | Set_of_closures set ->
       subst_bound_set_of_closures set ~let_body named
     | Code id ->
       subst_code_id id ~let_body named)
  | Invalid _ -> e

(* [Set of closures]
   Given the code for its functions and closure variables, the set of closures
    keeps track of the mapping between them.
   i.e. it is the code generated by
    [let f = closure f_0 @f] where [@f] is the function slot and [f_0] refers
    to the code *)
and subst_bound_set_of_closures (bound : Bound_var.t) ~let_body
      (e : named) =
  match e with
  | Simple v ->
    (match must_be_static_consts let_body with
     | Some consts ->
       (* Assumption : there is at most one [set_of_closures] definition *)
       let set =
         List.find_opt is_static_set_of_closures consts
       in
       (match set with
        | Some (Static_const const) ->
          (match must_be_static_set_of_closures const with
           | Some set ->
             if Simple.same v (Simple.var (Bound_var.var bound)) then
              Named (Static_consts [Static_const (Static_set_of_closures set)])
              else Named e
           | None -> Named e)
        | Some (Deleted_code | Code _) -> Misc.fatal_error "Cannot be reached"
        | None -> Named e)
     | None -> Named e
    )
  | Prim e ->
    prim_fix (subst_pattern_static
                ~bound:(Bound_codelike.Pattern.set_of_closures bound)
                ~let_body) e
  | Static_consts e ->
    static_const_group_fix
      (subst_pattern_static
      ~bound:(Bound_codelike.Pattern.set_of_closures bound)
      ~let_body) (fun _ v -> Named (Simple v)) () e
  | Slot (phi, Function_slot slot) ->
    (match must_be_static_consts let_body with
     | Some consts ->
        let set = List.find_opt is_static_set_of_closures consts
        in
        (match set with
         | Some (Static_const const) ->
           (match must_be_static_set_of_closures const with
            | Some set ->
              let bound = Function_slot.Lmap.bindings set.function_decls.in_order
              in
              (* try to find if any of the symbols being bound is the same as the
                variable v *)
              let bound_closure =
                List.find_opt (fun (x, _) -> x = slot) bound
              in
              (match bound_closure with
              | None -> Named e
              | Some (k, _) -> Named (Closure_expr (phi, k, set)))
            | None -> Misc.fatal_error "Cannot be reached")
          | Some (Deleted_code | Code _) -> Misc.fatal_error "Cannot be reached"
          | None -> Named e)
     | None -> Named e
    )
  | Slot (_, Value_slot _) |  Closure_expr _ | Set_of_closures _ | Rec_info _ -> Named e

and subst_code_id_set_of_closures (bound : Code_id.t) ~(let_body : core_exp)
      {function_decls; value_slots; alloc_mode}
  : set_of_closures =
  let in_order : function_expr Function_slot.Lmap.t =
    function_decls.in_order |>
      Function_slot.Lmap.map
        (fun x ->
            match x with
            | Id code_id ->
              if (Code_id.compare code_id bound = 0)
              then Exp let_body
              else Id code_id
            | Exp e ->
              Exp (subst_pattern_static ~bound:(Bound_codelike.Pattern.code bound)
                    ~let_body e))
  in
  let function_decls =
    { funs = Function_slot.Map.of_list (Function_slot.Lmap.bindings in_order);
      in_order}
  in
  {function_decls; value_slots; alloc_mode}

and subst_code_id (bound : Code_id.t) ~(let_body : core_exp) (e : named) : core_exp =
  match e with
  | Simple _ | Slot _ -> Named e
  | Prim e ->
    prim_fix
      (subst_pattern_static
         ~bound:(Bound_codelike.Pattern.code bound) ~let_body) e
  | Closure_expr (phi, slot, set) ->
    Named (Closure_expr (phi, slot, subst_code_id_set_of_closures bound ~let_body set))
  | Set_of_closures set ->
    let set = subst_code_id_set_of_closures bound ~let_body set
    in
    Named (Set_of_closures set)
  | Static_consts consts ->
    static_const_group_fix'
      (subst_pattern_static ~bound:(Bound_codelike.Pattern.code bound) ~let_body)
      (fun _ x -> Named (Simple x))
      (fun (bound, let_body) code_id ->
         if (Code_id.compare code_id bound = 0)
         then Exp let_body
         else Id code_id)
      (bound, let_body) consts
  | Rec_info _ -> Named e

and subst_block_like
      ~(bound : Symbol.t) ~(let_body : core_exp) (e : named) : core_exp =
  core_fmap
    (fun _ v ->
       if Simple.equal v (Simple.symbol bound) then
         let_body else Named (Simple v))
    (fun x -> Cont_id x)
    (fun x -> Cont_id x) () (Named e)

(* ∀ p i, p ∈ params -> params[i] = p -> e [p \ args[i]] *)
(* There can be partial applications: don't try to do [List.combine] to avoid
   fatal errors *)
(* [Bound_parameters] are [Variable]s *)
let subst_params
  (params : Bound_parameters.t) (e : core_exp) (args : core_exp list) =
  let param_list =
    Bound_parameters.to_list params |> List.map Bound_parameter.simple
  in
  let param_args = List.combine param_list args in
  core_fmap
    (fun () s ->
      match List.assoc_opt s param_args with
      | Some arg_v -> arg_v
      | None -> Named (Simple s))
    (fun x -> Cont_id x)
    (fun x -> Cont_id x)
    () e

(* [LetCont-β] *)
let rec subst_cont (cont_e1: core_exp) (k: Bound_continuation.t)
          (args: Bound_parameters.t) (cont_e2: core_exp) : core_exp =
  let f_res (cont : Apply_expr.Result_continuation.t) : continuation_expr =
    match cont with
    | Return cont ->
      if Continuation.equal cont k
      then Handler (Core_continuation_handler.create args cont_e2)
      else Cont_id (Return cont)
    | Never_returns -> Cont_id cont
  in
  let f_exn (cont : Continuation.t) : exn_continuation_expr =
    if Continuation.equal cont k
    then Handler (Core_continuation_handler.create args cont_e2)
    else Cont_id cont
  in
  match cont_e1 with
  | Named _ -> cont_e1
  | Let e ->
    let_fix (fun e -> subst_cont e k args cont_e2) e
  | Let_cont e ->
    let_cont_fix (fun e -> subst_cont e k args cont_e2) e
  | Apply e ->
    apply_fix (fun e -> subst_cont e k args cont_e2) f_res f_exn e
  | Apply_cont {k = cont; args = concrete_args} ->
    if Continuation.equal cont k
    then subst_params args cont_e2 concrete_args
    else
      Apply_cont
        {k = cont;
         args = List.map (fun x -> subst_cont x k args cont_e2) concrete_args}
  | Lambda e ->
    lambda_fix (fun e -> subst_cont e k args cont_e2) e
  | Switch e ->
    switch_fix (fun e -> subst_cont e k args cont_e2) e
  | Invalid _ -> cont_e1

let rec normalize (e:core_exp) : core_exp =
  match e with
  | Let { let_abst; expr_body } ->
    normalize_let let_abst expr_body
    |> normalize
  | Let_cont e ->
    normalize_let_cont e
    |> normalize
  | Apply {callee; continuation; exn_continuation; apply_args} ->
    normalize_apply callee continuation exn_continuation apply_args
  | Apply_cont {k ; args} ->
    (* The recursive call for [apply_cont] is done for the arguments *)
    normalize_apply_cont k args
  | Lambda e ->
    Core_lambda.pattern_match e
      ~f:(fun x e ->
        Lambda (Core_lambda.create x (normalize e)))
  | Switch {scrutinee; arms} ->
    let scrutinee = normalize scrutinee
    in
    let arms = Targetint_31_63.Map.map normalize arms
    in
    normalize_switch scrutinee arms
  (* Note that we don't normalize [named] expressions here because static
     constants, when normalized, get a new binding because the [code] blocks
     are substituted into the static set of closures. We only normalize
     [named] expressions that are part of let-bound expressions for this
     reason.*)
  | Named _
  | Invalid _ -> e

and normalize_switch scrutinee arms : core_exp =
  (* if the scrutinee is exactly one of the arms, simplify *)
  match must_be_simple_or_immediate scrutinee with
  | Some s ->
    (match Simple.must_be_constant s with
      | Some constant ->
        (match Int_ids.Const.descr constant with
          | Naked_immediate i | Tagged_immediate i ->
            (Targetint_31_63.Map.find i arms)
          | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ ->
            Switch {scrutinee; arms})
      | None -> Switch {scrutinee; arms})
  | None -> Switch {scrutinee; arms}

and normalize_let let_abst body : core_exp =
  Core_let.pattern_match {let_abst; expr_body = body}
    ~f:(fun ~x ~e1 ~e2 ->
    (* [LetL]
                    e1 ⟶ e1'
      -------------------------------------
        let x = e1 in e2 ⟶ let x = e1' in e2 *)
    let x, e1 =
      (match must_be_named e1 with
      | Some e -> normalize_named x e
      | None -> x, normalize e1)
    in
    (* [Let-β]
        let x = v in e1 ⟶ e2 [x\v] *)
    subst_pattern ~bound:x ~let_body:e1 e2)

and normalize_let_cont (e:let_cont_expr) : core_exp =
  match e with
  | Non_recursive {handler; body} ->
    Core_continuation_handler.pattern_match handler
      (fun args e2 ->
         Core_letcont_body.pattern_match body
           (fun k e1 ->
              (* [LetCont-β]
                e1 where k args = e2 ⟶ e1 [k \ λ args. e2] *)
              let result = subst_cont e1 k args e2 in
              result
           )
      )
  | Recursive _handlers -> failwith "Unimplemented_recursive"

and normalize_apply_no_beta_redex callee continuation exn_continuation apply_args =
  let continuation =
     (match continuation with
      | Cont_id _ -> continuation
      | Handler handler -> Handler (normalize_continuation_handler handler))
   in
   let exn_continuation =
     (match exn_continuation with
      | Cont_id _ -> exn_continuation
      | Handler handler -> Handler (normalize_continuation_handler handler))
   in
   Apply {callee;continuation;exn_continuation;apply_args}

and normalize_apply_function_decls function_decls
      callee continuation exn_continuation apply_args =
  let in_order = function_decls.in_order in
  (* LATER: Do we need to handle cases other than singleton? *)
  match Function_slot.Lmap.get_singleton in_order with
  | Some (_, Exp (Lambda exp)) ->
    normalize_apply_lambda exp continuation exn_continuation apply_args
  | (Some (_,
    (Exp (Named _ | Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _
          | Invalid _) | Id _)) | None) ->
    normalize_apply_no_beta_redex
      callee continuation exn_continuation apply_args

and normalize_apply_lambda lambda_expr continuation exn_continuation apply_args =
  Core_lambda.pattern_match lambda_expr
    ~f:(fun bound exp ->
        let params = bound.params
        in
        let exp =
          (match continuation with
            | Cont_id (Apply_expr.Result_continuation.Return continuation) ->
              (apply_directed_renaming
                 [(bound.return_continuation, continuation)] exp)
            | Handler handler ->
              Core_continuation_handler.pattern_match handler
                (fun args e2 ->
                   subst_cont exp bound.return_continuation args e2)
            | Cont_id (Apply_expr.Result_continuation.Never_returns) -> exp)
        in
        let exp =
          (match exn_continuation with
            | Cont_id continuation ->
              (apply_directed_renaming
                 [(bound.exn_continuation, continuation)] exp)
            | Handler handler ->
              Core_continuation_handler.pattern_match handler
                (fun args e2 ->
                   subst_cont exp bound.exn_continuation args e2))
        in
        subst_params params exp apply_args |> normalize)

(* LATER: We may want to reduce the callee before matching on it *)
and normalize_apply callee continuation exn_continuation apply_args : core_exp =
  let apply_args = List.map normalize apply_args in
  match callee with
  | Named (Closure_expr (_, _,
                         {function_decls; value_slots = _; alloc_mode = _})) ->
    normalize_apply_function_decls function_decls
      callee continuation exn_continuation apply_args
  | Lambda expr ->
    normalize_apply_lambda expr continuation exn_continuation apply_args
  | (Named (Static_consts ((Code _ | Deleted_code | Static_const _)::_ | [])
           | Simple _ | Prim _ | Slot _ | Set_of_closures _ | Rec_info _)
    | Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _) ->
    normalize_apply_no_beta_redex callee continuation exn_continuation apply_args

and normalize_continuation_handler (e : continuation_handler) =
  Core_continuation_handler.pattern_match e
    (fun param e ->
       Core_continuation_handler.create param (normalize e))

(* Note that the beta-reduction for [apply_cont] is implemented in
   [normalize_let_cont] (LATER: Refactor) *)
and normalize_apply_cont k args : core_exp =
  (* [ApplyCont]
            args ⟶ args'
      --------------------------
          k args ⟶ k args'       *)
  Apply_cont {k = k; args = List.map normalize args}

and normalize_static_const (phi : Bound_for_let.t) (const : static_const) : static_const =
  match const with
  | Static_set_of_closures set ->
    Static_set_of_closures (normalize_set_of_closures phi set)
  | Block (tag, mut, list) ->
    Block (tag, mut, List.map normalize list)
  | (Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
    | Immutable_float_block _ | Immutable_float_array _ | Immutable_value_array _
    | Empty_array | Mutable_string _ | Immutable_string _) -> const (* CHECK *)

and normalize_static_const_or_code (phi : Bound_for_let.t)
      (const_or_code : static_const_or_code) : static_const_or_code =
  match const_or_code with
  | Code {expr=code; anon}->
    Core_function_params_and_body.pattern_match code
      ~f:(fun param y ->
        Core_lambda.pattern_match y
          ~f:(fun bound body ->
            let params_and_body =
              Core_function_params_and_body.create param
                (Core_lambda.create bound (normalize body))
            in
            Code {expr=params_and_body; anon}))
  | Static_const const -> Static_const (normalize_static_const phi const)
  | Deleted_code -> Deleted_code

and normalize_static_const_group (phi : Bound_codelike.Pattern.t list)
      (consts : static_const_group) : Bound_codelike.Pattern.t list * core_exp =
  let phi_consts = List.combine phi consts in
  let set_of_closures, static_consts =
    List.partition (fun (_, x) -> is_static_set_of_closures x) phi_consts
  in
  (* If the set of closures is non-empty, substitute in the list of code
     blocks into the set of closures *)
  match set_of_closures with
  | [] -> (phi, Named (Static_consts consts))
  | _ ->
   (let code, static_consts =
      List.partition (fun (_, x) -> is_code x) static_consts
    in
    let anon_code, code =
      List.partition (fun (_, x) ->
        (match x with
        | Code {expr=_; anon} -> anon
        | (Static_const _ | Deleted_code) -> false)) code
    in
    (* Substitute in the anonymous functions first, partition them into
       "anon-fn"-prefixed code and non anonymous functions.
       This is because anonymous functions get its set of closures declaration
       locally inside of a code block after simplification *)
    let code = code @ anon_code
    in
    let process_set_of_closures (set : set_of_closures) =
      List.fold_left
        (fun acc (id, x) ->
          match x with
          | Code x ->
            let code_id : Code_id.t =
              (match Bound_codelike.Pattern.must_be_code id with
                | Some id -> id
                | None -> Misc.fatal_error "Expected code id")
            in
            let code =
              subst_code_id_set_of_closures code_id
                ~let_body:(Named (Static_consts [Code x])) acc
            in
            code
          | (Static_const _ | Deleted_code) ->
            Misc.fatal_error "Expected code bound") set code
    in
    let set_of_closures =
      List.map
        (fun (phi, x) ->
          match x with
          | Static_const x ->
            (match must_be_static_set_of_closures x with
              | Some x ->
                let phi = Bound_for_let.Static (Bound_codelike.create [phi])
                in
                Static_const (Static_set_of_closures
                  (process_set_of_closures x |> normalize_set_of_closures phi))
              | None -> Misc.fatal_error "Expected set of closures")
          | (Code _ | Deleted_code) ->
            Misc.fatal_error "Expected set of closures") set_of_closures
    in
    let static_consts =
      List.map (fun (_, x) ->
        normalize_static_const_or_code
          (Bound_for_let.Static (Bound_codelike.create phi)) x) static_consts
    in
    let consts = set_of_closures @ static_consts in
    let phi =
      List.filter (fun x -> not (Bound_codelike.Pattern.is_code x)) phi
    in
    (phi, Named (Static_consts consts)))

(* N.B. This normalization is rather inefficient;
   Right now (for the sake of clarity) it goes through three passes of the
   value and function expressions *)
and normalize_set_of_closures (phi : Bound_for_let.t)
      {function_decls; value_slots; alloc_mode}
  : set_of_closures =
  let value_slots =
    Value_slot.Map.map
      (fun val_expr -> normalize_value_expr val_expr)
      value_slots
  in
  (* [ClosureVal] and [ClosureFn]
     substituting in value slots for [Project_value_slots] and
     substituting in function slots for [Project_function_slots] *)
  let in_order =
    Function_slot.Lmap.mapi
      (fun slot x ->
         match x with
          | Exp e ->
            (match must_be_code e with
             | Some code ->
                let params_and_body =
                  subst_my_closure phi slot code
                    {function_decls;value_slots;alloc_mode}
                in
                Exp params_and_body
             | None -> x)
         | Id _ -> x)
      function_decls.in_order
  in
  (* normalize function slots
     NOTE (for later):
     This might need to change when we're dealing with effectful functions *)
  let in_order =
    Function_slot.Lmap.map normalize_function_expr in_order
  in
  { function_decls =
      { funs =
          Function_slot.Map.of_list (Function_slot.Lmap.bindings in_order);
        in_order}
  ; value_slots = Value_slot.Map.empty
  ; alloc_mode = alloc_mode }

(* For every occurrence of the "my_closure" argument in [fn_expr],
   substitute in [Slot(phi, clo)] *)
and subst_my_closure (phi : Bound_for_let.t) (slot : Function_slot.t)
      ({expr=fn_expr;anon} : function_params_and_body)
      (clo : set_of_closures) : core_exp =
  (match phi with
  | Singleton var
  | Static [Set_of_closures var] ->
    (let phi = Bound_var.var var
     in
      Core_function_params_and_body.pattern_match fn_expr
        ~f:(fun bff e ->
          Lambda
            (Core_lambda.pattern_match e
               ~f:(fun bound body ->
                 (* Note: Can't use [Renaming] because it is bidirectional:
                    we only want to substitute in one direction here, namely
                    if we see any occurrence of a [my_closure], substitute in
                    the closure [phi] variable. *)
                 let body =
                   core_fmap
                     (fun _ simple  ->
                        if (Simple.same (Simple.var (Bound_var.var bff)) simple)
                        then
                          Named (Slot (phi, Function_slot slot))
                        else (Named (Simple simple)))
                     (fun x -> Cont_id x)
                     (fun x -> Cont_id x)
                     () body
                 in
                Core_lambda.create bound (subst_my_closure_body clo body)))))
  | Static ((Code _ | Block_like _ | Set_of_closures _) :: _ | []) ->
    Named (Static_consts [Code {expr=fn_expr; anon}]))

(* N.B. [Projection reduction]
    When we substitute in a set of closures for primitives,
    (Here is where the `Projection` primitives occur),
    we eliminate the projection. *)
and subst_my_closure_body (clo: set_of_closures) (e : core_exp) : core_exp =
  match e with
  | Named e -> subst_my_closure_body_named clo e
  | Let e ->
    let_fix (subst_my_closure_body clo) e
  | Let_cont e ->
    let_cont_fix (subst_my_closure_body clo) e
  | Apply e ->
    apply_fix
      (subst_my_closure_body clo)
      (fun x -> Cont_id x)
      (fun x -> Cont_id x)
      e
  | Apply_cont e ->
    apply_cont_fix (subst_my_closure_body clo) e
  | Lambda e ->
    lambda_fix (subst_my_closure_body clo) e
  | Switch e ->
    switch_fix (subst_my_closure_body clo) e
  | Invalid _ -> e

and subst_my_closure_body_named
    ({function_decls;value_slots;alloc_mode=_}: set_of_closures) (e : named)
  : core_exp =
  (match e with
   | Prim (Unary (Project_value_slot slot, _)) ->
    (match Value_slot.Map.find_opt slot.value_slot value_slots with
     | Some (Exp exp) ->
       (match must_have_closure exp with
        | Some clo ->
          (let fun_decls = clo.function_decls.in_order
          in
           match Function_slot.Lmap.get_singleton fun_decls with
           | Some (_, Exp e) -> e
           | (Some (_, Id _) | None) -> Named e)
        | None ->
          (match must_be_function_slot_expr exp with
            | Some (phi, slot) ->
              (match Function_slot.Lmap.find_opt slot function_decls.in_order with
              | Some (Exp e) -> e
              | (Some (Id _) | None) -> (Named (Slot (phi, Function_slot slot))))
            | None -> exp))
     | (Some (Id _) | None) -> Named e)
  | Prim (Unary (Project_function_slot {move_from ; move_to}, arg)) ->
    (match must_be_function_slot_expr arg with
     | Some (phi, slot) ->
       (if (move_from = slot) then
        Named (Slot (phi, Function_slot move_to))
        else
          Named e)
     | None -> Named e)
  | (Prim (Unary
             ((Reinterpret_int64_as_float | Tag_immediate | Untag_immediate
              | Duplicate_block _ | Duplicate_array _
              | Is_int _ | Get_tag | Array_length | Bigarray_length _ | String_length _
              | Int_as_pointer | Opaque_identity _ | Int_arith _ | Float_arith _
              | Num_conv _ | Boolean_not | Unbox_number _ | Box_number _
              | Is_boxed_float | Is_flat_float_array | Begin_try_region | End_region
              | Obj_dup), _) | Nullary _ | Binary _ | Ternary _ | Variadic _)
    | Simple _ | Slot _ | Closure_expr _ | Set_of_closures _ | Static_consts _
    | Rec_info _) -> Named e)

and normalize_function_expr (fun_expr : function_expr) : function_expr =
  match fun_expr with
  | Id _ -> fun_expr
  | Exp exp -> Exp (normalize exp)

and normalize_value_expr (val_expr : value_expr) : value_expr =
  match val_expr with
  | Id _ -> val_expr
  | Exp exp -> Exp (normalize exp)

(* This is a "normalization" of [named] expression, in quotations because there
  is some simple evaluation that occurs for primitive arithmetic expressions *)
and normalize_named (var: Bound_for_let.t) (body : named)
  : Bound_for_let.t * core_exp =
  match body with
  | Simple _ (* A [Simple] is a register-sized value *)
  | Slot _
  | Rec_info _ (* Information about inlining recursive calls, an integer variable *) ->
    (var, Named (body))
  | Closure_expr (phi, slot, set) ->
    (var, Named (Closure_expr (phi, slot, normalize_set_of_closures var set)))
  | Set_of_closures set -> (* Map of [Code_id]s and [Simple]s corresponding to
                         function and value slots*)
    (var, Named (Set_of_closures (normalize_set_of_closures var set)))
  | Static_consts consts -> (* [Static_consts] are statically-allocated values *)
    (match var with
     | Static var ->
       let bound_vars = Bound_codelike.to_list var in
       let phi, exp = normalize_static_const_group bound_vars consts in
       (Static (Bound_codelike.create phi), exp)
     | Singleton _ -> Misc.fatal_error "Expected bound static variables")
  | Prim v -> (var, Eval_prim.eval v)
