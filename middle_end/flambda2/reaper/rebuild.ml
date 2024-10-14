(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
open! Rev_expr
module Float = Numeric_types.Float_by_bit_pattern
module Float32 = Numeric_types.Float32_by_bit_pattern
module RE = Rebuilt_expr

type rev_expr = Rev_expr.t

let all_slot_offsets = ref Slot_offsets.empty

let all_code = ref Code_id.Map.empty

type env =
  { uses : Dep_solver.result;
    get_code_metadata : Code_id.t -> Code_metadata.t;
    cont_params_to_keep : bool list Continuation.Map.t;
  }

let is_used (env : env) cn = Hashtbl.mem env.uses cn

let is_code_id_used (env : env) code_id =
  is_used env (Code_id_or_name.code_id code_id)
  || not (Compilation_unit.is_current (Code_id.get_compilation_unit code_id))

let is_name_used (env : env) name = is_used env (Code_id_or_name.name name)

let is_var_used (env : env) var = is_used env (Code_id_or_name.var var)

let is_symbol_used (env : env) symbol =
  is_used env (Code_id_or_name.symbol symbol)

let poison_value = 0 (* 123456789 *)

let poison kind = Simple.const_int_of_kind kind poison_value

let rewrite_simple kinds (env : env) simple =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ ->
      if is_name_used env name
      then simple
      else
        let kind =
          match Name.Map.find_opt name kinds with
          | Some k -> k
          | None ->
            if Name.is_symbol name
            then Flambda_kind.value
            else Misc.fatal_errorf "Unbound name %a" Name.print name
        in
        poison kind)
    ~const:(fun _ -> simple)

let rewrite_simple_opt (env : env) = function
  | None -> None
  | Some simple as simpl ->
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ ->
        if is_name_used env name then simpl else None)
      ~const:(fun _ -> simpl)

let rewrite_or_variable default env (or_variable : _ Or_variable.t) =
  match or_variable with
  | Const _ -> or_variable
  | Var (v, _) ->
    if is_var_used env v then or_variable else Or_variable.Const default

let rewrite_simple_with_debuginfo kinds env (simple : Simple.With_debuginfo.t) =
  Simple.With_debuginfo.create
    (rewrite_simple kinds env (Simple.With_debuginfo.simple simple))
    (Simple.With_debuginfo.dbg simple)

let rewrite_static_const kinds (env : env) (sc : Static_const.t) =
  match sc with
  | Set_of_closures sc ->
    let function_decls = Set_of_closures.function_decls sc in
    let function_decls =
      let module FD = Function_declarations in
      FD.create
        (Function_slot.Lmap.mapi
           (fun _slot (code_id : FD.code_id_in_function_declaration) :
                FD.code_id_in_function_declaration ->
             match code_id with
             | Deleted _ -> code_id
             | Code_id code_id ->
               if is_code_id_used env code_id
               then Code_id code_id
               else
                 let code_metadata = env.get_code_metadata code_id in
                 Deleted
                   { function_slot_size =
                       Code_metadata.function_slot_size code_metadata;
                     dbg = Code_metadata.dbg code_metadata
                   })
           (FD.funs_in_order function_decls))
    in
    let set_of_closures =
      Set_of_closures.create
        ~value_slots:
          (Value_slot.Map.map (rewrite_simple kinds env)
             (Set_of_closures.value_slots sc))
        (Set_of_closures.alloc_mode sc)
        function_decls
    in
    all_slot_offsets
      := Slot_offsets.add_set_of_closures !all_slot_offsets ~is_phantom:false
           set_of_closures;
    Static_const.set_of_closures set_of_closures
  | Block (tag, mut, shape, fields) ->
    let fields = List.map (rewrite_simple_with_debuginfo kinds env) fields in
    Static_const.block tag mut shape fields
  | Boxed_float f ->
    Static_const.boxed_float (rewrite_or_variable Float.zero env f)
  | Boxed_float32 f ->
    Static_const.boxed_float32 (rewrite_or_variable Float32.zero env f)
  | Boxed_int32 n ->
    Static_const.boxed_int32 (rewrite_or_variable Int32.zero env n)
  | Boxed_int64 n ->
    Static_const.boxed_int64 (rewrite_or_variable Int64.zero env n)
  | Boxed_nativeint n ->
    Static_const.boxed_nativeint
      (rewrite_or_variable Targetint_32_64.zero env n)
  | Boxed_vec128 n ->
    Static_const.boxed_vec128
      (rewrite_or_variable Vector_types.Vec128.Bit_pattern.zero env n)
  | Immutable_float_block fields ->
    let fields = List.map (rewrite_or_variable Float.zero env) fields in
    Static_const.immutable_float_block fields
  | Immutable_float_array fields ->
    let fields = List.map (rewrite_or_variable Float.zero env) fields in
    Static_const.immutable_float_array fields
  | Immutable_float32_array fields ->
    let fields = List.map (rewrite_or_variable Float32.zero env) fields in
    Static_const.immutable_float32_array fields
  | Immutable_value_array fields ->
    let fields = List.map (rewrite_simple_with_debuginfo kinds env) fields in
    Static_const.immutable_value_array fields
  | Immutable_int32_array fields ->
    let fields = List.map (rewrite_or_variable Int32.zero env) fields in
    Static_const.immutable_int32_array fields
  | Immutable_int64_array fields ->
    let fields = List.map (rewrite_or_variable Int64.zero env) fields in
    Static_const.immutable_int64_array fields
  | Immutable_nativeint_array fields ->
    let fields =
      List.map (rewrite_or_variable Targetint_32_64.zero env) fields
    in
    Static_const.immutable_nativeint_array fields
  | Immutable_vec128_array fields ->
    let fields =
      List.map
        (rewrite_or_variable Vector_types.Vec128.Bit_pattern.zero env)
        fields
    in
    Static_const.immutable_vec128_array fields
  | Empty_array _ | Mutable_string _ | Immutable_string _ -> sc

let rewrite_static_const_or_code kinds env (sc : Static_const_or_code.t) =
  match sc with
  | Code _ -> sc
  | Deleted_code -> sc
  | Static_const sc ->
    Static_const_or_code.create_static_const (rewrite_static_const kinds env sc)

let rewrite_static_const_group kinds env (group : Static_const_group.t) =
  Static_const_group.map ~f:(rewrite_static_const_or_code kinds env) group

let rewrite_set_of_closures bound (env : env) value_slots alloc_mode
    function_decls =
  let slot_is_used slot =
    List.exists
      (fun bv ->
        match
          Hashtbl.find_opt env.uses (Code_id_or_name.var (Bound_var.var bv))
        with
        | None | Some Bottom -> false
        | Some Top -> true
        | Some (Fields f) -> Global_flow_graph.Field.Map.mem slot f)
      bound
  in
  let code_is_used bv =
    match
      Hashtbl.find_opt env.uses (Code_id_or_name.var (Bound_var.var bv))
    with
    | None | Some Bottom -> false
    | Some Top -> true
    | Some (Fields f) -> Global_flow_graph.Field.Map.mem Code_of_closure f
  in
  let value_slots =
    Value_slot.Map.filter
      (fun slot _ -> slot_is_used (Value_slot slot))
      value_slots
  in
  let open Function_declarations in
  let function_decls =
    List.map2
      (fun bound_var (slot, code_id) ->
        let code_id =
          match code_id with
          | Deleted _ -> code_id
          | Code_id code_id ->
            if code_is_used bound_var
            then Code_id code_id
            else
              let code_metadata = env.get_code_metadata code_id in
              Deleted
                { function_slot_size =
                    Code_metadata.function_slot_size code_metadata;
                  dbg = Code_metadata.dbg code_metadata
                }
        in
        slot, code_id)
      bound
      (Function_slot.Lmap.bindings
         (Function_declarations.funs_in_order function_decls))
  in
  let function_decls =
    Function_declarations.create (Function_slot.Lmap.of_list function_decls)
  in
  Set_of_closures.create ~value_slots alloc_mode function_decls

let rewrite_named kinds env (named : Named.t) =
  match named with
  | Simple simple -> Named.create_simple (rewrite_simple kinds env simple)
  | Prim (prim, dbg) ->
    let prim = Flambda_primitive.map_args (rewrite_simple kinds env) prim in
    Named.create_prim prim dbg
  | Set_of_closures s -> Named.create_set_of_closures s (* Already rewritten *)
  | Static_consts sc ->
    Named.create_static_consts (rewrite_static_const_group kinds env sc)
  | Rec_info r -> Named.create_rec_info r

let select_list_elements to_select l =
   List.filter_map (fun (x, to_select) -> if to_select then Some x else None) (List.combine l to_select)

let rewrite_apply_cont_expr kinds env ac =
  let cont = Apply_cont_expr.continuation ac in
  let args = Apply_cont_expr.args ac in
  let args =
    try (* only for testing, TODO remove this try/with *)
    let args_to_keep = Continuation.Map.find cont env.cont_params_to_keep in
    select_list_elements args_to_keep args
    with Not_found -> args
  in
  let args = List.map (rewrite_simple kinds env) args in
  Apply_cont_expr.with_continuation_and_args ac cont ~args

let rec rebuild_expr (kinds : Flambda_kind.t Name.Map.t) (env : env)
    (rev_expr : rev_expr) : RE.t =
  let { expr; holed_expr } = rev_expr in
  let expr, free_names =
    match expr with
    | Invalid { message } ->
      Expr.create_invalid (Message message), Name_occurrences.empty
    | Apply_cont ac ->
      let ac = rewrite_apply_cont_expr kinds env ac in
      Expr.create_apply_cont ac, Apply_cont_expr.free_names ac
    | Switch switch ->
      let switch =
        Switch_expr.create
          ~condition_dbg:(Switch_expr.condition_dbg switch)
            (* Scrutinee should never need rewriting, do it anyway for
               completeness *)
          ~scrutinee:(rewrite_simple kinds env (Switch_expr.scrutinee switch))
          ~arms:
            (Targetint_31_63.Map.map
               (rewrite_apply_cont_expr kinds env)
               (Switch_expr.arms switch))
      in
      Expr.create_switch switch, Switch_expr.free_names switch
    | Apply apply ->
      (* CR ncourant: we never rewrite alloc_mode. This is currently ok because
         we never remove begin- or end-region primitives, but might be needed
         later if we chose to handle them. *)
      let call_kind =
        let rewrite_simple = rewrite_simple kinds env in
        match Apply.call_kind apply with
        | Function _ as ck -> ck
        | Method { kind; obj; alloc_mode } ->
          Call_kind.method_call kind ~obj:(rewrite_simple obj) alloc_mode
        | C_call _ as ck -> ck
        | Effect (Perform { eff }) ->
          Call_kind.effect (Call_kind.Effect.perform ~eff:(rewrite_simple eff))
        | Effect (Reperform { eff; cont; last_fiber }) ->
          Call_kind.effect
            (Call_kind.Effect.reperform ~eff:(rewrite_simple eff)
               ~cont:(rewrite_simple cont)
               ~last_fiber:(rewrite_simple last_fiber))
        | Effect (Run_stack { stack; f; arg }) ->
          Call_kind.effect
            (Call_kind.Effect.run_stack ~stack:(rewrite_simple stack)
               ~f:(rewrite_simple f) ~arg:(rewrite_simple arg))
        | Effect (Resume { stack; f; arg; last_fiber }) ->
          Call_kind.effect
            (Call_kind.Effect.resume ~stack:(rewrite_simple stack)
               ~f:(rewrite_simple f) ~arg:(rewrite_simple arg)
               ~last_fiber:(rewrite_simple last_fiber))
      in
      let exn_continuation = Apply.exn_continuation apply in
      let exn_continuation =
        Exn_continuation.create
          ~exn_handler:(Exn_continuation.exn_handler exn_continuation)
          ~extra_args:
            (List.map
               (fun (simple, kind) -> rewrite_simple kinds env simple, kind)
               (Exn_continuation.extra_args exn_continuation))
      in
      let apply =
        Apply.create
        (* Note here that callee is rewritten with [rewrite_simple_opt], which
           will put [None] as the callee instead of a dummy value, as a dummy
           value would then be further used in a later simplify pass to refine
           the call kind and produce an invalid. *)
          ~callee:(rewrite_simple_opt env (Apply.callee apply))
          ~continuation:(Apply.continuation apply) exn_continuation
          ~args:(List.map (rewrite_simple kinds env) (Apply.args apply))
          ~args_arity:(Apply.args_arity apply)
          ~return_arity:(Apply.return_arity apply) ~call_kind (Apply.dbg apply)
          ~inlined:(Apply.inlined apply)
          ~inlining_state:(Apply.inlining_state apply)
          ~probe:(Apply.probe apply) ~position:(Apply.position apply)
          ~relative_history:(Apply.relative_history apply)
      in
      Expr.create_apply apply, Apply.free_names apply
  in
  rebuild_holed kinds env holed_expr (RE.from_expr ~expr ~free_names)

and rebuild_function_params_and_body (kinds : Flambda_kind.t Name.Map.t)
    (env : env) (params_and_body : rev_params_and_body) =
  let { return_continuation;
        exn_continuation;
        params;
        body;
        my_closure;
        my_region;
        my_ghost_region;
        my_depth
      } =
    params_and_body
  in
  let body = rebuild_expr kinds env body in
  Function_params_and_body.create ~return_continuation ~exn_continuation params
    ~body:body.expr ~free_names_of_body:(Known body.free_names) ~my_closure
    ~my_region ~my_ghost_region ~my_depth

and rebuild_holed (kinds : Flambda_kind.t Name.Map.t) (env : env)
    (rev_expr : rev_expr_holed) (hole : RE.t) : RE.t =
  match rev_expr with
  | Up -> hole
  | Let let_ -> (
    let[@local] erase () = rebuild_holed kinds env let_.parent hole in
    let[@local] default () =
      let subexpr =
        let bp, defining_expr =
          match let_.defining_expr with
          | Named defining_expr -> let_.bound_pattern, defining_expr
          | Static_consts group ->
            let bound_static =
              match let_.bound_pattern with
              | Static l -> l
              | Set_of_closures _ | Singleton _ ->
                (* Bound pattern is static consts, so can't bind something
                   else *)
                assert false
            in
            let bound_and_group =
              List.filter_map
                (fun ((p, e) as arg : Bound_static.Pattern.t * _) ->
                  match p with
                  | Code code_id ->
                    if is_code_id_used env code_id
                    then Some arg
                    else (
                      (match e with
                      | Code _ -> ()
                      | Deleted_code -> ()
                      | Static_const _ ->
                        (* Pattern is [Code _], so can't bind static const *)
                        assert false);
                      Some (p, Deleted_code))
                  | Block_like sym ->
                    if is_symbol_used env sym then Some arg else None
                  | Set_of_closures m ->
                    if Function_slot.Lmap.exists
                         (fun _ sym -> is_symbol_used env sym)
                         m
                    then Some arg
                    else None)
                (List.combine (Bound_static.to_list bound_static) group)
            in
            let bound_static, group = List.split bound_and_group in
            let static_const_or_code = function
              | Deleted_code -> Static_const_or_code.deleted_code
              | Code
                  { params_and_body;
                    code_metadata;
                    free_names_of_params_and_body
                  } ->
                let is_my_closure_used =
                  is_var_used env params_and_body.my_closure
                in
                let params_and_body =
                  rebuild_function_params_and_body kinds env params_and_body
                in
                let code_metadata =
                  if Bool.equal is_my_closure_used
                       (Code_metadata.is_my_closure_used code_metadata)
                  then code_metadata
                  else (
                    assert (not is_my_closure_used);
                    Code_metadata.with_is_my_closure_used is_my_closure_used
                      code_metadata)
                in
                let code =
                  Code.create_with_metadata ~params_and_body ~code_metadata
                    ~free_names_of_params_and_body
                in
                all_code := Code_id.Map.add (Code.code_id code) code !all_code;
                Static_const_or_code.create_code code
              | Static_const static_const ->
                Static_const_or_code.create_static_const static_const
            in
            let group =
              Static_const_group.create (List.map static_const_or_code group)
            in
            ( Bound_pattern.static (Bound_static.create bound_static),
              Named.create_static_consts group )
          | Set_of_closures { value_slots; alloc_mode; function_decls } ->
            let bound =
              match let_.bound_pattern with
              | Set_of_closures s -> s
              | Static _ | Singleton _ ->
                (* Pattern is a set of closures *)
                assert false
            in
            let set_of_closures =
              rewrite_set_of_closures bound env value_slots alloc_mode
                function_decls
            in
            let is_phantom =
              Name_mode.is_phantom @@ Bound_pattern.name_mode let_.bound_pattern
            in
            all_slot_offsets
              := Slot_offsets.add_set_of_closures !all_slot_offsets ~is_phantom
                   set_of_closures;
            let_.bound_pattern, Named.create_set_of_closures set_of_closures
        in
        let defining_expr = rewrite_named kinds env defining_expr in
        RE.create_let bp defining_expr ~body:hole
      in
      rebuild_holed kinds env let_.parent subexpr
    in
    match let_.bound_pattern with
    | Set_of_closures _ -> default ()
    | Static _ -> default ()
    | Singleton v ->
      let v = Bound_var.var v in
      if is_var_used env v then default () else erase ())
  | Let_cont { cont; parent; handler } ->
    let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
    (* Unfortunately, this causes failures due to continuations that have forced arity such as continuations that are given as return continuations... *)
    let parameters_to_keep =
      List.mapi (fun i param ->
        (is_exn_handler && i = 0) || is_var_used env (Bound_parameter.var param)
      ) (Bound_parameters.to_list bound_parameters)
    in
    let env = { env with cont_params_to_keep = Continuation.Map.add cont parameters_to_keep env.cont_params_to_keep } in
    let cont_handler =
      let handler = rebuild_expr kinds env expr in
      RE.create_continuation_handler (Bound_parameters.create (select_list_elements parameters_to_keep (Bound_parameters.to_list bound_parameters))) ~handler ~is_exn_handler
        ~is_cold
    in
    let let_cont_expr =
      RE.create_non_recursive_let_cont cont cont_handler ~body:hole
    in
    rebuild_holed kinds env parent let_cont_expr
  | Let_cont_rec { parent; handlers; invariant_params } ->
    let handlers =
      Continuation.Map.map
        (fun handler ->
          let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
          let handler = rebuild_expr kinds env expr in
          RE.create_continuation_handler bound_parameters ~handler
            ~is_exn_handler ~is_cold)
        handlers
    in
    let let_cont_expr =
      RE.create_recursive_let_cont ~invariant_params handlers ~body:hole
    in
    rebuild_holed kinds env parent let_cont_expr

type result =
  { body : Expr.t;
    free_names : Name_occurrences.t;
    all_code : Code.t Code_id.Map.t;
    slot_offsets : Slot_offsets.t
  }

let rebuild kinds solved_dep get_code_metadata holed =
  all_slot_offsets := Slot_offsets.empty;
  all_code := Code_id.Map.empty;
  let env = { uses = solved_dep; get_code_metadata; cont_params_to_keep = Continuation.Map.empty } in
  let rebuilt_expr =
    Profile.record_call ~accumulate:true "up" (fun () ->
        rebuild_expr kinds env holed)
  in
  { body = rebuilt_expr.expr;
    free_names = rebuilt_expr.free_names;
    all_code = !all_code;
    slot_offsets = !all_slot_offsets
  }
