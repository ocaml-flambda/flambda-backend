(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Simplify_import

type 'a after_rebuild = Rebuilt_expr.t -> Upwards_acc.t -> 'a

type 'a rebuild = Upwards_acc.t -> after_rebuild:'a after_rebuild -> 'a

type ('a, 'b) down_to_up = Downwards_acc.t -> rebuild:'a rebuild -> 'b

type 'a expr_simplifier =
  Downwards_acc.t ->
  'a ->
  down_to_up:
    (Rebuilt_expr.t * Upwards_acc.t, Rebuilt_expr.t * Upwards_acc.t) down_to_up ->
  Rebuilt_expr.t * Upwards_acc.t

type simplify_toplevel =
  Downwards_acc.t ->
  Expr.t ->
  return_continuation:Continuation.t ->
  return_arity:[`Unarized] Flambda_arity.t ->
  exn_continuation:Continuation.t ->
  Rebuilt_expr.t * Upwards_acc.t

type simplify_function_body =
  Downwards_acc.t ->
  Expr.t ->
  return_continuation:Continuation.t ->
  return_arity:[`Unarized] Flambda_arity.t ->
  exn_continuation:Continuation.t ->
  loopify_state:Loopify_state.t ->
  params:Bound_parameters.t ->
  implicit_params:Bound_parameters.t ->
  Rebuilt_expr.t * Upwards_acc.t

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
    ~result_kind =
  let env = DA.typing_env dacc in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom ->
    let dacc = DA.add_variable dacc result_var (T.bottom result_kind) in
    Simplify_primitive_result.create_invalid dacc
  | Ok env_extension ->
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
          DE.define_variable_and_extend_typing_environment denv result_var
            result_kind env_extension)
    in
    Simplify_primitive_result.create original_term ~try_reify:true dacc

let update_exn_continuation_extra_args uacc ~exn_cont_use_id apply =
  let exn_cont_rewrite =
    UE.find_apply_cont_rewrite (UA.uenv uacc)
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
  in
  match exn_cont_rewrite with
  | None -> apply
  | Some rewrite ->
    Apply.with_exn_continuation apply
      (Expr_builder.rewrite_exn_continuation rewrite exn_cont_use_id
         (Apply.exn_continuation apply))

(* generate the projection of the i-th field of a n-tuple *)
let project_tuple ~dbg ~size ~field tuple =
  let module BAK = P.Block_access_kind in
  let bak : BAK.t =
    Values
      { field_kind = Any_value;
        tag = Known Tag.Scannable.zero;
        size = Known (Targetint_31_63.of_int size)
      }
  in
  let mutability : Mutability.t = Immutable in
  let index = Simple.const_int (Targetint_31_63.of_int field) in
  let prim = P.Binary (Block_load (bak, mutability), tuple, index) in
  Named.create_prim prim dbg

let split_direct_over_application apply
    ~(apply_alloc_mode : Alloc_mode.For_allocations.t) ~callee's_code_id
    ~callee's_code_metadata =
  let callee's_params_arity =
    Code_metadata.params_arity callee's_code_metadata
  in
  let num_non_unarized_params =
    Flambda_arity.num_params callee's_params_arity
  in
  let args_arity = Apply.args_arity apply in
  let num_non_unarized_args = Flambda_arity.num_params args_arity in
  assert (num_non_unarized_params < num_non_unarized_args);
  let args = Apply.args apply in
  let first_args, remaining_args =
    Misc.Stdlib.List.split_at
      (Flambda_arity.cardinal_unarized callee's_params_arity)
      args
  in
  let remaining_arity =
    Flambda_arity.partially_apply args_arity
      ~num_non_unarized_params_provided:num_non_unarized_params
  in
  assert (
    List.compare_length_with remaining_args
      (Flambda_arity.cardinal_unarized remaining_arity)
    = 0);
  let func_var = Variable.create "full_apply" in
  let result_mode = Code_metadata.result_mode callee's_code_metadata in
  let outer_apply_alloc_mode = apply_alloc_mode in
  let needs_region, inner_apply_alloc_mode =
    (* If the function being called might do a local allocation that escapes,
       then we need a region for such function's return value, unless the
       allocation mode of the [Apply] is already [Local]. Note that we must not
       add an extra region if the allocation mode of the [Apply] is [Local],
       otherwise the allocation stack pointer might be reset (by the
       [End_region] primitive corresponding to such region) whilst the return
       value is still live (and with the caller expecting such value to have
       been allocated in their region). *)
    match result_mode with
    | Alloc_heap -> None, Alloc_mode.For_allocations.heap
    | Alloc_local -> (
      match apply_alloc_mode with
      | Heap ->
        let region = Variable.create "over_app_region" in
        ( Some (region, Continuation.create ()),
          Alloc_mode.For_allocations.local ~region )
      | Local _ -> None, apply_alloc_mode)
  in
  let perform_over_application =
    let continuation =
      (* If there is no need for a new region, then the second (over)
         application jumps directly to the return continuation of the original
         [Apply]. Otherwise it will need to go through [cont], which we define
         below, so that the [End_region] marker for the new region can be
         inserted. *)
      match needs_region with
      | None -> Apply.continuation apply
      | Some (_, cont) -> Apply.Result_continuation.Return cont
    in
    Apply.create
      ~callee:(Some (Simple.var func_var))
      ~continuation
      (Apply.exn_continuation apply)
      ~args:remaining_args ~args_arity:remaining_arity
      ~return_arity:(Apply.return_arity apply)
      ~call_kind:
        (Call_kind.indirect_function_call_unknown_arity outer_apply_alloc_mode)
      (Apply.dbg apply) ~inlined:(Apply.inlined apply)
      ~inlining_state:(Apply.inlining_state apply)
      ~probe:(Apply.probe apply) ~tail:(Apply.tail apply)
      ~position:(Apply.position apply)
      ~relative_history:(Apply.relative_history apply)
  in
  let perform_over_application_free_names =
    Apply.free_names perform_over_application
  in
  let perform_over_application =
    match needs_region with
    | None -> Expr.create_apply perform_over_application
    | Some (region, after_over_application) ->
      (* This wraps both applications (the full application and the second
         application) with [Begin_region] ... [End_region]. The applications
         might raise an exception, but that doesn't need any special handling,
         since we're not actually introducing any more local allocations here.
         (Missing the [End_region] on the exceptional return path is fine, c.f.
         the usual compilation of [try ... with] -- see
         [Closure_conversion].) *)
      let over_application_results =
        List.mapi
          (fun i kind ->
            BP.create (Variable.create ("result" ^ string_of_int i)) kind)
          (Flambda_arity.unarized_components (Apply.return_arity apply))
      in
      let call_return_continuation, call_return_continuation_free_names =
        match Apply.continuation apply with
        | Return return_cont ->
          let apply_cont =
            Apply_cont.create return_cont
              ~args:(List.map BP.simple over_application_results)
              ~dbg:(Apply.dbg apply)
          in
          Expr.create_apply_cont apply_cont, Apply_cont.free_names apply_cont
        | Never_returns ->
          (* The whole overapplication never returns, so this point is
             unreachable. *)
          Expr.create_invalid (Over_application_never_returns apply), NO.empty
      in
      let handler_expr =
        Let.create
          (Bound_pattern.singleton
             (Bound_var.create (Variable.create "unit") Name_mode.normal))
          (Named.create_prim
             (Unary (End_region, Simple.var region))
             (Apply.dbg apply))
          ~body:call_return_continuation
          ~free_names_of_body:(Known call_return_continuation_free_names)
        |> Expr.create_let
      in
      let handler_expr_free_names =
        NO.add_variable call_return_continuation_free_names region
          Name_mode.normal
      in
      let handler =
        Continuation_handler.create
          (Bound_parameters.create over_application_results)
          ~handler:handler_expr
          ~free_names_of_handler:(Known handler_expr_free_names)
          ~is_exn_handler:false ~is_cold:false
      in
      Let_cont.create_non_recursive after_over_application handler
        ~body:(Expr.create_apply perform_over_application)
        ~free_names_of_body:(Known perform_over_application_free_names)
  in
  let after_full_application = Continuation.create () in
  let after_full_application_handler =
    let func_param = BP.create func_var K.With_subkind.any_value in
    Continuation_handler.create
      (Bound_parameters.create [func_param])
      ~handler:perform_over_application
      ~free_names_of_handler:(Known perform_over_application_free_names)
      ~is_exn_handler:false ~is_cold:false
  in
  let full_apply =
    Apply.create ~callee:(Apply.callee apply)
      ~continuation:(Return after_full_application)
      (Apply.exn_continuation apply)
      ~args:first_args ~args_arity:callee's_params_arity
      ~return_arity:(Code_metadata.result_arity callee's_code_metadata)
      ~call_kind:
        (Call_kind.direct_function_call callee's_code_id inner_apply_alloc_mode)
      (Apply.dbg apply) ~inlined:(Apply.inlined apply)
      ~inlining_state:(Apply.inlining_state apply)
      ~probe:(Apply.probe apply) ~tail:(Apply.tail apply)
      ~position:(Apply.position apply)
      ~relative_history:(Apply.relative_history apply)
  in
  let both_applications =
    Let_cont.create_non_recursive after_full_application
      after_full_application_handler
      ~body:(Expr.create_apply full_apply)
      ~free_names_of_body:(Known (Apply.free_names full_apply))
  in
  match needs_region with
  | None -> both_applications
  | Some (region, _) ->
    Let.create
      (Bound_pattern.singleton (Bound_var.create region Name_mode.normal))
      (Named.create_prim (Nullary Begin_region) (Apply.dbg apply))
      ~body:both_applications
      ~free_names_of_body:
        (Known
           (NO.union
              (Apply.free_names full_apply)
              perform_over_application_free_names))
    |> Expr.create_let

type apply_cont_context =
  | Apply_cont_expr
  | Switch_branch

let apply_cont_use_kind ~context apply_cont : Continuation_use_kind.t =
  let default : Continuation_use_kind.t =
    match context with
    | Apply_cont_expr -> Inlinable
    | Switch_branch -> Non_inlinable { escaping = false }
  in
  match Continuation.sort (AC.continuation apply_cont) with
  | Normal_or_exn -> (
    match Apply_cont.trap_action apply_cont with
    | None -> default
    | Some (Push _) -> Non_inlinable { escaping = false }
    | Some (Pop { raise_kind; _ }) -> (
      match raise_kind with
      | None | Some Regular | Some Reraise ->
        (* Until such time as we can manually add to the backtrace buffer, we
           only convert "raise_notrace" into jumps, except if debugging
           information generation is disabled. (This matches the handling at Cmm
           level; see [Cmm_helpers.raise_prim].)

           We set [escaping = true] for the cases we do not want to convert into
           jumps. *)
        if Flambda_features.debug ()
        then Non_inlinable { escaping = true }
        else Non_inlinable { escaping = false }
      | Some No_trace -> Non_inlinable { escaping = false }))
  | Return | Toplevel_return -> Non_inlinable { escaping = false }
  | Define_root_symbol ->
    assert (Option.is_none (Apply_cont.trap_action apply_cont));
    default

let clear_demoted_trap_action uacc apply_cont : AC.t =
  match AC.trap_action apply_cont with
  | None -> apply_cont
  | Some (Push { exn_handler } | Pop { exn_handler; _ }) ->
    if UE.mem_continuation (UA.uenv uacc) exn_handler
       && not (UA.is_demoted_exn_handler uacc exn_handler)
    then apply_cont
    else AC.clear_trap_action apply_cont

let patch_unused_exn_bucket uacc apply_cont =
  if AC.is_raise apply_cont
  then
    match AC.args apply_cont with
    | [] -> assert false
    | exn_value :: other_args ->
      (* Note: [exn_value_is_used] is a global property, not one local to the
         continuation specified by [apply_cont]. In the event of one exception
         value propagating to (say) two different exception continuations, where
         only one of them uses the value, it will still be passed to both
         continuations. However this should be fine as the value still needed to
         be allocated anyway and some value still has to be passed (because of
         the calling convention of exception continuations). *)
      let exn_value_is_used =
        Simple.pattern_match
          ~const:(fun _ -> true)
          ~name:(fun name ~coercion:_ ->
            Name.Set.mem name (UA.required_names uacc))
          exn_value
      in
      if exn_value_is_used
      then apply_cont
      else
        (* The raise argument must be present, if it is unused, we replace it by
           a dummy value to avoid keeping a useless value alive *)
        let dummy_value = Simple.const_zero in
        AC.update_args ~args:(dummy_value :: other_args) apply_cont
  else apply_cont

let clear_demoted_trap_action_and_patch_unused_exn_bucket uacc apply_cont =
  let apply_cont = clear_demoted_trap_action uacc apply_cont in
  patch_unused_exn_bucket uacc apply_cont

(* Warning: This function relies on [T.meet_is_flat_float_array], which could
   return any kind for empty arrays. So this function is only safe for
   operations that are invalid on empty arrays. *)
let specialise_array_kind dacc (array_kind : P.Array_kind.t) ~array_ty :
    _ Or_bottom.t =
  let typing_env = DA.typing_env dacc in
  let for_naked_number kind : _ Or_bottom.t =
    match T.meet_is_naked_number_array typing_env array_ty kind with
    | Known_result true | Need_meet -> Ok array_kind
    | Known_result false | Invalid -> Bottom
  in
  match array_kind with
  | Naked_floats -> for_naked_number Naked_float
  | Naked_float32s -> for_naked_number Naked_float32
  | Naked_int32s -> for_naked_number Naked_int32
  | Naked_int64s -> for_naked_number Naked_int64
  | Naked_nativeints -> for_naked_number Naked_nativeint
  | Immediates -> (
    (* The only thing worth checking is for float arrays, as that would allow us
       to remove the branch *)
    match T.meet_is_naked_number_array typing_env array_ty Naked_float with
    | Known_result false | Need_meet -> Ok array_kind
    | Known_result true | Invalid -> Bottom)
  | Values -> (
    (* Try to specialise to immediates *)
    match T.prove_is_immediates_array typing_env array_ty with
    | Proved () ->
      (* Specialise the array operation to [Immediates]. *)
      Ok P.Array_kind.Immediates
    | Unknown -> (
      (* Check for float arrays *)
      match T.meet_is_naked_number_array typing_env array_ty Naked_float with
      | Known_result false | Need_meet -> Ok array_kind
      | Known_result true | Invalid -> Bottom))

let add_symbol_projection dacc ~projected_from projection ~projection_bound_to
    ~kind =
  if DE.at_unit_toplevel (DA.denv dacc)
  then dacc
  else
    let module SP = Symbol_projection in
    Simple.pattern_match' projected_from
      ~const:(fun _ -> dacc)
      ~symbol:(fun symbol_projected_from ~coercion:_ ->
        let proj = SP.create symbol_projected_from projection kind in
        let var = Bound_var.var projection_bound_to in
        DA.map_denv dacc ~f:(fun denv -> DE.add_symbol_projection denv var proj))
      ~var:(fun _ ~coercion:_ -> dacc)
