(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
open! Simplify_import

let fail_if_probe apply =
  match Apply.probe apply with
  | None -> ()
  | Some _ ->
    Misc.fatal_errorf
      "[Apply] terms with a [probe] (i.e. that call a tracing probe) must \
       always be direct applications of an OCaml function:@ %a"
      Apply.print apply

let record_free_names_of_apply_as_used0 apply ~use_id ~exn_cont_use_id data_flow
    =
  let data_flow =
    Flow.Acc.add_used_in_current_handler
      (Apply.free_names_without_exn_continuation apply)
      data_flow
  in
  let exn_cont = Apply.exn_continuation apply in
  let result_cont =
    match Apply.continuation apply, use_id with
    | Never_returns, None -> None
    | Return k, Some use_id -> Some (use_id, k)
    | Never_returns, Some _ | Return _, None -> assert false
  in
  Flow.Acc.add_apply_conts
    ~exn_cont:(exn_cont_use_id, exn_cont)
    ~result_cont ~result_arity:(Apply.return_arity apply) data_flow

let record_free_names_of_apply_as_used dacc ~use_id ~exn_cont_use_id apply =
  DA.map_flow_acc dacc
    ~f:(record_free_names_of_apply_as_used0 ~use_id ~exn_cont_use_id apply)

let loopify_decision_for_call dacc apply =
  let denv = DA.denv dacc in
  match DE.closure_info denv with
  | Not_in_a_closure | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    Loopify_state.do_not_loopify
  | Closure { return_continuation; exn_continuation; my_closure; _ } -> (
    let tenv = DE.typing_env denv in
    let[@inline always] canon simple =
      Simple.without_coercion (TE.get_canonical_simple_exn tenv simple)
    in
    match Apply.callee apply with
    | None -> Loopify_state.do_not_loopify
    | Some callee ->
      if Simple.equal (canon (Simple.var my_closure)) (canon callee)
         && (match Apply.continuation apply with
            | Never_returns ->
              (* If we never return, then this call is a tail-call *)
              true
            | Return apply_return_continuation ->
              Continuation.equal apply_return_continuation return_continuation)
         && Exn_continuation.equal
              (Apply.exn_continuation apply)
              (Exn_continuation.create ~exn_handler:exn_continuation
                 ~extra_args:[])
      then DE.loopify_state denv
      else Loopify_state.do_not_loopify)

let simplify_self_tail_call dacc apply self_cont ~down_to_up =
  Simplify_apply_cont_expr.simplify_apply_cont dacc
    (Apply_cont_expr.create self_cont ~args:(Apply.args apply)
       ~dbg:(Apply.dbg apply))
    ~down_to_up

let simplify_direct_tuple_application ~simplify_expr dacc apply
    ~apply_alloc_mode ~callee's_code_id ~callee's_code_metadata ~down_to_up =
  let dbg = Apply.dbg apply in
  let tuple_size =
    (* The code for the function being applied has exactly as many parameters as
       there are components of the tuple (which is the first element of
       [Apply.args apply]). The components must be of kind [Value] (in Lambda,
       [layout_field]) and therefore cannot be unboxed products themselves. *)
    Flambda_arity.cardinal_unarized
      (Code_metadata.params_arity callee's_code_metadata)
  in
  (* Split the tuple argument from any over application arguments *)
  let tuple_arg, over_application_args =
    match Apply.args apply with
    | tuple :: others -> tuple, others
    | _ ->
      Misc.fatal_errorf "Empty argument list for direct tuple application:@ %a"
        Apply.print apply
  in
  (* Create the list of variables and projections *)
  let vars_and_fields =
    List.init tuple_size (fun field ->
        ( Variable.create "tuple_field",
          Simplify_common.project_tuple ~dbg ~size:tuple_size ~field tuple_arg ))
  in
  (* Construct the arities for the tuple and any over application arguments *)
  let args_arity =
    let tuple_arity =
      Flambda_arity.create_singletons
        (List.init tuple_size (fun _ -> K.With_subkind.any_value))
    in
    let over_application_arity =
      (* Any over application might involve complex arities. *)
      Flambda_arity.partially_apply (Apply.args_arity apply)
        ~num_non_unarized_params_provided:1
    in
    Flambda_arity.concat tuple_arity over_application_arity
  in
  (* Change the application to operate on the fields of the tuple *)
  let apply =
    Apply.with_args apply
      (List.map (fun (v, _) -> Simple.var v) vars_and_fields
      @ over_application_args)
      ~args_arity
  in
  (* Immediately simplify over_applications to avoid having direct applications
     with the wrong arity. *)
  let apply_expr =
    match over_application_args with
    | [] -> Expr.create_apply apply
    | _ ->
      (* [apply] already got a correct relative_history and
         [split_direct_over_application] infers the relative history from the
         one on [apply] so there's nothing to do here. *)
      Simplify_common.split_direct_over_application apply ~apply_alloc_mode
        ~callee's_code_id ~callee's_code_metadata
  in
  (* Insert the projections and simplify the new expression, to allow field
     projections to be simplified, and over-application/full_application
     optimizations *)
  let expr =
    List.fold_right
      (fun (v, defining_expr) body ->
        let var_bind = Bound_var.create v Name_mode.normal in
        Let.create
          (Bound_pattern.singleton var_bind)
          defining_expr ~body ~free_names_of_body:Unknown
        |> Expr.create_let)
      vars_and_fields apply_expr
  in
  simplify_expr dacc expr ~down_to_up

let rebuild_non_inlined_direct_full_application apply ~use_id ~exn_cont_use_id
    ~result_arity ~coming_from_indirect ~callee's_code_metadata:_ uacc
    ~after_rebuild =
  let uacc =
    if coming_from_indirect
    then
      UA.notify_removed ~operation:Removed_operations.direct_call_of_indirect
        uacc
    else uacc
  in
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let erase_callee = false in
  (* CR ncourant: find out how we can erase the callee in simplify mode and
     still update to newer code pointers after resimplification. *)
  (* let erase_callee =
   *   match Apply.callee apply with
   *   | None -> false
   *   | Some callee ->
   *     Simple.is_symbol callee
   *     && not (Code_metadata.is_my_closure_used callee's_code_metadata)
   * in *)
  let apply = if erase_callee then Apply.erase_callee apply else apply in
  let uacc, expr =
    EB.rewrite_fixed_arity_apply uacc ~use_id result_arity apply
  in
  after_rebuild expr uacc

type inlining_decision =
  | Do_not_inline of { erase_attribute : bool }
  | Inline of DA.t * Expr.t

(* CR vlaviron: fetch [params_arity], [result_arity] and [result_types] from
   [callee's_code_metadata] to prevent using the wrong one by mistake *)
let simplify_direct_full_application ~simplify_expr dacc apply function_type
    ~params_arity ~result_arity ~(result_types : _ Or_unknown_or_bottom.t)
    ~down_to_up ~coming_from_indirect ~callee's_code_metadata =
  let inlined =
    match function_type with
    | None ->
      (* No rec info available, prevent inlining to avoid problems *)
      Do_not_inline { erase_attribute = false }
    | Some function_type -> (
      let decision =
        Call_site_inlining_decision.make_decision dacc ~simplify_expr ~apply
          ~function_type ~return_arity:result_arity
      in
      let unrolling_depth =
        Simplify_rec_info_expr.known_remaining_unrolling_depth dacc
          (Call_site_inlining_decision.get_rec_info dacc ~function_type)
      in
      if Are_rebuilding_terms.are_rebuilding
           (DE.are_rebuilding_terms (DA.denv dacc))
      then
        Inlining_report.record_decision_at_call_site_for_known_function
          ~pass:Inlining_report.Pass.Before_simplify ~unrolling_depth
          ~callee:(Code_metadata.absolute_history callee's_code_metadata)
          ~tracker:(DE.inlining_history_tracker (DA.denv dacc))
          ~are_rebuilding_terms:(DA.are_rebuilding_terms dacc)
          ~apply decision;
      match Call_site_inlining_decision_type.can_inline decision with
      | Do_not_inline { erase_attribute_if_ignored } ->
        Do_not_inline { erase_attribute = erase_attribute_if_ignored }
      | Inline { unroll_to; was_inline_always } ->
        let dacc, inlined =
          Inlining_transforms.inline dacc ~apply ~unroll_to ~was_inline_always
            function_type
        in
        Inline (dacc, inlined))
  in
  match inlined with
  | Inline (dacc, inlined) -> simplify_expr dacc inlined ~down_to_up
  | Do_not_inline { erase_attribute } -> (
    let apply =
      let inlined : Inlined_attribute.t =
        if erase_attribute
        then Default_inlined
        else
          Inlined_attribute.with_use_info (Apply.inlined apply)
            Unused_because_of_call_site_decision
      in
      Apply.with_inlined_attribute apply inlined
    in
    match loopify_decision_for_call dacc apply with
    | Loopify self_cont ->
      simplify_self_tail_call dacc apply self_cont ~down_to_up
    | Do_not_loopify ->
      let dacc, use_id, result_continuation =
        let result_continuation = Apply.continuation apply in
        match result_continuation, result_types with
        | Never_returns, (Unknown | Bottom | Ok _) | Return _, Bottom ->
          dacc, None, Apply.Result_continuation.Never_returns
        | Return apply_return_continuation, Unknown ->
          let dacc, use_id =
            DA.record_continuation_use dacc apply_return_continuation
              (Non_inlinable { escaping = true })
              ~env_at_use:(DA.denv dacc)
              ~arg_types:(T.unknown_types_from_arity result_arity)
          in
          dacc, Some use_id, result_continuation
        | Return apply_return_continuation, Ok result_types ->
          Result_types.pattern_match result_types
            ~f:(fun ~params ~results env_extension ->
              if Flambda_arity.cardinal_unarized params_arity
                 <> Bound_parameters.cardinal params
              then
                Misc.fatal_errorf
                  "Params arity %a does not match up with params in the result \
                   types structure:@ %a@ for application:@ %a"
                  Flambda_arity.print params_arity Result_types.print
                  result_types Apply.print apply;
              if Flambda_arity.cardinal_unarized result_arity
                 <> Bound_parameters.cardinal results
              then
                Misc.fatal_errorf
                  "Result arity %a does not match up with the result types \
                   structure:@ %a@ for application:@ %a"
                  Flambda_arity.print params_arity Result_types.print
                  result_types Apply.print apply;
              let denv = DA.denv dacc in
              let denv =
                DE.add_parameters_with_unknown_types
                  ~name_mode:Name_mode.in_types denv params
              in
              let params = Bound_parameters.to_list params in
              let results = Bound_parameters.to_list results in
              let denv =
                let args = Apply.args apply in
                assert (List.compare_lengths params args = 0);
                List.fold_left2
                  (fun denv param arg ->
                    DE.add_equation_on_variable denv (BP.var param)
                      (T.alias_type_of
                         (K.With_subkind.kind (BP.kind param))
                         arg))
                  denv params args
              in
              let result_arity =
                Flambda_arity.unarized_components result_arity
              in
              let denv =
                List.fold_left2
                  (fun denv kind result ->
                    DE.add_variable denv
                      (VB.create (BP.var result) NM.in_types)
                      (T.unknown_with_subkind kind))
                  denv result_arity results
              in
              let denv = DE.extend_typing_environment denv env_extension in
              (* Note: the result types of the application will go into a meet
                 with the kind information on the parameter(s) of the return
                 continuation (just like the normal [Apply_cont] case where the
                 meet is only done upon reaching the handler). *)
              let arg_types =
                List.map2
                  (fun kind result_var ->
                    T.alias_type_of (K.With_subkind.kind kind)
                      (BP.simple result_var))
                  result_arity results
              in
              let dacc = DA.with_denv dacc denv in
              let dacc, use_id =
                DA.record_continuation_use dacc apply_return_continuation
                  (Non_inlinable { escaping = true })
                  ~env_at_use:(DA.denv dacc) ~arg_types
              in
              dacc, Some use_id, result_continuation)
      in
      let dacc, exn_cont_use_id =
        DA.record_continuation_use dacc
          (Exn_continuation.exn_handler (Apply.exn_continuation apply))
          (Non_inlinable { escaping = true })
          ~env_at_use:(DA.denv dacc)
          ~arg_types:
            (T.unknown_types_from_arity
               (Exn_continuation.arity (Apply.exn_continuation apply)))
      in
      let apply = Apply.with_continuation apply result_continuation in
      let dacc =
        record_free_names_of_apply_as_used dacc ~use_id ~exn_cont_use_id apply
      in
      down_to_up dacc
        ~rebuild:
          (rebuild_non_inlined_direct_full_application apply ~use_id
             ~exn_cont_use_id ~result_arity ~coming_from_indirect
             ~callee's_code_metadata))

(* CR mshinwell: need to work out what to do for local alloc transformations
   when there are zero args. *)

let simplify_direct_partial_application ~simplify_expr dacc apply
    ~callee's_code_id ~callee's_code_metadata ~callee's_function_slot
    ~param_arity ~param_modes ~args_arity ~result_arity ~recursive ~down_to_up
    ~coming_from_indirect
    ~(closure_alloc_mode_from_type : Alloc_mode.For_types.t) ~apply_alloc_mode
    ~first_complex_local_param =
  (* Partial-applications are converted in full applications. Let's assume that
     [foo] takes 6 arguments. Then [foo a b c] gets transformed into:

     let foo_partial x y z = foo a b c x y z in foo_partial

     The call to [foo] as an empty relative history as it was defined right
     after [foo_partial]. The definition of [foo_partial] will inherit the
     relative history of the original code. *)
  (* For simplicity, we disallow [@inline] attributes on partial applications.
     The user may always write an explicit wrapper instead with such an
     attribute. *)
  (* CR-someday mshinwell: Pierre noted that we might like a function to be
     inlined when applied to its first set of arguments, e.g. for some kind of
     type class like thing. *)
  fail_if_probe apply;
  let args = Apply.args apply in
  let dbg = Apply.dbg apply in
  let apply_continuation =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error
        "cannot simplify a partial application that never returns"
    | Return continuation -> continuation
  in
  (match Apply.inlined apply with
  | Always_inlined _ | Never_inlined ->
    Location.prerr_warning
      (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible
         Inlining_helpers.(inlined_attribute_on_partial_application_msg Inlined))
  | Unroll _ ->
    Location.prerr_warning
      (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible
         Inlining_helpers.(
           inlined_attribute_on_partial_application_msg Unrolled))
  | Default_inlined | Hint_inlined -> ());
  let num_non_unarized_params = Flambda_arity.num_params param_arity in
  let num_non_unarized_args = Flambda_arity.num_params args_arity in
  assert (num_non_unarized_params > num_non_unarized_args);
  let remaining_param_arity =
    Flambda_arity.partially_apply param_arity
      ~num_non_unarized_params_provided:num_non_unarized_args
  in
  let applied_unarized_args, _ =
    Misc.Stdlib.List.map2_prefix
      (fun arg kind -> arg, kind)
      args
      (Flambda_arity.unarize param_arity)
  in
  let wrapper_var = Variable.create "partial_app" in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let wrapper_function_slot =
    Function_slot.create compilation_unit ~name:"partial_app_closure"
      K.With_subkind.any_value
  in
  (* The allocation mode of the closure is directly determined by the alloc_mode
     of the application. We check here that it is consistent with
     [first_complex_local_param]. *)
  let new_closure_alloc_mode, first_complex_local_param =
    if num_non_unarized_args <= first_complex_local_param
    then
      (* At this point, we *have* to allocate the closure on the heap, even if
         the alloc_mode of the application was local. Indeed, consider a
         three-argument function, of type [string -> string -> string ->
         string], coerced to [string -> local_ t] where [type t = string ->
         string -> string].

         If we apply this function twice to single arguments, the first
         application will have a local alloc_mode. However, the second
         application has a heap alloc_mode, and contains a reference to the
         partial closure made by the first application. Due to this, the first
         application must have a closure allocated on the heap as well, even
         though it was with a local alloc_mode. *)
      ( Alloc_mode.For_allocations.heap,
        first_complex_local_param - num_non_unarized_args )
    else
      match (apply_alloc_mode : Alloc_mode.For_allocations.t) with
      | Heap ->
        Misc.fatal_errorf
          "Partial application of %a with wrong mode:@.apply = \
           %a@callee's_code_metadata = %a@."
          Code_id.print callee's_code_id Apply.print apply Code_metadata.print
          callee's_code_metadata
      | Local _ -> apply_alloc_mode, 0
  in
  (match closure_alloc_mode_from_type with
  | Heap_or_local -> ()
  | Heap -> ()
  | Local -> (
    match (new_closure_alloc_mode : Alloc_mode.For_allocations.t) with
    | Local _ -> ()
    | Heap ->
      Misc.fatal_errorf
        "New closure alloc mode cannot be [Heap] when existing closure alloc \
         mode is [Local]: direct partial application:@ %a"
        Apply.print apply));
  let result_mode = Code_metadata.result_mode callee's_code_metadata in
  let wrapper_taking_remaining_args, dacc, code_id, code =
    let return_continuation = Continuation.create () in
    let remaining_params =
      List.map
        (fun kind ->
          let param = Variable.create "param" in
          Bound_parameter.create param kind)
        (Flambda_arity.unarize remaining_param_arity)
      |> Bound_parameters.create
    in
    let _, remaining_params_alloc_modes =
      Misc.Stdlib.List.split_at (List.length args) param_modes
    in
    let open struct
      (* An argument or the callee, with information about its entry in the
         closure, if any. If the argument is a constant or uncoerced symbol, we
         don't need to put it in the closure. *)
      (* CR-someday lmaurer: Also allow coerced symbols to be left out of the
         closure. Would require putting any depth variables in the closure,
         which is desirable but currently not possible. This workaround -
         binding the coerced symbol in the closure - wastes a bit of memory, and
         it has the effect of turning the callee from a symbol into a variable.
         Fortunately, the reconstituted [Apply_expr] should retain the original
         call kind, so it will remain a direct call. *)
      type applied_value =
        | Const of Reg_width_const.t
        | Symbol of Symbol.t
        | In_closure of
            { var : Variable.t;
              (* name to bind to projected variable *)
              value : Simple.t;
              (* value to store in closure *)
              value_slot : Value_slot.t
            }
    end in
    let mk_value_slot kind =
      Value_slot.create compilation_unit ~name:"arg" kind
    in
    let applied_value (value, kind) =
      Simple.pattern_match' value
        ~const:(fun const -> Const const)
        ~symbol:(fun symbol ~coercion ->
          if Coercion.is_id coercion
          then Symbol symbol
          else
            let var = Variable.create "symbol" in
            if not (K.equal (K.With_subkind.kind kind) K.value)
            then
              Misc.fatal_errorf
                "Simple %a which is a symbol should be of kind Value"
                Simple.print value;
            In_closure { var; value; value_slot = mk_value_slot kind })
        ~var:(fun var ~coercion:_ ->
          In_closure { var; value; value_slot = mk_value_slot kind })
    in
    let applied_callee =
      match Apply.callee apply with
      | None -> None
      | Some callee -> Some (applied_value (callee, K.With_subkind.any_value))
    in
    let applied_unarized_args = List.map applied_value applied_unarized_args in
    let applied_values =
      match applied_callee with
      | None -> applied_unarized_args
      | Some applied_callee -> applied_callee :: applied_unarized_args
    in
    let my_closure = Variable.create "my_closure" in
    let my_region = Variable.create "my_region" in
    let my_depth = Variable.create "my_depth" in
    let exn_continuation =
      Apply.exn_continuation apply |> Exn_continuation.without_extra_args
    in
    let apply_alloc_mode =
      Alloc_mode.For_allocations.from_lambda result_mode
        ~current_region:my_region
    in
    let call_kind =
      Call_kind.direct_function_call callee's_code_id apply_alloc_mode
    in
    let body, cost_metrics_of_body, free_names =
      (* [free_names] is going to be the free names of the whole resulting
         function params and body (i.e. as seen from outside the lambda). *)
      let arg = function
        | Const const -> Simple.const const
        | Symbol symbol -> Simple.symbol symbol
        | In_closure { var; _ } -> Simple.var var
      in
      let callee = Option.map arg applied_callee in
      let args =
        List.map arg applied_unarized_args
        @ Bound_parameters.simples remaining_params
      in
      let full_application =
        Apply.create ~callee ~continuation:(Return return_continuation)
          exn_continuation ~args ~args_arity:param_arity
          ~return_arity:result_arity ~call_kind dbg ~inlined:Default_inlined
          ~inlining_state:(Apply.inlining_state apply)
          ~position:Normal ~probe:None
          ~relative_history:Inlining_history.Relative.empty
      in
      let cost_metrics =
        Cost_metrics.from_size (Code_size.apply full_application)
      in
      List.fold_left
        (fun (expr, cost_metrics, free_names) applied_value ->
          match applied_value with
          | Const _ | Symbol _ -> expr, cost_metrics, free_names
          | In_closure { var; value_slot; value = _ } ->
            let arg = VB.create var Name_mode.normal in
            let prim =
              P.Unary
                ( Project_value_slot
                    { project_from = wrapper_function_slot; value_slot },
                  Simple.var my_closure )
            in
            let cost_metrics_of_defining_expr =
              Cost_metrics.from_size (Code_size.prim prim)
            in
            let free_names =
              NO.add_value_slot_in_projection free_names value_slot NM.normal
            in
            let expr =
              Let.create
                (Bound_pattern.singleton arg)
                (Named.create_prim prim dbg)
                ~body:expr ~free_names_of_body:Unknown
              |> Expr.create_let
            in
            ( expr,
              Cost_metrics.( + ) cost_metrics
                (Cost_metrics.increase_due_to_let_expr ~is_phantom:false
                   ~cost_metrics_of_defining_expr),
              free_names ))
        ( Expr.create_apply full_application,
          cost_metrics,
          Apply.free_names full_application |> NO.without_names_or_continuations
        )
        (List.rev applied_values)
    in
    let params_and_body =
      (* Note that [exn_continuation] has no extra args -- see above. *)
      Function_params_and_body.create ~return_continuation
        ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
        remaining_params ~body ~my_closure ~my_region ~my_depth
        ~free_names_of_body:Unknown
    in
    let name = Function_slot.to_string callee's_function_slot ^ "_partial" in
    let absolute_history, relative_history =
      DE.inlining_history_tracker (DA.denv dacc)
      |> Inlining_history.Tracker.fundecl
           ~function_relative_history:Inlining_history.Relative.empty ~dbg ~name
    in
    let code_id = Code_id.create ~name (Compilation_unit.get_current_exn ()) in
    (* We could create better result types by combining the types for the first
       arguments with the result types from the called function. However given
       that stubs are supposed to be inlined, and the inner full application
       will come with the expected result types, it's not going to be
       particularly useful. *)
    let code : Static_const_or_code.t =
      let code =
        Code.create code_id ~params_and_body
          ~free_names_of_params_and_body:free_names ~newer_version_of:None
          ~params_arity:remaining_param_arity
          ~param_modes:remaining_params_alloc_modes ~first_complex_local_param
          ~result_arity ~result_types:Unknown ~result_mode
          ~contains_no_escaping_local_allocs:
            (Code_metadata.contains_no_escaping_local_allocs
               callee's_code_metadata)
          ~stub:true ~inline:Default_inline ~poll_attribute:Default
          ~check:Check_attribute.Default_check ~is_a_functor:false
          ~is_opaque:false ~recursive ~cost_metrics:cost_metrics_of_body
          ~inlining_arguments:(DE.inlining_arguments (DA.denv dacc))
          ~dbg ~is_tupled:false
          ~is_my_closure_used:
            (Function_params_and_body.is_my_closure_used params_and_body)
          ~inlining_decision:Stub ~absolute_history ~relative_history
          ~loopify:Never_loopify
      in
      Static_const_or_code.create_code code
    in
    let function_decls =
      Function_declarations.create
        (Function_slot.Lmap.singleton wrapper_function_slot code_id)
    in
    let value_slots =
      List.filter_map
        (fun value ->
          match value with
          | Const _ | Symbol _ -> None
          | In_closure { value_slot; value; var = _ } -> Some (value_slot, value))
        applied_values
      |> Value_slot.Map.of_list
    in
    ( Set_of_closures.create ~value_slots new_closure_alloc_mode function_decls,
      dacc,
      code_id,
      code )
  in
  let apply_cont =
    Apply_cont.create apply_continuation ~args:[Simple.var wrapper_var] ~dbg
  in
  let expr =
    let wrapper_var = VB.create wrapper_var Name_mode.normal in
    let bound_vars = [wrapper_var] in
    let bound = Bound_pattern.set_of_closures bound_vars in
    let body =
      Let.create bound
        (Named.create_set_of_closures wrapper_taking_remaining_args)
        ~body:(Expr.create_apply_cont apply_cont)
        ~free_names_of_body:Unknown
      |> Expr.create_let
    in
    let bound_static =
      Bound_static.singleton (Bound_static.Pattern.code code_id)
    in
    let static_consts = Static_const_group.create [code] in
    (* Since we are only generating a "let code" binding and not a "let symbol",
       it doesn't matter if we are not at toplevel. *)
    Let.create
      (Bound_pattern.static bound_static)
      (Named.create_static_consts static_consts)
      ~body ~free_names_of_body:Unknown
    |> Expr.create_let
  in
  let down_to_up dacc ~rebuild =
    down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        let uacc =
          if coming_from_indirect
          then
            UA.notify_removed
              ~operation:Removed_operations.direct_call_of_indirect uacc
          else uacc
        in
        (* Increase the counter of calls as the original apply node was removed.
           [simplify] is called over the two apply nodes that were created to
           replace the original one so they will be taken into account in the
           cost metrics, mainly by increasing the code size. *)
        let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
        rebuild uacc ~after_rebuild)
  in
  simplify_expr dacc expr ~down_to_up

let simplify_direct_over_application ~simplify_expr dacc apply ~down_to_up
    ~coming_from_indirect ~apply_alloc_mode ~callee's_code_id
    ~callee's_code_metadata =
  fail_if_probe apply;
  let expr =
    Simplify_common.split_direct_over_application apply ~apply_alloc_mode
      ~callee's_code_id ~callee's_code_metadata
  in
  let down_to_up dacc ~rebuild =
    let rebuild uacc ~after_rebuild =
      (* Remove one function call as this apply was removed and replaced by two
         new ones. *)
      let uacc =
        if coming_from_indirect
        then
          UA.notify_removed
            ~operation:Removed_operations.direct_call_of_indirect uacc
        else uacc
      in
      let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
      rebuild uacc ~after_rebuild
    in
    down_to_up dacc ~rebuild
  in
  simplify_expr dacc expr ~down_to_up

let simplify_direct_function_call ~simplify_expr dacc apply
    ~callee's_code_id_from_type ~callee's_code_id_from_call_kind
    ~callee's_function_slot ~result_arity ~result_types ~recursive
    ~must_be_detupled ~closure_alloc_mode_from_type ~apply_alloc_mode
    function_decl ~down_to_up =
  (match Apply.probe apply, Apply.inlined apply with
  | None, _ | Some _, Never_inlined -> ()
  | Some _, (Hint_inlined | Unroll _ | Default_inlined | Always_inlined _) ->
    Misc.fatal_errorf
      "[Apply] terms with a [probe] (i.e. that call a tracing probe) must \
       always be marked as [Never_inline]:@ %a"
      Apply.print apply);
  let coming_from_indirect = Option.is_none callee's_code_id_from_call_kind in
  let callee's_code_id : _ Or_bottom.t =
    match callee's_code_id_from_call_kind with
    | None -> Ok callee's_code_id_from_type
    | Some callee's_code_id_from_call_kind ->
      let typing_env = DA.typing_env dacc in
      Code_age_relation.meet
        (TE.code_age_relation typing_env)
        ~resolver:(TE.code_age_relation_resolver typing_env)
        callee's_code_id_from_call_kind callee's_code_id_from_type
  in
  match callee's_code_id with
  | Bottom ->
    down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
        EB.rebuild_invalid uacc (Closure_type_was_invalid apply) ~after_rebuild)
  | Ok callee's_code_id ->
    let call_kind =
      Call_kind.direct_function_call callee's_code_id apply_alloc_mode
    in
    let apply = Apply.with_call_kind apply call_kind in
    let callee's_code_or_metadata =
      DE.find_code_exn (DA.denv dacc) callee's_code_id
    in
    let callee's_code_metadata =
      Code_or_metadata.code_metadata callee's_code_or_metadata
    in
    let params_arity = Code_metadata.params_arity callee's_code_metadata in
    (* A function declaration with [is_tupled = true] must be treated specially:

       - Direct calls adopt the normal calling convention of the code's body,
       i.e. that given by [Code.params_arity].

       - Indirect calls adopt the calling convention consisting of a single
       tuple argument, irrespective of what [Code.params_arity] says. *)
    if must_be_detupled
    then
      simplify_direct_tuple_application ~simplify_expr dacc apply
        ~apply_alloc_mode ~callee's_code_id ~callee's_code_metadata ~down_to_up
    else
      let args_arity = Apply.args_arity apply in
      let provided_num_args = Flambda_arity.num_params args_arity in
      let num_params = Flambda_arity.num_params params_arity in
      let result_arity_of_application = Apply.return_arity apply in
      if provided_num_args = num_params
      then (
        (* This check can only be performed for exact applications:

           - In the partial application case, the type checker should have
           specified kind Value as the return kind of the application
           (propagated through Lambda to this point), and it would be wrong to
           compare against the return arity of the fully-applied function.

           - In the overapplication case, the correct return arity is only
           present on the application expression, so all we can do is check that
           the function being overapplied returns kind Value. *)
        if not
             (Flambda_arity.equal_ignoring_subkinds result_arity
                result_arity_of_application)
        then
          Misc.fatal_errorf
            "Wrong return arity for direct OCaml function call\n\
            \     (expected %a, found  %a):@ %a" Flambda_arity.print
            result_arity Flambda_arity.print result_arity_of_application
            Apply.print apply;
        simplify_direct_full_application ~simplify_expr dacc apply
          (Some function_decl) ~params_arity ~result_arity ~result_types
          ~down_to_up ~coming_from_indirect ~callee's_code_metadata)
      else if provided_num_args > num_params
      then (
        (* See comment above. *)
        if not (Flambda_arity.is_one_param_of_kind_value result_arity)
        then
          Misc.fatal_errorf
            "Non-singleton-value return arity for overapplied OCaml function:@ \
             %a"
            Apply.print apply;
        simplify_direct_over_application ~simplify_expr dacc apply ~down_to_up
          ~coming_from_indirect ~apply_alloc_mode ~callee's_code_id
          ~callee's_code_metadata)
      else if provided_num_args > 0 && provided_num_args < num_params
      then (
        (* See comment above. *)
        if not
             (Flambda_arity.is_one_param_of_kind_value
                result_arity_of_application)
        then
          Misc.fatal_errorf
            "Non-singleton-value return arity for partially-applied OCaml \
             function:@ %a"
            Apply.print apply;
        simplify_direct_partial_application ~simplify_expr dacc apply
          ~callee's_code_id ~callee's_code_metadata ~callee's_function_slot
          ~param_arity:params_arity
          ~param_modes:(Code_metadata.param_modes callee's_code_metadata)
          ~args_arity ~result_arity ~recursive ~down_to_up ~coming_from_indirect
          ~closure_alloc_mode_from_type ~apply_alloc_mode
          ~first_complex_local_param:
            (Code_metadata.first_complex_local_param callee's_code_metadata))
      else
        Misc.fatal_errorf
          "Function with %d params when simplifying direct OCaml function call \
           with %d arguments: %a"
          num_params provided_num_args Apply.print apply

let rebuild_function_call_where_callee's_type_unavailable apply call_kind
    ~use_id ~exn_cont_use_id uacc ~after_rebuild =
  let apply =
    Apply.with_call_kind apply call_kind
    |> Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
  in
  let apply =
    Apply.with_inlined_attribute apply
      (Inlined_attribute.with_use_info (Apply.inlined apply)
         Unused_because_function_unknown)
  in
  let uacc, expr =
    EB.rewrite_fixed_arity_apply uacc ~use_id (Apply.return_arity apply) apply
  in
  after_rebuild expr uacc

let simplify_function_call_where_callee's_type_unavailable dacc apply
    (call : Call_kind.Function_call.t) ~apply_alloc_mode ~down_to_up =
  fail_if_probe apply;
  let denv = DA.denv dacc in
  if Are_rebuilding_terms.are_rebuilding (DE.are_rebuilding_terms denv)
  then
    Inlining_report.record_decision_at_call_site_for_unknown_function
      ~pass:Inlining_report.Pass.Before_simplify
      ~tracker:(DE.inlining_history_tracker denv)
      ~apply ();
  let env_at_use = denv in
  let dacc, use_id =
    match Apply.continuation apply with
    | Never_returns -> dacc, None
    | Return continuation ->
      let dacc, use_id =
        DA.record_continuation_use dacc continuation
          (Non_inlinable { escaping = true })
          ~env_at_use
          ~arg_types:(T.unknown_types_from_arity (Apply.return_arity apply))
      in
      dacc, Some use_id
  in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      (Non_inlinable { escaping = true })
      ~env_at_use:(DA.denv dacc)
      ~arg_types:
        (T.unknown_types_from_arity
           (Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let call_kind =
    match call with
    | Indirect_unknown_arity ->
      Call_kind.indirect_function_call_unknown_arity apply_alloc_mode
    | Indirect_known_arity ->
      Call_kind.indirect_function_call_known_arity apply_alloc_mode
    | Direct _code_id ->
      (* Some types have regressed in precision. Since this used to be a direct
         call, however, we know the function's arity even though we don't know
         which function it is. *)
      Call_kind.indirect_function_call_known_arity apply_alloc_mode
  in
  let dacc =
    record_free_names_of_apply_as_used ~use_id ~exn_cont_use_id dacc apply
  in
  down_to_up dacc
    ~rebuild:
      (rebuild_function_call_where_callee's_type_unavailable apply call_kind
         ~use_id ~exn_cont_use_id)

let simplify_function_call ~simplify_expr dacc apply ~callee_ty
    (call : Call_kind.Function_call.t) ~apply_alloc_mode ~down_to_up =
  (* Function declarations and params and body might not have the same calling
     convention. Currently the only case when it happens is for tupled
     functions. For such functions, the function_declaration declares a
     param_arity with a single argument (which is the tuple), whereas the code
     body takes an argument for each field of the tuple (the body is currified).

     When simplifying a function call, it can happen that we need to change the
     calling convention. Currently this only happens when we have a generic call
     (Indirect_unknown_arity), which uses the generic/function_declaration
     calling convention, but se simplify it into a direct call, which uses the
     callee's code calling convention. In this case, we need to "detuple" the
     call in order to correctly adapt to the change in calling convention. *)
  let call_must_be_detupled is_function_decl_tupled =
    match call with
    | Direct _ | Indirect_known_arity ->
      (* In these cases, the calling convention already used in the application
         being simplified is that of the code actually called. Thus we must not
         detuple the function. *)
      false
      (* In the indirect case, the calling convention used currently is the
         generic one. Thus we need to detuple the call iff the function
         declaration is tupled. *)
    | Indirect_unknown_arity -> is_function_decl_tupled
  in
  let type_unavailable () =
    simplify_function_call_where_callee's_type_unavailable dacc apply call
      ~apply_alloc_mode ~down_to_up
  in
  (* CR-someday mshinwell: Should this be using [meet_shape], like for
     primitives? *)
  let denv = DA.denv dacc in
  match callee_ty with
  | None -> (
    match call with
    | Direct callee's_code_id ->
      let callee's_code_or_metadata = DE.find_code_exn denv callee's_code_id in
      let callee's_code_metadata =
        Code_or_metadata.code_metadata callee's_code_or_metadata
      in
      simplify_direct_full_application ~simplify_expr dacc apply None
        ~params_arity:(Code_metadata.params_arity callee's_code_metadata)
        ~result_arity:(Code_metadata.result_arity callee's_code_metadata)
        ~result_types:(Code_metadata.result_types callee's_code_metadata)
        ~down_to_up ~coming_from_indirect:false ~callee's_code_metadata
    | Indirect_known_arity | Indirect_unknown_arity ->
      Misc.fatal_errorf
        "No callee provided for non-direct OCaml function call:@ %a" Apply.print
        apply)
  | Some callee_ty -> (
    match T.meet_single_closures_entry (DE.typing_env denv) callee_ty with
    | Known_result
        ( callee's_function_slot,
          closure_alloc_mode_from_type,
          _closures_entry,
          func_decl_type ) ->
      let callee's_code_id_from_call_kind =
        match call with
        | Direct code_id -> Some code_id
        | Indirect_unknown_arity | Indirect_known_arity -> None
      in
      let callee's_code_id_from_type = T.Function_type.code_id func_decl_type in
      let callee's_code_or_metadata =
        DE.find_code_exn denv callee's_code_id_from_type
      in
      let callee's_code_metadata =
        Code_or_metadata.code_metadata callee's_code_or_metadata
      in
      let must_be_detupled =
        call_must_be_detupled (Code_metadata.is_tupled callee's_code_metadata)
      in
      simplify_direct_function_call ~simplify_expr dacc apply
        ~callee's_code_id_from_type ~callee's_code_id_from_call_kind
        ~callee's_function_slot
        ~result_arity:(Code_metadata.result_arity callee's_code_metadata)
        ~result_types:(Code_metadata.result_types callee's_code_metadata)
        ~recursive:(Code_metadata.recursive callee's_code_metadata)
        ~must_be_detupled ~closure_alloc_mode_from_type ~apply_alloc_mode
        func_decl_type ~down_to_up
    | Need_meet -> type_unavailable ()
    | Invalid ->
      let rebuild uacc ~after_rebuild =
        let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
        EB.rebuild_invalid uacc (Closure_type_was_invalid apply) ~after_rebuild
      in
      down_to_up dacc ~rebuild)

let simplify_apply_shared dacc apply =
  let simplified_callee, callee_ty =
    match Apply.callee apply with
    | None -> None, None
    | Some callee ->
      let callee_ty = S.simplify_simple dacc callee ~min_name_mode:NM.normal in
      Some (T.get_alias_exn callee_ty), Some callee_ty
  in
  let { S.simples = args; simple_tys = arg_types } =
    S.simplify_simples dacc (Apply.args apply)
  in
  List.iter2
    (fun kind_with_subkind arg_type ->
      let kind = K.With_subkind.kind kind_with_subkind in
      if not (K.equal kind (T.kind arg_type))
      then
        Misc.fatal_errorf
          "Argument kind %a from arity does not match kind from type %a for \
           application:@ %a"
          K.print kind T.print arg_type Apply.print apply)
    (Flambda_arity.unarize (Apply.args_arity apply))
    arg_types;
  let inlining_state =
    Inlining_state.meet
      (DE.get_inlining_state (DA.denv dacc))
      (Apply.inlining_state apply)
  in
  let apply =
    Apply.create ~callee:simplified_callee
      ~continuation:(Apply.continuation apply)
      (Apply.exn_continuation apply)
      ~args ~args_arity:(Apply.args_arity apply)
      ~return_arity:(Apply.return_arity apply)
      ~call_kind:(Apply.call_kind apply)
      (DE.add_inlined_debuginfo (DA.denv dacc) (Apply.dbg apply))
      ~inlined:(Apply.inlined apply) ~inlining_state ~probe:(Apply.probe apply)
      ~position:(Apply.position apply)
      ~relative_history:
        (Inlining_history.Relative.concat
           ~earlier:(DE.relative_history (DA.denv dacc))
           ~later:(Apply.relative_history apply))
  in
  dacc, callee_ty, apply, arg_types

let rebuild_method_call apply ~use_id ~exn_cont_use_id uacc ~after_rebuild =
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let uacc, expr =
    EB.rewrite_fixed_arity_apply uacc ~use_id:(Some use_id)
      (Apply.return_arity apply) apply
  in
  after_rebuild expr uacc

let simplify_method_call dacc apply ~callee_ty ~kind:_ ~obj ~arg_types
    ~down_to_up =
  fail_if_probe apply;
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind)
  then
    Misc.fatal_errorf "Method call with callee of wrong kind %a: %a" K.print
      callee_kind T.print callee_ty;
  let apply_cont =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "Cannot simplify a method call that never returns"
    | Return continuation -> continuation
  in
  let denv = DA.denv dacc in
  DE.check_simple_is_bound denv obj;
  let args_arity = Apply.args_arity apply in
  let args_arity_from_types = T.arity_of_list arg_types in
  if not
       (Flambda_arity.equal_ignoring_subkinds args_arity_from_types
          (Flambda_arity.unarize_t args_arity))
  then
    Misc.fatal_errorf
      "Arity %a of [Apply] arguments doesn't match parameter arity %a of \
       method:@ %a"
      Flambda_arity.print args_arity Flambda_arity.print args_arity Apply.print
      apply;
  let dacc, use_id =
    DA.record_continuation_use dacc apply_cont
      (Non_inlinable { escaping = true })
      ~env_at_use:denv
      ~arg_types:(T.unknown_types_from_arity (Apply.return_arity apply))
  in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      (Non_inlinable { escaping = true })
      ~env_at_use:(DA.denv dacc)
      ~arg_types:
        (T.unknown_types_from_arity
           (Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let dacc =
    record_free_names_of_apply_as_used dacc ~use_id:(Some use_id)
      ~exn_cont_use_id apply
  in
  down_to_up dacc ~rebuild:(rebuild_method_call apply ~use_id ~exn_cont_use_id)

let rebuild_c_call apply ~use_id ~exn_cont_use_id ~return_arity uacc
    ~after_rebuild =
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let uacc, expr =
    EB.rewrite_fixed_arity_apply uacc ~use_id return_arity apply
  in
  after_rebuild expr uacc

let simplify_c_call ~simplify_expr dacc apply ~callee_ty ~arg_types ~down_to_up
    =
  fail_if_probe apply;
  let args_arity = Apply.args_arity apply in
  let return_arity = Apply.return_arity apply in
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind)
  then
    Misc.fatal_errorf "C callees must be of kind [Value], not %a: %a" K.print
      callee_kind T.print callee_ty;
  let args_arity_from_types = T.arity_of_list arg_types in
  if not
       (Flambda_arity.equal_ignoring_subkinds args_arity_from_types
          (Flambda_arity.unarize_t args_arity))
  then
    Misc.fatal_errorf
      "Arity %a of [Apply] arguments doesn't match parameter arity %a of C \
       callee:@ %a"
      Flambda_arity.print args_arity Flambda_arity.print args_arity Apply.print
      apply;
  let simplified =
    Simplify_extcall.simplify_extcall dacc apply ~callee_ty ~arg_types
  in
  match simplified with
  | Specialised (dacc, expr, operation) ->
    let down_to_up dacc ~rebuild =
      let rebuild uacc ~after_rebuild =
        let uacc = UA.notify_removed uacc ~operation in
        rebuild uacc ~after_rebuild
      in
      down_to_up dacc ~rebuild
    in
    simplify_expr dacc expr ~down_to_up
  | Unchanged { return_types } ->
    let dacc, use_id =
      match Apply.continuation apply with
      | Return apply_continuation ->
        let apply_continuation_arg_types =
          let from_arity = T.unknown_types_from_arity return_arity in
          match return_types with
          | Unknown -> from_arity
          | Known return_types ->
            assert (List.compare_lengths return_types from_arity = 0);
            return_types
        in
        let dacc, use_id =
          DA.record_continuation_use dacc apply_continuation
            (Non_inlinable { escaping = true })
            ~env_at_use:(DA.denv dacc) ~arg_types:apply_continuation_arg_types
        in
        dacc, Some use_id
      | Never_returns -> dacc, None
    in
    let dacc, exn_cont_use_id =
      DA.record_continuation_use dacc
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        (Non_inlinable { escaping = true })
        ~env_at_use:(DA.denv dacc)
        ~arg_types:
          (T.unknown_types_from_arity
             (Exn_continuation.arity (Apply.exn_continuation apply)))
    in
    let dacc =
      record_free_names_of_apply_as_used dacc ~use_id ~exn_cont_use_id apply
    in
    down_to_up dacc
      ~rebuild:(rebuild_c_call apply ~use_id ~exn_cont_use_id ~return_arity)
  | Invalid ->
    let rebuild uacc ~after_rebuild =
      let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
      EB.rebuild_invalid uacc (Closure_type_was_invalid apply) ~after_rebuild
    in
    down_to_up dacc ~rebuild

let simplify_apply ~simplify_expr dacc apply ~down_to_up =
  let dacc, callee_ty, apply, arg_types = simplify_apply_shared dacc apply in
  match Apply.call_kind apply with
  | Function { function_call; alloc_mode = apply_alloc_mode } ->
    simplify_function_call ~simplify_expr dacc apply ~callee_ty function_call
      ~apply_alloc_mode ~down_to_up
  | Method { kind; obj; alloc_mode = _ } ->
    let callee_ty =
      match callee_ty with
      | Some callee_ty -> callee_ty
      | None ->
        Misc.fatal_errorf "No callee provided for method call:@ %a" Apply.print
          apply
    in
    simplify_method_call dacc apply ~callee_ty ~kind ~obj ~arg_types ~down_to_up
  | C_call { alloc = _; is_c_builtin = _ } ->
    let callee_ty =
      match callee_ty with
      | Some callee_ty -> callee_ty
      | None ->
        Misc.fatal_errorf "No callee provided for C call:@ %a" Apply.print apply
    in
    simplify_c_call ~simplify_expr dacc apply ~callee_ty ~arg_types ~down_to_up
