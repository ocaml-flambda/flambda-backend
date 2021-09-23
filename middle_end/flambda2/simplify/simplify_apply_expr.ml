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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let fail_if_probe apply =
  match Apply.probe_name apply with
  | None -> ()
  | Some _ ->
    Misc.fatal_errorf
      "[Apply] terms with a [probe_name] (i.e. that call a tracing probe) must \
       always be direct applications of an OCaml function:@ %a"
      Apply.print apply

let warn_not_inlined_if_needed apply reason =
  match Apply.inline apply with
  | Hint_inline | Never_inline | Default_inline -> ()
  | Always_inline | Unroll _ ->
    Location.prerr_warning
      (Debuginfo.to_location (Apply.dbg apply))
      (Warnings.Inlining_impossible reason)

(* Note that this considers that the extra arguments of the exn_continuation are
   always used. *)
let record_free_names_of_apply_as_used0 apply data_flow =
  let data_flow =
    Data_flow.add_used_in_current_handler (Apply.free_names apply) data_flow
  in
  match Apply.continuation apply with
  | Never_returns -> data_flow
  | Return k -> Data_flow.add_apply_result_cont k data_flow

let record_free_names_of_apply_as_used dacc apply =
  DA.map_data_flow dacc ~f:(record_free_names_of_apply_as_used0 apply)

let simplify_direct_tuple_application ~simplify_expr dacc apply ~callee's_code
    ~down_to_up =
  let dbg = Apply.dbg apply in
  let param_arity = Code.params_arity callee's_code in
  let n = List.length param_arity in
  (* Split the tuple argument from other potential over application arguments *)
  let tuple, over_application_args =
    match Apply.args apply with
    | tuple :: others -> tuple, others
    | _ -> Misc.fatal_errorf "Empty argument list for direct application"
  in
  (* create the list of variables and projections *)
  let vars_and_fields =
    List.init n (fun i ->
        let var = Variable.create "tuple_field" in
        let e = Simplify_common.project_tuple ~dbg ~size:n ~field:i tuple in
        var, e)
  in
  (* Change the application to operate on the fields of the tuple *)
  let apply =
    Apply.with_args apply
    @@ List.map (fun (v, _) -> Simple.var v) vars_and_fields
    @ over_application_args
  in
  (* Immediately simplify over_applications to avoid having direct applications
     with the wrong arity. *)
  let apply_expr =
    match over_application_args with
    | [] -> Expr.create_apply apply
    | _ -> Simplify_common.split_direct_over_application apply ~param_arity
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
    ~result_arity ~coming_from_indirect uacc ~after_rebuild =
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
  let expr, uacc =
    match use_id with
    | None ->
      let uacc =
        UA.add_free_names uacc (Apply.free_names apply)
        |> UA.notify_added ~code_size:(Code_size.apply apply)
      in
      RE.create_apply (UA.are_rebuilding_terms uacc) apply, uacc
    | Some use_id ->
      EB.add_wrapper_for_fixed_arity_apply uacc ~use_id result_arity apply
  in
  after_rebuild expr uacc

let simplify_direct_full_application ~simplify_expr dacc apply function_decl
    ~callee's_code_id ~result_arity ~down_to_up ~coming_from_indirect =
  let inlined =
    (* CR mshinwell: Make sure no other warnings or inlining report decisions
       get emitted when not rebuilding terms. *)
    let decision =
      Call_site_inlining_decision.make_decision dacc ~simplify_expr ~apply
        ~function_decl ~return_arity:result_arity
    in
    Inlining_report.record_decision
      (At_call_site
         (Known_function { code_id = Code_id.export callee's_code_id; decision }))
      ~dbg:(DE.add_inlined_debuginfo' (DA.denv dacc) (Apply.dbg apply));
    match Call_site_inlining_decision.can_inline decision with
    | Do_not_inline { warn_if_attribute_ignored; because_of_definition } ->
      (* emission of the warning at this point should not happen, if it does,
         then that means that {Inlining_decision.make_decision_for_call_site}
         did not honour the attributes on the call site *)
      if warn_if_attribute_ignored && not (DA.do_not_rebuild_terms dacc)
      then
        if because_of_definition
        then
          warn_not_inlined_if_needed apply
            "[@inlined] attribute was not used on this function application \
             (the optimizer decided not to inline the function given its \
             definition)"
        else
          warn_not_inlined_if_needed apply
            "[@inlined] attribute was not used on this function \
             application{Do_not_inline}";
      None
    | Inline { unroll_to } ->
      let dacc, inlined =
        Inlining_transforms.inline dacc ~apply ~unroll_to function_decl
      in
      Some (dacc, inlined)
  in
  match inlined with
  | Some (dacc, inlined) -> simplify_expr dacc inlined ~down_to_up
  | None ->
    let dacc = record_free_names_of_apply_as_used dacc apply in
    let dacc, use_id =
      match Apply.continuation apply with
      | Never_returns -> dacc, None
      | Return apply_return_continuation ->
        let dacc, use_id =
          DA.record_continuation_use dacc apply_return_continuation
            (Non_inlinable { escaping = true })
            ~env_at_use:(DA.denv dacc)
            ~arg_types:(T.unknown_types_from_arity_with_subkinds result_arity)
        in
        dacc, Some use_id
    in
    let dacc, exn_cont_use_id =
      DA.record_continuation_use dacc
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        (Non_inlinable { escaping = true })
        ~env_at_use:(DA.denv dacc)
        ~arg_types:
          (T.unknown_types_from_arity_with_subkinds
             (Exn_continuation.arity (Apply.exn_continuation apply)))
    in
    down_to_up dacc
      ~rebuild:
        (rebuild_non_inlined_direct_full_application apply ~use_id
           ~exn_cont_use_id ~result_arity ~coming_from_indirect)

let simplify_direct_partial_application ~simplify_expr dacc apply
    ~callee's_code_id ~callee's_closure_id ~param_arity ~result_arity ~recursive
    ~down_to_up ~coming_from_indirect =
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
  begin
    match Apply.inline apply with
    | Always_inline | Never_inline ->
      Location.prerr_warning
        (Debuginfo.to_location dbg)
        (Warnings.Inlining_impossible
           "[@inlined] attributes may not be used on partial applications")
    | Unroll _ ->
      Location.prerr_warning
        (Debuginfo.to_location dbg)
        (Warnings.Inlining_impossible
           "[@unroll] attributes may not be used on partial applications")
    | Default_inline | Hint_inline -> ()
  end;
  let arity = List.length param_arity in
  assert (arity > List.length args);
  let applied_args, remaining_param_arity =
    Misc.Stdlib.List.map2_prefix
      (fun arg kind ->
        if not (K.equal (K.With_subkind.kind kind) K.value)
        then
          Misc.fatal_errorf "Non-[value] kind in partial application: %a"
            Apply.print apply;
        arg)
      args param_arity
  in
  if not (Flambda_arity.With_subkinds.is_singleton_value result_arity)
  then
    Misc.fatal_errorf
      "Partially-applied function with non-[value] return kind: %a" Apply.print
      apply;
  let wrapper_var = Variable.create "partial_app" in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let wrapper_closure_id =
    Closure_id.wrap compilation_unit (Variable.create "partial_app_closure")
  in
  let wrapper_taking_remaining_args, dacc, code_id, code =
    let return_continuation = Continuation.create () in
    let remaining_params =
      List.map
        (fun kind ->
          let param = Variable.create "param" in
          Kinded_parameter.create param kind)
        remaining_param_arity
    in
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let open struct
      (* An argument or the callee, with information about its entry in the
         closure, if any. If the argument is a constant or uncoerced symbol, we
         don't need to put it in the closure. *)
      (* CR lmaurer: Also allow coerced symbols to be left out of the closure.
         Would require putting any depth variables in the closure, which is
         desirable but currently not possible. This workaround - binding the
         coerced symbol in the closure - wastes a bit of memory, and it has the
         effect of turning the callee from a symbol into a variable.
         Fortunately, the reconstituted [Apply_expr] should retain the original
         call kind, so it will remain a direct call. *)
      type applied_value =
        | Const of Reg_width_things.Const.t
        | Symbol of Symbol.t
        | In_closure of
            { var : Variable.t;
              (* name to bind to projected variable *)
              value : Simple.t;
              (* value to store in closure *)
              closure_var : Var_within_closure.t
            }
    end in
    let mk_closure_var () =
      Var_within_closure.wrap compilation_unit (Variable.create "arg")
    in
    let applied_value value =
      Simple.pattern_match' value
        ~const:(fun const -> Const const)
        ~symbol:(fun symbol ~coercion ->
          if Coercion.is_id coercion
          then Symbol symbol
          else
            let var = Variable.create "symbol" in
            In_closure { var; value; closure_var = mk_closure_var () })
        ~var:(fun var ~coercion:_ ->
          In_closure { var; value; closure_var = mk_closure_var () })
    in
    let applied_callee = applied_value (Apply.callee apply) in
    let applied_args = List.map applied_value applied_args in
    let applied_values = applied_callee :: applied_args in
    let my_closure = Variable.create "my_closure" in
    let my_depth = Variable.create "my_depth" in
    let exn_continuation =
      Apply.exn_continuation apply |> Exn_continuation.without_extra_args
    in
    let body, cost_metrics_of_body =
      let arg = function
        | Const const -> Simple.const const
        | Symbol symbol -> Simple.symbol symbol
        | In_closure { var; _ } -> Simple.var var
      in
      let callee = arg applied_callee in
      let args =
        List.map arg applied_args @ List.map KP.simple remaining_params
      in
      let full_application =
        Apply.create ~callee ~continuation:(Return return_continuation)
          exn_continuation ~args ~call_kind dbg ~inline:Default_inline
          ~inlining_state:(Apply.inlining_state apply)
          ~probe_name:None
      in
      let cost_metrics =
        Cost_metrics.from_size (Code_size.apply full_application)
      in
      List.fold_left
        (fun (expr, cost_metrics) applied_value ->
          match applied_value with
          | Const _ | Symbol _ -> expr, cost_metrics
          | In_closure { var; closure_var; value = _ } ->
            let arg = VB.create var Name_mode.normal in
            let prim =
              P.Unary
                ( Project_var
                    { project_from = wrapper_closure_id; var = closure_var },
                  Simple.var my_closure )
            in
            let cost_metrics_of_defining_expr =
              Cost_metrics.from_size (Code_size.prim prim)
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
                   ~cost_metrics_of_defining_expr) ))
        (Expr.create_apply full_application, cost_metrics)
        (List.rev applied_values)
    in
    let params_and_body =
      Function_params_and_body.create ~return_continuation exn_continuation
        remaining_params ~body ~dbg ~my_closure ~my_depth
        ~free_names_of_body:Unknown
    in
    let code_id =
      Code_id.create
        ~name:(Closure_id.to_string callee's_closure_id ^ "_partial")
        (Compilation_unit.get_current_exn ())
    in
    let code =
      let free_names = Function_params_and_body.free_names params_and_body in
      let code =
        Code.create code_id
          ~params_and_body:(Present (params_and_body, free_names))
          ~newer_version_of:None
          ~params_arity:(KP.List.arity_with_subkinds remaining_params)
          ~result_arity ~stub:true ~inline:Default_inline ~is_a_functor:false
          ~recursive ~cost_metrics:cost_metrics_of_body
          ~inlining_arguments:(DE.inlining_arguments (DA.denv dacc))
          ~dbg ~is_tupled:false ~inlining_decision:Stub
      in
      Static_const.Code code
    in
    let function_decls =
      Function_declarations.create
        (Closure_id.Lmap.singleton wrapper_closure_id code_id)
    in
    let closure_elements =
      List.filter_map
        (fun value ->
          match value with
          | Const _ | Symbol _ -> None
          | In_closure { closure_var; value; var = _ } ->
            Some (closure_var, value))
        applied_values
      |> Var_within_closure.Map.of_list
    in
    Set_of_closures.create function_decls ~closure_elements, dacc, code_id, code
  in
  let apply_cont =
    Apply_cont.create apply_continuation ~args:[Simple.var wrapper_var] ~dbg
  in
  let expr =
    let wrapper_var = VB.create wrapper_var Name_mode.normal in
    let closure_vars = [wrapper_var] in
    let bound = Bound_pattern.set_of_closures ~closure_vars in
    let body =
      Let.create bound
        (Named.create_set_of_closures wrapper_taking_remaining_args)
        ~body:(Expr.create_apply_cont apply_cont)
        ~free_names_of_body:Unknown
      |> Expr.create_let
    in
    let bound_symbols =
      Bound_symbols.singleton (Bound_symbols.Pattern.code code_id)
    in
    let static_consts = Static_const.Group.create [code] in
    (* Since we are only generating a "let code" binding and not a "let symbol",
       it doesn't matter if we are not at toplevel. *)
    Let.create
      (Bound_pattern.symbols bound_symbols)
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

(* CR mshinwell: Should it be an error to encounter a non-direct application of
   a symbol after [Simplify]? This shouldn't usually happen, but I'm not 100%
   sure it cannot in every case. *)

let simplify_direct_over_application ~simplify_expr dacc apply ~param_arity
    ~result_arity:_ ~down_to_up ~coming_from_indirect =
  fail_if_probe apply;
  let expr = Simplify_common.split_direct_over_application apply ~param_arity in
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
    ~callee's_closure_id ~result_arity ~recursive ~arg_types:_ ~must_be_detupled
    function_decl ~down_to_up ~type_unavailable =
  begin
    match Apply.probe_name apply, Apply.inline apply with
    | None, _ | Some _, Never_inline -> ()
    | Some _, (Hint_inline | Unroll _ | Default_inline | Always_inline) ->
      Misc.fatal_errorf
        "[Apply] terms with a [probe_name] (i.e. that call a tracing probe) \
         must always be marked as [Never_inline]:@ %a"
        Apply.print apply
  end;
  let result_arity_of_application =
    Call_kind.return_arity (Apply.call_kind apply)
  in
  if not
       (Flambda_arity.With_subkinds.compatible result_arity
          ~when_used_at:result_arity_of_application)
  then
    Misc.fatal_errorf
      "Wrong return arity for direct OCaml function call (expected %a, found \
       %a):@ %a"
      Flambda_arity.With_subkinds.print result_arity
      Flambda_arity.With_subkinds.print result_arity_of_application Apply.print
      apply;
  let coming_from_indirect = callee's_code_id_from_call_kind = None in
  let callee's_code_id : _ Or_bottom.t =
    match callee's_code_id_from_call_kind with
    | None -> Ok callee's_code_id_from_type
    | Some callee's_code_id_from_call_kind ->
      let code_age_rel = TE.code_age_relation (DA.typing_env dacc) in
      let resolver = TE.code_age_relation_resolver (DA.typing_env dacc) in
      Code_age_relation.meet code_age_rel ~resolver
        callee's_code_id_from_call_kind callee's_code_id_from_type
  in
  match callee's_code_id with
  | Bottom ->
    down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
        EB.rebuild_invalid uacc ~after_rebuild)
  | Ok callee's_code_id -> (
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let apply = Apply.with_call_kind apply call_kind in
    match DE.find_code (DA.denv dacc) callee's_code_id with
    | None -> type_unavailable ()
    | Some callee's_code ->
      if must_be_detupled
      then
        simplify_direct_tuple_application ~simplify_expr dacc apply
          ~callee's_code ~down_to_up
      else
        let args = Apply.args apply in
        let provided_num_args = List.length args in
        (* A function declaration with [is_tupled = true] must be treated
           specially:

           - Direct calls adopt the normal calling convention of the code's
           body, i.e. that given by [Code.params_arity].

           - Indirect calls adopt the calling convention consisting of a single
           tuple argument, irrespective of what [Code.params_arity] says. *)
        let param_arity = Code.params_arity callee's_code in
        let num_params = List.length param_arity in
        if provided_num_args = num_params
        then
          simplify_direct_full_application ~simplify_expr dacc apply
            function_decl ~callee's_code_id ~result_arity ~down_to_up
            ~coming_from_indirect
        else if provided_num_args > num_params
        then
          simplify_direct_over_application ~simplify_expr dacc apply
            ~param_arity ~result_arity ~down_to_up ~coming_from_indirect
        else if provided_num_args > 0 && provided_num_args < num_params
        then
          simplify_direct_partial_application ~simplify_expr dacc apply
            ~callee's_code_id ~callee's_closure_id ~param_arity ~result_arity
            ~recursive ~down_to_up ~coming_from_indirect
        else
          Misc.fatal_errorf
            "Function with %d params when simplifying direct OCaml function \
             call with %d arguments: %a"
            num_params provided_num_args Apply.print apply)

let rebuild_function_call_where_callee's_type_unavailable apply call_kind
    ~use_id ~exn_cont_use_id uacc ~after_rebuild =
  let apply =
    Apply.with_call_kind apply call_kind
    |> Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
  in
  let expr, uacc =
    EB.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Call_kind.return_arity call_kind)
      apply
  in
  after_rebuild expr uacc

let simplify_function_call_where_callee's_type_unavailable dacc apply
    (call : Call_kind.Function_call.t) ~args:_ ~arg_types ~down_to_up =
  fail_if_probe apply;
  let cont =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "cannot simplify an application that never returns"
    | Return continuation -> continuation
  in
  let denv = DA.denv dacc in
  Inlining_report.record_decision (At_call_site Unknown_function)
    ~dbg:(DE.add_inlined_debuginfo' denv (Apply.dbg apply));
  let env_at_use = denv in
  let dacc = record_free_names_of_apply_as_used dacc apply in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      (Non_inlinable { escaping = true })
      ~env_at_use:(DA.denv dacc)
      ~arg_types:
        (T.unknown_types_from_arity_with_subkinds
           (Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let check_return_arity_and_record_return_cont_use ~return_arity =
    (* let cont_arity = DA.continuation_arity dacc cont in if not
       (Flambda_arity.equal return_arity cont_arity) then begin
       Misc.fatal_errorf "Return arity (%a) on application's continuation@ \
       doesn't match return arity (%a) specified in [Call_kind]:@ %a"
       Flambda_arity.print cont_arity Flambda_arity.print return_arity
       Apply.print apply end; *)
    DA.record_continuation_use dacc cont
      (Non_inlinable { escaping = true })
      ~env_at_use
      ~arg_types:(T.unknown_types_from_arity_with_subkinds return_arity)
  in
  let call_kind, use_id, dacc =
    match call with
    | Indirect_unknown_arity ->
      let dacc, use_id =
        DA.record_continuation_use dacc cont
          (Non_inlinable { escaping = true })
          ~env_at_use ~arg_types:[T.any_value ()]
      in
      Call_kind.indirect_function_call_unknown_arity (), use_id, dacc
    | Indirect_known_arity { param_arity; return_arity } ->
      let args_arity =
        T.arity_of_list arg_types
        |> List.map (fun kind -> K.With_subkind.create kind Anything)
      in
      if not
           (Flambda_arity.With_subkinds.compatible args_arity
              ~when_used_at:param_arity)
      then
        Misc.fatal_errorf
          "Argument arity on indirect-known-arity application doesn't match \
           [Call_kind] (expected %a, found %a):@ %a"
          Flambda_arity.With_subkinds.print param_arity
          Flambda_arity.With_subkinds.print args_arity Apply.print apply;
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity ~return_arity
      in
      call_kind, use_id, dacc
    | Direct { return_arity; _ } ->
      let param_arity =
        T.arity_of_list arg_types
        |> List.map (fun kind -> K.With_subkind.create kind Anything)
      in
      (* Some types have regressed in precision. Since this used to be a direct
         call, however, we know the function's arity even though we don't know
         which function it is. *)
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity ~return_arity
      in
      call_kind, use_id, dacc
  in
  down_to_up dacc
    ~rebuild:
      (rebuild_function_call_where_callee's_type_unavailable apply call_kind
         ~use_id ~exn_cont_use_id)

(* CR mshinwell: I've seen at least one case where a call of kind
   [Indirect_unknown_arity] has been generated with no warning, despite having
   [@inlined always]. *)

let simplify_function_call ~simplify_expr dacc apply ~callee_ty
    (call : Call_kind.Function_call.t) ~arg_types ~down_to_up =
  let args = Apply.args apply in
  (* Function declarations and params and body might not have the same calling
     convention. Currently the only case when it happens is for tupled
     functions. For such functions, the function_declaration declares a
     param_arity with a single argument (which is the tuple), whereas the code
     body takes an argument for each field of the tuple (the body is currified).

     When simplifying a function call, it can happen that we need to change the
     calling convention. Currently this only happens when we have a generic call
     (indirect_unknown_arity), which uses the generic/function_declaration
     calling convention, but se simplify it into a direct call, which uses the
     callee's code calling convention. In this case, we need to "detuple" the
     call in order to correctly adopt to the change in calling convention. *)
  let call_must_be_detupled is_function_decl_tupled =
    match call with
    | Direct _ | Indirect_known_arity _ ->
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
    if not (DA.do_not_rebuild_terms dacc)
    then
      warn_not_inlined_if_needed apply
        "[@inlined] attribute was not used on this function application (the \
         optimizer did not know what function was being applied)";
    simplify_function_call_where_callee's_type_unavailable dacc apply call ~args
      ~arg_types ~down_to_up
  in
  (* CR mshinwell: Should this be using [meet_shape], like for primitives? *)
  let denv = DA.denv dacc in
  match T.prove_single_closures_entry (DE.typing_env denv) callee_ty with
  | Proved (callee's_closure_id, _closures_entry, func_decl_type) -> begin
    (* CR mshinwell: We should check that the [set_of_closures] in the
       [closures_entry] structure in the type does indeed contain the closure in
       question. *)
    match func_decl_type with
    | Ok func_decl_type0 -> (
      let module I = T.Function_declaration_type.T0 in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; closure_id; _ } ->
          if not (Closure_id.equal closure_id callee's_closure_id)
          then
            Misc.fatal_errorf
              "Closure ID %a in application doesn't match closure ID %a \
               discovered via typing.@ Application:@ %a"
              Closure_id.print closure_id Closure_id.print callee's_closure_id
              Apply.print apply;
          Some code_id
        | Indirect_unknown_arity | Indirect_known_arity _ -> None
      in
      let callee's_code_id_from_type = I.code_id func_decl_type0 in
      match DE.find_code denv callee's_code_id_from_type with
      | None ->
        (* This will happen e.g. if the code age relation used in [meet] and
           [join] for function declaration types returns [Deleted] code for a
           code ID in another unit. *)
        type_unavailable ()
      | Some callee's_code ->
        let must_be_detupled =
          call_must_be_detupled (Code.is_tupled callee's_code)
        in
        simplify_direct_function_call ~simplify_expr dacc apply
          ~callee's_code_id_from_type ~callee's_code_id_from_call_kind
          ~callee's_closure_id ~arg_types
          ~result_arity:(Code.result_arity callee's_code)
          ~recursive:(Code.recursive callee's_code)
          ~must_be_detupled func_decl_type0 ~down_to_up ~type_unavailable)
    | Bottom ->
      let rebuild uacc ~after_rebuild =
        let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
        EB.rebuild_invalid uacc ~after_rebuild
      in
      down_to_up dacc ~rebuild
    | Unknown -> type_unavailable ()
  end
  | Unknown -> type_unavailable ()
  | Invalid ->
    let rebuild uacc ~after_rebuild =
      let uacc = UA.notify_removed ~operation:Removed_operations.call uacc in
      EB.rebuild_invalid uacc ~after_rebuild
    in
    down_to_up dacc ~rebuild

let simplify_apply_shared dacc apply =
  let callee_ty =
    S.simplify_simple dacc (Apply.callee apply) ~min_name_mode:NM.normal
  in
  let simplified_callee = T.get_alias_exn callee_ty in
  let { S.simples = args; simple_tys = arg_types } =
    S.simplify_simples dacc (Apply.args apply)
  in
  let inlining_state =
    Inlining_state.meet
      (DE.get_inlining_state (DA.denv dacc))
      (Apply.inlining_state apply)
  in
  let apply =
    Apply.create ~callee:simplified_callee
      ~continuation:(Apply.continuation apply)
      (Apply.exn_continuation apply)
      ~args ~call_kind:(Apply.call_kind apply)
      (DE.add_inlined_debuginfo' (DA.denv dacc) (Apply.dbg apply))
      ~inline:(Apply.inline apply) ~inlining_state
      ~probe_name:(Apply.probe_name apply)
  in
  dacc, callee_ty, apply, arg_types

let rebuild_method_call apply ~use_id ~exn_cont_use_id uacc ~after_rebuild =
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr, uacc =
    EB.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Flambda_arity.With_subkinds.create [K.With_subkind.any_value])
      apply
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
      Misc.fatal_error "cannot simplify a method call that never returns"
    | Return continuation -> continuation
  in
  let denv = DA.denv dacc in
  DE.check_simple_is_bound denv obj;
  let expected_arity = List.map (fun _ -> K.value) arg_types in
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal expected_arity args_arity)
  then
    Misc.fatal_errorf
      "All arguments to a method call must be of kind [value]:@ %a" Apply.print
      apply;
  let dacc = record_free_names_of_apply_as_used dacc apply in
  let dacc, use_id =
    DA.record_continuation_use dacc apply_cont
      (Non_inlinable { escaping = true })
      ~env_at_use:denv ~arg_types:[T.any_value ()]
  in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      (Non_inlinable { escaping = true })
      ~env_at_use:(DA.denv dacc)
      ~arg_types:
        (T.unknown_types_from_arity_with_subkinds
           (Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  (* CR mshinwell: Need to record exception continuation use (check all other
     cases like this too) *)
  down_to_up dacc ~rebuild:(rebuild_method_call apply ~use_id ~exn_cont_use_id)

let rebuild_c_call apply ~use_id ~exn_cont_use_id ~return_arity uacc
    ~after_rebuild =
  (* CR mshinwell: Make sure that [resolve_continuation_aliases] has been called
     before building of any term that contains a continuation *)
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr, uacc =
    match use_id with
    | Some use_id ->
      EB.add_wrapper_for_fixed_arity_apply uacc ~use_id
        (Flambda_arity.With_subkinds.of_arity return_arity)
        apply
    | None ->
      let uacc =
        UA.add_free_names uacc (Apply.free_names apply)
        |> UA.notify_added ~code_size:(Code_size.apply apply)
      in
      RE.create_apply (UA.are_rebuilding_terms uacc) apply, uacc
  in
  after_rebuild expr uacc

let simplify_c_call ~simplify_expr dacc apply ~callee_ty ~param_arity
    ~return_arity ~arg_types ~down_to_up =
  fail_if_probe apply;
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind)
  then
    Misc.fatal_errorf "C callees must be of kind [value], not %a: %a" K.print
      callee_kind T.print callee_ty;
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal args_arity param_arity)
  then
    Misc.fatal_errorf
      "Arity %a of [Apply] arguments doesn't match parameter arity %a of C \
       callee:@ %a"
      Flambda_arity.print args_arity Flambda_arity.print param_arity Apply.print
      apply;
  (* CR mshinwell: We can't do these checks (here and elsewhere) on [DA] any
     more. Maybe we can check on [UA] after calling [k] instead. let cont =
     Apply.continuation apply in let cont_arity = DA.continuation_arity dacc
     cont in if not (Flambda_arity.equal cont_arity return_arity) then begin
     Misc.fatal_errorf "Arity %a of [Apply] continuation doesn't match \ return
     arity %a of C callee:@ %a" Flambda_arity.print cont_arity
     Flambda_arity.print return_arity Apply.print apply end; *)
  let simplified =
    Simplify_extcall.simplify_extcall dacc apply ~callee_ty ~param_arity
      ~return_arity ~arg_types
  in
  match simplified with
  | Poly_compare_specialized (dacc, expr) ->
    let down_to_up dacc ~rebuild =
      let rebuild uacc ~after_rebuild =
        let uacc =
          UA.notify_removed uacc
            ~operation:Removed_operations.specialized_poly_compare
        in
        rebuild uacc ~after_rebuild
      in
      down_to_up dacc ~rebuild
    in
    simplify_expr dacc expr ~down_to_up
  | Unchanged ->
    let dacc = record_free_names_of_apply_as_used dacc apply in
    let dacc, use_id =
      match Apply.continuation apply with
      | Return apply_continuation ->
        let dacc, use_id =
          DA.record_continuation_use dacc apply_continuation
            (Non_inlinable { escaping = true })
            ~env_at_use:(DA.denv dacc)
            ~arg_types:(T.unknown_types_from_arity return_arity)
        in
        dacc, Some use_id
      | Never_returns -> dacc, None
    in
    let dacc, exn_cont_use_id =
      (* CR mshinwell: Try to factor out these stanzas, here and above. *)
      DA.record_continuation_use dacc
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        (Non_inlinable { escaping = true })
        ~env_at_use:(DA.denv dacc)
        ~arg_types:
          (T.unknown_types_from_arity_with_subkinds
             (Exn_continuation.arity (Apply.exn_continuation apply)))
    in
    down_to_up dacc
      ~rebuild:(rebuild_c_call apply ~use_id ~exn_cont_use_id ~return_arity)

let simplify_apply ~simplify_expr dacc apply ~down_to_up =
  let dacc, callee_ty, apply, arg_types = simplify_apply_shared dacc apply in
  match Apply.call_kind apply with
  | Function call ->
    simplify_function_call ~simplify_expr dacc apply ~callee_ty call ~arg_types
      ~down_to_up
  | Method { kind; obj } ->
    simplify_method_call dacc apply ~callee_ty ~kind ~obj ~arg_types ~down_to_up
  | C_call { alloc = _; param_arity; return_arity; is_c_builtin = _ } ->
    simplify_c_call ~simplify_expr dacc apply ~callee_ty ~param_arity
      ~return_arity ~arg_types ~down_to_up
