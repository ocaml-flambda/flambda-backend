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

open! Simplify_import

type used_extra_params =
  { extra_params_used_as_normal : BP.t list;
    extra_params_not_used_as_normal : BP.t list
  }

let compute_used_extra_params uacc (extra_params_and_args : EPA.t)
    ~is_single_inlinable_use ~free_names ~handler =
  (* If the continuation is going to be inlined out, we don't need to spend time
     here calculating unused parameters, since the creation of [Let]-expressions
     around the continuation's handler will do that anyway. *)
  if is_single_inlinable_use
  then
    { extra_params_used_as_normal =
        Bound_parameters.to_list (EPA.extra_params extra_params_and_args);
      extra_params_not_used_as_normal = []
    }
  else
    let used_or_not extra_param =
      let used =
        NO.greatest_name_mode_var free_names (BP.var extra_param)
        |> Name_mode.Or_absent.is_present_as_normal
      in
      (* The free_names computation is the reference here, because it records
         precisely what is actually used in the term being rebuilt. The required
         variables computed by the data_flow analysis can only be an over
         approximation of it here (given that some simplification/dead code
         elimination may have removed some uses on the way up). To make sure the
         data_flow analysis is correct (or rather than the pre-condition for its
         correctness are verified, i.e. that on the way down, the use
         constraints accumulated are an over-approximation of the actual use
         constraints), we check here that all actually-used variables were also
         marked as used by the data_flow analysis. *)
      if not (Flambda_features.check_invariants ())
      then used
      else
        let marked_as_required =
          Name.Set.mem (Name.var (BP.var extra_param)) (UA.required_names uacc)
        in
        if used && not marked_as_required
        then
          Misc.fatal_errorf
            "The data_flow analysis marked the extra param %a@ as not \
             required, but the free_names indicate it is actually used:@ \n\
             free_names = %a@ \n\
             handler = %a" BP.print extra_param NO.print free_names
            (RE.print (UA.are_rebuilding_terms uacc))
            handler;
        used
    in
    let extra_params_used_as_normal, extra_params_not_used_as_normal =
      ListLabels.partition
        (Bound_parameters.to_list (EPA.extra_params extra_params_and_args))
        ~f:used_or_not
    in
    { extra_params_used_as_normal; extra_params_not_used_as_normal }

type used_params =
  { params_used_as_normal : BP.t list;
    params_not_used_as_normal : BP.t list
  }

let compute_used_params uacc params ~is_exn_handler ~is_single_inlinable_use
    ~free_names ~handler =
  let params = Bound_parameters.to_list params in
  if is_single_inlinable_use
  then { params_used_as_normal = params; params_not_used_as_normal = [] }
  else
    let first = ref true in
    let param_is_used param =
      (* CR mshinwell: We should have a robust means of propagating which
         parameter is the exception bucket. Then this hack can be removed. *)
      if !first && is_exn_handler
      then (
        (* If this argument is actually unused, the Apply_conts are updated
           accordingly in simplify_apply_cont. Apply_cont_rewrite can't at the
           moment represent this transformation. *)
        first := false;
        true)
      else (
        first := false;
        let param_var = BP.var param in
        let num = NO.count_variable_normal_mode free_names param_var in
        match num with
        | Zero -> false
        | One | More_than_one ->
          (* Same as above *)
          if Flambda_features.check_invariants ()
             && not (Name.Set.mem (Name.var param_var) (UA.required_names uacc))
          then
            Misc.fatal_errorf
              "The data_flow analysis marked the original param %a@ as not \
               required, but the free_names indicate it is actually used:@ \n\
               free_names = %a@ \n\
               handler = %a" BP.print param NO.print free_names
              (RE.print (UA.are_rebuilding_terms uacc))
              handler;
          true)
    in
    let params_used_as_normal, params_not_used_as_normal =
      List.partition param_is_used params
    in
    { params_used_as_normal; params_not_used_as_normal }

let add_extra_params_for_continuation_param_aliases cont uacc rewrite_ids
    extra_params_and_args =
  let Flow_types.Alias_result.{ continuation_parameters; aliases_kind; _ } =
    UA.continuation_param_aliases uacc
  in
  let required_extra_args =
    Continuation.Map.find cont continuation_parameters
  in
  Variable.Set.fold
    (fun var epa ->
      let extra_args =
        Apply_cont_rewrite_id.Map.of_set
          (fun _id -> EPA.Extra_arg.Already_in_scope (Simple.var var))
          rewrite_ids
      in
      let var_kind =
        Flambda_kind.With_subkind.create
          (Variable.Map.find var aliases_kind)
          Anything
      in
      EPA.add ~extra_param:(Bound_parameter.create var var_kind) ~extra_args epa)
    required_extra_args.extra_args_for_aliases extra_params_and_args

let add_extra_params_for_reference_fields cont uacc extra_params_and_args =
  let Flow_types.Mutable_unboxing_result.{ additionnal_epa; _ } =
    UA.mutable_unboxing_result uacc
  in
  match Continuation.Map.find cont additionnal_epa with
  | exception Not_found -> extra_params_and_args
  | additionnal_epa ->
    EPA.concat ~outer:extra_params_and_args ~inner:additionnal_epa

let rebuild_one_continuation_handler cont ~at_unit_toplevel
    (recursive : Recursive.t) ~params ~(extra_params_and_args : EPA.t)
    ~rewrite_ids ~is_single_inlinable_use ~is_exn_handler handler uacc
    ~after_rebuild =
  Format.eprintf "rebuild_one_continuation_handler %a@." Continuation.print cont;
  let Flow_types.Alias_result.{ continuation_parameters; _ } =
    UA.continuation_param_aliases uacc
  in
  let continuation_parameters =
    Continuation.Map.find cont continuation_parameters
  in
  let add_lets_around_handler uacc handler =
    let handler, uacc =
      Variable.Map.fold
        (fun var bound_to (handler, uacc) ->
          let bound_pattern =
            Bound_pattern.singleton (Bound_var.create var Name_mode.normal)
          in
          let named = Named.create_simple (Simple.var bound_to) in
          let handler, uacc =
            Expr_builder.create_let_binding uacc bound_pattern named
              ~free_names_of_defining_expr:
                (Name_occurrences.singleton_variable bound_to Name_mode.normal)
              ~cost_metrics_of_defining_expr:Cost_metrics.zero ~body:handler
          in
          handler, uacc)
        continuation_parameters.lets_to_introduce (handler, uacc)
    in
    let handler, uacc =
      (* We might need to place lifted constants now, as they could depend on
         continuation parameters. As such we must also compute the unused
         parameters after placing any constants! *)
      if not at_unit_toplevel
      then handler, uacc
      else
        let uacc, lifted_constants_from_body =
          UA.get_and_clear_lifted_constants uacc
        in
        EB.place_lifted_constants uacc
          ~lifted_constants_from_defining_expr:LCS.empty
          ~lifted_constants_from_body
          ~put_bindings_around_body:(fun uacc ~body -> body, uacc)
          ~body:handler
    in
    let free_names = UA.name_occurrences uacc in
    let cost_metrics = UA.cost_metrics uacc in
    handler, uacc, free_names, cost_metrics
  in
  let uacc, params, new_phantom_params, handler, free_names, cost_metrics =
    match recursive with
    | Recursive -> (
      (* In the recursive case, we have already added an apply_cont_rewrite for
         the recursive continuation to eliminate unused parameters in its
         handler. *)
      match UE.find_apply_cont_rewrite (UA.uenv uacc) cont with
      | None ->
        Misc.fatal_errorf
          "An [Apply_cont_rewrite] for the recursive continuation %a should \
           have already been added"
          Continuation.print cont
      | Some rewrite ->
        let handler, uacc, free_names, cost_metrics =
          add_lets_around_handler uacc handler
        in
        let used_params_set = Apply_cont_rewrite.used_params rewrite in
        let used_params, unused_params =
          List.partition
            (fun param -> BP.Set.mem param used_params_set)
            (Bound_parameters.to_list params)
        in
        let used_extra_params =
          Apply_cont_rewrite.used_extra_params rewrite
          |> Bound_parameters.to_list
        in
        let new_phantom_params =
          List.filter
            (fun param -> NO.mem_var free_names (BP.var param))
            unused_params
          |> Bound_parameters.create
        in
        ( uacc,
          Bound_parameters.create (used_params @ used_extra_params),
          new_phantom_params,
          handler,
          free_names,
          cost_metrics ))
    | Non_recursive ->
      (* If the continuation is going to be inlined out, we don't need to spend
         time here calculating unused parameters, since the creation of
         [Let]-expressions around the continuation's handler will do that
         anyway. *)
      let handler, uacc, free_names, cost_metrics =
        add_lets_around_handler uacc handler
      in
      let extra_params_and_args =
        add_extra_params_for_continuation_param_aliases cont uacc rewrite_ids
          extra_params_and_args
        |> add_extra_params_for_reference_fields cont uacc
      in
      let { extra_params_used_as_normal; extra_params_not_used_as_normal } =
        compute_used_extra_params uacc extra_params_and_args
          ~is_single_inlinable_use ~free_names ~handler
      in
      let { params_used_as_normal; params_not_used_as_normal } =
        compute_used_params uacc params ~is_exn_handler ~is_single_inlinable_use
          ~free_names ~handler
      in
      let new_phantom_params =
        List.filter
          (fun param -> NO.mem_var free_names (BP.var param))
          (params_not_used_as_normal @ extra_params_not_used_as_normal)
      in
      let rewrite =
        Apply_cont_rewrite.create ~original_params:params
          ~used_params:(BP.Set.of_list params_used_as_normal)
          ~extra_params:(EPA.extra_params extra_params_and_args)
          ~extra_args:(EPA.extra_args extra_params_and_args)
          ~used_extra_params:(BP.Set.of_list extra_params_used_as_normal)
      in
      let uacc =
        UA.map_uenv uacc ~f:(fun uenv ->
            UE.add_apply_cont_rewrite uenv cont rewrite)
      in
      ( uacc,
        Bound_parameters.create
          (params_used_as_normal @ extra_params_used_as_normal),
        Bound_parameters.create new_phantom_params,
        handler,
        free_names,
        cost_metrics )
  in
  let handler, uacc =
    let new_phantom_param_bindings_outermost_first =
      List.map
        (fun param ->
          let var = BP.var param in
          let kind = K.With_subkind.kind (BP.kind param) in
          let var = Bound_var.create var Name_mode.phantom in
          let let_bound = Bound_pattern.singleton var in
          let prim = Flambda_primitive.(Nullary (Optimised_out kind)) in
          let named = Named.create_prim prim Debuginfo.none in
          let simplified_defining_expr = Simplified_named.create named in
          { Simplify_named_result.let_bound;
            simplified_defining_expr;
            original_defining_expr = named
          })
        (Bound_parameters.to_list new_phantom_params)
    in
    EB.make_new_let_bindings uacc ~body:handler
      ~bindings_outermost_first:new_phantom_param_bindings_outermost_first
  in
  let cont_handler =
    RE.Continuation_handler.create
      (UA.are_rebuilding_terms uacc)
      params ~handler ~free_names_of_handler:free_names ~is_exn_handler
  in
  after_rebuild cont_handler ~params ~handler ~free_names_of_handler:free_names
    ~cost_metrics_of_handler:cost_metrics uacc

let simplify_one_continuation_handler ~simplify_expr dacc cont ~at_unit_toplevel
    recursive ~params ~handler ~extra_params_and_args ~rewrite_ids
    ~is_single_inlinable_use ~is_exn_handler ~down_to_up =
  Format.eprintf "simplify_one_continuation_handler %a@." Continuation.print cont;
  let down_to_up dacc ~rebuild =
    Format.eprintf "simplify_one_continuation_handler.down_to_up %a@." Continuation.print cont;
    let rebuild uacc ~after_rebuild =
      Format.eprintf "simplify_one_continuation_handler.rebuild %a@." Continuation.print cont;
      (* The name occurrences component of this [uacc] is cleared (see further
         down this file) before rebuilding a handler. This is done so we can
         precisely identify the free names of the handler. *)
      assert (NO.is_empty (UA.name_occurrences uacc));
      let after_rebuild handler uacc =
        Format.eprintf "simplify_one_continuation_handler.after_rebuild %a@." Continuation.print cont;
        rebuild_one_continuation_handler cont ~at_unit_toplevel recursive
          ~params ~extra_params_and_args ~rewrite_ids ~is_single_inlinable_use
          ~is_exn_handler handler uacc ~after_rebuild
      in
      rebuild uacc ~after_rebuild
    in
    down_to_up dacc ~rebuild
  in
  simplify_expr dacc handler ~down_to_up

type behaviour =
  | Invalid
  | Alias_for of Continuation.t
  | Unknown

let rebuild_non_recursive_let_cont_handler cont
    (uses : Join_points.result option) ~params ~handler ~free_names_of_handler
    ~cost_metrics_of_handler ~is_single_inlinable_use scope ~is_exn_handler
    (extra_params_and_args : EPA.t) ~rewrite_ids:_
    (cont_handler : RE.Continuation_handler.t) uacc ~after_rebuild =
  Format.eprintf "rebuild_non_recursive_let_cont_handler %a@." Continuation.print cont;
  let uenv = UA.uenv uacc in
  let uenv =
    (* CR mshinwell: Change types so that [free_names_of_handler] only needs to
       be provided in the [Uses] case. *)
    match uses with
    | None -> uenv
    | Some _ -> (
      if (* We must make the final decision now as to whether to inline this
            continuation or not; we can't wait until
            [Simplify_apply_cont.rebuild_apply_cont] because we need to decide
            sooner than that whether to keep the [Let_cont] (in order to keep
            free name sets correct). *)
         is_single_inlinable_use
      then (
        (* Note that [Continuation_uses] won't set [is_single_inlinable_use] if
           [cont] is an exception handler. *)
        assert (not is_exn_handler);
        (* We pass the parameters and the handler expression, rather than the
           [CH.t], to avoid re-opening the name abstraction. *)
        UE.add_linearly_used_inlinable_continuation uenv cont scope ~params
          ~handler ~free_names_of_handler ~cost_metrics_of_handler)
      else
        let behaviour =
          (* CR-someday mshinwell: This could be replaced by a more
             sophisticated analysis, but for the moment we just use a simple
             syntactic check. *)
          if is_exn_handler
          then Unknown
          else
            match RE.to_apply_cont handler with
            | Some apply_cont -> (
              match Apply_cont.trap_action apply_cont with
              | Some _ -> Unknown
              | None ->
                let args = Apply_cont.args apply_cont in
                let params = Bound_parameters.simples params in
                if Misc.Stdlib.List.compare Simple.compare args params = 0
                then Alias_for (Apply_cont.continuation apply_cont)
                else Unknown)
            | None ->
              if RE.can_be_removed_as_invalid handler
                   (UA.are_rebuilding_terms uacc)
              then Invalid
              else Unknown
        in
        match behaviour with
        | Invalid ->
          let arity = Bound_parameters.arity_with_subkinds params in
          UE.add_invalid_continuation uenv cont scope arity
        | Alias_for alias_for ->
          let arity = Bound_parameters.arity_with_subkinds params in
          UE.add_continuation_alias uenv cont arity ~alias_for
        | Unknown ->
          UE.add_non_inlinable_continuation uenv cont scope ~params
            ~handler:(Known handler))
  in
  (* The parameters are removed from the free name information as they are no
     longer in scope. *)
  let uacc =
    let name_occurrences =
      ListLabels.fold_left
        (Bound_parameters.to_list params
        @ Bound_parameters.to_list (EPA.extra_params extra_params_and_args))
        ~init:(UA.name_occurrences uacc)
        ~f:(fun name_occurrences param ->
          NO.remove_var name_occurrences ~var:(BP.var param))
    in
    UA.with_name_occurrences uacc ~name_occurrences
  in
  after_rebuild cont_handler ~handler_expr:handler (UA.with_uenv uacc uenv)

let simplify_non_recursive_let_cont_handler ~simplify_expr ~denv_before_body
    ~dacc_after_body cont params ~(handler : Expr.t) ~prior_lifted_constants
    ~scope ~is_exn_handler ~unit_toplevel_exn_cont ~prior_cont_uses_env
    ~down_to_up =
  Format.eprintf "simplify_non_recursive_let_cont_handler %a@." Continuation.print cont;
  let cont_uses_env = DA.continuation_uses_env dacc_after_body in
  let at_unit_toplevel =
    (* We try to show that [handler] postdominates [body] (which is done by
       showing that [body] can only return through [cont]) and that if [body]
       raises any exceptions then it only does so to toplevel. If this can be
       shown and we are currently at the toplevel of a compilation unit, the
       handler for the environment can remain marked as toplevel (and suitable
       for "let symbol" bindings); otherwise, it cannot. *)
    DE.at_unit_toplevel denv_before_body
    && (not is_exn_handler)
    && Continuation.Set.subset
         (CUE.all_continuations_used cont_uses_env)
         (Continuation.Set.of_list [cont; unit_toplevel_exn_cont])
  in
  let env_at_fork =
    DE.set_at_unit_toplevel_state denv_before_body at_unit_toplevel
  in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let consts_lifted_during_body = DA.get_lifted_constants dacc_after_body in
  let uses, rewrite_ids =
    match CUE.get_continuation_uses cont_uses_env cont with
    | None -> None, Apply_cont_rewrite_id.Set.empty
    | Some uses ->
      ( Some
          (Join_points.compute_handler_env uses ~params ~env_at_fork
             ~consts_lifted_during_body ~code_age_relation_after_body),
        Continuation_uses.get_use_ids uses )
  in
  let dacc =
    DA.add_to_lifted_constant_accumulator dacc_after_body prior_lifted_constants
  in
  match uses with
  | None ->
    (* Don't simplify the handler if there aren't any uses: otherwise, its code
       will be deleted but any continuation usage information collected during
       its simplification will remain. *)
    let cont_uses_env =
      CUE.union prior_cont_uses_env (CUE.remove cont_uses_env cont)
    in
    let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
    let rebuild uacc ~after_rebuild =
      Format.eprintf "simplify_non_recursive_let_cont_handler.rebuild(None) %a@." Continuation.print cont;
      (* The code will never be used, so we can swap it out for [Invalid]. *)
      let handler = RE.create_invalid (Body_of_unreachable_continuation cont) in
      let cont_handler =
        RE.Continuation_handler.create
          (UA.are_rebuilding_terms uacc)
          params ~handler ~free_names_of_handler:NO.empty ~is_exn_handler
      in
      (* Even though the handler is discarded, marking an operation as removed
         is unnecessary: the handler would have been left untouched during
         execution. *)
      rebuild_non_recursive_let_cont_handler cont uses ~params ~handler
        ~free_names_of_handler:NO.empty
        ~cost_metrics_of_handler:Cost_metrics.zero
        ~is_single_inlinable_use:false scope ~is_exn_handler EPA.empty
        ~rewrite_ids:Apply_cont_rewrite_id.Set.empty cont_handler uacc
        ~after_rebuild
    in
    down_to_up dacc ~continuation_has_zero_uses:true ~rebuild
  | Some
      { handler_env;
        arg_types_by_use_id;
        extra_params_and_args;
        is_single_inlinable_use;
        escapes
      } ->
    let handler_env, extra_params_and_args, is_exn_handler, dacc =
      match Continuation.sort cont with
      | Normal_or_exn when is_single_inlinable_use ->
        if is_exn_handler
        then
          (* This should be prevented by [Simplify_apply_cont_expr]. *)
          Misc.fatal_errorf
            "Exception handlers should never be marked as [Inlinable]:@ %a@ %a"
            Continuation.print cont Expr.print handler;
        handler_env, extra_params_and_args, false, dacc
      | Normal_or_exn | Define_root_symbol ->
        let old_is_exn_handler = is_exn_handler in
        (* If the continuation is an exception handler but it never escapes, it
           can be demoted to a normal (non-exception) handler. It will then
           become eligible for unboxing. *)
        let is_exn_handler = is_exn_handler && escapes in
        let dacc =
          if not (Bool.equal old_is_exn_handler is_exn_handler)
          then DA.demote_exn_handler dacc cont
          else dacc
        in
        if is_exn_handler
        then handler_env, extra_params_and_args, true, dacc
        else
          (* Unbox the parameters of the continuation if possible. Any such
             unboxing will induce a rewrite (or wrapper) on the application
             sites of the continuation. *)
          let param_types = TE.find_params (DE.typing_env handler_env) params in
          let handler_env, decisions =
            Unbox_continuation_params.make_decisions handler_env
              ~continuation_is_recursive:false ~arg_types_by_use_id params
              param_types
          in
          let epa =
            Unbox_continuation_params.compute_extra_params_and_args
              ~arg_types_by_use_id decisions extra_params_and_args
          in
          handler_env, epa, false, dacc
      | Return | Toplevel_return ->
        if is_exn_handler
        then
          (* This should be prevented by [Simplify_apply_cont_expr]. *)
          Misc.fatal_errorf
            "Exception handlers should never be marked as [Return] or \
             [Toplevel_return]:@ %a@ %a"
            Continuation.print cont Expr.print handler;
        handler_env, extra_params_and_args, false, dacc
    in
    let dacc =
      DA.map_flow_acc dacc
        ~f:(Flow.Acc.add_extra_params_and_args cont extra_params_and_args)
    in
    let dacc =
      let cont_uses_env =
        CUE.union prior_cont_uses_env (CUE.remove cont_uses_env cont)
      in
      let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
      let denv =
        if not at_unit_toplevel
        then handler_env
        else DE.mark_parameters_as_toplevel handler_env params
      in
      DA.with_denv dacc denv
    in
    let down_to_up dacc ~rebuild =
      Format.eprintf "simplify_non_recursive_let_cont_handler.down_to_up(Some) %a@." Continuation.print cont;
      let rebuild uacc ~after_rebuild =
        Format.eprintf "simplify_non_recursive_let_cont_handler.rebuild(Some) %a@." Continuation.print cont;
        let after_rebuild cont_handler ~params ~handler ~free_names_of_handler
            ~cost_metrics_of_handler uacc =
          Format.eprintf "simplify_non_recursive_let_cont_handler.after_rebuild(Some) %a@." Continuation.print cont;
          rebuild_non_recursive_let_cont_handler cont uses ~params ~handler
            ~free_names_of_handler ~cost_metrics_of_handler
            ~is_single_inlinable_use scope ~is_exn_handler extra_params_and_args
            ~rewrite_ids cont_handler uacc ~after_rebuild
        in
        rebuild uacc ~after_rebuild
      in
      down_to_up dacc ~continuation_has_zero_uses:false ~rebuild
    in
    simplify_one_continuation_handler ~simplify_expr dacc cont ~at_unit_toplevel
      Non_recursive ~params ~handler ~extra_params_and_args ~rewrite_ids
      ~is_single_inlinable_use ~is_exn_handler ~down_to_up

let after_non_recursive_let_cont_body_rebuilt cont ~uenv_without_cont
    ~name_occurrences_subsequent_exprs ~cost_metrics_of_subsequent_exprs
    ~name_occurrences_handler ~cost_metrics_of_handler handler ~handler_expr
    ~after_rebuild body uacc =
  Format.eprintf "after_non_recursive_let_cont_body_rebuilt %a@." Continuation.print cont;
  let name_occurrences_body = UA.name_occurrences uacc in
  let num_free_occurrences_of_cont_in_body =
    (* Note that this does not count uses in trap actions. If there are uses in
       trap actions, but [remove_let_cont_leaving_body] is [true] below, then
       this must be a case where the exception handler can be demoted to a
       normal handler. This will cause the trap actions to be erased. *)
    NO.count_continuation name_occurrences_body cont
  in
  let is_applied_with_traps =
    NO.continuation_is_applied_with_traps name_occurrences_body cont
  in
  let remove_let_cont_leaving_body =
    match num_free_occurrences_of_cont_in_body with
    | Zero -> true
    | One | More_than_one -> false
  in
  (* We are passing back over a binder, so remove the bound continuation from
     the free name information. Then compute the free names of the whole
     [Let_cont]. *)
  let name_occurrences_body =
    NO.remove_continuation name_occurrences_body ~continuation:cont
  in
  (* Having rebuilt both the body and handler, the [Let_cont] expression itself
     is rebuilt -- unless either the continuation had zero uses, in which case
     we're left with the body; or if the body is just an [Apply_cont] (with no
     trap action) of [cont], in which case we're left with the handler.

     The upwards environment of [uacc] is replaced so that out-of-scope
     continuation bindings do not end up in the accumulator. *)
  let uacc = UA.with_uenv uacc uenv_without_cont in
  let expr, uacc =
    if remove_let_cont_leaving_body
    then
      let uacc =
        let name_occurrences =
          NO.union name_occurrences_body name_occurrences_subsequent_exprs
        in
        UA.with_name_occurrences ~name_occurrences uacc
      in
      (* The cost_metrics stored in uacc is the cost_metrics of the body at this
         point *)
      body, uacc
    else
      let remove_let_cont_leaving_handler =
        match RE.to_apply_cont body with
        | Some apply_cont -> (
          if not (Continuation.equal cont (Apply_cont.continuation apply_cont))
          then false
          else
            match Apply_cont.args apply_cont with
            | [] -> Option.is_none (Apply_cont.trap_action apply_cont)
            | _ :: _ -> false)
        | None -> false
      in
      if remove_let_cont_leaving_handler
      then
        let uacc =
          let name_occurrences =
            NO.union name_occurrences_handler name_occurrences_subsequent_exprs
          in
          UA.with_name_occurrences uacc ~name_occurrences
          (* The body was discarded -- the cost_metrics in uacc should be set to
             the cost_metrics of handler. *)
          |> UA.with_cost_metrics cost_metrics_of_handler
        in
        handler_expr, uacc
      else
        let uacc =
          let name_occurrences =
            NO.union name_occurrences_body
              (NO.union name_occurrences_handler
                 name_occurrences_subsequent_exprs)
          in
          UA.with_name_occurrences uacc ~name_occurrences
        in
        let expr =
          RE.create_non_recursive_let_cont'
            (UA.are_rebuilding_terms uacc)
            cont handler ~body ~num_free_occurrences_of_cont_in_body
            ~is_applied_with_traps
        in
        let uacc =
          UA.add_cost_metrics
            (Cost_metrics.increase_due_to_let_cont_non_recursive
               ~cost_metrics_of_handler)
            uacc
        in
        expr, uacc
  in
  (* Add the cost_metrics of subsequent expressions back on the accumulator as
     the accumulated cost_metrics was cleared before rebuilding the let cont.*)
  let uacc = UA.add_cost_metrics cost_metrics_of_subsequent_exprs uacc in
  after_rebuild expr uacc

let after_non_recursive_let_cont_handler_rebuilt ~rebuild_body
    ~continuation_has_zero_uses ~uenv_without_cont
    ~name_occurrences_subsequent_exprs ~cost_metrics_of_subsequent_exprs
    ~after_rebuild cont handler ~handler_expr uacc =
  Format.eprintf "after_non_recursive_let_cont_handler_rebuilt %a@." Continuation.print cont;
  let name_occurrences_handler =
    if continuation_has_zero_uses then NO.empty else UA.name_occurrences uacc
  in
  let cost_metrics_of_handler = UA.cost_metrics uacc in
  (* As was done for the handler (see next function below), the free names
     information and the cost metrics are set aside, then cleared in [uacc].
     This is so that the free names and cost metrics arising from the body can
     be identified. *)
  let uacc = uacc |> UA.clear_name_occurrences |> UA.clear_cost_metrics in
  (* Having rebuilt the handler, we now rebuild the body. *)
  rebuild_body uacc
    ~after_rebuild:
      (after_non_recursive_let_cont_body_rebuilt cont ~uenv_without_cont
         ~name_occurrences_subsequent_exprs ~cost_metrics_of_subsequent_exprs
         ~name_occurrences_handler ~cost_metrics_of_handler handler
         ~handler_expr ~after_rebuild)

let after_downwards_traversal_of_non_recursive_let_cont_handler ~down_to_up
    ~rebuild_body cont dacc ~continuation_has_zero_uses ~rebuild:rebuild_handler
    =
    Format.eprintf "after_downwards_traversal_of_non_recursive_let_cont_handler %a@." Continuation.print cont;
  let dacc = DA.map_flow_acc dacc ~f:(Flow.Acc.exit_continuation cont) in
  let rebuild uacc ~after_rebuild =
    Format.eprintf "after_downards_traversal_of_non_recursive_let_cont_handler.rebuild %a@." Continuation.print cont;
    let uenv_without_cont = UA.uenv uacc in
    (* Now, on the upwards traversal, the handler is rebuilt. We need to be
       careful with the free names information returned in [uacc] in two ways:

       - Observe that linear inlining of the continuation doesn't change the
       free names of the whole [Let_cont] (so nothing extra to do here).

       - If the continuation has zero uses, we must not count the free names of
       the handler, as it will be removed.

       The free names information and the cost metrics currently in [uacc],
       which arose from the rebuilding of subsequent expressions, are set aside
       so that we can identify exactly what arises from rebuilding the
       handler. *)
    let name_occurrences_subsequent_exprs = UA.name_occurrences uacc in
    let cost_metrics_of_subsequent_exprs = UA.cost_metrics uacc in
    let uacc = uacc |> UA.clear_name_occurrences |> UA.clear_cost_metrics in
    rebuild_handler uacc
      ~after_rebuild:
        (after_non_recursive_let_cont_handler_rebuilt ~rebuild_body
           ~continuation_has_zero_uses ~uenv_without_cont
           ~name_occurrences_subsequent_exprs ~cost_metrics_of_subsequent_exprs
           ~after_rebuild cont)
  in
  down_to_up dacc ~rebuild

let after_downwards_traversal_of_non_recursive_let_cont_body ~simplify_expr
    ~denv_before_body ~unit_toplevel_exn_cont ~prior_lifted_constants ~scope
    ~is_exn_handler ~prior_cont_uses_env cont params ~handler ~down_to_up
    dacc_after_body ~rebuild:rebuild_body =
  Format.eprintf "after_downwards_traversal_of_non_recursive_let_cont_body %a@." Continuation.print cont;
  let dacc_after_body =
    DA.map_flow_acc dacc_after_body
      ~f:
        (Flow.Acc.enter_continuation cont ~recursive:false ~is_exn_handler
           params)
  in
  (* Before the upwards traversal of the body, we do the downwards traversal of
     the handler. *)
  simplify_non_recursive_let_cont_handler ~simplify_expr ~denv_before_body
    ~dacc_after_body cont params ~handler ~prior_lifted_constants ~scope
    ~is_exn_handler ~unit_toplevel_exn_cont
    ~prior_cont_uses_env
      (* After doing the downwards traversal of the handler, we continue the
         downwards traversal of any surrounding expression (which would have to
         be a [Let_cont]; as such, there's no problem with returning the [DE]
         from the [handler] inside [dacc] since it will be replaced by the one
         from the surrounding context). This is the end of the current scope. *)
    ~down_to_up:
      (after_downwards_traversal_of_non_recursive_let_cont_handler ~down_to_up
         ~rebuild_body cont)

let simplify_non_recursive_let_cont_stage1 ~simplify_expr dacc cont
    ~is_exn_handler ~body ~down_to_up params ~handler =
  Format.eprintf "simplify_non_recursive_let_cont_stage1 %a@." Continuation.print cont;
  let denv = DA.denv dacc in
  let unit_toplevel_exn_cont = DE.unit_toplevel_exn_continuation denv in
  let scope = DE.get_continuation_scope denv in
  let dacc, prior_lifted_constants =
    (* We clear the lifted constants accumulator so that we can easily obtain,
       below, any constants that are generated during the simplification of the
       [body]. We will add these [prior_lifted_constants] back into [dacc]
       later. *)
    DA.get_and_clear_lifted_constants dacc
  in
  let denv_before_body = DA.denv dacc in
  let dacc_for_body =
    (* This increment is required so that we can extract the portion of the
       environment(s) arising between the fork point and the use(s) of the
       continuation. *)
    DE.increment_continuation_scope denv_before_body |> DA.with_denv dacc
  in
  let prior_cont_uses_env = DA.continuation_uses_env dacc_for_body in
  let dacc_for_body =
    DA.with_continuation_uses_env dacc_for_body ~cont_uses_env:CUE.empty
  in
  assert (DA.no_lifted_constants dacc_for_body);
  (* We start by simplifying the body of the [Let_cont]. The handler will follow
     later. *)
  simplify_expr dacc_for_body body
    ~down_to_up:
      (after_downwards_traversal_of_non_recursive_let_cont_body ~simplify_expr
         ~denv_before_body ~unit_toplevel_exn_cont ~prior_lifted_constants
         ~scope ~is_exn_handler ~prior_cont_uses_env cont params ~handler
         ~down_to_up)

let simplify_non_recursive_let_cont_stage0 ~simplify_expr dacc non_rec
    ~down_to_up cont ~body =
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec in
  let is_exn_handler = CH.is_exn_handler cont_handler in
  CH.pattern_match cont_handler
    ~f:
      (simplify_non_recursive_let_cont_stage1 ~simplify_expr dacc cont
         ~is_exn_handler ~body ~down_to_up)

let simplify_non_recursive_let_cont ~simplify_expr dacc non_rec ~down_to_up =
  Non_recursive_let_cont_handler.pattern_match non_rec
    ~f:
      (simplify_non_recursive_let_cont_stage0 ~simplify_expr dacc non_rec
         ~down_to_up)

type make_rewrite_context =
  | In_handler
  | In_body of { rewrite_ids : Apply_cont_rewrite_id.Set.t }

let make_rewrite_for_recursive_continuation uacc ~cont ~original_cont_scope
    ~original_params ~context ~extra_params_and_args =
  let extra_params_and_args =
    match context with
    | In_handler -> extra_params_and_args
    | In_body { rewrite_ids } ->
      (* In the body, the rewrite will refer to the wrapper continuation, if
         there is one, which might have additionnal arguments for the aliases *)
      let extra_params_and_args =
        add_extra_params_for_continuation_param_aliases cont uacc rewrite_ids
          extra_params_and_args
      in
      extra_params_and_args
  in
  let extra_params_and_args =
    add_extra_params_for_reference_fields cont uacc extra_params_and_args
  in
  let required_names = UA.required_names uacc in
  let Flow_types.Alias_result.{ continuation_parameters; _ } =
    UA.continuation_param_aliases uacc
  in
  let { Flow_types.Continuation_param_aliases
        .removed_aliased_params_and_extra_params;
        _
      } =
    Continuation.Map.find cont continuation_parameters
  in
  let kept_param param =
    let var = BP.var param in
    (not (Variable.Set.mem var removed_aliased_params_and_extra_params))
    && Name.Set.mem (Name.var var) required_names
  in
  let used_params_list = Bound_parameters.filter kept_param original_params in
  let used_params = Bound_parameters.to_set used_params_list in
  let extra_params = EPA.extra_params extra_params_and_args in
  let used_extra_params_list =
    Bound_parameters.filter kept_param extra_params
  in
  let used_extra_params = Bound_parameters.to_set used_extra_params_list in
  let rewrite =
    Apply_cont_rewrite.create ~original_params ~used_params
      ~extra_params:(EPA.extra_params extra_params_and_args)
      ~extra_args:(EPA.extra_args extra_params_and_args)
      ~used_extra_params
  in
  let uacc =
    UA.map_uenv uacc ~f:(fun uenv ->
        match context with
        | In_handler -> UE.add_apply_cont_rewrite uenv cont rewrite
        | In_body _ -> UE.replace_apply_cont_rewrite uenv cont rewrite)
  in
  let uacc =
    UA.map_uenv uacc ~f:(fun uenv ->
        let params =
          Bound_parameters.append used_params_list used_extra_params_list
        in
        UE.add_non_inlinable_continuation uenv cont original_cont_scope ~params
          ~handler:Unknown)
  in
  uacc, rewrite

let recursive_let_cont_handler_wrapper_params uacc ~cont ~rewrite =
  let required_names = UA.required_names uacc in
  let Flow_types.Alias_result.{ continuation_parameters; _ } =
    UA.continuation_param_aliases uacc
  in
  let { Flow_types.Continuation_param_aliases
        .removed_aliased_params_and_extra_params;
        _
      } =
    Continuation.Map.find cont continuation_parameters
  in
  let kept_param param =
    let var = BP.var param in
    (not (Variable.Set.mem var removed_aliased_params_and_extra_params))
    && Name.Set.mem (Name.var var) required_names
  in
  let original_params =
    Bound_parameters.to_list (Apply_cont_rewrite.original_params rewrite)
  in
  let used_extra_params =
    Bound_parameters.to_list (Apply_cont_rewrite.used_extra_params rewrite)
  in
  let params = List.filter kept_param (original_params @ used_extra_params) in
  Bound_parameters.create params

type decision =
  | Inline
  | Non_rec
  | Rec
[@@warning "-37"]

type recursive_let_cont_handlers_element =
  | Unused_rec_continuation of Continuation.t
  | Inlined_handler of Continuation.t * RE.Continuation_handler.t
  | Non_recursive_handler of Continuation.t * RE.Continuation_handler.t
  | Recursive_handlers of RE.Continuation_handler.t Continuation.Map.t
[@@warning "-37"]

let rebuild_recursive_let_cont_handlers cont ~params ~original_cont_scope
    cont_handler ~handler ~free_names_of_handler ~cost_metrics_of_handler:_
    ~cont_uses_in_body:_ ~extra_params_and_args ~original_params ~rewrite_ids
    uacc ~after_rebuild =
  Format.eprintf "rebuild_recursive_let_cont_handlers %a@." Continuation.print cont;
  (* Temporarily disable this optimisation until the refactor/re-writing of
     simplify_let_cont *)
  let _is_actually_recursive =
    Continuation.Set.mem cont
      (Name_occurrences.continuations_including_in_trap_actions
         free_names_of_handler)
  in
  let decision =
    Rec
    (*
     * if is_actually_recursive
     * then Rec
     * else
     *  match Continuation_uses.get_uses cont_uses_in_body with
     *  | [use] -> (
     *   match One_continuation_use.use_kind use with
     *   | Inlinable -> (* Inline *) Non_rec
     *   | Non_inlinable _ -> Non_rec)
     * | _ -> Non_rec
     *)
  in
  let uacc, handlers =
    match decision with
    | Inline ->
      Misc.fatal_errorf "TODO"
      (*
       * let uacc =
       * UA.map_uenv uacc ~f:(fun uenv ->
       *     UE.add_linearly_used_inlinable_continuation uenv cont
       *       original_cont_scope ~params ~handler ~free_names_of_handler
       *       ~cost_metrics_of_handler)
       * in
       * uacc, Inlined_handler (cont, cont_handler)
       *)
    | Non_rec ->
      Misc.fatal_errorf "TODO"
      (*
       * let uacc =
       *  UA.map_uenv uacc ~f:(fun uenv ->
       *     UE.add_non_inlinable_continuation uenv cont original_cont_scope
       *       ~params ~handler:(Known handler))
       * in
       * let handlers = Non_recursive_handler (cont, cont_handler) in
       * uacc, handlers
       *)
    | Rec ->
      let uacc =
        UA.map_uenv uacc ~f:(fun uenv ->
            UE.add_non_inlinable_continuation uenv cont original_cont_scope
              ~params ~handler:(Known handler))
      in
      let handlers =
        Recursive_handlers (Continuation.Map.singleton cont cont_handler)
      in
      (* We are inserting a rewrite for cont a second time: the first one was
         int the handler, this one for the body of the let cont. Those does not
         really relate to the same continuation, but to continuations with the
         same name: If the rewrites are different, this one will refer to a
         wrapper continuation with more arguments. *)
      let uacc, rewrite =
        make_rewrite_for_recursive_continuation uacc ~cont ~original_cont_scope
          ~original_params
          ~context:(In_body { rewrite_ids })
          ~extra_params_and_args
      in
      let wrapper_params =
        recursive_let_cont_handler_wrapper_params uacc ~cont ~rewrite
      in
      let uacc =
        (* If the arguments of the wrapper continuation and the recursive
           continuation are different, we need to remove the arguments of the
           wrapper from the free names of the handlers.

           It is correct to do, even when no wrapper are going to be introduced:
           In that case the rewrite inside the handler and the one for the body
           are the same: the parameters computed by
           [recursive_let_cont_handler_wrapper_params] are exactly the same as
           the recursive continuation, which where already removed from uacc in
           after_one_recursive_let_cont_handler_rebuilt *)
        let name_occurrences =
          List.fold_left
            (fun name_occurrences param ->
              NO.remove_var name_occurrences ~var:(BP.var param))
            (UA.name_occurrences uacc)
            (Bound_parameters.to_list wrapper_params)
        in
        UA.with_name_occurrences uacc ~name_occurrences
      in
      uacc, handlers
  in
  let name_occurrences =
    Name_occurrences.increase_counts (UA.name_occurrences uacc)
  in
  let uacc = UA.with_name_occurrences uacc ~name_occurrences in
  after_rebuild handlers uacc

let after_one_recursive_let_cont_handler_rebuilt cont ~original_cont_scope
    ~name_occurrences_subsequent_exprs ~after_rebuild cont_handler ~params
    ~original_params ~extra_params_and_args ~rewrite_ids ~cont_uses_in_body
    ~handler ~free_names_of_handler ~cost_metrics_of_handler uacc =
  Format.eprintf "after_one_recursive_let_cont_handler_rebuilt %a@." Continuation.print cont;
  let uacc = UA.add_free_names uacc name_occurrences_subsequent_exprs in
  (* The parameters are removed from the free name information as they are no
     longer in scope. *)
  let uacc =
    let name_occurrences =
      ListLabels.fold_left (Bound_parameters.to_list params)
        ~init:(UA.name_occurrences uacc) ~f:(fun name_occurrences param ->
          NO.remove_var name_occurrences ~var:(BP.var param))
    in
    UA.with_name_occurrences uacc ~name_occurrences
  in
  rebuild_recursive_let_cont_handlers cont ~params ~original_cont_scope
    cont_handler ~handler ~original_params ~extra_params_and_args ~rewrite_ids
    ~free_names_of_handler ~cost_metrics_of_handler ~cont_uses_in_body uacc
    ~after_rebuild

let prepare_to_rebuild_one_recursive_let_cont_handler cont params
    (extra_params_and_args : EPA.t) ~rewrite_ids ~original_cont_scope
    ~rebuild_handler ~cont_uses_in_body uacc ~after_rebuild =
  Format.eprintf "prepare_to_rebuild_one_recursive_let_cont_handler %a@." Continuation.print cont;
  let uacc, _rewrite =
    make_rewrite_for_recursive_continuation uacc ~cont ~original_cont_scope
      ~original_params:params ~context:In_handler ~extra_params_and_args
  in
  let name_occurrences_subsequent_exprs = UA.name_occurrences uacc in
  assert (Name_occurrences.is_empty name_occurrences_subsequent_exprs);
  let uacc = UA.clear_name_occurrences uacc in
  rebuild_handler uacc
    ~after_rebuild:
      (after_one_recursive_let_cont_handler_rebuilt cont ~original_cont_scope
         ~name_occurrences_subsequent_exprs ~after_rebuild ~cont_uses_in_body
         ~extra_params_and_args ~rewrite_ids ~original_params:params)

let after_downwards_traversal_of_one_recursive_let_cont_handler cont
    unboxing_decisions ~down_to_up params ~original_cont_scope
    ~cont_uses_in_body ~body_continuation_uses_env dacc ~rebuild:rebuild_handler
    =
    Format.eprintf "after_downwards_traversal_of_one_recursive_let_cont_handler %a@." Continuation.print cont;
  let dacc = DA.map_flow_acc dacc ~f:(Flow.Acc.exit_continuation cont) in
  let handler_continuation_uses_env = DA.continuation_uses_env dacc in
  let continuation_uses_env =
    CUE.union body_continuation_uses_env
      (CUE.mark_non_inlinable handler_continuation_uses_env)
  in
  let rewrite_ids, arg_types_by_use_id =
    (* At this point all uses (in both the body and the handler) of [cont] are
       in [dacc]. *)
    match CUE.get_continuation_uses continuation_uses_env cont with
    | None ->
      ( Apply_cont_rewrite_id.Set.empty,
        ListLabels.map (Bound_parameters.to_list params) ~f:(fun _ ->
            Apply_cont_rewrite_id.Map.empty) )
    | Some continuation_uses ->
      ( Continuation_uses.get_use_ids continuation_uses,
        Continuation_uses.get_arg_types_by_use_id continuation_uses )
  in
  let extra_params_and_args =
    Unbox_continuation_params.compute_extra_params_and_args unboxing_decisions
      ~arg_types_by_use_id EPA.empty
  in
  let dacc =
    (* CR pchambart: perhaps the normal parameters and the extra params/args
       could be added in a single call to [Data_flow] *)
    DA.map_flow_acc dacc ~f:(fun data_flow ->
        Flow.Acc.add_extra_params_and_args cont extra_params_and_args data_flow)
  in
  let cont_uses_env = CUE.remove continuation_uses_env cont in
  let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
  down_to_up dacc
    ~rebuild:
      (prepare_to_rebuild_one_recursive_let_cont_handler cont params
         extra_params_and_args ~rewrite_ids ~original_cont_scope
         ~rebuild_handler ~cont_uses_in_body)

(* This only takes one handler at present since we don't yet support
   simplification of multiple recursive handlers. *)
let simplify_recursive_let_cont_handlers ~simplify_expr ~denv_before_body
    ~dacc_after_body cont params ~handler ~prior_lifted_constants
    ~original_cont_scope ~down_to_up =
  Format.eprintf "simplify_recursive_let_cont_handlers %a@." Continuation.print cont;
  let dacc_after_body =
    DA.map_flow_acc dacc_after_body
      ~f:
        (Flow.Acc.enter_continuation cont ~recursive:true ~is_exn_handler:false
           params)
  in
  let denv =
    DE.add_parameters_with_unknown_types ~at_unit_toplevel:false
      denv_before_body params
  in
  let code_age_relation_after_body =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let denv = DA.get_lifted_constants dacc_after_body |> LCS.add_to_denv denv in
  let typing_env =
    TE.with_code_age_relation (DE.typing_env denv) code_age_relation_after_body
  in
  let denv = DE.with_typing_env denv typing_env in
  let dacc = DA.with_denv dacc_after_body denv in
  let dacc =
    DA.add_to_lifted_constant_accumulator dacc prior_lifted_constants
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv -> DE.set_at_unit_toplevel_state denv false)
  in
  let body_continuation_uses_env = DA.continuation_uses_env dacc in
  let cont_uses_in_body =
    CUE.get_continuation_uses body_continuation_uses_env cont
  in
  match cont_uses_in_body with
  | None ->
    let rebuild uacc ~after_rebuild =
      after_rebuild (Unused_rec_continuation cont) uacc
    in
    down_to_up dacc ~rebuild
  | Some cont_uses_in_body ->
    let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env:CUE.empty in
    let arg_types_by_use_id_in_body =
      Continuation_uses.get_arg_types_by_use_id cont_uses_in_body
    in
    (* Compute unboxing decisions. This works only from the subkind information
       since no other types are available: there are no types on the
       continuation parameters in the terms, and a join cannot be performed
       (unlike in the non-recursive case) as not all of the uses have been seen
       yet. *)
    let param_types =
      ListLabels.map (Bound_parameters.to_list params) ~f:(fun param ->
          Flambda2_types.unknown_with_subkind (BP.kind param))
    in
    let denv, unboxing_decisions =
      Unbox_continuation_params.make_decisions ~continuation_is_recursive:true
        ~arg_types_by_use_id:arg_types_by_use_id_in_body (DA.denv dacc) params
        param_types
    in
    let dacc = DA.with_denv dacc denv in
    (* {simplify_one_continuation_handler} requires an [extra_params_and_args]
       argument, but we can't provide a meaningful one at this point: we need to
       finish the downwards traversal of the handler to compute the extra args
       for unboxing.

       Thankfully, for recursive continuations, this argument is not used (see
       the use of [extra_params_and_args] in {rebuild_one_continuation_handler})
       because there are no CSE parameters introduced. Therefore, we pass an
       empty one to {simplify_one_continuation_handler}. *)
    let extra_params_and_args = EPA.empty in
    let rewrite_ids = Apply_cont_rewrite_id.Set.empty in
    simplify_one_continuation_handler ~simplify_expr dacc cont
      ~at_unit_toplevel:false Recursive ~params ~handler ~extra_params_and_args
      ~rewrite_ids ~is_single_inlinable_use:false ~is_exn_handler:false
      ~down_to_up:
        (after_downwards_traversal_of_one_recursive_let_cont_handler cont
           unboxing_decisions params ~original_cont_scope ~down_to_up
           ~cont_uses_in_body ~body_continuation_uses_env)

let rebuild_recursive_let_cont ~body handlers ~cost_metrics_of_handlers
    ~free_names_of_body:_ ~uenv_without_cont uacc ~after_rebuild =
  match handlers with
  | Unused_rec_continuation cont ->
    Format.eprintf "rebuild_recursive_let_cont %a@." Continuation.print cont;
    let uacc = UA.with_uenv uacc uenv_without_cont in
    after_rebuild body uacc
  | Inlined_handler (cont, handler) ->
    Misc.fatal_errorf
      "ERROR: inlined rec continuation with invariant parameters:@\n\
       %a@\n\
       @\n\
       body:@\n\
       %a@\n\
       @\n\
       %a"
      (RE.Continuation_handler.print ~cont ~recursive:Non_recursive)
      handler
      (RE.print (UA.are_rebuilding_terms uacc))
      body UA.print uacc
  (*
   * let uacc = UA.with_uenv uacc uenv_without_cont in
   * after_rebuild body uacc *)
  | Non_recursive_handler (cont, handler) ->
    Misc.fatal_errorf
      "ERROR: non-rec rec continuation with invariant parameters:@\n\
       %a@\n\
       @\n\
       body:@\n\
       %a@\n\
       @\n\
       %a"
      (RE.Continuation_handler.print ~cont ~recursive:Non_recursive)
      handler
      (RE.print (UA.are_rebuilding_terms uacc))
      body UA.print uacc
    (*
     * let uacc = UA.with_uenv uacc uenv_without_cont in
     * (* TODO: cost metrics *)
     * let expr =
     * RE.create_non_recursive_let_cont (UA.are_rebuilding_terms uacc) cont handler ~body
     *   ~free_names_of_body
     * in
     * after_rebuild expr uacc *)
  | Recursive_handlers rec_handlers ->
    let Flow_types.Alias_result.{ continuation_parameters; _ } =
      UA.continuation_param_aliases uacc
    in
    let cont, _handler = Continuation.Map.min_binding rec_handlers in
    Format.eprintf "rebuild_recursive_let_cont %a@." Continuation.print cont;
    let ({ removed_aliased_params_and_extra_params;
           extra_args_for_aliases;
           recursive_continuation_wrapper;
           _
         }
          : Flow_types.Continuation_param_aliases.t) =
      Continuation.Map.find cont continuation_parameters
    in
    let expr, uacc =
      match recursive_continuation_wrapper with
      | No_wrapper ->
        ( RE.create_recursive_let_cont
            (UA.are_rebuilding_terms uacc)
            rec_handlers ~body,
          uacc )
      | Wrapper_needed ->
        let rewrite =
          match UE.find_apply_cont_rewrite (UA.uenv uacc) cont with
          | None -> assert false
          | Some rewrite -> rewrite
        in
        let rec_params =
          let original_params =
            Bound_parameters.to_list
              (Apply_cont_rewrite.original_params rewrite)
          in
          let used_params = Apply_cont_rewrite.used_params rewrite in
          let used_original_params =
            List.filter
              (fun param -> BP.Set.mem param used_params)
              original_params
          in
          let used_extra_params =
            Bound_parameters.to_list
              (Apply_cont_rewrite.used_extra_params rewrite)
          in
          List.filter
            (fun param ->
              (not (Variable.Set.mem (BP.var param) extra_args_for_aliases))
              && not
                   (Variable.Set.mem (BP.var param)
                      removed_aliased_params_and_extra_params))
            (used_original_params @ used_extra_params)
        in
        let rec_cont =
          let args =
            List.map (fun param -> Simple.var (BP.var param)) rec_params
          in
          let apply_cont = Apply_cont.create cont ~args ~dbg:Debuginfo.none in
          let body = RE.create_apply_cont apply_cont in
          RE.create_recursive_let_cont
            (UA.are_rebuilding_terms uacc)
            rec_handlers ~body
        in
        let params =
          recursive_let_cont_handler_wrapper_params uacc ~cont ~rewrite
        in
        let handler =
          RE.Continuation_handler.create'
            (UA.are_rebuilding_terms uacc)
            params ~handler:rec_cont ~is_exn_handler:false
        in
        let expr =
          RE.create_non_recursive_let_cont_without_free_names
            (UA.are_rebuilding_terms uacc)
            cont handler ~body
        in
        expr, uacc
    in
    let uacc =
      UA.add_cost_metrics
        (Cost_metrics.increase_due_to_let_cont_recursive
           ~cost_metrics_of_handlers)
        uacc
    in
    let uacc = UA.with_uenv uacc uenv_without_cont in
    after_rebuild expr uacc

let after_recursive_let_cont_body_rebuilt continuation handlers
    ~uenv_without_cont ~free_names_of_handlers ~cost_metrics_of_handlers
    ~after_rebuild body uacc =
  let cont = match [@ocaml.warning "-4"] handlers with
    | Non_recursive_handler (cont, _) -> cont
    | Recursive_handlers m -> fst (Continuation.Map.min_binding m)
    | _ -> assert false
  in
  Format.eprintf "after_recursive_let_cont_body_rebuilt %a@." Continuation.print cont;
  (* We are passing back over a binder, so remove the bound continuation from
     the free name information. [uacc] contains only the free names of the body,
     since the free names were cleared by
     [after_recursive_let_cont_handlers_rebuilt] *)
  let free_names_of_body = UA.name_occurrences uacc in
  let uacc =
    UA.with_name_occurrences uacc
      ~name_occurrences:
        (NO.remove_continuation
           (NO.union free_names_of_handlers free_names_of_body)
           ~continuation)
  in
  rebuild_recursive_let_cont ~body handlers ~uenv_without_cont
    ~free_names_of_body uacc ~cost_metrics_of_handlers ~after_rebuild

let after_recursive_let_cont_handlers_rebuilt cont ~rebuild_body
    ~uenv_without_cont ~after_rebuild handlers uacc =
  Format.eprintf "after_recursive_let_cont_handlers_rebuilt %a@." Continuation.print cont;
  let free_names_of_handlers = UA.name_occurrences uacc in
  let cost_metrics_of_handlers = UA.cost_metrics uacc in
  let uacc = uacc |> UA.clear_name_occurrences |> UA.clear_cost_metrics in
  rebuild_body uacc
    ~after_rebuild:
      (after_recursive_let_cont_body_rebuilt cont handlers ~uenv_without_cont
         ~free_names_of_handlers ~cost_metrics_of_handlers ~after_rebuild)

let after_downwards_traversal_of_recursive_let_cont_handlers cont ~rebuild_body
    ~down_to_up dacc ~rebuild:rebuild_handlers =
  Format.eprintf "after_downwards_traversal_of_recursive_let_cont_handlers %a@." Continuation.print cont;
  down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
      Format.eprintf "after_downwards_traversal_of_recursive_let_cont_handlers.rebuild %a@." Continuation.print cont;
      let uenv_without_cont = UA.uenv uacc in
      let uacc = UA.clear_cost_metrics uacc in
      rebuild_handlers uacc
        ~after_rebuild:
          (after_recursive_let_cont_handlers_rebuilt cont ~rebuild_body
             ~uenv_without_cont ~after_rebuild))

let after_downwards_traversal_of_recursive_let_cont_body ~simplify_expr
    ~denv_before_body cont params ~handler ~prior_lifted_constants
    ~original_cont_scope ~down_to_up dacc_after_body ~rebuild:rebuild_body =
  Format.eprintf "after_downwards_traversal_of_recursive_let_cont_body@.";
  simplify_recursive_let_cont_handlers ~simplify_expr ~denv_before_body
    ~dacc_after_body cont params ~handler ~prior_lifted_constants
    ~original_cont_scope
    ~down_to_up:
      (after_downwards_traversal_of_recursive_let_cont_handlers cont
         ~rebuild_body ~down_to_up)

let simplify_recursive_let_cont_stage1 ~simplify_expr ~denv_before_body ~body
    cont ~original_cont_scope ~down_to_up dacc params ~handler =
  Format.eprintf "simplify_recursive_let_cont_stage1 %a@." Continuation.print cont;
  let dacc = DA.map_denv dacc ~f:DE.increment_continuation_scope in
  let dacc, prior_lifted_constants =
    (* We clear the lifted constants accumulator so that we can easily obtain,
       below, any constants that are generated during the simplification of the
       [body]. We will add these [prior_lifted_constants] back into [dacc]
       later. *)
    DA.get_and_clear_lifted_constants dacc
  in
  (* The body is simplified before the handler, so that if there are no uses
     found in the body, the handler's simplification could be skipped (although
     this is not currently implemented). *)
  simplify_expr dacc body
    ~down_to_up:
      (after_downwards_traversal_of_recursive_let_cont_body ~simplify_expr
         ~denv_before_body cont params ~handler ~prior_lifted_constants
         ~original_cont_scope ~down_to_up)

let simplify_recursive_let_cont_stage0 ~simplify_expr dacc ~down_to_up ~body
    handlers =
  let denv_before_body = DA.denv dacc in
  let original_cont_scope = DE.get_continuation_scope denv_before_body in
  let cont, cont_handler =
    match Continuation.Map.bindings handlers with
    | [] | _ :: _ :: _ ->
      Misc.fatal_error
        "Support for simplification of multiply-recursive continuations is not \
         yet implemented"
    | [c] -> c
  in
  Continuation_handler.pattern_match cont_handler
    ~f:
      (simplify_recursive_let_cont_stage1 ~simplify_expr ~denv_before_body ~body
         cont ~original_cont_scope ~down_to_up dacc)

let simplify_as_recursive_let_cont ~simplify_expr dacc (body, handlers)
    ~down_to_up =
  simplify_recursive_let_cont_stage0 ~simplify_expr dacc ~down_to_up ~body
    handlers

let simplify_recursive_let_cont ~simplify_expr dacc recs ~down_to_up =
  Recursive_let_cont_handlers.pattern_match recs ~f:(fun ~body rec_handlers ->
      assert (not (Continuation_handlers.contains_exn_handler rec_handlers));
      let handlers = Continuation_handlers.to_map rec_handlers in
      simplify_recursive_let_cont_stage0 ~simplify_expr dacc ~down_to_up ~body
        handlers)

let simplify_let_cont ~simplify_expr dacc (let_cont : Let_cont.t) ~down_to_up =
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont ~simplify_expr dacc handler ~down_to_up
  | Recursive handlers ->
    simplify_recursive_let_cont ~simplify_expr dacc handlers ~down_to_up




type stage1_recinfos =
  | Recursive of {
      continuation_handlers : (Bound_parameters.t * Expr.t) Continuation.Map.t
    }
  | Non_recursive of {
      cont : Continuation.t ;
      params : Bound_parameters.t ;
      handler : Expr.t ;
      is_exn_handler : bool ;
    }

type stage1 = {
  body : Expr.t ;
  recinfo : stage1_recinfos ;
}

type stage2 = {
  denv_before_body : DE.t ;
  prior_lifted_constants : LCS.t ;
  prior_cont_uses_env : CUE.t ;
  original_cont_scope : Scope.t ;
  recinfo : stage1_recinfos ;
}

type expr_to_rebuild = (Rebuilt_expr.t * Upwards_acc.t) Simplify_common.rebuild

type stage3_handler_to_rebuild = {
  params : Bound_parameters.t ;
  rebuild_handler : expr_to_rebuild ;
  is_exn_handler : bool ;
  continuations_used : Continuation.Set.t ;
  unbox_decisions : Unbox_continuation_params.Decisions.t option ;
  extra_params_and_args_for_cse : EPA.t ;
}

type stage3 = {
  rebuild_body : expr_to_rebuild ;
  cont_uses_env : CUE.t ;
  at_unit_toplevel : bool ;
  handlers : stage3_handler_to_rebuild Continuation.Map.t ;
}

type stage4_handler_to_rebuild = {
  params : Bound_parameters.t ;
  rebuild_handler : expr_to_rebuild ;
  is_exn_handler : bool ;
  extra_params_and_args : EPA.t ;
  rewrite_ids : Apply_cont_rewrite_id.Set.t ;
}

type stage4_handlers_group =
  | Recursive of {
      rebuild_continuation_handlers : stage4_handler_to_rebuild Continuation.Map.t
    }
  | Non_recursive of {
      cont : Continuation.t ;
      handler : stage4_handler_to_rebuild ;
      is_single_inlinable_use : bool ;
    }

type stage4 = {
  rebuild_body : expr_to_rebuild ;
  at_unit_toplevel : bool ;
  handlers_from_the_outside_to_the_inside : stage4_handlers_group list ;
}

type rebuilt_handler = {
  handler : Rebuilt_expr.Continuation_handler.t ;
  name_occurrences_of_handler : Name_occurrences.t ;
  cost_metrics_of_handler : Cost_metrics.t ;
}

type stage5_handlers_group =
  | Recursive of {
      continuation_handlers : rebuilt_handler Continuation.Map.t
    }
  | Non_recursive of {
      cont : Continuation.t ;
      handler : rebuilt_handler ;
    }

type stage5 = {
  rebuild_body : expr_to_rebuild ;
  handlers_from_the_inside_to_the_outside : stage5_handlers_group list ;
  name_occurrences_of_subsequent_exprs : Name_occurrences.t ;
  cost_metrics_of_subsequent_exprs : Cost_metrics.t ;
  uenv_of_subsequent_exprs : UE.t ;
}

type stage6 = {
  handlers_from_the_inside_to_the_outside : stage5_handlers_group list ;
  name_occurrences_of_subsequent_exprs : Name_occurrences.t ;
  cost_metrics_of_subsequent_exprs : Cost_metrics.t ;
  uenv_of_subsequent_exprs : UE.t ;
}

let reset_dacc_denv dacc previous_denv =
  let new_code_age_relation = TE.code_age_relation (DA.typing_env dacc) in
  let typing_env =
    TE.with_code_age_relation (DE.typing_env previous_denv) new_code_age_relation
  in
  DA.with_denv dacc (DE.with_typing_env previous_denv typing_env)

let simplify_let_cont_stage6 (stage6 : stage6) ~after_rebuild body uacc =
  (* Here both the body and the handlers have been rebuilt. We only need to restore the cost metrics
     and name occurrences accumulators, rebuild all the let cont expressions, and call after_rebuild
     with the result and the new upwards accumulator. *)
  let name_occurrences_body = UA.name_occurrences uacc in
  let cost_metrics_of_body = UA.cost_metrics uacc in
  let rec rebuild_groups body name_occurrences_body cost_metrics_of_body uacc groups =
    match groups with
    | [] ->
      let uacc =
        UA.with_name_occurrences ~name_occurrences:(Name_occurrences.union name_occurrences_body stage6.name_occurrences_of_subsequent_exprs) uacc
      in
      let uacc =
        UA.with_cost_metrics (Cost_metrics.(+) cost_metrics_of_body stage6.cost_metrics_of_subsequent_exprs)
          uacc
      in
      let uacc = UA.with_uenv uacc stage6.uenv_of_subsequent_exprs in
      after_rebuild body uacc
    | Non_recursive { cont; handler } :: groups ->

  let num_free_occurrences_of_cont_in_body =
    (* Note that this does not count uses in trap actions. If there are uses in
       trap actions, but [remove_let_cont_leaving_body] is [true] below, then
       this must be a case where the exception handler can be demoted to a
       normal handler. This will cause the trap actions to be erased. *)
    NO.count_continuation name_occurrences_body cont
  in
  let is_applied_with_traps =
    NO.continuation_is_applied_with_traps name_occurrences_body cont
  in
  let remove_let_cont_leaving_body =
    match num_free_occurrences_of_cont_in_body with
    | Zero -> true
    | One | More_than_one -> false
  in
  (* We are passing back over a binder, so remove the bound continuation from
     the free name information. Then compute the free names of the whole
     [Let_cont]. *)
  let name_occurrences_body =
    NO.remove_continuation name_occurrences_body ~continuation:cont
  in
  (* Having rebuilt both the body and handler, the [Let_cont] expression itself
     is rebuilt -- unless either the continuation had zero uses, in which case
     we're left with the body; or if the body is just an [Apply_cont] (with no
     trap action) of [cont], in which case we're left with the handler.

TODO update this comment

     The upwards environment of [uacc] is replaced so that out-of-scope
     continuation bindings do not end up in the accumulator. *)
  let expr, name_occurrences, cost_metrics =
    if remove_let_cont_leaving_body
    then
      body, name_occurrences_body, cost_metrics_of_body
    else
      let remove_let_cont_leaving_handler =
        match RE.to_apply_cont body with
        | Some apply_cont -> (
          if not (Continuation.equal cont (Apply_cont.continuation apply_cont))
          then false
          else
            match Apply_cont.args apply_cont with
            | [] -> Option.is_none (Apply_cont.trap_action apply_cont)
            | _ :: _ -> false)
        | None -> false
      in
      if remove_let_cont_leaving_handler
      then
        failwith("handler_expr"), handler.name_occurrences_of_handler, handler.cost_metrics_of_handler
      else
        let name_occurrences = NO.union name_occurrences_body handler.name_occurrences_of_handler in
        let cost_metrics =
          Cost_metrics.(+) cost_metrics_of_body (Cost_metrics.increase_due_to_let_cont_non_recursive ~cost_metrics_of_handler:handler.cost_metrics_of_handler)
        in
        let expr =
          RE.create_non_recursive_let_cont'
            (UA.are_rebuilding_terms uacc)
            cont handler.handler ~body ~num_free_occurrences_of_cont_in_body
            ~is_applied_with_traps
        in
        expr, name_occurrences, cost_metrics
  in
   rebuild_groups expr name_occurrences cost_metrics uacc groups
    | Recursive { continuation_handlers } :: group ->
      failwith "TODO"
  in
  rebuild_groups body name_occurrences_body cost_metrics_of_body uacc stage6.handlers_from_the_inside_to_the_outside

let simplify_let_cont_stage5 (stage5 : stage5) uacc ~after_rebuild =
  (* At this point all handlers have been rebuild and added to the upwards environment. All that we
     still need to do is to rebuild the body, and then rebuild the chain of let cont expressions once
     this is done. We reinit the name occurrences and cost metrics one last time to get precise
     information for those two in the body, we rebuild the body, and we pass on to the final stage
     for the reconstruction of the let cont expressions. *)
  let uacc = UA.clear_cost_metrics (UA.clear_name_occurrences uacc) in
  let stage6 : stage6 = {
    name_occurrences_of_subsequent_exprs = stage5.name_occurrences_of_subsequent_exprs ;
    cost_metrics_of_subsequent_exprs = stage5.cost_metrics_of_subsequent_exprs ;
    uenv_of_subsequent_exprs = stage5.uenv_of_subsequent_exprs ;
    handlers_from_the_inside_to_the_outside = stage5.handlers_from_the_inside_to_the_outside ;
  } in
  stage5.rebuild_body uacc ~after_rebuild:(simplify_let_cont_stage6 stage6 ~after_rebuild)


  let add_lets_around_handler cont at_unit_toplevel uacc handler =
    let Flow_types.Alias_result.{ continuation_parameters; _ } =
      UA.continuation_param_aliases uacc
    in
    let continuation_parameters =
      Continuation.Map.find cont continuation_parameters
    in
    let handler, uacc =
      Variable.Map.fold
        (fun var bound_to (handler, uacc) ->
          let bound_pattern =
            Bound_pattern.singleton (Bound_var.create var Name_mode.normal)
          in
          let named = Named.create_simple (Simple.var bound_to) in
          let handler, uacc =
            Expr_builder.create_let_binding uacc bound_pattern named
              ~free_names_of_defining_expr:
                (Name_occurrences.singleton_variable bound_to Name_mode.normal)
              ~cost_metrics_of_defining_expr:Cost_metrics.zero ~body:handler
          in
          handler, uacc)
        continuation_parameters.lets_to_introduce (handler, uacc)
    in
    let handler, uacc =
      (* We might need to place lifted constants now, as they could depend on
         continuation parameters. As such we must also compute the unused
         parameters after placing any constants! *)
      if not at_unit_toplevel
      then handler, uacc
      else
        let uacc, lifted_constants_from_body =
          UA.get_and_clear_lifted_constants uacc
        in
        EB.place_lifted_constants uacc
          ~lifted_constants_from_defining_expr:LCS.empty
          ~lifted_constants_from_body
          ~put_bindings_around_body:(fun uacc ~body -> body, uacc)
          ~body:handler
    in
    let free_names = UA.name_occurrences uacc in
    let cost_metrics = UA.cost_metrics uacc in
    handler, uacc, free_names, cost_metrics

let add_phantom_params_bindings uacc handler new_phantom_params =
  let new_phantom_param_bindings_outermost_first =
    List.map
      (fun param ->
         let var = BP.var param in
         let kind = K.With_subkind.kind (BP.kind param) in
         let var = Bound_var.create var Name_mode.phantom in
         let let_bound = Bound_pattern.singleton var in
         let prim = Flambda_primitive.(Nullary (Optimised_out kind)) in
         let named = Named.create_prim prim Debuginfo.none in
         let simplified_defining_expr = Simplified_named.create named in
         { Simplify_named_result.let_bound;
           simplified_defining_expr;
           original_defining_expr = named
         })
      (Bound_parameters.to_list new_phantom_params)
  in
  EB.make_new_let_bindings uacc ~body:handler
    ~bindings_outermost_first:new_phantom_param_bindings_outermost_first


let rebuild_single_non_recursive_handler ~at_unit_toplevel ~is_single_inlinable_use cont (handler_to_rebuild : stage4_handler_to_rebuild) uacc k =
  (* Clear existing name occurrences & cost metrics *)
  let uacc = UA.clear_name_occurrences (UA.clear_cost_metrics uacc) in
  let { is_exn_handler ; rewrite_ids ; params ; rebuild_handler ; extra_params_and_args } = handler_to_rebuild in
  rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
      let handler, uacc, free_names, cost_metrics = add_lets_around_handler cont at_unit_toplevel uacc handler in
            let extra_params_and_args =
        add_extra_params_for_continuation_param_aliases cont uacc rewrite_ids
          extra_params_and_args
        |> add_extra_params_for_reference_fields cont uacc
      in
      let { extra_params_used_as_normal; extra_params_not_used_as_normal } =
        compute_used_extra_params uacc extra_params_and_args
          ~is_single_inlinable_use ~free_names ~handler
      in
      let { params_used_as_normal; params_not_used_as_normal } =
        compute_used_params uacc params ~is_exn_handler ~is_single_inlinable_use
          ~free_names ~handler
      in
      let new_phantom_params =
        List.filter
          (fun param -> NO.mem_var free_names (BP.var param))
          (params_not_used_as_normal @ extra_params_not_used_as_normal)
      in
      let rewrite =
        Apply_cont_rewrite.create ~original_params:params
          ~used_params:(BP.Set.of_list params_used_as_normal)
          ~extra_params:(EPA.extra_params extra_params_and_args)
          ~extra_args:(EPA.extra_args extra_params_and_args)
          ~used_extra_params:(BP.Set.of_list extra_params_used_as_normal)
      in
      let uacc =
        UA.map_uenv uacc ~f:(fun uenv ->
            UE.add_apply_cont_rewrite uenv cont rewrite)
      in
      let params = Bound_parameters.create
          (params_used_as_normal @ extra_params_used_as_normal)
      in
      let new_phantom_params = Bound_parameters.create new_phantom_params in
      let handler, uacc = add_phantom_params_bindings uacc handler new_phantom_params in
      let cont_handler =
        RE.Continuation_handler.create
          (UA.are_rebuilding_terms uacc)
          params ~handler ~free_names_of_handler:free_names ~is_exn_handler
      in

  let uenv = UA.uenv uacc in
  let uenv =
    let scope = Scope.initial in (* CR ncourant: I think this is never used in UE, we could remove it *)
      if (* We must make the final decision now as to whether to inline this
            continuation or not; we can't wait until
            [Simplify_apply_cont.rebuild_apply_cont] because we need to decide
            sooner than that whether to keep the [Let_cont] (in order to keep
            free name sets correct). *)
         is_single_inlinable_use
      then (
        (* Note that [Continuation_uses] won't set [is_single_inlinable_use] if
           [cont] is an exception handler. *)
        assert (not is_exn_handler);
        (* We pass the parameters and the handler expression, rather than the
           [CH.t], to avoid re-opening the name abstraction. *)
        UE.add_linearly_used_inlinable_continuation uenv cont scope ~params
          ~handler ~free_names_of_handler:free_names ~cost_metrics_of_handler:cost_metrics)
      else
        let behaviour =
          (* CR-someday mshinwell: This could be replaced by a more
             sophisticated analysis, but for the moment we just use a simple
             syntactic check. *)
          if is_exn_handler
          then Unknown
          else
            match RE.to_apply_cont handler with
            | Some apply_cont -> (
              match Apply_cont.trap_action apply_cont with
              | Some _ -> Unknown
              | None ->
                let args = Apply_cont.args apply_cont in
                let params = Bound_parameters.simples params in
                if Misc.Stdlib.List.compare Simple.compare args params = 0
                then Alias_for (Apply_cont.continuation apply_cont)
                else Unknown)
            | None ->
              if RE.can_be_removed_as_invalid handler
                   (UA.are_rebuilding_terms uacc)
              then Invalid
              else Unknown
        in
        match behaviour with
        | Invalid ->
          let arity = Bound_parameters.arity_with_subkinds params in
          UE.add_invalid_continuation uenv cont scope arity
        | Alias_for alias_for ->
          let arity = Bound_parameters.arity_with_subkinds params in
          UE.add_continuation_alias uenv cont arity ~alias_for
        | Unknown ->
          UE.add_non_inlinable_continuation uenv cont scope ~params
            ~handler:(Known handler)
  in
  let uacc = UA.with_uenv uacc uenv in
  (* The parameters are removed from the free name information as they are no
     longer in scope. *)
    let free_names =
      ListLabels.fold_left
        (Bound_parameters.to_list params
        @ Bound_parameters.to_list (EPA.extra_params extra_params_and_args))
        ~init:free_names
        ~f:(fun name_occurrences param ->
          NO.remove_var name_occurrences ~var:(BP.var param))
    in

      let rebuilt_handler : rebuilt_handler = {
        handler = cont_handler ;
        name_occurrences_of_handler = free_names ;
        cost_metrics_of_handler = cost_metrics ;
      } in
      k rebuilt_handler uacc
    )

let rebuild_single_recursive_handler cont (handler_to_rebuild : stage4_handler_to_rebuild) at_unit_toplevel uacc k =
  (* Clear existing name occurrences & cost metrics *)
  let uacc = UA.clear_name_occurrences (UA.clear_cost_metrics uacc) in
  (* For recursive only, TODO
     let uacc, _rewrite =
     make_rewrite_for_recursive_continuation uacc ~cont ~original_cont_scope
      ~original_params:params ~context:In_handler ~extra_params_and_args
     in
  *)
  handler_to_rebuild.rebuild_handler uacc ~after_rebuild:(fun handler uacc ->
      let handler, uacc, free_names, cost_metrics = add_lets_around_handler cont false uacc handler in

      failwith "TODO" ()
    )

let rec rebuild_continuation_handlers_loop ~rebuild_body ~name_occurrences_of_subsequent_exprs ~cost_metrics_of_subsequent_exprs ~uenv_of_subsequent_exprs ~at_unit_toplevel uacc ~after_rebuild (groups_to_rebuild : stage4_handlers_group list) rebuilt_groups =
  match groups_to_rebuild with
  | [] ->
    let stage5 : stage5 = {
      rebuild_body ;
      name_occurrences_of_subsequent_exprs ;
      cost_metrics_of_subsequent_exprs ;
      uenv_of_subsequent_exprs ;
      handlers_from_the_inside_to_the_outside = rebuilt_groups ;
    } in
    simplify_let_cont_stage5 stage5 uacc ~after_rebuild
  | Non_recursive { cont; handler; is_single_inlinable_use } :: groups_to_rebuild ->
    rebuild_single_non_recursive_handler ~at_unit_toplevel ~is_single_inlinable_use cont handler uacc
      (fun rebuilt_handler uacc ->
         rebuild_continuation_handlers_loop ~rebuild_body ~name_occurrences_of_subsequent_exprs ~cost_metrics_of_subsequent_exprs ~uenv_of_subsequent_exprs ~at_unit_toplevel uacc ~after_rebuild
           groups_to_rebuild (Non_recursive { cont; handler = rebuilt_handler } :: rebuilt_groups)
      )
  | Recursive { rebuild_continuation_handlers } :: groups_to_rebuild ->
    (* Common setup for recursive handlers: add rewrites *)
    failwith "TODO" ()
    (* Rebuild all the handlers *)
    failwith "TODO" ()
    (* Add all rewrites and continue rebuilding *)
    failwith "TODO" ()

let simplify_let_cont_stage4 (stage4 : stage4) uacc ~after_rebuild =
  (* Here we just returned from the global [down_to_up], which is asking us to rebuild the let cont.
     The flow analyses have been done, and we start to rebuild the expressions. As with the downward
     pass, we loop over each defined handler to rebuild it. As the handlers have been sorted by
     strongly-connected components, we must process the handlers group-by-group, and rebuild the groups
     from the outside to the inside. For each of these groups, we need to perform different steps
     depending on whether it is a mutually-recursive group or a single non-recursive handler.

     For mutually-recursive groups, we need to add the rewrites to the environment, and then rebuild
     the handlers, as the handlers might use themselves, so the rewrites need to be ready at that
     point. We can only use the dataflow information to compute which parameters are used and
     which are not, since we won't be able to remove parameters after the handlers have been rebuilt,
     since we can't rewrite them inside themselves.

     For a single non-recursive handler, we first rebuild the handler. This allows us to get precise
     information on the parameters which are used, which are then used to add the rewrites to the
     environment.

     In both cases, we add the handlers to the environment after rebuilding them, so that the
     environment is ready when rebuilding the remaining handlers. We also reset the name occurrences
     and the cost metrics before rebuilding each handler, so that we know the name occurrences and
     cost metrics corresponding to each handler when rebuilding later.
  *)
  let name_occurrences_of_subsequent_exprs = UA.name_occurrences uacc in
  let cost_metrics_of_subsequent_exprs = UA.cost_metrics uacc in
  let uenv_of_subsequent_exprs = UA.uenv uacc in
  rebuild_continuation_handlers_loop ~rebuild_body:stage4.rebuild_body ~at_unit_toplevel:stage4.at_unit_toplevel ~name_occurrences_of_subsequent_exprs ~cost_metrics_of_subsequent_exprs ~uenv_of_subsequent_exprs uacc ~after_rebuild stage4.handlers_from_the_outside_to_the_inside []

module SCC = Strongly_connected_components.Make(Continuation)

let simplify_let_cont_stage3 (stage3 : stage3) ~down_to_up dacc =
  (* At this point we have done a downwards traversal on the body and all the handlers,
     and we need to call the global [down_to_up] function. First however, we have to take care of
     several things:

     - We need to compute the extra params and args related to unboxed parameters. This could not
       be done earlier, as we might not have seen every use of the continuations.

     - Now that we were able to compute the extra params and args, we add this information to the
       flow analysis, so that it can correctly compute all the information we need when going up.

     - We perform a strongly-connected components analysis of the continuations to be able to turn
       mutually-recursive continuations into several independant blocks of recursive or non-recursive
       continuations. In case one of those is non-recursive, we can check whether the continuation is
       inlinable if it is used a single time.
  *)
  let get_uses cont =
    match CUE.get_continuation_uses stage3.cont_uses_env cont with
    | None -> Misc.fatal_errorf "Uses of %a not found in [simplify_let_cont_stage3]@." Continuation.print cont
    | Some cont -> cont
  in
  let handlers = Continuation.Map.mapi (fun cont (handler : stage3_handler_to_rebuild) ->
      let uses = get_uses cont in
      (* CR ncourant don't use an option *)
      let extra_params_and_args = match handler.unbox_decisions with
        | None -> handler.extra_params_and_args_for_cse
        | Some decisions ->
          let arg_types_by_use_id = Continuation_uses.get_arg_types_by_use_id uses in
          Unbox_continuation_params.compute_extra_params_and_args decisions ~arg_types_by_use_id handler.extra_params_and_args_for_cse
      in
      { params = handler.params; rebuild_handler = handler.rebuild_handler; is_exn_handler = handler.is_exn_handler; extra_params_and_args; rewrite_ids = Continuation_uses.get_use_ids uses }
    ) stage3.handlers
  in

  let dacc =
    DA.map_flow_acc dacc ~f:(fun flow_acc ->
        Continuation.Map.fold (fun cont handler flow_acc ->
            Flow.Acc.add_extra_params_and_args cont handler.extra_params_and_args flow_acc
          ) handlers flow_acc
      )
  in

  let handlers_graph = Continuation.Map.map (fun handler -> handler.continuations_used) stage3.handlers in
  let sorted_handlers_from_the_inside_to_the_outside =
    SCC.connected_components_sorted_from_roots_to_leaf handlers_graph
  in
  let handlers_from_the_outside_to_the_inside =
    Array.fold_left (fun inner group ->
        let group : stage4_handlers_group = match (group : SCC.component) with
          | Has_loop conts ->
            let rebuild_continuation_handlers = List.fold_left (fun group cont ->
              Continuation.Map.add cont (Continuation.Map.find cont handlers) group)
                Continuation.Map.empty conts
            in
            Recursive { rebuild_continuation_handlers }
          | No_loop cont ->
            let handler = Continuation.Map.find cont handlers in
            let is_single_inlinable_use =
              match Continuation_uses.get_uses (get_uses cont) with
              | [] | _ :: _ :: _ -> false
              | [use] -> match One_continuation_use.use_kind use with
                | Inlinable -> true
                | Non_inlinable _ -> false
            in
            Non_recursive { cont; handler; is_single_inlinable_use }
        in
        group :: inner
      ) [] sorted_handlers_from_the_inside_to_the_outside
  in
  let stage4 : stage4 = {
    rebuild_body = stage3.rebuild_body ;
    handlers_from_the_outside_to_the_inside ;
    at_unit_toplevel = stage3.at_unit_toplevel ;
  } in
  down_to_up dacc ~rebuild:(simplify_let_cont_stage4 stage4)

(* simplify for single handler: compute unbox decisions *)
let simplify_single_handler ~simplify_expr is_recursive cont_uses_env_so_far consts_lifted_during_body all_handlers_set denv_to_reset dacc cont (params, handler, is_exn_handler) k =
  (* Here we perform the downwards traversal on a single handler. As an invariant enforced by the loop in
     [simplify_handlers], the continuation must have been used at least once for this function to be called,
     so we can use the uses from [cont_uses_env_so_far] to compute the environment for the handler.

     We also make unboxing decisions at this step, which are necessary to correctly simplify the handler
     using the unboxed parameters, but we delay the unboxing extra_params_and_args to later, when we
     will have seen all uses.
  *)
  let uses = match CUE.get_continuation_uses cont_uses_env_so_far cont with
    | None -> Misc.fatal_errorf "No continuation uses found for %a but simplify_single_handler called@." Continuation.print cont
    | Some uses -> uses
  in
  let code_age_relation = TE.code_age_relation (DA.typing_env dacc) in
  let ( handler_env,
    arg_types_by_use_id,
    extra_params_and_args_for_cse,
    is_single_inlinable_use,
    escapes) =
    if is_recursive then
      let denv = DE.add_parameters_with_unknown_types ~at_unit_toplevel:false denv_to_reset params in
      let denv = LCS.add_to_denv denv consts_lifted_during_body in
      let typing_env = TE.with_code_age_relation (DE.typing_env denv) code_age_relation in
      let handler_env = DE.with_typing_env denv typing_env in
      let arg_types_by_use_id = Continuation_uses.get_arg_types_by_use_id uses in
      (handler_env, arg_types_by_use_id, EPA.empty, false, false)
    else
      (* CR ncourant: I'm not sure about this, the lifted constants have already been reset
         TODO: don't reset lifted constants, pass them until here *)
      let Join_points.{ handler_env; arg_types_by_use_id; extra_params_and_args; is_single_inlinable_use; escapes } = Join_points.compute_handler_env uses ~params ~env_at_fork:denv_to_reset ~consts_lifted_during_body
        ~code_age_relation_after_body:code_age_relation in
      ( handler_env, arg_types_by_use_id, extra_params_and_args, is_single_inlinable_use, escapes)
  in
  let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env:CUE.empty in

    let handler_env, unbox_decisions, is_exn_handler, dacc =
      match Continuation.sort cont with
      | Normal_or_exn when is_single_inlinable_use ->
        if is_exn_handler
        then
          (* This should be prevented by [Simplify_apply_cont_expr]. *)
          Misc.fatal_errorf
            "Exception handlers should never be marked as [Inlinable]:@ %a@ %a"
            Continuation.print cont Expr.print handler;
        (* Don't try to unbox parameters of inlinable continuations, since the typing env still contains
           enough information to avoid re-reading the fields. *)
        handler_env, None, false, dacc
      | Normal_or_exn | Define_root_symbol ->
        let old_is_exn_handler = is_exn_handler in
        (* If the continuation is an exception handler but it never escapes, it
           can be demoted to a normal (non-exception) handler. It will then
           become eligible for unboxing. *)
        let is_exn_handler = is_exn_handler && escapes in
        let dacc =
          if not (Bool.equal old_is_exn_handler is_exn_handler)
          then DA.demote_exn_handler dacc cont
          else dacc
        in
        if is_exn_handler
        then handler_env, None, true, dacc
        else
          (* Unbox the parameters of the continuation if possible. Any such
             unboxing will induce a rewrite (or wrapper) on the application
             sites of the continuation. *)
          let param_types = TE.find_params (DE.typing_env handler_env) params in
          let handler_env, decisions =
            Unbox_continuation_params.make_decisions handler_env
              ~continuation_is_recursive:false ~arg_types_by_use_id params
              param_types
          in
          handler_env, Some decisions, false, dacc
      | Return | Toplevel_return ->
        if is_exn_handler
        then
          (* This should be prevented by [Simplify_apply_cont_expr]. *)
          Misc.fatal_errorf
            "Exception handlers should never be marked as [Return] or \
             [Toplevel_return]:@ %a@ %a"
            Continuation.print cont Expr.print handler;
        handler_env, None, false, dacc
    in
  let dacc = DA.with_denv dacc handler_env in
    (* TODO does this work correctly with mutually-recursive continuations? *)
    let dacc = DA.map_flow_acc ~f:(Flow.Acc.enter_continuation cont ~recursive:is_recursive ~is_exn_handler params) dacc in
  simplify_expr dacc handler ~down_to_up:(fun dacc ~rebuild:rebuild_handler ->
    let dacc = DA.map_flow_acc ~f:(Flow.Acc.exit_continuation cont) dacc in
    let cont_uses_env_in_handler = DA.continuation_uses_env dacc in
    let cont_uses_env_in_handler =
      if is_recursive then CUE.mark_non_inlinable cont_uses_env_in_handler else cont_uses_env_in_handler
    in
    let cont_uses_env_so_far = CUE.union cont_uses_env_so_far cont_uses_env_in_handler in
    let continuations_used = Continuation.Set.inter all_handlers_set (CUE.all_continuations_used cont_uses_env_in_handler) in
    k dacc { params; rebuild_handler; is_exn_handler; continuations_used ; unbox_decisions ; extra_params_and_args_for_cse } cont_uses_env_so_far
  )

let rec simplify_handlers ~simplify_expr ~down_to_up rebuild_body at_unit_toplevel is_recursive cont_uses_env_so_far consts_lifted_during_body all_handlers_set used_handlers remaining_handlers simplified_handlers_set simplified_handlers dacc denv_to_reset =
  (* This is the core loop to simplify all handlers defined by a let cont. We loop over all handlers,
     each time taking the first handler that we have not yet processed and that has at least one use, until
     we have seen every handler that is reachable.
  *)
  (* CR ncourant: this makes the order in which continuations are processed dependant on things like
     the name of the compilation unit. However, recursive continuations are specified using a
     [Continuation.Map.t], which already depends on the name of the compilation unit, so we leave
     this as-is for now. *)
  match Continuation.Set.min_elt_opt used_handlers with
  | None ->
    let cont_uses_env = Continuation.Set.fold (fun cont cont_uses_env ->
        CUE.remove cont_uses_env cont) simplified_handlers_set cont_uses_env_so_far in
    let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env in
    (* TODO mark all remaning handlers as unused? *)
    let (stage3 : stage3) = { rebuild_body ; cont_uses_env = cont_uses_env_so_far ; handlers = simplified_handlers ; at_unit_toplevel } in
    simplify_let_cont_stage3 stage3 ~down_to_up dacc
  | Some cont ->
    let used_handlers = Continuation.Set.remove cont used_handlers in
    let handler = Continuation.Map.find cont remaining_handlers in
    let remaining_handlers = Continuation.Map.remove cont remaining_handlers in
    simplify_single_handler ~simplify_expr is_recursive cont_uses_env_so_far consts_lifted_during_body all_handlers_set denv_to_reset dacc cont handler (fun dacc rebuild cont_uses_env_so_far ->
        let simplified_handlers_set = Continuation.Set.add cont simplified_handlers_set in
        let used_handlers = Continuation.Set.union used_handlers
            (Continuation.Set.diff rebuild.continuations_used simplified_handlers_set)
        in
        let simplified_handlers = Continuation.Map.add cont rebuild simplified_handlers in
        simplify_handlers ~simplify_expr ~down_to_up rebuild_body at_unit_toplevel is_recursive cont_uses_env_so_far consts_lifted_during_body all_handlers_set used_handlers remaining_handlers simplified_handlers_set simplified_handlers dacc denv_to_reset
      )


let simplify_let_cont_stage2 ~simplify_expr (stage2 : stage2) ~down_to_up dacc ~rebuild:rebuild_body =
  (* At this point, we have done the downwards traversal of the body. We prepare to loop over all the
     handlers defined by the let cont. *)
  let body_continuation_uses_env = DA.continuation_uses_env dacc in
  let denv = stage2.denv_before_body in
  let at_unit_toplevel, is_recursive, remaining_handlers =
    match stage2.recinfo with
    | Non_recursive { cont; params; handler; is_exn_handler } ->
      let at_unit_toplevel =
        (* We try to show that [handler] postdominates [body] (which is done by
           showing that [body] can only return through [cont]) and that if [body]
           raises any exceptions then it only does so to toplevel. If this can be
           shown and we are currently at the toplevel of a compilation unit, the
           handler for the environment can remain marked as toplevel (and suitable
           for "let symbol" bindings); otherwise, it cannot. *)
        DE.at_unit_toplevel denv
        && (not is_exn_handler)
        && Continuation.Set.subset
          (CUE.all_continuations_used body_continuation_uses_env)
          (Continuation.Set.of_list [cont; DE.unit_toplevel_exn_continuation denv])
      in
      at_unit_toplevel, false, Continuation.Map.singleton cont (params, handler, is_exn_handler)
    | Recursive { continuation_handlers } ->
      false, true, Continuation.Map.map (fun (params, handler) -> (params, handler, false)) continuation_handlers
  in
  let consts_lifted_during_body = DA.get_lifted_constants dacc in
  let denv = DE.set_at_unit_toplevel_state denv at_unit_toplevel in
  let all_handlers_set = Continuation.Map.keys remaining_handlers in
  let used_handlers =
    Continuation.Set.inter all_handlers_set
      (CUE.all_continuations_used body_continuation_uses_env)
  in
  simplify_handlers ~simplify_expr ~down_to_up rebuild_body at_unit_toplevel is_recursive body_continuation_uses_env consts_lifted_during_body all_handlers_set used_handlers remaining_handlers Continuation.Set.empty Continuation.Map.empty dacc denv

let simplify_let_cont_stage1 ~simplify_expr dacc (stage1 : stage1) ~down_to_up =
  (* We begin to simplify a let cont by simplifying its body, so that we can see all uses of the handler
     in the non-recursive case, and so that we can know the values of all invariant arguments in the recursive
     case. We reset the [continuation_uses_env] so we can have precise information on continuations called
     by the body, and we reset the lifted constants because we need to add them to the handler's denv. *)
  let prior_cont_uses_env = DA.continuation_uses_env dacc in
  let dacc = DA.with_continuation_uses_env dacc ~cont_uses_env:CUE.empty in
  let dacc, prior_lifted_constants = DA.get_and_clear_lifted_constants dacc in
  let denv_before_body = DA.denv dacc in
  let original_cont_scope = DE.get_continuation_scope denv_before_body in
  let dacc =
    DA.with_denv dacc (DE.increment_continuation_scope denv_before_body)
  in
  let stage2 = {
    denv_before_body ;
    prior_lifted_constants ;
    prior_cont_uses_env ;
    original_cont_scope ;
    recinfo = stage1.recinfo
  }
  in
  simplify_expr dacc stage1.body ~down_to_up:(simplify_let_cont_stage2 ~simplify_expr stage2 ~down_to_up)

let simplify_let_cont ~simplify_expr dacc (let_cont : Let_cont.t) ~down_to_up =
  (* This is the entry point to simplify a let cont expression. The only thing it does is to match all handlers
     to break the name abstraction, and then call [simplify_let_cont_stage1]. *)
  let stage1 =
    match let_cont with
    | Non_recursive { handler; _ } ->
      let (cont, body) =
        Non_recursive_let_cont_handler.pattern_match handler ~f:(fun cont ~body -> (cont, body))
      in
      let cont_handler = Non_recursive_let_cont_handler.handler handler in
      let is_exn_handler = CH.is_exn_handler cont_handler in
      let (params, handler) = CH.pattern_match cont_handler ~f:(fun params ~handler -> (params, handler)) in
      { body ; recinfo = Non_recursive { cont; params; handler; is_exn_handler } }
    | Recursive handlers ->
      let (body, rec_handlers) = Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body rec_handlers -> (body, rec_handlers)) in
      assert (not (Continuation_handlers.contains_exn_handler rec_handlers));
      let handlers = Continuation_handlers.to_map rec_handlers in
      let continuation_handlers =
        Continuation.Map.map (fun handler -> CH.pattern_match handler ~f:(fun params ~handler -> (params, handler))) handlers
      in
      { body ; recinfo = Recursive { continuation_handlers } }
  in
  simplify_let_cont_stage1 ~simplify_expr dacc stage1 ~down_to_up
