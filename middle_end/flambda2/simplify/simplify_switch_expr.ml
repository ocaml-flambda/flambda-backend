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
module TE = Flambda2_types.Typing_env
module Alias_set = TE.Alias_set

[@@@ocaml.warning "-37"]

type mergeable_arms =
  | No_arms
  | Mergeable of
      { cont : Continuation.t;
        args : Alias_set.t list
      }
  | Not_mergeable

let find_all_aliases env arg =
  let find_all_aliases () =
    TE.aliases_of_simple env ~min_name_mode:NM.normal arg
  in
  Simple.pattern_match'
    ~var:(fun _var ~coercion:_ ->
      (* We use find alias to find a common simple to different
         simples.

         This simple is already guaranteed to be the cannonical alias.

       * If there is a common alias between variables, the
         cannonical alias must also be a common alias.

       * For constants and symbols there can be a common alias that
         is not cannonical: A variable can have different constant
         values in different branches: this variable is not the
         cannonical alias, the cannonical would be the constant or
         the symbol. But the only common alias could be a variable
         in that case.

         hence there is no loss of generality in returning the
         cannonical alias as the single alias if it is a variable.

         Note that the main reason for this is to allow changing the
         arguments of continuations to variables that where not in
         scope during the downward traversal. In particular for the
         alias rewriting provided by data_flow *)
      TE.Alias_set.singleton arg)
    ~symbol:(fun _sym ~coercion:_ -> find_all_aliases ())
    ~const:(fun _cst -> find_all_aliases ())
    arg

let rebuild_arm uacc arm (action, use_id, arity, env_at_use)
    ( new_let_conts,
      arms,
      (mergeable_arms : mergeable_arms),
      identity_arms,
      not_arms ) =
  let action =
    Simplify_common.clear_demoted_trap_action_and_patch_unused_exn_bucket uacc
      action
  in
  match EB.rewrite_switch_arm uacc action ~use_id arity with
  | Apply_cont action -> (
    let action =
      let cont = Apply_cont.continuation action in
      let cont_info_from_uenv = UE.find_continuation (UA.uenv uacc) cont in
      (* First try to absorb any [Apply_cont] expression that forms the entirety
         of the arm's action (via an intermediate zero-arity continuation
         without trap action) into the [Switch] expression itself. *)
      match cont_info_from_uenv with
      | Invalid _ -> None
      | Linearly_used_and_inlinable _ | Non_inlinable_zero_arity _
      | Non_inlinable_non_zero_arity _
      | Toplevel_or_function_return_or_exn_continuation _ -> (
        if not (Apply_cont.is_goto action)
        then Some action
        else
          let check_handler ~handler ~action =
            match RE.to_apply_cont handler with
            | Some action -> Some action
            | None -> Some action
          in
          match cont_info_from_uenv with
          | Linearly_used_and_inlinable
              { handler;
                free_names_of_handler = _;
                params;
                cost_metrics_of_handler = _
              } ->
            assert (Bound_parameters.is_empty params);
            check_handler ~handler ~action
          | Non_inlinable_zero_arity { handler = Known handler } ->
            check_handler ~handler ~action
          | Non_inlinable_zero_arity { handler = Unknown } -> Some action
          | Invalid _ -> None
          | Non_inlinable_non_zero_arity _
          | Toplevel_or_function_return_or_exn_continuation _ ->
            Misc.fatal_errorf
              "Inconsistency for %a between [Apply_cont.is_goto] and \
               continuation environment in [UA]:@ %a"
              Continuation.print cont UA.print uacc)
    in
    match action with
    | None ->
      (* The destination is unreachable; delete the [Switch] arm. *)
      new_let_conts, arms, mergeable_arms, identity_arms, not_arms
    | Some action -> (
      (* CR mshinwell/vlaviron: Fix alias handling so that identity switches
         like those in id_switch.ml can be simplified by only using
         [mergeable_arms]. Then remove [identity_arms]. *)
      let maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms =
        let arms = Targetint_31_63.Map.add arm action arms in
        (* Check to see if this arm may be merged with others. *)
        if Option.is_some (Apply_cont.trap_action action)
        then new_let_conts, arms, Not_mergeable, identity_arms, not_arms
        else
          match mergeable_arms with
          | Not_mergeable ->
            new_let_conts, arms, Not_mergeable, identity_arms, not_arms
          | No_arms ->
            let cont = Apply_cont.continuation action in
            let args =
              List.map
                (fun arg -> find_all_aliases env_at_use arg)
                (Apply_cont.args action)
            in
            ( new_let_conts,
              arms,
              Mergeable { cont; args },
              identity_arms,
              not_arms )
          | Mergeable { cont; args } ->
            if not (Continuation.equal cont (Apply_cont.continuation action))
            then new_let_conts, arms, Not_mergeable, identity_arms, not_arms
            else
              let args =
                List.map2
                  (fun arg_set arg ->
                    Alias_set.inter (find_all_aliases env_at_use arg) arg_set)
                  args (Apply_cont.args action)
              in
              ( new_let_conts,
                arms,
                Mergeable { cont; args },
                identity_arms,
                not_arms )
      in
      (* Check to see if the arm is of a form that might mean the whole [Switch]
         is a boolean NOT. *)
      match Apply_cont.to_one_arg_without_trap_action action with
      | None -> maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
      | Some arg ->
        let[@inline always] const arg =
          match Reg_width_const.descr arg with
          | Tagged_immediate arg ->
            if Targetint_31_63.equal arm arg
            then
              let identity_arms =
                Targetint_31_63.Map.add arm action identity_arms
              in
              maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
            else if Targetint_31_63.equal arm Targetint_31_63.bool_true
                    && Targetint_31_63.equal arg Targetint_31_63.bool_false
                    || Targetint_31_63.equal arm Targetint_31_63.bool_false
                       && Targetint_31_63.equal arg Targetint_31_63.bool_true
            then
              let not_arms = Targetint_31_63.Map.add arm action not_arms in
              maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
            else maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
          | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms
        in
        Simple.pattern_match arg ~const ~name:(fun _ ~coercion:_ ->
            maybe_mergeable ~mergeable_arms ~identity_arms ~not_arms)))
  | New_wrapper new_let_cont ->
    let new_let_conts = new_let_cont :: new_let_conts in
    let action = Apply_cont.goto new_let_cont.cont in
    let arms = Targetint_31_63.Map.add arm action arms in
    new_let_conts, arms, Not_mergeable, identity_arms, not_arms

let filter_and_choose_alias required_names alias_set =
  let available_alias_set =
    Alias_set.filter alias_set ~f:(fun alias ->
        Simple.pattern_match alias
          ~name:(fun name ~coercion:_ -> Name.Set.mem name required_names)
          ~const:(fun _ -> true))
  in
  Alias_set.find_best available_alias_set

let find_cse_simple dacc required_names prim =
  match P.Eligible_for_cse.create prim with
  | None -> None (* Constant *)
  | Some with_fixed_value -> (
    match DE.find_cse (DA.denv dacc) with_fixed_value with
    | None -> None
    | Some simple ->
      filter_and_choose_alias required_names
        (find_all_aliases (DA.typing_env dacc) simple))

let rebuild_switch ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
    ~dacc_before_switch uacc ~after_rebuild =
  let new_let_conts, arms, mergeable_arms, identity_arms, not_arms =
    Targetint_31_63.Map.fold (rebuild_arm uacc) arms
      ( [],
        Targetint_31_63.Map.empty,
        No_arms,
        Targetint_31_63.Map.empty,
        Targetint_31_63.Map.empty )
  in
  let switch_merged =
    match mergeable_arms with
    | No_arms | Not_mergeable -> None
    | Mergeable { cont; args } ->
      let num_args = List.length args in
      let required_names = UA.required_names uacc in
      let args =
        List.filter_map (filter_and_choose_alias required_names) args
      in
      if List.compare_length_with args num_args = 0
      then Some (cont, args)
      else None
  in
  let switch_is_identity =
    let arm_discrs = Targetint_31_63.Map.keys arms in
    let identity_arms_discrs = Targetint_31_63.Map.keys identity_arms in
    if not (Targetint_31_63.Set.equal arm_discrs identity_arms_discrs)
    then None
    else
      Targetint_31_63.Map.data identity_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list |> Continuation.Set.get_singleton
  in
  let switch_is_boolean_not =
    let arm_discrs = Targetint_31_63.Map.keys arms in
    let not_arms_discrs = Targetint_31_63.Map.keys not_arms in
    if (not (Targetint_31_63.Set.equal arm_discrs Targetint_31_63.all_bools))
       || not (Targetint_31_63.Set.equal arm_discrs not_arms_discrs)
    then None
    else
      Targetint_31_63.Map.data not_arms
      |> List.map Apply_cont.continuation
      |> Continuation.Set.of_list |> Continuation.Set.get_singleton
  in
  let body, uacc =
    if Targetint_31_63.Map.cardinal arms < 1
    then
      let uacc = UA.notify_removed ~operation:Removed_operations.branch uacc in
      RE.create_invalid Zero_switch_arms, uacc
    else
      let dbg = Debuginfo.none in
      let[@inline] normal_case uacc =
        (* In that case, even though some branches were removed by simplify we
           should not count them in the number of removed operations: these
           branches wouldn't have been taken during execution anyway. *)
        let expr, uacc =
          EB.create_switch uacc ~condition_dbg ~scrutinee ~arms
        in
        if Flambda_features.check_invariants ()
           && Simple.is_const scrutinee
           && Targetint_31_63.Map.cardinal arms > 1
        then
          Misc.fatal_errorf
            "[Switch] with constant scrutinee (type: %a) should have been \
             simplified away:@ %a"
            T.print scrutinee_ty
            (RE.print (UA.are_rebuilding_terms uacc))
            expr;
        expr, uacc
      in
      match switch_merged with
      | Some (dest, args) ->
        let uacc =
          UA.notify_removed ~operation:Removed_operations.branch uacc
        in
        let apply_cont = Apply_cont.create dest ~args ~dbg in
        let expr = RE.create_apply_cont apply_cont in
        let uacc = UA.add_free_names uacc (Apply_cont.free_names apply_cont) in
        expr, uacc
      | None -> (
        match switch_is_identity with
        | Some dest -> (
          let uacc =
            UA.notify_removed ~operation:Removed_operations.branch uacc
          in
          let tagging_prim : P.t = Unary (Tag_immediate, scrutinee) in
          match
            find_cse_simple dacc_before_switch (UA.required_names uacc)
              tagging_prim
          with
          | None -> normal_case uacc
          | Some tagged_scrutinee ->
            let apply_cont =
              Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg
            in
            let expr = RE.create_apply_cont apply_cont in
            let uacc =
              UA.add_free_names uacc (Apply_cont.free_names apply_cont)
            in
            expr, uacc)
        | None -> (
          match switch_is_boolean_not with
          | Some dest -> (
            let uacc =
              UA.notify_removed ~operation:Removed_operations.branch uacc
            in
            let not_scrutinee = Variable.create "not_scrutinee" in
            let not_scrutinee' = Simple.var not_scrutinee in
            let tagging_prim : P.t = Unary (Tag_immediate, scrutinee) in
            match
              find_cse_simple dacc_before_switch (UA.required_names uacc)
                tagging_prim
            with
            | None -> normal_case uacc
            | Some tagged_scrutinee ->
              let do_tagging =
                Named.create_prim
                  (P.Unary (Boolean_not, tagged_scrutinee))
                  Debuginfo.none
              in
              let bound =
                VB.create not_scrutinee NM.normal |> Bound_pattern.singleton
              in
              let apply_cont =
                Apply_cont.create dest ~args:[not_scrutinee'] ~dbg
              in
              let body = RE.create_apply_cont apply_cont in
              let free_names_of_body = Apply_cont.free_names apply_cont in
              let expr =
                RE.create_let
                  (UA.are_rebuilding_terms uacc)
                  bound do_tagging ~body ~free_names_of_body
              in
              let uacc =
                UA.add_free_names uacc
                  (NO.union
                     (Named.free_names do_tagging)
                     (NO.diff free_names_of_body
                        ~without:(NO.singleton_variable not_scrutinee NM.normal)))
              in
              expr, uacc)
          | None -> normal_case uacc))
  in
  let uacc, expr = EB.bind_let_conts uacc ~body new_let_conts in
  after_rebuild expr uacc

let simplify_arm ~typing_env_at_use ~scrutinee_ty arm action (arms, dacc) =
  let shape = T.this_naked_immediate arm in
  match T.meet typing_env_at_use scrutinee_ty shape with
  | Bottom -> arms, dacc
  | Ok (_meet_ty, env_extension) ->
    let env_at_use = TE.add_env_extension typing_env_at_use env_extension in
    let denv_at_use = DE.with_typing_env (DA.denv dacc) env_at_use in
    let args = AC.args action in
    let use_kind =
      Simplify_common.apply_cont_use_kind ~context:Switch_branch action
    in
    let { S.simples = args; simple_tys = arg_types } =
      S.simplify_simples dacc args
    in
    let dacc, rewrite_id =
      DA.record_continuation_use dacc (AC.continuation action) use_kind
        ~env_at_use:denv_at_use ~arg_types
    in
    let arity =
      arg_types
      |> List.map (fun ty -> K.With_subkind.anything (T.kind ty))
      |> Flambda_arity.create
    in
    let action = Apply_cont.update_args action ~args in
    let dacc =
      DA.map_flow_acc dacc
        ~f:
          (Flow.Acc.add_apply_cont_args ~rewrite_id
             (Apply_cont.continuation action)
             args)
    in
    let arms =
      Targetint_31_63.Map.add arm (action, rewrite_id, arity, env_at_use) arms
    in
    arms, dacc

let simplify_switch0 dacc switch ~down_to_up =
  let scrutinee = Switch.scrutinee switch in
  let scrutinee_ty =
    S.simplify_simple dacc scrutinee ~min_name_mode:NM.normal
  in
  let scrutinee = T.get_alias_exn scrutinee_ty in
  let dacc_before_switch = dacc in
  let typing_env_at_use = DA.typing_env dacc in
  let arms, dacc =
    Targetint_31_63.Map.fold
      (simplify_arm ~typing_env_at_use ~scrutinee_ty)
      (Switch.arms switch)
      (Targetint_31_63.Map.empty, dacc)
  in
  let dacc =
    if Targetint_31_63.Map.cardinal arms <= 1
    then dacc
    else
      DA.map_flow_acc dacc
        ~f:(Flow.Acc.add_used_in_current_handler (Simple.free_names scrutinee))
  in
  let condition_dbg =
    DE.add_inlined_debuginfo (DA.denv dacc) (Switch.condition_dbg switch)
  in
  down_to_up dacc
    ~rebuild:
      (rebuild_switch ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
         ~dacc_before_switch)

let simplify_switch ~simplify_let ~simplify_function_body dacc switch
    ~down_to_up =
  let tagged_scrutinee = Variable.create "tagged_scrutinee" in
  let tagging_prim =
    Named.create_prim
      (Unary (Tag_immediate, Switch.scrutinee switch))
      Debuginfo.none
  in
  let let_expr =
    (* [body] won't be looked at (see below). *)
    Let.create
      (Bound_pattern.singleton (Bound_var.create tagged_scrutinee NM.normal))
      tagging_prim
      ~body:(Expr.create_switch switch)
      ~free_names_of_body:Unknown
  in
  let dacc =
    DA.map_flow_acc dacc
      ~f:
        (Flow.Acc.add_used_in_current_handler
           (NO.singleton_variable tagged_scrutinee NM.normal))
  in
  simplify_let
    ~simplify_expr:(fun dacc _body ~down_to_up ->
      simplify_switch0 dacc switch ~down_to_up)
    ~simplify_function_body dacc let_expr ~down_to_up
