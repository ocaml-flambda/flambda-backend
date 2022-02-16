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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import
module DE = Downwards_env
module DA = Downwards_acc
module T = Flambda2_types
module TE = T.Typing_env
module UA = Upwards_acc
module UE = Upwards_env

(* CR mshinwell for poechsel: We need to emit [Warnings.Inlining_impossible] as
   required.

   When in fallback-inlining mode: if we want to follow Closure we should not
   complain about function declarations with e.g. [@inline always] if the
   function contains other functions and therefore cannot be inlined. We should
   however contain at call sites if inlining is requested but cannot be done for
   this reason. I think this will probably all happen without any specific code
   once [Inlining_impossible] handling is implemented for the
   non-fallback-inlining cases. *)

(* CR-someday mshinwell: Overhaul handling of the inlining depth tracking so
   that it takes into account the depth of closures (or code), as per
   conversation with lwhite. *)

module FT = Flambda2_types.Function_type

let speculative_inlining dacc ~apply ~function_type ~simplify_expr ~return_arity
    =
  let dacc = DA.set_do_not_rebuild_terms_and_disable_inlining dacc in
  (* CR-someday poechsel: [Inlining_transforms.inline] is preparing the body for
     inlining. Right know it may be called twice (once there and once in
     [simplify_apply_expr]) on the same apply expr. It should be possible to
     only call it once and remove some allocations. *)
  let dacc, expr =
    (* The only way for [unroll_to] not to be None is when an explicit Unroll
       annotation is provided by the user. If this is the case then inliner will
       always inline the function and will not call [speculative_inlining]. Thus
       inside of [speculative_inlining] we will always have [unroll_to] = None.
       We are not disabling unrolling when speculating, it just happens that no
       unrolling can happen while speculating right now. *)
    Inlining_transforms.inline dacc ~apply ~unroll_to:None function_type
  in
  let scope = DE.get_continuation_scope (DA.denv dacc) in
  let dummy_toplevel_cont =
    Continuation.create ~name:"dummy_toplevel_continuation" ()
  in
  let dacc =
    DA.map_data_flow dacc ~f:(fun _ ->
        Data_flow.init_toplevel dummy_toplevel_cont [] Data_flow.empty)
  in
  let _, uacc =
    simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
        let exn_continuation = Apply.exn_continuation apply in
        let dacc =
          DA.map_data_flow dacc
            ~f:(Data_flow.exit_continuation dummy_toplevel_cont)
        in
        let data_flow = DA.data_flow dacc in
        (* The dataflow analysis *)
        let function_return_cont =
          match Apply.continuation apply with
          | Never_returns -> Continuation.create ()
          | Return cont -> cont
        in
        (* When doing the speculative analysis, in order to not blow up, the
           data_flow analysis is only done on the speculatively inlined body;
           however the reachable code_ids part of the data flow analysis is only
           correct at toplevel when all information about the code_age relation
           and used_closure vars is available (for the whole compilation unit).
           Thus we here provide empty/dummy values for the used_closure_vars and
           code_age_relation, and ignore the reachable_code_id part of the
           data_flow analysis. *)
        let ({ required_names; reachable_code_ids = _ } : Data_flow.result) =
          Data_flow.analyze data_flow ~code_age_relation:Code_age_relation.empty
            ~used_closure_vars:Unknown ~return_continuation:function_return_cont
            ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
        in
        let uenv =
          (* Note that we don't need to do anything special if the exception
             continuation takes extra arguments, since we are only simplifying
             the body of the function in question, not substituting it into an
             existing context. *)
          UE.add_function_return_or_exn_continuation
            (UE.create (DA.are_rebuilding_terms dacc))
            (Exn_continuation.exn_handler exn_continuation)
            scope
            [Flambda_kind.With_subkind.any_value]
        in
        let uenv =
          match Apply.continuation apply with
          | Never_returns -> uenv
          | Return return_continuation ->
            UE.add_function_return_or_exn_continuation uenv return_continuation
              scope return_arity
        in
        let uacc =
          UA.create ~required_names ~reachable_code_ids:Unknown
            ~compute_closure_offsets:false uenv dacc
        in
        rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc))
  in
  UA.cost_metrics uacc

let argument_types_useful dacc apply =
  if not
       (Flambda_features.Inlining.speculative_inlining_only_if_arguments_useful
          ())
  then true
  else
    let typing_env = DE.typing_env (DA.denv dacc) in
    List.exists
      (fun simple ->
        Simple.pattern_match simple
          ~name:(fun name ~coercion:_ ->
            let ty = TE.find typing_env name None in
            not (T.is_unknown typing_env ty))
          ~const:(fun _ -> true))
      (Apply.args apply)

let might_inline dacc ~apply ~code_or_metadata ~function_type ~simplify_expr
    ~return_arity : Call_site_inlining_decision_type.t =
  let denv = DA.denv dacc in
  let env_prohibits_inlining = not (DE.can_inline denv) in
  let decision =
    Code_or_metadata.code_metadata code_or_metadata
    |> Code_metadata.inlining_decision
  in
  if Function_decl_inlining_decision_type.must_be_inlined decision
  then Definition_says_inline
  else if Function_decl_inlining_decision_type.cannot_be_inlined decision
  then Definition_says_not_to_inline
  else if env_prohibits_inlining
  then Environment_says_never_inline
  else if not (argument_types_useful dacc apply)
  then Argument_types_not_useful
  else
    let cost_metrics =
      speculative_inlining ~apply dacc ~simplify_expr ~return_arity
        ~function_type
    in
    let inlining_args =
      Apply.inlining_arguments apply
      |> Inlining_arguments.meet (DE.inlining_arguments denv)
    in
    let evaluated_to = Cost_metrics.evaluate ~args:inlining_args cost_metrics in
    let threshold = Inlining_arguments.threshold inlining_args in
    let is_under_inline_threshold = Float.compare evaluated_to threshold <= 0 in
    if is_under_inline_threshold
    then Speculatively_inline { cost_metrics; evaluated_to; threshold }
    else Speculatively_not_inline { cost_metrics; evaluated_to; threshold }

let get_rec_info dacc ~function_type =
  let rec_info = FT.rec_info function_type in
  match Flambda2_types.prove_rec_info (DA.typing_env dacc) rec_info with
  | Proved rec_info -> rec_info
  | Unknown -> Rec_info_expr.unknown
  | Invalid -> Rec_info_expr.do_not_inline

let make_decision dacc ~simplify_expr ~function_type ~apply ~return_arity :
    Call_site_inlining_decision_type.t =
  let rec_info = get_rec_info dacc ~function_type in
  let inlined = Apply.inlined apply in
  match inlined with
  | Never_inlined -> Never_inlined_attribute
  | Default_inlined | Unroll _ | Always_inlined | Hint_inlined -> (
    let code_or_metadata =
      DE.find_code_exn (DA.denv dacc) (FT.code_id function_type)
    in
    if not (Code_or_metadata.code_present code_or_metadata)
    then Missing_code
    else
      (* The unrolling process is rather subtle, but it boils down to two steps:

         1. We see an [@unrolled n] annotation (with n > 0) on an apply
         expression whose [rec_info] has the unrolling state [Not_unrolling].
         When we inline the body, we bind [my_depth] to a rec_info whose
         unrolling state is [Unrolling { remaining_depth = n }].

         2. When we see that application again, its rec_info will have the
         unrolling state [Unrolling { remaining_depth = n - 1 }] (because its
         depth is [succ my_depth]). At that point, we short-circuit most of the
         inlining logic and inline if and only if n > 0.

         Here we're performing step _2_ (but only, of course, if we performed
         step 1 in a previous call to this function). *)
      let unrolling_depth =
        Simplify_rec_info_expr.known_remaining_unrolling_depth dacc rec_info
      in
      match unrolling_depth with
      | Some 0 -> Unrolling_depth_exceeded
      | Some _ ->
        might_inline dacc ~apply ~code_or_metadata ~function_type ~simplify_expr
          ~return_arity
      | None -> (
        (* lmaurer: This seems semantically dodgy: If we really think of a free
           depth variable as [Unknown], then we shouldn't be considering
           inlining here, because we don't _know_ that we're not unrolling. The
           behavior is what we want, though (and is consistent with FLambda 1):
           If there's a free depth variable, that means this is an internal
           recursive call, which means we consider unrolling if [@unrolled]
           appears. If it's known that the unrolling depth is zero, that means
           we're inlining into another function and we're done unrolling, so we
           immediately stop inlining.

           So this seems to be working for the moment, but I wonder what are the
           ramifications of treating unknown-ness as an observable property this
           way. Are we relying on monotonicity somewhere? *)
        let apply_inlining_state = Apply.inlining_state apply in
        if Inlining_state.is_depth_exceeded apply_inlining_state
        then Max_inlining_depth_exceeded
        else
          match inlined with
          | Never_inlined -> assert false
          | Default_inlined ->
            let max_rec_depth =
              Flambda_features.Inlining.max_rec_depth
                (Round (DE.round (DA.denv dacc)))
            in
            if Simplify_rec_info_expr.depth_may_be_at_least dacc rec_info
                 max_rec_depth
            then Recursion_depth_exceeded
            else
              might_inline dacc ~apply ~code_or_metadata ~function_type
                ~simplify_expr ~return_arity
          | Unroll unroll_to ->
            if Simplify_rec_info_expr.can_unroll dacc rec_info
            then
              (* This sets off step 1 in the comment above; see
                 [Inlining_transforms] for how [unroll_to] is ultimately
                 handled. *)
              Attribute_unroll unroll_to
            else Unrolling_depth_exceeded
          | Always_inlined | Hint_inlined -> Attribute_always))
