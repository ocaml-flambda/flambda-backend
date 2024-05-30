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
module U = Unboxing_types
module Decisions = U.Decisions

let refine_decision_based_on_arg_types_at_uses ~pass ~rewrite_ids_seen
    ~rewrites_ids_known_as_invalid nth_arg arg_type_by_use_id
    (decision : U.decision) =
  match decision with
  | Do_not_unbox _ as decision -> decision, Apply_cont_rewrite_id.Set.empty
  | Unbox _ as decision ->
    Apply_cont_rewrite_id.Map.fold
      (fun rewrite_id (arg_at_use : Continuation_uses.arg_at_use)
           (decision, invalids) ->
        if Apply_cont_rewrite_id.Set.mem rewrite_id rewrite_ids_seen
           || Apply_cont_rewrite_id.Set.mem rewrite_id
                rewrites_ids_known_as_invalid
        then decision, invalids
        else
          let typing_env_at_use = arg_at_use.typing_env in
          let arg_type_at_use = arg_at_use.arg_type in
          let unboxed_arg =
            match
              TE.get_alias_then_canonical_simple_exn typing_env_at_use
                ~min_name_mode:Name_mode.normal arg_type_at_use
            with
            | simple -> Unboxing_epa.Available simple
            | exception Not_found ->
              Unboxing_epa.Added_by_wrapper_at_rewrite_use { nth_arg }
          in
          try
            let decision =
              Unboxing_epa.compute_extra_args_for_one_decision_and_use ~pass
                rewrite_id ~typing_env_at_use unboxed_arg decision
            in
            decision, invalids
          with Unboxing_epa.Invalid_apply_cont ->
            decision, Apply_cont_rewrite_id.Set.add rewrite_id invalids)
      arg_type_by_use_id
      (decision, rewrites_ids_known_as_invalid)

module List = struct
  include List

  let rec fold_left3 f accu l1 l2 l3 =
    match l1, l2, l3 with
    | [], [], [] -> accu
    | a1 :: l1, a2 :: l2, a3 :: l3 -> fold_left3 f (f accu a1 a2 a3) l1 l2 l3
    | _, _, _ -> invalid_arg "List.fold_left3"
end

let make_do_not_unbox_decisions params : Decisions.t =
  let decisions =
    List.map
      (fun param -> param, U.Do_not_unbox Unboxing_not_requested)
      (Bound_parameters.to_list params)
  in
  { decisions;
    rewrite_ids_seen = Apply_cont_rewrite_id.Set.empty;
    rewrites_ids_known_as_invalid = Apply_cont_rewrite_id.Set.empty
  }

type continuation_arg_types =
  | Recursive
  | Non_recursive of Continuation_uses.arg_types_by_use_id

let make_decisions ~continuation_arg_types denv params params_types :
    DE.t * Decisions.t =
  let params = Bound_parameters.to_list params in
  let continuation_is_recursive, arg_types_by_use_id =
    match continuation_arg_types with
    | Recursive ->
      true, List.map (fun _ -> Apply_cont_rewrite_id.Map.empty) params
    | Non_recursive arg_types_by_use_id -> false, arg_types_by_use_id
  in
  let empty = Apply_cont_rewrite_id.Set.empty in
  let _, denv, rev_decisions, seen, invalids =
    List.fold_left3
      (fun (nth, denv, rev_decisions, seen, invalids) param param_type
           arg_type_by_use_id ->
        (* Make an optimistic decision, filter it based on the arg types at the
           use sites (to prevent decisions that would be detrimental), and
           compute the necessary denv. *)
        let decision =
          Optimistic_unboxing_decision.make_optimistic_decision ~depth:0
            ~recursive:continuation_is_recursive (DE.typing_env denv)
            ~param_type
        in
        let decision, invalids =
          if continuation_is_recursive
          then
            (* For recursive continuation whether unboxing is beneficial or not
               does not really depends on the external use site: the body of the
               loop matters more than the entry. In the worst case, unboxing for
               recursive continuation risk introducing an allocation when
               leaving the loop if the value is unused, while the benefit might
               be great most of the time. *)
            decision, invalids
          else
            let decision, invalids =
              refine_decision_based_on_arg_types_at_uses ~rewrite_ids_seen:empty
                ~rewrites_ids_known_as_invalid:invalids nth arg_type_by_use_id
                ~pass:Filter decision
            in
            let decision =
              Is_unboxing_beneficial.filter_non_beneficial_decisions decision
            in
            decision, invalids
        in
        let denv =
          Build_unboxing_denv.denv_of_decision denv ~param_var:(BP.var param)
            decision
        in
        (* Compute the set of rewrite ids that have been considered when
           updating decisions, and check that all [arg_type_by_use_id]s cover
           the same set of rewrite ids. *)
        let seen =
          match seen with
          | Some s ->
            assert (
              Apply_cont_rewrite_id.Map.for_all
                (fun id _ -> Apply_cont_rewrite_id.Set.mem id s)
                arg_type_by_use_id);
            s
          | None ->
            Apply_cont_rewrite_id.Map.fold
              (fun id _ acc -> Apply_cont_rewrite_id.Set.add id acc)
              arg_type_by_use_id empty
        in
        nth + 1, denv, decision :: rev_decisions, Some seen, invalids)
      (0, denv, [], None, Apply_cont_rewrite_id.Set.empty)
      params params_types arg_types_by_use_id
  in
  let rewrite_ids_seen = match seen with None -> empty | Some s -> s in
  let decisions = List.combine params (List.rev rev_decisions) in
  ( denv,
    { decisions; rewrite_ids_seen; rewrites_ids_known_as_invalid = invalids } )

let compute_extra_params_and_args
    ({ decisions; rewrite_ids_seen; rewrites_ids_known_as_invalid } :
      Decisions.t) ~arg_types_by_use_id existing_extra_params_and_args =
  let _, extra_params_and_args, _ =
    List.fold_left2
      (fun (nth, extra_params_and_args, invalids) arg_type_by_use_id
           (_, decision) ->
        let decision, invalids =
          refine_decision_based_on_arg_types_at_uses
            ~pass:Compute_all_extra_args ~rewrite_ids_seen
            ~rewrites_ids_known_as_invalid:invalids nth arg_type_by_use_id
            decision
        in
        let extra_params_and_args =
          Unboxing_epa.add_extra_params_and_args extra_params_and_args ~invalids
            decision
        in
        nth + 1, extra_params_and_args, invalids)
      (0, existing_extra_params_and_args, rewrites_ids_known_as_invalid)
      arg_types_by_use_id decisions
  in
  extra_params_and_args
