(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module DE = Downwards_env
module BP = Bound_parameter
module LCS = Lifted_constant_state
module T = Flambda_type
module TE = Flambda_type.Typing_env
module U = One_continuation_use

let simple_join denv typing_env uses ~params =
  (* This join is intended to be sufficient to match Closure + Cmmgen on
     unboxing, but not really anything more. *)
  let bottom_types =
    ListLabels.map params ~f:(fun param ->
        BP.kind param |> Flambda_kind.With_subkind.kind |> T.bottom)
  in
  let joined_types =
    ListLabels.fold_left uses ~init:bottom_types ~f:(fun joined_types use ->
        ListLabels.map2 joined_types (U.arg_types use)
          ~f:(fun joined_type arg_type ->
            let arg_type =
              T.eviscerate arg_type (DE.typing_env (U.env_at_use use))
            in
            (* The only names left in [arg_type] will be symbols; they will
               always be defined in [typing_env]. So we can use the same
               environment throughout the join. *)
            T.join typing_env ~left_env:typing_env ~left_ty:joined_type
              ~right_env:typing_env ~right_ty:arg_type))
  in
  let handler_env =
    ListLabels.fold_left2 params joined_types ~init:typing_env
      ~f:(fun handler_env param joined_type ->
        let name = BP.name param in
        TE.add_equation handler_env name joined_type)
  in
  let denv = DE.with_typing_env denv handler_env in
  denv, Continuation_extra_params_and_args.empty

let normal_join denv typing_env params ~env_at_fork_plus_params
    ~consts_lifted_during_body ~use_envs_with_ids =
  let definition_scope_level =
    DE.get_continuation_scope_level env_at_fork_plus_params
  in
  let extra_lifted_consts_in_use_envs =
    LCS.all_defined_symbols consts_lifted_during_body
  in
  let use_envs_with_ids' =
    (* CR-someday mshinwell: Stop allocating this *)
    List.map
      (fun (use_env, id, use_kind) -> DE.typing_env use_env, id, use_kind)
      use_envs_with_ids
  in
  let module CSE = Common_subexpression_elimination in
  let cse_join_result =
    assert (Scope.equal definition_scope_level (TE.current_scope typing_env));
    CSE.join ~typing_env_at_fork:typing_env ~cse_at_fork:(DE.cse denv)
      ~use_info:use_envs_with_ids
      ~get_typing_env:(fun (use_env, _, _) -> DE.typing_env use_env)
      ~get_rewrite_id:(fun (_, id, _) -> id)
      ~get_cse:(fun (use_env, _, _) -> DE.cse use_env)
      ~params
  in
  let extra_params_and_args =
    match cse_join_result with
    | None -> Continuation_extra_params_and_args.empty
    | Some cse_join_result -> cse_join_result.extra_params
  in
  let extra_allowed_names =
    match cse_join_result with
    | None -> Name_occurrences.empty
    | Some cse_join_result -> cse_join_result.extra_allowed_names
  in
  let env =
    TE.cut_and_n_way_join typing_env use_envs_with_ids'
      ~params
        (* CR-someday mshinwell: If this didn't do Scope.next then TE could
           probably be slightly more efficient, as it wouldn't need to look at
           the middle of the three return values from Scope.Map.Split. *)
      ~unknown_if_defined_at_or_later_than:(Scope.next definition_scope_level)
      ~extra_lifted_consts_in_use_envs ~extra_allowed_names
  in
  let handler_env =
    env
    |> TE.add_definitions_of_params ~params:extra_params_and_args.extra_params
  in
  let handler_env =
    match cse_join_result with
    | None -> handler_env
    | Some cse_join_result ->
      Name.Map.fold
        (fun name ty handler_env -> TE.add_equation handler_env name ty)
        cse_join_result.extra_equations handler_env
  in
  let denv =
    let denv = DE.with_typing_env denv handler_env in
    match cse_join_result with
    | None -> denv
    | Some cse_join_result -> DE.with_cse denv cse_join_result.cse_at_join_point
  in
  denv, extra_params_and_args

let compute_handler_env uses ~env_at_fork_plus_params ~consts_lifted_during_body
    ~params ~code_age_relation_after_body : Continuation_env_and_param_types.t =
  (* Augment the environment at each use with the necessary equations about the
     parameters (whose variables will already be defined in the environment). *)
  let need_to_meet_param_types =
    (* If there is information available from the subkinds of the parameters, we
       will need to meet the existing parameter types (e.g. "unknown boxed
       float") with the argument types at each use. *)
    List.exists
      (fun param ->
        BP.kind param |> Flambda_kind.With_subkind.has_useful_subkind_info)
      params
  in
  let uses_list = Continuation_uses.get_uses uses in
  let use_envs_with_ids =
    List.map
      (fun use ->
        let add_or_meet_param_type typing_env =
          let param_types = U.arg_types use in
          if need_to_meet_param_types
          then TE.meet_equations_on_params typing_env ~params ~param_types
          else TE.add_equations_on_params typing_env ~params ~param_types
        in
        let use_env =
          DE.map_typing_env (U.env_at_use use) ~f:add_or_meet_param_type
        in
        use_env, U.id use, U.use_kind use)
      uses_list
  in
  let arg_types_by_use_id = Continuation_uses.get_arg_types_by_use_id uses in
  match use_envs_with_ids with
  | [(use_env, _, Inlinable)] ->
    (* There is only one use of the continuation and it is inlinable. No join
       calculations are required.

       We need to make sure any lifted constants generated during the
       simplification of the body are in the returned environment for the
       handler. Otherwise we might share a constant based on information in [DA]
       but then find the definition of the corresponding constant isn't in [DE].
       Note that some of the constants may already be defined (for example
       because they were defined on the path between the fork point and this
       particular use). *)
    let handler_env =
      LCS.add_to_denv ~maybe_already_defined:() use_env
        consts_lifted_during_body
    in
    Uses
      { handler_env;
        arg_types_by_use_id;
        extra_params_and_args = Continuation_extra_params_and_args.empty;
        is_single_inlinable_use = true;
        escapes = false
      }
  | [] | [(_, _, Non_inlinable _)] | (_, _, (Inlinable | Non_inlinable _)) :: _
    ->
    (* This is the general case.

       The lifted constants are put into the _fork_ environment now because it
       overall makes things easier; the join operation can just discard any
       equation about a lifted constant (any such equation could not be
       materially more precise anyway). *)
    let denv =
      LCS.add_to_denv env_at_fork_plus_params consts_lifted_during_body
    in
    let typing_env = DE.typing_env denv in
    let handler_env, extra_params_and_args =
      if not (Flambda_features.join_points ())
      then simple_join denv typing_env uses_list ~params
      else
        normal_join denv typing_env params ~env_at_fork_plus_params
          ~consts_lifted_during_body ~use_envs_with_ids
    in
    let handler_env =
      DE.map_typing_env handler_env ~f:(fun handler_env ->
          TE.with_code_age_relation handler_env code_age_relation_after_body)
    in
    let escapes =
      List.exists
        (fun (_, _, (cont_use_kind : Continuation_use_kind.t)) ->
          match cont_use_kind with
          | Inlinable | Non_inlinable { escaping = false } -> false
          | Non_inlinable { escaping = true } -> true)
        use_envs_with_ids
    in
    Uses
      { handler_env;
        arg_types_by_use_id;
        extra_params_and_args;
        is_single_inlinable_use = false;
        escapes
      }
