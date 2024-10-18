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

open Simplify_import
module U = One_continuation_use

type result =
  { handler_env : DE.t;
    extra_params_and_args : Continuation_extra_params_and_args.t;
    is_single_inlinable_use : bool;
    escapes : bool
  }

let introduce_extra_params_in_use_env epa (denv_at_use, use_id, kind) =
  let tenv_at_use = DE.typing_env denv_at_use in
  let tenv_at_use =
    TE.add_definitions_of_params tenv_at_use ~params:(EPA.extra_params epa)
  in
  match Apply_cont_rewrite_id.Map.find use_id (EPA.extra_args epa) with
  | exception Not_found ->
    Misc.fatal_errorf
      "No extra args for rewrite Id %a@.Extra params and args: %a"
      Apply_cont_rewrite_id.print use_id EPA.print epa
  | Invalid -> None
  | Ok extra_args ->
    let tenv_at_use =
      List.fold_left2
        (fun tenv_at_use param (arg : EPA.Extra_arg.t) ->
          match arg with
          | Already_in_scope s ->
            TE.add_equation tenv_at_use (BP.name param)
              (T.alias_type_of
                 (BP.kind param |> Flambda_kind.With_subkind.kind)
                 s)
          | New_let_binding _ | New_let_binding_with_named_args _ -> tenv_at_use)
        tenv_at_use
        (Bound_parameters.to_list (EPA.extra_params epa))
        extra_args
    in
    Some (tenv_at_use, use_id, kind)

let introduce_extra_params_for_join denv use_envs_with_ids
    ~extra_params_and_args =
  if EPA.is_empty extra_params_and_args
  then
    let use_tenvs_with_ids =
      List.map
        (fun (denv_at_use, use_id, kind) ->
          DE.typing_env denv_at_use, use_id, kind)
        use_envs_with_ids
    in
    denv, use_tenvs_with_ids
  else
    let extra_params = EPA.extra_params extra_params_and_args in
    let denv = DE.define_parameters denv ~params:extra_params in
    let use_envs_with_ids =
      List.filter_map
        (introduce_extra_params_in_use_env extra_params_and_args)
        use_envs_with_ids
    in
    denv, use_envs_with_ids

let join ?cut_after denv params ~consts_lifted_during_body ~use_envs_with_ids
    ~lifted_cont_extra_params_and_args =
  let definition_scope = DE.get_continuation_scope denv in
  let extra_lifted_consts_in_use_envs =
    LCS.all_defined_symbols consts_lifted_during_body
  in
  let module CSE = Common_subexpression_elimination in
  let cse_join_result =
    let typing_env = DE.typing_env denv in
    assert (Scope.equal definition_scope (TE.current_scope typing_env));
    CSE.join ~typing_env_at_fork:typing_env ~cse_at_fork:(DE.cse denv)
      ~use_info:use_envs_with_ids
      ~get_typing_env:(fun (use_env, _, _) -> DE.typing_env use_env)
      ~get_rewrite_id:(fun (_, id, _) -> id)
      ~get_cse:(fun (use_env, _, _) -> DE.cse use_env)
      ~params
  in
  let extra_params_and_args =
    match cse_join_result with
    | None -> lifted_cont_extra_params_and_args
    | Some cse_join_result ->
      (* CR gbury: the order of the EPA should not matter here *)
      EPA.concat ~outer:cse_join_result.extra_params
        ~inner:lifted_cont_extra_params_and_args
  in
  let denv, use_envs_with_ids' =
    introduce_extra_params_for_join denv use_envs_with_ids
      ~extra_params_and_args
  in
  let extra_allowed_names =
    match cse_join_result with
    | None -> NO.empty
    | Some cse_join_result -> cse_join_result.extra_allowed_names
  in
  let cut_after = Option.value cut_after ~default:definition_scope in
  let handler_env =
    T.cut_and_n_way_join (DE.typing_env denv) use_envs_with_ids'
      ~params:
        (Bound_parameters.append params
           (EPA.extra_params extra_params_and_args))
      ~cut_after ~extra_lifted_consts_in_use_envs ~extra_allowed_names
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

let add_equations_on_params typing_env ~is_recursive ~params:params'
    ~param_types =
  let params = Bound_parameters.to_list params' in
  let number_of_parameters_mismatch () =
    Misc.fatal_errorf
      "Mismatch between number of continuation parameters and arguments at a \
       use site:@ (%a)@ and@ %a"
      Bound_parameters.print params'
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      param_types
  in
  let rec add_equations_on_params typing_env params param_types =
    match params with
    | [] ->
      if Flambda_features.check_invariants ()
         && (not is_recursive)
         && match param_types with [] -> false | _ :: _ -> true
      then number_of_parameters_mismatch ();
      typing_env
    | param :: params -> (
      match param_types with
      | [] -> number_of_parameters_mismatch ()
      | param_type :: param_types ->
        let name = Bound_parameter.name param in
        let kind = Bound_parameter.kind param in
        let typing_env =
          if Flambda_kind.With_subkind.has_useful_subkind_info kind
          then
            let raw_kind = Flambda_kind.With_subkind.kind kind in
            let type_from_kind = T.unknown_with_subkind kind in
            match T.meet typing_env type_from_kind param_type with
            | Bottom ->
              (* This should really replace the corresponding uses with
                 [Invalid], but this seems an unusual situation, so we don't do
                 that currently. *)
              TE.add_equation typing_env name (T.bottom raw_kind)
            | Ok (meet_ty, env_extension) ->
              let typing_env = TE.add_equation typing_env name meet_ty in
              TE.add_env_extension typing_env env_extension
          else TE.add_equation typing_env name param_type
        in
        add_equations_on_params typing_env params param_types)
  in
  add_equations_on_params typing_env params param_types

let compute_handler_env ?cut_after uses ~is_recursive ~env_at_fork
    ~consts_lifted_during_body ~params ~lifted_cont_extra_params_and_args =
  (* Augment the environment at each use with the parameter definitions and
     associated equations. *)
  let use_envs_with_ids =
    List.map
      (fun use ->
        let add_or_meet_param_type typing_env =
          let param_types = U.arg_types use in
          add_equations_on_params typing_env ~is_recursive ~params ~param_types
        in
        let use_env =
          let use_env = DE.define_parameters (U.env_at_use use) ~params in
          DE.map_typing_env use_env ~f:add_or_meet_param_type
        in
        use_env, U.id use, U.use_kind use)
      uses
  in
  match use_envs_with_ids with
  | [(use_env, use_id, Inlinable)] when not is_recursive ->
    (* First add the extra params and args equations in the typing env *)
    let use_tenv =
      let use = use_env, use_id, Continuation_use_kind.Inlinable in
      match
        introduce_extra_params_in_use_env lifted_cont_extra_params_and_args use
      with
      | Some (use_env, _, _) -> use_env
      | None ->
        (* CR gbury: This case means that the EPA rewrite states the apply_cont
           is actually invalid. This should not happen currently as lifted cont
           epas do not generate invalid rewrites.

           We could try and handle this case by replacing the continuation's
           handler with an [Invalid] *)
        Misc.fatal_errorf
          "Apply_cont of a single-use inlinable continuation is Invalid."
    in
    let use_env = DE.with_typing_env use_env use_tenv in
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
    (* The use environment might have a deeper inlining depth increment than the
       fork environment. (e.g. where an [Apply] was inlined, revealing the
       linear inlinable use of the continuation). We need to make sure the
       handler is simplified using the depth from the fork environment. Likewise
       for the inlining history tracker and debuginfo. *)
    let handler_env =
      DE.set_inlining_state handler_env (DE.get_inlining_state env_at_fork)
    in
    let handler_env =
      DE.set_inlining_history_tracker
        (DE.inlining_history_tracker env_at_fork)
        handler_env
    in
    let handler_env = DE.set_inlined_debuginfo handler_env ~from:env_at_fork in
    let handler_env =
      DE.set_at_unit_toplevel_state handler_env
        (DE.at_unit_toplevel env_at_fork)
    in
    { handler_env;
      extra_params_and_args = EPA.empty;
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
    let denv = LCS.add_to_denv env_at_fork consts_lifted_during_body in
    let should_do_join =
      Flambda_features.join_points ()
      || match use_envs_with_ids with [] | [_] -> true | _ :: _ :: _ -> false
    in
    let handler_env, extra_params_and_args =
      if should_do_join
      then
        (* No need to add equations, as they will be computed from the use
           environments *)
        let denv = DE.define_parameters denv ~params in
        join ?cut_after denv params ~consts_lifted_during_body
          ~use_envs_with_ids ~lifted_cont_extra_params_and_args
      else
        (* Define parameters with basic equations from the subkinds *)
        let denv = DE.add_parameters_with_unknown_types denv params in
        let denv =
          DE.add_parameters_with_unknown_types denv
            (EPA.extra_params lifted_cont_extra_params_and_args)
        in
        denv, lifted_cont_extra_params_and_args
    in
    let escapes =
      List.exists
        (fun (_, _, (cont_use_kind : Continuation_use_kind.t)) ->
          match cont_use_kind with
          | Inlinable | Non_inlinable { escaping = false } -> false
          | Non_inlinable { escaping = true } -> true)
        use_envs_with_ids
    in
    { handler_env;
      extra_params_and_args;
      is_single_inlinable_use = false;
      escapes
    }
