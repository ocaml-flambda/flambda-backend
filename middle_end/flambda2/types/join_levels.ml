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

module K = Flambda_kind
module MTC = More_type_creators
module TE = Typing_env
module TEE = Typing_env_extension
module TEL = Typing_env_level
module TG = Type_grammar
module Join_env = TE.Join_env

let join_types ~params ~env_at_fork envs_with_levels =
  (* Add all the variables defined by the branches as existentials to the
     [env_at_fork].

     Any such variable will be given type [Unknown] on a branch where it was not
     originally present.

     Iterating on [level.binding_times] instead of [level.defined_vars] ensures
     consistency of binding time order in the branches and the result. In
     addition, this also aggregates the code age relations of the branches. *)
  let base_env =
    List.fold_left
      (fun base_env (env_at_use, _, _, level) ->
        let base_env =
          Binding_time.Map.fold
            (fun _ vars base_env ->
              Variable.Set.fold
                (fun var base_env ->
                  if TE.mem base_env (Name.var var)
                  then base_env
                  else
                    let kind = TEL.find_kind level var in
                    TE.add_definition base_env
                      (Bound_name.var (Bound_var.create var Name_mode.in_types))
                      kind)
                vars base_env)
            (TEL.variables_by_binding_time level)
            base_env
        in
        let code_age_relation =
          Code_age_relation.union
            (TE.code_age_relation base_env)
            (TE.code_age_relation env_at_use)
        in
        TE.with_code_age_relation base_env code_age_relation)
      env_at_fork envs_with_levels
  in
  (* Special handling for parameters: they're defined in [env_at_fork], but
     their type (Bottom) is only a placeholder until we compute the actual join.

     So we start the join with equations binding the parameters to Bottom, to
     make sure we end up with the right type in the end. *)
  let initial_joined_types =
    let bottom_ty param =
      MTC.bottom (K.With_subkind.kind (Bound_parameter.kind param))
    in
    List.fold_left
      (fun initial_types param ->
        Name.Map.add
          (Bound_parameter.name param)
          (bottom_ty param) initial_types)
      Name.Map.empty params
  in
  (* Now fold over the levels doing the actual join operation on equations. *)
  ListLabels.fold_left envs_with_levels
    ~init:(initial_joined_types, Variable.Set.empty)
    ~f:(fun (joined_types, defined_variables) (env_at_use, _, _, t) ->
      let left_env =
        (* CR vlaviron: This is very likely quadratic (number of uses times
           number of variables in all uses). However it's hard to know how we
           could do better. *)
        TE.add_env_extension base_env
          (TEE.from_map joined_types)
          ~meet_type:Meet_and_join.meet
      in
      let join_types name joined_ty use_ty =
        (* CR mshinwell for vlaviron: Looks like [TE.mem] needs fixing with
           respect to names from other units with their .cmx missing (c.f.
           testsuite/tests/lib-dynlink-native/). *)
        let same_unit =
          Compilation_unit.equal
            (Name.compilation_unit name)
            (Compilation_unit.get_current_exn ())
        in
        if same_unit && not (TE.mem base_env name)
        then
          Misc.fatal_errorf "Name %a not defined in [base_env]:@ %a" Name.print
            name TE.print base_env;
        (* If [name] is that of a lifted constant symbol generated during one of
           the levels, then ignore it. [Simplify_expr] will already have made
           its type suitable for [base_env] and inserted it into that
           environment.

           If [name] is a symbol that is not a lifted constant, then it was
           defined before the fork and already has an equation in base_env.
           While it is possible that its type could be refined by all of the
           branches, it is unlikely. *)
        match Name.must_be_var_opt name with
        | None -> (* Symbol *) None
        | Some var -> (
          let joined_ty =
            match joined_ty, use_ty with
            | None, Some use_ty ->
              (* In this case, we haven't yet got a joined type for [name]. *)
              let left_ty =
                (* If this is the first occurrence of [name] in the join, and
                   [name] was not part of the base environment, we just need to
                   make the type suitable for the joined environment, so we use
                   [Bottom] to avoid losing precision... *)
                let is_first_definition =
                  let is_previously_defined =
                    Variable.Set.mem var defined_variables
                    || TE.mem env_at_fork name
                  in
                  not is_previously_defined
                in
                if is_first_definition
                then
                  MTC.bottom_like use_ty
                  (* ...but if this is not the case, then we need to get the
                     best type we can for [name] which will be valid on all of
                     the previous paths. This is either the type of [name] in
                     the original [env_at_fork] (passed to [join], below), or if
                     [name] was undefined there, [Unknown].

                     Since the current version of [base_env] has definitions for
                     all the variables present in the branches, we can actually
                     always just look the type up there, without needing to case
                     split. *)
                else
                  let expected_kind = Some (TG.kind use_ty) in
                  TE.find base_env name expected_kind
              in
              (* Recall: the order of environments matters for [join]. *)
              let join_env =
                Join_env.create base_env ~left_env ~right_env:env_at_use
              in
              Meet_and_join.join ~bound_name:name join_env left_ty use_ty
            | Some joined_ty, None ->
              (* There is no equation, at all (not even saying "unknown"), on
                 the current level for [name].

                 However, we know we've already seen [name] earlier. So like in
                 the case above, we have three cases:

                 - [name] is defined in [env_at_fork]. In that case, the type
                 for [name] at the current use is the one from [env_at_fork],
                 and we need to join with it.

                 - [name] is not defined in [env_at_fork], but is defined in
                 [env_at_use]. In this case (which we can check by looking at
                 [t.defined_vars]), the type for [name] at the current use is
                 [Unknown], and we don't have any guarantee that [joined_ty] is
                 already [Unknown], so we need to do a join. However, since the
                 join of anything with [Unknown] is [Unknown], we can return it
                 directly.

                 - [name] is defined neither in [env_at_fork] nor in
                 [env_at_use]. In this case, the type for [name] is considered
                 [Bottom] in this branch, so we can return [joined_ty]
                 directly. *)
              let is_defined_at_fork = TE.mem env_at_fork name in
              let is_defined_at_use = TEL.variable_is_defined t var in
              if is_defined_at_fork
              then
                let use_ty =
                  let expected_kind = Some (TG.kind joined_ty) in
                  TE.find env_at_fork name expected_kind
                in
                let join_env =
                  Join_env.create base_env ~left_env ~right_env:env_at_fork
                  (* env_at_use would be correct too *)
                in
                Meet_and_join.join ~bound_name:name join_env joined_ty use_ty
              else if is_defined_at_use
              then Or_unknown.Unknown
              else Or_unknown.Known joined_ty
            | Some joined_ty, Some use_ty ->
              (* This is the straightforward case, where we have already started
                 computing a joined type for [name], and there is an equation
                 for [name] on the current level. *)
              let join_env =
                Join_env.create base_env ~left_env ~right_env:env_at_use
              in
              Meet_and_join.join ~bound_name:name join_env joined_ty use_ty
            | None, None -> assert false
          in
          match joined_ty with
          | Known joined_ty -> Some joined_ty
          | Unknown -> None)
      in
      let joined_types =
        Name.Map.merge join_types joined_types (TEL.equations t)
      in
      let defined_variables =
        Variable.Set.union defined_variables (TEL.defined_variables t)
      in
      joined_types, defined_variables)
  |> fun (joined_types, _) -> joined_types

let construct_joined_level envs_with_levels ~env_at_fork ~allowed ~joined_types
    =
  let defined_vars, binding_times =
    List.fold_left
      (fun (defined_vars, binding_times) (_env_at_use, _id, _use_kind, t) ->
        let defined_vars_this_level =
          Variable.Map.filter
            (fun var _ -> Name_occurrences.mem_var allowed var)
            (TEL.defined_variables_with_kinds t)
        in
        let defined_vars =
          Variable.Map.union
            (fun var kind1 kind2 ->
              if K.equal kind1 kind2
              then Some kind1
              else
                Misc.fatal_errorf
                  "Cannot join levels that disagree on the kind of \
                   [defined_vars] (%a and %a for %a)"
                  K.print kind1 K.print kind2 Variable.print var)
            defined_vars defined_vars_this_level
        in
        let binding_times_this_level =
          Binding_time.Map.filter_map
            (fun _ vars ->
              let vars =
                Variable.Set.filter
                  (fun var -> Name_occurrences.mem_var allowed var)
                  vars
              in
              if Variable.Set.is_empty vars then None else Some vars)
            (TEL.variables_by_binding_time t)
        in
        let binding_times =
          Binding_time.Map.union
            (fun _bt vars1 vars2 -> Some (Variable.Set.union vars1 vars2))
            binding_times binding_times_this_level
        in
        defined_vars, binding_times)
      (Variable.Map.empty, Binding_time.Map.empty)
      envs_with_levels
  in
  let equations =
    Name.Map.filter
      (fun name _ty -> Name_occurrences.mem_name allowed name)
      joined_types
  in
  let symbol_projections =
    List.fold_left
      (fun symbol_projections (_env_at_use, _id, _use_kind, t) ->
        let projs_this_level =
          Variable.Map.filter
            (fun var _ ->
              let name = Name.var var in
              TE.mem ~min_name_mode:Name_mode.normal env_at_fork name
              || Name_occurrences.mem_name allowed name)
            (TEL.symbol_projections t)
        in
        Variable.Map.union
          (fun _var proj1 proj2 ->
            if Symbol_projection.equal proj1 proj2 then Some proj1 else None)
          symbol_projections projs_this_level)
      Variable.Map.empty envs_with_levels
  in
  TEL.create ~defined_vars ~binding_times ~equations ~symbol_projections

let check_join_inputs ~env_at_fork _envs_with_levels ~params
    ~extra_lifted_consts_in_use_envs =
  (* It might seem as if every name defined in [env_at_fork], with the exception
     of the lifted constant symbols, should occur in every use environment.
     However this is not the case: the introduction of the lifted constants into
     [env_at_fork] in [Simplify_expr] may have produced [In_types] variables
     (from [make_suitable_for_environment]) that will not be present in any use
     environment. *)
  List.iter
    (fun param ->
      if not (TE.mem env_at_fork (Bound_parameter.name param))
      then
        Misc.fatal_errorf "Parameter %a not defined in [env_at_fork] at join"
          Bound_parameter.print param)
    params;
  Symbol.Set.iter
    (fun symbol ->
      if not (TE.mem env_at_fork (Name.symbol symbol))
      then
        Misc.fatal_errorf
          "Symbol %a, which is a new lifted constant that arose during the \
           simplification of the continuation's body, is not defined in the \
           [env_at_fork] when calling [join]"
          Symbol.print symbol)
    extra_lifted_consts_in_use_envs

let join ~env_at_fork envs_with_levels ~params ~extra_lifted_consts_in_use_envs
    ~extra_allowed_names:allowed =
  check_join_inputs ~env_at_fork envs_with_levels ~params
    ~extra_lifted_consts_in_use_envs;
  (* Calculate the joined types of all the names involved. *)
  let joined_types = join_types ~params ~env_at_fork envs_with_levels in
  (* Next calculate which equations (describing joined types) to propagate to
     the join point. (Recall that the environment at the fork point includes the
     parameters of the continuation being called at the join. We wish to ensure
     that information in the types of these parameters is not lost.)

     - Equations on names defined in the environment at the fork point are
     always propagated.

     - Definitions of, and equations on, names that occur free on the right-hand
     sides of the propagated equations are also themselves propagated. The
     definition of any such propagated name (i.e. one that does not occur in the
     environment at the fork point) will be made existential. *)
  (* CR vlaviron: We need to compute the free names of joined_types, we can't
     use a typing environment *)
  let free_names_transitive typ =
    let rec free_names_transitive0 typ ~result =
      let free_names = TG.free_names typ in
      let to_traverse = Name_occurrences.diff free_names result in
      Name_occurrences.fold_names to_traverse ~init:result
        ~f:(fun result name ->
          let result =
            Name_occurrences.add_name result name Name_mode.in_types
          in
          match Name.Map.find name joined_types with
          | exception Not_found -> result
          | typ -> free_names_transitive0 typ ~result)
    in
    free_names_transitive0 typ ~result:Name_occurrences.empty
  in
  let allowed =
    Name.Map.fold
      (fun name ty allowed ->
        if TE.mem env_at_fork name || Name.is_symbol name
        then
          Name_occurrences.add_name
            (Name_occurrences.union allowed (free_names_transitive ty))
            name Name_mode.in_types
        else allowed)
      joined_types allowed
  in
  let allowed =
    Symbol.Set.fold
      (fun symbol allowed ->
        Name_occurrences.add_symbol allowed symbol Name_mode.in_types)
      extra_lifted_consts_in_use_envs allowed
  in
  (* Having calculated which equations to propagate, the resulting level can now
     be constructed. *)
  construct_joined_level envs_with_levels ~env_at_fork ~allowed ~joined_types

let n_way_join ~env_at_fork envs_with_levels ~params
    ~extra_lifted_consts_in_use_envs ~extra_allowed_names =
  match envs_with_levels with
  | [] -> TEL.empty
  | envs_with_levels ->
    join ~env_at_fork envs_with_levels ~params ~extra_lifted_consts_in_use_envs
      ~extra_allowed_names

let cut_and_n_way_join definition_typing_env ts_and_use_ids ~params
    ~unknown_if_defined_at_or_later_than ~extra_lifted_consts_in_use_envs
    ~extra_allowed_names =
  (* CR mshinwell: Can't [unknown_if_defined_at_or_later_than] just be computed
     by this function? *)
  let after_cuts =
    List.map
      (fun (t, use_id, use_kind) ->
        let level = TE.cut t ~unknown_if_defined_at_or_later_than in
        t, use_id, use_kind, level)
      ts_and_use_ids
  in
  let level =
    n_way_join ~env_at_fork:definition_typing_env after_cuts ~params
      ~extra_lifted_consts_in_use_envs ~extra_allowed_names
  in
  TE.add_env_extension_from_level definition_typing_env level
    ~meet_type:Meet_and_join.meet
