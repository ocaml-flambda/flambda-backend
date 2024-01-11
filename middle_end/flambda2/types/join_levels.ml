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

module K = Flambda_kind
module MTC = More_type_creators
module TE = Typing_env
module TEE = Typing_env_extension
module TEL = Typing_env_level
module TG = Type_grammar
module Join_env = TE.Join_env

let join_types ~env_at_fork envs_with_levels =
  (* Add all the variables defined by the branches as existentials to the
     [env_at_fork].

     Any such variable will be given type [Bottom] on a branch where it was not
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
                      (Bound_name.create_var
                         (Bound_var.create var Name_mode.in_types))
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
  (* Find the actual domain of the join of the levels

     We compute an extension that is the join of the extensions corresponding to
     all the levels. To avoid the difficulty with computing the domain lazily
     during the join, we pre-compute the domain and initialise our accumulator
     with bottom types for all variables involved. *)
  let initial_types =
    List.fold_left
      (fun initial_types (_, _, _, level) ->
        Name.Map.fold
          (fun name ty initial_types ->
            if Name.is_var name
            then Name.Map.add name (MTC.bottom_like ty) initial_types
            else initial_types)
          (TEL.equations level) initial_types)
      Name.Map.empty envs_with_levels
  in
  (* Now fold over the levels doing the actual join operation on equations. *)
  ListLabels.fold_left envs_with_levels ~init:initial_types
    ~f:(fun joined_types (env_at_use, _, _, t) ->
      let left_env =
        (* CR vlaviron: This is very likely quadratic (number of uses times
           number of variables in all uses). However it's hard to know how we
           could do better. *)
        TE.add_env_extension_maybe_bottom base_env
          (TEE.from_map joined_types)
          ~meet_type:Meet_and_join.meet_type
      in
      let join_types name joined_ty use_ty =
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
        if not (Name.is_var name)
        then None
        else
          let joined_ty, use_ty =
            match joined_ty, use_ty with
            | None, Some _use_ty ->
              assert false (* See the computation of [initial_types] *)
            | Some joined_ty, None ->
              (* There is no equation, at all (not even saying "unknown"), on
                 the current level for [name]. There are two possible cases for
                 that:

                 - The environment at use knows of this variable, but this level
                 has no equation on it. In this case, we need to retrieve the
                 type from [env_at_use] and join with it.

                 - The variable doesn't exist in this environment. This happens
                 if the variable is defined in one of the other branches, and
                 will be quantified existentially in the result. In this case,
                 it's safe to join with Bottom. *)
              let is_defined_at_use = TE.mem env_at_use name in
              if is_defined_at_use
              then
                let use_ty =
                  let expected_kind = Some (TG.kind joined_ty) in
                  TE.find env_at_use name expected_kind
                in
                joined_ty, use_ty
              else joined_ty, MTC.bottom_like joined_ty
            | Some joined_ty, Some use_ty -> joined_ty, use_ty
            | None, None -> assert false
          in
          let join_env =
            Join_env.create base_env ~left_env ~right_env:env_at_use
          in
          match
            Meet_and_join.join ~bound_name:name join_env joined_ty use_ty
          with
          | Known joined_ty -> Some joined_ty
          | Unknown -> None
      in
      Name.Map.merge join_types joined_types (TEL.equations t))

let construct_joined_level envs_with_levels ~env_at_fork ~allowed ~joined_types
    ~params =
  let allowed_and_new =
    (* Parameters are already in the resulting environment *)
    List.fold_left
      (fun allowed_and_new param ->
        Name_occurrences.remove_var allowed_and_new
          ~var:(Bound_parameter.var param))
      allowed params
  in
  let variable_is_in_new_level var =
    Name_occurrences.mem_var allowed_and_new var
  in
  let defined_vars, binding_times =
    List.fold_left
      (fun (defined_vars, binding_times) (_env_at_use, _id, _use_kind, t) ->
        let defined_vars_this_level =
          Variable.Map.filter
            (fun var _ -> variable_is_in_new_level var)
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
              let vars = Variable.Set.filter variable_is_in_new_level vars in
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
  let joined_types = join_types ~env_at_fork envs_with_levels in
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
  let free_names_transitive typ =
    (* We need to compute the free names of joined_types, but we can't use a
       typing environment. *)
    let rec free_names_transitive0 typ ~result =
      let free_names = TG.free_names typ in
      let to_traverse = Name_occurrences.diff free_names ~without:result in
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
    ~params

let n_way_join ~env_at_fork envs_with_levels ~params
    ~extra_lifted_consts_in_use_envs ~extra_allowed_names =
  match envs_with_levels with
  | [] -> TEL.empty
  | envs_with_levels ->
    join ~env_at_fork envs_with_levels ~params ~extra_lifted_consts_in_use_envs
      ~extra_allowed_names

let cut_and_n_way_join definition_typing_env ts_and_use_ids ~params ~cut_after
    ~extra_lifted_consts_in_use_envs ~extra_allowed_names =
  let after_cuts =
    List.map
      (fun (t, use_id, use_kind) ->
        let level = TE.cut t ~cut_after in
        t, use_id, use_kind, level)
      ts_and_use_ids
  in
  let params = Bound_parameters.to_list params in
  let level =
    n_way_join ~env_at_fork:definition_typing_env after_cuts ~params
      ~extra_lifted_consts_in_use_envs ~extra_allowed_names
  in
  TE.add_env_extension_from_level definition_typing_env level
    ~meet_type:Meet_and_join.meet_type
