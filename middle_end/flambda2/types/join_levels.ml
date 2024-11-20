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

module TE = Typing_env
module TEL = Typing_env_level

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

let cut_and_n_way_join definition_typing_env ts_and_use_ids ~params ~cut_after
    ~extra_lifted_consts_in_use_envs ~extra_allowed_names:_ =
  let params = Bound_parameters.to_list params in
  check_join_inputs ~env_at_fork:definition_typing_env ts_and_use_ids ~params
    ~extra_lifted_consts_in_use_envs;
  let ts = List.rev_map (fun (t, _, _) -> t) ts_and_use_ids in
  Join_env.cut_and_n_way_join ~meet_type:Meet_and_n_way_join.meet_type
    ~n_way_join_type:Meet_and_n_way_join.n_way_join definition_typing_env
    ~cut_after ts

let ignore_names =
  String.split_on_char ','
    (Option.value ~default:""
       (Sys.getenv_opt "FLAMBDA2_JOIN_DEBUG_IGNORE_NAMES"))

let cut_and_n_way_join_checked definition_typing_env ts_and_use_ids ~params
    ~cut_after ~extra_lifted_consts_in_use_envs ~extra_allowed_names =
  let scope = TE.current_scope definition_typing_env in
  let typing_env = TE.increment_scope definition_typing_env in
  let old_joined_env =
    Join_levels_old.cut_and_n_way_join typing_env ts_and_use_ids ~params
      ~cut_after ~extra_lifted_consts_in_use_envs ~extra_allowed_names
  in
  let old_joined_level = TE.cut old_joined_env ~cut_after:scope in
  let new_joined_env =
    cut_and_n_way_join typing_env ts_and_use_ids ~params ~cut_after
      ~extra_lifted_consts_in_use_envs ~extra_allowed_names
  in
  let new_joined_level = TE.cut new_joined_env ~cut_after:scope in
  (let distinct_names =
     Equal_types_for_debug.names_with_non_equal_types_level_ignoring_name_mode
       ~meet_type:Meet_and_join.meet_type typing_env old_joined_level
       new_joined_level
   in
   let distinct_names =
     Name.Set.filter
       (fun name ->
         match Name.must_be_var_opt name with
         | Some var ->
           let raw_name = Variable.raw_name var in
           not (List.exists (String.equal raw_name) ignore_names)
         | None -> true)
       distinct_names
   in
   if not (Name.Set.is_empty distinct_names)
   then (
     Format.eprintf "@[<v 1>%s Distinct joins %s@ " (String.make 22 '=')
       (String.make 22 '=');
     if Flambda_features.debug_flambda2 ()
     then
       List.iteri
         (fun i (t, _, _) ->
           let level = TE.cut t ~cut_after in
           Format.eprintf "@[<v 1>-- Level %d --@ %a@]@ " i TEL.print level)
         ts_and_use_ids;
     Format.eprintf "@[<v 1>-- Old join --@ %a@]@ " TEL.print old_joined_level;
     Format.eprintf "@[<v 1>-- New join --@ %a@]@ " TEL.print new_joined_level;
     Format.eprintf "@[Names with distinct types:@ %a@]" Name.Set.print
       distinct_names;
     Format.eprintf "@]@\n%s@." (String.make 60 '=')));
  TE.add_env_extension_from_level definition_typing_env new_joined_level
    ~meet_type:Meet_and_join.meet_type

let cut_and_n_way_join =
  match Sys.getenv "FLAMBDA2_JOIN_ALGORITHM" with
  | "old" -> Join_levels_old.cut_and_n_way_join
  | "checked" -> cut_and_n_way_join_checked
  | _ | (exception Not_found) -> cut_and_n_way_join
