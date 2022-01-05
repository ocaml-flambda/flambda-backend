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

(** Compute, for each parameter of a continuation, the join of all corresponding
    argument types across the recorded uses; together with the environment to be
    used for simplifying the continuation itself. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val compute_handler_env :
  ?unknown_if_defined_at_or_later_than:Scope.t ->
  Continuation_uses.t ->
  env_at_fork_plus_params:Downwards_env.t ->
  consts_lifted_during_body:Lifted_constant_state.t ->
  params:Bound_parameter.t list ->
  code_age_relation_after_body:Code_age_relation.t ->
  Continuation_env_and_param_types.t
