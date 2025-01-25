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

type result = private
  { handler_env : Downwards_env.t;
    extra_params_and_args : Continuation_extra_params_and_args.t;
    is_single_inlinable_use : bool;
    escapes : bool
  }

val compute_handler_env :
  ?cut_after:Scope.t ->
  One_continuation_use.t list ->
  is_recursive:bool ->
  env_at_fork:Downwards_env.t ->
  consts_lifted_during_body:Lifted_constant_state.t ->
  params:Bound_parameters.t ->
  lifted_cont_extra_params_and_args:Continuation_extra_params_and_args.t ->
  result
