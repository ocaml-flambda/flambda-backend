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

(** Handling of the extra params and args required for the unboxing of a
    continuation's parameter(s). *)

type unboxed_arg =
  | Poison (* used for recursive calls *)
  | Available of Simple.t
  | Generated of Variable.t
  | Added_by_wrapper_at_rewrite_use of { nth_arg : int }

val compute_extra_args_for_one_decision_and_use :
  pass:Unboxing_types.pass ->
  Apply_cont_rewrite_id.t ->
  typing_env_at_use:Flambda2_types.Typing_env.t ->
  unboxed_arg ->
  Unboxing_types.decision ->
  Unboxing_types.decision

val add_extra_params_and_args :
  Continuation_extra_params_and_args.t ->
  Unboxing_types.decision ->
  Continuation_extra_params_and_args.t
