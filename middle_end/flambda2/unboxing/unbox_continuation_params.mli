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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

module Decisions : sig
  type t
  val print : Format.formatter -> t -> unit
end

val make_decisions
   : continuation_is_recursive:bool
  -> arg_types_by_use_id:
       Continuation_env_and_param_types.arg_at_use
         Apply_cont_rewrite_id.Map.t list
  -> DE.t
  -> KP.t list
  -> T.t list
  -> DE.t * Decisions.t

val compute_extra_params_and_args
   : Decisions.t
  -> arg_types_by_use_id:
       Continuation_env_and_param_types.arg_at_use
         Apply_cont_rewrite_id.Map.t list
  -> EPA.t
  -> EPA.t
