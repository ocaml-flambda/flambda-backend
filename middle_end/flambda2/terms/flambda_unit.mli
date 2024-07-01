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

(** The Flambda representation of a single compilation unit's code. *)

type t

val print : Format.formatter -> t -> unit

val create :
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  toplevel_my_region:Variable.t ->
  toplevel_my_ghost_region:Variable.t ->
  body:Flambda.Expr.t ->
  module_symbol:Symbol.t ->
  used_value_slots:Value_slot.Set.t Or_unknown.t ->
  t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Continuation.t

val toplevel_my_region : t -> Variable.t

val toplevel_my_ghost_region : t -> Variable.t

val module_symbol : t -> Symbol.t

val used_value_slots : t -> Value_slot.Set.t Or_unknown.t

val body : t -> Flambda.Expr.t
