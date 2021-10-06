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

(** Upwards environments used during simplification. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val empty : t

val print : Format.formatter -> t -> unit

val add_non_inlinable_continuation :
  t ->
  Continuation.t ->
  Scope.t ->
  params:Kinded_parameter.t list ->
  handler:Rebuilt_expr.t Or_unknown.t ->
  t

val add_unreachable_continuation :
  t -> Continuation.t -> Scope.t -> Flambda_arity.With_subkinds.t -> t

val add_continuation_alias :
  t ->
  Continuation.t ->
  Flambda_arity.With_subkinds.t ->
  alias_for:Continuation.t ->
  t

val add_linearly_used_inlinable_continuation :
  t ->
  Continuation.t ->
  Scope.t ->
  params:Kinded_parameter.t list ->
  handler:Rebuilt_expr.t ->
  free_names_of_handler:Name_occurrences.t ->
  cost_metrics_of_handler:Cost_metrics.t ->
  t

val add_return_continuation :
  t -> Continuation.t -> Scope.t -> Flambda_arity.With_subkinds.t -> t

val add_exn_continuation : t -> Exn_continuation.t -> Scope.t -> t

val find_continuation : t -> Continuation.t -> Continuation_in_env.t

val mem_continuation : t -> Continuation.t -> bool

val resolve_continuation_aliases : t -> Continuation.t -> Continuation.t

val resolve_exn_continuation_aliases :
  t -> Exn_continuation.t -> Exn_continuation.t

val check_continuation_is_bound : t -> Continuation.t -> unit

val check_exn_continuation_is_bound : t -> Exn_continuation.t -> unit

val add_apply_cont_rewrite : t -> Continuation.t -> Apply_cont_rewrite.t -> t

val find_apply_cont_rewrite : t -> Continuation.t -> Apply_cont_rewrite.t option

val delete_apply_cont_rewrite : t -> Continuation.t -> t

val will_inline_continuation : t -> Continuation.t -> bool
