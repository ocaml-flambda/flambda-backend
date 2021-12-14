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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

(** Unlike [Named.t], this type does not include [Static_consts] because such
    constants are propagated separately after simplification. *)
type simplified_named = private
  | Simple of Simple.t
  | Prim of Flambda_primitive.t * Debuginfo.t
  | Set_of_closures of Set_of_closures.t
  | Rec_info of Rec_info_expr.t

val to_named : simplified_named -> Named.t

type t = private
  | Reachable of
      { named : simplified_named;
        cost_metrics : Cost_metrics.t;
        free_names : Name_occurrences.t
      }
  | Reachable_try_reify of
      { named : simplified_named;
        ty : Flambda2_types.t;
        cost_metrics : Cost_metrics.t;
        free_names : Name_occurrences.t
      }
  | Invalid of Invalid_term_semantics.t

(** It is an error to pass [Set_of_closures] or [Static_consts] to this
    function. (Sets of closures are disallowed because computation of their free
    names might be expensive; use [reachable_with_known_free_names] instead.) *)
val reachable : Named.t -> try_reify:Flambda2_types.t option -> t

(** It is an error to pass [Static_consts] to this function. *)
val reachable_with_known_free_names :
  find_code_characteristics:(Code_id.t -> Cost_metrics.code_characteristics) ->
  Named.t ->
  free_names:Name_occurrences.t ->
  try_reify:Flambda2_types.t option ->
  t

val invalid : unit -> t

val is_invalid : t -> bool

val print : Format.formatter -> t -> unit

val cost_metrics : t -> Cost_metrics.t

val update_cost_metrics : Cost_metrics.t -> t -> t
