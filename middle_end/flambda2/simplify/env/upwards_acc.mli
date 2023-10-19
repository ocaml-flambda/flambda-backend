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

type t

(** Print a upwards accumulator to a formatter. *)
val print : Format.formatter -> t -> unit

val create :
  flow_result:Flow_types.Flow_result.t ->
  compute_slot_offsets:bool ->
  Upwards_env.t ->
  Downwards_acc.t ->
  t

val creation_dacc : t -> Downwards_acc.t

(** Extract the environment component of the given upwards accumulator. *)
val uenv : t -> Upwards_env.t

val cost_metrics : t -> Cost_metrics.t

val code_age_relation : t -> Code_age_relation.t

(** Return the lifted constants that still need to be placed (i.e. have
    [Let]-expressions made for them) on the upwards traversal. *)
val lifted_constants : t -> Lifted_constant_state.t

val get_and_clear_lifted_constants : t -> t * Lifted_constant_state.t

(** Replace the accumulator of lifted constants. *)
val with_lifted_constants : t -> Lifted_constant_state.t -> t

val no_lifted_constants : t -> bool

val add_lifted_constant : t -> Lifted_constant.t -> t

(** Map the environment component of the given upwards accumulator. *)
val map_uenv : t -> f:(Upwards_env.t -> Upwards_env.t) -> t

(** Replace the environment component of the given upwards accumulator. *)
val with_uenv : t -> Upwards_env.t -> t

val remember_code_for_cmx : t -> Code.t Code_id.Map.t -> t

val all_code : t -> Exported_code.t

val shareable_constants : t -> Symbol.t Static_const.Map.t

val name_occurrences : t -> Name_occurrences.t

val with_name_occurrences : t -> name_occurrences:Name_occurrences.t -> t

val clear_name_occurrences : t -> t

val add_free_names : t -> Name_occurrences.t -> t

(** Note that this only includes uses of value slots in projections. *)
val used_value_slots : t -> Name_occurrences.t

val remove_all_occurrences_of_free_names : t -> Name_occurrences.t -> t

val clear_cost_metrics : t -> t

val with_cost_metrics : Cost_metrics.t -> t -> t

val add_cost_metrics : Cost_metrics.t -> t -> t

(* CR lmaurer: This is tragic. We can be rid of it once we have PDCE, if I
   understand correctly. *)

(** This function exists as an optimisation to reduce allocation. *)
val add_cost_metrics_and_with_name_occurrences :
  t -> Cost_metrics.t -> Name_occurrences.t -> t

val notify_added : code_size:Code_size.t -> t -> t

val notify_removed : operation:Removed_operations.t -> t -> t

val generate_phantom_lets : t -> bool

val are_rebuilding_terms : t -> Are_rebuilding_terms.t

val is_demoted_exn_handler : t -> Continuation.t -> bool

val slot_offsets : t -> Slot_offsets.t Or_unknown.t

val with_slot_offsets : t -> Slot_offsets.t Or_unknown.t -> t

(* Functions to extract specific fields of [flow_result]. *)

val required_names : t -> Name.Set.t

val reachable_code_ids : t -> Flow_types.Reachable_code_ids.t Or_unknown.t

val continuation_param_aliases : t -> Flow_types.Alias_result.t

val mutable_unboxing_result : t -> Flow_types.Mutable_unboxing_result.t

val set_resimplify : t -> t

val resimplify : t -> bool
