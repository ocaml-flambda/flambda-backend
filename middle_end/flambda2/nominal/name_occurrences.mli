(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A structure for counting name-like entities that occur free in terms or
    types. *)

(* CR mshinwell: (from gbury on PR#44) Additionally, it might be useful in the
   future to extend the Name_occurrences.t type to distinguish names used one
   semantically from those used once syntaxically, so that variables used once,
   but in the body of a loop can be distinguished from those that are really
   only used once in a program. *)

type t

val empty : t

val is_empty : t -> bool

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val apply_renaming : t -> Renaming.t -> t

include Contains_ids.S with type t := t

(** True if and only if [not (equal (apply_renaming t renaming) t)] *)
val affected_by_renaming : t -> Renaming.t -> bool

val singleton_continuation : Continuation.t -> t

val singleton_continuation_in_trap_action : Continuation.t -> t

val add_continuation : t -> Continuation.t -> has_traps:bool -> t

val count_continuation : t -> Continuation.t -> Num_occurrences.t

val continuation_is_applied_with_traps : t -> Continuation.t -> bool

val count_variable : t -> Variable.t -> Num_occurrences.t

val count_variable_normal_mode : t -> Variable.t -> Num_occurrences.t

val singleton_variable : Variable.t -> Name_mode.t -> t

val add_variable : t -> Variable.t -> Name_mode.t -> t

val add_symbol : t -> Symbol.t -> Name_mode.t -> t

val add_name : t -> Name.t -> Name_mode.t -> t

val add_function_slot_in_projection : t -> Function_slot.t -> Name_mode.t -> t

val add_value_slot_in_projection : t -> Value_slot.t -> Name_mode.t -> t

val add_function_slot_in_declaration : t -> Function_slot.t -> Name_mode.t -> t

val add_value_slot_in_declaration : t -> Value_slot.t -> Name_mode.t -> t

(** Closure_variables and function slots in types count as both projection and
    declaration *)
val add_function_slot_in_types : t -> Function_slot.t -> t

val add_value_slot_in_types : t -> Value_slot.t -> t

val singleton_code_id : Code_id.t -> Name_mode.t -> t

(** If the use of the code ID is in a "newer version of" field, use
    [add_newer_version_of_code_id], not this function -- see below. *)
val add_code_id : t -> Code_id.t -> Name_mode.t -> t

(** [add_newer_version_of_code_id] registers a use of a code ID occurring in a
    "newer version of" field (e.g. in [Flambda_static.Static_part.code]). *)
val add_newer_version_of_code_id : t -> Code_id.t -> Name_mode.t -> t

val singleton_name : Name.t -> Name_mode.t -> t

val singleton_symbol : Symbol.t -> Name_mode.t -> t

val create_variables : Variable.Set.t -> Name_mode.t -> t

val create_names : Name.Set.t -> Name_mode.t -> t

(* val create_value_slots : Value_slot.Set.t -> t *)

(** [diff t1 ~without:t2] removes from [t1] all those names that occur in [t2].

    The number of occurrences of any names in the return value will be exactly
    the same as in [t1].

    Note that a code ID in [t2] will not only be removed from the code ID set in
    [t1] but also the newer-version-of code ID set in [t1]. *)
val diff : t -> without:t -> t

val union : t -> t -> t

val union_list : t list -> t

(** [subset_domain t1 t2] is the usual "set subset" test on the names occurring
    in [t1] and [t2]. The numbers of occurrences and the name_modes of those
    occurrences are ignored. *)
val subset_domain : t -> t -> bool

val inter_domain_is_non_empty : t -> t -> bool

val no_variables : t -> bool

val no_continuations : t -> bool

val continuations : t -> Continuation.Set.t

val continuations_with_traps : t -> Continuation.Set.t

val continuations_including_in_trap_actions : t -> Continuation.Set.t

val function_slots_in_normal_projections : t -> Function_slot.Set.t

val all_function_slots : t -> Function_slot.Set.t

val value_slots_in_normal_projections : t -> Value_slot.Set.t

val all_value_slots : t -> Value_slot.Set.t

val symbols : t -> Symbol.Set.t

val code_ids : t -> Code_id.Set.t

val newer_version_of_code_ids : t -> Code_id.Set.t

val restrict_to_value_slots_and_function_slots : t -> t

val code_ids_and_newer_version_of_code_ids : t -> Code_id.Set.t

val without_code_ids : t -> t

val with_only_variables : t -> t

(** The value returned by this function only records occurrences in two fields:

    - names, as per the input

    - code IDs, containing *both* the code IDs and the "newer version of" code
    IDs from the input.

    The "newer version of" code IDs field in the returned value will always be
    empty. *)
val with_only_names_and_code_ids_promoting_newer_version_of : t -> t

val without_names_or_continuations : t -> t

val mem_var : t -> Variable.t -> bool

val mem_symbol : t -> Symbol.t -> bool

val mem_name : t -> Name.t -> bool

val mem_code_id : t -> Code_id.t -> bool

val value_slot_is_used_or_imported : t -> Value_slot.t -> bool

val remove_var : t -> var:Variable.t -> t

val remove_code_id_or_symbol : t -> code_id_or_symbol:Code_id_or_symbol.t -> t

val remove_continuation : t -> continuation:Continuation.t -> t

val greatest_name_mode_var : t -> Variable.t -> Name_mode.Or_absent.t

val downgrade_occurrences_at_strictly_greater_name_mode : t -> Name_mode.t -> t

val fold_names : t -> init:'a -> f:('a -> Name.t -> 'a) -> 'a

val fold_variables : t -> init:'a -> f:('a -> Variable.t -> 'a) -> 'a

val fold_continuations_including_in_trap_actions :
  t -> init:'a -> f:('a -> Continuation.t -> 'a) -> 'a

val fold_code_ids : t -> init:'a -> f:('a -> Code_id.t -> 'a) -> 'a

val increase_counts : t -> t
