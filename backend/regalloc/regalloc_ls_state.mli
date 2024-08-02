[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_ls_utils

type t

val for_fatal : t -> Interval.t list * ClassIntervals.t array

val make :
  stack_slots:Regalloc_stack_slots.t -> next_instruction_id:Instruction.id -> t

val update_intervals : t -> Interval.t Reg.Tbl.t -> unit

val iter_intervals : t -> f:(Interval.t -> unit) -> unit

val fold_intervals : t -> f:('a -> Interval.t -> 'a) -> init:'a -> 'a

val release_expired_intervals : t -> pos:int -> unit

val active : t -> reg_class:int -> ClassIntervals.t

val active_classes : t -> ClassIntervals.t array

val stack_slots : t -> Regalloc_stack_slots.t

val get_and_incr_instruction_id : t -> Instruction.id

val get_round_num : t -> int

val incr_round_num : t -> unit

val invariant_intervals : t -> Cfg_with_infos.t -> unit

val invariant_active : t -> unit
