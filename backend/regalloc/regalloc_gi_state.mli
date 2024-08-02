[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_gi_utils

type t

val make :
  initial_temporaries:int ->
  stack_slots:Regalloc_stack_slots.t ->
  next_instruction_id:Instruction.id ->
  t

val add_assignment : t -> Reg.t -> to_:Hardware_register.location -> unit

val remove_assignment : t -> Reg.t -> unit

val find_assignment : t -> Reg.t -> Hardware_register.location option

val clear_assignments : t -> unit

val add_introduced_temporaries_list : t -> Reg.t list -> unit

val mem_introduced_temporaries : t -> Reg.t -> bool

val iter_introduced_temporaries : t -> f:(Reg.t -> unit) -> unit

val initial_temporary_count : t -> int

val introduced_temporary_count : t -> int

val stack_slots : t -> Regalloc_stack_slots.t

val get_and_incr_instruction_id : t -> Instruction.id

val get_round_num : t -> int

val incr_round_num : t -> unit
