[@@@ocaml.warning "+a-4-30-40-41-42"]

open Regalloc_utils
open Regalloc_irc_utils

type t

val make :
  initial:Reg.t list ->
  stack_slots:Regalloc_stack_slots.t ->
  next_instruction_id:Instruction.id ->
  unit ->
  t

val add_initial_one : t -> Reg.t -> unit

val add_initial_list : t -> Reg.t list -> unit

val reset : t -> new_temporaries:Reg.t list -> unit

val is_precolored : t -> Reg.t -> bool

val is_precolored_or_colored : t -> Reg.t -> bool

val iter_and_clear_initial : t -> f:(Reg.t -> unit) -> unit

val is_empty_simplify_work_list : t -> bool

val add_simplify_work_list : t -> Reg.t -> unit

val choose_and_remove_simplify_work_list : t -> Reg.t

val is_empty_freeze_work_list : t -> bool

val mem_freeze_work_list : t -> Reg.t -> bool

val add_freeze_work_list : t -> Reg.t -> unit

val remove_freeze_work_list : t -> Reg.t -> unit

val choose_and_remove_freeze_work_list : t -> Reg.t

val is_empty_spill_work_list : t -> bool

val mem_spill_work_list : t -> Reg.t -> bool

val add_spill_work_list : t -> Reg.t -> unit

val remove_spill_work_list : t -> Reg.t -> unit

val fold_spill_work_list : t -> f:('a -> Reg.t -> 'a) -> init:'a -> 'a

val spill_work_list : t -> Reg.Set.t

val is_empty_spilled_nodes : t -> bool

val add_spilled_nodes : t -> Reg.t -> unit

val spilled_nodes : t -> Reg.t list

val clear_spilled_nodes : t -> unit

val add_coalesced_nodes : t -> Reg.t -> unit

val iter_coalesced_nodes : t -> f:(Reg.t -> unit) -> unit

val add_colored_nodes : t -> Reg.t -> unit

val is_empty_select_stack : t -> bool

val push_select_stack : t -> Reg.t -> unit

val pop_select_stack : t -> Reg.t

val iter_and_clear_select_stack : t -> f:(Reg.t -> unit) -> unit

val add_coalesced_moves : t -> Instruction.t -> unit

val add_constrained_moves : t -> Instruction.t -> unit

val add_frozen_moves : t -> Instruction.t -> unit

val is_empty_work_list_moves : t -> bool

val add_work_list_moves : t -> Instruction.t -> unit

val choose_and_remove_work_list_moves : t -> Instruction.t

val mem_active_moves : t -> Instruction.t -> bool

val add_active_moves : t -> Instruction.t -> unit

val remove_active_moves : t -> Instruction.t -> unit

val mem_adj_set : t -> Reg.t -> Reg.t -> bool

val adj_list : t -> Reg.t -> Reg.t list

val interferes_with_adj : t -> Reg.t -> Reg.t -> bool

val adj_set : t -> RegisterStamp.PairSet.t

val add_edge : t -> Reg.t -> Reg.t -> unit

val iter_adjacent : t -> Reg.t -> f:(Reg.t -> unit) -> unit

val for_all_adjacent : t -> Reg.t -> f:(Reg.t -> bool) -> bool

val is_empty_node_moves : t -> Reg.t -> bool

val iter_node_moves : t -> Reg.t -> f:(Instruction.t -> unit) -> unit

val is_move_related : t -> Reg.t -> bool

val enable_moves_one : t -> Reg.t -> unit

val decr_degree : t -> Reg.t -> unit

val find_move_list : t -> Reg.t -> Instruction.Set.t

val add_move_list : t -> Reg.t -> Instruction.t -> unit

val union_move_list : t -> Reg.t -> Instruction.Set.t -> unit

val find_alias : t -> Reg.t -> Reg.t

val add_alias : t -> Reg.t -> Reg.t -> unit

val stack_slots : t -> Regalloc_stack_slots.t

val get_and_incr_instruction_id : t -> Instruction.id

val add_inst_temporaries_list : t -> Reg.t list -> unit

val add_block_temporaries_list : t -> Reg.t list -> unit

val mem_inst_temporaries : t -> Reg.t -> bool

val mem_all_introduced_temporaries : t -> Reg.t -> bool

val diff_all_introduced_temporaries : t -> Reg.Set.t -> Reg.Set.t

val invariant : t -> unit
