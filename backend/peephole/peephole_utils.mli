[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

val are_equal_regs : Reg.t -> Reg.t -> bool

val go_back_const : int

val prev_at_most : int -> 'a DLL.cell -> 'a DLL.cell

val get_cells :
  Cfg.basic Cfg.instruction DLL.cell ->
  int ->
  Cfg.basic Cfg.instruction DLL.cell list

val is_bitwise_op : Mach.integer_operation -> bool

val bitwise_shift_assert : int -> int -> unit

val amd64_imm32_within_bounds : int -> int -> (int -> int -> int) -> bool
