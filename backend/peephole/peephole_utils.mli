[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

module IntCsv : Csv_data.T

val csv_singleton : IntCsv.t option ref

val get_csv : unit -> IntCsv.t

val update_csv : string -> unit

val are_equal_regs : Reg.t -> Reg.t -> bool

val prev_at_most : int -> 'a DLL.cell -> 'a DLL.cell

val get_cells :
  Cfg.basic Cfg.instruction DLL.cell ->
  int ->
  Cfg.basic Cfg.instruction DLL.cell list option

val bitwise_shift_assert : int -> int -> unit

val amd64_imm32_within_bounds : int -> int -> (int -> int -> int) -> bool

val amd64_imm32_within_bounds_assert_if_false :
  int -> int -> (int -> int -> int) -> bool
