[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

module IntCsv : Csv_data.T

val set_csv : unit -> unit

val get_csv : unit -> IntCsv.t

val update_csv : string -> unit

val are_equal_regs : Reg.t -> Reg.t -> bool

val go_back_const : int

val prev_at_most : int -> 'a DLL.cell -> 'a DLL.cell

val get_cells : Cfg.basic Cfg.instruction DLL.cell ->
  int -> Cfg.basic Cfg.instruction DLL.cell list

type rule = Cfg.basic Cfg.instruction DLL.cell -> Cfg.basic Cfg.instruction DLL.cell option
