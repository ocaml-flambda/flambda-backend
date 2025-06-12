[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

val are_equal_regs : Reg.t -> Reg.t -> bool

val go_back_const : int

val prev_at_most : int -> 'a DLL.cell -> 'a DLL.cell

val get_cells :
  Cfg.basic Cfg.instruction DLL.cell ->
  int ->
  Cfg.basic Cfg.instruction DLL.cell list

(** The following functions check for overflow and ranges of immediates w.r.t. the
    operation and optionally rewrite the operation.  *)
val add_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (Operation.integer_operation * int) option

val sub_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (Operation.integer_operation * int) option

val mul_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (Operation.integer_operation * int) option

val bitwise_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (int -> int -> int) ->
  (Operation.integer_operation * int) option

val assert_within_range : Operation.integer_operation -> int -> unit

val is_immediate_for_intop : Operation.integer_operation -> int -> bool
