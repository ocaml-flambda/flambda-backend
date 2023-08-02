(** Representation of the DWARF .debug_line section. *)

open Asm_targets

type t

val create : code_begin:Asm_symbol.t -> t

val add_source_file : t -> file_name:string -> file_num:int -> unit

val add_line_number_matrix_row :
  t ->
  instr_address:int ->
  file_num:int ->
  line:int ->
  col:int ->
  discriminator:int option ->
  unit

val checkpoint : t -> unit

val rollback : t -> unit

include Dwarf_emittable.S with type t := t
