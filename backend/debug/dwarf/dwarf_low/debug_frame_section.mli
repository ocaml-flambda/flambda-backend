(** Representation of the DWARF .debug_frame section. *)

open Asm_targets

type t

val create : code_begin:Asm_symbol.t -> t

val process_cfi_startproc : t -> address:int -> unit

val process_cfi_adjust_cfa_offset : t -> address:int -> offset:int -> unit

val process_cfi_endproc : t -> address:int -> unit

val checkpoint : t -> unit

val rollback : t -> unit

include Dwarf_emittable.S with type t := t
