(** Conversion from [Cfg] to [Linear] code. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val run : Cfg_with_layout.t -> Linear.instruction

val print_assembly : Cfg.basic_block list -> unit
