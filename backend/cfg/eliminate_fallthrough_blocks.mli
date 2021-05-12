(** In-place disconnection of fallthrough blocks in a CFG. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val run : Cfg_with_layout.t -> unit
