(** Insertion of extra debugging information used to correlate between
    machine instructions, [Linear] and [Cfg] code. *)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Adds the id of each cfg instruction into the debug info of each
    instruction. The id is encoded as the "line number" in the [file]. *)
val add : Cfg.t -> file:string -> unit
