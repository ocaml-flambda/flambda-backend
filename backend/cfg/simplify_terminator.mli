(** Merge successors that go to the same label and simplify their conditions.
    Modifies the terminators in place. Does not merge blocks. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val block : Cfg.t -> Cfg.basic_block -> unit

val run : Cfg.t -> unit
