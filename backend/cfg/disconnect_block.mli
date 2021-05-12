[@@@ocaml.warning "+a-30-40-41-42"]

(** Remove a block from a CFG. The block must either have no predecessors or
    at most one successor. *)
val disconnect : Cfg_with_layout.t -> Label.t -> unit
