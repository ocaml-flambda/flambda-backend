[@@@ocaml.warning "+a-30-40-41-42"]

(* Combine heap allocations occurring in the same basic block. *)
val run : Cfg_with_layout.t -> Cfg_with_layout.t
