(* Finds independent scalar operations within the same basic block and tries to
   use vector operations if possible *)
[@@@ocaml.warning "+a-40-41-42"]

val cfg : Format.formatter -> Cfg_with_layout.t -> Cfg_with_layout.t
