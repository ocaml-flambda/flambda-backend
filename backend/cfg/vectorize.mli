(* Finds independent scalar operations within the same basic block and tries to
   use vector operations if possible *)

val cfg : Format.formatter -> Cfg_with_layout.t -> Cfg_with_layout.t
