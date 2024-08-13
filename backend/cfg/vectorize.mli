(* Finds adjacent memory chunks and tries to use vector operations if
   possible *)

val cfg : Format.formatter -> Cfg_with_layout.t -> Cfg_with_layout.t
