[@@@ocaml.warning "+a-40-41-42"]

(** Merge blocks and eliminate dead blocks  *)
val run : Cfg_with_layout.t -> Cfg_with_layout.t
