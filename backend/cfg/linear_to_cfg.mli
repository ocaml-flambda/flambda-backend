(** Conversion from [Linear] to [Cfg] code. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val run : Linear.fundecl -> preserve_orig_labels:bool -> Cfg_with_layout.t
