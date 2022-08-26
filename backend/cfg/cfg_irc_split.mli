[@@@ocaml.warning "+a-4-30-40-41-42"]

open Cfg_regalloc_utils

val naive_split_cfg :
  Cfg_irc_state.t -> Cfg_with_layout.t -> liveness -> Reg.t list
