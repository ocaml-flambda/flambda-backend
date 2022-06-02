[@@@ocaml.warning "+a-4-30-40-41-42"]

open Cfg_regalloc_utils

val naive_split_points
  : Cfg_with_layout.t -> Instruction.id list

val naive_split_cfg
  : Cfg_irc_state.t -> Cfg_with_layout.t -> Instruction.id list -> liveness -> Reg.t list
