[@@@ocaml.warning "+a-30-40-41-42"]

type t = Reg.t Reg.Tbl.t

val apply_reg : t -> Reg.t -> Reg.t

val apply_array_in_place : t -> Reg.t array -> unit

val apply_array : t -> Reg.t array -> Reg.t array

val apply_set : t -> Reg.Set.t -> Reg.Set.t

val apply_instruction_in_place : t -> _ Cfg.instruction -> unit

val apply_block_in_place : t -> Cfg.basic_block -> unit

type map = t Label.Tbl.t

val for_label : map -> Label.t -> t

val apply_cfg_in_place : map -> Cfg.t -> unit
