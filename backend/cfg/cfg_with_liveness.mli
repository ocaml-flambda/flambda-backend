[@@@ocaml.warning "+a-30-40-41-42"]

type liveness = Cfg_liveness.Liveness.domain Cfg_dataflow.Instr.Tbl.t

type t
(* Holds a Cfg_with_layout.t value, and a "cache" of the liveness information
   that can be invalidated. *)

val make : Cfg_with_layout.t -> t

val cfg_with_layout : t -> Cfg_with_layout.t

val cfg : t -> Cfg.t

val fold_blocks :
  t -> f:(Label.t -> Cfg.basic_block -> 'a -> 'a) -> init:'a -> 'a

val get_block_exn : t -> Label.t -> Cfg.basic_block

val liveness : t -> liveness

val liveness_find : t -> int -> Cfg_liveness.Liveness.domain

val liveness_find_opt : t -> int -> Cfg_liveness.Liveness.domain option

val invalidate_liveness : t -> unit
