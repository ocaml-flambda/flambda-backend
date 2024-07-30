[@@@ocaml.warning "+a-30-40-41-42"]

type liveness = Cfg_liveness.Liveness.domain Cfg_dataflow.Instr.Tbl.t

(** Holds a Cfg_with_layout.t value, and "caches" for:
    - the liveness information;
    - the dominators information;
    - the loop information.

   Each cache can be invalidated, and should be when modification of the
   underlying CFG would cause the cached information to become invalid.

   Typically, any structural change to the graph, or change to the `arg` and
   `res` field of the instrutions is likely to result in a different liveness
   information, but only structural changes to the graph would affect dominators
   and loop information. *)
type t

val make : Cfg_with_layout.t -> t

val cfg_with_layout : t -> Cfg_with_layout.t

val cfg : t -> Cfg.t

val fold_blocks :
  t -> f:(Label.t -> Cfg.basic_block -> 'a -> 'a) -> init:'a -> 'a

val fold_body_instructions :
  t -> f:('a -> Cfg.basic Cfg.instruction -> 'a) -> init:'a -> 'a

val get_block_exn : t -> Label.t -> Cfg.basic_block

val liveness : t -> liveness

val liveness_find : t -> int -> Cfg_liveness.Liveness.domain

val liveness_find_opt : t -> int -> Cfg_liveness.Liveness.domain option

val invalidate_liveness : t -> unit

val dominators : t -> Cfg_dominators.t

val loop_infos : t -> Cfg_loop_infos.t

val invalidate_dominators_and_loop_infos : t -> unit
