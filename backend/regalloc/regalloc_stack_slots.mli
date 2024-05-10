[@@@ocaml.warning "+a-4-30-40-41-42"]

type slot = int

type t

val make : unit -> t

val iter : t -> f:(Reg.Tbl.key -> int -> unit) -> unit

val size_for_all_stack_classes : t -> int

val get_and_incr : t -> stack_class:int -> slot

val get_or_create : t -> Reg.t -> slot

val get_or_fatal : t -> Reg.t -> slot

val use_same_slot_or_fatal : t -> Reg.t -> existing:Reg.t -> unit

val update_cfg_with_layout : t -> Cfg_with_layout.t -> unit

(** Reduces the number of slots, by merging slots whose use
    intervals do not overlap. If a reduction occurs, registers
    are modified and liveness is invalidated, but the CFG is
    left untouched. *)
val optimize : t -> Cfg_with_infos.t -> unit
