[@@@ocaml.warning "+a-4-30-40-41-42"]

type slot = int

type t

val make : unit -> t

val get_and_incr : t -> reg_class:int -> slot

val get_or_create : t -> Reg.t -> slot

val get_or_fatal : t -> Reg.t -> slot

val use_same_slot_or_fatal : t -> Reg.t -> existing:Reg.t -> unit

val update_cfg_with_layout : t -> Cfg_with_layout.t -> unit
