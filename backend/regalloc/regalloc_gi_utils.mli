[@@@ocaml.warning "+a-4-30-40-41-42"]

open Regalloc_utils

val gi_debug : bool

val gi_verbose : bool Lazy.t

val gi_invariants : bool Lazy.t

val log :
  indent:int -> ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

val log_body_and_terminator :
  indent:int ->
  Cfg.basic_instruction_list ->
  Cfg.terminator Cfg.instruction ->
  liveness ->
  unit

val log_cfg_with_infos : indent:int -> Cfg_with_infos.t -> unit

module Priority_heuristics : sig
  type t =
    | Interval_length
    | Random_for_testing

  val all : t list

  val to_string : t -> string

  val random : unit -> int

  val value : t Lazy.t
end

module Selection_heuristics : sig
  type t =
    | First_available
    | Best_fit
    | Worst_fit
    | Random_for_testing

  val all : t list

  val to_string : t -> string

  val random : unit -> t

  val value : t Lazy.t
end

module Spilling_heuristics : sig
  type t =
    | Flat_uses
    | Hierarchical_uses
    | Random_for_testing

  val all : t list

  val to_string : t -> string

  val random : unit -> bool

  val value : t Lazy.t
end

module type Order = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type Priority_queue = sig
  type priority

  type 'a t

  type 'a element =
    { priority : priority;
      data : 'a
    }

  val make : initial_capacity:int -> 'a t

  val is_empty : 'a t -> bool

  val size : 'a t -> int

  val add : 'a t -> priority:priority -> data:'a -> unit

  val get : 'a t -> 'a element

  val remove : 'a t -> unit

  val get_and_remove : 'a t -> 'a element

  val iter : 'a t -> f:('a element -> unit) -> unit
end

module Make_max_priority_queue (Priority : Order) :
  Priority_queue with type priority = Priority.t

module Make_min_priority_queue (Priority : Order) :
  Priority_queue with type priority = Priority.t

val iter_cfg_layout : Cfg_with_layout.t -> f:(Cfg.basic_block -> unit) -> unit

val iter_instructions_layout :
  Cfg_with_layout.t ->
  instruction:(trap_handler:bool -> Cfg.basic Cfg.instruction -> unit) ->
  terminator:(trap_handler:bool -> Cfg.terminator Cfg.instruction -> unit) ->
  unit

module Range : sig
  type t =
    { begin_ : int;
      mutable end_ : int
    }

  val length : t -> int

  val copy : t -> t

  val print : Format.formatter -> t -> unit

  val overlap : t list -> t list -> bool

  val is_live : t list -> pos:int -> bool

  val remove_expired : t list -> pos:int -> t list
end

module Interval : sig
  type t =
    { mutable begin_ : int;
      mutable end_ : int;
      mutable ranges : Range.t list
    }

  val make_empty : unit -> t

  val length : t -> int

  val print : Format.formatter -> t -> unit

  val overlap : t -> t -> bool
end

val build_intervals : Cfg_with_infos.t -> Interval.t Reg.Tbl.t

module Hardware_register : sig
  type location = private
    { reg_class : int;
      reg_index_in_class : int
    }

  val make_location : reg_class:int -> reg_index_in_class:int -> location

  val print_location : Format.formatter -> location -> unit

  val reg_location_of_location : location -> Reg.location

  type assigned =
    { pseudo_reg : Reg.t;
      interval : Interval.t;
      evictable : bool
    }

  val print_assigned : Format.formatter -> assigned -> unit

  type t =
    { location : location;
      interval : Interval.t;
      mutable assigned : assigned list
    }

  val add_non_evictable : t -> Reg.t -> Interval.t -> unit
end

type available =
  | For_assignment of { hardware_reg : Hardware_register.t }
  | For_eviction of
      { hardware_reg : Hardware_register.t;
        evicted_regs : Hardware_register.assigned list
      }
  | Split_or_spill

module Hardware_registers : sig
  type t

  val make : unit -> t

  val of_reg : t -> Reg.t -> Hardware_register.t

  val find_available : t -> Reg.t -> Interval.t -> available
end
