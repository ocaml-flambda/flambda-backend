[@@@ocaml.warning "+a-4-30-40-41-42"]

open Regalloc_utils

val irc_debug : bool

val irc_verbose : bool Lazy.t

val irc_invariants : bool Lazy.t

val log :
  indent:int -> ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

val log_body_and_terminator :
  indent:int ->
  Cfg.basic_instruction_list ->
  Cfg.terminator Cfg.instruction ->
  liveness ->
  unit

val log_cfg_with_infos : indent:int -> Cfg_with_infos.t -> unit

module Color : sig
  type t = int
end

module RegisterStamp : sig
  type t = int

  type pair

  val pair : t -> t -> pair

  val fst : pair -> t

  val snd : pair -> t

  module PairSet : sig
    type t

    val make : num_registers:int -> t

    val clear : t -> unit

    val mem : t -> pair -> bool

    val add : t -> pair -> unit

    val cardinal : t -> int

    val iter : t -> f:(pair -> unit) -> unit
  end
end

module Degree : sig
  type t = int

  val infinite : t

  val to_string : t -> string

  val to_float : t -> float
end

val is_move_instruction : Instruction.t -> bool

val all_precolored_regs : unit -> Reg.Set.t

val k : Reg.t -> int

val update_register_locations : unit -> unit

module Spilling_heuristics : sig
  type t =
    | Set_choose
    | Flat_uses
    | Hierarchical_uses

  val all : t list

  val to_string : t -> string

  val value : t Lazy.t
end

module ArraySet : sig
  module type S = sig
    type e

    type t

    val make : original_capacity:int -> t

    val clear : t -> unit

    val is_empty : t -> bool

    val choose_and_remove : t -> e option

    val add : t -> e -> unit

    val remove : t -> e -> unit

    val iter : t -> f:(e -> unit) -> unit

    val fold : t -> f:('a -> e -> 'a) -> init:'a -> 'a

    val to_list : t -> e list
  end

  module type OrderedTypeWithDummy = sig
    include Set.OrderedType

    val dummy : t (* note: does not 0-compare to any "interesting" value *)
  end

  module Make (T : OrderedTypeWithDummy) : S with type e = T.t
end
