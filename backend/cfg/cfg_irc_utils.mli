[@@@ocaml.warning "+a-4-30-40-41-42"]

open Cfg_regalloc_utils

val irc_debug : bool

val irc_verbose : bool

val irc_invariants : bool

val log : indent:int -> ('a, Format.formatter, unit) format -> 'a

val log_body_and_terminator :
  indent:int ->
  Cfg.basic Cfg.instruction list ->
  Cfg.terminator Cfg.instruction ->
  unit

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

val all_precolored_regs : Reg.t array

val k : Reg.t -> int

val update_register_locations : unit -> unit

module Split_mode : sig
  type t =
    | Off
    | Naive

  val all : t list

  val to_string : t -> string

  val env : t Lazy.t
end

module Spilling_heuristics : sig
  type t =
    | Set_choose
    | Flat_uses
  (* CR xclerc for xclerc: | Hierarchical_uses *)

  val all : t list

  val to_string : t -> string

  val env : t Lazy.t
end

(* Actually work "sets", uses "lists" to follow the article/book. *)
module WorkList : sig
  module type S = sig
    type e

    type t

    module Set : Set.S with type elt = e

    val make : expected_max_size:int -> t

    val empty : t -> t

    val is_empty : t -> bool

    val add : t -> e -> t

    val remove : t -> e -> t

    val choose_and_remove : t -> (e * t) option

    val iter : t -> f:(e -> unit) -> unit

    val fold : t -> f:(e -> 'a -> 'a) -> init:'a -> 'a

    val to_list : t -> e list

    val to_set : t -> Set.t
  end

  module Make (E : Set.OrderedType) (ES : Set.S with type elt = E.t) :
    S with type e = E.t and module Set = ES
end
