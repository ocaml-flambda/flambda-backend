[@@@ocaml.warning "+a-30-40-41-42"]

open Regalloc_utils

val indent : unit -> unit

val dedent : unit -> unit

val reset_indentation : unit -> unit

val log : ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

val log_body_and_terminator :
  Cfg.basic_instruction_list ->
  Cfg.terminator Cfg.instruction ->
  liveness ->
  unit

val log_cfg_with_infos : Cfg_with_infos.t -> unit

module WorkList : sig
  (* CR xclerc for xclerc: double check all constructors are actually used. *)
  type t =
    | Unknown_list
    | Precolored
    | Initial
    | Simplify
    | Freeze
    | Spill
    | Spilled
    | Coalesced
    | Colored
    | Select_stack

  val equal : t -> t -> bool

  val to_string : t -> string
end

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

module Spilling_heuristics : sig
  type t =
    | Set_choose
    | Flat_uses
    | Hierarchical_uses

  val all : t list

  val to_string : t -> string

  val value : t Lazy.t
end
