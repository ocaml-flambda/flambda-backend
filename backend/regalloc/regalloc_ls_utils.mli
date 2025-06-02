[@@@ocaml.warning "+a-30-40-41-42"]

open Regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list
module Skip_list = Flambda_backend_utils.Skip_list

val log : ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

val equal_list_dll : ('a -> 'a -> bool) -> 'a list -> 'a DLL.t -> bool

val indent : unit -> unit

val dedent : unit -> unit

val reset_indentation : unit -> unit

val log_body_and_terminator :
  Cfg.basic_instruction_list ->
  Cfg.terminator Cfg.instruction ->
  liveness ->
  unit

val log_cfg_with_infos : Cfg_with_infos.t -> unit

(* The [trap_handler] parameter to the [instruction] and [terminator] functions
   is set to [true] iff the instruction is the first one of a block which is a
   trap handler. *)
val iter_instructions_dfs :
  Cfg_with_layout.t ->
  instruction:(trap_handler:bool -> Cfg.basic Cfg.instruction -> unit) ->
  terminator:(trap_handler:bool -> Cfg.terminator Cfg.instruction -> unit) ->
  unit

module Range : sig
  (* Similar to [Interval.range] (in "backend/interval.mli"). *)
  type t =
    { mutable begin_ : int;
      mutable end_ : int
    }

  val copy : t -> t

  val print : Format.formatter -> t -> unit

  val overlap : t DLL.t -> t DLL.t -> bool

  val is_live : t DLL.t -> pos:int -> bool

  val remove_expired : t DLL.t -> pos:int -> unit
end

module Interval : sig
  (* Similar to [Interval.t] (in "backend/interval.mli"). *)
  type t =
    { reg : Reg.t;
      mutable begin_ : int;
      mutable end_ : int;
      ranges : Range.t DLL.t
    }

  val equal : t -> t -> bool

  val compare_asc_begin : t -> t -> int

  val compare_desc_end : t -> t -> int

  val copy : t -> t

  val print : Format.formatter -> t -> unit

  val overlap : t -> t -> bool

  val is_live : t -> pos:int -> bool

  val remove_expired : t -> pos:int -> unit

  module DLL : sig
    val print : Format.formatter -> t DLL.t -> unit

    val release_expired_fixed : t DLL.t -> pos:int -> unit

    val insert_sorted : t DLL.t -> t -> unit
  end

  module AscBeginList : Skip_list.T with type elem = t

  module DescEndList : Skip_list.T with type elem = t
end

val equal_dll_asc_sl : Interval.t DLL.t -> Interval.AscBeginList.t -> bool

val equal_dll_desc_sl : Interval.t DLL.t -> Interval.DescEndList.t -> bool

module ClassIntervals : sig
  (* Similar to [Linscan.class_intervals] (in "backend/linscan.ml"). *)
  type t =
    { fixed_sl : Interval.DescEndList.t;
      active_sl : Interval.DescEndList.t;
      inactive_sl : Interval.DescEndList.t
    }

  val make : unit -> t

  val copy : t -> t

  val print : Format.formatter -> t -> unit

  val clear : t -> unit

  val release_expired_intervals : t -> pos:int -> unit
end

val log_interval : kind:string -> Interval.t -> unit

val log_interval_dll : kind:string -> Interval.t DLL.t -> unit

val log_interval_asc_sl : kind:string -> Interval.AscBeginList.t -> unit

val log_interval_desc_sl : kind:string -> Interval.DescEndList.t -> unit
