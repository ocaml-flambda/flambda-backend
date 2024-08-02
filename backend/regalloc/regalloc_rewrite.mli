[@@@ocaml.warning "+a-4-30-40-41-42"]

open Regalloc_utils

module type State = sig
  type t

  val stack_slots : t -> Regalloc_stack_slots.t

  val get_and_incr_instruction_id : t -> Instruction.id

  val get_round_num : t -> int
end

module type Utils = sig
  val debug : bool

  val invariants : bool Lazy.t

  val log :
    indent:int -> ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

  val log_body_and_terminator :
    indent:int ->
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit

  (* Tests whether the passed register is marked as "spilled". *)
  val is_spilled : Reg.t -> bool

  (* Sets the passed register as "spilled". *)
  val set_spilled : Reg.t -> unit
end

(* This is the `rewrite` function from IRC, parametrized by state, functions for
   debugging, and function to test/set the "spilled" state of a register. It
   inserts spills and reloads for registers in the [spilled_nodes] parameter
   (thus basically corresponding to Upstream's [Reload] pass). The returned
   couple contains the list of introduced temporaries, and a boolean which is
   `true` iff at least one block was inserted. *)
val rewrite_gen :
  (module State with type t = 's) ->
  (module Utils) ->
  's ->
  Cfg_with_infos.t ->
  spilled_nodes:Reg.t list ->
  Reg.t list * bool

(* Runs the first steps common to register allocators, reinitializing registers,
   checking preconditions, and collecting information from the CFG. *)
val prelude :
  (module Utils) ->
  on_fatal_callback:(unit -> unit) ->
  Cfg_with_infos.t ->
  cfg_infos * Regalloc_stack_slots.t

(* Runs the last steps common to register allocators, updating the CFG (stack
   slots, live fields, and prologue), running [f], and checking
   postconditions. *)
val postlude :
  (module State with type t = 's) ->
  (module Utils) ->
  's ->
  f:(unit -> unit) ->
  Cfg_with_infos.t ->
  unit
