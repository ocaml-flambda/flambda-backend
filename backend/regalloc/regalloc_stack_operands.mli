[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils

(* The functions below can rewrite (in place) a spilled register (in arg or
   res), if the operand constraints of the instruction allow it. The passed
   mapping is used to determine the location of the register on the stack; it
   means that for all spilled registers, there is a mapping from the spilled
   register to one whose location is on the stack (i.e. spilling slot for the
   first register). The functions return a value indicating whether there may
   still be spilled registers in arg or res. *)

val basic : spilled_map -> Cfg.basic Cfg.instruction -> stack_operands_rewrite

val terminator :
  spilled_map -> Cfg.terminator Cfg.instruction -> stack_operands_rewrite
