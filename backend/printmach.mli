(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pretty-printing of pseudo machine code *)

open Format

val reg: formatter -> Reg.t -> unit
val regs: formatter -> Reg.t array -> unit
val regset: formatter -> Reg.Set.t -> unit
val regsetaddr: formatter -> Reg.Set.t -> unit
val operation: Mach.operation -> Mach.operand array -> formatter ->
  Reg.t array -> unit
val test: Mach.test -> formatter -> Mach.operand array -> unit
val instr: formatter -> Mach.instruction -> unit
val fundecl: formatter -> Mach.fundecl -> unit
val phase: string -> formatter -> Mach.fundecl -> unit
val interferences: formatter -> unit -> unit
val intervals: formatter -> unit -> unit
val preferences: formatter -> unit -> unit
val intop: Mach.integer_operation -> string
val floatop: Mach.float_operation -> string
val intcomp: Mach.integer_comparison -> string
val floatcomp: Mach.float_comparison -> string
val operand: formatter -> Mach.operand -> unit
val operands: formatter -> Mach.operand array -> unit
