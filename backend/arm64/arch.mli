(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the ARM processor, 64-bit mode *)

val macosx : bool
val is_asan_enabled : bool ref

(* Machine-specific command-line options *)

val command_line_options : (string * Arg.spec * string) list

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)
  | Ibased of string * int              (* global var + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type cmm_label = Label.t
  (* Do not introduce a dependency to Cmm *)

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type specific_operation =
  | Ifar_poll
  | Ifar_alloc of { bytes : int; dbginfo : Cmm.alloc_dbginfo }
  | Ishiftarith of arith_operation * int
  | Imuladd       (* multiply and add *)
  | Imulsub       (* multiply and subtract *)
  | Inegmulf      (* floating-point negate and multiply *)
  | Imuladdf      (* floating-point multiply and add *)
  | Inegmuladdf   (* floating-point negate, multiply and add *)
  | Imulsubf      (* floating-point multiply and subtract *)
  | Inegmulsubf   (* floating-point negate, multiply and subtract *)
  | Isqrtf        (* floating-point square root *)
  | Ibswap of { bitwidth: bswap_bitwidth; } (* endianness conversion *)
  | Imove32       (* 32-bit integer move *)
  | Isignext of int (* sign extension *)
  | Isimd of Simd.operation

and arith_operation =
    Ishiftadd
  | Ishiftsub

val equal_specific_operation : specific_operation -> specific_operation -> bool

(* Sizes, endianness *)

val big_endian : bool

val size_addr : int

val size_int : int

val size_float : int

val size_vec128 : int

val allow_unaligned_access : bool

(* Behavior of division *)

val division_crashes_on_overflow : bool

(* Operations on addressing modes *)

val equal_addressing_mode : addressing_mode -> addressing_mode -> bool

val identity_addressing : addressing_mode

val offset_addressing : addressing_mode -> int -> addressing_mode

val num_args_addressing : addressing_mode -> int

(* Printing operations and addressing modes *)

val print_addressing :
  (Format.formatter -> 'a -> unit) -> addressing_mode ->
  Format.formatter -> 'a array -> unit

val print_specific_operation :
  (Format.formatter -> 'a -> unit) -> specific_operation ->
  Format.formatter -> 'a array -> unit

val is_logical_immediate : nativeint -> bool

(* Specific operations that are pure *)

val operation_is_pure : specific_operation -> bool

(* Specific operations that allocate *)

val operation_allocates : specific_operation -> bool

(* Specific operations that can raise *)

val isomorphic_specific_operation : specific_operation -> specific_operation -> bool

(* See `amd64/arch.mli`. *)
val equal_addressing_mode_without_displ : addressing_mode -> addressing_mode -> bool

val addressing_offset_in_bytes
  : addressing_mode
  -> addressing_mode
  -> arg_offset_in_bytes:('a -> 'a -> int option)
  -> 'a array
  -> 'a array
  -> int option

(* CR-soon gyorsh: This function is a copy of code from selectgen + target specific
   selection.  It is used only in peephole rules.  When objects are replaced with a
   functor, pass the instantiated functor's [is_immediate] to the peephole pass in
   [Asmgen]. *)
val is_immediate_for_intop : Operation.integer_operation -> int -> bool
