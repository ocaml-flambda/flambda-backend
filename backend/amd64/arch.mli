# 2 "asmcomp/amd64/arch.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Machine-specific command-line options *)

module Extension : sig
  type t =
    | POPCNT
    | PREFETCHW
    | PREFETCHWT1
    | SSE3
    | SSSE3
    | SSE4_1
    | SSE4_2
    | CLMUL
    | LZCNT
    | BMI (* IMPORTANT: LZCNT/TZCNT are interpreted as BSR/BSF on architectures prior
             to Haswell, i.e. they do not cause an illegal instruction fault.
             That means code using LZCNT/TZCNT will silently produce wrong results. *)
    | BMI2

  val name : t -> string

  val enabled : t -> bool
  val available : unit -> t list
end

val trap_notes : bool ref
val arch_check_symbols : bool ref
val command_line_options : (string * Arg.spec * string) list

(* Specific operations for the AMD64 processor *)

type sym_global = Global | Local

type addressing_mode =
    Ibased of string * sym_global * int (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

val equal_addressing_mode : addressing_mode -> addressing_mode -> bool

type prefetch_temporal_locality_hint = Nonlocal | Low | Moderate | High

type prefetch_info = {
  is_write: bool;
  locality: prefetch_temporal_locality_hint;
  addr: addressing_mode;
}

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type float_width = Cmm.float_width

type specific_operation =
    Ilea of addressing_mode             (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                        (* Store an integer constant *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarithmem of float_width * float_operation * addressing_mode
                                       (* Float arith operation with memory *)
  | Ibswap of { bitwidth: bswap_bitwidth; } (* endianness conversion *)
  | Isextend32                         (* 32 to 64 bit conversion with sign
                                          extension *)
  | Izextend32                         (* 32 to 64 bit conversion with zero
                                          extension *)
  | Irdtsc                             (* read timestamp *)
  | Irdpmc                             (* read performance counter *)
  | Ilfence                            (* load fence *)
  | Isfence                            (* store fence *)
  | Imfence                            (* memory fence *)
  | Ipause                             (* hint for spin-wait loops *)
  | Isimd of Simd.operation            (* SIMD instruction set operations *)
  | Iprefetch of                       (* memory prefetching hint *)
      { is_write: bool;
        locality: prefetch_temporal_locality_hint;
        addr: addressing_mode;
      }

and float_operation =
  | Ifloatadd
  | Ifloatsub
  | Ifloatmul
  | Ifloatdiv

val equal_specific_operation : specific_operation -> specific_operation -> bool

val big_endian : bool

val size_addr : int

val size_int : int

val size_float : int

val size_vec128 : int

val allow_unaligned_access : bool

val division_crashes_on_overflow : bool

val identity_addressing : addressing_mode

val offset_addressing : addressing_mode -> int -> addressing_mode

val num_args_addressing : addressing_mode -> int

val print_addressing :
  (Format.formatter -> 'a -> unit) -> addressing_mode ->
  Format.formatter -> 'a array -> unit

val print_specific_operation :
  (Format.formatter -> 'a -> unit) -> specific_operation ->
  Format.formatter -> 'a array -> unit

val win64 : bool

val operation_is_pure : specific_operation -> bool

val operation_can_raise : specific_operation -> bool

val operation_allocates : specific_operation -> bool

val float_cond_and_need_swap
  :  Lambda.float_comparison -> X86_ast.float_condition * bool

(* addressing mode functions *)

val compare_addressing_mode_without_displ : addressing_mode -> addressing_mode -> int

val compare_addressing_mode_displ : addressing_mode -> addressing_mode -> int option

val addressing_offset_in_bytes : addressing_mode -> addressing_mode -> int option

val can_cross_loads_or_stores : specific_operation -> bool

val may_break_alloc_freshness : specific_operation -> bool
