# 2 "backend/amd64/arch.ml"
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
[@@@ocaml.warning "+4+26+27+30+31+32+33+34+35+36+37+38+39"]

(* POPCNT instruction is not available prior to Nehalem, released in 2008. *)
let popcnt_support = ref true

(* CRC32 requires SSE 4.2 support *)
let crc32_support = ref true

(* PREFETCHW instruction is not available on processors
   based on Haswell or earlier microarchitectures. *)
let prefetchw_support = ref true

(* PREFETCHWT1 is Intel Xeon Phi only. *)
let prefetchwt1_support = ref false

(* Machine-specific command-line options *)

let command_line_options =
  [ "-fPIC", Arg.Set Clflags.pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear Clflags.pic_code,
      " Generate position-dependent machine code";
    "-fpopcnt", Arg.Set popcnt_support,
      " Use POPCNT instruction (not available prior to Nehalem) (default)";
    "-fno-popcnt", Arg.Clear popcnt_support,
      " Do not use POPCNT instruction";
    "-fcrc32", Arg.Set crc32_support,
      " Use CRC32 instructions (requires SSE4.2 support) (default)";
    "-fno-crc32", Arg.Clear crc32_support,
      " Do not emit CRC32 instructions";
    "-fprefetchw", Arg.Set prefetchw_support,
      " Use PREFETCHW instructions (not available on Haswell and earlier) \
        (default)";
    "-fno-prefetchw", Arg.Clear prefetchw_support,
      " Do not use PREFETCHW instructions";
    "-fprefetchwt1", Arg.Set prefetchwt1_support,
      " Use PREFETCHWT1 instructions (Intel Xeon Phi only)";
    "-fno-prefetchwt1", Arg.Clear prefetchwt1_support,
      " Do not use PREFETCHWT1 instructions (default)";
  ]

(* Specific operations for the AMD64 processor *)

open Format

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

type prefetch_temporal_locality_hint = Nonlocal | Low | Moderate | High

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type rounding_mode = Half_to_even | Down | Up | Towards_zero | Current

type specific_operation =
    Ilea                               (* "lea" gives scaled adds *)
  | Ioffset_loc                        (* Add a constant to a location *)
  | Ibswap of { bitwidth: bswap_bitwidth; } (* endianness conversion *)
  | Isqrtf                             (* Float square root *)
  | Ifloat_iround                      (* Rounds a [float] to an [int64]
                                          using the current rounding mode *)
  | Ifloat_round of rounding_mode      (* Round [float] to an integer [float]
                                          using the specified mode *)
  | Ifloat_min                         (* Return min of two floats *)
  | Ifloat_max                         (* Return max of two floats *)
  | Isextend32                         (* 32 to 64 bit conversion with sign
                                          extension *)
  | Izextend32                         (* 32 to 64 bit conversion with zero
                                          extension *)
  | Irdtsc                             (* read timestamp *)
  | Irdpmc                             (* read performance counter *)
  | Icrc32q                            (* compute crc *)
  | Ipause                             (* hint for spin-wait loops *)
  | Iprefetch of                       (* memory prefetching hint *)
      { is_write: bool;
        locality: prefetch_temporal_locality_hint;
      }

and float_operation =
    Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iscaled(scale, n) -> Iscaled(scale, n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased _ -> 0
  | Iindexed _ -> 1
  | Iindexed2 _ -> 2
  | Iscaled _ -> 1
  | Iindexed2scaled _ -> 2

(* Printing operations and addressing modes *)

let string_of_prefetch_temporal_locality_hint = function
  | Nonlocal -> "nonlocal"
  | Low -> "low"
  | Moderate -> "moderate"
  | High -> "high"

let string_of_rounding_mode = function
  | Half_to_even -> "half_to_even"
  | Down -> "down"
  | Up -> "up"
  | Towards_zero -> "truncate"
  | Current -> "current"

let int_of_bswap_bitwidth = function
  | Sixteen -> 16
  | Thirtytwo -> 32
  | Sixtyfour -> 64

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a%s" printreg arg.(0) printreg arg.(1) idx
  | Iscaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a  * %i%s" printreg arg.(0) scale idx
  | Iindexed2scaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a * %i%s" printreg arg.(0) printreg arg.(1) scale idx

let print_specific_operation_name op =
  match op with
  | Ilea -> "lea"
  | Ioffset_loc -> "offset_loc"
  | Isqrtf -> "sqrtf"
  | Ifloat_iround -> "float_iround"
  | Ifloat_round mode -> "float_round "^(string_of_rounding_mode mode)
  | Ifloat_min -> "float_min"
  | Ifloat_max -> "float_max"
  | Ibswap _ -> "bswap"
  | Isextend32 -> "sextend32"
  | Izextend32 -> "zextend32"
  | Irdtsc -> "rdtsc"
  | Irdpmc -> "rdpmc"
  | Icrc32q -> "crc32"
  | Iprefetch _ -> "prefetch"
  | Ipause -> "pause"

let print_specific_operation printoperand op ppf arg =
  match op with
  | Ilea -> printoperand ppf arg.(0)
  | Ioffset_loc ->
      fprintf ppf "[%a] +:= %a" printoperand arg.(0) printoperand arg.(1)
  | Isqrtf ->
      fprintf ppf "sqrtf %a" printoperand arg.(0)
  | Ifloat_iround -> fprintf ppf "float_iround %a" printoperand arg.(0)
  | Ifloat_round mode ->
     fprintf ppf "float_round %s %a" (string_of_rounding_mode mode)
       printoperand arg.(0)
  | Ifloat_min -> fprintf ppf "float_min %a %a"
                    printoperand arg.(0) printoperand arg.(1)
  | Ifloat_max -> fprintf ppf "float_max %a %a"
                    printoperand arg.(0) printoperand arg.(1)
  | Ibswap { bitwidth } ->
      fprintf ppf "bswap_%i %a" (int_of_bswap_bitwidth bitwidth)
        printoperand arg.(0)
  | Isextend32 ->
      fprintf ppf "sextend32 %a" printoperand arg.(0)
  | Izextend32 ->
      fprintf ppf "zextend32 %a" printoperand arg.(0)
  | Irdtsc ->
      fprintf ppf "rdtsc"
  | Irdpmc ->
      fprintf ppf "rdpmc %a" printoperand arg.(0)
  | Icrc32q ->
      fprintf ppf "crc32 %a %a" printoperand arg.(0) printoperand arg.(1)
  | Ipause ->
      fprintf ppf "pause"
  | Iprefetch { is_write; locality; } ->
      fprintf ppf "prefetch is_write=%b prefetch_temporal_locality_hint=%s %a"
        is_write (string_of_prefetch_temporal_locality_hint locality)
        printoperand arg.(0)

let win64 =
  match Config.system with
  | "win64" | "mingw64" | "cygwin" -> true
  | _                   -> false

open X86_ast

(* Certain float conditions aren't represented directly in the opcode for
   cmpsd, so we have to swap the arguments. The swap information
   is also needed downstream because one of the arguments is clobbered. *)
let float_compare_and_need_swap cond =
  match (cond : Lambda.float_comparison) with
  | CFeq  -> EQf,  false
  | CFneq -> NEQf, false
  | CFlt  -> LTf,  false
  | CFnlt -> NLTf, false
  | CFgt  -> LTf,  true
  | CFngt -> NLTf, true
  | CFle  -> LEf,  false
  | CFnle -> NLEf, false
  | CFge  -> LEf,  true
  | CFnge -> NLEf, true

(* CR gyorsh: This referring to Lambda is horrible,
   but CMM creates a dependency cycle.  *)
let float_test_need_swap : Lambda.float_comparison -> bool = function
  | CFlt | CFnlt | CFle  | CFnle -> true
  | CFeq | CFneq | CFgt | CFngt | CFge | CFnge -> false

let equal_addressing_mode left right =
  match left, right with
  | Ibased (left_sym, left_displ), Ibased (right_sym, right_displ) ->
    String.equal left_sym right_sym && Int.equal left_displ right_displ
  | Iindexed left_displ, Iindexed right_displ ->
    Int.equal left_displ right_displ
  | Iindexed2 left_displ, Iindexed2 right_displ ->
    Int.equal left_displ right_displ
  | Iscaled (left_scale, left_displ), Iscaled (right_scale, right_displ) ->
    Int.equal left_scale right_scale && Int.equal left_displ right_displ
  | Iindexed2scaled (left_scale, left_displ), Iindexed2scaled (right_scale, right_displ) ->
    Int.equal left_scale right_scale && Int.equal left_displ right_displ
  | (Ibased _ | Iindexed _ | Iindexed2 _ | Iscaled _ | Iindexed2scaled _), _ ->
    false

let equal_prefetch_temporal_locality_hint left right =
  match left, right with
  | Nonlocal, Nonlocal -> true
  | Low, Low -> true
  | Moderate, Moderate -> true
  | High, High -> true
  | (Nonlocal | Low | Moderate | High), _ -> false

let equal_float_operation left right =
  match left, right with
  | Ifloatadd, Ifloatadd -> true
  | Ifloatsub, Ifloatsub -> true
  | Ifloatmul, Ifloatmul -> true
  | Ifloatdiv, Ifloatdiv -> true
  | (Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv), _ -> false

let equal_rounding_mode left right =
  match left, right with
  | Half_to_even, Half_to_even -> true
  | Down, Down -> true
  | Up, Up -> true
  | Towards_zero, Towards_zero -> true
  | Current, Current -> true
  | (Half_to_even | Down | Up | Towards_zero | Current), _ -> false

let equal_specific_operation left right =
  match left, right with
  | Ilea, Ilea -> true
  | Ioffset_loc, Ioffset_loc -> true
  | Ibswap { bitwidth = left }, Ibswap { bitwidth = right } ->
    Int.equal (int_of_bswap_bitwidth left) (int_of_bswap_bitwidth right)
  | Isqrtf, Isqrtf ->
    true
  | Isextend32, Isextend32 ->
    true
  | Izextend32, Izextend32 ->
    true
  | Irdtsc, Irdtsc ->
    true
  | Irdpmc, Irdpmc ->
    true
  | Icrc32q, Icrc32q ->
    true
  | Ifloat_iround, Ifloat_iround -> true
  | Ifloat_round x, Ifloat_round y -> equal_rounding_mode x y
  | Ifloat_min, Ifloat_min -> true
  | Ifloat_max, Ifloat_max -> true
  | Ipause, Ipause -> true
  | Iprefetch { is_write = left_is_write; locality = left_locality; },
    Iprefetch { is_write = right_is_write; locality = right_locality; } ->
    Bool.equal left_is_write right_is_write
    && equal_prefetch_temporal_locality_hint left_locality right_locality
  | (Ilea | Ioffset_loc | Ibswap _
    | Isqrtf | Isextend32 | Izextend32 | Irdtsc | Irdpmc
    | Ifloat_iround | Ifloat_round _ | Ifloat_min | Ifloat_max
    | Ipause
    | Icrc32q | Iprefetch _), _ ->
    false
