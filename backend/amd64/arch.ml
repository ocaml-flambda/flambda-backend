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
[@@@ocaml.warning "+4"]

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

type prefetch_info = {
  is_write: bool;
  locality: prefetch_temporal_locality_hint;
  addr: addressing_mode;
}

type rounding_mode = Half_to_even | Down | Up | Towards_zero

type specific_operation =
    Ilea of addressing_mode             (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                        (* Store an integer constant *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarithmem of float_operation * addressing_mode
                                       (* Float arith operation with memory *)
  | Ibswap of int                      (* endianness conversion *)
  | Isqrtf                             (* Float square root *)
  | Ifloatsqrtf of addressing_mode     (* Float square root from memory *)
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
  | Iprefetch of                       (* memory prefetching hint *)
      { is_write: bool;
        locality: prefetch_temporal_locality_hint;
        addr: addressing_mode;
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

let print_specific_operation printreg op ppf arg =
  match op with
  | Ilea addr -> print_addressing printreg addr ppf arg
  | Istore_int(n, addr, is_assign) ->
      fprintf ppf "[%a] := %nd %s"
         (print_addressing printreg addr) arg n
         (if is_assign then "(assign)" else "(init)")
  | Ioffset_loc(n, addr) ->
      fprintf ppf "[%a] +:= %i" (print_addressing printreg addr) arg n
  | Isqrtf ->
      fprintf ppf "sqrtf %a" printreg arg.(0)
  | Ifloat_iround -> fprintf ppf "float_iround %a" printreg arg.(0)
  | Ifloat_round mode ->
     fprintf ppf "float_round %s %a" (string_of_rounding_mode mode)
       printreg arg.(0)
  | Ifloat_min -> fprintf ppf "float_min %a %a" printreg arg.(0) printreg arg.(1)
  | Ifloat_max -> fprintf ppf "float_max %a %a" printreg arg.(0) printreg arg.(1)
  | Ifloatsqrtf addr ->
     fprintf ppf "sqrtf float64[%a]"
             (print_addressing printreg addr) [|arg.(0)|]
  | Ifloatarithmem(op, addr) ->
      let op_name = function
      | Ifloatadd -> "+f"
      | Ifloatsub -> "-f"
      | Ifloatmul -> "*f"
      | Ifloatdiv -> "/f" in
      fprintf ppf "%a %s float64[%a]" printreg arg.(0) (op_name op)
                   (print_addressing printreg addr)
                   (Array.sub arg 1 (Array.length arg - 1))
  | Ibswap i ->
      fprintf ppf "bswap_%i %a" i printreg arg.(0)
  | Isextend32 ->
      fprintf ppf "sextend32 %a" printreg arg.(0)
  | Izextend32 ->
      fprintf ppf "zextend32 %a" printreg arg.(0)
  | Irdtsc ->
      fprintf ppf "rdtsc"
  | Irdpmc ->
      fprintf ppf "rdpmc %a" printreg arg.(0)
  | Icrc32q ->
      fprintf ppf "crc32 %a %a" printreg arg.(0) printreg arg.(1)
  | Iprefetch { is_write; locality; } ->
      fprintf ppf "prefetch is_write=%b prefetch_temporal_locality_hint=%s %a"
        is_write (string_of_prefetch_temporal_locality_hint locality)
        printreg arg.(0)

let win64 =
  match Config.system with
  | "win64" | "mingw64" | "cygwin" -> true
  | _                   -> false

open X86_ast

(* Certain float conditions aren't represented directly in the opcode for
   float comparison, so we have to swap the arguments. The swap information
   is also needed downstream because one of the arguments is clobbered. *)
let float_cond_and_need_swap cond =
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

let equal_addressing_mode left right = left = right
let equal_specific_operation left right = left = right
