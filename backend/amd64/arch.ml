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

module Extension = struct
  module T = struct
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
      | BMI
      | BMI2

    let compare = compare
  end

  include T
  module Set = Set.Make(T)

  let name = function
    | POPCNT -> "POPCNT"
    | PREFETCHW -> "PREFETCHW"
    | PREFETCHWT1 -> "PREFETCHWT1"
    | SSE3 -> "SSE3"
    | SSSE3 -> "SSSE3"
    | SSE4_1 -> "SSE41"
    | SSE4_2 -> "SSE42"
    | CLMUL -> "CLMUL"
    | LZCNT -> "LZCNT"
    | BMI -> "BMI"
    | BMI2 -> "BMI2"

  let generation = function
    | POPCNT -> "Nehalem+"
    | PREFETCHW -> "Broadwell+"
    | PREFETCHWT1 -> "Xeon Phi"
    | SSE3 -> "Prescott+"
    | SSSE3 -> "Core+"
    | SSE4_1 -> "Penryn+"
    | SSE4_2 -> "Nehalem+"
    | CLMUL -> "Westmere+"
    | LZCNT -> "Haswell+"
    | BMI -> "Haswell+"
    | BMI2 -> "Haswell+"

  let enabled_by_default = function
    | SSE3 | SSSE3 | SSE4_1 | SSE4_2
    | POPCNT | CLMUL | LZCNT | BMI | BMI2 -> true
    | PREFETCHW | PREFETCHWT1 -> false

  let all = Set.of_list [ POPCNT; PREFETCHW; PREFETCHWT1; SSE3; SSSE3; SSE4_1; SSE4_2; CLMUL; LZCNT; BMI; BMI2 ]
  let config = ref (Set.filter enabled_by_default all)

  let enabled t = Set.mem t !config
  let disabled t = not (enabled t)

  let args =
    let y t = "-f" ^ (name t |> String.lowercase_ascii) in
    let n t = "-fno-" ^ (name t |> String.lowercase_ascii) in
    Set.fold (fun t acc ->
      let print_default b = if b then " (default)" else "" in
      let yd = print_default (enabled t) in
      let nd = print_default (disabled t) in
      (y t, Arg.Unit (fun () -> config := Set.add t !config),
        Printf.sprintf "Enable %s instructions (%s)%s" (name t) (generation t) yd) ::
      (n t, Arg.Unit (fun () -> config := Set.remove t !config),
        Printf.sprintf "Disable %s instructions (%s)%s" (name t) (generation t) nd) :: acc)
    all []

    let available () = Set.fold (fun t acc -> t :: acc) !config []
end

(* Emit elf notes with trap handling information. *)
let trap_notes = ref true

(* Emit extension symbols for CPUID startup check  *)
let arch_check_symbols = ref true

(* Limit hardware registers available for allocation *)
let limit_regalloc = ref 9

(* Machine-specific command-line options *)

let command_line_options =
  [ "-fPIC", Arg.Set Clflags.pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear Clflags.pic_code,
      " Generate position-dependent machine code";
    "-ftrap-notes", Arg.Set trap_notes,
      " Emit .note.ocaml_eh section with trap handling information (default)";
    "-fno-trap-notes", Arg.Clear trap_notes,
      " Do not emit .note.ocaml_eh section with trap handling information";
    "-farch-check", Arg.Set arch_check_symbols,
      " Emit ISA extension symbols for CPUID check (default)";
    "-fno-arch-check", Arg.Clear arch_check_symbols,
      " Do not emit ISA extension symbols for CPUID check";
    "-flimit-regalloc", Arg.Set_int limit_regalloc,
      " Maximum number of hardware registers available for allocation";
  ] @ Extension.args

let assert_simd_enabled () =
  if not (Language_extension.is_enabled SIMD) then
  Misc.fatal_error "SIMD is not enabled."

(* Specific operations for the AMD64 processor *)

open Format

type sym_global = Global | Local

type addressing_mode =
    Ibased of string * sym_global * int (* symbol + displ *)
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

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type specific_operation =
    Ilea of addressing_mode             (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                        (* Store an integer constant *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarithmem of float_operation * addressing_mode
                                       (* Float arith operation with memory *)
  | Ifloatsqrtf of addressing_mode     (* Float square root from memory *)
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
    Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let size_vec128 = 16

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, glob, n) -> Ibased(s, glob, n + delta)
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

let int_of_bswap_bitwidth = function
  | Sixteen -> 16
  | Thirtytwo -> 32
  | Sixtyfour -> 64

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, _glob, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, _glob, n) ->
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
  | Ibswap { bitwidth } ->
    fprintf ppf "bswap_%i %a" (int_of_bswap_bitwidth bitwidth) printreg arg.(0)
  | Isextend32 ->
      fprintf ppf "sextend32 %a" printreg arg.(0)
  | Izextend32 ->
      fprintf ppf "zextend32 %a" printreg arg.(0)
  | Irdtsc ->
      fprintf ppf "rdtsc"
  | Ilfence ->
      fprintf ppf "lfence"
  | Isfence ->
      fprintf ppf "sfence"
  | Imfence ->
      fprintf ppf "mfence"
  | Irdpmc ->
      fprintf ppf "rdpmc %a" printreg arg.(0)
  | Isimd simd ->
      Simd.print_operation printreg simd ppf arg
  | Ipause ->
      fprintf ppf "pause"
  | Iprefetch { is_write; locality; } ->
      fprintf ppf "prefetch is_write=%b prefetch_temporal_locality_hint=%s %a"
        is_write (string_of_prefetch_temporal_locality_hint locality)
        printreg arg.(0)

(* Are we using the Windows 64-bit ABI? *)
let win64 =
  match Config.system with
  | "win64" | "mingw64" | "cygwin" -> true
  | _                   -> false

(* Specific operations that are pure *)

let operation_is_pure = function
  | Ilea _ | Ibswap _ | Isextend32 | Izextend32
  | Ifloatarithmem _ | Ifloatsqrtf _ -> true
  | Irdtsc | Irdpmc | Ipause
  | Ilfence | Isfence | Imfence
  | Istore_int (_, _, _) | Ioffset_loc (_, _)
  | Iprefetch _ -> false
  | Isimd op -> Simd.is_pure op

(* Specific operations that can raise *)

let operation_can_raise = function
  | Ilea _ | Ibswap _ | Isextend32 | Izextend32
  | Ifloatarithmem _ | Ifloatsqrtf _
  | Irdtsc | Irdpmc | Ipause | Isimd _
  | Ilfence | Isfence | Imfence
  | Istore_int (_, _, _) | Ioffset_loc (_, _)
  | Iprefetch _ -> false

let operation_allocates = function
  | Ilea _ | Ibswap _ | Isextend32 | Izextend32
  | Ifloatarithmem _ | Ifloatsqrtf _
  | Irdtsc | Irdpmc | Ipause | Isimd _
  | Ilfence | Isfence | Imfence
  | Istore_int (_, _, _) | Ioffset_loc (_, _)
  | Iprefetch _ -> false

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


let equal_addressing_mode left right =
  match left, right with
  | Ibased (left_sym, left_glob, left_displ), Ibased (right_sym, right_glob, right_displ) ->
    String.equal left_sym right_sym && left_glob = right_glob && Int.equal left_displ right_displ
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

let equal_specific_operation left right =
  match left, right with
  | Ilea x, Ilea y -> equal_addressing_mode x y
  | Istore_int (x, x', x''), Istore_int (y, y', y'') ->
    Nativeint.equal x y && equal_addressing_mode x' y' && Bool.equal x'' y''
  | Ioffset_loc (x, x'), Ioffset_loc (y, y') ->
    Int.equal x y && equal_addressing_mode x' y'
  | Ifloatarithmem (x, x'), Ifloatarithmem (y, y') ->
    equal_float_operation x y && equal_addressing_mode x' y'
  | Ibswap { bitwidth = left }, Ibswap { bitwidth = right } ->
    Int.equal (int_of_bswap_bitwidth left) (int_of_bswap_bitwidth right)
  | Ifloatsqrtf left, Ifloatsqrtf right ->
    equal_addressing_mode left right
  | Isextend32, Isextend32 ->
    true
  | Izextend32, Izextend32 ->
    true
  | Irdtsc, Irdtsc ->
    true
  | Irdpmc, Irdpmc ->
    true
  | Ilfence, Ilfence ->
    true
  | Isfence, Isfence ->
    true
  | Imfence, Imfence ->
    true
  | Ipause, Ipause -> true
  | Iprefetch { is_write = left_is_write; locality = left_locality; addr = left_addr; },
    Iprefetch { is_write = right_is_write; locality = right_locality; addr = right_addr; } ->
    Bool.equal left_is_write right_is_write
    && equal_prefetch_temporal_locality_hint left_locality right_locality
    && equal_addressing_mode left_addr right_addr
  | Isimd l, Isimd r ->
    Simd.equal_operation l r
  | (Ilea _ | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Ifloatsqrtf _ | Ibswap _ |
     Isextend32 | Izextend32 | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence |
     Ipause | Isimd _ | Iprefetch _), _ ->
    false
