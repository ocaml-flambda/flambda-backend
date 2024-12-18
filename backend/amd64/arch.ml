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

(* Machine-specific command-line options *)

let command_line_options =
  [ "-fPIC", Arg.Set Clflags.pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear Clflags.pic_code,
      " Generate position-dependent machine code";
    "-ftrap-notes", Arg.Set trap_notes,
      " Emit .note.ocaml_eh section with trap handling information (default)";
    "-fno-trap-notes", Arg.Clear trap_notes,
      " Do not emit .note.ocaml_eh section with trap handling information"
  ] @ Extension.args

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

type float_width = Cmm.float_width

type specific_operation =
    Ilea of addressing_mode            (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                       (* Store an integer constant *)
  | Ioffset_loc of int * addressing_mode
                                       (* Add a constant to a location *)
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
  | Icldemote of addressing_mode       (* hint to demote a cacheline to L3 *)
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
  | Ifloatarithmem(width, op, addr) ->
      let op_name = match width, op with
      | Float64, Ifloatadd -> "+f"
      | Float64, Ifloatsub -> "-f"
      | Float64, Ifloatmul -> "*f"
      | Float64, Ifloatdiv -> "/f"
      | Float32, Ifloatadd -> "+f32"
      | Float32, Ifloatsub -> "-f32"
      | Float32, Ifloatmul -> "*f32"
      | Float32, Ifloatdiv -> "/f32" in
      fprintf ppf "%a %s float64[%a]" printreg arg.(0) op_name
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
  | Icldemote _ ->
      fprintf ppf "cldemote %a" printreg arg.(0)
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
(* Keep in sync with [of_specific_operation] *)
let operation_is_pure = function
  | Ilea _ | Ibswap _ | Isextend32 | Izextend32
  | Ifloatarithmem _  -> true
  | Irdtsc | Irdpmc | Ipause
  | Ilfence | Isfence | Imfence
  | Istore_int (_, _, _) | Ioffset_loc (_, _)
  | Icldemote _ | Iprefetch _ -> false
  | Isimd op -> Simd.is_pure op

(* Specific operations that can raise *)
(* Keep in sync with [of_specific_operation] *)
let operation_can_raise = function
  | Ilea _ | Ibswap _ | Isextend32 | Izextend32
  | Ifloatarithmem _
  | Irdtsc | Irdpmc | Ipause | Isimd _
  | Ilfence | Isfence | Imfence
  | Istore_int (_, _, _) | Ioffset_loc (_, _)
  | Icldemote _ | Iprefetch _ -> false

(* Keep in sync with [of_specific_operation] *)
let operation_allocates = function
  | Ilea _ | Ibswap _ | Isextend32 | Izextend32
  | Ifloatarithmem _
  | Irdtsc | Irdpmc | Ipause | Isimd _
  | Ilfence | Isfence | Imfence
  | Istore_int (_, _, _) | Ioffset_loc (_, _)
  | Icldemote _ | Iprefetch _ -> false

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
  | Ifloatadd, Ifloatadd
  | Ifloatsub, Ifloatsub
  | Ifloatmul, Ifloatmul
  | Ifloatdiv, Ifloatdiv -> true
  | (Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv), _ -> false

let equal_specific_operation left right =
  match left, right with
  | Ilea x, Ilea y -> equal_addressing_mode x y
  | Istore_int (x, x', x''), Istore_int (y, y', y'') ->
    Nativeint.equal x y && equal_addressing_mode x' y' && Bool.equal x'' y''
  | Ioffset_loc (x, x'), Ioffset_loc (y, y') ->
    Int.equal x y && equal_addressing_mode x' y'
  | Ifloatarithmem (xw, x, x'), Ifloatarithmem (yw, y, y') ->
    Cmm.equal_float_width xw yw &&
    equal_float_operation x y &&
    equal_addressing_mode x' y'
  | Ibswap { bitwidth = left }, Ibswap { bitwidth = right } ->
    Int.equal (int_of_bswap_bitwidth left) (int_of_bswap_bitwidth right)
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
  | Icldemote x, Icldemote x' -> equal_addressing_mode x x'
  | Iprefetch { is_write = left_is_write; locality = left_locality; addr = left_addr; },
    Iprefetch { is_write = right_is_write; locality = right_locality; addr = right_addr; } ->
    Bool.equal left_is_write right_is_write
    && equal_prefetch_temporal_locality_hint left_locality right_locality
    && equal_addressing_mode left_addr right_addr
  | Isimd l, Isimd r ->
    Simd.equal_operation l r
  | (Ilea _ | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Ibswap _ |
     Isextend32 | Izextend32 | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence |
     Ipause | Isimd _ | Icldemote _ | Iprefetch _), _ ->
    false

(* addressing mode functions *)

let compare_addressing_mode_without_displ (addressing_mode_1: addressing_mode) (addressing_mode_2 : addressing_mode) =
  (* Ignores displ when comparing to show that it is possible to calculate the offset *)
  match addressing_mode_1, addressing_mode_2 with
  | Ibased (symbol1, global1, _), Ibased (symbol2, global2, _) -> (
    match global1, global2 with
    | Global, Global | Local, Local ->
      String.compare symbol1 symbol2
    | Global, Local -> -1
    | Local, Global -> 1)
  | Ibased _, _ -> -1
  | _, Ibased _ -> 1
  | Iindexed _, Iindexed _ -> 0
  | Iindexed _, _ -> -1
  | _, Iindexed _ -> 1
  | Iindexed2 _, Iindexed2 _ -> 0
  | Iindexed2 _, _ -> -1
  | _, Iindexed2 _ -> 1
  | Iscaled (scale1, _), Iscaled (scale2, _) -> Int.compare scale1 scale2
  | Iscaled _, _ -> -1
  | _, Iscaled _ -> 1
  | Iindexed2scaled (scale1, _), Iindexed2scaled (scale2, _) ->
    Int.compare scale1 scale2

let addressing_offset_in_bytes
      (addressing_mode_1: addressing_mode)
      (addressing_mode_2 : addressing_mode)
      ~arg_offset_in_bytes
      args_1
      args_2
  =
  let address_arg_offset_in_bytes index =
    arg_offset_in_bytes args_1.(index) args_2.(index)
  in
  match addressing_mode_1, addressing_mode_2 with
  | Ibased (symbol1, global1, n1), Ibased (symbol2, global2, n2) ->
    (* symbol + displ *)
    (match global1, global2 with
     | Global, Global | Local, Local ->
       if String.equal symbol1 symbol2 then Some (n2 - n1) else None
     | Global, Local | Local, Global -> None)
  | Iindexed n1, Iindexed n2 ->
    (* reg + displ *)
    (match address_arg_offset_in_bytes 0 with
     | Some base_off -> Some (base_off + (n2 - n1))
     | None -> None)
  | Iindexed2 n1, Iindexed2 n2 ->
    (* reg + reg + displ *)
    (match address_arg_offset_in_bytes 0, address_arg_offset_in_bytes 1 with
     | Some arg0_offset, Some arg1_offset ->
       Some (arg0_offset + arg1_offset + (n2 - n1))
     | (None, _|Some _, _) -> None)
  | Iscaled (scale1, n1), Iscaled (scale2, n2) ->
    (* reg * scale + displ *)
    if not (Int.compare scale1 scale2 = 0) then None
    else
      (match address_arg_offset_in_bytes 0 with
       | Some offset -> Some ((offset * scale1) + (n2 - n1))
       | None -> None)
  | Iindexed2scaled (scale1, n1), Iindexed2scaled (scale2, n2) ->
    (* reg + reg * scale + displ *)
    if not (Int.compare scale1 scale2 = 0) then None else
      (match address_arg_offset_in_bytes 0, address_arg_offset_in_bytes 1 with
       | Some arg0_offset, Some arg1_offset ->
         Some (arg0_offset + (arg1_offset*scale1) + (n2 - n1))
       | (None, _|Some _, _) -> None)
  | Ibased _, _ -> None
  | Iindexed _, _ -> None
  | Iindexed2 _, _ -> None
  | Iscaled _, _ -> None
  | Iindexed2scaled _, _ -> None

module Memory_access = struct
  module Init_or_assign = struct
    type t =
      | Initialization
      | Assignment
  end

  type desc =
    | Alloc
    | Arbitrary
    | Read of
        { width_in_bits : int;
          addressing_mode : addressing_mode;
          is_mutable: bool;
          is_atomic: bool;
        }
    | Write of
        { width_in_bits : int;
          addressing_mode : addressing_mode;
          init_or_assign : Init_or_assign.t
        }
    | Read_and_write of
        {
          width_in_bits : int;
          addressing_mode : addressing_mode;
          is_atomic: bool;
        }

  type t =
    { desc : desc;
      first_memory_arg_index : int
    }

  let create ?(first_memory_arg_index=0) desc =
    Some { desc; first_memory_arg_index; }

  let desc t = t.desc

  let first_memory_arg_index t = t.first_memory_arg_index

  (* Keep in sync with [operation_is_pure], [operation_can_raise],
     [operation_allocates]. *)
  let of_specific_operation : specific_operation -> t option =
    fun op ->
    match op with
    | Istore_int (_n, addressing_mode, is_assignment) ->
      let desc =
        Write { width_in_bits = 64;
                addressing_mode;
                init_or_assign = if is_assignment then Assignment else Initialization
              }
      in
      create ~first_memory_arg_index:0 desc
    | Ifloatarithmem (float_width, _float_op, addressing_mode) ->
      let width_in_bits =
        match float_width with
        | Float64 -> 64
        | Float32 -> 32
      in
      let is_mutable =
        (* CR-someday gyorsh: conservative, propagate mutability of Ifloatarithmem from
           selection to make it precise. *)
        true
      in
      let desc =
        Read {
          width_in_bits; addressing_mode; is_mutable; is_atomic = false;
        }
      in
      create ~first_memory_arg_index:1 desc
    | Ioffset_loc (_n, addressing_mode) ->
      let desc =
        Read_and_write { width_in_bits = 64;
                         addressing_mode;
                         is_atomic = false;
                       }
      in
      create desc
    | Iprefetch  { is_write = _; locality = _; addr = _ } ->
      (* Conservative, to prevent reordering anything around this instruction.
         Using [addressing_mode] is tricky because it need not be the start of the
         prefetch cache line and the interval would depend on cache line size. *)
      create Arbitrary
    | Icldemote _
    | Irdtsc
    | Irdpmc
    | Ilfence | Isfence | Imfence | Ipause ->
      (* Conservative, don't reorder around timing or ordering instructions. *)
      create Arbitrary
    | Isimd op ->
      (* Conservative. we don't have any simd operations with memory operations
         at the moment. *)
      if Simd.is_pure op
      then None
      else create Arbitrary
    | Ilea _ | Ibswap _ | Isextend32 | Izextend32 -> None

end
