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

(* Instruction selection for the AMD64 *)

open Arch
open Proc

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
  | Asymbol of Cmm.symbol
  | Alinear of Cmm.expression
  | Aadd of Cmm.expression * Cmm.expression
  | Ascale of Cmm.expression * int
  | Ascaledadd of Cmm.expression * Cmm.expression * int

let rec select_addr exp =
  let default = Alinear exp, 0 in
  match exp with
  | Cmm.Cconst_symbol (s, _) when not !Clflags.dlcode -> Asymbol s, 0
  | Cmm.Cop ((Caddi | Caddv | Cadda), [arg; Cconst_int (m, _)], _)
  | Cmm.Cop ((Caddi | Caddv | Cadda), [Cconst_int (m, _); arg], _) ->
    let a, n = select_addr arg in
    if Misc.no_overflow_add n m then a, n + m else default
  | Cmm.Cop (Csubi, [arg; Cconst_int (m, _)], _) ->
    let a, n = select_addr arg in
    if Misc.no_overflow_sub n m then a, n - m else default
  | Cmm.Cop (Clsl, [arg; Cconst_int (((1 | 2 | 3) as shift), _)], _) -> (
    let default = Ascale (arg, 1 lsl shift), 0 in
    match select_addr arg with
    | Alinear e, n ->
      if Misc.no_overflow_lsl n shift
      then Ascale (e, 1 lsl shift), n lsl shift
      else default
    | (Asymbol _ | Aadd (_, _) | Ascale (_, _) | Ascaledadd (_, _, _)), _ ->
      default)
  | Cmm.Cop (Cmuli, [arg; Cconst_int (((2 | 4 | 8) as mult), _)], _)
  | Cmm.Cop (Cmuli, [Cconst_int (((2 | 4 | 8) as mult), _); arg], _) -> (
    let default = Ascale (arg, mult), 0 in
    match select_addr arg with
    | Alinear e, n ->
      if Misc.no_overflow_mul n mult
      then Ascale (e, mult), n * mult
      else default
    | (Asymbol _ | Aadd (_, _) | Ascale (_, _) | Ascaledadd (_, _, _)), _ ->
      default)
  | Cmm.Cop ((Caddi | Caddv | Cadda), [arg1; arg2], _) -> (
    match select_addr arg1, select_addr arg2 with
    | (Alinear e1, n1), (Alinear e2, n2) when Misc.no_overflow_add n1 n2 ->
      Aadd (e1, e2), n1 + n2
    | (Alinear e1, n1), (Ascale (e2, scale), n2)
    | (Ascale (e2, scale), n2), (Alinear e1, n1)
      when Misc.no_overflow_add n1 n2 ->
      Ascaledadd (e1, e2, scale), n1 + n2
    | _, (Ascale (e2, scale), n2) -> Ascaledadd (arg1, e2, scale), n2
    | (Ascale (e1, scale), n1), _ -> Ascaledadd (arg2, e1, scale), n1
    | ( (Alinear _, _),
        ((Alinear _ | Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _) )
    | ( ((Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _),
        ((Asymbol _ | Alinear _ | Aadd (_, _) | Ascaledadd (_, _, _)), _) ) ->
      Aadd (arg1, arg2), 0)
  | _ -> default

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg Int 0

let rcx = phys_reg Int 5

let rdx = phys_reg Int 4

let _xmm0v () = phys_reg Vec128 100

let pseudoregs_for_operation op arg res =
  match (op : Mach.operation) with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
  | Iintop (Iadd | Isub | Imul | Iand | Ior | Ixor)
  | Ifloatop ((Float32 | Float64), (Iaddf | Isubf | Imulf | Idivf)) ->
    [| res.(0); arg.(1) |], res
  | Iintop_atomic { op = Compare_and_swap; size = _; addr = _ } ->
    (* first arg must be rax *)
    let arg = Array.copy arg in
    arg.(0) <- rax;
    arg, res
  | Iintop_atomic { op = Fetch_and_add; size = _; addr = _ } ->
    (* first arg must be the same as res.(0) *)
    let arg = Array.copy arg in
    arg.(0) <- res.(0);
    arg, res
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  | Iintop_imm ((Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr), _)
  | Ifloatop ((Float64 | Float32), (Iabsf | Inegf))
  | Ispecific (Ibswap { bitwidth = Thirtytwo | Sixtyfour }) ->
    res, res
  (* For xchg, args must be a register allowing access to high 8 bit register
     (rax, rbx, rcx or rdx). Keep it simple, just force the argument in rax. *)
  | Ispecific (Ibswap { bitwidth = Sixteen }) -> [| rax |], [| rax |]
  (* For imulh, first arg must be in rax, rax is clobbered, and result is in
     rdx. *)
  | Iintop (Imulh _) -> [| rax; arg.(1) |], [| rdx |]
  | Ispecific (Ifloatarithmem (_, _, _)) ->
    let arg' = Array.copy arg in
    arg'.(0) <- res.(0);
    arg', res
  (* For shifts with variable shift count, second arg must be in rcx *)
  | Iintop (Ilsl | Ilsr | Iasr) -> [| res.(0); rcx |], res
  (* For div and mod, first arg must be in rax, rdx is clobbered, and result is
     in rax or rdx respectively. Keep it simple, just force second argument in
     rcx. *)
  | Iintop Idiv -> [| rax; rcx |], [| rax |]
  | Iintop Imod -> [| rax; rcx |], [| rdx |]
  | Ifloatop (Float64, Icompf cond) ->
    (* CR gyorsh: make this optimization as a separate PR. *)
    (* We need to temporarily store the result of the comparison in a float
       register, but we don't want to clobber any of the inputs if they would
       still be live after this operation -- so we add a fresh register as both
       an input and output. We don't use [destroyed_at_oper], because that
       forces us to choose a fixed register, which makes it more likely an extra
       mov would be added to transfer the argument to the fixed register. *)
    let treg = Reg.create Float in
    let _, is_swapped = float_cond_and_need_swap cond in
    ( (if is_swapped then [| arg.(0); treg |] else [| treg; arg.(1) |]),
      [| res.(0); treg |] )
  | Ifloatop (Float32, Icompf cond) ->
    let treg = Reg.create Float32 in
    let _, is_swapped = float_cond_and_need_swap cond in
    ( (if is_swapped then [| arg.(0); treg |] else [| treg; arg.(1) |]),
      [| res.(0); treg |] )
  | Ispecific Irdpmc ->
    (* For rdpmc instruction, the argument must be in ecx and the result is in
       edx (high) and eax (low). Make it simple and force the argument in rcx,
       and rax and rdx clobbered *)
    [| rcx |], res
  | Ispecific (Isimd op) -> Simd_selection.pseudoregs_for_operation op arg res
  | Icsel _ ->
    (* last arg must be the same as res.(0) *)
    let len = Array.length arg in
    let arg = Array.copy arg in
    arg.(len - 1) <- res.(0);
    arg, res
  (* Other instructions are regular *)
  | Iintop (Ipopcnt | Iclz _ | Ictz _ | Icomp _)
  | Iintop_imm ((Imulh _ | Idiv | Imod | Icomp _ | Ipopcnt | Iclz _ | Ictz _), _)
  | Ispecific
      ( Isextend32 | Izextend32 | Ilea _
      | Istore_int (_, _, _)
      | Ipause | Ilfence | Isfence | Imfence
      | Ioffset_loc (_, _)
      | Irdtsc | Iprefetch _ )
  | Imove | Ispill | Ireload | Ireinterpret_cast _ | Istatic_cast _
  | Iconst_int _ | Iconst_float32 _ | Iconst_float _ | Iconst_vec128 _
  | Iconst_symbol _ | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Iload _
  | Istore (_, _, _)
  | Ialloc _ | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion | Ipoll _ | Idls_get ->
    raise Use_default

let select_locality (l : Cmm.prefetch_temporal_locality_hint) :
    Arch.prefetch_temporal_locality_hint =
  match l with
  | Nonlocal -> Nonlocal
  | Low -> Low
  | Moderate -> Moderate
  | High -> High

let select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth = function
  | Sixteen -> Sixteen
  | Thirtytwo -> Thirtytwo
  | Sixtyfour -> Sixtyfour

let one_arg name args =
  match args with
  | [arg] -> arg
  | _ -> Misc.fatal_errorf "Selection: expected exactly 1 argument for %s" name

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops = ["sqrt"]

let is_immediate n = n <= 0x7FFF_FFFF && n >= -0x8000_0000

let is_immediate_natint n = n <= 0x7FFF_FFFFn && n >= -0x8000_0000n
