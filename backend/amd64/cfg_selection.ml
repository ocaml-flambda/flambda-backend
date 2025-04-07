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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-40-41-42"]

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
  | Cmm.Cop (Cor, [arg; Cconst_int (1, _)], _)
  | Cmm.Cop (Cor, [Cconst_int (1, _); arg], _) -> (
    (* optimize tagging integers *)
    match select_addr arg with
    | Ascale (e, scale), off when scale mod 2 = 0 ->
      Ascale (e, scale), off lor 1
    | _ -> default)
  | _ -> default

(* Special constraints on operand and result registers *)

exception Use_default_exn

let rax = phys_reg Int 0

let rcx = phys_reg Int 5

let rdx = phys_reg Int 4

let _xmm0v () = phys_reg Vec128 100

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

let int_is_immediate n = n <= 0x7FFF_FFFF && n >= -0x8000_0000

let is_immediate_natint n =
  Nativeint.compare n 0x7FFF_FFFFn <= 0
  && Nativeint.compare n (-0x8000_0000n) >= 0

let specific x : Cfg.basic = Op (Specific x)

let pseudoregs_for_operation op arg res =
  match (op : Operation.t) with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
  | Intop (Iadd | Isub | Imul | Iand | Ior | Ixor)
  | Floatop ((Float32 | Float64), (Iaddf | Isubf | Imulf | Idivf)) ->
    [| res.(0); arg.(1) |], res
  | Intop_atomic { op = Compare_set; size = _; addr = _ } ->
    (* first arg must be rax *)
    let arg = Array.copy arg in
    arg.(0) <- rax;
    arg, res
  | Intop_atomic { op = Compare_exchange; size = _; addr = _ } ->
    (* first arg must be rax, res.(0) must be rax. *)
    let arg = Array.copy arg in
    arg.(0) <- rax;
    arg, [| rax |]
  | Intop_atomic { op = Exchange | Fetch_and_add; size = _; addr = _ } ->
    (* first arg must be the same as res.(0) *)
    let arg = Array.copy arg in
    arg.(0) <- res.(0);
    arg, res
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  | Intop_imm ((Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr), _)
  | Floatop ((Float64 | Float32), (Iabsf | Inegf))
  | Specific (Ibswap { bitwidth = Thirtytwo | Sixtyfour }) ->
    res, res
  (* For xchg, args must be a register allowing access to high 8 bit register
     (rax, rbx, rcx or rdx). Keep it simple, just force the argument in rax. *)
  | Specific (Ibswap { bitwidth = Sixteen }) -> [| rax |], [| rax |]
  (* For imulh, first arg must be in rax, rax is clobbered, and result is in
     rdx. *)
  | Intop (Imulh _) -> [| rax; arg.(1) |], [| rdx |]
  | Specific (Ifloatarithmem (_, _, _)) ->
    let arg' = Array.copy arg in
    arg'.(0) <- res.(0);
    arg', res
  (* For shifts with variable shift count, second arg must be in rcx *)
  | Intop (Ilsl | Ilsr | Iasr) -> [| res.(0); rcx |], res
  (* For div and mod, first arg must be in rax, rdx is clobbered, and result is
     in rax or rdx respectively. Keep it simple, just force second argument in
     rcx. *)
  | Intop Idiv -> [| rax; rcx |], [| rax |]
  | Intop Imod -> [| rax; rcx |], [| rdx |]
  | Floatop (Float64, Icompf cond) ->
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
  | Floatop (Float32, Icompf cond) ->
    let treg = Reg.create Float32 in
    let _, is_swapped = float_cond_and_need_swap cond in
    ( (if is_swapped then [| arg.(0); treg |] else [| treg; arg.(1) |]),
      [| res.(0); treg |] )
  | Specific Irdpmc ->
    (* For rdpmc instruction, the argument must be in ecx and the result is in
       edx (high) and eax (low). Make it simple and force the argument in rcx,
       and rax and rdx clobbered *)
    [| rcx |], res
  | Specific (Isimd op) ->
    Simd_selection.pseudoregs_for_operation
      (Simd_proc.register_behavior op)
      arg res
  | Specific (Isimd_mem (op, _addr)) ->
    Simd_selection.pseudoregs_for_operation
      (Simd_proc.Mem.register_behavior op)
      arg res
  | Csel _ ->
    (* last arg must be the same as res.(0) *)
    let len = Array.length arg in
    let arg = Array.copy arg in
    arg.(len - 1) <- res.(0);
    arg, res
  (* Other instructions are regular *)
  | Intop_atomic { op = Add | Sub | Land | Lor | Lxor; _ }
  | Intop (Ipopcnt | Iclz _ | Ictz _ | Icomp _)
  | Intop_imm ((Imulh _ | Idiv | Imod | Icomp _ | Ipopcnt | Iclz _ | Ictz _), _)
  | Specific
      ( Isextend32 | Izextend32 | Ilea _
      | Istore_int (_, _, _)
      | Ipause | Ilfence | Isfence | Imfence
      | Ioffset_loc (_, _)
      | Irdtsc | Icldemote _ | Iprefetch _ )
  | Move | Spill | Reload | Reinterpret_cast _ | Static_cast _ | Const_int _
  | Const_float32 _ | Const_float _ | Const_vec128 _ | Const_symbol _
  | Stackoffset _ | Load _
  | Store (_, _, _)
  | Alloc _ | Name_for_debugger _ | Probe_is_enabled _ | Opaque | Begin_region
  | End_region | Poll | Dls_get ->
    raise Use_default_exn

let is_immediate (op : Operation.integer_operation) n :
    Cfg_selectgen_target_intf.is_immediate_result =
  match op with
  | Iadd | Isub | Imul | Iand | Ior | Ixor | Icomp _ ->
    Is_immediate (int_is_immediate n)
  | _ -> Use_default

let is_immediate_test _cmp n : Cfg_selectgen_target_intf.is_immediate_result =
  Is_immediate (int_is_immediate n)

let is_simple_expr (expr : Cmm.expression) :
    Cfg_selectgen_target_intf.is_simple_expr_result =
  match expr with
  | Cop (Cextcall { func = fn; _ }, args, _) when List.mem fn inline_ops ->
    (* inlined ops are simple if their arguments are *)
    Simple_if_all_expressions_are args
  | _ -> Use_default

let effects_of (expr : Cmm.expression) :
    Cfg_selectgen_target_intf.effects_of_result =
  match expr with
  | Cop (Cextcall { func = fn; _ }, args, _) when List.mem fn inline_ops ->
    Effects_of_all_expressions args
  | _ -> Use_default

let select_addressing (_chunk : Cmm.memory_chunk) exp :
    addressing_mode * Cmm.expression =
  let a, d = select_addr exp in
  (* PR#4625: displacement must be a signed 32-bit immediate *)
  if not (int_is_immediate d)
  then Iindexed 0, exp
  else
    match a with
    | Asymbol s ->
      let glob : Arch.sym_global =
        match s.sym_global with Global -> Global | Local -> Local
      in
      Ibased (s.sym_name, glob, d), Ctuple []
    | Alinear e -> Iindexed d, e
    | Aadd (e1, e2) -> Iindexed2 d, Ctuple [e1; e2]
    | Ascale (e, scale) -> Iscaled (scale, d), e
    | Ascaledadd (e1, e2, scale) -> Iindexed2scaled (scale, d), Ctuple [e1; e2]

let select_store ~is_assign addr (exp : Cmm.expression) :
    Cfg_selectgen_target_intf.select_store_result =
  match exp with
  | Cconst_int (n, _dbg) when int_is_immediate n ->
    Rewritten
      (Specific (Istore_int (Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | Cconst_natint (n, _dbg) when is_immediate_natint n ->
    Rewritten (Specific (Istore_int (n, addr, is_assign)), Ctuple [])
  | Cconst_int _ | Cconst_vec128 _
  | Cconst_natint (_, _)
  | Cconst_float32 (_, _)
  | Cconst_float (_, _)
  | Cconst_symbol (_, _)
  | Cvar _
  | Clet (_, _, _)
  | Cphantom_let (_, _, _)
  | Ctuple _
  | Cop (_, _, _)
  | Csequence (_, _)
  | Cifthenelse (_, _, _, _, _, _)
  | Cswitch (_, _, _, _)
  | Ccatch (_, _, _)
  | Cexit (_, _, _) ->
    Use_default

let is_store_out_of_range _chunk ~byte_offset:_ :
    Cfg_selectgen_target_intf.is_store_out_of_range_result =
  Within_range

let insert_move_extcall_arg _exttype _src _dst :
    Cfg_selectgen_target_intf.insert_move_extcall_arg_result =
  Use_default

(* Recognize float arithmetic with mem *)

let select_floatarith commutative width (regular_op : Operation.float_operation)
    mem_op args : Cfg_selectgen_target_intf.select_operation_result =
  let open Cmm in
  match width, args with
  | Float64, [arg1; Cop (Cload { memory_chunk = Double as chunk; _ }, [loc2], _)]
  | ( Float32,
      [ arg1;
        Cop
          ( Cload { memory_chunk = Single { reg = Float32 } as chunk; _ },
            [loc2],
            _ ) ] ) ->
    let addr, arg2 = select_addressing chunk loc2 in
    Rewritten (specific (Ifloatarithmem (width, mem_op, addr)), [arg1; arg2])
  | Float64, [Cop (Cload { memory_chunk = Double as chunk; _ }, [loc1], _); arg2]
  | ( Float32,
      [ Cop
          ( Cload { memory_chunk = Single { reg = Float32 } as chunk; _ },
            [loc1],
            _ );
        arg2 ] )
    when commutative ->
    let addr, arg1 = select_addressing chunk loc1 in
    Rewritten (specific (Ifloatarithmem (width, mem_op, addr)), [arg2; arg1])
  | _, [arg1; arg2] ->
    Rewritten (Basic (Op (Floatop (width, regular_op))), [arg1; arg2])
  | _ -> assert false

let select_operation
    ~(generic_select_condition :
       Cmm.expression -> Operation.test * Cmm.expression) (op : Cmm.operation)
    (args : Cmm.expression list) dbg ~label_after:_ :
    Cfg_selectgen_target_intf.select_operation_result =
  match op with
  (* Recognize the LEA instruction *)
  | Caddi | Caddv | Cadda | Csubi | Cor -> (
    match select_addressing Word_int (Cop (op, args, dbg)) with
    | Iindexed _, _ | Iindexed2 0, _ -> Use_default
    | ((Iindexed2 _ | Iscaled _ | Iindexed2scaled _ | Ibased _) as addr), arg ->
      Rewritten (specific (Ilea addr), [arg]))
  (* Recognize float arithmetic with memory. *)
  | Caddf width -> select_floatarith true width Iaddf Ifloatadd args
  | Csubf width -> select_floatarith false width Isubf Ifloatsub args
  | Cmulf width -> select_floatarith true width Imulf Ifloatmul args
  | Cdivf width -> select_floatarith false width Idivf Ifloatdiv args
  | Cpackf32 ->
    (* We must operate on registers. This is because if the second argument was
       a float stack slot, the resulting UNPCKLPS instruction would enforce the
       validity of loading it as a 128-bit memory location, even though it only
       loads 64 bits. *)
    Rewritten (specific (Isimd (SSE Interleave_low_32_regs)), args)
  (* Special cases overriding C implementations (regardless of [@@builtin]). *)
  | Cextcall { func = "sqrt" as func; _ }
  (* x86 intrinsics ([@@builtin]) *)
  | Cextcall { func; builtin = true; _ } -> (
    match func with
    | "caml_rdtsc_unboxed" -> Rewritten (specific Irdtsc, args)
    | "caml_rdpmc_unboxed" -> Rewritten (specific Irdpmc, args)
    | "caml_pause_hint" -> Rewritten (specific Ipause, args)
    | "caml_load_fence" -> Rewritten (specific Ilfence, args)
    | "caml_store_fence" -> Rewritten (specific Isfence, args)
    | "caml_memory_fence" -> Rewritten (specific Imfence, args)
    | "caml_cldemote" ->
      let addr, eloc = select_addressing Word_int (one_arg "cldemote" args) in
      Rewritten (specific (Icldemote addr), [eloc])
    | _ -> (
      match Simd_selection.select_operation_cfg func args with
      | Some (op, args) -> Rewritten (Basic (Op op), args)
      | None -> Use_default))
  (* Recognize store instructions *)
  | Cstore (((Word_int | Word_val) as chunk), _init) -> (
    match args with
    | [loc; Cop (Caddi, [Cop (Cload _, [loc'], _); Cconst_int (n, _dbg)], _)]
      when Stdlib.( = ) loc loc' && int_is_immediate n ->
      let addr, arg = select_addressing chunk loc in
      Rewritten (specific (Ioffset_loc (n, addr)), [arg])
    | _ -> Use_default)
  | Cbswap { bitwidth } ->
    let bitwidth = select_bitwidth bitwidth in
    Rewritten (specific (Ibswap { bitwidth }), args)
  | Casr -> (
    (* Recognize sign extension *)
    match args with
    | [Cop (Clsl, [k; Cconst_int (32, _)], _); Cconst_int (32, _)] ->
      Rewritten (specific Isextend32, [k])
    | _ -> Use_default)
  (* Recognize zero extension *)
  | Clsr -> (
    match args with
    | [Cop (Clsl, [k; Cconst_int (32, _)], _); Cconst_int (32, _)] ->
      Rewritten (specific Izextend32, [k])
    | _ -> Use_default)
  | Cand -> (
    match args with
    | [arg; Cconst_int (0xffff_ffff, _)]
    | [arg; Cconst_natint (0xffff_ffffn, _)]
    | [Cconst_int (0xffff_ffff, _); arg]
    | [Cconst_natint (0xffff_ffffn, _); arg] ->
      Rewritten (specific Izextend32, [arg])
    | _ -> Use_default)
  | Ccsel _ -> (
    match args with
    | [cond; ifso; ifnot] -> (
      let cond, earg = generic_select_condition cond in
      match cond with
      | Ifloattest (w, CFeq) ->
        (* CFeq cannot be represented as cmov without a jump. CFneq emits cmov
           for "unordered" and "not equal" cases. Use Cneq and swap the
           arguments. *)
        Rewritten
          (Basic (Op (Csel (Ifloattest (w, CFneq)))), [earg; ifnot; ifso])
      | _ -> Rewritten (Basic (Op (Csel cond)), [earg; ifso; ifnot]))
    | _ -> Use_default)
  | Cprefetch { is_write; locality } ->
    (* Emit prefetch for read hint when prefetchw is not supported. Matches the
       behavior of gcc's __builtin_prefetch *)
    let is_write =
      if is_write && not (Arch.Extension.enabled PREFETCHW)
      then false
      else is_write
    in
    let locality : Arch.prefetch_temporal_locality_hint =
      match select_locality locality with
      | Moderate when is_write && not (Arch.Extension.enabled PREFETCHWT1) ->
        High
      | l -> l
    in
    let addr, eloc = select_addressing Word_int (one_arg "prefetch" args) in
    Rewritten (specific (Iprefetch { is_write; addr; locality }), [eloc])
  | _ -> Use_default

(* Deal with register constraints *)

let insert_op_debug env sub_cfg op dbg rs rd :
    Cfg_selectgen_target_intf.insert_op_debug_result =
  try
    let rsrc, rdst = pseudoregs_for_operation op rs rd in
    Select_utils.insert_moves env sub_cfg rs rsrc;
    Select_utils.insert_debug env sub_cfg (Op op) dbg rsrc rdst;
    Select_utils.insert_moves env sub_cfg rdst rd;
    Regs rd
  with Use_default_exn -> Use_default
