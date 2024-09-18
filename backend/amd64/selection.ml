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

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Arch
open Selection_utils

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

(* The selector class *)

class selector =
  object (self)
    inherit Selectgen.selector_generic as super

    method! is_immediate op n =
      match op with
      | Iadd | Isub | Imul | Iand | Ior | Ixor | Icomp _ -> is_immediate n
      | _ -> super#is_immediate op n

    method is_immediate_test _cmp n = is_immediate n

    method! is_simple_expr e =
      match e with
      | Cop (Cextcall { func = fn }, args, _) when List.mem fn inline_ops ->
        (* inlined ops are simple if their arguments are *)
        List.for_all self#is_simple_expr args
      | _ -> super#is_simple_expr e

    method! effects_of e =
      match e with
      | Cop (Cextcall { func = fn }, args, _) when List.mem fn inline_ops ->
        Select_utils.Effect_and_coeffect.join_list_map args self#effects_of
      | _ -> super#effects_of e

    method select_addressing _chunk exp =
      let a, d = select_addr exp in
      (* PR#4625: displacement must be a signed 32-bit immediate *)
      if not (is_immediate d)
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
        | Ascaledadd (e1, e2, scale) ->
          Iindexed2scaled (scale, d), Ctuple [e1; e2]

    method! select_store is_assign addr exp =
      match exp with
      | Cconst_int (n, _dbg) when is_immediate n ->
        Ispecific (Istore_int (Nativeint.of_int n, addr, is_assign)), Ctuple []
      | Cconst_natint (n, _dbg) when is_immediate_natint n ->
        Ispecific (Istore_int (n, addr, is_assign)), Ctuple []
      | Cconst_int _ | Cconst_vec128 _
      | Cconst_natint (_, _)
      | Cconst_float32 (_, _)
      | Cconst_float (_, _)
      | Cconst_symbol (_, _)
      | Cvar _
      | Clet (_, _, _)
      | Clet_mut (_, _, _, _)
      | Cphantom_let (_, _, _)
      | Cassign (_, _)
      | Ctuple _
      | Cop (_, _, _)
      | Csequence (_, _)
      | Cifthenelse (_, _, _, _, _, _, _)
      | Cswitch (_, _, _, _, _)
      | Ccatch (_, _, _, _)
      | Cexit (_, _, _)
      | Ctrywith (_, _, _, _, _, _) ->
        super#select_store is_assign addr exp

    method! select_operation op args dbg =
      match op with
      (* Recognize the LEA instruction *)
      | Caddi | Caddv | Cadda | Csubi -> (
        match self#select_addressing Word_int (Cop (op, args, dbg)) with
        | Iindexed _, _ | Iindexed2 0, _ -> super#select_operation op args dbg
        | ( ((Iindexed2 _ | Iscaled _ | Iindexed2scaled _ | Ibased _) as addr),
            arg ) ->
          Ispecific (Ilea addr), [arg])
      (* Recognize float arithmetic with memory. *)
      | Caddf width ->
        self#select_floatarith true width Mach.Iaddf Arch.Ifloatadd args
      | Csubf width ->
        self#select_floatarith false width Mach.Isubf Arch.Ifloatsub args
      | Cmulf width ->
        self#select_floatarith true width Mach.Imulf Arch.Ifloatmul args
      | Cdivf width ->
        self#select_floatarith false width Mach.Idivf Arch.Ifloatdiv args
      | Cpackf32 ->
        (* We must operate on registers. This is because if the second argument
           was a float stack slot, the resulting UNPCKLPS instruction would
           enforce the validity of loading it as a 128-bit memory location, even
           though it only loads 64 bits. *)
        Ispecific (Isimd (SSE Interleave_low_32_regs)), args
      (* Special cases overriding C implementations (regardless of
         [@@builtin]). *)
      | Cextcall { func = "sqrt" as func; _ }
      (* x86 intrinsics ([@@builtin]) *)
      | Cextcall { func; builtin = true; _ } -> (
        match func with
        | "caml_rdtsc_unboxed" -> Ispecific Irdtsc, args
        | "caml_rdpmc_unboxed" -> Ispecific Irdpmc, args
        | "caml_pause_hint" -> Ispecific Ipause, args
        | "caml_load_fence" -> Ispecific Ilfence, args
        | "caml_store_fence" -> Ispecific Isfence, args
        | "caml_memory_fence" -> Ispecific Imfence, args
        | _ -> (
          match Simd_selection.select_operation func args with
          | Some (op, args) -> op, args
          | None -> super#select_operation op args dbg))
      (* Recognize store instructions *)
      | Cstore (((Word_int | Word_val) as chunk), _init) -> (
        match args with
        | [loc; Cop (Caddi, [Cop (Cload _, [loc'], _); Cconst_int (n, _dbg)], _)]
          when loc = loc' && is_immediate n ->
          let addr, arg = self#select_addressing chunk loc in
          Ispecific (Ioffset_loc (n, addr)), [arg]
        | _ -> super#select_operation op args dbg)
      | Cbswap { bitwidth } ->
        let bitwidth = select_bitwidth bitwidth in
        Ispecific (Ibswap { bitwidth }), args
      (* Recognize sign extension *)
      | Casr -> (
        match args with
        | [Cop (Clsl, [k; Cconst_int (32, _)], _); Cconst_int (32, _)] ->
          Ispecific Isextend32, [k]
        | _ -> super#select_operation op args dbg)
      (* Recognize zero extension *)
      | Cand -> (
        match args with
        | [arg; Cconst_int (0xffff_ffff, _)]
        | [arg; Cconst_natint (0xffff_ffffn, _)]
        | [Cconst_int (0xffff_ffff, _); arg]
        | [Cconst_natint (0xffff_ffffn, _); arg] ->
          Ispecific Izextend32, [arg]
        | _ -> super#select_operation op args dbg)
      | Ccsel _ -> (
        match args with
        | [cond; ifso; ifnot] -> (
          let cond, earg = self#select_condition cond in
          match cond with
          | Ifloattest (w, CFeq) ->
            (* CFeq cannot be represented as cmov without a jump. CFneq emits
               cmov for "unordered" and "not equal" cases. Use Cneq and swap the
               arguments. *)
            Icsel (Ifloattest (w, CFneq)), [earg; ifnot; ifso]
          | _ -> Icsel cond, [earg; ifso; ifnot])
        | _ -> super#select_operation op args dbg)
      | Cprefetch { is_write; locality } ->
        (* Emit prefetch for read hint when prefetchw is not supported. Matches
           the behavior of gcc's __builtin_prefetch *)
        let is_write =
          if is_write && not (Arch.Extension.enabled PREFETCHW)
          then false
          else is_write
        in
        let locality : Arch.prefetch_temporal_locality_hint =
          match select_locality locality with
          | Moderate when is_write && not (Arch.Extension.enabled PREFETCHWT1)
            ->
            High
          | l -> l
        in
        let addr, eloc =
          self#select_addressing Word_int (one_arg "prefetch" args)
        in
        Ispecific (Iprefetch { is_write; addr; locality }), [eloc]
      | _ -> super#select_operation op args dbg

    (* Recognize float arithmetic with mem *)

    method select_floatarith commutative width regular_op mem_op args =
      let open Cmm in
      match width, args with
      | ( Float64,
          [arg1; Cop (Cload { memory_chunk = Double as chunk; _ }, [loc2], _)] )
      | ( Float32,
          [ arg1;
            Cop
              ( Cload { memory_chunk = Single { reg = Float32 } as chunk; _ },
                [loc2],
                _ ) ] ) ->
        let addr, arg2 = self#select_addressing chunk loc2 in
        Mach.Ispecific (Ifloatarithmem (width, mem_op, addr)), [arg1; arg2]
      | ( Float64,
          [Cop (Cload { memory_chunk = Double as chunk; _ }, [loc1], _); arg2] )
      | ( Float32,
          [ Cop
              ( Cload { memory_chunk = Single { reg = Float32 } as chunk; _ },
                [loc1],
                _ );
            arg2 ] )
        when commutative ->
        let addr, arg1 = self#select_addressing chunk loc1 in
        Mach.Ispecific (Ifloatarithmem (width, mem_op, addr)), [arg2; arg1]
      | _, [arg1; arg2] -> Mach.Ifloatop (width, regular_op), [arg1; arg2]
      | _ -> assert false

    method! mark_c_tailcall = contains_calls := true

    (* Deal with register constraints *)

    method! insert_op_debug env op dbg rs rd =
      try
        let rsrc, rdst = pseudoregs_for_operation op rs rd in
        self#insert_moves env rs rsrc;
        self#insert_debug env (Iop op) dbg rsrc rdst;
        self#insert_moves env rdst rd;
        rd
      with Use_default -> super#insert_op_debug env op dbg rs rd
  end

let fundecl ~future_funcnames f =
  (new selector)#emit_fundecl ~future_funcnames f
