# 2 "backend/amd64/selection.ml"
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
open Cmm
open Mach

module O = Selectgen.Operands

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  match exp with
    Cconst_symbol (s, _) when not !Clflags.dlcode ->
      (Asymbol s, 0)
  | Cop((Caddi | Caddv | Cadda), [arg; Cconst_int (m, _)], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Csubi, [arg; Cconst_int (m, _)], _) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Caddv | Cadda), [Cconst_int (m, _); arg], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int((1|2|3 as shift), _)], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | ((Asymbol _ | Aadd (_, _) | Ascale (_,_) | Ascaledadd (_, _, _)), _)
        -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int((2|4|8 as mult), _)], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | ((Asymbol _ | Aadd (_, _) | Ascale (_,_) | Ascaledadd (_, _, _)), _)
        -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int((2|4|8 as mult), _); arg], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | ((Asymbol _ | Aadd (_, _) | Ascale (_,_) | Ascaledadd (_, _, _)), _)
        -> (Alinear exp, 0)
      end
  | Cop((Caddi | Caddv | Cadda), [arg1; arg2], _) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2)) ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | ((Ascale(e1, scale), n1), (Alinear e2, n2)) ->
              (Ascaledadd(e2, e1, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | ((Alinear _, _),
           ((Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _))
        | (((Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _),
           ((Asymbol _ | Alinear _ | Aadd (_, _) | Ascaledadd (_, _, _)), _))
          ->
              (Aadd(arg1, arg2), 0)
      end
  | arg ->
      (Alinear arg, 0)

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg 0
let rcx = phys_reg 5
let rdx = phys_reg 4

(* Make arg.(0) the same as res.(0) *)
let make_same_reg_res0_arg0 arg res =
  let arg' = Array.copy arg in
  arg'.(0) <- Ireg res.(0);
  (arg', res)

let pseudoregs_for_operation op arg res  =
  match op with
  (* arg.(0) and res.(0) must be the same *)
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor)
  | Ifloatop(Iaddf|Isubf|Imulf|Idivf) ->
      make_same_reg_res0_arg0 arg res
  | Ifloatop(Iabsf | Inegf)
  | Ispecific(Ibswap { bitwidth = (Thirtytwo | Sixtyfour) }) ->
     (* CR gyorsh: this is not equivalent to the previously used [(res, res)],
        where physically the same arrays are passed. *)
     make_same_reg_res0_arg0 arg res
  (* For xchg, args must be a register allowing access to high 8 bit register
     (rax, rbx, rcx or rdx). Keep it simple, just force the argument in rax. *)
  | Ispecific(Ibswap { bitwidth = Sixteen }) ->
      ([| Ireg rax |], [| rax |])
  (* For imulh, first arg must be in rax, rax is clobbered, and result is in
     rdx. *)
  | Iintop(Imulh _) ->
      ([| Ireg rax; arg.(1) |], [| rdx |])
  | Iintop(Ilsl|Ilsr|Iasr) ->
     (* arg.(0) and res.(0) must be the same *)
     if Mach.is_immediate arg.(1) then
       make_same_reg_res0_arg0 arg res
     else
       (* For shifts with variable shift count, second arg must be in rcx *)
       ([| Ireg res.(0); Ireg rcx|], res)
  (* For div and mod, first arg must be in rax, rdx is clobbered,
     and result is in rax or rdx respectively.
     Keep it simple, just force second argument in rcx. *)
  | Iintop(Idiv) ->
      ([| Ireg rax; Ireg rcx |], [| rax |])
  | Iintop(Imod) ->
      ([| Ireg rax; Ireg rcx |], [| rdx |])
  | Ifloatop(Icompf cond) ->
      (* We need to temporarily store the result of the comparison in a
         float register, but we don't want to clobber any of the inputs
         if they would still be live after this operation -- so we
         add a fresh register as both an input and output. We don't use
         [destroyed_at_oper], because that forces us to choose a fixed
         register, which makes it more likely an extra mov would be added
         to transfer the argument to the fixed register. *)
      (* Icompf is emitted as the sequence:
         cmpsd treg a; movd treg res.(0); neg res.(0) *)
      (* For cmpsd instruction, the result must be in register
         and the first argument must be in the same register.
         Second argument can in register or stack or memory. *)
      let treg = Reg.create Float in
      [| Ireg treg; arg.(1) |], [| res.(0); treg |]
  | Ispecific Irdpmc ->
  (* For rdpmc instruction, the argument must be in ecx
     and the result is in edx (high) and eax (low).
     Make it simple and force the argument in rcx, and rax and rdx clobbered *)
    ([| Ireg rcx |], res)
  | Ispecific (Ifloat_min | Ifloat_max)
  | Ispecific Icrc32q ->
    (* arg.(0) and res.(0) must be the same *)
    ([| Ireg res.(0); arg.(1)|], res)
  (* Other instructions are regular *)
  | Iintop (Ipopcnt|Iclz _|Ictz _|Icomp _|Icheckbound)
  | Ispecific (Isqrtf|Isextend32|Izextend32|Ilea
              |Ifloat_iround|Ifloat_round _
              |Ioffset_loc|Ipause|Irdtsc|Iprefetch _)
  | Imove|Ispill|Ireload|Ifloatofint|Iintoffloat|Iconst_int _|Iconst_float _
  | Iconst_symbol _|Icall_ind|Icall_imm _|Itailcall_ind|Itailcall_imm _
  | Iextcall _|Istackoffset _|Iload (_, _)|Istore _|Ialloc _
  | Iname_for_debugger _|Iprobe _|Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion
    -> raise Use_default

let select_locality (l : Cmm.prefetch_temporal_locality_hint)
  : Arch.prefetch_temporal_locality_hint =
  match l with
  | Nonlocal -> Nonlocal
  | Low -> Low
  | Moderate -> Moderate
  | High -> High

let select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth =
  function
  | Sixteen -> Sixteen
  | Thirtytwo -> Thirtytwo
  | Sixtyfour -> Sixtyfour

let one_arg name args =
  match args with
  | [arg] -> arg
  | _ ->
    Misc.fatal_errorf "Selection: expected exactly 1 argument for %s" name

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops =
  [ "sqrt"; ]

let is_immediate n = n <= 0x7FFF_FFFF && n >= -0x8000_0000

let is_immediate_natint n = n <= 0x7FFF_FFFFn && n >= -0x8000_0000n

(* The selector class *)

class selector = object (self)

inherit Selectgen.selector_generic as super

method! is_immediate_int op n =
  match op with
  | Iintop (Iadd | Isub | Imul | Iand | Ior | Ixor | Icomp _ | Icheckbound) ->
      is_immediate n
  | _ ->
      super#is_immediate_int op n

method! is_immediate_float op f =
  match op with
  | Ifloatop (Iaddf | Isubf | Imulf | Idivf) -> f <> +0.0
  | _ -> false

method is_immediate_test_int _cmp n = is_immediate n

method is_immediate_test_float cmp f =
  match cmp with
  | Ifloattest _ -> f <> +0.0
  | _ -> false

method! memory_operands_supported op chunk =
  match op, chunk with
  | Ifloatop (Iaddf | Isubf | Imulf |Idivf), Double -> true
  | Ispecific Isqrtf, Double -> true
  | Iintop _, (Word_int | Word_val) -> true
  | Iintop _, _ ->
    (* The value loaded from memory needs to be extended before use in Iintop  *)
    false
  | Ifloatop (Icompf _ ), Double -> true
  | Ifloatop (Inegf | Iabsf), _ -> false
  | Ifloatofint, (Word_int | Word_val) -> true
  | Iintoffloat, Double -> true
  | (Ifloatop _ | Ispecific _ | Ifloatofint | Iintoffloat), _ -> false
  | ((Imove | Ispill | Ireload | Icall_ind | Itailcall_ind | Iopaque
     | Iconst_int _ | Iconst_float _ | Iconst_symbol _ | Icall_imm _
     | Itailcall_imm _ |Iextcall _ | Istackoffset _
     | Iload (_, _) | Istore _ | Ialloc _ | Iname_for_debugger _
     | Iprobe _|Iprobe_is_enabled _), _) -> assert false

method! memory_operands_supported_condition (op : Mach.test) chunk =
  match op, chunk with
  | (Iinttest (Isigned _ | Iunsigned _)
    | Ioddtest | Ieventest | Itruetest | Ifalsetest),
    (Word_int | Word_val) -> true
  | (Iinttest (Isigned _ | Iunsigned _)
    | Ioddtest | Ieventest | Itruetest | Ifalsetest),
    (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
    | Thirtytwo_unsigned | Thirtytwo_signed) -> false
  | (Iinttest (Isigned _ | Iunsigned _)
    | Ioddtest | Ieventest | Itruetest | Ifalsetest),_ ->
    Misc.fatal_errorf "memory_operands_condition inttest with chunk=%s"
      (Printcmm.chunk chunk) ()
  | (Ifloattest cmp, Double) -> true
  | Ifloattest cmp, _ ->
    Misc.fatal_errorf "memory_operands_condition floattest with chunk=%s"
      (Printcmm.chunk chunk) ()

method! is_simple_expr e =
  match e with
  | Cop(Cextcall { func = fn; }, args, _)
    when List.mem fn inline_ops ->
      (* inlined ops are simple if their arguments are *)
      List.for_all self#is_simple_expr args
  | _ ->
      super#is_simple_expr e

method! effects_of e =
  match e with
  | Cop(Cextcall { func = fn; }, args, _)
    when List.mem fn inline_ops ->
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | _ ->
      super#effects_of e

method select_addressing _chunk exp =
  let (a, d) = select_addr exp in
  (* PR#4625: displacement must be a signed 32-bit immediate *)
  if not (is_immediate d)
  then (Iindexed 0, exp, 0)
  else match a with
    | Asymbol s ->
        (Ibased(s, d), Ctuple [], 0)
    | Alinear e ->
        (Iindexed d, e, 1)
    | Aadd(e1, e2) ->
        (Iindexed2 d, Ctuple[e1; e2], 2)
    | Ascale(e, scale) ->
        (Iscaled(scale, d), e, 1)
    | Ascaledadd(e1, e2, scale) ->
        (Iindexed2scaled(scale, d), Ctuple[e1; e2], 2)

method! select_store is_assign chunk addr len exp =
  let chunk_for_imm =
    match chunk with
    | Some (Word_int | Word_val) -> true
    | Some (Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Single | Double)
    | None -> false
  in
  let mk_mem n =
    O.(selected [| imm n; mem (Some Word_int) addr ~len ~index:0 |]) in
  match exp with
    Cconst_int (n, _dbg) when chunk_for_imm && is_immediate n ->
    (Istore is_assign, Ctuple [], mk_mem (Targetint.of_int n))
  | (Cconst_natint (n, _dbg)) when chunk_for_imm && is_immediate_natint n ->
    (* CR gyorsh: add to Targetint module or
       change Cconst_intnat to take Targetint.t?  *)
    let targetint_of_nativeint n =
      Int64.of_nativeint n |> Targetint.of_int64 in
    (Istore is_assign, Ctuple [], mk_mem (targetint_of_nativeint n))
  | Cconst_int _
  | Cconst_natint (_, _) | Cconst_float (_, _) | Cconst_symbol (_, _)
  | Cvar _ | Clet (_, _, _) | Clet_mut (_, _, _, _) | Cphantom_let (_, _, _)
  | Cassign (_, _) | Ctuple _ | Cop (_, _, _) | Csequence (_, _)
  | Cifthenelse (_, _, _, _, _, _) | Cswitch (_, _, _, _) | Ccatch (_, _, _)
  | Cexit (_, _, _) | Ctrywith (_, _, _, _, _)
  | Cregion _ | Ctail _
    ->
      super#select_store is_assign chunk addr len exp

method select_condition cond =
  match cond with
  | Cop(Ccmpf cmp, ([arg0;arg1] as args), _) ->
     self#select_operands_condition (Ifloattest cmp)
       (if Arch.float_test_need_swap cmp then [arg1;arg0] else args)
  | arg ->
      super#select_condition cond

method! select_operation op args dbg =
  (* CR gyorsh: some operations, like crc32 and roundsd can have memory operands *)
  let in_reg = O.in_registers () in
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Caddv | Cadda | Csubi ->
      begin match self#select_addressing Word_int (Cop(op, args, dbg)) with
        (Iindexed _, _, _)
      | (Iindexed2 0, _, _) -> super#select_operation op args dbg
      | ((Iindexed2 _ | Iscaled _ | Iindexed2scaled _ | Ibased _) as addr,
         arg, len) -> (Ispecific Ilea, [arg],
                       O.(selected [| mem None addr ~len ~index:0 |]))
      end
  (* Recognize float arithmetic with memory. *)
  | Cextcall { func = "sqrt"; alloc = false; } ->
      super#select_operands (Ispecific Isqrtf) args
  | Cextcall { func = "caml_int64_bits_of_float_unboxed"; alloc = false;
               ty = [|Int|]; ty_args = [XFloat] } ->
      (match args with
      | [Cop(Cload (Double, _), [loc], _dbg)] ->
        let c = Word_int in
        let (addr, arg) = self#select_addressing c loc in
        Iload(c, addr), [arg], in_reg
      | _ -> Imove, args, in_reg)
  | Cextcall { func = "caml_int64_float_of_bits_unboxed"; alloc = false;
               ty = [|Float|]; ty_args = [XInt64] } ->
      (match args with
      | [Cop(Cload (Word_int, _), [loc], _dbg)] ->
        let c = Double in
        let (addr, arg) = self#select_addressing c loc in
        Iload(c, addr), [arg], in_reg
      | _ -> Imove, args, in_reg)
  | Cextcall { func; builtin = true; ty = ret; ty_args = _; } ->
      begin match func, ret with
      | "caml_rdtsc_unboxed", [|Int|] -> Ispecific Irdtsc, args, in_reg
      | "caml_rdpmc_unboxed", [|Int|] -> Ispecific Irdpmc, args, in_reg
      | ("caml_int64_crc_unboxed", [|Int|]
        | "caml_int_crc_untagged", [|Int|]) when !Arch.crc32_support ->
          Ispecific Icrc32q, args, in_reg
      | "caml_float_iround_half_to_even_unboxed", [|Int|] ->
         Ispecific Ifloat_iround, args, in_reg
      | "caml_float_round_half_to_even_unboxed", [|Float|] ->
         Ispecific (Ifloat_round Half_to_even), args, in_reg
      | "caml_float_round_up_unboxed", [|Float|] ->
         Ispecific (Ifloat_round Up), args, in_reg
      | "caml_float_round_down_unboxed", [|Float|] ->
         Ispecific (Ifloat_round Down), args, in_reg
      | "caml_float_round_towards_zero_unboxed", [|Float|] ->
         Ispecific (Ifloat_round Towards_zero), args, in_reg
      | "caml_float_round_current_unboxed", [|Float|] ->
         Ispecific (Ifloat_round Current), args, in_reg
      | "caml_float_min_unboxed", [|Float|] ->
         Ispecific Ifloat_min, args, in_reg
      | "caml_float_max_unboxed", [|Float|] ->
         Ispecific Ifloat_max, args, in_reg
      | "caml_pause_hint", [|Val|] ->
         Ispecific Ipause, args, in_reg
      | _ ->
        super#select_operation op args dbg
      end
  (* Recognize store instructions *)
  | Cstore ((Word_int|Word_val as chunk), _init) ->
      begin match args with
        [loc; Cop(Caddi, [Cop(Cload _, [loc'], _); Cconst_int (n, _dbg)], _)]
        when loc = loc' && is_immediate n ->
          let (addr, arg, len) = self#select_addressing chunk loc in
          (Ispecific Ioffset_loc, [arg],
           O.(selected [| mem (Some chunk) addr ~len ~index:0;
                          imm (Targetint.of_int n) |]))
      | _ ->
          super#select_operation op args dbg
      end
  | Cbswap { bitwidth } ->
    let bitwidth = select_bitwidth bitwidth in
    (Ispecific (Ibswap { bitwidth }), args, in_reg)
  (* Recognize sign extension *)
  | Casr ->
      begin match args with
        [Cop(Clsl, [k; Cconst_int (32, _)], _); Cconst_int (32, _)] ->
          (Ispecific Isextend32, [k], in_reg)
        | _ -> super#select_operation op args dbg
      end
  (* Recognize zero extension *)
  | Cand ->
    begin match args with
    | [arg; Cconst_int (0xffff_ffff, _)]
    | [arg; Cconst_natint (0xffff_ffffn, _)]
    | [Cconst_int (0xffff_ffff, _); arg]
    | [Cconst_natint (0xffff_ffffn, _); arg] ->
      Ispecific Izextend32, [arg], in_reg
    | _ -> super#select_operation op args dbg
    end
  | Cprefetch { is_write; locality; } ->
      (* Emit prefetch for read hint when prefetchw is not supported.
         Matches the behavior of gcc's __builtin_prefetch *)
      let is_write =
        if is_write && not !prefetchw_support
        then false
        else is_write
      in
      let locality : Arch.prefetch_temporal_locality_hint =
        match select_locality locality with
        | Moderate when is_write && not !prefetchwt1_support -> High
        | l -> l
      in
      let addr, eloc, len =
        self#select_addressing Word_int (one_arg "prefetch" args)
      in
      Ispecific (Iprefetch { is_write; locality; }), [eloc],
      O.(selected [| mem (Some Word_int) addr ~len ~index:0 |])
  | Ccmpf comp ->
      let _,need_swap = Arch.float_compare_and_need_swap comp in
      let args =
        if need_swap then
          match args with
          | [arg0; arg1] -> [arg1;arg0]
          | _ -> assert false
        else
          args
      in
      self#select_operands (Ifloatop (Icompf comp)) args
  | _ -> super#select_operation op args dbg


method! mark_c_tailcall =
  contains_calls := true

method private insert_moves_operands env src dst =
  let eq_stamp : Reg.t -> Reg.t -> bool =
    fun r1 r2 -> Int.equal r1.stamp r2.stamp
  in
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    match src.(i), dst.(i) with
    | Iimm left, Iimm right when Targetint.equal left right -> ()
    | Iimmf left, Iimmf right when Int64.equal left right -> ()
    | Ireg left, Ireg right when eq_stamp left right -> ()
    | Imem { chunk=left_chunk; addr=left_addr; reg=left_reg },
      Imem { chunk=right_chunk; addr=right_addr; reg=right_reg } when
      Option.equal Cmm.equal_memory_chunk left_chunk right_chunk &&
      Arch.equal_addressing_mode left_addr right_addr &&
      Array.length left_reg = Array.length right_reg &&
      Array.for_all2 eq_stamp left_reg right_reg -> ()
    | (Ireg _ | Iimm _ | Iimmf _) as src, Ireg dst ->
      self#insert env (Iop Imove) [|src|] [|dst|]
    | Imem _, Ireg _ ->
      Misc.fatal_error "Selection.insert_moves_operands: mem to reg"
    | (Iimm _ | Iimmf _ | Ireg _ | Imem _), _ ->
      Misc.fatal_error "Selection.insert_moves_operands"
  done

(* Deal with register constraints *)
method! insert_op_debug env op dbg rs rd =
  try
    let (rsrc, rdst) = pseudoregs_for_operation op rs rd in
    self#insert_moves_operands env rs rsrc;
    self#insert_debug env (Iop op) dbg rsrc rdst;
    self#insert_moves env rdst rd;
    rd
  with Use_default ->
    super#insert_op_debug env op dbg rs rd

end

let fundecl f ~ppf_dump = (new selector)#emit_fundecl f ~ppf_dump
