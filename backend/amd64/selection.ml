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

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of Cmm.symbol
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  let default = (Alinear exp, 0) in
  match exp with
    Cconst_symbol (s, _) when not !Clflags.dlcode ->
      (Asymbol s, 0)
  | Cop((Caddi | Caddv | Cadda), [arg; Cconst_int (m, _)], _)
  | Cop((Caddi | Caddv | Cadda), [Cconst_int (m, _); arg], _) ->
      let (a, n) = select_addr arg in
      if Misc.no_overflow_add n m then a, n + m else default
  | Cop(Csubi, [arg; Cconst_int (m, _)], _) ->
      let (a, n) = select_addr arg in
      if Misc.no_overflow_sub n m then (a, n - m) else default
  | Cop(Clsl, [arg; Cconst_int((1|2|3 as shift), _)], _) ->
      let default = (Ascale (arg, 1 lsl shift), 0) in
      begin match select_addr arg with
        (Alinear e, n) ->
        if Misc.no_overflow_lsl n shift
        then (Ascale(e, 1 lsl shift), n lsl shift)
        else default
      | ((Asymbol _ | Aadd (_, _) | Ascale (_,_) | Ascaledadd (_, _, _)), _)
        -> default
      end
  | Cop(Cmuli, [arg; Cconst_int((2|4|8 as mult), _)], _)
  | Cop(Cmuli, [Cconst_int((2|4|8 as mult), _); arg], _) ->
      let default = (Ascale (arg, mult), 0) in
      begin match select_addr arg with
        (Alinear e, n) ->
        if Misc.no_overflow_mul n mult
        then (Ascale(e, mult), n * mult)
        else default
      | ((Asymbol _ | Aadd (_, _) | Ascale (_,_) | Ascaledadd (_, _, _)), _)
        -> default
      end
  | Cop((Caddi | Caddv | Cadda), [arg1; arg2], _) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2))
          when Misc.no_overflow_add n1 n2 ->
              (Aadd(e1, e2), n1 + n2)
        | (((Alinear e1, n1), (Ascale(e2, scale), n2)) |
           ((Ascale(e2, scale), n2), (Alinear e1, n1)))
          when Misc.no_overflow_add n1 n2 ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | ((Alinear _, _),
           ((Alinear _ | Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _))
        | (((Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _),
           ((Asymbol _ | Alinear _ | Aadd (_, _) | Ascaledadd (_, _, _)), _))
          ->
              (Aadd(arg1, arg2), 0)
      end
  | _ -> default

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg Int 0
let rcx = phys_reg Int 5
let rdx = phys_reg Int 4
let _xmm0v () = phys_reg Vec128 100

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
  | Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor)
  | Ifloatop((Float32|Float64), (Iaddf|Isubf|Imulf|Idivf)) ->
      ([|res.(0); arg.(1)|], res)
  | Iintop_atomic {op = Compare_and_swap; size = _; addr = _} ->
      (* first arg must be rax *)
      let arg = Array.copy arg in
      arg.(0) <- rax;
      (arg, res)
  | Iintop_atomic {op = Fetch_and_add; size = _; addr = _} ->
      (* first arg must be the same as res.(0) *)
      let arg = Array.copy arg in
      arg.(0) <- res.(0);
      (arg, res)
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  | Iintop_imm((Iadd|Isub|Imul|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr), _)
  | Ifloatop((Float64 | Float32), (Iabsf | Inegf))
  | Ispecific(Ibswap { bitwidth = (Thirtytwo | Sixtyfour) }) ->
      (res, res)
  (* For xchg, args must be a register allowing access to high 8 bit register
     (rax, rbx, rcx or rdx). Keep it simple, just force the argument in rax. *)
  | Ispecific(Ibswap { bitwidth = Sixteen }) ->
      ([| rax |], [| rax |])
  (* For imulh, first arg must be in rax, rax is clobbered, and result is in
     rdx. *)
  | Iintop(Imulh _) ->
      ([| rax; arg.(1) |], [| rdx |])
  | Ispecific(Ifloatarithmem(_,_,_)) ->
      let arg' = Array.copy arg in
      arg'.(0) <- res.(0);
      (arg', res)
  (* For shifts with variable shift count, second arg must be in rcx *)
  | Iintop(Ilsl|Ilsr|Iasr) ->
      ([|res.(0); rcx|], res)
  (* For div and mod, first arg must be in rax, rdx is clobbered,
     and result is in rax or rdx respectively.
     Keep it simple, just force second argument in rcx. *)
  | Iintop(Idiv) ->
      ([| rax; rcx |], [| rax |])
  | Iintop(Imod) ->
      ([| rax; rcx |], [| rdx |])
  | Ifloatop(Float64, Icompf cond) ->
    (* CR gyorsh: make this optimization as a separate PR. *)
      (* We need to temporarily store the result of the comparison in a
         float register, but we don't want to clobber any of the inputs
         if they would still be live after this operation -- so we
         add a fresh register as both an input and output. We don't use
         [destroyed_at_oper], because that forces us to choose a fixed
         register, which makes it more likely an extra mov would be added
         to transfer the argument to the fixed register. *)
      let treg = Reg.create Float in
      let _,is_swapped = float_cond_and_need_swap cond in
      (if is_swapped then [| arg.(0); treg |] else [| treg; arg.(1) |])
    , [| res.(0); treg |]
  | Ifloatop(Float32, Icompf cond) ->
      let treg = Reg.create Float32 in
      let _,is_swapped = float_cond_and_need_swap cond in
      (if is_swapped then [| arg.(0); treg |] else [| treg; arg.(1) |])
    , [| res.(0); treg |]
  | Ispecific Irdpmc ->
  (* For rdpmc instruction, the argument must be in ecx
     and the result is in edx (high) and eax (low).
     Make it simple and force the argument in rcx, and rax and rdx clobbered *)
    ([| rcx |], res)
  | Ispecific (Isimd op) -> Simd_selection.pseudoregs_for_operation op arg res
  | Icsel _ ->
    (* last arg must be the same as res.(0) *)
    let len = Array.length arg in
    let arg = Array.copy arg in
    arg.(len-1) <- res.(0);
    (arg, res)
  (* Other instructions are regular *)
  | Iintop (Ipopcnt|Iclz _|Ictz _|Icomp _)
  | Iintop_imm ((Imulh _|Idiv|Imod|Icomp _
                |Ipopcnt|Iclz _|Ictz _), _)
  | Ispecific (Isextend32|Izextend32|Ilea _|Istore_int (_, _, _)
              |Ipause|Ilfence|Isfence|Imfence
              |Ioffset_loc (_, _)|Irdtsc|Iprefetch _)
  | Imove|Ispill|Ireload
  | Ireinterpret_cast _ | Istatic_cast _
  | Iconst_int _|Iconst_float32 _|Iconst_float _|Iconst_vec128 _
  | Iconst_symbol _|Icall_ind|Icall_imm _|Itailcall_ind|Itailcall_imm _
  | Iextcall _|Istackoffset _|Iload _ | Istore (_, _, _)|Ialloc _
  | Iname_for_debugger _|Iprobe _|Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion | Ipoll _ | Idls_get
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

method! is_immediate op n =
  match op with
  | Iadd | Isub | Imul | Iand | Ior | Ixor | Icomp _  ->
      is_immediate n
  | _ ->
      super#is_immediate op n

method is_immediate_test _cmp n = is_immediate n

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
  then (Iindexed 0, exp)
  else match a with
    | Asymbol s ->
        let glob : Arch.sym_global =
          match s.sym_global with Global -> Global | Local -> Local in
        (Ibased(s.sym_name, glob, d), Ctuple [])
    | Alinear e ->
        (Iindexed d, e)
    | Aadd(e1, e2) ->
        (Iindexed2 d, Ctuple[e1; e2])
    | Ascale(e, scale) ->
        (Iscaled(scale, d), e)
    | Ascaledadd(e1, e2, scale) ->
        (Iindexed2scaled(scale, d), Ctuple[e1; e2])

method! select_store is_assign addr exp =
  match exp with
    Cconst_int (n, _dbg) when is_immediate n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | (Cconst_natint (n, _dbg)) when is_immediate_natint n ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | Cconst_int _ | Cconst_vec128 _
  | Cconst_natint (_, _) | Cconst_float32 (_, _) | Cconst_float (_, _)
  | Cconst_symbol (_, _) | Cvar _ | Clet (_, _, _) | Clet_mut (_, _, _, _)
  | Cphantom_let (_, _, _) | Cassign (_, _) | Ctuple _ | Cop (_, _, _)
  | Csequence (_, _) | Cifthenelse (_, _, _, _, _, _, _)
  | Cswitch (_, _, _, _, _) | Ccatch (_, _, _, _) | Cexit (_, _, _)
  | Ctrywith (_, _, _, _, _, _)
    ->
      super#select_store is_assign addr exp

method! select_operation op args dbg =
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Caddv | Cadda | Csubi ->
      begin match self#select_addressing Word_int (Cop(op, args, dbg)) with
        (Iindexed _, _)
      | (Iindexed2 0, _) -> super#select_operation op args dbg
      | ((Iindexed2 _ | Iscaled _ | Iindexed2scaled _ | Ibased _) as addr,
         arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize float arithmetic with memory. *)
  | Caddf width ->
      self#select_floatarith true width Iaddf Ifloatadd args
  | Csubf width ->
      self#select_floatarith false width Isubf Ifloatsub args
  | Cmulf width ->
      self#select_floatarith true width Imulf Ifloatmul args
  | Cdivf width ->
      self#select_floatarith false width Idivf Ifloatdiv args
  | Cpackf32 ->
      (* We must operate on registers. This is because if the second argument
         was a float stack slot, the resulting UNPCKLPS instruction would
         enforce the validity of loading it as a 128-bit memory location,
         even though it only loads 64 bits. *)
      Ispecific (Isimd (SSE Interleave_low_32_regs)), args
  (* Special cases overriding C implementations (regardless of [@@builtin]). *)
  | Cextcall { func = ("sqrt" as func); _ }
  (* x86 intrinsics ([@@builtin]) *)
  | Cextcall { func; builtin = true; _ } ->
      begin match func with
      | "caml_rdtsc_unboxed" -> Ispecific Irdtsc, args
      | "caml_rdpmc_unboxed" -> Ispecific Irdpmc, args
      | "caml_pause_hint" -> Ispecific Ipause, args
      | "caml_load_fence" -> Ispecific Ilfence, args
      | "caml_store_fence" -> Ispecific Isfence, args
      | "caml_memory_fence" -> Ispecific Imfence, args
      | _ ->
        (match Simd_selection.select_operation func args with
         | Some (op, args) -> op, args
         | None -> super#select_operation op args dbg)
      end
  (* Recognize store instructions *)
  | Cstore ((Word_int|Word_val as chunk), _init) ->
      begin match args with
        [loc; Cop(Caddi, [Cop(Cload _, [loc'], _); Cconst_int (n, _dbg)], _)]
        when loc = loc' && is_immediate n ->
          let (addr, arg) = self#select_addressing chunk loc in
          (Ispecific(Ioffset_loc(n, addr)), [arg])
      | _ ->
          super#select_operation op args dbg
      end
  | Cbswap { bitwidth } ->
    let bitwidth = select_bitwidth bitwidth in
    (Ispecific (Ibswap { bitwidth }), args)
  (* Recognize sign extension *)
  | Casr ->
      begin match args with
        [Cop(Clsl, [k; Cconst_int (32, _)], _); Cconst_int (32, _)] ->
          (Ispecific Isextend32, [k])
        | _ -> super#select_operation op args dbg
      end
  (* Recognize zero extension *)
  | Cand ->
    begin match args with
    | [arg; Cconst_int (0xffff_ffff, _)]
    | [arg; Cconst_natint (0xffff_ffffn, _)]
    | [Cconst_int (0xffff_ffff, _); arg]
    | [Cconst_natint (0xffff_ffffn, _); arg] ->
      Ispecific Izextend32, [arg]
    | _ -> super#select_operation op args dbg
    end
  | Ccsel _ ->
     begin match args with
     | [cond; ifso; ifnot] ->
       let (cond, earg) = self#select_condition cond in
       (match cond with
        | Ifloattest (w,CFeq) ->
          (* CFeq cannot be represented as cmov without a jump.
             CFneq emits cmov for "unordered" and "not equal" cases.
             Use Cneq and swap the arguments. *)
          Icsel (Ifloattest (w, CFneq)), [ earg; ifnot; ifso ]
        | _ ->
          (Icsel cond, [ earg; ifso; ifnot ]))
     | _ -> super#select_operation op args dbg
     end
  | Cprefetch { is_write; locality; } ->
      (* Emit prefetch for read hint when prefetchw is not supported.
         Matches the behavior of gcc's __builtin_prefetch *)
      let is_write =
        if is_write && not (Arch.Extension.enabled PREFETCHW)
        then false
        else is_write
      in
      let locality : Arch.prefetch_temporal_locality_hint =
        match select_locality locality with
        | Moderate when is_write && not (Arch.Extension.enabled PREFETCHWT1) -> High
        | l -> l
      in
      let addr, eloc =
        self#select_addressing Word_int (one_arg "prefetch" args)
      in
      Ispecific (Iprefetch { is_write; addr; locality; }), [eloc]
  | _ -> super#select_operation op args dbg

(* Recognize float arithmetic with mem *)

method select_floatarith commutative width regular_op mem_op args =
  match width, args with
  | Float64, [arg1; Cop(Cload { memory_chunk = Double as chunk; _ }, [loc2], _)]
  | Float32, [arg1; Cop(Cload { memory_chunk = Single { reg = Float32 } as chunk; _ }, [loc2], _)] ->
      let (addr, arg2) = self#select_addressing chunk loc2 in
      (Ispecific(Ifloatarithmem(width, mem_op, addr)),
                 [arg1; arg2])
  | Float64, [Cop(Cload { memory_chunk = Double as chunk; _}, [loc1], _); arg2]
  | Float32, [Cop(Cload { memory_chunk = Single { reg = Float32 } as chunk; _}, [loc1], _); arg2]
        when commutative ->
      let (addr, arg1) = self#select_addressing chunk loc1 in
      (Ispecific(Ifloatarithmem(width, mem_op, addr)),
                 [arg2; arg1])
  | _, [arg1; arg2] ->
      (Ifloatop (width, regular_op), [arg1; arg2])
  | _ ->
      assert false

method! mark_c_tailcall =
  contains_calls := true

(* Deal with register constraints *)

method! insert_op_debug env op dbg rs rd =
  try
    let (rsrc, rdst) = pseudoregs_for_operation op rs rd in
    self#insert_moves env rs rsrc;
    self#insert_debug env (Iop op) dbg rsrc rdst;
    self#insert_moves env rdst rd;
    rd
  with Use_default ->
    super#insert_op_debug env op dbg rs rd

end

let fundecl ~future_funcnames f =
  (new selector)#emit_fundecl ~future_funcnames f
