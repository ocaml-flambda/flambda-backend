# 2 "backend/amd64/reload.ml"
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

open Cmm
open Reg
open Mach

(* Reloading for the AMD64 *)

(* Summary of instruction set constraints:
   "S" means either stack or register, "R" means register only.
   Operation                    Res     Arg1    Arg2
     Imove                      R       S
                             or S       R
     Iconst_int                 S if 32-bit signed, R otherwise
     Iconst_float               R
     Iconst_symbol (not PIC)    S
     Iconst_symbol (PIC)        R
     Icall_ind                          R
     Itailcall_ind                      R
     Iload                      R       R       R
     Istore                             R       R
     Iintop(Icomp)              R       R       S
                            or  R       S       R
     Iintop(Imul|Idiv|Imod)     R       R       S
     Iintop(Imulh)              R       R       S
     Iintop(shift)              S       S       R
     Iintop(others)             R       R       S
                            or  S       S       R
     Iintop_imm(Iadd, n)/lea    R       R
     Iintop_imm(Imul, n)        R       R
     Iintop_imm(Icomp _)        R       S
     Iintop_imm(others)         S       S
     Iintop(Ipopcnt|Iclz|Ictz)  R       S
     Inegf...Idivf              R       R       S
     Ifloatofint                R       S
     Iintoffloat                R       S
     Ispecific(Ilea)            R       R       R
     Ispecific(Ifloatarithmem)  R       R       R
     Ispecific(Icrc32q)         R       R       S   (and Res = Arg1)
     Ispecific(Irdtsc)          R
     Ispecific(Irdpmc)          R       R           (Arg1 = rcx)
     Ispecific(Ifloat_iround)   R       S
     Ispecific(Ifloat_round _)  R       S
     Ispecific(Ifloat_min)      R       R       S   (and Res = Arg1)
     Ispecific(Ifloat_max)      R       R       S   (and Res = Arg1)

   Conditional branches:
     Iinttest                           S       R
                                    or  R       S
     Ifloattest                         R       S    (or  S R if swapped test)
     other tests                        S
*)

let stackp r =
  match r.loc with
    Stack _ -> true
  | Reg _ | Unknown -> false

class reload = object (self)

inherit Reloadgen.reload_generic as super

method private one_stack arg =
  if stackp arg.(0) && stackp arg.(1)
  then [|arg.(0); self#makereg arg.(1)|]
  else arg

method private one_mem_or_stack operands =
  (* First operand must be Ireg *)
  let r = Mach.arg_reg operands.(0) in
  if Reg.is_stack r then
    match operands.(1) with
    | Ireg r' when Reg.is_stack r' ->
      [| operands.(0); Ireg (self#makereg r') |]
    | Ireg _ | Iimm _ -> operands
    | Iimmf _  (* float immediate is implemented as a memory load *)
    | Imem _ ->
      (* CR gyorsh: this is different, previously we would always
         force operands.(1), but not that it can be mem, we have to force
         operands.(0) which is already forced to be the same as res.(0). *)
      [| Ireg (self#makereg r); operands.(1) |]
  else
    operands

(* First argument (= result) must be in register, second arg
         can reside in the stack or memory or immediate *)
method private same_reg_res0_arg0 res operands =
  match operands.(0) with
  | Ireg r when Reg.is_stack r ->
    let r' = self#makereg r in
    operands.(0) <- Ireg r';
    (operands, [|r'|])
  | Ireg _ | Iimm _ | Iimmf _ | Imem _ ->
    (operands, res)

method! reload_operation op arg res =
  let arg = self#makeregs_for_memory_operands arg in
  match op with
  | Iintop(Iadd) when (not (Reg.same_loc (Mach.arg_reg arg.(0)) res.(0)))
                      && Mach.is_immediate arg.(1) ->
      (* This add will be turned into a lea; args and results must be
         in registers *)
      super#reload_operation op arg res
  | Iintop(Iadd|Isub|Iand|Ior|Ixor|Icheckbound) ->
      (* One of the two arguments can reside in the stack or memory, but not both *)
      (self#one_mem_or_stack arg, res)
  | Iintop (Icomp _) ->
      (* One of the two arguments can reside in the stack, but not both.
         The result must be in a register. *)
      let res =
        if stackp res.(0) then [| self#makereg res.(0) |] else res
      in
      (self#one_mem_or_stack arg, res)
  | Ispecific Ifloat_iround
  | Ispecific (Ifloat_round _) ->
      (* The argument(s) can be either in register or on stack.
         The result must be in a register. *)
      let res =
        if stackp res.(0) then [| self#makereg res.(0) |] else res
      in
      (arg, res)
  | Iintop(Imulh _ | Idiv | Imod | Ilsl | Ilsr | Iasr) ->
      (* The argument(s) and results can be either in register or on stack *)
      (* Note: Imulh, Idiv, Imod: arg(0) and res(0) already forced in regs
               Ilsl, Ilsr, Iasr: arg(1) already forced in regs *)
      (arg, res)
  | Iintop(Imul) | Ifloatop (Iaddf | Isubf | Imulf | Idivf) ->
      (* First argument (= result) must be in register, second arg
         can reside in the stack *)
      self#same_reg_res0_arg0 res arg
  | Ispecific (Irdtsc | Irdpmc) ->
      (* Irdtsc: result must be in register.
         Irdpmc: result must be in register, arg.(0) already forced in reg. *)
      if stackp res.(0)
      then (let r = self#makereg res.(0) in (arg, [|r|]))
      else (arg, res)
  | Ispecific(Ifloat_min | Ifloat_max)
  | Ispecific Icrc32q ->
    (* First argument and result must be in the same register.
       Second argument can be either in a register or on stack. *)
      self#same_reg_res0_arg0 res arg
  | Ifloatop(Icompf _) ->
    (* First argument is forced to be the same as the second result,
       and it must be in register. *)
      if stackp res.(1)
      then (let r = self#makereg res.(1) in
            ([|Ireg r; arg.(1)|], [|res.(0); r|]))
      else (arg, res)
  | Ifloatofint | Iintoffloat ->
      (* Result must be in register, but argument can be on stack *)
      (arg, (if stackp res.(0) then [| self#makereg res.(0) |] else res))
  | Iconst_int n ->
      if n <= 0x7FFFFFFFn && n >= -0x80000000n
      then (arg, res)
      else super#reload_operation op arg res
  | Iconst_symbol _ ->
      if !Clflags.pic_code || !Clflags.dlcode || Arch.win64
      then super#reload_operation op arg res
      else (arg, res)
  | Iintop (Ipopcnt | Iclz _| Ictz _) ->
      (* Result must be in register, but argument can be on stack *)
      (arg, (if stackp res.(0) then [| self#makereg res.(0) |] else res))
  | Ispecific  (Isqrtf | Isextend32 | Izextend32 | Ilea
               | Ioffset_loc | Ipause
               | Iprefetch _
               | Ibswap _)
  | Imove|Ispill|Ireload|Ifloatop(Inegf|Iabsf)
  | Iconst_float _|Icall_ind|Icall_imm _
  | Itailcall_ind|Itailcall_imm _|Iextcall _|Istackoffset _|Iload (_, _)
  | Istore  _|Ialloc _|Iname_for_debugger _|Iprobe _|Iprobe_is_enabled _
  | Iopaque
  | Ibeginregion | Iendregion
    -> (* Other operations: all args and results in registers *)
      super#reload_operation op arg res

method! reload_test tst operands =
  let operands = self#makeregs_for_memory_operands operands in
  match tst with
  | Iinttest _ ->
      (* One of the two arguments can reside in memory or on stack *)
      self#one_mem_or_stack operands
  | Ifloattest (CFlt | CFnlt | CFle | CFnle | CFeq
               | CFneq | CFgt | CFngt | CFge | CFnge) ->
      (* Second argument can be on stack, first must be in register *)
      operands.(0) <- self#makereg_operand operands.(0);
      operands
  | Itruetest
  | Ifalsetest
  | Ioddtest
  | Ieventest ->
      (* The argument(s) can be either in register or on stack *)
      operands

end

let fundecl f num_stack_slots =
  (new reload)#fundecl f num_stack_slots
