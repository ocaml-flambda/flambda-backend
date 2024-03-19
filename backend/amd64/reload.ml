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

method! reload_operation op arg res =
  match op with
  | Iintop(Iadd|Isub|Iand|Ior|Ixor) ->
      (* One of the two arguments can reside in the stack, but not both *)
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); self#makereg arg.(1)|], res)
      else (arg, res)
  | Iintop (Icomp _) ->
      (* One of the two arguments can reside in the stack, but not both.
         The result must be in a register. *)
      let res =
        if stackp res.(0) then [| self#makereg res.(0) |] else res
      in
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); self#makereg arg.(1)|], res)
      else (arg, res)
  | Iintop_imm(Iadd, _) when arg.(0).loc <> res.(0).loc ->
      (* This add will be turned into a lea; args and results must be
         in registers *)
      super#reload_operation op arg res
  | Iintop_imm (Imul, _) ->
      (* The result (= the argument) must be a register (#10626) *)
      if stackp arg.(0)
      then let r = self#makereg arg.(0) in ([|r|],[|r|])
      else (arg, res)
  | Iintop_imm (Icomp _, _) ->
      (* The argument(s) can be either in register or on stack.
         The result must be in a register. *)
      let res =
        if stackp res.(0) then [| self#makereg res.(0) |] else res
      in
      arg, res
  | Iintop(Imulh _ | Idiv | Imod | Ilsl | Ilsr | Iasr)
  | Iintop_imm((Iadd | Isub | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
               | Imulh _ | Idiv | Imod ), _) ->
      (* The argument(s) and results can be either in register or on stack *)
      (* Note: Imulh, Idiv, Imod: arg(0) and res(0) already forced in regs
               Ilsl, Ilsr, Iasr: arg(1) already forced in regs *)
      (arg, res)
  | Iintop_imm ((Ipopcnt | Iclz _ | Ictz _), _) -> assert false
  | Iintop(Imul) | Iaddf | Isubf | Imulf | Idivf ->
      (* First argument (= result) must be in register, second arg
         can reside in the stack *)
      if stackp arg.(0)
      then (let r = self#makereg arg.(0) in ([|r; arg.(1)|], [|r|]))
      else (arg, res)
  | Ispecific (Irdtsc | Irdpmc) ->
      (* Irdtsc: result must be in register.
         Irdpmc: result must be in register, arg.(0) already forced in reg. *)
      if stackp res.(0)
      then (let r = self#makereg res.(0) in (arg, [|r|]))
      else (arg, res)
  | Ispecific(Isimd op) -> Simd_reload.reload_operation self#makereg op arg res
  | Iscalarcast (Float_to_int _ | Float_of_int _ |
                 Float_of_float32 | Float_to_float32) ->
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
  | Icsel tst ->
      (* Last argument and result must be in the same register.
         Result must be in register. The last two arguments are used
         for emitting cmov, the remaining for [Mach.test]. *)
    (*  CR gyorsh: we already use Array.sub here,
        so no reason for this convoluted arrangement,
        using the first two args for cmov would simplify most of the
        code as it won't need to have [len], it will be able to have indexes
        directly, but then in Emit we will have to do Array.sub again
        to call emit_test (unless emit_test takes an index, which is also
        weird). *)
        (* CR-soon gyorsh: [reload_test] may lose some sharing
           between the arguments of the test and the last two arguments
           and the result of the move. *)
        let r = if stackp res.(0) then self#makereg res.(0) else res.(0) in
        let len = Array.length arg in
        let arg' = Array.copy arg in
        let test_arg = self#reload_test tst (Array.sub arg 0 (len - 2)) in
        for i = 0 to len - 2 - 1 do
          arg'.(i) <- test_arg.(i)
        done;
        arg'.(len - 1) <- r;
        (arg', [|r|])
  | Iscalarcast (V128_to_scalar (Float64x2) | V128_of_scalar (Float64x2)) ->
    (* These are just moves; either the argument or result may be on the stack. *)
    begin match stackp arg.(0), stackp res.(0) with
    | true, true -> ([| self#makereg arg.(0) |], res)
    | _ -> (arg, res)
    end
  | Iscalarcast (V128_to_scalar (Float32x4) | V128_of_scalar (Float32x4)) ->
    (* These do additional logic requiring the result to be a register.
       CR mslater: (SIMD) replace once we have unboxed float32 *)
    (arg, [| self#makereg res.(0) |])
  | Iscalarcast (V128_of_scalar (Int64x2 | Int32x4 | Int16x8 | Int8x16)) ->
    (* Int -> Vec regs need the result to be a register. *)
    (arg, [| self#makereg res.(0) |])
  | Iscalarcast (V128_to_scalar (Int64x2 | Int32x4)) ->
    (* Vec -> Int regs need the argument to be a register. *)
    ([| self#makereg arg.(0) |], res)
  | Iscalarcast (V128_to_scalar (Int16x8 | Int8x16)) ->
    (* These do additional logic requiring the result to be a register.
       CR mslater: (SIMD) replace once we have unboxed int16/int8 *)
    ([| self#makereg arg.(0) |], [| self#makereg res.(0) |])
  | Iintop (Ipopcnt | Iclz _| Ictz _)
  | Iintop_atomic _
  | Ispecific  (Isextend32 | Izextend32 | Ilea _
               | Istore_int (_, _, _)
               | Ioffset_loc (_, _) | Ifloatarithmem (_, _) | Ifloatsqrtf _
               | Ipause
               | Ilfence | Isfence | Imfence
               | Iprefetch _ | Ibswap _)
  | Imove|Ispill|Ireload|Inegf|Iabsf|Iconst_float32 _|Iconst_float _
  | Iconst_vec128 _|Icall_ind|Icall_imm _
  | Icompf _
  | Itailcall_ind|Itailcall_imm _|Iextcall _|Istackoffset _|Iload _
  | Istore (_, _, _)|Ialloc _|Iname_for_debugger _|Iprobe _|Iprobe_is_enabled _
  | Ivalueofint | Iintofvalue | Iopaque | Ivectorcast _
  | Ibeginregion | Iendregion | Ipoll _ | Idls_get
    -> (* Other operations: all args and results in registers,
          except moves, probes, and vector casts. *)
      super#reload_operation op arg res

method! reload_test tst arg =
  match tst with
    Iinttest _ ->
      (* One of the two arguments can reside on stack *)
      if stackp arg.(0) && stackp arg.(1)
      then [| self#makereg arg.(0); arg.(1) |]
      else arg
  | Ifloattest (CFlt | CFnlt | CFle | CFnle) ->
      (* Cf. emit.mlp: we swap arguments in this case *)
      (* First argument can be on stack, second must be in register *)
      if stackp arg.(1)
      then [| arg.(0); self#makereg arg.(1) |]
      else arg
  | Ifloattest (CFeq | CFneq | CFgt | CFngt | CFge | CFnge) ->
      (* Second argument can be on stack, first must be in register *)
      if stackp arg.(0)
      then [| self#makereg arg.(0); arg.(1) |]
      else arg
  | Iinttest_imm (_, _)
  | Itruetest
  | Ifalsetest
  | Ioddtest
  | Ieventest ->
      (* The argument(s) can be either in register or on stack *)
      arg

end

let fundecl f num_stack_slots =
  (new reload)#fundecl f num_stack_slots
