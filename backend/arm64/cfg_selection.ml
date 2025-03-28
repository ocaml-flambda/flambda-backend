(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the ARM processor *)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Arch
open Cmm
open Selection_utils

let specific x ~label_after =
  if Arch.operation_can_raise x
  then
    Cfg_selectgen.Terminator
      (Cfg.Specific_can_raise { Cfg.op = x; label_after })
  else Cfg_selectgen.Basic Cfg.(Op (Specific x))

class selector =
  object (self)
    inherit Cfg_selectgen.selector_generic as super

    method is_immediate_test _cmp n = is_immediate n

    method! is_immediate op n =
      match op with
      | Iadd | Isub -> n <= 0xFFF_FFF && n >= -0xFFF_FFF
      | Iand | Ior | Ixor -> is_logical_immediate_int n
      | Icomp _ -> is_immediate n
      | _ -> super#is_immediate op n

    method! is_simple_expr =
      function
      (* inlined floating-point ops are simple if their arguments are *)
      | Cop (Cextcall { func }, args, _) when List.mem func inline_ops ->
        List.for_all self#is_simple_expr args
      | e -> super#is_simple_expr e

    method! effects_of e =
      match e with
      | Cop (Cextcall { func }, args, _) when List.mem func inline_ops ->
        Select_utils.Effect_and_coeffect.join_list_map args self#effects_of
      | e -> super#effects_of e

    method select_addressing chunk =
      function
      | Cop ((Caddv | Cadda), [Cconst_symbol (s, _); Cconst_int (n, _)], _)
        when use_direct_addressing s ->
        Ibased (s.sym_name, n), Ctuple []
      | Cop ((Caddv | Cadda), [arg; Cconst_int (n, _)], _)
        when is_offset chunk n ->
        Iindexed n, arg
      | Cop
          ( ((Caddv | Cadda) as op),
            [arg1; Cop (Caddi, [arg2; Cconst_int (n, _)], _)],
            dbg )
        when is_offset chunk n ->
        Iindexed n, Cop (op, [arg1; arg2], dbg)
      | Cconst_symbol (s, _) when use_direct_addressing s ->
        Ibased (s.sym_name, 0), Ctuple []
      | arg -> Iindexed 0, arg

    method! select_operation op args dbg ~label_after =
      let[@inline] specific op = specific op ~label_after in
      match op with
      (* Integer addition *)
      | Caddi | Caddv | Cadda -> (
        match args with
        (* Shift-add *)
        | [arg1; Cop (Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64
          ->
          specific (Ishiftarith (Ishiftadd, n)), [arg1; arg2]
        | [arg1; Cop (Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64
          ->
          specific (Ishiftarith (Ishiftadd, -n)), [arg1; arg2]
        | [Cop (Clsl, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64
          ->
          specific (Ishiftarith (Ishiftadd, n)), [arg2; arg1]
        | [Cop (Casr, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64
          ->
          specific (Ishiftarith (Ishiftadd, -n)), [arg2; arg1]
        (* Multiply-add *)
        | [arg1; Cop (Cmuli, args2, dbg)] | [Cop (Cmuli, args2, dbg); arg1] -> (
          match self#select_operation Cmuli args2 dbg ~label_after with
          | Basic (Op (Intop_imm (Ilsl, l))), [arg3] ->
            specific (Ishiftarith (Ishiftadd, l)), [arg1; arg3]
          | Basic (Op (Intop Imul)), [arg3; arg4] ->
            specific Imuladd, [arg3; arg4; arg1]
          | _ -> super#select_operation op args dbg ~label_after)
        | _ -> super#select_operation op args dbg ~label_after)
      (* Integer subtraction *)
      | Csubi -> (
        match args with
        (* Shift-sub *)
        | [arg1; Cop (Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64
          ->
          specific (Ishiftarith (Ishiftsub, n)), [arg1; arg2]
        | [arg1; Cop (Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64
          ->
          specific (Ishiftarith (Ishiftsub, -n)), [arg1; arg2]
        (* Multiply-sub *)
        | [arg1; Cop (Cmuli, args2, dbg)] -> (
          match self#select_operation Cmuli args2 dbg ~label_after with
          | Basic (Op (Intop_imm (Ilsl, l))), [arg3] ->
            specific (Ishiftarith (Ishiftsub, l)), [arg1; arg3]
          | Basic (Op (Intop Imul)), [arg3; arg4] ->
            specific Imulsub, [arg3; arg4; arg1]
          | _ -> super#select_operation op args dbg ~label_after)
        | _ -> super#select_operation op args dbg ~label_after)
      (* Recognize sign extension *)
      | Casr -> (
        match args with
        | [Cop (Clsl, [k; Cconst_int (n, _)], _); Cconst_int (n', _)]
          when n' = n && 0 < n && n < 64 ->
          specific (Isignext (64 - n)), [k]
        | _ -> super#select_operation op args dbg ~label_after)
      (* Use trivial addressing mode for atomic loads *)
      | Cload { memory_chunk; mutability; is_atomic = true } ->
        ( Basic
            (Op
               (Load
                  { memory_chunk;
                    addressing_mode = Iindexed 0;
                    mutability = Select_utils.select_mutable_flag mutability;
                    is_atomic = true
                  })),
          args )
      (* Recognize floating-point negate and multiply *)
      | Cnegf Float64 -> (
        match args with
        | [Cop (Cmulf Float64, args, _)] -> specific Inegmulf, args
        | _ -> super#select_operation op args dbg ~label_after)
      (* Recognize floating-point multiply and add/sub *)
      | Caddf Float64 -> (
        match args with
        | [arg; Cop (Cmulf Float64, args, _)]
        | [Cop (Cmulf Float64, args, _); arg] ->
          specific Imuladdf, arg :: args
        | _ -> super#select_operation op args dbg ~label_after)
      | Csubf Float64 -> (
        match args with
        | [arg; Cop (Cmulf Float64, args, _)] -> specific Imulsubf, arg :: args
        | [Cop (Cmulf Float64, args, _); arg] ->
          specific Inegmulsubf, arg :: args
        | _ -> super#select_operation op args dbg ~label_after)
      | Cpackf32 -> specific (Isimd Zip1_f32), args
      (* Recognize floating-point square root *)
      | Cextcall { func = "sqrt" | "sqrtf" | "caml_neon_float64_sqrt" } ->
        specific Isqrtf, args
      | Cextcall { func; builtin = true; _ } -> (
        match Simd_selection.select_operation_cfg func args with
        | Some (op, args) -> Basic (Op op), args
        | None -> super#select_operation op args dbg ~label_after)
      (* Recognize bswap instructions *)
      | Cbswap { bitwidth } ->
        let bitwidth = select_bitwidth bitwidth in
        specific (Ibswap { bitwidth }), args
      (* Other operations are regular *)
      | _ -> super#select_operation op args dbg ~label_after

    method! emit_stores env dbg data regs_addr =
      (* Override [emit_stores] to ensure that addressing mode always uses a
         legal offset. *)
      let offset = ref (-Arch.size_int) in
      let base =
        assert (Array.length regs_addr = 1);
        ref regs_addr
      in
      List.iter
        (fun arg ->
          match self#emit_expr env arg ~bound_name:None with
          | None -> assert false
          | Some regs ->
            for i = 0 to Array.length regs - 1 do
              let r = regs.(i) in
              let kind =
                match r.Reg.typ with
                | Float -> Double
                | Float32 -> Single { reg = Float32 }
                | Vec128 ->
                  (* 128-bit memory operations are default unaligned. Aligned
                     (big)array operations are handled separately via cmm. *)
                  Onetwentyeight_unaligned
                | Val | Addr | Int -> Word_val
                | Valx2 ->
                  Misc.fatal_error "Unexpected machtype_component Valx2"
              in
              if not (Selection_utils.is_offset kind !offset)
              then (
                (* Use a temporary to store the address [!base + offset]. *)
                let tmp = self#regs_for typ_int in
                self#insert_debug env
                  (self#lift_op
                     (self#make_const_int (Nativeint.of_int !offset)))
                  dbg [||] tmp;
                self#insert_debug env
                  (self#lift_op (Operation.Intop Iadd))
                  dbg (Array.append !base tmp) tmp;
                (* Use the temporary as the new base address. *)
                base := tmp;
                offset := 0);
              self#insert_debug env
                (self#make_store kind (Iindexed !offset) false)
                dbg
                (Array.append [| r |] !base)
                [||];
              offset := !offset + Select_utils.size_component r.Reg.typ
            done)
        data

    method! insert_move_extcall_arg env ty_arg src dst =
      let ty_arg_is_int32 =
        match ty_arg with
        | XInt32 -> true
        | XInt | XInt64 | XFloat32 | XFloat | XVec128 -> false
      in
      if macosx && ty_arg_is_int32 && is_stack_slot dst
      then self#insert env (Op (Specific Imove32)) src dst
      else self#insert_moves env src dst
  end

let fundecl ~future_funcnames f =
  Cfg.reset_instr_id ();
  (new selector)#emit_fundecl ~future_funcnames f
