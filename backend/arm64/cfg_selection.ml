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

let is_offset chunk n =
  (n >= -256 && n <= 255) (* 9 bits signed unscaled *)
  || n >= 0
     &&
     match (chunk : Cmm.memory_chunk) with
     (* 12 bits unsigned, scaled by chunk size *)
     | Byte_unsigned | Byte_signed -> n < 0x1000
     | Sixteen_unsigned | Sixteen_signed -> n land 1 = 0 && n lsr 1 < 0x1000
     | Thirtytwo_unsigned | Thirtytwo_signed
     | Single { reg = Float64 | Float32 } ->
       n land 3 = 0 && n lsr 2 < 0x1000
     | Word_int | Word_val | Double -> n land 7 = 0 && n lsr 3 < 0x1000
     | Onetwentyeight_aligned | Onetwentyeight_unaligned ->
       n land 15 = 0 && n lsr 4 < 0x1000

let is_logical_immediate_int n = Arch.is_logical_immediate (Nativeint.of_int n)

(* Signed immediates are simpler *)

let is_immediate n =
  let mn = -n in
  n land 0xFFF = n
  || n land 0xFFF_000 = n
  || mn land 0xFFF = mn
  || mn land 0xFFF_000 = mn

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops = ["sqrt"]

let use_direct_addressing _symb = (not !Clflags.dlcode) && not Arch.macosx

let is_stack_slot rv =
  Reg.(match rv with [| { loc = Stack _ } |] -> true | _ -> false)

let select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth = function
  | Sixteen -> Sixteen
  | Thirtytwo -> Thirtytwo
  | Sixtyfour -> Sixtyfour

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

    method! emit_stores env sub_cfg dbg data regs_addr =
      (* Override [emit_stores] to ensure that addressing mode always uses a
         legal offset. *)
      let offset = ref (-Arch.size_int) in
      let base =
        assert (Array.length regs_addr = 1);
        ref regs_addr
      in
      List.iter
        (fun arg ->
          match self#emit_expr env sub_cfg arg ~bound_name:None with
          | Never_returns -> assert false
          | Ok regs ->
            for i = 0 to Array.length regs - 1 do
              let r = regs.(i) in
              let kind : Cmm.memory_chunk =
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
              if not (is_offset kind !offset)
              then (
                (* Use a temporary to store the address [!base + offset]. *)
                let tmp = self#regs_for Cmm.typ_int in
                self#insert_debug env sub_cfg
                  (self#lift_op
                     (self#make_const_int (Nativeint.of_int !offset)))
                  dbg [||] tmp;
                self#insert_debug env sub_cfg
                  (self#lift_op (Operation.Intop Iadd))
                  dbg (Array.append !base tmp) tmp;
                (* Use the temporary as the new base address. *)
                base := tmp;
                offset := 0);
              self#insert_debug env sub_cfg
                (self#make_store kind (Iindexed !offset) false)
                dbg
                (Array.append [| r |] !base)
                [||];
              offset := !offset + Select_utils.size_component r.Reg.typ
            done)
        data

    method! insert_move_extcall_arg env sub_cfg ty_arg src dst =
      let ty_arg_is_int32 =
        match ty_arg with
        | XInt32 -> true
        | XInt | XInt64 | XFloat32 | XFloat | XVec128 -> false
      in
      if macosx && ty_arg_is_int32 && is_stack_slot dst
      then self#insert env sub_cfg (Op (Specific Imove32)) src dst
      else self#insert_moves env sub_cfg src dst
  end

let fundecl ~future_funcnames f =
  Cfg.reset_instr_id ();
  (new selector)#emit_fundecl ~future_funcnames f
