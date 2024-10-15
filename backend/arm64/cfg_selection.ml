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

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Arch
open Cmm
open Selection_utils

let specific x = Cfg_selectgen.Basic Cfg.(Op (Specific x))

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

    method! select_operation op args dbg =
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
          match self#select_operation Cmuli args2 dbg with
          | Basic (Op (Intop_imm (Ilsl, l))), [arg3] ->
            specific (Ishiftarith (Ishiftadd, l)), [arg1; arg3]
          | Basic (Op (Intop Imul)), [arg3; arg4] ->
            specific Imuladd, [arg3; arg4; arg1]
          | _ -> super#select_operation op args dbg)
        | _ -> super#select_operation op args dbg)
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
          match self#select_operation Cmuli args2 dbg with
          | Basic (Op (Intop_imm (Ilsl, l))), [arg3] ->
            specific (Ishiftarith (Ishiftsub, l)), [arg1; arg3]
          | Basic (Op (Intop Imul)), [arg3; arg4] ->
            specific Imulsub, [arg3; arg4; arg1]
          | _ -> super#select_operation op args dbg)
        | _ -> super#select_operation op args dbg)
      (* Recognize sign extension *)
      | Casr -> (
        match args with
        | [Cop (Clsl, [k; Cconst_int (n, _)], _); Cconst_int (n', _)]
          when n' = n && 0 < n && n < 64 ->
          specific (Isignext (64 - n)), [k]
        | _ -> super#select_operation op args dbg)
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
        | _ -> super#select_operation op args dbg)
      (* Recognize floating-point multiply and add/sub *)
      | Caddf Float64 -> (
        match args with
        | [arg; Cop (Cmulf Float64, args, _)]
        | [Cop (Cmulf Float64, args, _); arg] ->
          specific Imuladdf, arg :: args
        | _ -> super#select_operation op args dbg)
      | Csubf Float64 -> (
        match args with
        | [arg; Cop (Cmulf Float64, args, _)] -> specific Imulsubf, arg :: args
        | [Cop (Cmulf Float64, args, _); arg] ->
          specific Inegmulsubf, arg :: args
        | _ -> super#select_operation op args dbg)
      (* Recognize floating-point square root *)
      | Cextcall { func = "sqrt" } -> specific Isqrtf, args
      (* Recognize bswap instructions *)
      | Cbswap { bitwidth } ->
        let bitwidth = select_bitwidth bitwidth in
        specific (Ibswap { bitwidth }), args
      (* Other operations are regular *)
      | _ -> super#select_operation op args dbg

    method! insert_move_extcall_arg env ty_arg src dst =
      if macosx && ty_arg = XInt32 && is_stack_slot dst
      then self#insert env (Op (Specific Imove32)) src dst
      else self#insert_moves env src dst
  end

let fundecl ~future_funcnames f =
  Cfg_selectgen.reset_next_instr_id ();
  (new selector)#emit_fundecl ~future_funcnames f
