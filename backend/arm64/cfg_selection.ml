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
(*   Copyright 2025 Jane Street Group LLC.                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the ARM processor *)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-40-41-42"]

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

let int_is_immediate n =
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
  Reg.(match rv with [| { loc = Stack _; _ } |] -> true | _ -> false)

let select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth = function
  | Sixteen -> Sixteen
  | Thirtytwo -> Thirtytwo
  | Sixtyfour -> Sixtyfour

let specific x : Cfg.basic = Op (Specific x)

let is_immediate (op : Operation.integer_operation) n :
    Cfg_selectgen_target_intf.is_immediate_result =
  match op with
  | Iadd | Isub -> Is_immediate (n <= 0xFFF_FFF && n >= -0xFFF_FFF)
  | Iand | Ior | Ixor -> Is_immediate (is_logical_immediate_int n)
  | Icomp _ -> Is_immediate (int_is_immediate n)
  | _ -> Use_default

let is_immediate_test _cmp n : Cfg_selectgen_target_intf.is_immediate_result =
  Is_immediate (int_is_immediate n)

let is_simple_expr (expr : Cmm.expression) :
    Cfg_selectgen_target_intf.is_simple_expr_result =
  match expr with
  (* inlined floating-point ops are simple if their arguments are *)
  | Cop (Cextcall { func; _ }, args, _) when List.mem func inline_ops ->
    Simple_if_all_expressions_are args
  | _ -> Use_default

let effects_of (expr : Cmm.expression) :
    Cfg_selectgen_target_intf.effects_of_result =
  match expr with
  | Cop (Cextcall { func; _ }, args, _) when List.mem func inline_ops ->
    Effects_of_all_expressions args
  | _ -> Use_default

let select_addressing chunk (expr : Cmm.expression) :
    addressing_mode * Cmm.expression =
  match expr with
  | Cop ((Caddv | Cadda), [Cconst_symbol (s, _); Cconst_int (n, _)], _)
    when use_direct_addressing s ->
    Ibased (s.sym_name, n), Ctuple []
  | Cop ((Caddv | Cadda), [arg; Cconst_int (n, _)], _) when is_offset chunk n ->
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

let select_operation ~generic_select_condition:_ (op : Cmm.operation)
    (args : Cmm.expression list) _dbg :
    Cfg_selectgen_target_intf.select_operation_result =
  let[@inline] rewrite_multiply_add_or_sub shift_op mul_op ~arg1 ~args2 dbg :
      Cfg_selectgen_target_intf.select_operation_result =
    Select_operation_then_rewrite
      ( Cmuli,
        args2,
        dbg,
        fun (basic : Cfg.basic) ~args ->
          match basic, args with
          | Op (Intop_imm (Ilsl, l)), [arg3] ->
            Rewritten (specific (Ishiftarith (shift_op, l)), [arg1; arg3])
          | Op (Intop Imul), [arg3; arg4] ->
            Rewritten (specific mul_op, [arg3; arg4; arg1])
          | _ -> Use_default )
  in
  match op with
  (* Integer addition *)
  | Caddi | Caddv | Cadda -> (
    match args with
    (* Shift-add *)
    | [arg1; Cop (Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, n)), [arg1; arg2])
    | [arg1; Cop (Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, -n)), [arg1; arg2])
    | [Cop (Clsl, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, n)), [arg2; arg1])
    | [Cop (Casr, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, -n)), [arg2; arg1])
    (* Multiply-add *)
    | [arg1; Cop (Cmuli, args2, dbg)] | [Cop (Cmuli, args2, dbg); arg1] ->
      rewrite_multiply_add_or_sub Ishiftadd Imuladd ~arg1 ~args2 dbg
    | _ -> Use_default)
  (* Integer subtraction *)
  | Csubi -> (
    match args with
    (* Shift-sub *)
    | [arg1; Cop (Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftsub, n)), [arg1; arg2])
    | [arg1; Cop (Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftsub, -n)), [arg1; arg2])
    (* Multiply-sub *)
    | [arg1; Cop (Cmuli, args2, dbg)] ->
      rewrite_multiply_add_or_sub Ishiftsub Imulsub ~arg1 ~args2 dbg
    | _ -> Use_default)
  (* Recognize sign extension *)
  | Casr -> (
    match args with
    | [Cop (Clsl, [k; Cconst_int (n, _)], _); Cconst_int (n', _)]
      when n' = n && 0 < n && n < 64 ->
      Rewritten (specific (Isignext (64 - n)), [k])
    | _ -> Use_default)
  (* Use trivial addressing mode for atomic loads *)
  | Cload { memory_chunk; mutability; is_atomic = true } ->
    Rewritten
      ( Op
          (Load
             { memory_chunk;
               addressing_mode = Iindexed 0;
               mutability = Select_utils.select_mutable_flag mutability;
               is_atomic = true
             }),
        args )
  (* Recognize floating-point negate and multiply *)
  | Cnegf Float64 -> (
    match args with
    | [Cop (Cmulf Float64, args, _)] -> Rewritten (specific Inegmulf, args)
    | _ -> Use_default)
  (* Recognize floating-point multiply and add/sub *)
  | Caddf Float64 -> (
    match args with
    | [arg; Cop (Cmulf Float64, args, _)] | [Cop (Cmulf Float64, args, _); arg]
      ->
      Rewritten (specific Imuladdf, arg :: args)
    | _ -> Use_default)
  | Csubf Float64 -> (
    match args with
    | [arg; Cop (Cmulf Float64, args, _)] ->
      Rewritten (specific Imulsubf, arg :: args)
    | [Cop (Cmulf Float64, args, _); arg] ->
      Rewritten (specific Inegmulsubf, arg :: args)
    | _ -> Use_default)
  | Cpackf32 -> Rewritten (specific (Isimd Zip1_f32), args)
  (* Recognize floating-point square root *)
  | Cextcall { func = "sqrt" | "sqrtf" | "caml_neon_float64_sqrt"; _ } ->
    (* XXX check for these ones + any in amd64/ that they really are generated
       as Cop, or these patterns won't hit *)
    Rewritten (specific Isqrtf, args)
  | Cextcall { func; builtin = true; _ } -> (
    match Simd_selection.select_operation_cfg func args with
    | Some (op, args) -> Rewritten (Op op, args)
    | None -> Use_default)
  (* Recognize bswap instructions *)
  | Cbswap { bitwidth } ->
    let bitwidth = select_bitwidth bitwidth in
    Rewritten (specific (Ibswap { bitwidth }), args)
  (* Other operations are regular *)
  | _ -> Use_default

let select_store ~is_assign:_ _addr _exp :
    Cfg_selectgen_target_intf.select_store_result =
  Maybe_out_of_range

let is_store_out_of_range kind ~byte_offset :
    Cfg_selectgen_target_intf.is_store_out_of_range_result =
  if is_offset kind byte_offset then Within_range else Out_of_range

let insert_move_extcall_arg (ty_arg : Cmm.exttype) src dst :
    Cfg_selectgen_target_intf.insert_move_extcall_arg_result =
  let ty_arg_is_int32 =
    match ty_arg with
    | XInt32 -> true
    | XInt | XInt64 | XFloat32 | XFloat | XVec128 -> false
  in
  if macosx && ty_arg_is_int32 && is_stack_slot dst
  then Rewritten (Op (Specific Imove32), src, dst)
  else Use_default

let insert_op_debug _env _sub_cfg _op _dbg _rs _rd :
    Cfg_selectgen_target_intf.insert_op_debug_result =
  Use_default
