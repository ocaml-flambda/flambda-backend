(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Representation of machine code by sequences of pseudoinstructions *)

type trap_stack =
  | Uncaught
  | Specific_trap of Cmm.trywith_shared_label * trap_stack

type integer_comparison =
    Isigned of Cmm.integer_comparison
  | Iunsigned of Cmm.integer_comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh  of { signed: bool }  | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Iclz of { arg_is_non_zero: bool; }
  | Ictz of { arg_is_non_zero: bool; }
  | Ipopcnt
  | Icomp of integer_comparison

type float_width = Cmm.float_width

type float_comparison = Cmm.float_comparison

type float_operation =
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Icompf of float_comparison

type mutable_flag = Immutable | Mutable

let of_ast_mutable_flag
  : Asttypes.mutable_flag -> mutable_flag
  = function
    | Immutable -> Immutable
    | Mutable -> Mutable

let to_ast_mutable_flag
  : mutable_flag -> Asttypes.mutable_flag
  = function
    | Immutable -> Immutable
    | Mutable -> Mutable

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of float_width * float_comparison
  | Ioddtest
  | Ieventest

type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of nativeint
  | Iconst_float32 of int32
  | Iconst_float of int64
  | Iconst_vec128 of Cmm.vec128_bits
  | Iconst_symbol of Cmm.symbol
  | Icall_ind of { tail : Lambda.tail_attribute }
  | Icall_imm of { func : Cmm.symbol; tail : Lambda.tail_attribute }
  | Itailcall_ind of { tail : Lambda.tail_attribute }
  | Itailcall_imm of { func : Cmm.symbol; tail : Lambda.tail_attribute }
  | Iextcall of { func : string;
                  ty_res : Cmm.machtype; ty_args : Cmm.exttype list;
                  alloc : bool; returns : bool;
                  stack_ofs : int; }
  | Istackoffset of int
  | Iload of { memory_chunk : Cmm.memory_chunk;
               addressing_mode : Arch.addressing_mode;
               mutability : Asttypes.mutable_flag;
               is_atomic : bool }
  | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Ialloc of { bytes : int; dbginfo : Debuginfo.alloc_dbginfo;
                mode : Lambda.alloc_mode }
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Iintop_atomic of { op : Cmm.atomic_op; size : Cmm.atomic_bitwidth;
                       addr : Arch.addressing_mode }
  | Ifloatop of float_width * float_operation
  | Icsel of test
  | Ireinterpret_cast of Cmm.reinterpret_cast
  | Istatic_cast of Cmm.static_cast
  | Iopaque
  | Ispecific of Arch.specific_operation
  | Ipoll of { return_label: Cmm.label option }
  | Iname_for_debugger of { ident : Backend_var.t; which_parameter : int option;
      provenance : Backend_var.Provenance.t option; is_assignment : bool;
      regs : Reg.t array }
  | Iprobe of { name: string; handler_code_sym: string; enabled_at_init: bool; }
  | Iprobe_is_enabled of { name: string }
  | Ibeginregion | Iendregion
  | Idls_get

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    mutable live: Reg.Set.t;
    mutable available_before: Reg_availability_set.t;
    mutable available_across: Reg_availability_set.t option;
  }

and instruction_desc =
    Iend
  | Iop of operation
  | Ireturn of Cmm.trap_action list
  | Iifthenelse of test * instruction * instruction
  | Iswitch of int array * instruction array
  | Icatch of Cmm.rec_flag * trap_stack * (int * trap_stack * instruction * bool) list * instruction
  | Iexit of int * Cmm.trap_action list
  | Itrywith of instruction * Cmm.trywith_shared_label
      * (trap_stack * instruction)
  | Iraise of Lambda.raise_kind

type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.t;
    fun_poll: Lambda.poll_attribute;
    fun_num_stack_slots: int array;
    fun_contains_calls: bool;
  }

let rec dummy_instr =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let end_instr () =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let instr_cons_debug d a r dbg n =
  { desc = d; next = n; arg = a; res = r; dbg = dbg; live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let rec instr_iter f i =
  match i.desc with
    Iend -> ()
  | _ ->
      f i;
      match i.desc with
        Iend -> ()
      | Ireturn _ | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(_tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(_index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Icatch(_, _ts, handlers, body) ->
          instr_iter f body;
          List.iter (fun (_n, _ts, handler, _is_cold) -> instr_iter f handler) handlers;
          instr_iter f i.next
      | Iexit _ -> ()
      | Itrywith(body, _kind, (_ts, handler)) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise _ -> ()
      | Iop (Imove | Ispill | Ireload
            | Iconst_int _ | Iconst_float32 _ | Iconst_float _
            | Iconst_symbol _ | Iconst_vec128 _
            | Icall_ind _ | Icall_imm _ | Iextcall _ | Istackoffset _
            | Iload _ | Istore _ | Ialloc _
            | Iintop _ | Iintop_imm _ | Iintop_atomic _
            | Ifloatop _
            | Icsel _ | Ireinterpret_cast _ | Istatic_cast _
            | Ispecific _ | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _
            | Iopaque
            | Ibeginregion | Iendregion | Ipoll _ | Idls_get) ->
        instr_iter f i.next

let operation_is_pure = function
  | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _ | Ipoll _
  | Idls_get
  | Iopaque
  (* Conservative to ensure valueofint/intofvalue are not eliminated before emit. *)
  | Ireinterpret_cast (Value_of_int | Int_of_value) | Iintop_atomic _ -> false
  | Ibeginregion | Iendregion -> false
  | Iprobe _ -> false
  | Iprobe_is_enabled _-> true
  | Ispecific sop -> Arch.operation_is_pure sop
  | Iintop_imm((Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
               | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _|Ictz _|Icomp _), _)
  | Iintop(Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _|Ictz _|Icomp _)
  | Imove | Ispill | Ireload | Ifloatop _
  | Icsel _
  | Ireinterpret_cast (Float32_of_float | Float_of_float32 |
                       Int64_of_float | Float_of_int64 |
                       Int32_of_float32 | Float32_of_int32 |
                       V128_of_v128)
  | Istatic_cast (Float_of_int _ | Int_of_float _ |
                  Float_of_float32 | Float32_of_float |
                  Scalar_of_v128 _ | V128_of_scalar _)
  | Iconst_int _ | Iconst_float _ | Iconst_float32 _
  | Iconst_symbol _ | Iconst_vec128 _
  | Iload _ -> true
  | Iname_for_debugger _ -> false


let operation_can_raise op =
  match op with
  | Icall_ind _ | Icall_imm _ | Iextcall _
  | Iprobe _ -> true
  | Ispecific sop -> Arch.operation_can_raise sop
  | Iintop_imm((Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
               | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _|Ictz _|Icomp _), _)
  | Iintop(Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _|Ictz _|Icomp _)
  | Iintop_atomic _
  | Imove | Ispill | Ireload | Ifloatop _
  | Icsel _ | Ireinterpret_cast _ | Istatic_cast _
  | Iconst_int _ | Iconst_float _ | Iconst_float32 _
  | Iconst_symbol _ | Iconst_vec128 _
  | Istackoffset _ | Istore _  | Iload _ | Iname_for_debugger _
  | Itailcall_imm _ | Itailcall_ind _
  | Iopaque | Ibeginregion | Iendregion
  | Iprobe_is_enabled _ | Ialloc _ | Ipoll _ | Idls_get
    -> false

let free_conts_for_handlers fundecl =
  let module S = Numbers.Int.Set in
  let module M = Numbers.Int.Map in
  let acc = ref M.empty in
  let rec free_conts i =
    match i.desc with
    | Iend -> S.empty
    | desc ->
      let next_conts = free_conts i.next in
      match desc with
      | Iend -> assert false
      | Iop _ -> next_conts
      | Ireturn _ -> next_conts
      | Iifthenelse (_, then_, else_) ->
        S.union next_conts (S.union (free_conts then_) (free_conts else_))
      | Iswitch (_, cases) ->
        Array.fold_left (fun conts instr -> S.union conts (free_conts instr))
          next_conts cases
      | Icatch (_rec_flag, _ts, handlers, body) ->
        let conts = S.union next_conts (free_conts body) in
        let conts =
          List.fold_left (fun conts (nfail, ts, i, _is_cold) ->
            let rec add_exn_conts conts = function
              | Uncaught -> conts
              | Specific_trap (nfail, ts) -> add_exn_conts (S.add nfail conts) ts
            in
            let free = add_exn_conts (free_conts i) ts in
            acc := M.add nfail free !acc;
            S.union conts free)
            conts handlers
        in
        List.fold_left (fun conts (nfail, _ts, _i, _is_cold) ->
          S.remove nfail conts)
          conts handlers
      | Iexit (nfail, _) -> S.add nfail next_conts
      | Itrywith (body, nfail, (_ts, handler)) ->
        let conts =
          S.union next_conts (S.union (free_conts body) (free_conts handler))
        in
        S.remove nfail conts
      | Iraise _ -> next_conts
  in
  let free = free_conts fundecl.fun_body in
  assert(S.is_empty free);
  !acc

let rec equal_trap_stack ts1 ts2 =
  match ts1, ts2 with
  | Uncaught, Uncaught -> true
  | Specific_trap (lbl1, ts1), Specific_trap (lbl2, ts2) ->
    Int.equal lbl1 lbl2 && equal_trap_stack ts1 ts2
  | Uncaught, Specific_trap _
  | Specific_trap _, Uncaught -> false


let equal_integer_comparison left right =
  match left, right with
  | Isigned left, Isigned right -> Cmm.equal_integer_comparison left right
  | Iunsigned left, Iunsigned right -> Cmm.equal_integer_comparison left right
  | Isigned _, Iunsigned _
  | Iunsigned _, Isigned _ ->
    false

let equal_integer_operation left right =
  match left, right with
  | Iadd,  Iadd  -> true
  | Isub,  Isub  -> true
  | Imul,  Imul  -> true
  | Imulh { signed = left }, Imulh  { signed = right } ->
    Bool.equal left right
  | Idiv,  Idiv  -> true
  | Imod,  Imod  -> true
  | Iand,  Iand  -> true
  | Ior,   Ior   -> true
  | Ixor,  Ixor  -> true
  | Ilsl,  Ilsl  -> true
  | Ilsr,  Ilsr  -> true
  | Iasr,  Iasr  -> true
  | Iclz { arg_is_non_zero = left_arg_is_non_zero; },
    Iclz { arg_is_non_zero = right_arg_is_non_zero; } ->
    Bool.equal left_arg_is_non_zero right_arg_is_non_zero
  | Ictz { arg_is_non_zero = left_arg_is_non_zero; },
    Ictz { arg_is_non_zero = right_arg_is_non_zero; } ->
    Bool.equal left_arg_is_non_zero right_arg_is_non_zero
  | Ipopcnt, Ipopcnt -> true
  | Icomp left, Icomp right -> equal_integer_comparison left right
  | Iadd, (Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Isub, (Iadd | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Imul, (Iadd | Isub | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Imulh _, (Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ilsl
           | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Idiv, (Iadd | Isub | Imul | Imulh _ | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Imod, (Iadd | Isub | Imul | Imulh _ | Idiv | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Iand, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Ior, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ixor | Ilsl
         | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Ixor, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Ilsl, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Ilsr, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Iasr, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iclz _ | Ictz _ | Ipopcnt | Icomp _)
  | Iclz _, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Ictz _ | Ipopcnt | Icomp _)
  | Ictz _, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Iclz _ | Ipopcnt | Icomp _)
  | Ipopcnt, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
             | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Icomp _)
  | Icomp _, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
             | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt)
  -> false

let equal_float_comparison = Cmm.equal_float_comparison

let equal_float_width = Cmm.equal_float_width

let equal_float_operation left right =
  match left, right with
  | Inegf, Inegf -> true
  | Iabsf, Iabsf -> true
  | Iaddf, Iaddf -> true
  | Isubf, Isubf -> true
  | Imulf, Imulf -> true
  | Idivf, Idivf -> true
  | Icompf left, Icompf right -> equal_float_comparison left right
  | Inegf, (Iabsf | Iaddf | Isubf | Imulf | Idivf | Icompf _)
  | Iabsf, (Inegf | Iaddf | Isubf | Imulf | Idivf | Icompf _)
  | Iaddf, (Inegf | Iabsf | Isubf | Imulf | Idivf | Icompf _)
  | Isubf, (Inegf | Iabsf | Iaddf | Imulf | Idivf | Icompf _)
  | Imulf, (Inegf | Iabsf | Iaddf | Isubf | Idivf | Icompf _)
  | Idivf, (Inegf | Iabsf | Iaddf | Isubf | Imulf | Icompf _)
  | Icompf _, (Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf)
    -> false
