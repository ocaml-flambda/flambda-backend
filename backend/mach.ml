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
  | Generic_trap of trap_stack
  | Specific_trap of Cmm.trywith_shared_label * trap_stack

type integer_comparison =
    Isigned of Cmm.integer_comparison
  | Iunsigned of Cmm.integer_comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Iclz of { arg_is_non_zero: bool; }
  | Ictz of { arg_is_non_zero: bool; }
  | Ipopcnt
  | Icomp of integer_comparison
  | Icheckbound

type float_comparison = Cmm.float_comparison

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of float_comparison
  | Ioddtest
  | Ieventest

type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of nativeint
  | Iconst_float of int64
  | Iconst_symbol of string
  | Icall_ind
  | Icall_imm of { func : string; }
  | Itailcall_ind
  | Itailcall_imm of { func : string; }
  | Iextcall of { func : string;
                  ty_res : Cmm.machtype; ty_args : Cmm.exttype list;
                  alloc : bool; returns : bool; }
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * Arch.addressing_mode
  | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Ialloc of { bytes : int; dbginfo : Debuginfo.alloc_dbginfo; }
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Icompf of float_comparison
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation
  | Iname_for_debugger of { ident : Backend_var.t; which_parameter : int option;
      provenance : unit option; is_assignment : bool; }
  | Iprobe of { name: string; handler_code_sym: string; }
  | Iprobe_is_enabled of { name: string }

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
  | Icatch of Cmm.rec_flag * trap_stack * (int * trap_stack * instruction) list * instruction
  | Iexit of int * Cmm.trap_action list
  | Itrywith of instruction * Cmm.trywith_kind * (trap_stack * instruction)
  | Iraise of Lambda.raise_kind

type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_codegen_options : Cmm.codegen_option list;
    fun_dbg : Debuginfo.t;
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

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty;
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
      | Ireturn _ | Iop Itailcall_ind | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(_tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(_index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Icatch(_, _ts, handlers, body) ->
          instr_iter f body;
          List.iter (fun (_n, _ts, handler) -> instr_iter f handler) handlers;
          instr_iter f i.next
      | Iexit _ -> ()
      | Itrywith(body, _kind, (_ts, handler)) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise _ -> ()
      | Iop (Imove | Ispill | Ireload
            | Iconst_int _ | Iconst_float _ | Iconst_symbol _
            | Icall_ind | Icall_imm _ | Iextcall _ | Istackoffset _
            | Iload _ | Istore _ | Ialloc _
            | Iintop _ | Iintop_imm _
            | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
            | Icompf _
            | Ifloatofint | Iintoffloat
            | Ispecific _ | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _) ->
        instr_iter f i.next

let operation_can_raise op =
  match op with
  | Icall_ind | Icall_imm _ | Iextcall _
  | Iintop (Icheckbound) | Iintop_imm (Icheckbound, _)
  | Iprobe _
  | Ialloc _ -> true
  | _ -> false

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
          List.fold_left (fun conts (nfail, ts, i) ->
            let rec add_exn_conts conts = function
              | Uncaught -> conts
              | Generic_trap ts -> add_exn_conts conts ts
              | Specific_trap (nfail, ts) -> add_exn_conts (S.add nfail conts) ts
            in
            let free = add_exn_conts (free_conts i) ts in
            acc := M.add nfail free !acc;
            S.union conts free)
            conts handlers
        in
        List.fold_left (fun conts (nfail, _ts, _i) ->
          S.remove nfail conts)
          conts handlers
      | Iexit (nfail, _) -> S.add nfail next_conts
      | Itrywith (body, kind, (_ts, handler)) ->
        let conts =
          S.union next_conts (S.union (free_conts body) (free_conts handler))
        in
        begin match kind with
        | Regular -> conts
        | Delayed nfail -> S.remove nfail conts
        end
      | Iraise _ -> next_conts
  in
  let free = free_conts fundecl.fun_body in
  assert(S.is_empty free);
  !acc

let rec equal_trap_stack ts1 ts2 =
  match ts1, ts2 with
  | Uncaught, Uncaught -> true
  | Generic_trap ts1, Generic_trap ts2 -> equal_trap_stack ts1 ts2
  | Specific_trap (lbl1, ts1), Specific_trap (lbl2, ts2) ->
    Int.equal lbl1 lbl2 && equal_trap_stack ts1 ts2
  | Uncaught, (Generic_trap _ | Specific_trap _)
  | Generic_trap _, (Uncaught | Specific_trap _)
  | Specific_trap _, (Uncaught | Generic_trap _) -> false


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
  | Imulh, Imulh -> true
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
  | Icheckbound, Icheckbound -> true
  | Iadd, (Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Isub, (Iadd | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Imul, (Iadd | Isub | Imulh | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Imulh, (Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ilsl
           | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Idiv, (Iadd | Isub | Imul | Imulh | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Imod, (Iadd | Isub | Imul | Imulh | Idiv | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Iand, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ior, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ixor | Ilsl
         | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ixor, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ilsl, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ilsr, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Iasr, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Iclz _, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ictz _, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Iclz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ipopcnt, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
             | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Icomp _ | Icheckbound)
  | Icomp _, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
             | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icheckbound)
  | Icheckbound, (Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior
                 | Ixor | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _) ->
    false
