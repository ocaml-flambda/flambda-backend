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
    Iadd | Isub | Imul | Imulh  of { signed: bool }  | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Iclz of { arg_is_non_zero: bool; }
  | Ictz of { arg_is_non_zero: bool; }
  | Ipopcnt
  | Icomp of integer_comparison
  | Icheckbound

type float_comparison = Cmm.float_comparison

type float_operation =
  | Icompf of float_comparison
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
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
  | Istore of bool
  | Ialloc of { bytes : int; dbginfo : Debuginfo.alloc_dbginfo;
                mode : Lambda.alloc_mode }
  | Iintop of integer_operation
  | Ifloatop of float_operation
  | Ifloatofint | Iintoffloat
  | Iopaque
  | Ispecific of Arch.specific_operation
  | Iname_for_debugger of { ident : Backend_var.t; which_parameter : int option;
      provenance : unit option; is_assignment : bool; }
  | Iprobe of { name: string; handler_code_sym: string; }
  | Iprobe_is_enabled of { name: string }
  | Ibeginregion | Iendregion

type operand =
  | Iimm of Targetint.t
  | Iimmf of int64
  | Ireg of Reg.t
  | Imem of { chunk : Cmm.memory_chunk option;
              addr : Arch.addressing_mode;
              reg : Reg.t array;
            }

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: operand array;
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

let arg_reg operand =
  match operand with
  | Ireg r -> r
  | Imem _ -> Misc.fatal_error "Mach.arg_reg: expected Ireg, found Imem"
  | Iimm _ -> Misc.fatal_error "Mach.arg_reg: expected Ireg, found Iimm"
  | Iimmf _ -> Misc.fatal_error "Mach.arg_reg: expected Ireg, found Iimmf"

let arg_regset operands =
  Array.fold_left (fun s -> function
    | Iimm _ | Iimmf _ -> s
    | Ireg r -> Reg.Set.add r s
    | Imem { reg } -> Reg.add_set_array s reg)
    Reg.Set.empty operands

let same_loc operand reg =
  match operand with
  | Iimm _ | Iimmf _ -> false
  | Imem _ ->
    (* CR gyorsh: can be optimized if reg.loc is Stack and mem is
       statically known to refer to the same stack location.
       This case will need to be handled if we replace the representation of
       Reg.Stack to use Imem. *)
    false
  | Ireg r -> Reg.same_loc r reg

let is_immediate = function
  | Iimm _ | Iimmf _ -> true
  | Ireg _ | Imem _ -> false

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
            | Iintop _
            | Ifloatop _
            | Ifloatofint | Iintoffloat
            | Ispecific _ | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _
            | Iopaque
            | Ibeginregion | Iendregion) ->
        instr_iter f i.next

let operation_can_raise op =
  match op with
  | Icall_ind | Icall_imm _ | Iextcall _
  | Iintop (Icheckbound)
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
  | Icheckbound, Icheckbound -> true
  | Iadd, (Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Isub, (Iadd | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Imul, (Iadd | Isub | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Imulh _, (Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ilsl
           | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Idiv, (Iadd | Isub | Imul | Imulh _ | Imod | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Imod, (Iadd | Isub | Imul | Imulh _ | Idiv | Iand | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Iand, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Ior | Ixor | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ior, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ixor | Ilsl
         | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ixor, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ilsl
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ilsl, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ilsr, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Iasr, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Iclz _, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Ictz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ictz _, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Iclz _ | Ipopcnt | Icomp _ | Icheckbound)
  | Ipopcnt, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
             | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Icomp _ | Icheckbound)
  | Icomp _, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
             | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icheckbound)
  | Icheckbound, (Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior
                 | Ixor | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _) ->
    false

let equal_float_operation left right =
  match left, right with
  | Icompf left_comp, Icompf right_comp -> Cmm.equal_float_comparison left_comp right_comp
  | Inegf, Inegf -> true
  | Iabsf, Iabsf -> true
  | Iaddf, Iaddf -> true
  | Isubf, Isubf -> true
  | Imulf, Imulf -> true
  | Idivf, Idivf -> true
  | (Icompf _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf), _ -> false

let equal_operand left right =
  match left, right with
  | Iimm left, Iimm right -> Targetint.equal left right
  | Iimmf left, Iimmf right -> Int64.equal left right
  | Ireg left, Ireg right -> Reg.same_loc left right
  | Imem { chunk=left_chunk; addr=left_addr; reg=left_reg },
    Imem { chunk=right_chunk; addr=right_addr; reg=right_reg } ->
    Option.equal Cmm.equal_memory_chunk left_chunk right_chunk &&
    Arch.equal_addressing_mode left_addr right_addr &&
    Array.length left_reg = Array.length right_reg &&
    Array.for_all2 Reg.same_loc left_reg right_reg
  | (Iimm _ | Iimmf _ | Ireg _ | Imem _),_ -> false

