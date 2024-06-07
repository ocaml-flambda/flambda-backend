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
  (** Exceptions escape the current function *)
  | Specific_trap of Cmm.trywith_shared_label * trap_stack
  (** Current handler is a delayed/shared Trywith *)

type integer_comparison =
    Isigned of Cmm.integer_comparison
  | Iunsigned of Cmm.integer_comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh of { signed: bool } | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Iclz of { arg_is_non_zero: bool; }
  | Ictz of { arg_is_non_zero: bool; }
  | Ipopcnt
  | Icomp of integer_comparison

type float_comparison = Cmm.float_comparison

type float_width = Cmm.float_width

type float_operation =
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Icompf of float_comparison

type mutable_flag = Immutable | Mutable

val of_ast_mutable_flag : Asttypes.mutable_flag -> mutable_flag
val to_ast_mutable_flag : mutable_flag -> Asttypes.mutable_flag

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
  | Icall_ind
  | Icall_imm of { func : Cmm.symbol; }
  | Itailcall_ind
  | Itailcall_imm of { func : Cmm.symbol; }
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
                                 (* false = initialization, true = assignment *)
  | Ialloc of { bytes : int; dbginfo : Debuginfo.alloc_dbginfo;
                mode: Lambda.alloc_mode }
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
    (** [Iname_for_debugger] has the following semantics:
        (a) The argument register(s) is/are deemed to contain the value of the
            given identifier.
        (b) If [is_assignment] is [true], any information about other [Reg.t]s
            that have been previously deemed to hold the value of that
            identifier is forgotten. *)
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
    (* CR mshinwell: maybe this should be [option]: *)
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

val dummy_instr: instruction
val end_instr: unit -> instruction
val instr_cons_debug:
      instruction_desc -> Reg.t array -> Reg.t array -> Debuginfo.t ->
        instruction -> instruction
val instr_iter: (instruction -> unit) -> instruction -> unit

val operation_is_pure : operation -> bool
  (** Returns [true] if the given operation only produces a result
      in its destination registers, but has no side effects whatsoever:
      it doesn't raise exceptions, it doesn't modify already-allocated
      blocks, it doesn't adjust the stack frame, etc. *)

val operation_can_raise : operation -> bool
  (** Returns [true] if the given operation can raise an exception. *)

val free_conts_for_handlers : fundecl -> Numbers.Int.Set.t Numbers.Int.Map.t
val equal_trap_stack : trap_stack -> trap_stack -> bool

val equal_integer_comparison : integer_comparison -> integer_comparison -> bool
val equal_integer_operation : integer_operation -> integer_operation -> bool

val equal_float_width : float_width -> float_width -> bool
val equal_float_comparison : float_comparison -> float_comparison -> bool
val equal_float_operation : float_operation -> float_operation -> bool
