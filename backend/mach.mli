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
  | Generic_trap of trap_stack
  (** Current handler is a regular Trywith *)
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
  | Icheckbound

type float_comparison = Cmm.float_comparison

type float_operation =
  | Icompf of float_comparison
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf

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
                                 (* false = initialization, true = assignment *)
  | Ialloc of { bytes : int; dbginfo : Debuginfo.alloc_dbginfo;
                mode: Lambda.alloc_mode }
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Ifloatop of float_operation
  | Ifloatofint | Iintoffloat
  | Iopaque
  | Ispecific of Arch.specific_operation
  | Iname_for_debugger of { ident : Backend_var.t; which_parameter : int option;
      provenance : unit option; is_assignment : bool; }
    (** [Iname_for_debugger] has the following semantics:
        (a) The argument register(s) is/are deemed to contain the value of the
            given identifier.
        (b) If [is_assignment] is [true], any information about other [Reg.t]s
            that have been previously deemed to hold the value of that
            identifier is forgotten. *)
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

(* [arg_reg o] asserts that [o)] is [Ireg r] and returns [r]. *)
val arg_reg : operand -> Reg.t

(* [arg_regset operands] returns the set of registers used in [operands]. *)
val arg_regset : operand array -> Reg.Set.t

val same_loc : operand -> Reg.t -> bool
val is_immediate : operand -> bool

val dummy_instr: instruction
val end_instr: unit -> instruction
val instr_cons:
      instruction_desc -> operand array -> Reg.t array -> instruction ->
        instruction
val instr_cons_debug:
      instruction_desc -> operand array -> Reg.t array -> Debuginfo.t ->
        instruction -> instruction
val instr_iter: (instruction -> unit) -> instruction -> unit

val operation_can_raise : operation -> bool

val free_conts_for_handlers : fundecl -> Numbers.Int.Set.t Numbers.Int.Map.t
val equal_trap_stack : trap_stack -> trap_stack -> bool

val equal_integer_comparison : integer_comparison -> integer_comparison -> bool
val equal_integer_operation : integer_operation -> integer_operation -> bool
val equal_float_operation : float_operation -> float_operation -> bool

val equal_operand : operand -> operand -> bool
