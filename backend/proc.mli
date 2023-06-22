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

(* Processor descriptions *)

(* Instruction selection *)
val word_addressed: bool

(* Registers available for register allocation *)
val num_register_classes: int
val register_class: Reg.t -> int
val register_class_tag: int -> string
val num_available_registers: int array
val first_available_register: int array
val register_name: int -> string
val phys_reg: int -> Reg.t
val rotate_registers: bool
val all_phys_regs : Reg.t array

(* Calling conventions *)
val loc_arguments: Cmm.machtype -> Reg.t array * int
val loc_results_call: Cmm.machtype -> Reg.t array * int
val loc_parameters: Cmm.machtype -> Reg.t array
val loc_results_return: Cmm.machtype -> Reg.t array
(* For argument number [n] split across multiple registers, the target-specific
   implementation of [loc_external_arguments] must return [regs] such that
   [regs.(n).(0)] is to hold the part of the value at the lowest address. *)
val loc_external_arguments: Cmm.exttype list -> Reg.t array array * int
val loc_external_results: Cmm.machtype -> Reg.t array
val loc_exn_bucket: Reg.t

(* The maximum number of arguments of an OCaml to OCaml function call for
   which it is guaranteed there will be no arguments passed on the stack.
   (Above this limit, tail call optimization may be disabled.)
   N.B. The values for this parameter in the backends currently assume
   that no unboxed floats are passed using the OCaml calling conventions.
*)
val max_arguments_for_tailcalls : int

(* Maximal register pressures for pre-spilling *)
val safe_register_pressure: Mach.operation -> int
val max_register_pressure: Mach.operation -> int array

(* Registers destroyed by operations *)
val destroyed_at_oper: Mach.instruction_desc -> Reg.t array
val destroyed_at_raise: Reg.t array
val destroyed_at_reloadretaddr : Reg.t array
val destroyed_at_pushtrap : Reg.t array
val destroyed_at_basic : Cfg_intf.S.basic -> Reg.t array
val destroyed_at_terminator : Cfg_intf.S.terminator -> Reg.t array
val is_destruction_point : Cfg_intf.S.terminator -> bool

(* Volatile registers: those that change value when read *)
val regs_are_volatile: Reg.t array -> bool

(* Info for laying out the stack frame *)
val frame_required :
  fun_contains_calls:bool ->
  fun_num_stack_slots: int array ->
  bool

(* Function prologues *)
val prologue_required :
  fun_contains_calls : bool ->
  fun_num_stack_slots : int array ->
  bool

(** For a given register class, the DWARF register numbering for that class.
    Given an allocated register with location [Reg n] and class [reg_class], the
    returned array contains the corresponding DWARF register number at index
    [n - first_available_register.(reg_class)]. *)
val dwarf_register_numbers : reg_class:int -> int array

(** The DWARF register number corresponding to the stack pointer. *)
val stack_ptr_dwarf_register_number : int

(* Calling the assembler *)
val assemble_file: string -> string -> int

(* Called before translating a fundecl. *)
val init : unit -> unit

(** [operation_supported op] returns true when [op]
    can be implemented directly with a hardware instruction.
    Used in Cmmgen when converting [@@builtin] external calls
    to primitive operations. *)
val operation_supported : Cmm.operation -> bool

(** The number of bytes each trap occupies on the stack. *)
val trap_size_in_bytes : int
