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

(* Transformation of Mach code into a list of pseudo-instructions. *)

type label = Cmm.label

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    fdo: Fdo_info.t;
    live: Reg.Set.t;
    available_before: Reg_availability_set.t option;
    available_across: Reg_availability_set.t option;
  }

and instruction_desc =
  | Lprologue
  | Lend
  | Lop of Operation.t
  | Lcall_op of call_operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of { label : label; section_name : string option }
  | Lbranch of label
  | Lcondbranch of Simple_operation.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lentertrap
  | Ladjust_stack_offset of { delta_bytes : int; }
  | Lpushtrap of { lbl_handler : label; }
  | Lpoptrap
  | Lraise of Lambda.raise_kind
  | Lstackcheck of { max_frame_size_bytes : int; }

and call_operation =
  | Lcall_ind
  | Lcall_imm of { func : Cmm.symbol; }
  | Ltailcall_ind
  | Ltailcall_imm of { func : Cmm.symbol; }
  | Lextcall of { func : string;
                  ty_res : Cmm.machtype; ty_args : Cmm.exttype list;
                  alloc : bool; returns : bool;
                  stack_ofs : int; }
  | Lprobe of { name: string; handler_code_sym: string; enabled_at_init: bool; }

val has_fallthrough :  instruction_desc -> bool
val end_instr: instruction
val instr_cons:
  instruction_desc -> Reg.t array -> Reg.t array -> instruction
  -> available_before:Reg_availability_set.t option
  -> available_across:Reg_availability_set.t option -> instruction

type fundecl =
  { fun_name: string;
    fun_args: Reg.Set.t;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
    fun_tailrec_entry_point_label : label option;
    fun_contains_calls: bool;
    fun_num_stack_slots: int array;
    fun_frame_required: bool;
    fun_prologue_required: bool;
    fun_section_name: string option;
  }

val traps_to_bytes : int -> int
