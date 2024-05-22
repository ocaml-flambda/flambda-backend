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
open Mach

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
  | Lop of Mach.operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of { label : label; section_name : string option }
  | Lbranch of label
  | Lcondbranch of Mach.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lentertrap
  | Ladjust_stack_offset of { delta_bytes : int; }
  | Lpushtrap of { lbl_handler : label; }
  | Lpoptrap
  | Lraise of Lambda.raise_kind
  | Lstackcheck of { max_frame_size_bytes : int; }

let has_fallthrough = function
  | Lreturn | Lbranch _ | Lswitch _ | Lraise _
  | Lop Itailcall_ind | Lop (Itailcall_imm _)
  | Lop (Ipoll { return_label = Some _ }) -> false
  | _ -> true

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

(* Invert a test *)

let invert_integer_test = function
    Isigned cmp -> Isigned(Cmm.negate_integer_comparison cmp)
  | Iunsigned cmp -> Iunsigned(Cmm.negate_integer_comparison cmp)

let invert_test = function
    Itruetest -> Ifalsetest
  | Ifalsetest -> Itruetest
  | Iinttest(cmp) -> Iinttest(invert_integer_test cmp)
  | Iinttest_imm(cmp, n) -> Iinttest_imm(invert_integer_test cmp, n)
  | Ifloattest(w, cmp) -> Ifloattest(w, Cmm.negate_float_comparison cmp)
  | Ieventest -> Ioddtest
  | Ioddtest -> Ieventest

(* The "end" instruction *)

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    fdo = Fdo_info.none;
    live = Reg.Set.empty;
    available_before = Some Unreachable;
    available_across = None
  }

(* Cons an instruction (live, debug empty) *)

let instr_cons d a r n ~available_before ~available_across =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; fdo = Fdo_info.none; live = Reg.Set.empty;
    available_before; available_across }

let traps_to_bytes traps = Proc.trap_size_in_bytes * traps
