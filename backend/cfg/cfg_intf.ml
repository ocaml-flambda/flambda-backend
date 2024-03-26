(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

(** Control flow graph structure types that are shared between the internal
    (mutable) and external (immutable) views of [Cfg]. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module S = struct
  type func_call_operation =
    | Indirect
    | Direct of Cmm.symbol

  type external_call_operation =
    { func_symbol : string;
      alloc : bool;
      ty_res : Cmm.machtype;
      ty_args : Cmm.exttype list;
      stack_ofs : int
    }

  type prim_call_operation =
    | External of external_call_operation
    | Probe of
        { name : string;
          handler_code_sym : string;
          enabled_at_init : bool
        }

  type operation =
    | Move
    | Spill
    | Reload
    | Const_int of nativeint (* CR-someday xclerc: change to `Targetint.t` *)
    | Const_float32 of int32
    | Const_float of int64
    | Const_symbol of Cmm.symbol
    | Const_vec128 of Cmm.vec128_bits
    | Stackoffset of int
    | Load of
        { memory_chunk : Cmm.memory_chunk;
          addressing_mode : Arch.addressing_mode;
          mutability : Mach.mutable_flag;
          is_atomic : bool
        }
    | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
    | Intop of Mach.integer_operation
    | Intop_imm of Mach.integer_operation * int
    | Intop_atomic of
        { op : Cmm.atomic_op;
          size : Cmm.atomic_bitwidth;
          addr : Arch.addressing_mode
        }
    | Negf of Cmm.float_width
    | Absf of Cmm.float_width
    | Addf of Cmm.float_width
    | Subf of Cmm.float_width
    | Mulf of Cmm.float_width
    | Divf of Cmm.float_width
    | Compf of
        Cmm.float_width
        * Mach.float_comparison (* CR gyorsh: can merge with float_test? *)
    | Csel of Mach.test
    | Valueofint
    | Intofvalue
    | Vectorcast of Cmm.vector_cast
    | Scalarcast of Cmm.scalar_cast
    | Probe_is_enabled of { name : string }
    | Opaque
    | Begin_region
    | End_region
    | Specific of Arch.specific_operation
    | Name_for_debugger of
        { ident : Ident.t;
          which_parameter : int option;
          provenance : Backend_var.Provenance.t option;
          is_assignment : bool;
          regs : Reg.t array
        }
    | Dls_get
    | Poll
    | Alloc of
        { bytes : int;
          dbginfo : Debuginfo.alloc_dbginfo;
          mode : Lambda.alloc_mode
        }

  type bool_test =
    { ifso : Label.t;  (** if test is true goto [ifso] label *)
      ifnot : Label.t  (** if test is false goto [ifnot] label *)
    }

  (** [int_test] represents all possible outcomes of a comparison between two
      integers. When [imm] field is [None], compare variables x and y, specified
      by the arguments of the enclosing [instruction]. When [imm] field is [Some
      n], compare variable x and immediate [n]. This corresponds to
      [Mach.Iinttest] and [Mach.Iinttest_imm] in the compiler. *)
  type int_test =
    { lt : Label.t;  (** if x < y (resp. x < n) goto [lt] label *)
      eq : Label.t;  (** if x = y (resp. x = n) goto [eq] label *)
      gt : Label.t;  (** if x > y (resp. x > n) goto [gt] label *)
      is_signed : bool;
      imm : int option
    }

  (** [float_test] represents possible outcomes of comparison between arguments
      x and y of type float. It is not enough to check "=,<,>" because possible
      outcomes of comparison include "unordered" (see e.g. x86-64 emitter) when
      the arguments involve NaNs. *)
  type float_test =
    { width : Cmm.float_width;
      lt : Label.t;
      eq : Label.t;
      gt : Label.t;
      uo : Label.t  (** if at least one of x or y is NaN *)
    }

  type irc_work_list =
    | Unknown_list
    | Coalesced
    | Constrained
    | Frozen
    | Work_list
    | Active

  type 'a instruction =
    { desc : 'a;
      mutable arg : Reg.t array;
      mutable res : Reg.t array;
      mutable dbg : Debuginfo.t;
      mutable fdo : Fdo_info.t;
      mutable live : Reg.Set.t;
      mutable stack_offset : int;
      id : int;
      mutable irc_work_list : irc_work_list;
      mutable ls_order : int;
      mutable available_before : Reg_availability_set.t option;
      mutable available_across : Reg_availability_set.t option
    }

  (* [basic] instruction cannot raise *)
  type basic =
    | Op of operation
    | Reloadretaddr
        (** This instruction loads the return address from a predefined hidden
            address (e.g. bottom of the current frame) and stores it in a
            special hidden register. It can use standard registers for that
            purpose. They are defined in [Proc.destroyed_at_reloadretaddr]. *)
    | Pushtrap of { lbl_handler : Label.t }
    | Poptrap
    | Prologue

  type 'a with_label_after =
    { op : 'a;
      label_after : Label.t
    }

  (* Properties of the representation of successors:
   * - Tests of different types are not mixed. For example, a test that
   *   compares between variables of type int cannot be combined with a
   *   float comparison in the same block terminator.
   * - Total: all possible outcomes of a test have a defined target label
   * - Disjoint: at most one of the outcomes of a test is true
   * - Redundancy of labels: more than one outcome of test can lead to the
   *   same label
   * - Redundancy of representation of unconditional jump: if all outcomes
   *   of a test lead to the same label, it can be represented as (Always
   *   l). For example, [Parity_test {true_=l;false_=l}] can be simplified
   *   to [(Always l)]. *)
  type terminator =
    | Never
    | Always of Label.t
    | Parity_test of bool_test  (** Check if the argument is even or odd *)
    | Truth_test of bool_test  (** Check if the argument is true or false. *)
    | Float_test of float_test
    | Int_test of int_test
    | Switch of Label.t array
    | Return
    | Raise of Lambda.raise_kind
    | Tailcall_self of { destination : Label.t }
    | Tailcall_func of func_call_operation
    | Call_no_return of external_call_operation
    | Call of func_call_operation with_label_after
    | Prim of prim_call_operation with_label_after
    | Specific_can_raise of Arch.specific_operation with_label_after
end

(* CR-someday gyorsh: Switch can be translated to Branch. *)
