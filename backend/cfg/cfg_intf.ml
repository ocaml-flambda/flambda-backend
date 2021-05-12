(** Control flow graph structure types that are shared between the internal
    (mutable) and external (immutable) views of [Cfg]. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module S = struct
  type func_call_operation =
    | Indirect of { label_after : Label.t }
    | Direct of
        { func_symbol : string;
          label_after : Label.t
        }

  type tail_call_operation =
    | Self of { label_after : Label.t }
    | Func of func_call_operation

  type prim_call_operation =
    | External of
        { func_symbol : string;
          alloc : bool;
          label_after : Label.t
        }
    | Alloc of
        { bytes : int;
          label_after_call_gc : Label.t option;
          dbginfo : Debuginfo.alloc_dbginfo;
          spacetime_index : int
        }
    | Checkbound of
        { immediate : int option;
          label_after_error : Label.t option;
          spacetime_index : int
        }

  type operation =
    | Move
    | Spill
    | Reload
    | Const_int of nativeint (* CR-someday xclerc: change to `Targetint.t` *)
    | Const_float of int64
    | Const_symbol of string
    | Stackoffset of int
    | Load of Cmm.memory_chunk * Arch.addressing_mode
    | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
    | Intop of Mach.integer_operation
    | Intop_imm of Mach.integer_operation * int
    | Negf
    | Absf
    | Addf
    | Subf
    | Mulf
    | Divf
    | Floatofint
    | Intoffloat
    | Probe of
        { name : string;
          handler_code_sym : string
        }
    | Probe_is_enabled of { name : string }
    | Specific of Arch.specific_operation
    | Name_for_debugger of
        { ident : Ident.t;
          which_parameter : int option;
          provenance : unit option;
          is_assignment : bool
        }

  type call_operation =
    | P of prim_call_operation
    | F of func_call_operation

  type bool_test =
    { ifso : Label.t;  (** if test is true goto [ifso] label *)
      ifnot : Label.t  (** if test is false goto [ifnot] label *)
    }

  (** [int_test] represents all possible outcomes of a comparison between two
      integers. When [imm] field is [None], compare variables x and y,
      specified by the arguments of the enclosing [instruction]. When [imm]
      field is [Some n], compare variable x and immediate [n]. This
      corresponds to [Mach.Iinttest] and [Mach.Iinttest_imm] in the compiler. *)
  type int_test =
    { lt : Label.t;  (** if x < y (resp. x < n) goto [lt] label *)
      eq : Label.t;  (** if x = y (resp. x = n) goto [eq] label *)
      gt : Label.t;  (** if x > y (resp. x > n) goto [gt] label *)
      is_signed : bool;
      imm : int option
    }

  (** [float_test] represents possible outcomes of comparison between
      arguments x and y of type float. It is not enough to check "=,<,>"
      because possible outcomes of comparison include "unordered" (see e.g.
      x86-64 emitter) when the arguments involve NaNs. *)
  type float_test =
    { lt : Label.t;
      eq : Label.t;
      gt : Label.t;
      uo : Label.t  (** if at least one of x or y is NaN *)
    }

  type 'a instruction =
    { desc : 'a;
      arg : Reg.t array;
      res : Reg.t array;
      dbg : Debuginfo.t;
      live : Reg.Set.t;
      trap_depth : int;
      id : int
    }

  type basic =
    | Op of operation
    | Call of call_operation
    | Reloadretaddr
    | Pushtrap of { lbl_handler : Label.t }
    | Poptrap
    | Prologue

  (** Properties of the representation of successors:

      - Tests of different types are not mixed. For example, a test that
        compares between variables of type int cannot be combined with a
        float comparison in the same block terminator.
      - Total: all possible outcomes of a test have a defined target label
      - Disjoint: at most one of the outcomes of a test is true
      - Redundancy of labels: more than one outcome of test can lead to the
        same label
      - Redundancy of representation of unconditional jump: if all outcomes
        of a test lead to the same label, it can be represented as (Always
        l). For example, [Parity_test {true_=l;false_=l}] can be simplified
        to [(Always l)]. *)
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
    | Tailcall of tail_call_operation
end

(* CR-someday gyorsh: Switch can be translated to Branch. *)
