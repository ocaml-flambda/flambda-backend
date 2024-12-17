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

(* Operations common to CFG and Linear. *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

(* CR-soon xclerc for xclerc: consider whether `Simple_operation` and
   `Operation` should be merged into a single module. *)
type memory_access =
  | No_memory_access
  | Arbitrary
  | Read of
      { memory_chunk : Cmm.memory_chunk;
        addressing_mode : Arch.addressing_mode;
        mutability : Simple_operation.mutable_flag
      }
  | Write of
      { memory_chunk : Cmm.memory_chunk;
        addressing_mode : Arch.addressing_mode;
        is_assignment : bool (* false means initialization *)
      }

type t =
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
        mutability : Simple_operation.mutable_flag;
        is_atomic : bool
      }
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of Simple_operation.integer_operation
  | Intop_imm of Simple_operation.integer_operation * int
  | Intop_atomic of
      { op : Cmm.atomic_op;
        size : Cmm.atomic_bitwidth;
        addr : Arch.addressing_mode
      }
  | Floatop of Simple_operation.float_width * Simple_operation.float_operation
  | Csel of Simple_operation.test
  | Reinterpret_cast of Cmm.reinterpret_cast
  | Static_cast of Cmm.static_cast
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
        mode : Cmm.Alloc_mode.t
      }

val is_pure : t -> bool

val dump : Format.formatter -> t -> unit
