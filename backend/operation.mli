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

[@@@ocaml.warning "+a-40-41-42"]

type trap_stack =
  | Uncaught  (** Exceptions escape the current function *)
  | Specific_trap of Cmm.trywith_shared_label * trap_stack
      (** Current handler is a delayed/shared Trywith *)

val equal_trap_stack : trap_stack -> trap_stack -> bool

type integer_comparison =
  | Isigned of Cmm.integer_comparison
  | Iunsigned of Cmm.integer_comparison

val string_of_integer_comparison : integer_comparison -> string

val equal_integer_comparison : integer_comparison -> integer_comparison -> bool

val invert_integer_comparison : integer_comparison -> integer_comparison

type integer_operation =
  | Iadd
  | Isub
  | Imul
  | Imulh of { signed : bool }
  | Idiv
  | Imod
  | Iand
  | Ior
  | Ixor
  | Ilsl
  | Ilsr
  | Iasr
  | Iclz of { arg_is_non_zero : bool }
  | Ictz of { arg_is_non_zero : bool }
  | Ipopcnt
  | Icomp of integer_comparison

val string_of_integer_operation : integer_operation -> string

val is_unary_integer_operation : integer_operation -> bool

val equal_integer_operation : integer_operation -> integer_operation -> bool

type float_comparison = Cmm.float_comparison

val equal_float_comparison : float_comparison -> float_comparison -> bool

type float_width = Cmm.float_width

val equal_float_width : float_width -> float_width -> bool

type float_operation =
  | Inegf
  | Iabsf
  | Iaddf
  | Isubf
  | Imulf
  | Idivf
  | Icompf of float_comparison

val string_of_float_operation : float_operation -> string

val format_float_operation : Format.formatter -> float_operation -> unit

val equal_float_operation : float_operation -> float_operation -> bool

type mutable_flag =
  | Immutable
  | Mutable

val equal_mutable_flag : mutable_flag -> mutable_flag -> bool

val of_ast_mutable_flag : Asttypes.mutable_flag -> mutable_flag

val to_ast_mutable_flag : mutable_flag -> Asttypes.mutable_flag

type test =
  | Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of float_width * float_comparison
  | Ioddtest
  | Ieventest

val format_test :
  print_reg:(Format.formatter -> Reg.t -> unit) ->
  test ->
  Format.formatter ->
  Reg.t array ->
  unit

val invert_test : test -> test

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
        mutability : mutable_flag;
        is_atomic : bool
      }
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of integer_operation
  | Intop_imm of integer_operation * int
  | Intop_atomic of
      { op : Cmm.atomic_op;
        size : Cmm.atomic_bitwidth;
        addr : Arch.addressing_mode
      }
  | Floatop of float_width * float_operation
  | Csel of test
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
        dbginfo : Cmm.alloc_dbginfo;
        mode : Cmm.Alloc_mode.t
      }
  | External of
      { func_symbol : string;
        effects : Cmm.effects;
        ty_res : Cmm.machtype;
        ty_args : Cmm.exttype list;
        stack_ofs : int
      }
      (** [Externals] cannot require [caml_c_call] or diverge.  Use [Prim] for
          such cases. *)

val is_pure : t -> bool

val dump : Format.formatter -> t -> unit
