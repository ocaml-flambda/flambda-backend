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

[@@@ocaml.warning "+a-4-9-40-41-42"]

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

let is_pure = function
  | Move -> true
  | Spill -> true
  | Reload -> true
  | Const_int _ -> true
  | Const_float32 _ -> true
  | Const_float _ -> true
  | Const_symbol _ -> true
  | Const_vec128 _ -> true
  | Stackoffset _ -> false
  | Load _ -> true
  | Store _ -> false
  | Intop _ -> true
  | Intop_imm _ -> true
  | Intop_atomic _ -> false
  | Floatop _ -> true
  | Csel _ -> true
  | Reinterpret_cast
      ( V128_of_v128 | Float32_of_float | Float32_of_int32 | Float_of_float32
      | Float_of_int64 | Int64_of_float | Int32_of_float32 ) ->
    true
  | Static_cast _ -> true
  (* Conservative to ensure valueofint/intofvalue are not eliminated before
     emit. *)
  | Reinterpret_cast (Value_of_int | Int_of_value) -> false
  | Probe_is_enabled _ -> true
  | Opaque -> false
  | Begin_region -> false
  | End_region -> false
  | Specific s ->
    assert (not (Arch.operation_can_raise s));
    Arch.operation_is_pure s
  | Name_for_debugger _ -> false
  | Dls_get -> true
  | Poll -> false
  | Alloc _ -> false

(* The next 2 functions are copied almost as is from asmcomp/printmach.ml
   because there is no interface to call them. Eventually this won't be needed
   when we change cfg to have its own types rather than referring back to mach
   and cmm. *)
(* CR-someday gyorsh: implement desc printing, and args/res/dbg, etc, properly,
   with regs, use the dreaded Format. *)

let intcomp (comp : Simple_operation.integer_comparison) =
  match comp with
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let intop_atomic (op : Cmm.atomic_op) =
  match op with Fetch_and_add -> " += " | Compare_and_swap -> " cas "

let intop (op : Simple_operation.integer_operation) =
  match op with
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh { signed : bool } -> " *h" ^ if signed then " " else "u "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior -> " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Ipopcnt -> " pop "
  | Iclz _ -> " clz "
  | Ictz _ -> " ctz "
  | Icomp cmp -> intcomp cmp

let floatop ppf (op : Simple_operation.float_operation) =
  match op with
  | Iaddf -> Format.fprintf ppf "+."
  | Isubf -> Format.fprintf ppf "-."
  | Imulf -> Format.fprintf ppf "*."
  | Idivf -> Format.fprintf ppf "/."
  | Iabsf -> Format.fprintf ppf "abs"
  | Inegf -> Format.fprintf ppf "neg"
  | Icompf cmp -> Format.fprintf ppf "%s" (Printcmm.float_comparison cmp)

let dump ppf op =
  match op with
  | Move -> Format.fprintf ppf "mov"
  | Spill -> Format.fprintf ppf "spill"
  | Reload -> Format.fprintf ppf "reload"
  | Const_int n -> Format.fprintf ppf "const_int %nd" n
  | Const_float32 f ->
    Format.fprintf ppf "const_float32 %Fs" (Int32.float_of_bits f)
  | Const_float f -> Format.fprintf ppf "const_float %F" (Int64.float_of_bits f)
  | Const_symbol s -> Format.fprintf ppf "const_symbol %s" s.sym_name
  | Const_vec128 { high; low } ->
    Format.fprintf ppf "const vec128 %016Lx:%016Lx" high low
  | Stackoffset n -> Format.fprintf ppf "stackoffset %d" n
  | Load _ -> Format.fprintf ppf "load"
  | Store _ -> Format.fprintf ppf "store"
  | Intop op -> Format.fprintf ppf "intop %s" (intop op)
  | Intop_imm (op, n) -> Format.fprintf ppf "intop %s %d" (intop op) n
  | Intop_atomic { op; size = _; addr = _ } ->
    Format.fprintf ppf "intop atomic %s" (intop_atomic op)
  | Floatop (Float64, op) -> Format.fprintf ppf "floatop %a" floatop op
  | Floatop (Float32, op) -> Format.fprintf ppf "float32op %a" floatop op
  | Csel _ -> Format.fprintf ppf "csel"
  | Reinterpret_cast cast ->
    Format.fprintf ppf "%s" (Printcmm.reinterpret_cast cast)
  | Static_cast cast -> Format.fprintf ppf "%s" (Printcmm.static_cast cast)
  | Specific _ -> Format.fprintf ppf "specific"
  | Probe_is_enabled { name } -> Format.fprintf ppf "probe_is_enabled %s" name
  | Opaque -> Format.fprintf ppf "opaque"
  | Begin_region -> Format.fprintf ppf "beginregion"
  | End_region -> Format.fprintf ppf "endregion"
  | Name_for_debugger _ -> Format.fprintf ppf "name_for_debugger"
  | Dls_get -> Format.fprintf ppf "dls_get"
  | Poll -> Format.fprintf ppf "poll"
  | Alloc { bytes; dbginfo = _; mode = Heap } ->
    Format.fprintf ppf "alloc %i" bytes
  | Alloc { bytes; dbginfo = _; mode = Local } ->
    Format.fprintf ppf "alloc_local %i" bytes
