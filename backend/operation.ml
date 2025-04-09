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

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

type trap_stack =
  | Uncaught
  | Specific_trap of Cmm.trywith_shared_label * trap_stack

let rec equal_trap_stack ts1 ts2 =
  match ts1, ts2 with
  | Uncaught, Uncaught -> true
  | Specific_trap (lbl1, ts1), Specific_trap (lbl2, ts2) ->
    Int.equal lbl1 lbl2 && equal_trap_stack ts1 ts2
  | Uncaught, Specific_trap _ | Specific_trap _, Uncaught -> false

type integer_comparison =
  | Isigned of Cmm.integer_comparison
  | Iunsigned of Cmm.integer_comparison

let string_of_integer_comparison = function
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let equal_integer_comparison left right =
  match left, right with
  | Isigned left, Isigned right -> Cmm.equal_integer_comparison left right
  | Iunsigned left, Iunsigned right -> Cmm.equal_integer_comparison left right
  | Isigned _, Iunsigned _ | Iunsigned _, Isigned _ -> false

let invert_integer_comparison = function
  | Isigned cmp -> Isigned (Cmm.negate_integer_comparison cmp)
  | Iunsigned cmp -> Iunsigned (Cmm.negate_integer_comparison cmp)

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

let string_of_integer_operation = function
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh { signed } -> " *h " ^ if signed then "" else "u"
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior -> " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Iclz { arg_is_non_zero } -> Printf.sprintf "clz %B " arg_is_non_zero
  | Ictz { arg_is_non_zero } -> Printf.sprintf "ctz %B " arg_is_non_zero
  | Ipopcnt -> "popcnt "
  | Icomp cmp -> string_of_integer_comparison cmp

let is_unary_integer_operation = function
  | Iclz _ | Ictz _ | Ipopcnt -> true
  | Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
  | Iasr | Icomp _ ->
    false

let equal_integer_operation left right =
  match left, right with
  | Iadd, Iadd -> true
  | Isub, Isub -> true
  | Imul, Imul -> true
  | Imulh { signed = left }, Imulh { signed = right } -> Bool.equal left right
  | Idiv, Idiv -> true
  | Imod, Imod -> true
  | Iand, Iand -> true
  | Ior, Ior -> true
  | Ixor, Ixor -> true
  | Ilsl, Ilsl -> true
  | Ilsr, Ilsr -> true
  | Iasr, Iasr -> true
  | ( Iclz { arg_is_non_zero = left_arg_is_non_zero },
      Iclz { arg_is_non_zero = right_arg_is_non_zero } ) ->
    Bool.equal left_arg_is_non_zero right_arg_is_non_zero
  | ( Ictz { arg_is_non_zero = left_arg_is_non_zero },
      Ictz { arg_is_non_zero = right_arg_is_non_zero } ) ->
    Bool.equal left_arg_is_non_zero right_arg_is_non_zero
  | Ipopcnt, Ipopcnt -> true
  | Icomp left, Icomp right -> equal_integer_comparison left right
  | ( Iadd,
      ( Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Isub,
      ( Iadd | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Imul,
      ( Iadd | Isub | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Imulh _,
      ( Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Idiv,
      ( Iadd | Isub | Imul | Imulh _ | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Imod,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Iand,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ior,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ixor,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ilsl,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ilsr,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Iasr,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Iclz _,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ictz _,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Iclz _ | Ipopcnt | Icomp _ ) )
  | ( Ipopcnt,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Iclz _ | Ictz _ | Icomp _ ) )
  | ( Icomp _,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt ) ) ->
    false

type float_width = Cmm.float_width

let equal_float_width = Cmm.equal_float_width

type float_comparison = Cmm.float_comparison

let equal_float_comparison = Cmm.equal_float_comparison

type float_operation =
  | Inegf
  | Iabsf
  | Iaddf
  | Isubf
  | Imulf
  | Idivf
  | Icompf of float_comparison

let string_of_float_operation = function
  | Iaddf -> "+."
  | Isubf -> "-."
  | Imulf -> "*."
  | Idivf -> "/."
  | Iabsf -> "abs"
  | Inegf -> "neg"
  | Icompf cmp -> Printcmm.float_comparison cmp

let format_float_operation ppf op =
  Format.fprintf ppf "%s" (string_of_float_operation op)

let equal_float_operation left right =
  match left, right with
  | Inegf, Inegf -> true
  | Iabsf, Iabsf -> true
  | Iaddf, Iaddf -> true
  | Isubf, Isubf -> true
  | Imulf, Imulf -> true
  | Idivf, Idivf -> true
  | Icompf left, Icompf right -> equal_float_comparison left right
  | Inegf, (Iabsf | Iaddf | Isubf | Imulf | Idivf | Icompf _)
  | Iabsf, (Inegf | Iaddf | Isubf | Imulf | Idivf | Icompf _)
  | Iaddf, (Inegf | Iabsf | Isubf | Imulf | Idivf | Icompf _)
  | Isubf, (Inegf | Iabsf | Iaddf | Imulf | Idivf | Icompf _)
  | Imulf, (Inegf | Iabsf | Iaddf | Isubf | Idivf | Icompf _)
  | Idivf, (Inegf | Iabsf | Iaddf | Isubf | Imulf | Icompf _)
  | Icompf _, (Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf) ->
    false

type mutable_flag =
  | Immutable
  | Mutable

let equal_mutable_flag left right =
  match left, right with
  | Immutable, Immutable | Mutable, Mutable -> true
  | (Immutable | Mutable), _ -> false

let of_ast_mutable_flag : Asttypes.mutable_flag -> mutable_flag = function
  | Immutable -> Immutable
  | Mutable -> Mutable

let to_ast_mutable_flag : mutable_flag -> Asttypes.mutable_flag = function
  | Immutable -> Immutable
  | Mutable -> Mutable

type test =
  | Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of float_width * float_comparison
  | Ioddtest
  | Ieventest

let format_test ~print_reg tst ppf arg =
  let reg = print_reg in
  match tst with
  | Itruetest -> reg ppf arg.(0)
  | Ifalsetest -> Format.fprintf ppf "not %a" reg arg.(0)
  | Iinttest cmp ->
    Format.fprintf ppf "%a%s%a" reg arg.(0)
      (string_of_integer_comparison cmp)
      reg arg.(1)
  | Iinttest_imm (cmp, n) ->
    Format.fprintf ppf "%a%s%i" reg arg.(0) (string_of_integer_comparison cmp) n
  | Ifloattest (_, cmp) ->
    Format.fprintf ppf "%a %s %a" reg arg.(0)
      (Printcmm.float_comparison cmp)
      reg arg.(1)
  | Ieventest -> Format.fprintf ppf "%a & 1 == 0" reg arg.(0)
  | Ioddtest -> Format.fprintf ppf "%a & 1 == 1" reg arg.(0)

let invert_test = function
  | Itruetest -> Ifalsetest
  | Ifalsetest -> Itruetest
  | Iinttest cmp -> Iinttest (invert_integer_comparison cmp)
  | Iinttest_imm (cmp, n) -> Iinttest_imm (invert_integer_comparison cmp, n)
  | Ifloattest (w, cmp) -> Ifloattest (w, Cmm.negate_float_comparison cmp)
  | Ieventest -> Ioddtest
  | Ioddtest -> Ieventest

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
  | Specific s -> Arch.operation_is_pure s
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

let intcomp (comp : integer_comparison) =
  match comp with
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let intop (op : integer_operation) =
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

let floatop ppf (op : float_operation) =
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
    Format.fprintf ppf "intop atomic %s" (Printcmm.atomic_op op)
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
