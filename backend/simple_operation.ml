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

(* Operations common to Mach and CFG. *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

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
