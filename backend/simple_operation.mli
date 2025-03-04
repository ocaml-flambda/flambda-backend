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
