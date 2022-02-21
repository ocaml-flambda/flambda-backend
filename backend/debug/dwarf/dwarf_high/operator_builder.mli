(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functions for constructing DWARF operators including some simple
    optimisations thereon. *)

open Asm_targets
open Dwarf_low

[@@@ocaml.warning "+a-4-30-40-41-42"]

val register_as_lvalue : dwarf_reg_number:int -> Dwarf_operator.t

val contents_of_register : dwarf_reg_number:int -> Dwarf_operator.t

val address_of_stack_slot : offset_in_bytes:Targetint.t -> Dwarf_operator.t list

val contents_of_stack_slot :
  offset_in_bytes:Targetint.t -> Dwarf_operator.t list

val value_of_symbol : symbol:Asm_symbol.t -> Dwarf_operator.t

val signed_int_const : Targetint.t -> Dwarf_operator.t

val add_unsigned_const : Targetint.t -> Dwarf_operator.t list

val float_const : Int64.t -> Dwarf_operator.t

val implicit_pointer :
  offset_in_bytes:Targetint.t ->
  die_label:Asm_label.t ->
  Dwarf_version.t ->
  Dwarf_operator.t

val call :
  die_label:Asm_label.t ->
  compilation_unit_header_label:Asm_label.t ->
  Dwarf_operator.t

val conditional :
  ?at_join:Dwarf_operator.t list ->
  if_zero:Dwarf_operator.t list ->
  if_nonzero:Dwarf_operator.t list ->
  unit ->
  Dwarf_operator.t list
