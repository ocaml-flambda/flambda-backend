(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Handling of DWARF descriptions of variables and function parameters. *)

open! Dwarf_low
open! Dwarf_high

val normal_type_for_var :
  ?reference:Proto_die.reference ->
  parent:Proto_die.t option ->
  (Compilation_unit.t * Ident.t) option ->
  Is_parameter.t ->
  Proto_die.t

val dwarf :
  Dwarf_state.t ->
  function_proto_die:Proto_die.t ->
  Available_ranges_vars.t ->
  unit
