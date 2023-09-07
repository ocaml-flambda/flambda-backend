(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helper for emitting the various DWARF sections required for full debugging
    information. *)

open Asm_targets
open Dwarf_low

val emit :
  asm_directives:(module Asm_directives.S) ->
  compilation_unit_proto_die:Proto_die.t ->
  compilation_unit_header_label:Asm_label.t ->
  debug_loc_table:Debug_loc_table.t ->
  debug_ranges_table:Debug_ranges_table.t ->
  address_table:Address_table.t ->
  location_list_table:Location_list_table.t ->
  basic_block_sections:bool ->
  binary_backend_available:bool ->
  unit

val emit_delayed :
  asm_directives:(module Asm_directives.S) ->
  basic_block_sections:bool ->
  binary_backend_available:bool ->
  unit
