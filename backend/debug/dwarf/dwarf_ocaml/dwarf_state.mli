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

(** State that is shared amongst the various dwarf_* modules. *)

open Asm_targets
open Dwarf_low
open Dwarf_high

type t

val create :
  compilation_unit_header_label:Asm_label.t ->
  compilation_unit_proto_die:Proto_die.t ->
  value_type_proto_die:Proto_die.t ->
  start_of_code_symbol:Asm_symbol.t ->
  Debug_loc_table.t ->
  Debug_ranges_table.t ->
  Address_table.t ->
  Location_list_table.t ->
  get_file_num:(string -> int) ->
  t

val compilation_unit_header_label : t -> Asm_label.t

val compilation_unit_proto_die : t -> Proto_die.t

val value_type_proto_die : t -> Proto_die.t

val start_of_code_symbol : t -> Asm_symbol.t

val debug_loc_table : t -> Debug_loc_table.t

val debug_ranges_table : t -> Debug_ranges_table.t

val address_table : t -> Address_table.t

val location_list_table : t -> Location_list_table.t

val function_abstract_instances :
  t -> (Proto_die.t * Asm_symbol.t) Asm_symbol.Tbl.t

val can_reference_dies_across_units : t -> bool

val get_file_num : t -> string -> int

module Debug : sig
  val log : ('a, Format.formatter, unit) format -> 'a
end
