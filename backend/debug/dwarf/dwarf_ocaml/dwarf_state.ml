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

open Asm_targets
open Dwarf_low
open Dwarf_high

type t =
  { compilation_unit_header_label : Asm_label.t;
    compilation_unit_proto_die : Proto_die.t;
    value_type_proto_die : Proto_die.t;
    start_of_code_symbol : Asm_symbol.t;
    debug_loc_table : Debug_loc_table.t;
    debug_ranges_table : Debug_ranges_table.t;
    address_table : Address_table.t;
    location_list_table : Location_list_table.t
  }

let create ~compilation_unit_header_label ~compilation_unit_proto_die
    ~value_type_proto_die ~start_of_code_symbol debug_loc_table
    debug_ranges_table address_table location_list_table =
  { compilation_unit_header_label;
    compilation_unit_proto_die;
    value_type_proto_die;
    start_of_code_symbol;
    debug_loc_table;
    debug_ranges_table;
    address_table;
    location_list_table
  }

let compilation_unit_header_label t = t.compilation_unit_header_label

let compilation_unit_proto_die t = t.compilation_unit_proto_die

let value_type_proto_die t = t.value_type_proto_die

let start_of_code_symbol t = t.start_of_code_symbol

let debug_loc_table t = t.debug_loc_table

let debug_ranges_table t = t.debug_ranges_table

let address_table t = t.address_table

let location_list_table t = t.location_list_table
