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

open Asm_targets
open Dwarf_low

let emit0_delayed ~asm_directives:_ = ()

let emit0 ~asm_directives ~compilation_unit_proto_die
    ~compilation_unit_header_label ~debug_loc_table ~debug_ranges_table
    ~address_table ~location_list_table =
  let module A = (val asm_directives : Asm_directives.S) in
  (* CR-soon mshinwell: the [compilation_unit_die] member of the record returned
     from [Assign_abbrevs.run] is now unused *)
  let assigned_abbrevs =
    Profile.record "assign_abbrevs"
      (fun () -> Assign_abbrevs.run ~proto_die_root:compilation_unit_proto_die)
      ()
  in
  List.iter
    (fun location_list -> Debug_loc_table.insert debug_loc_table location_list)
    assigned_abbrevs.dwarf_4_location_lists;
  let debug_abbrev_label = Asm_label.for_section (DWARF Debug_abbrev) in
  let debug_info =
    Profile.record "debug_info_section"
      (fun () ->
        Debug_info_section.create ~dies:assigned_abbrevs.dies
          ~debug_abbrev_label ~compilation_unit_header_label)
      ()
  in
  Profile.record "dwarf_world_emit"
    (fun () ->
      A.switch_to_section (DWARF Debug_info);
      Profile.record "debug_info_section"
        (Debug_info_section.emit ~asm_directives)
        debug_info;
      A.switch_to_section (DWARF Debug_abbrev);
      Profile.record "abbreviations_table"
        (Abbreviations_table.emit ~asm_directives)
        assigned_abbrevs.abbrev_table;
      A.switch_to_section (DWARF Debug_str);
      A.emit_cached_strings ();
      match !Dwarf_flags.gdwarf_version with
      | Four ->
        A.switch_to_section (DWARF Debug_loc);
        Profile.record "debug_loc"
          (Debug_loc_table.emit ~asm_directives)
          debug_loc_table;
        A.switch_to_section (DWARF Debug_ranges);
        Profile.record "debug_ranges"
          (Debug_ranges_table.emit ~asm_directives)
          debug_ranges_table
      | Five ->
        Profile.record "addr_table"
          (Address_table.emit ~asm_directives)
          address_table;
        A.switch_to_section (DWARF Debug_loclists);
        Profile.record "loclists_table"
          (Location_list_table.emit ~asm_directives)
          location_list_table)
    ()

let emit ~asm_directives ~compilation_unit_proto_die
    ~compilation_unit_header_label ~debug_loc_table ~debug_ranges_table
    ~address_table ~location_list_table ~basic_block_sections
    ~binary_backend_available =
  if (* CR mshinwell: support function sections *)
     !Clflags.function_sections || basic_block_sections
     (* CR mshinwell: support the internal assembler *)
     || binary_backend_available
  then ()
  else
    emit0 ~asm_directives ~compilation_unit_proto_die
      ~compilation_unit_header_label ~debug_loc_table ~debug_ranges_table
      ~address_table ~location_list_table

let emit_delayed ~asm_directives ~basic_block_sections ~binary_backend_available
    =
  if (* CR mshinwell: support function sections *)
     !Clflags.function_sections || basic_block_sections
     (* CR mshinwell: support the internal assembler *)
     || binary_backend_available
  then ()
  else emit0_delayed ~asm_directives
