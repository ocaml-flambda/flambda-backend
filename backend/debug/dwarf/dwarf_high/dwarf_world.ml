(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Dwarf_low

[@@@ocaml.warning "+a-4-30-40-41-42"]

let emit ~params ~compilation_unit_proto_die ~compilation_unit_header_label =
  let module Params = (val params : Dwarf_params.S) in
  let module A = Params.Asm_directives in
  (* CR-soon mshinwell: the [compilation_unit_die] member of the record returned
     from [Assign_abbrevs.run] is now unused *)
  let assigned_abbrevs =
    Profile.record "assign_abbrevs"
      (fun () -> Assign_abbrevs.run ~proto_die_root:compilation_unit_proto_die)
      ()
  in
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
        (Debug_info_section.emit ~params)
        debug_info;
      A.switch_to_section (DWARF Debug_abbrev);
      Profile.record "abbreviations_table"
        (Abbreviations_table.emit ~params)
        assigned_abbrevs.abbrev_table;
      A.switch_to_section (DWARF Debug_str);
      A.emit_cached_strings ())
    ()
