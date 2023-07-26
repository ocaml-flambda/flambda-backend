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

open Asm_targets
open Dwarf_low

[@@@ocaml.warning "+a-4-30-40-41-42"]

let emit ~asm_directives ~compilation_unit_proto_die
    ~compilation_unit_header_label ~debug_line ~debug_frame =
  let module A = (val asm_directives : Asm_directives.S) in
  if Dwarf_flags.debug_thing Dwarf_flags.Debug_source_lines
  then
    Profile.record "dwarf_world_emit"
      (fun () ->
        A.switch_to_section (DWARF Debug_line);
        Profile.record "debug_line_section"
          (Debug_line_section.emit ~asm_directives)
          debug_line)
      ();
  if Dwarf_flags.debug_thing Dwarf_flags.Debug_dwarf_cfi
  then
    Profile.record "dwarf_world_emit"
      (fun () ->
        A.switch_to_section (DWARF Debug_frame);
        Profile.record "debug_frame_section"
          (Debug_frame_section.emit ~asm_directives)
          debug_frame)
      ();
  if not !Dwarf_flags.restrict_to_upstream_dwarf
  then
    (* CR-soon mshinwell: the [compilation_unit_die] member of the record
       returned from [Assign_abbrevs.run] is now unused *)
    let assigned_abbrevs =
      Profile.record "assign_abbrevs"
        (fun () ->
          Assign_abbrevs.run ~proto_die_root:compilation_unit_proto_die)
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
          (Debug_info_section.emit ~asm_directives)
          debug_info;
        A.switch_to_section (DWARF Debug_abbrev);
        Profile.record "abbreviations_table"
          (Abbreviations_table.emit ~asm_directives)
          assigned_abbrevs.abbrev_table;
        A.switch_to_section (DWARF Debug_str);
        A.emit_cached_strings ())
      ()
