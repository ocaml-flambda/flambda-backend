(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets
open Dwarf_high
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear

let for_fundecl ~get_file_id state (fundecl : L.fundecl) ~fun_end_label
    available_ranges_vars inlined_frame_ranges =
  let parent = Dwarf_state.compilation_unit_proto_die state in
  let fun_name = fundecl.fun_name in
  let loc = Debuginfo.to_location fundecl.fun_dbg in
  let linkage_name =
    match Debuginfo.Dbg.to_list (Debuginfo.get_dbg fundecl.fun_dbg) with
    | [item] ->
      Debuginfo.Scoped_location.string_of_scopes item.dinfo_scopes
      |> Misc.remove_double_underscores
    (* XXX Not sure what to do in the cases below *)
    | [] | _ :: _ -> fun_name
  in
  let start_sym = Asm_symbol.create fun_name in
  let location_attributes =
    if Location.is_none loc
    then [DAH.create_artificial ()]
    else
      let file, line, startchar = Location.get_pos_info loc.loc_start in
      let attributes = [DAH.create_decl_file (get_file_id file)] in
      if line < 0
      then attributes
      else if startchar < 0
      then DAH.create_decl_line line :: attributes
      else
        (* Both line and startchar are >= 0 *)
        DAH.create_decl_line line
        :: DAH.create_decl_column startchar
        :: attributes
  in
  let _abstract_instance_root_proto_die, _abstract_instance_root_symbol =
    (* Add the abstract instance root for this function *)
    DS.Debug.log "*** Adding absint root for %s\n%!" fundecl.fun_name;
    Dwarf_abstract_instances.add_root state ~parent ~demangled_name:linkage_name
      start_sym ~location_attributes
  in
  let attribute_values =
    [ DAH.create_low_pc_from_symbol start_sym;
      DAH.create_high_pc ~low_pc:start_sym fun_end_label;
      (* CR mshinwell: Probably no need to set this at the moment since the low
         PC value should be assumed, which is correct. *)
      DAH.create_entry_pc_from_symbol start_sym;
      DAH.create_stmt_list
        ~debug_line_label:(Asm_label.for_dwarf_section Asm_section.Debug_line);
      DAH.create_abstract_origin ~die_symbol:_abstract_instance_root_symbol ]
  in
  let concrete_instance_proto_die =
    Proto_die.create ~parent:(Some parent) ~tag:Subprogram ~attribute_values ()
  in
  let _inlined_frame_proto_dies =
    Profile.record "dwarf_inlined_frames"
      (fun () ->
        Dwarf_inlined_frames.dwarf state fundecl
          ~function_proto_die:concrete_instance_proto_die inlined_frame_ranges)
      ~accumulate:true ()
  in
  if not !Dwarf_flags.restrict_to_upstream_dwarf
  then
    Profile.record "dwarf_variables_and_parameters"
      (fun () ->
        Dwarf_variables_and_parameters.dwarf state
          ~function_proto_die:concrete_instance_proto_die available_ranges_vars)
      ~accumulate:true ();
  (* CR mshinwell: When cross-referencing of DIEs across files is necessary we
     need to be careful about symbol table size. let name = Printf.sprintf
     "__concrete_instance_%s" fun_name in Proto_die.set_name
     concrete_instance_proto_die (Asm_symbol.create name) *)
  ()
