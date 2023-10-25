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

open! Asm_targets
open! Dwarf_low
open! Dwarf_high
module ARV = Available_ranges_vars
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear
module SLDL = Simple_location_description_lang
module V = Backend_var

type proto_dies_for_var =
  { value_die_lvalue : Proto_die.reference;
    type_die : Proto_die.reference
  }

let arch_size_addr = Targetint.of_int_exn Arch.size_addr

let proto_dies_for_variable var ~proto_dies_for_vars =
  match Backend_var.Tbl.find proto_dies_for_vars var with
  | exception Not_found -> None
  | result -> Some result

let normal_type_for_var ?reference ~parent ident_for_type is_parameter =
  let name_attribute =
    match ident_for_type with
    | None -> []
    | Some (compilation_unit, var) ->
      let name =
        Dwarf_name_laundry.base_type_die_name_for_var compilation_unit var
          is_parameter
      in
      [DAH.create_name name]
  in
  (* CR mshinwell: This should not create duplicates when the name is missing *)
  Proto_die.create ?reference ~parent ~tag:Base_type
    ~attribute_values:
      (name_attribute
      @ [ DAH.create_encoding ~encoding:Encoding_attribute.signed;
          DAH.create_byte_size_exn ~byte_size:Arch.size_addr ])
    ()

let type_die_reference_for_var var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some dies -> Some dies.type_die

let construct_type_of_value_description state ~parent ident_for_type
    is_parameter ~proto_dies_for_vars ~reference =
  normal_type_for_var ~reference ~parent ident_for_type is_parameter

type location_description =
  | Simple of Simple_location_description.t
  | Composite of Composite_location_description.t

let _to_silence_warning x = Composite x

let reg_location_description reg ~offset ~need_rvalue :
    location_description option =
  match
    Dwarf_reg_locations.reg_location_description reg ~offset ~need_rvalue
  with
  | None -> None
  | Some simple_loc_desc -> Some (Simple simple_loc_desc)

let single_location_description state ~parent ~subrange ~proto_dies_for_vars
    ~need_rvalue =
  let location_description =
    let subrange_info = ARV.Subrange.info subrange in
    let reg = ARV.Subrange_info.reg subrange_info in
    let offset = ARV.Subrange_info.offset subrange_info in
    reg_location_description reg ~offset ~need_rvalue
  in
  match location_description with
  | None -> None
  | Some (Simple simple) ->
    Some (Single_location_description.of_simple_location_description simple)
  | Some (Composite composite) ->
    Some
      (Single_location_description.of_composite_location_description composite)

type location_list_entry =
  | Dwarf_4 of Dwarf_4_location_list_entry.t
  | Dwarf_5 of Location_list_entry.t

let location_list_entry state ~subrange single_location_description :
    location_list_entry =
  let start_pos = Asm_label.create_int Text (ARV.Subrange.start_pos subrange) in
  let start_pos_offset = ARV.Subrange.start_pos_offset subrange in
  let end_pos = Asm_label.create_int Text (ARV.Subrange.end_pos subrange) in
  let end_pos_offset = ARV.Subrange.end_pos_offset subrange in
  match !Dwarf_flags.gdwarf_version with
  | Four ->
    let location_list_entry =
      Dwarf_4_location_list_entry.create_location_list_entry
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
        ~first_address_when_in_scope:start_pos
        ~first_address_when_in_scope_offset:(Some start_pos_offset)
        ~first_address_when_not_in_scope:end_pos
        ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
        ~single_location_description
    in
    Dwarf_4 location_list_entry
  | Five ->
    let start_inclusive =
      Address_table.add (DS.address_table state) start_pos
        ~adjustment:start_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
    in
    let end_exclusive =
      Address_table.add (DS.address_table state) end_pos
        ~adjustment:end_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
    in
    let loc_desc =
      Counted_location_description.create single_location_description
    in
    let location_list_entry : Location_list_entry.entry =
      (* DWARF-5 spec page 45 line 1. *)
      Startx_endx { start_inclusive; end_exclusive; payload = loc_desc }
    in
    Dwarf_5
      (Location_list_entry.create location_list_entry
         ~start_of_code_symbol:(DS.start_of_code_symbol state))

let dwarf_for_variable state ~function_proto_die ~proto_dies_for_vars
    (var : Backend_var.t) ~ident_for_type ~range =
  let range_info = ARV.Range.info range in
  let provenance = ARV.Range_info.provenance range_info in
  let (parent_proto_die : Proto_die.t), hidden =
    match provenance with
    | None ->
      (* Any variable (e.g. try_region) or parameter (e.g. my_closure) without
         provenance gets hidden. *)
      function_proto_die, true
    | Some _provenance -> function_proto_die, false
  in
  let location_attribute_value, location_list_in_debug_loc_table =
    (* Build a location list that identifies where the value of [var] may be
       found at runtime, indexed by program counter range. The representations
       of location lists (and range lists, used below to describe lexical
       blocks) changed completely between DWARF-4 and DWARF-5. *)
    let dwarf_4_location_list_entries, location_list =
      ARV.Range.fold range
        ~init:([], Location_list.create ())
        ~f:(fun (dwarf_4_location_list_entries, location_list) subrange ->
          let single_location_description =
            single_location_description state ~parent:(Some function_proto_die)
              ~subrange ~proto_dies_for_vars ~need_rvalue:false
          in
          match single_location_description with
          | None -> dwarf_4_location_list_entries, location_list
          | Some single_location_description -> (
            let location_list_entry =
              location_list_entry state ~subrange single_location_description
            in
            match location_list_entry with
            | Dwarf_4 location_list_entry ->
              let dwarf_4_location_list_entries =
                location_list_entry :: dwarf_4_location_list_entries
              in
              dwarf_4_location_list_entries, location_list
            | Dwarf_5 location_list_entry ->
              let location_list =
                Location_list.add location_list location_list_entry
              in
              dwarf_4_location_list_entries, location_list))
    in
    match !Dwarf_flags.gdwarf_version with
    | Four ->
      let location_list_entries = dwarf_4_location_list_entries in
      let location_list = Dwarf_4_location_list.create ~location_list_entries in
      ( [Debug_loc_table.attribute_to_reference_location_list location_list],
        Some location_list )
    | Five ->
      let location_list_index =
        Location_list_table.add (DS.location_list_table state) location_list
      in
      [DAH.create_location location_list_index], None
  in
  let is_parameter = ARV.Range_info.is_parameter range_info in
  let type_and_name_attributes =
    match type_die_reference_for_var var ~proto_dies_for_vars with
    | None -> []
    | Some reference ->
      let name_for_var =
        (* For the moment assume function parameter names are unique, they
           almost always will be, and avoiding the stamps looks much better in
           the debugger. *)
        match is_parameter with
        | Parameter _ -> Backend_var.name_for_debugger var
        | Local -> Backend_var.unique_name_for_debugger var
      in
      let type_attribute =
        [ DAH.create_type_from_reference
            ~proto_die_reference:
              (Proto_die.reference (DS.value_type_proto_die state)) ]
      in
      let name_attribute = [DAH.create_name name_for_var] in
      name_attribute @ type_attribute
  in
  let tag : Dwarf_tag.t =
    match is_parameter with
    | Parameter _index ->
      (* The lvalue DIE is the "normal" one for variables and parameters; it is
         the one that is marked with a name, for example. To avoid erroneous
         display of, or confusion around, rvalue DIEs we always mark them as
         variables not parameters. *)
      Formal_parameter
    | Local -> Variable
  in
  let reference =
    match proto_dies_for_variable var ~proto_dies_for_vars with
    | None -> None
    | Some proto_dies -> Some proto_dies.value_die_lvalue
  in
  let sort_priority =
    match is_parameter with
    | Local -> None
    | Parameter { index } ->
      (* Ensure that parameters appear in the correct order in the debugger. *)
      Some index
  in
  if not hidden
  then
    Proto_die.create_ignore ?reference ?sort_priority
      ?location_list_in_debug_loc_table ~parent:(Some parent_proto_die) ~tag
      ~attribute_values:(type_and_name_attributes @ location_attribute_value)
      ()

let iterate_over_variable_like_things state ~available_ranges_vars ~f =
  ARV.iter available_ranges_vars ~f:(fun var range ->
      let ident_for_type = Some (Compilation_unit.get_current_exn (), var) in
      f var ~ident_for_type ~range)

let dwarf state ~function_proto_die available_ranges_vars =
  let proto_dies_for_vars = Backend_var.Tbl.create 42 in
  iterate_over_variable_like_things state ~available_ranges_vars
    ~f:(fun var ~ident_for_type:_ ~range:_ ->
      let value_die_lvalue = Proto_die.create_reference () in
      let type_die = Proto_die.create_reference () in
      assert (not (Backend_var.Tbl.mem proto_dies_for_vars var));
      Backend_var.Tbl.add proto_dies_for_vars var { value_die_lvalue; type_die });
  iterate_over_variable_like_things state ~available_ranges_vars
    ~f:(dwarf_for_variable state ~function_proto_die ~proto_dies_for_vars)
