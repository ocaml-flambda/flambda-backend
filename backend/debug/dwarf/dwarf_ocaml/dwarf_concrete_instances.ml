[@@@ocaml.warning "+a-4-30-40-41-42"]

open Dwarf_high

type fundecl =
  { fun_name : string;
    fun_dbg : Debuginfo.t
  }

module DAH = Dwarf_attribute_helpers

let end_symbol_name start_symbol = start_symbol ^ "__end"

let for_fundecl ~params:(module Params : Dwarf_params.S) state fundecl =
  let parent = Dwarf_state.compilation_unit_proto_die state in
  let fun_name = fundecl.fun_name in
  let linkage_name =
    match fundecl.fun_dbg with
    | [item] -> Debuginfo.Scoped_location.string_of_scopes item.dinfo_scopes
    (* Not sure what to do in the cases below *)
    | [] -> fun_name
    | _ -> Misc.fatal_errorf "multiple debug entries"
  in
  let start_sym = Asm_symbol.create fun_name in
  let end_sym = Asm_symbol.create (end_symbol_name fun_name) in
  let file_num, line, startchar =
    let loc = Debuginfo.to_location fundecl.fun_dbg in
    if Location.is_none loc
    then 0, 0, 0
    else
      let file, line, startchar = Location.get_pos_info loc.loc_start in
      Params.get_file_num file, line, startchar
  in
  let concrete_instance_proto_die =
    Proto_die.create ~parent:(Some parent) ~tag:Subprogram
      ~attribute_values:
        [ DAH.create_name fun_name;
          DAH.create_linkage_name ~linkage_name;
          DAH.create_low_pc_from_symbol start_sym;
          DAH.create_high_pc_from_symbol ~low_pc:start_sym end_sym;
          DAH.create_entry_pc_from_symbol start_sym;
          DAH.create_decl_file file_num;
          DAH.create_decl_line line;
          DAH.create_decl_column startchar;
          DAH.create_stmt_list
            ~debug_line_label:
              (Asm_label.for_dwarf_section Asm_section.Debug_line) ]
      ()
  in
  let name = Printf.sprintf "__concrete_instance_%s" fun_name in
  Proto_die.set_name concrete_instance_proto_die (Asm_symbol.create name)
