[@@@ocaml.warning "+a-4-30-40-41-42"]

open Dwarf_high

type fundecl =
  { fun_name : string
  ; fun_dbg : Debuginfo.t
  }

module DAH = Dwarf_attribute_helpers

let end_symbol_name start_symbol = start_symbol ^ "__end"

let for_fundecl ~params:_ state fundecl =
  let parent = Dwarf_state.compilation_unit_proto_die state in
  let fun_name = fundecl.fun_name in
  let linkage_name =
    match fundecl.fun_dbg with 
    | [ item ] -> Debuginfo.Scoped_location.string_of_scopes item.dinfo_scopes
    (* Not sure what to do in the cases below *)
    | [] -> fun_name 
    | _ -> Misc.fatal_errorf "multiple debug entries"
  in
  let start_sym = Asm_symbol.create_no_mangle fun_name in
  let end_sym = Asm_symbol.create_no_mangle (end_symbol_name fun_name) in
  let _concrete_instance_proto_die =
    Proto_die.create ~parent:(Some parent)
      ~tag:Subprogram
      ~attribute_values:
        [ DAH.create_name fun_name
        ; DAH.create_linkage_name ~linkage_name
        ; DAH.create_low_pc_from_symbol start_sym
        ; DAH.create_high_pc_from_symbol ~low_pc:start_sym end_sym
        ; DAH.create_entry_pc_from_symbol start_sym
        ]
      ()
  in
  (* TODO: Fix this name here! *)
  (* Proto_die.set_name concrete_instance_proto_die start_sym; *)
  ()