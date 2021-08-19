[@@@ocaml.warning "+a-4-30-40-41-42"]

open Dwarf_high

type fundecl = { fun_name : string }

module DAH = Dwarf_attribute_helpers

let for_fundecl ~params:(module Params : Dwarf_params.S) state { fun_name } =
  let parent = Dwarf_state.compilation_unit_proto_die state in
  let concrete_instance_proto_die =
    Proto_die.create ~parent:(Some parent)
      ~tag:Subprogram
      ~attribute_values:[ DAH.create_linkage_name ~linkage_name:fun_name ]
      ()
  in
  Proto_die.set_name concrete_instance_proto_die
    (Asm_symbol.create ~make_symbol:Params.make_symbol fun_name);