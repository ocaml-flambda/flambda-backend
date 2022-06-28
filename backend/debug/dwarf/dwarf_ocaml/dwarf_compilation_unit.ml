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

let compile_unit_proto_die ~sourcefile ~unit_name ~code_begin ~code_end =
  let source_filename =
    (* CR mshinwell: Think more about build reproducibility and what we should
       be putting here (and also in the compilation directory attribute). For
       the moment this is very conservative. *)
    Filename.basename sourcefile
  in
  let attribute_values =
    [ DAH.create_name source_filename;
      (* The [OCaml] attribute value here is only defined in DWARF-5, but it
         doesn't mean anything else in DWARF-4, so we always emit it. This saves
         special-case logic in gdb based on the producer name. *)
      DAH.create_language OCaml;
      DAH.create_producer "ocamlopt";
      DAH.create_ocaml_unit_name unit_name;
      DAH.create_ocaml_compiler_version Sys.ocaml_version;
      DAH.create_stmt_list
        ~debug_line_label:(Asm_label.for_dwarf_section Asm_section.Debug_line);
      DAH.create_low_pc_from_symbol code_begin;
      DAH.create_high_pc_from_symbol ~low_pc:code_begin code_end ]
  in
  Proto_die.create ~parent:None ~tag:Compile_unit ~attribute_values ()
