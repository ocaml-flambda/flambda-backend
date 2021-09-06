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

open Dwarf_high

module DAH = Dwarf_attribute_helpers

let compile_unit_proto_die ~sourcefile ~unit_name =
  let cwd = Sys.getcwd () in
  let source_directory_path, source_filename =
    if Filename.is_relative sourcefile then cwd, sourcefile
    else Filename.dirname sourcefile, Filename.basename sourcefile
  in
  let source_directory_path =
    Location.rewrite_absolute_path source_directory_path
  in
  let attribute_values =
    [ DAH.create_name source_filename
    ; DAH.create_comp_dir source_directory_path
      (* The [OCaml] attribute value here is only defined in DWARF-5, but
         it doesn't mean anything else in DWARF-4, so we always emit it.
         This saves special-case logic in gdb based on the producer name. *)
    ; DAH.create_language OCaml
    ; DAH.create_producer "ocamlopt"
    ; DAH.create_ocaml_unit_name unit_name
    ; DAH.create_ocaml_compiler_version Sys.ocaml_version
    ; DAH.create_stmt_list ~debug_line_label:(Asm_label.for_dwarf_section Asm_section.Debug_line)
    ] 
  in
  Proto_die.create ~parent:None
    ~tag:Compile_unit
    ~attribute_values
    ()
