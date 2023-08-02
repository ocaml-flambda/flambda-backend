(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets
open Dwarf_high
open Dwarf_low
module DS = Dwarf_state

type t =
  { state : DS.t;
    asm_directives : (module Asm_directives.S);
    get_file_id : string -> int;
    mutable emitted : bool
  }

(* CR mshinwell: On OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)

let create ~sourcefile ~unit_name ~asm_directives ~get_file_id ~code_begin
    ~code_end =
  (match !Dwarf_flags.gdwarf_format with
  | Thirty_two -> Dwarf_format.set Thirty_two
  | Sixty_four -> Dwarf_format.set Sixty_four);
  let compilation_unit_proto_die =
    Dwarf_compilation_unit.compile_unit_proto_die ~sourcefile ~unit_name
      ~code_begin ~code_end
  in
  let compilation_unit_header_label = Asm_label.create (DWARF Debug_info) in
  let debug_line_section = Debug_line_section.create ~code_begin in
  let state =
    DS.create ~compilation_unit_header_label ~compilation_unit_proto_die
      ~debug_line_section
  in
  { state; asm_directives; emitted = false; get_file_id }

let dwarf_for_fundecl t fundecl =
  Dwarf_concrete_instances.for_fundecl ~get_file_id:t.get_file_id t.state
    fundecl

let dwarf_for_source_file t ~file_name ~file_num =
  let debug_line_section = DS.debug_line_section t.state in
  Debug_line_section.add_source_file debug_line_section ~file_name ~file_num

let dwarf_for_line_number_matrix_row t ~instr_address ~file_num ~line ~col
    ~discriminator =
  let debug_line_section = DS.debug_line_section t.state in
  Debug_line_section.add_line_number_matrix_row debug_line_section
    ~instr_address ~file_num ~line ~col ~discriminator

let debug_line_checkpoint t =
  Debug_line_section.checkpoint (DS.debug_line_section t.state)

let debug_line_rollback t =
  Debug_line_section.rollback (DS.debug_line_section t.state)

let emit t =
  if t.emitted
  then
    Misc.fatal_error
      "Cannot call [Dwarf.emit] more than once on a given value of type \
       [Dwarf.t]";
  t.emitted <- true;
  Dwarf_world.emit ~asm_directives:t.asm_directives
    ~compilation_unit_proto_die:(DS.compilation_unit_proto_die t.state)
    ~compilation_unit_header_label:(DS.compilation_unit_header_label t.state)
    ~debug_line:(DS.debug_line_section t.state)

let emit t = Profile.record "emit_dwarf" emit t
