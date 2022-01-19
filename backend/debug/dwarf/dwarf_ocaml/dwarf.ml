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

let create ~sourcefile ~unit_name ~asm_directives ~get_file_id =
  begin
    match !Dwarf_flags.gdwarf_format with
    | Thirty_two -> Dwarf_format.set Thirty_two
    | Sixty_four -> Dwarf_format.set Sixty_four
  end;
  let compilation_unit_proto_die =
    Dwarf_compilation_unit.compile_unit_proto_die ~sourcefile ~unit_name
  in
  let compilation_unit_header_label = Asm_label.create (DWARF Debug_info) in
  let state =
    DS.create ~compilation_unit_header_label ~compilation_unit_proto_die
  in
  { state; asm_directives; emitted = false; get_file_id }

let dwarf_for_fundecl t fundecl =
  Dwarf_concrete_instances.for_fundecl ~get_file_id:t.get_file_id t.state
    fundecl

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

let emit t = Profile.record "emit_dwarf" emit t
