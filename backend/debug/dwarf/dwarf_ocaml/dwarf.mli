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

(** Generation and emission of DWARF debugging information for OCaml compilation
    units. *)

open Asm_targets

type t

(** Create a value of type [t], which holds all state necessary to emit DWARF
    debugging information for a single compilation unit. The names of certain
    parameters line up with the code in [Asmgen]. The current [Compilation_unit]
    must have been set before calling this function. *)
val create :
  sourcefile:string ->
  unit_name:Ident.t ->
  asm_directives:(module Asm_directives.S) ->
  get_file_id:(string -> int) ->
  code_begin:Asm_symbol.t ->
  code_end:Asm_symbol.t ->
  t

val dwarf_for_fundecl : t -> Dwarf_concrete_instances.fundecl -> unit

val dwarf_for_source_file : t -> file_name:string -> file_num:int -> unit

val dwarf_for_line_number_matrix_row :
  t ->
  instr_address:int ->
  file_num:int ->
  line:int ->
  col:int ->
  discriminator:int option ->
  unit

val debug_line_checkpoint : t -> unit

val debug_line_rollback : t -> unit

(** Write the DWARF information to the assembly file. This should only be called
    once all (in)constants and function declarations have been passed to the
    above functions. *)
val emit : t -> unit
