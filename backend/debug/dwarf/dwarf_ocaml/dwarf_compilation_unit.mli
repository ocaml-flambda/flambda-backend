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

(** Construction of DWARF "compile_unit" DIEs and associated entities. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets
open Dwarf_high

val compile_unit_proto_die :
  sourcefile:string ->
  unit_name:Ident.t ->
  code_begin:Asm_symbol.t ->
  code_end:Asm_symbol.t ->
  Proto_die.t
