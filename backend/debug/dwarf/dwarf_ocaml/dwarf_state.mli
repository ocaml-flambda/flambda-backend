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

(** State that is shared amongst the various dwarf_* modules. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets
open Dwarf_high
open Dwarf_low

type t

val create :
  compilation_unit_header_label:Asm_label.t ->
  compilation_unit_proto_die:Proto_die.t ->
  debug_line_section:Debug_line_section.t ->
  t

val compilation_unit_header_label : t -> Asm_label.t

val compilation_unit_proto_die : t -> Proto_die.t

val debug_line_section : t -> Dwarf_low.Debug_line_section.t
