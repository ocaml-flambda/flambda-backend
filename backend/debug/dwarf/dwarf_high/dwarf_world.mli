(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helper for emitting the various DWARF sections required for full debugging
    information. *)

open Dwarf_low

[@@@ocaml.warning "+a-4-30-40-41-42"]

val emit :
  params:(module Dwarf_params.S) ->
  compilation_unit_proto_die:Proto_die.t ->
  compilation_unit_header_label:Asm_label.t ->
  unit
