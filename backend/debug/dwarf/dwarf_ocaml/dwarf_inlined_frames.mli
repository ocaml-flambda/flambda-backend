(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generation of descriptions of lexical blocks and inlined frames in
    DWARF. *)

open! Dwarf_low
open! Dwarf_high

(* val find_scope_die_from_debuginfo : Debuginfo.t ->
   function_proto_die:Proto_die.t -> Proto_die.t option *)

val dwarf :
  Dwarf_state.t ->
  Linear.fundecl ->
  Inlined_frame_ranges.t ->
  function_proto_die:Proto_die.t ->
  Proto_die.t Inlined_frame_ranges.Inlined_frames.Key.Map.t
