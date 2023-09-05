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

val for_fundecl :
  get_file_id:(string -> int) ->
  Dwarf_state.t ->
  Linear.fundecl ->
  fun_end_label:Asm_targets.Asm_label.t ->
  Available_ranges_vars.t ->
  Inlined_frame_ranges.t ->
  unit
