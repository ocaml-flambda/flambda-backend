(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

let stack_threshold_size = 0

let frame_size
  : stack_offset:int -> frame_required:bool -> num_stack_slots:int array ->  int
  = fun ~stack_offset:_ ~frame_required:_ ~num_stack_slots:_ ->
  Misc.fatal_error "stack checks are not supported on arm64"

let linear
  : Linear.fundecl -> Linear.fundecl
  = fun fundecl ->
  match Config.runtime5 with
  | false -> fundecl
  | true ->
    Misc.fatal_error "stack checks are not supported on arm64"
