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

let stack_threshold_size = Config.stack_threshold * 8 (* bytes *)

let stack_offset = 0

let trap_size = 16

let linear : Linear.fundecl -> Linear.fundecl =
 fun fundecl ->
  match Config.runtime5 with
  | false -> fundecl
  | true ->
    Emitaux.add_stack_checks_if_needed fundecl ~stack_offset
      ~stack_threshold_size ~trap_size
