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

val stack_threshold_size : int

val initial_stack_offset :
  num_stack_slots:int array -> contains_calls:bool -> int

(* CR mshinwell: We should maybe remove [frame_required]. For example this isn't
   required on arm64, and on amd64 we should be able to compute it using the
   other parameters here. *)
val frame_size :
  stack_offset:int ->
  frame_required:bool ->
  num_stack_slots:int array ->
  contains_calls:bool ->
  int

val linear : Linear.fundecl -> Linear.fundecl
