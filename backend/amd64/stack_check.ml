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

let fp = Config.with_frame_pointers

(* includes return address *)
let frame_size :
    stack_offset:int -> frame_required:bool -> num_stack_slots:int array -> int
    =
 fun ~stack_offset ~frame_required ~num_stack_slots ->
  if frame_required
  then (
    if num_stack_slots.(2) > 0 then Arch.assert_simd_enabled ();
    let sz =
      stack_offset + 8
      + (8 * num_stack_slots.(0))
      + (8 * num_stack_slots.(1))
      + (16 * num_stack_slots.(2))
      + if fp then 8 else 0
    in
    Misc.align sz 16)
  else stack_offset + 8

let linear : Linear.fundecl -> Linear.fundecl =
 fun fundecl ->
  match Config.runtime5 with
  | false -> fundecl
  | true ->
    let frame_size =
      frame_size ~stack_offset:0 ~frame_required:fundecl.fun_frame_required
        ~num_stack_slots:fundecl.fun_num_stack_slots
    in
    let { Emitaux.max_frame_size; contains_nontail_calls } =
      Emitaux.preproc_stack_check ~fun_body:fundecl.fun_body ~frame_size
        ~trap_size:16
    in
    let insert_stack_check =
      contains_nontail_calls || max_frame_size >= stack_threshold_size
    in
    if insert_stack_check
    then
      let fun_body =
        Linear.instr_cons
          (Lstackcheck { max_frame_size_bytes = max_frame_size })
          [||] [||] ~available_before:fundecl.fun_body.available_before
          ~available_across:fundecl.fun_body.available_across fundecl.fun_body
      in
      { fundecl with fun_body }
    else fundecl
