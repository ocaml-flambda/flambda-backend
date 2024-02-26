(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* SIMD instruction reload for AMD64 *)

let reload_operation makereg op arg res =
  let stackp r =
    match r.Reg.loc with Stack _ -> true | Reg _ | Unknown -> false
  in
  match Simd_proc.register_behavior op with
  | R_to_fst ->
    (* Argument must be in a register; result must be the argument. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    [| arg0 |], [| arg0 |]
  | R_to_RM ->
    (* Argument must be in a register. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    [| arg0 |], res
  | RM_to_R ->
    (* Result must be in a register. *)
    let res0 = if stackp res.(0) then makereg res.(0) else res.(0) in
    arg, [| res0 |]
  | R_to_R ->
    (* Argument and result must be in registers. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    let res0 = if stackp res.(0) then makereg res.(0) else res.(0) in
    [| arg0 |], [| res0 |]
  | R_RM_to_R ->
    (* First argument and result must be in registers. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    let res0 = if stackp res.(0) then makereg res.(0) else res.(0) in
    [| arg0; arg.(1) |], [| res0 |]
  | R_R_to_fst ->
    (* Both arguments must be registers; the result must be the first arg. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    let arg1 = if stackp arg.(1) then makereg arg.(1) else arg.(1) in
    [| arg0; arg1 |], [| arg0 |]
  | R_RM_to_fst ->
    (* First argument must be a register; the result must be the first arg. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    [| arg0; arg.(1) |], [| arg0 |]
  | R_RM_xmm0_to_fst ->
    (* First argument must be a register; the result must be the first arg.
       Specific register constraints are enforced by selection. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    [| arg0; arg.(1); arg.(2) |], [| arg0 |]
  | R_RM_to_rcx | R_RM_to_xmm0 ->
    (* First argument must be a register. Specific register constraints are
       enforced by selection. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    [| arg0; arg.(1) |], res
  | R_RM_rax_rdx_to_rcx | R_RM_rax_rdx_to_xmm0 ->
    (* First argument must be a register. Specific register constraints are
       enforced by selection. *)
    let arg0 = if stackp arg.(0) then makereg arg.(0) else arg.(0) in
    [| arg0; arg.(1); arg.(2); arg.(3) |], res
