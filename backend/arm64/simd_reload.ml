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

[@@@ocaml.warning "+a-40-42"]

(* SIMD instruction reload for ARM64 *)

open! Int_replace_polymorphic_compare [@@warning "-66"]

let reload_operation makereg op arg res =
  let stackp r =
    match r.Reg.loc with Stack _ -> true | Reg _ | Unknown -> false
  in
  let ensure_reg reg = if stackp reg then makereg reg else reg in
  match Simd_proc.register_behavior op with
  (* Argument and result must be in registers. *)
  | Rf32_Rf32_to_Rf32 | Rf32_to_Rf32 | Rf32_to_Ri64 | Rf32x2_Rf32x2_to_Rf32x2
  | Rf32x4_Rf32x4_to_Rf32x4 | Rf64x2_Rf64x2_to_Rf64x2 | Ri64x2_Ri64x2_to_Ri64x2
    ->
    Array.map ensure_reg arg, Array.map ensure_reg res
