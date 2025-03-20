(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-42"]

(* SIMD register behavior for ARM64 *)

open! Int_replace_polymorphic_compare [@@warning "-66"]

(* [R] stands for register (not stack)
 *
 * [f32] Float32 in scalar register <Sn>
 * [i32] Int in general purpose register  <Wn>
 * [i64] Int in general purpose register <Xn>
 * [f32x2] vector of two Float32 values represented
 *  using machtype Float and emitted in vector register Vd.2S
 *)

(* CR gyorsh: should it be named using the corresponding arm64 vector reg names?
   [RS] [RW] [RX] [R2S] and so on, instead of the above.

   The type refers to the hard register to be used, not to the machtype of the
   corresponding Reg.t. *)
type register_behavior =
  (* vector *)
  | Rf32x2_Rf32x2_to_Rf32x2
  | Rf32x4_Rf32x4_to_Rf32x4
  | Rf64x2_Rf64x2_to_Rf64x2
  (* scalar *)
  | Rf32_Rf32_to_Rf32
  | Rf32_to_Rf32
  | Rf32_to_Ri64
  | Ri64x2_Ri64x2_to_Ri64x2

let register_behavior (op : Simd.operation) =
  match op with
  (* unary *)
  | Round_f32_i64 -> Rf32_to_Ri64
  | Round_f32 _ -> Rf32_to_Rf32
  (* binary *)
  | Fmin_f32 | Fmax_f32 | Min_scalar_f32 | Max_scalar_f32 -> Rf32_Rf32_to_Rf32
  | Zip1_f32 -> Rf32x2_Rf32x2_to_Rf32x2
  | Zip1q_f32 -> Rf32x4_Rf32x4_to_Rf32x4
  | Zip1q_f64 | Zip2q_f64 -> Rf64x2_Rf64x2_to_Rf64x2
  | Addq_i64 | Subq_i64 -> Ri64x2_Ri64x2_to_Ri64x2
