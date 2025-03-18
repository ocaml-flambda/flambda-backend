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

open Simd

(* SIMD instruction selection for ARM64 *)

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(* Intrinsics naming conventions:

   "caml_simd_*" for intrinsics used in the compiler distribution libraries, for
   example "caml_simd_float32_round_current" and "caml_simd_float32_min", or in
   compiler tests, for example "caml_simd_vec128_interleave_low_32". The
   behavior must match the corresponding amd64 intrinsics, and the name usually
   matches the corresponding "caml_sse*" intrinsic.

   "caml_neon_<type>_<mnemonic>" for example scalar type
   "caml_neon_float32_fmax" and vector type "caml_neon_float64x2_zip1" where the
   constructor such as [Zip1q_f64] matches the naming convention used by the
   standard arm intrinsics.

   Some intrinsics have both names to make it easier to correlate with both
   amd64 intrinsics and arm64 instructions, depending on context. *)

let select_simd_instr op args =
  match op with
  | "caml_simd_float32_round_neg_inf" -> Some (Round_f32 Neg_inf, args)
  | "caml_simd_float32_round_pos_inf" -> Some (Round_f32 Pos_inf, args)
  | "caml_simd_float32_round_towards_zero" -> Some (Round_f32 Zero, args)
  | "caml_simd_float32_round_current" -> Some (Round_f32 Current, args)
  | "caml_simd_cast_float32_int64" -> Some (Round_f32_i64, args)
  | "caml_simd_float32_min" -> Some (Min_scalar_f32, args)
  | "caml_simd_float32_max" -> Some (Max_scalar_f32, args)
  | "caml_neon_float32_fmin" -> Some (Fmin_f32, args)
  | "caml_neon_float32_fmax" -> Some (Fmax_f32, args)
  | "caml_neon_float32x2_zip1" -> Some (Zip1_f32, args)
  | "caml_simd_vec128_interleave_low_32" | "caml_neon_float32x4_zip1" ->
    Some (Zip1q_f32, args)
  | "caml_simd_vec128_interleave_low_64" | "caml_neon_float64x2_zip1" ->
    Some (Zip1q_f64, args)
  | "caml_simd_vec128_interleave_high_64" | "caml_neon_float64x2_zip2" ->
    Some (Zip2q_f64, args)
  | "caml_simd_int64x2_add" | "caml_neon_int64x2_add" -> Some (Addq_i64, args)
  | "caml_simd_int64x2_sub" | "caml_neon_int64x2_sub" -> Some (Subq_i64, args)
  | _ -> None

let select_operation_cfg op args =
  select_simd_instr op args
  |> Option.map (fun (op, args) -> Operation.Specific (Isimd op), args)

let pseudoregs_for_operation _ arg res = arg, res

(* See `amd64/simd_selection.ml`. *)

let vector_width_in_bits = 128

let vectorize_operation _ ~arg_count:_ ~res_count:_ ~alignment_in_bytes:_
    (_ : Operation.t list) :
    Vectorize_utils.Vectorized_instruction.t list option =
  None
