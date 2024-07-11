(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-42"]

(* SIMD register behavior for AMD64 *)

open Simd

(* This will need to be expanded with the addition of three and four argument
   operations in AVX2 and AVX512. *)
type register_behavior =
  | R_to_fst
  | R_to_R
  | R_to_RM
  | RM_to_R
  | R_R_to_fst
  | R_RM_to_fst
  | R_RM_to_R
  | R_RM_xmm0_to_fst
  | R_RM_rax_rdx_to_rcx
  | R_RM_to_rcx
  | R_RM_rax_rdx_to_xmm0
  | R_RM_to_xmm0

let register_behavior_clmul = function Clmul_64 _ -> R_RM_to_fst

let register_behavior_bmi2 = function Extract_64 | Deposit_64 -> R_RM_to_R

let register_behavior_sse = function
  | Min_scalar_f32 | Max_scalar_f32 | Cmp_f32 _ | Add_f32 | Sub_f32 | Mul_f32
  | Div_f32 | Max_f32 | Min_f32 | Interleave_low_32 | Interleave_high_32
  | Shuffle_32 _ ->
    R_RM_to_fst
  | Round_current_f32_i64 | Sqrt_scalar_f32 | Rcp_f32 | Sqrt_f32 | Rsqrt_f32 ->
    RM_to_R
  | Interleave_low_32_regs | High_64_to_low_64 | Low_64_to_high_64 -> R_R_to_fst
  | Movemask_32 -> R_to_R

let register_behavior_sse2 = function
  | Add_i8 | Add_i16 | Add_i32 | Add_i64 | Add_f64 | Add_saturating_i8
  | Min_scalar_f64 | Max_scalar_f64 | Add_saturating_i16
  | Add_saturating_unsigned_i8 | Add_saturating_unsigned_i16 | Sub_i8 | Sub_i16
  | Sub_i32 | Sub_i64 | Sub_f64 | Sub_saturating_i8 | Sub_saturating_i16
  | Sub_saturating_unsigned_i8 | Sub_saturating_unsigned_i16 | Max_unsigned_i8
  | Max_i16 | Max_f64 | Min_unsigned_i8 | Min_i16 | Min_f64 | Mul_f64 | Div_f64
  | And_bits | Andnot_bits | Or_bits | Xor_bits | Cmpeq_i8 | Cmpeq_i16
  | Cmpeq_i32 | Cmpgt_i8 | Cmpgt_i16 | Cmpgt_i32 | Cmp_f64 _ | SLL_i16 | SLL_i32
  | SLL_i64 | SRL_i16 | SRL_i32 | SRL_i64 | SRA_i16 | SRA_i32 | Avg_unsigned_i8
  | Avg_unsigned_i16 | SAD_unsigned_i8 | Shuffle_64 _ | Interleave_high_8
  | Interleave_high_16 | Interleave_high_64 | Interleave_low_8
  | Interleave_low_16 | Interleave_low_64 | I16_to_i8 | I32_to_i16
  | I16_to_unsigned_i8 | I32_to_unsigned_i16 | Mulhi_i16 | Mulhi_unsigned_i16
  | Mullo_i16 | Mul_hadd_i16_to_i32 ->
    R_RM_to_fst
  | Shuffle_high_16 _ | Shuffle_low_16 _ | I32_to_f64 | I32_to_f32 | F64_to_i32
  | Round_current_f64_i64 | F64_to_f32 | F32_to_i32 | F32_to_f64 | Sqrt_f64
  | Sqrt_scalar_f64 ->
    RM_to_R
  | SLLi_i16 _ | SLLi_i32 _ | SLLi_i64 _ | SRLi_i16 _ | SRLi_i32 _ | SRLi_i64 _
  | SRAi_i16 _ | SRAi_i32 _ | Shift_left_bytes _ | Shift_right_bytes _ ->
    R_to_fst
  | Movemask_8 | Movemask_64 -> R_to_R

let register_behavior_sse3 = function
  | Addsub_f32 | Addsub_f64 | Hadd_f32 | Hadd_f64 | Hsub_f32 | Hsub_f64 ->
    R_RM_to_fst
  | Dup_low_64 | Dup_odd_32 | Dup_even_32 -> RM_to_R

let register_behavior_ssse3 = function
  | Hadd_i16 | Hadd_i32 | Hadd_saturating_i16 | Hsub_i16 | Hsub_i32
  | Hsub_saturating_i16 | Mulsign_i8 | Mulsign_i16 | Mulsign_i32 | Shuffle_8
  | Alignr_i8 _ | Mul_unsigned_hadd_saturating_i8_to_i16 ->
    R_RM_to_fst
  | Abs_i8 | Abs_i16 | Abs_i32 -> RM_to_R

let register_behavior_sse41 = function
  | Blend_16 _ | Blend_32 _ | Blend_64 _ | Cmpeq_i64 | Dp_f32 _ | Dp_f64 _
  | Max_i8 | Max_i32 | Max_unsigned_i16 | Max_unsigned_i32 | Min_i8 | Min_i32
  | Min_unsigned_i16 | Min_unsigned_i32 | Insert_i8 _ | Insert_i16 _
  | Insert_i32 _ | Insert_i64 _ | Multi_sad_unsigned_i8 _ | Mullo_i32 ->
    R_RM_to_fst
  | I8_sx_i16 | I8_sx_i32 | I8_sx_i64 | I16_sx_i32 | I16_sx_i64 | I32_sx_i64
  | I8_zx_i16 | I8_zx_i32 | I8_zx_i64 | I16_zx_i32 | I16_zx_i64 | I32_zx_i64
  | Round_f64 _ | Round_f32 _ | Minpos_unsigned_i16 | Round_scalar_f64 _
  | Round_scalar_f32 _ ->
    RM_to_R
  | Blendv_8 | Blendv_32 | Blendv_64 -> R_RM_xmm0_to_fst
  | Extract_i64 _ | Extract_i32 _ -> R_to_RM
  | Extract_i8 _ | Extract_i16 _ ->
    (* CR mslater: (SIMD): replace once we have int8/int16 *)
    R_to_R

let register_behavior_sse42 = function
  | Crc32_64 | Cmpgt_i64 -> R_RM_to_fst
  | Cmpestrm _ -> R_RM_rax_rdx_to_xmm0
  | Cmpistrm _ -> R_RM_to_xmm0
  | Cmpestra _ | Cmpestrc _ | Cmpestri _ | Cmpestro _ | Cmpestrs _ | Cmpestrz _
    ->
    R_RM_rax_rdx_to_rcx
  | Cmpistra _ | Cmpistrc _ | Cmpistri _ | Cmpistro _ | Cmpistrs _ | Cmpistrz _
    ->
    R_RM_to_rcx

let register_behavior = function
  | CLMUL op -> register_behavior_clmul op
  | BMI2 op -> register_behavior_bmi2 op
  | SSE op -> register_behavior_sse op
  | SSE2 op -> register_behavior_sse2 op
  | SSE3 op -> register_behavior_sse3 op
  | SSSE3 op -> register_behavior_ssse3 op
  | SSE41 op -> register_behavior_sse41 op
  | SSE42 op -> register_behavior_sse42 op
