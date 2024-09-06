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

(* SIMD instruction selection for AMD64 *)

open Arch
open Simd

type error = Bad_immediate of string

exception Error of error

let bad_immediate fmt =
  Format.kasprintf (fun msg -> raise (Error (Bad_immediate msg))) fmt

(* Assumes untagged int *)
let[@ocaml.warning "-4"] extract_constant args name ~max =
  match args with
  | Cmm.Cconst_int (i, _) :: args ->
    if i < 0 || i > max
    then
      bad_immediate "Immediate for %s must be in range [0,%d] (got %d)" name max
        i;
    i, args
  | _ -> bad_immediate "Did not get integer immediate for %s" name

let float_condition_of_int = function
  | 0 -> EQf
  | 1 -> LTf
  | 2 -> LEf
  | 3 -> UNORDf
  | 4 -> NEQf
  | 5 -> NLTf
  | 6 -> NLEf
  | 7 -> ORDf
  | i -> bad_immediate "Invalid float condition immediate: %d" i

let float_rounding_of_int = function
  (* Starts at 8, as these rounding modes also imply _MM_FROUND_NO_EXC (0x8) *)
  | 0x8 -> RoundNearest
  | 0x9 -> RoundDown
  | 0xA -> RoundUp
  | 0xB -> RoundTruncate
  | 0xC -> RoundCurrent
  | i -> bad_immediate "Invalid float rounding immediate: %d" i

let select_operation_clmul op args =
  if not (Arch.Extension.enabled CLMUL)
  then None
  else
    match op with
    | "caml_clmul_int64x2" ->
      let i, args = extract_constant args ~max:31 op in
      Some (Clmul_64 i, args)
    | _ -> None

let select_operation_bmi2 op args =
  if not (Arch.Extension.enabled BMI2)
  then None
  else
    match op with
    | "caml_bmi2_int64_extract_bits" -> Some (Extract_64, args)
    | "caml_bmi2_int64_deposit_bits" -> Some (Deposit_64, args)
    | _ -> None

let select_operation_sse op args =
  match op with
  | "caml_sse_float32_sqrt" | "sqrtf" -> Some (Sqrt_scalar_f32, args)
  | "caml_sse_float32_max" -> Some (Max_scalar_f32, args)
  | "caml_sse_float32_min" -> Some (Min_scalar_f32, args)
  | "caml_sse_cast_float32_int64" -> Some (Round_current_f32_i64, args)
  | "caml_sse_float32x4_cmp" ->
    let i, args = extract_constant args ~max:7 op in
    Some (Cmp_f32 (float_condition_of_int i), args)
  | "caml_sse_float32x4_add" -> Some (Add_f32, args)
  | "caml_sse_float32x4_sub" -> Some (Sub_f32, args)
  | "caml_sse_float32x4_mul" -> Some (Mul_f32, args)
  | "caml_sse_float32x4_div" -> Some (Div_f32, args)
  | "caml_sse_float32x4_max" -> Some (Max_f32, args)
  | "caml_sse_float32x4_min" -> Some (Min_f32, args)
  | "caml_sse_float32x4_rcp" -> Some (Rcp_f32, args)
  | "caml_sse_float32x4_rsqrt" -> Some (Rsqrt_f32, args)
  | "caml_sse_float32x4_sqrt" -> Some (Sqrt_f32, args)
  | "caml_sse_vec128_high_64_to_low_64" -> Some (High_64_to_low_64, args)
  | "caml_sse_vec128_low_64_to_high_64" -> Some (Low_64_to_high_64, args)
  | "caml_sse_vec128_interleave_high_32" -> Some (Interleave_high_32, args)
  | "caml_sse_vec128_interleave_low_32" -> Some (Interleave_low_32, args)
  | "caml_sse_vec128_movemask_32" -> Some (Movemask_32, args)
  | "caml_sse_vec128_shuffle_32" ->
    let i, args = extract_constant args ~max:0xff op in
    Some (Shuffle_32 i, args)
  | _ -> None

let select_operation_sse2 op args =
  match op with
  | "caml_sse2_float64_sqrt" | "sqrt" -> Some (Sqrt_scalar_f64, args)
  | "caml_sse2_float64_max" -> Some (Max_scalar_f64, args)
  | "caml_sse2_float64_min" -> Some (Min_scalar_f64, args)
  | "caml_sse2_cast_float64_int64" -> Some (Round_current_f64_i64, args)
  | "caml_sse2_float64x2_sqrt" -> Some (Sqrt_f64, args)
  | "caml_sse2_int8x16_add" -> Some (Add_i8, args)
  | "caml_sse2_int16x8_add" -> Some (Add_i16, args)
  | "caml_sse2_int32x4_add" -> Some (Add_i32, args)
  | "caml_sse2_int64x2_add" -> Some (Add_i64, args)
  | "caml_sse2_float64x2_add" -> Some (Add_f64, args)
  | "caml_sse2_int8x16_add_saturating" -> Some (Add_saturating_i8, args)
  | "caml_sse2_int16x8_add_saturating" -> Some (Add_saturating_i16, args)
  | "caml_sse2_int8x16_add_saturating_unsigned" ->
    Some (Add_saturating_unsigned_i8, args)
  | "caml_sse2_int16x8_add_saturating_unsigned" ->
    Some (Add_saturating_unsigned_i16, args)
  | "caml_sse2_int8x16_sub" -> Some (Sub_i8, args)
  | "caml_sse2_int16x8_sub" -> Some (Sub_i16, args)
  | "caml_sse2_int32x4_sub" -> Some (Sub_i32, args)
  | "caml_sse2_int64x2_sub" -> Some (Sub_i64, args)
  | "caml_sse2_float64x2_sub" -> Some (Sub_f64, args)
  | "caml_sse2_int8x16_sub_saturating" -> Some (Sub_saturating_i8, args)
  | "caml_sse2_int16x8_sub_saturating" -> Some (Sub_saturating_i16, args)
  | "caml_sse2_int8x16_sub_saturating_unsigned" ->
    Some (Sub_saturating_unsigned_i8, args)
  | "caml_sse2_int16x8_sub_saturating_unsigned" ->
    Some (Sub_saturating_unsigned_i16, args)
  | "caml_sse2_int8x16_max_unsigned" -> Some (Max_unsigned_i8, args)
  | "caml_sse2_int16x8_max" -> Some (Max_i16, args)
  | "caml_sse2_float64x2_max" -> Some (Max_f64, args)
  | "caml_sse2_int8x16_min_unsigned" -> Some (Min_unsigned_i8, args)
  | "caml_sse2_int16x8_min" -> Some (Min_i16, args)
  | "caml_sse2_float64x2_min" -> Some (Min_f64, args)
  | "caml_sse2_float64x2_mul" -> Some (Mul_f64, args)
  | "caml_sse2_float64x2_div" -> Some (Div_f64, args)
  | "caml_sse2_vec128_and" -> Some (And_bits, args)
  | "caml_sse2_vec128_andnot" -> Some (Andnot_bits, args)
  | "caml_sse2_vec128_or" -> Some (Or_bits, args)
  | "caml_sse2_vec128_xor" -> Some (Xor_bits, args)
  | "caml_sse2_vec128_movemask_8" -> Some (Movemask_8, args)
  | "caml_sse2_vec128_movemask_64" -> Some (Movemask_64, args)
  | "caml_sse2_vec128_shift_left_bytes" ->
    let i, args = extract_constant args ~max:15 op in
    Some (Shift_left_bytes i, args)
  | "caml_sse2_vec128_shift_right_bytes" ->
    let i, args = extract_constant args ~max:15 op in
    Some (Shift_right_bytes i, args)
  | "caml_sse2_int8x16_cmpeq" -> Some (Cmpeq_i8, args)
  | "caml_sse2_int16x8_cmpeq" -> Some (Cmpeq_i16, args)
  | "caml_sse2_int32x4_cmpeq" -> Some (Cmpeq_i32, args)
  | "caml_sse2_int8x16_cmpgt" -> Some (Cmpgt_i8, args)
  | "caml_sse2_int16x8_cmpgt" -> Some (Cmpgt_i16, args)
  | "caml_sse2_int32x4_cmpgt" -> Some (Cmpgt_i32, args)
  | "caml_sse2_float64x2_cmp" ->
    let i, args = extract_constant args ~max:0x1f op in
    Some (Cmp_f64 (float_condition_of_int i), args)
  | "caml_sse2_cvt_int32x4_float64x2" -> Some (I32_to_f64, args)
  | "caml_sse2_cvt_int32x4_float32x4" -> Some (I32_to_f32, args)
  | "caml_sse2_cvt_float64x2_int32x4" -> Some (F64_to_i32, args)
  | "caml_sse2_cvt_float64x2_float32x4" -> Some (F64_to_f32, args)
  | "caml_sse2_cvt_float32x4_int32x4" -> Some (F32_to_i32, args)
  | "caml_sse2_cvt_float32x4_float64x2" -> Some (F32_to_f64, args)
  | "caml_sse2_cvt_int16x8_int8x16_saturating" -> Some (I16_to_i8, args)
  | "caml_sse2_cvt_int32x4_int16x8_saturating" -> Some (I32_to_i16, args)
  | "caml_sse2_cvt_int16x8_int8x16_saturating_unsigned" ->
    Some (I16_to_unsigned_i8, args)
  | "caml_sse2_cvt_int32x4_int16x8_saturating_unsigned" ->
    Some (I32_to_unsigned_i16, args)
  | "caml_sse2_int8x16_avg_unsigned" -> Some (Avg_unsigned_i8, args)
  | "caml_sse2_int16x8_avg_unsigned" -> Some (Avg_unsigned_i16, args)
  | "caml_sse2_int8x16_sad_unsigned" -> Some (SAD_unsigned_i8, args)
  | "caml_sse2_int16x8_sll" -> Some (SLL_i16, args)
  | "caml_sse2_int32x4_sll" -> Some (SLL_i32, args)
  | "caml_sse2_int64x2_sll" -> Some (SLL_i64, args)
  | "caml_sse2_int16x8_srl" -> Some (SRL_i16, args)
  | "caml_sse2_int32x4_srl" -> Some (SRL_i32, args)
  | "caml_sse2_int64x2_srl" -> Some (SRL_i64, args)
  | "caml_sse2_int16x8_sra" -> Some (SRA_i16, args)
  | "caml_sse2_int32x4_sra" -> Some (SRA_i32, args)
  | "caml_sse2_int16x8_slli" ->
    let i, args = extract_constant args ~max:15 op in
    Some (SLLi_i16 i, args)
  | "caml_sse2_int32x4_slli" ->
    let i, args = extract_constant args ~max:31 op in
    Some (SLLi_i32 i, args)
  | "caml_sse2_int64x2_slli" ->
    let i, args = extract_constant args ~max:63 op in
    Some (SLLi_i64 i, args)
  | "caml_sse2_int16x8_srli" ->
    let i, args = extract_constant args ~max:15 op in
    Some (SRLi_i16 i, args)
  | "caml_sse2_int32x4_srli" ->
    let i, args = extract_constant args ~max:31 op in
    Some (SRLi_i32 i, args)
  | "caml_sse2_int64x2_srli" ->
    let i, args = extract_constant args ~max:63 op in
    Some (SRLi_i64 i, args)
  | "caml_sse2_int16x8_srai" ->
    let i, args = extract_constant args ~max:15 op in
    Some (SRAi_i16 i, args)
  | "caml_sse2_int32x4_srai" ->
    let i, args = extract_constant args ~max:31 op in
    Some (SRAi_i32 i, args)
  | "caml_sse2_vec128_shuffle_64" ->
    let i, args = extract_constant args ~max:3 op in
    Some (Shuffle_64 i, args)
  | "caml_sse2_vec128_shuffle_high_16" ->
    let i, args = extract_constant args ~max:255 op in
    Some (Shuffle_high_16 i, args)
  | "caml_sse2_vec128_shuffle_low_16" ->
    let i, args = extract_constant args ~max:255 op in
    Some (Shuffle_low_16 i, args)
  | "caml_sse2_vec128_interleave_high_8" -> Some (Interleave_high_8, args)
  | "caml_sse2_vec128_interleave_low_8" -> Some (Interleave_low_8, args)
  | "caml_sse2_vec128_interleave_high_16" -> Some (Interleave_high_16, args)
  | "caml_sse2_vec128_interleave_low_16" -> Some (Interleave_low_16, args)
  | "caml_sse2_vec128_interleave_high_64" -> Some (Interleave_high_64, args)
  | "caml_sse2_vec128_interleave_low_64" -> Some (Interleave_low_64, args)
  | "caml_sse2_int16x8_mul_high" -> Some (Mulhi_i16, args)
  | "caml_sse2_int16x8_mul_high_unsigned" -> Some (Mulhi_unsigned_i16, args)
  | "caml_sse2_int16x8_mul_low" -> Some (Mullo_i16, args)
  | "caml_sse2_int16x8_mul_hadd_int32x4" -> Some (Mul_hadd_i16_to_i32, args)
  | _ -> None

let select_operation_sse3 op args =
  if not (Arch.Extension.enabled SSE3)
  then None
  else
    match op with
    | "caml_sse3_float32x4_addsub" -> Some (Addsub_f32, args)
    | "caml_sse3_float64x2_addsub" -> Some (Addsub_f64, args)
    | "caml_sse3_float32x4_hadd" -> Some (Hadd_f32, args)
    | "caml_sse3_float64x2_hadd" -> Some (Hadd_f64, args)
    | "caml_sse3_float32x4_hsub" -> Some (Hsub_f32, args)
    | "caml_sse3_float64x2_hsub" -> Some (Hsub_f64, args)
    | "caml_sse3_vec128_dup_low_64" -> Some (Dup_low_64, args)
    | "caml_sse3_vec128_dup_odd_32" -> Some (Dup_odd_32, args)
    | "caml_sse3_vec128_dup_even_32" -> Some (Dup_even_32, args)
    | _ -> None

let select_operation_ssse3 op args =
  if not (Arch.Extension.enabled SSSE3)
  then None
  else
    match op with
    | "caml_ssse3_int8x16_abs" -> Some (Abs_i8, args)
    | "caml_ssse3_int16x8_abs" -> Some (Abs_i16, args)
    | "caml_ssse3_int32x4_abs" -> Some (Abs_i32, args)
    | "caml_ssse3_int16x8_hadd" -> Some (Hadd_i16, args)
    | "caml_ssse3_int32x4_hadd" -> Some (Hadd_i32, args)
    | "caml_ssse3_int16x8_hadd_saturating" -> Some (Hadd_saturating_i16, args)
    | "caml_ssse3_int16x8_hsub" -> Some (Hsub_i16, args)
    | "caml_ssse3_int32x4_hsub" -> Some (Hsub_i32, args)
    | "caml_ssse3_int16x8_hsub_saturating" -> Some (Hsub_saturating_i16, args)
    | "caml_ssse3_int8x16_mulsign" -> Some (Mulsign_i8, args)
    | "caml_ssse3_int16x8_mulsign" -> Some (Mulsign_i16, args)
    | "caml_ssse3_int32x4_mulsign" -> Some (Mulsign_i32, args)
    | "caml_ssse3_vec128_shuffle_8" -> Some (Shuffle_8, args)
    | "caml_ssse3_vec128_align_right_bytes" ->
      let i, args = extract_constant args ~max:31 op in
      Some (Alignr_i8 i, args)
    | "caml_ssse3_int8x16_mul_unsigned_hadd_saturating_int16x8" ->
      Some (Mul_unsigned_hadd_saturating_i8_to_i16, args)
    | _ -> None

let select_operation_sse41 op args =
  if not (Arch.Extension.enabled SSE4_1)
  then None
  else
    match op with
    | "caml_sse41_vec128_blend_16" ->
      let i, args = extract_constant args ~max:255 op in
      Some (Blend_16 i, args)
    | "caml_sse41_vec128_blend_32" ->
      let i, args = extract_constant args ~max:15 op in
      Some (Blend_32 i, args)
    | "caml_sse41_vec128_blend_64" ->
      let i, args = extract_constant args ~max:3 op in
      Some (Blend_64 i, args)
    | "caml_sse41_vec128_blendv_8" -> Some (Blendv_8, args)
    | "caml_sse41_vec128_blendv_32" -> Some (Blendv_32, args)
    | "caml_sse41_vec128_blendv_64" -> Some (Blendv_64, args)
    | "caml_sse41_int64x2_cmpeq" -> Some (Cmpeq_i64, args)
    | "caml_sse41_cvtsx_int8x16_int16x8" -> Some (I8_sx_i16, args)
    | "caml_sse41_cvtsx_int8x16_int32x4" -> Some (I8_sx_i32, args)
    | "caml_sse41_cvtsx_int8x16_int64x2" -> Some (I8_sx_i64, args)
    | "caml_sse41_cvtsx_int16x8_int32x4" -> Some (I16_sx_i32, args)
    | "caml_sse41_cvtsx_int16x8_int64x2" -> Some (I16_sx_i64, args)
    | "caml_sse41_cvtsx_int32x4_int64x2" -> Some (I32_sx_i64, args)
    | "caml_sse41_cvtzx_int8x16_int16x8" -> Some (I8_zx_i16, args)
    | "caml_sse41_cvtzx_int8x16_int32x4" -> Some (I8_zx_i32, args)
    | "caml_sse41_cvtzx_int8x16_int64x2" -> Some (I8_zx_i64, args)
    | "caml_sse41_cvtzx_int16x8_int32x4" -> Some (I16_zx_i32, args)
    | "caml_sse41_cvtzx_int16x8_int64x2" -> Some (I16_zx_i64, args)
    | "caml_sse41_cvtzx_int32x4_int64x2" -> Some (I32_zx_i64, args)
    | "caml_sse41_float32x4_dp" ->
      let i, args = extract_constant args ~max:255 op in
      Some (Dp_f32 i, args)
    | "caml_sse41_float64x2_dp" ->
      let i, args = extract_constant args ~max:255 op in
      Some (Dp_f64 i, args)
    | "caml_sse41_int8x16_extract" ->
      let i, args = extract_constant args ~max:15 op in
      Some (Extract_i8 i, args)
    | "caml_sse41_int16x8_extract" ->
      let i, args = extract_constant args ~max:7 op in
      Some (Extract_i16 i, args)
    | "caml_sse41_int32x4_extract" ->
      let i, args = extract_constant args ~max:3 op in
      Some (Extract_i32 i, args)
    | "caml_sse41_int64x2_extract" ->
      let i, args = extract_constant args ~max:1 op in
      Some (Extract_i64 i, args)
    | "caml_sse41_int8x16_insert" ->
      let i, args = extract_constant args ~max:15 op in
      Some (Insert_i8 i, args)
    | "caml_sse41_int16x8_insert" ->
      let i, args = extract_constant args ~max:7 op in
      Some (Insert_i16 i, args)
    | "caml_sse41_int32x4_insert" ->
      let i, args = extract_constant args ~max:3 op in
      Some (Insert_i32 i, args)
    | "caml_sse41_int64x2_insert" ->
      let i, args = extract_constant args ~max:1 op in
      Some (Insert_i64 i, args)
    | "caml_sse41_float32x4_round" ->
      let i, args = extract_constant args ~max:15 op in
      Some (Round_f32 (float_rounding_of_int i), args)
    | "caml_sse41_float64x2_round" ->
      let i, args = extract_constant args ~max:15 op in
      Some (Round_f64 (float_rounding_of_int i), args)
    | "caml_sse41_float64_round" ->
      (* CR-someday mslater: the following CR also applies here, but this
         builtin is not exposed by any of the stdlib libraries. *)
      let i, args = extract_constant args ~max:15 op in
      Some (Round_scalar_f64 (float_rounding_of_int i), args)
    | "caml_sse41_float32_round" ->
      (* CR-someday mslater: this builtin is exposed by float32.ml, so must
         actually be cross-platform. Currently, non-amd64 architectures will
         fall back to a C implementation. If we want the arm64 backend to
         specialize it, we should redefine the constant mapping from the amd64
         values to a new sum type. *)
      let i, args = extract_constant args ~max:15 op in
      Some (Round_scalar_f32 (float_rounding_of_int i), args)
    | "caml_sse41_int8x16_max" -> Some (Max_i8, args)
    | "caml_sse41_int32x4_max" -> Some (Max_i32, args)
    | "caml_sse41_int16x8_max_unsigned" -> Some (Max_unsigned_i16, args)
    | "caml_sse41_int32x4_max_unsigned" -> Some (Max_unsigned_i32, args)
    | "caml_sse41_int8x16_min" -> Some (Min_i8, args)
    | "caml_sse41_int32x4_min" -> Some (Min_i32, args)
    | "caml_sse41_int16x8_min_unsigned" -> Some (Min_unsigned_i16, args)
    | "caml_sse41_int32x4_min_unsigned" -> Some (Min_unsigned_i32, args)
    | "caml_sse41_int8x16_multi_sad_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      Some (Multi_sad_unsigned_i8 i, args)
    | "caml_sse41_int16x8_minpos_unsigned" -> Some (Minpos_unsigned_i16, args)
    | "caml_sse41_int32x4_mul_low" -> Some (Mullo_i32, args)
    | _ -> None

let select_operation_sse42 op args =
  if not (Arch.Extension.enabled SSE4_2)
  then None
  else
    match op with
    | "caml_sse42_int64x2_cmpgt" -> Some (Cmpgt_i64, args)
    | "caml_sse42_int64_crc" | "caml_sse42_int_untagged_crc" ->
      Some (Crc32_64, args)
    | "caml_sse42_vec128_cmpestrm" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpestrm i, args)
    | "caml_sse42_vec128_cmpestra" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpestra i, args)
    | "caml_sse42_vec128_cmpestrc" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpestrc i, args)
    | "caml_sse42_vec128_cmpestri" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpestri i, args)
    | "caml_sse42_vec128_cmpestro" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpestro i, args)
    | "caml_sse42_vec128_cmpestrs" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpestrs i, args)
    | "caml_sse42_vec128_cmpestrz" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpestrz i, args)
    | "caml_sse42_vec128_cmpistrm" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpistrm i, args)
    | "caml_sse42_vec128_cmpistra" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpistra i, args)
    | "caml_sse42_vec128_cmpistrc" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpistrc i, args)
    | "caml_sse42_vec128_cmpistri" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpistri i, args)
    | "caml_sse42_vec128_cmpistro" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpistro i, args)
    | "caml_sse42_vec128_cmpistrs" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpistrs i, args)
    | "caml_sse42_vec128_cmpistrz" ->
      let i, args = extract_constant args ~max:127 op in
      Some (Cmpistrz i, args)
    | _ -> None

let select_simd_instr op args =
  let or_else try_ ctr opt =
    match opt with Some x -> Some x | None -> Option.map ctr (try_ op args)
  in
  None
  |> or_else select_operation_clmul (fun (op, args) -> CLMUL op, args)
  |> or_else select_operation_bmi2 (fun (op, args) -> BMI2 op, args)
  |> or_else select_operation_sse (fun (op, args) -> SSE op, args)
  |> or_else select_operation_sse2 (fun (op, args) -> SSE2 op, args)
  |> or_else select_operation_sse3 (fun (op, args) -> SSE3 op, args)
  |> or_else select_operation_ssse3 (fun (op, args) -> SSSE3 op, args)
  |> or_else select_operation_sse41 (fun (op, args) -> SSE41 op, args)
  |> or_else select_operation_sse42 (fun (op, args) -> SSE42 op, args)

let select_operation op args =
  select_simd_instr op args
  |> Option.map (fun (op, args) -> Mach.(Ispecific (Isimd op), args))

let pseudoregs_for_operation op arg res =
  let rax = Proc.phys_reg Int 0 in
  let rcx = Proc.phys_reg Int 5 in
  let rdx = Proc.phys_reg Int 4 in
  let xmm0v () = Proc.phys_reg Vec128 100 in
  match Simd_proc.register_behavior op with
  | R_to_R | RM_to_R | R_to_RM | R_RM_to_R -> arg, res
  | R_to_fst ->
    (* arg.(0) and res.(0) must be the same *)
    [| res.(0) |], res
  | R_R_to_fst | R_RM_to_fst ->
    (* arg.(0) and res.(0) must be the same *)
    [| res.(0); arg.(1) |], res
  | R_RM_xmm0_to_fst -> [| res.(0); arg.(1); xmm0v () |], res
  | R_RM_rax_rdx_to_rcx -> [| arg.(0); arg.(1); rax; rdx |], [| rcx |]
  | R_RM_rax_rdx_to_xmm0 -> [| arg.(0); arg.(1); rax; rdx |], [| xmm0v () |]
  | R_RM_to_rcx -> arg, [| rcx |]
  | R_RM_to_xmm0 -> arg, [| xmm0v () |]

(* Error report *)

let report_error ppf = function
  | Bad_immediate msg -> Format.pp_print_string ppf msg

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

(* Vectorize operations *)

type width =
  | W8
  | W16
  | W32
  | W64

type register =
  (* Registers used in vectorized instruction(s) of one scalar instruction *)
  | New of int
    (* The n-th new temporary register used in the vectorized instructions *)
  | Argument of int
    (* Vector version of the n-th argument's register of the scalar
       instruction *)
  | Result of int
    (* Vector version of the n-th result's register of the scalar instruction *)
  | Original of int
(* Keep the original instruction in the n-th argument/result (depending on
   whether it is used in the argument or result of the vectorized instructions)
   of the scalar instruction*)

type vectorized_instruction =
  { operation : Cfg.operation;
    arguments : register array;
    results : register array
  }

let vector_width_in_bits = 128

let vectorize_operation ~width_in_bits ~arg_count ~res_count
    (cfg_ops : Cfg.operation list) : vectorized_instruction list option =
  (* Assumes cfg_ops are isomorphic *)
  let length = List.length cfg_ops in
  assert (length * width_in_bits = vector_width_in_bits);
  let width_type =
    match width_in_bits with
    | 64 -> W64
    | 32 -> W32
    | 16 -> W16
    | 8 -> W8
    | _ -> assert false
  in
  let make_default ~arg_count ~res_count operation =
    Some
      [ { operation;
          arguments = Array.init arg_count (fun i -> Argument i);
          results = Array.init res_count (fun i -> Result i)
        } ]
  in
  let create_const_vec consts =
    let highs, lows = Misc.Stdlib.List.split_at (length / 2) consts in
    let pack_int64 nums =
      let mask =
        Int64.shift_right_logical Int64.minus_one (64 - width_in_bits)
      in
      List.fold_left
        (fun target num ->
          Int64.logor
            (Int64.shift_left target width_in_bits)
            (Int64.logand num mask))
        0L nums
    in
    Cfg.Const_vec128 { high = pack_int64 highs; low = pack_int64 lows }
    |> make_default ~arg_count:0 ~res_count:1
  in
  let add_op =
    let sse_op =
      match width_type with
      | W64 -> Add_i64
      | W32 -> Add_i32
      | W16 -> Add_i16
      | W8 -> Add_i8
    in
    Some (Cfg.Specific (Isimd (SSE2 sse_op)))
  in
  let mul_op =
    match width_type with
    | W64 -> None
    | W32 -> Some (Cfg.Specific (Isimd (SSE41 Mullo_i32)))
    | W16 -> Some (Cfg.Specific (Isimd (SSE2 Mullo_i16)))
    | W8 -> None
  in
  let vectorize_intop (intop : Mach.integer_operation) =
    match intop with
    | Iadd -> Option.bind add_op (make_default ~arg_count ~res_count)
    | Isub ->
      let sse_op =
        match width_type with
        | W64 -> Sub_i64
        | W32 -> Sub_i32
        | W16 -> Sub_i16
        | W8 -> Sub_i8
      in
      Cfg.Specific (Isimd (SSE2 sse_op)) |> make_default ~arg_count ~res_count
    | Imul -> Option.bind mul_op (make_default ~arg_count ~res_count)
    | Imulh { signed } -> (
      match width_type with
      | W64 -> None
      | W32 -> None
      | W16 ->
        if signed
        then
          Cfg.Specific (Isimd (SSE2 Mulhi_i16))
          |> make_default ~arg_count ~res_count
        else
          Cfg.Specific (Isimd (SSE2 Mulhi_unsigned_i16))
          |> make_default ~arg_count ~res_count
      | W8 -> None)
    | Iand ->
      Cfg.Specific (Isimd (SSE2 And_bits)) |> make_default ~arg_count ~res_count
    | Ior ->
      Cfg.Specific (Isimd (SSE2 Or_bits)) |> make_default ~arg_count ~res_count
    | Ixor ->
      Cfg.Specific (Isimd (SSE2 Xor_bits)) |> make_default ~arg_count ~res_count
    | Ilsl ->
      let sse_op =
        match width_type with
        | W64 -> SLL_i64
        | W32 -> SLL_i32
        | W16 -> SLL_i16
        | W8 -> assert false
      in
      Cfg.Specific (Isimd (SSE2 sse_op)) |> make_default ~arg_count ~res_count
    | Ilsr ->
      let sse_op =
        match width_type with
        | W64 -> SRL_i64
        | W32 -> SRL_i32
        | W16 -> SRL_i16
        | W8 -> assert false
      in
      Cfg.Specific (Isimd (SSE2 sse_op)) |> make_default ~arg_count ~res_count
    | Iasr ->
      let sse_op =
        match width_type with
        | W64 -> assert false
        | W32 -> SRA_i32
        | W16 -> SRA_i16
        | W8 -> assert false
      in
      Cfg.Specific (Isimd (SSE2 sse_op)) |> make_default ~arg_count ~res_count
    | Icomp (Isigned intcomp) -> (
      match intcomp with
      | Ceq ->
        let sse_op =
          match width_type with
          | W64 -> SSE41 Cmpeq_i64
          | W32 -> SSE2 Cmpeq_i32
          | W16 -> SSE2 Cmpeq_i16
          | W8 -> SSE2 Cmpeq_i8
        in
        Cfg.Specific (Isimd sse_op) |> make_default ~arg_count ~res_count
      | Cgt ->
        let sse_op =
          match width_type with
          | W64 -> SSE42 Cmpgt_i64
          | W32 -> SSE2 Cmpgt_i32
          | W16 -> SSE2 Cmpgt_i16
          | W8 -> SSE2 Cmpgt_i8
        in
        Cfg.Specific (Isimd sse_op) |> make_default ~arg_count ~res_count
      | Cne | Clt | Cle | Cge ->
        None
        (* These instructions seem to not have a simd counterpart yet, could
           also implement as a combination of other instructions if needed in
           the future *))
    | Idiv | Imod | Iclz _ | Ictz _ | Ipopcnt | Icomp (Iunsigned _) -> None
  in
  match List.hd cfg_ops with
  | Move -> Cfg.Move |> make_default ~arg_count ~res_count
  | Const_int _ ->
    let extract_const_int (op : Cfg.operation) =
      match op with
      | Const_int n -> Int64.of_nativeint n
      | Move | Load _ | Store _ | Intop _ | Intop_imm _ | Specific _ | Alloc _
      | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_float32 _
      | Const_float _ | Const_symbol _ | Const_vec128 _ | Stackoffset _
      | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque
      | Begin_region | End_region | Name_for_debugger _ | Dls_get | Poll ->
        assert false
    in
    assert (arg_count = 0 && res_count = 1);
    let consts = List.map extract_const_int cfg_ops in
    create_const_vec consts
  | Load { memory_chunk = _; addressing_mode; mutability; is_atomic } ->
    let num_args_addressing = Arch.num_args_addressing addressing_mode in
    assert (arg_count = num_args_addressing && res_count = 1);
    let operation =
      Cfg.Load
        { memory_chunk = Onetwentyeight_unaligned;
          addressing_mode;
          mutability;
          is_atomic
        }
    in
    Some
      [ { operation;
          arguments = Array.init num_args_addressing (fun i -> Original i);
          results = [| Result 0 |]
        } ]
  | Store (_, addressing_mode, is_assignment) ->
    let num_args_addressing = Arch.num_args_addressing addressing_mode in
    assert (arg_count = num_args_addressing + 1 && res_count = 0);
    let operation =
      Cfg.Store (Onetwentyeight_unaligned, addressing_mode, is_assignment)
    in
    Some
      [ { operation;
          arguments =
            Array.append [| Argument 0 |]
              (Array.init num_args_addressing (fun i -> Original (i + 1)));
          results = [||]
        } ]
  | Intop intop -> vectorize_intop intop
  | Intop_imm (intop, _) -> (
    let extract_intop_imm_int (op : Cfg.operation) =
      match op with
      | Intop_imm (_, n) -> Int64.of_int n
      | Move | Load _ | Store _ | Intop _ | Specific _ | Alloc _
      | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_int _
      | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
      | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
      | Opaque | Begin_region | End_region | Name_for_debugger _ | Dls_get
      | Poll ->
        assert false
    in
    let consts = List.map extract_intop_imm_int cfg_ops in
    match create_const_vec consts, vectorize_intop intop with
    | Some [const_instruction], Some [intop_instruction] ->
      if Array.length const_instruction.results = 1
         && Array.length intop_instruction.arguments = 2
      then (
        assert (arg_count = 1 && res_count = 1);
        const_instruction.results.(0) <- New 0;
        intop_instruction.arguments.(1) <- New 0;
        Some [const_instruction; intop_instruction])
      else None
    | _ -> None)
  | Specific op -> (
    match op with
    | Ilea addressing_mode -> (
      let extract_scale_displ (op : Cfg.operation) =
        match op with
        | Specific spec_op -> (
          match spec_op with
          | Ilea addressing_mode -> (
            match addressing_mode with
            | Iindexed displ -> None, Some displ
            | Iindexed2 displ -> None, Some displ
            | Iscaled (scale, displ) -> Some scale, Some displ
            | Iindexed2scaled (scale, displ) -> Some scale, Some displ
            | Ibased _ -> None, None)
          | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Ibswap _
          | Isextend32 | Izextend32 | Irdtsc | Irdpmc | Ilfence | Isfence
          | Imfence | Ipause | Isimd _ | Iprefetch _ ->
            assert false)
        | Move | Load _ | Store _ | Intop _ | Intop_imm _ | Alloc _
        | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_int _
        | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
        | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _
        | Probe_is_enabled _ | Opaque | Begin_region | End_region
        | Name_for_debugger _ | Dls_get | Poll ->
          assert false
      in
      let get_scale op =
        match extract_scale_displ op with
        | Some scale, _ -> scale |> Int64.of_int
        | _ -> assert false
      in
      let get_displ op =
        match extract_scale_displ op with
        | _, Some displ -> displ |> Int64.of_int
        | _ -> assert false
      in
      let make_move arg res =
        { operation = Move; arguments = [| arg |]; results = [| res |] }
      in
      let make_binary_operation arg_0 arg_1 res operation =
        { operation; arguments = [| arg_0; arg_1 |]; results = [| res |] }
      in
      let make_const res consts =
        match create_const_vec consts with
        | Some [const_instruction] ->
          assert (
            Array.length const_instruction.arguments = 0
            && Array.length const_instruction.results = 1);
          const_instruction.results.(0) <- res;
          const_instruction
        | _ -> assert false
      in
      match addressing_mode with
      | Iindexed _ -> (
        match add_op with
        | Some add ->
          assert (arg_count = 1 && res_count = 1);
          let displs = List.map get_displ cfg_ops in
          (* reg + displ *)
          Some
            [ make_move (Argument 0) (Result 0);
              make_const (New 0) displs;
              make_binary_operation (Result 0) (New 0) (Result 0) add ]
        | None -> None)
      | Iindexed2 _ -> (
        match add_op with
        | Some add ->
          assert (arg_count = 2 && res_count = 1);
          let displs = List.map get_displ cfg_ops in
          (* reg + reg + displ *)
          Some
            [ make_move (Argument 0) (Result 0);
              make_binary_operation (Result 0) (Argument 1) (Result 0) add;
              make_const (New 0) displs;
              make_binary_operation (Result 0) (New 0) (Result 0) add ]
        | None -> None)
      | Iscaled _ -> (
        match add_op, mul_op with
        | Some add, Some mul ->
          assert (arg_count = 1 && res_count = 1);
          let scales = List.map get_scale cfg_ops in
          let displs = List.map get_displ cfg_ops in
          (* reg * scale + displ *)
          Some
            [ make_move (Argument 0) (Result 0);
              make_const (New 0) scales;
              make_binary_operation (Result 0) (New 0) (Result 0) mul;
              make_const (New 1) displs;
              make_binary_operation (Result 0) (New 1) (Result 0) add ]
        | _ -> None)
      | Iindexed2scaled _ -> (
        match add_op, mul_op with
        | Some add, Some mul ->
          assert (arg_count = 2 && res_count = 1);
          let scales = List.map get_scale cfg_ops in
          let displs = List.map get_displ cfg_ops in
          (* reg + reg * scale + displ *)
          Some
            [ make_move (Argument 1) (Result 0);
              make_const (New 0) scales;
              make_binary_operation (Result 0) (New 0) (Result 0) mul;
              make_binary_operation (Result 0) (Argument 0) (Result 0) add;
              make_const (New 1) displs;
              make_binary_operation (Result 0) (New 1) (Result 0) add ]
        | _ -> None)
      | Ibased _ -> None)
    | Isextend32 -> (
      match width_type with
      | W64 ->
        Cfg.Specific (Isimd (SSE41 I32_sx_i64))
        |> make_default ~arg_count ~res_count
      | W32 ->
        None
        (* If the upper bits of the original register containing the smaller
           register is determined to be unused without relying on this file,
           these can also be vectorized to be a move *)
      | W16 -> None
      | W8 -> None)
    | Izextend32 -> (
      match width_type with
      | W64 ->
        Cfg.Specific (Isimd (SSE41 I32_zx_i64))
        |> make_default ~arg_count ~res_count
      | W32 -> None (* See previous comment *)
      | W16 -> None
      | W8 -> None)
    | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Ibswap _ | Irdtsc
    | Irdpmc | Ilfence | Isfence | Imfence | Ipause | Isimd _ | Iprefetch _ ->
      None)
  | Alloc _ | Reinterpret_cast _ | Static_cast _ | Spill | Reload
  | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
  | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
  | Opaque | Begin_region | End_region | Name_for_debugger _ | Dls_get | Poll ->
    None
