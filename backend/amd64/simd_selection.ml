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

open Arch
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

let arg i args = List.nth args i

(* Assumes untagged int *)
let extract_constant args low high name =
  match args with
  | Cmm.Cconst_int (i, _) :: args ->
    if i < low || i > high
    then
      Misc.fatal_errorf "Immediate for %s must be in range [%d,%d] (got %d)"
        name low high i;
    i, args
  | _ -> Misc.fatal_errorf "Did not get integer immediate for %s" name

let float_condition_of_int = function
  | 0 -> EQf
  | 1 -> LTf
  | 2 -> LEf
  | 3 -> UNORDf
  | 4 -> NEQf
  | 5 -> NLTf
  | 6 -> NLEf
  | 7 -> ORDf
  | i -> Misc.fatal_errorf "Invalid float condition immediate: %d" i

let float_rounding_of_int = function
  | 0 -> RoundNearest
  | 1 -> RoundDown
  | 2 -> RoundUp
  | 3 -> RoundTruncate
  | 4 -> RoundCurrent
  | i -> Misc.fatal_errorf "Invalid float condition immediate: %d" i

let select_operation_sse op args dbg =
  match op with
  | "caml_sse_float32x4_cmp" ->
    let i, args = extract_constant args 0 7 "caml_sse_float32x4_cmp" in
    Some (Cmp_f32 (float_condition_of_int i), args)
  | "caml_sse_float32x4_add" -> Some (Add_f32, args)
  | "caml_sse_float32x4_sub" -> Some (Sub_f32, args)
  | "caml_sse_float32x4_mul" -> Some (Mul_f32, args)
  | "caml_sse_float32x4_div" -> Some (Div_f32, args)
  | "caml_sse_float32x4_max" -> Some (Max_f32, args)
  | "caml_sse_float32x4_min" -> Some (Min_f32, args)
  | "caml_sse_float32x4_rcp" -> Some (Rcp_f32, [arg 0 args; arg 0 args])
  | "caml_sse_float32x4_rsqrt" -> Some (Rsqrt_f32, [arg 0 args; arg 0 args])
  | "caml_sse_float32x4_sqrt" -> Some (Sqrt_f32, [arg 0 args; arg 0 args])
  | "caml_sse_vec128_high_64_to_low_64" -> Some (High_64_to_low_64, args)
  | "caml_sse_vec128_low_64_to_high_64" -> Some (Low_64_to_high_64, args)
  | "caml_sse_vec128_interleave_high_32" -> Some (Interleave_high_32, args)
  | "caml_sse_vec128_interleave_low_32" -> Some (Interleave_low_32, args)
  | "caml_sse_vec128_movemask_32" -> Some (Movemask_32, args)
  | "caml_sse_vec128_shuffle_32" ->
    let i, args = extract_constant args 0 0xff "caml_sse_vec128_shuffle_32" in
    Some (Shuffle_32 i, args)
  | _ -> None

let select_operation_sse2 op args dbg =
  match op with
  | "caml_sse2_int8x16_add" -> Some (Add_i8, args)
  | "caml_sse2_int16x8_add" -> Some (Add_i16, args)
  | "caml_sse2_int32x4_add" -> Some (Add_i32, args)
  | "caml_sse2_int64x2_add" -> Some (Add_i64, args)
  | "caml_sse2_float64x2_add" -> Some (Add_f64, args)
  | "caml_sse2_int8x16_add_saturating" -> Some (Adds_i8, args)
  | "caml_sse2_int16x8_add_saturating" -> Some (Adds_i16, args)
  | "caml_sse2_int8x16_add_saturating_unsigned" -> Some (Adds_u8, args)
  | "caml_sse2_int16x8_add_saturating_unsigned" -> Some (Adds_u16, args)
  | "caml_sse2_int8x16_sub" -> Some (Sub_i8, args)
  | "caml_sse2_int16x8_sub" -> Some (Sub_i16, args)
  | "caml_sse2_int32x4_sub" -> Some (Sub_i32, args)
  | "caml_sse2_int64x2_sub" -> Some (Sub_i64, args)
  | "caml_sse2_float64x2_sub" -> Some (Sub_f64, args)
  | "caml_sse2_int8x16_sub_saturating" -> Some (Subs_i8, args)
  | "caml_sse2_int16x8_sub_saturating" -> Some (Subs_i16, args)
  | "caml_sse2_int8x16_sub_saturating_unsigned" -> Some (Subs_u8, args)
  | "caml_sse2_int16x8_sub_saturating_unsigned" -> Some (Subs_u16, args)
  | "caml_sse2_int8x16_max_unsigned" -> Some (Max_u8, args)
  | "caml_sse2_int16x8_max" -> Some (Max_i16, args)
  | "caml_sse2_float64x2_max" -> Some (Max_f64, args)
  | "caml_sse2_int8x16_min_unsigned" -> Some (Min_u8, args)
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
    let i, args =
      extract_constant args 0 15 "caml_sse2_vec128_shift_left_bytes"
    in
    Some (Shift_left_bytes i, args)
  | "caml_sse2_vec128_shift_right_bytes" ->
    let i, args =
      extract_constant args 0 15 "caml_sse2_vec128_shift_right_bytes"
    in
    Some (Shift_right_bytes i, args)
  | "caml_sse2_int8x16_cmpeq" -> Some (Cmpeq_i8, args)
  | "caml_sse2_int16x8_cmpeq" -> Some (Cmpeq_i16, args)
  | "caml_sse2_int32x4_cmpeq" -> Some (Cmpeq_i32, args)
  | "caml_sse2_int8x16_cmpgt" -> Some (Cmpgt_i8, args)
  | "caml_sse2_int16x8_cmpgt" -> Some (Cmpgt_i16, args)
  | "caml_sse2_int32x4_cmpgt" -> Some (Cmpgt_i32, args)
  | "caml_sse2_float64x2_cmp" ->
    let i, args = extract_constant args 0 0x1f "caml_sse2_float64x2_cmp" in
    Some (Cmp_f64 (float_condition_of_int i), args)
  | "caml_sse2_cvt_int32x4_float64x2" ->
    Some (I32_to_f64, [arg 0 args; arg 0 args])
  | "caml_sse2_cvt_int32x4_float32x4" ->
    Some (I32_to_f32, [arg 0 args; arg 0 args])
  | "caml_sse2_cvt_float64x2_int32x4" ->
    Some (F64_to_i32, [arg 0 args; arg 0 args])
  | "caml_sse2_cvt_float64x2_float32x4" ->
    Some (F64_to_f32, [arg 0 args; arg 0 args])
  | "caml_sse2_cvt_float32x4_int32x4" ->
    Some (F32_to_i32, [arg 0 args; arg 0 args])
  | "caml_sse2_cvt_float32x4_float64x2" ->
    Some (F32_to_f64, [arg 0 args; arg 0 args])
  | "caml_sse2_int16x8_sll" -> Some (SLL_i16, args)
  | "caml_sse2_int32x4_sll" -> Some (SLL_i32, args)
  | "caml_sse2_int64x2_sll" -> Some (SLL_i64, args)
  | "caml_sse2_int16x8_srl" -> Some (SRL_i16, args)
  | "caml_sse2_int32x4_srl" -> Some (SRL_i32, args)
  | "caml_sse2_int64x2_srl" -> Some (SRL_i64, args)
  | "caml_sse2_int16x8_sra" -> Some (SRA_i16, args)
  | "caml_sse2_int32x4_sra" -> Some (SRA_i32, args)
  | "caml_sse2_int16x8_slli" ->
    let i, args = extract_constant args 0 15 "caml_sse2_int16x8_slli" in
    Some (SLLi_i16 i, args)
  | "caml_sse2_int32x4_slli" ->
    let i, args = extract_constant args 0 31 "caml_sse2_int32x4_slli" in
    Some (SLLi_i32 i, args)
  | "caml_sse2_int64x2_slli" ->
    let i, args = extract_constant args 0 63 "caml_sse2_int64x2_slli" in
    Some (SLLi_i64 i, args)
  | "caml_sse2_int16x8_srli" ->
    let i, args = extract_constant args 0 15 "caml_sse2_int16x8_srli" in
    Some (SRLi_i16 i, args)
  | "caml_sse2_int32x4_srli" ->
    let i, args = extract_constant args 0 31 "caml_sse2_int32x4_srli" in
    Some (SRLi_i32 i, args)
  | "caml_sse2_int64x2_srli" ->
    let i, args = extract_constant args 0 63 "caml_sse2_int64x2_srli" in
    Some (SRLi_i64 i, args)
  | "caml_sse2_int16x8_srai" ->
    let i, args = extract_constant args 0 15 "caml_sse2_int16x8_srai" in
    Some (SRAi_i16 i, args)
  | "caml_sse2_int32x4_srai" ->
    let i, args = extract_constant args 0 31 "caml_sse2_int32x4_srai" in
    Some (SRAi_i32 i, args)
  | "caml_sse2_vec128_shuffle_64" ->
    let i, args = extract_constant args 0 3 "caml_sse2_vec128_shuffle_64" in
    Some (Shuffle_64 i, args)
  | "caml_sse2_vec128_shuffle_high_16" ->
    let i, args =
      extract_constant args 0 255 "caml_sse2_vec128_shuffle_high_16"
    in
    Some (Shuffle_high_16 i, args)
  | "caml_sse2_vec128_shuffle_low_16" ->
    let i, args =
      extract_constant args 0 255 "caml_sse2_vec128_shuffle_low_16"
    in
    Some (Shuffle_low_16 i, args)
  | "caml_sse2_vec128_interleave_high_8" -> Some (Interleave_high_8, args)
  | "caml_sse2_vec128_interleave_low_8" -> Some (Interleave_low_8, args)
  | "caml_sse2_vec128_interleave_high_16" -> Some (Interleave_high_16, args)
  | "caml_sse2_vec128_interleave_low_16" -> Some (Interleave_low_16, args)
  | "caml_sse2_vec128_interleave_high_64" -> Some (Interleave_high_64, args)
  | "caml_sse2_vec128_interleave_low_64" -> Some (Interleave_low_64, args)
  | _ -> None

let select_operation_sse3 op args dbg =
  if not !Arch.sse3_support
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

let select_operation_ssse3 op args dbg =
  if not !Arch.ssse3_support
  then None
  else
    match op with
    | "caml_ssse3_int8x16_abs" -> Some (Abs_i8, args)
    | "caml_ssse3_int16x8_abs" -> Some (Abs_i16, args)
    | "caml_ssse3_int32x4_abs" -> Some (Abs_i32, args)
    | "caml_ssse3_int16x8_hadd" -> Some (Hadd_i16, args)
    | "caml_ssse3_int32x4_hadd" -> Some (Hadd_i32, args)
    | "caml_ssse3_int16x8_hadd_saturating" -> Some (Hadds_i16, args)
    | "caml_ssse3_int16x8_hsub" -> Some (Hsub_i16, args)
    | "caml_ssse3_int32x4_hsub" -> Some (Hsub_i32, args)
    | "caml_ssse3_int16x8_hsub_saturating" -> Some (Hsubs_i16, args)
    | "caml_ssse3_int8x16_mulsign" -> Some (Mulsign_i8, args)
    | "caml_ssse3_int16x8_mulsign" -> Some (Mulsign_i16, args)
    | "caml_ssse3_int32x4_mulsign" -> Some (Mulsign_i32, args)
    | "caml_ssse3_vec128_shuffle_8" -> Some (Shuffle_8, args)
    | _ -> None

let select_operation_sse41 op args dbg =
  if not !Arch.sse41_support
  then None
  else
    match op with
    | "caml_sse41_vec128_blend_16" ->
      let i, args = extract_constant args 0 255 "caml_sse41_vec128_blend_16" in
      Some (Blend_16 i, args)
    | "caml_sse41_vec128_blend_32" ->
      let i, args = extract_constant args 0 15 "caml_sse41_vec128_blend_32" in
      Some (Blend_32 i, args)
    | "caml_sse41_vec128_blend_64" ->
      let i, args = extract_constant args 0 3 "caml_sse41_vec128_blend_64" in
      Some (Blend_64 i, args)
    | "caml_sse41_vec128_blendv_8" -> Some (Blendv_8, args)
    | "caml_sse41_vec128_blendv_32" -> Some (Blendv_32, args)
    | "caml_sse41_vec128_blendv_64" -> Some (Blendv_64, args)
    | "caml_sse41_int64x2_cmpeq" -> Some (Cmpeq_i64, args)
    | "caml_sse41_cvtsx_int8x16_int16x8" ->
      Some (I8_sx_i16, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtsx_int8x16_int32x4" ->
      Some (I8_sx_i32, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtsx_int8x16_int64x2" ->
      Some (I8_sx_i64, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtsx_int16x8_int32x4" ->
      Some (I16_sx_i32, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtsx_int16x8_int64x2" ->
      Some (I16_sx_i64, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtsx_int32x4_int64x2" ->
      Some (I32_sx_i64, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtzx_int8x16_int16x2" ->
      Some (U8_zx_i16, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtzx_int8x16_int32x4" ->
      Some (U8_zx_i32, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtzx_int8x16_int64x2" ->
      Some (U8_zx_i64, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtzx_int16x8_int32x4" ->
      Some (U16_zx_i32, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtzx_int16x8_int64x2" ->
      Some (U16_zx_i64, [arg 0 args; arg 0 args])
    | "caml_sse41_cvtzx_int32x4_int64x2" ->
      Some (U32_zx_i64, [arg 0 args; arg 0 args])
    | "caml_sse41_float32x4_dp" ->
      let i, args = extract_constant args 0 255 "caml_sse41_float32x4_dp" in
      Some (Dp_f32 i, args)
    | "caml_sse41_float64x2_dp" ->
      let i, args = extract_constant args 0 255 "caml_sse41_float64x2_dp" in
      Some (Dp_f64 i, args)
    | "caml_sse41_int8x16_extract" ->
      let i, args = extract_constant args 0 15 "caml_sse41_int8x16_extract" in
      Some (Extract_i8 i, args)
    | "caml_sse41_int16x8_extract" ->
      let i, args = extract_constant args 0 7 "caml_sse41_int16x8_extract" in
      Some (Extract_i16 i, args)
    | "caml_sse41_int32x4_extract" ->
      let i, args = extract_constant args 0 3 "caml_sse41_int32x4_extract" in
      Some (Extract_i32 i, args)
    | "caml_sse41_int64x2_extract" ->
      let i, args = extract_constant args 0 1 "caml_sse41_int64x2_extract" in
      Some (Extract_i64 i, args)
    | "caml_sse41_int8x16_insert" ->
      let i, args = extract_constant args 0 15 "caml_sse41_int8x16_insert" in
      Some (Insert_i8 i, args)
    | "caml_sse41_int16x8_insert" ->
      let i, args = extract_constant args 0 7 "caml_sse41_int16x8_insert" in
      Some (Insert_i16 i, args)
    | "caml_sse41_int32x4_insert" ->
      let i, args = extract_constant args 0 3 "caml_sse41_int32x4_insert" in
      Some (Insert_i32 i, args)
    | "caml_sse41_int64x2_insert" ->
      let i, args = extract_constant args 0 1 "caml_sse41_int64x2_insert" in
      Some (Insert_i64 i, args)
    | "caml_sse41_float32x4_round" ->
      let i, args = extract_constant args 0 15 "caml_sse41_float32x4_round" in
      Some (Round_f32 (float_rounding_of_int i), [arg 0 args; arg 0 args])
    | "caml_sse41_float64x2_round" ->
      let i, args = extract_constant args 0 15 "caml_sse41_float64x2_round" in
      Some (Round_f64 (float_rounding_of_int i), [arg 0 args; arg 0 args])
    | "caml_sse41_int8x16_max" -> Some (Max_i8, args)
    | "caml_sse41_int32x4_max" -> Some (Max_i32, args)
    | "caml_sse41_int16x8_max_unsigned" -> Some (Max_u16, args)
    | "caml_sse41_int32x4_max_unsigned" -> Some (Max_u32, args)
    | "caml_sse41_int8x16_min" -> Some (Min_i8, args)
    | "caml_sse41_int32x4_min" -> Some (Min_i32, args)
    | "caml_sse41_int16x8_min_unsigned" -> Some (Min_u16, args)
    | "caml_sse41_int32x4_min_unsigned" -> Some (Min_u32, args)
    | _ -> None

let select_operation_sse42 op args dbg =
  if not !Arch.sse42_support
  then None
  else
    match op with
    | "caml_sse42_int64x2_cmpgt" -> Some (Cmpgt_i64, args)
    | "caml_sse42_vec128_cmpestri" ->
      let i, args = extract_constant args 0 127 "caml_sse42_vec128_cmpestri" in
      Some (Cmpestri i, args)
    | "caml_sse42_vec128_cmpestrm" ->
      let i, args = extract_constant args 0 127 "caml_sse42_vec128_cmpestrm" in
      Some (Cmpestrm i, args)
    | "caml_sse42_vec128_cmpistri" ->
      let i, args = extract_constant args 0 127 "caml_sse42_vec128_cmpistri" in
      Some (Cmpistri i, args)
    | "caml_sse42_vec128_cmpistrm" ->
      let i, args = extract_constant args 0 127 "caml_sse42_vec128_cmpistrm" in
      Some (Cmpistrm i, args)
    | "caml_int64_crc_unboxed" | "caml_int_crc_untagged" -> Some (Crc32_64, args)
    | _ -> None

let select_simd_instr op args dbg =
  let or_else _try ctr opt =
    match opt with
    | Some x -> Some x
    | None -> Option.map ctr (_try op args dbg)
  in
  None
  |> or_else select_operation_sse (fun (op, args) -> SSE op, args)
  |> or_else select_operation_sse2 (fun (op, args) -> SSE2 op, args)
  |> or_else select_operation_sse3 (fun (op, args) -> SSE3 op, args)
  |> or_else select_operation_ssse3 (fun (op, args) -> SSSE3 op, args)
  |> or_else select_operation_sse41 (fun (op, args) -> SSE41 op, args)
  |> or_else select_operation_sse42 (fun (op, args) -> SSE42 op, args)

let select_operation op args dbg =
  select_simd_instr op args dbg
  |> Option.map (fun (op, args) -> Mach.(Ispecific (Isimd op), args))

let register_behavior_sse = function
  | Cmp_f32 _ | Add_f32 | Sub_f32 | Mul_f32 | Div_f32 | Max_f32 | Min_f32
  | Rcp_f32 | Sqrt_f32 | Rsqrt_f32 | Interleave_low_32 | Interleave_high_32
  | Shuffle_32 _ ->
    R_RM_to_fst
  | High_64_to_low_64 | Low_64_to_high_64 -> R_R_to_fst
  | Movemask_32 -> R_to_R

let register_behavior_sse2 = function
  | Add_i8 | Add_i16 | Add_i32 | Add_i64 | Add_f64 | Adds_i8 | Adds_i16
  | Adds_u8 | Adds_u16 | Sub_i8 | Sub_i16 | Sub_i32 | Sub_i64 | Sub_f64
  | Subs_i8 | Subs_i16 | Subs_u8 | Subs_u16 | Max_u8 | Max_i16 | Max_f64
  | Min_u8 | Min_i16 | Min_f64 | Mul_f64 | Div_f64 | And_bits | Andnot_bits
  | Or_bits | Xor_bits | Cmpeq_i8 | Cmpeq_i16 | Cmpeq_i32 | Cmpgt_i8 | Cmpgt_i16
  | Cmpgt_i32 | Cmp_f64 _ | I32_to_f64 | I32_to_f32 | F64_to_i32 | F64_to_f32
  | F32_to_i32 | F32_to_f64 | SLL_i16 | SLL_i32 | SLL_i64 | SRL_i16 | SRL_i32
  | SRL_i64 | SRA_i16 | SRA_i32 | SLLi_i16 _ | SLLi_i32 _ | SLLi_i64 _
  | SRLi_i16 _ | SRLi_i32 _ | SRLi_i64 _ | SRAi_i16 _ | SRAi_i32 _
  | Shuffle_64 _ | Shuffle_high_16 _ | Shuffle_low_16 _ | Interleave_high_8
  | Interleave_high_16 | Interleave_high_64 | Interleave_low_8
  | Interleave_low_16 | Interleave_low_64 ->
    R_RM_to_fst
  | Shift_left_bytes _ | Shift_right_bytes _ -> R_to_fst
  | Movemask_8 | Movemask_64 -> R_to_R

let register_behavior_sse3 = function
  | Addsub_f32 | Addsub_f64 | Hadd_f32 | Hadd_f64 | Hsub_f32 | Hsub_f64
  | Dup_low_64 | Dup_odd_32 | Dup_even_32 ->
    R_RM_to_fst

let register_behavior_ssse3 = function
  | Abs_i8 | Abs_i16 | Abs_i32 | Hadd_i16 | Hadd_i32 | Hadds_i16 | Hsub_i16
  | Hsub_i32 | Hsubs_i16 | Mulsign_i8 | Mulsign_i16 | Mulsign_i32 | Shuffle_8 ->
    R_RM_to_fst

let register_behavior_sse41 = function
  | Blend_16 _ | Blend_32 _ | Blend_64 _ | Blendv_8 | Blendv_32 | Blendv_64
  | Cmpeq_i64 | I8_sx_i16 | I8_sx_i32 | I8_sx_i64 | I16_sx_i32 | I16_sx_i64
  | I32_sx_i64 | U8_zx_i16 | U8_zx_i32 | U8_zx_i64 | U16_zx_i32 | U16_zx_i64
  | U32_zx_i64 | Dp_f32 _ | Dp_f64 _ | Max_i8 | Max_i32 | Max_u16 | Max_u32
  | Min_i8 | Min_i32 | Min_u16 | Min_u32 | Round_f64 _ | Round_f32 _
  | Insert_i8 _ | Insert_i16 _ | Insert_i32 _ | Insert_i64 _ ->
    R_RM_to_fst
  | Extract_i64 _ | Extract_i32 _ -> R_to_RM
  | Extract_i8 _ | Extract_i16 _ ->
    (* CR mslater: (SIMD): replace once we have int8/int16/float32 *)
    R_to_R

let register_behavior_sse42 = function
  | Cmpestri _ | Cmpestrm _ | Cmpistri _ | Cmpistrm _ | Crc32_64 | Cmpgt_i64 ->
    R_RM_to_fst

let register_behavior = function
  | SSE op -> register_behavior_sse op
  | SSE2 op -> register_behavior_sse2 op
  | SSE3 op -> register_behavior_sse3 op
  | SSSE3 op -> register_behavior_ssse3 op
  | SSE41 op -> register_behavior_sse41 op
  | SSE42 op -> register_behavior_sse42 op
