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

(* SIMD instructions for AMD64 *)

open Format

type operation_class = Pure

type float_condition = X86_ast.float_condition =
  | EQf
  | LTf
  | LEf
  | UNORDf
  | NEQf
  | NLTf
  | NLEf
  | ORDf

type float_rounding = X86_ast.rounding =
  | RoundUp
  | RoundDown
  | RoundNearest
  | RoundTruncate
  | RoundCurrent

let float_condition_equal l r =
  match l, r with
  | EQf, EQf
  | LTf, LTf
  | LEf, LEf
  | UNORDf, UNORDf
  | NEQf, NEQf
  | NLTf, NLTf
  | NLEf, NLEf
  | ORDf, ORDf ->
    true
  | (EQf | LTf | LEf | UNORDf | NEQf | NLTf | NLEf | ORDf), _ -> false

let float_rounding_equal l r =
  match l, r with
  | RoundUp, RoundUp
  | RoundDown, RoundDown
  | RoundNearest, RoundNearest
  | RoundTruncate, RoundTruncate
  | RoundCurrent, RoundCurrent ->
    true
  | (RoundUp | RoundDown | RoundNearest | RoundTruncate | RoundCurrent), _ ->
    false

type clmul_operation = Clmul_64 of int

type bmi2_operation =
  | Deposit_64
  | Extract_64

type sse_operation =
  | Cmp_f32 of float_condition
  | Add_f32
  | Sub_f32
  | Mul_f32
  | Div_f32
  | Max_f32
  | Min_f32
  | Rcp_f32
  | Sqrt_f32
  | Rsqrt_f32
  | High_64_to_low_64
  | Low_64_to_high_64
  | Interleave_high_32
  | Interleave_low_32
  | Movemask_32
  | Shuffle_32 of int

type sse2_operation =
  | Cast_scalar_f64_i64
  | Sqrt_scalar_f64
  | Min_scalar_f64
  | Max_scalar_f64
  | Sqrt_f64
  | Add_i8
  | Add_i16
  | Add_i32
  | Add_i64
  | Add_f64
  | Add_saturating_i8
  | Add_saturating_i16
  | Add_saturating_unsigned_i8
  | Add_saturating_unsigned_i16
  | Sub_i8
  | Sub_i16
  | Sub_i32
  | Sub_i64
  | Sub_f64
  | Sub_saturating_i8
  | Sub_saturating_i16
  | Sub_saturating_unsigned_i8
  | Sub_saturating_unsigned_i16
  | Max_unsigned_i8
  | Max_i16
  | Max_f64
  | Min_unsigned_i8
  | Min_i16
  | Min_f64
  | Mul_f64
  | Div_f64
  | Avg_unsigned_i8
  | Avg_unsigned_i16
  | SAD_unsigned_i8
  | Mulhi_i16
  | Mulhi_unsigned_i16
  | Mullo_i16
  | Mul_hadd_i16_to_i32
  | And_bits
  | Andnot_bits
  | Or_bits
  | Xor_bits
  | Movemask_8
  | Movemask_64
  | Shift_left_bytes of int
  | Shift_right_bytes of int
  | Cmpeq_i8
  | Cmpeq_i16
  | Cmpeq_i32
  | Cmpgt_i8
  | Cmpgt_i16
  | Cmpgt_i32
  | Cmp_f64 of float_condition
  | I32_to_f64
  | I32_to_f32
  | F64_to_i32
  | F64_to_f32
  | F32_to_i32
  | F32_to_f64
  | I16_to_i8
  | I32_to_i16
  | I16_to_unsigned_i8
  | I32_to_unsigned_i16
  | SLL_i16
  | SLL_i32
  | SLL_i64
  | SRL_i16
  | SRL_i32
  | SRL_i64
  | SRA_i16
  | SRA_i32
  | SLLi_i16 of int
  | SLLi_i32 of int
  | SLLi_i64 of int
  | SRLi_i16 of int
  | SRLi_i32 of int
  | SRLi_i64 of int
  | SRAi_i16 of int
  | SRAi_i32 of int
  | Shuffle_64 of int
  | Shuffle_high_16 of int
  | Shuffle_low_16 of int
  | Interleave_high_8
  | Interleave_high_16
  | Interleave_high_64
  | Interleave_low_8
  | Interleave_low_16
  | Interleave_low_64

type sse3_operation =
  | Addsub_f32
  | Addsub_f64
  | Hadd_f32
  | Hadd_f64
  | Hsub_f32
  | Hsub_f64
  | Dup_low_64
  | Dup_odd_32
  | Dup_even_32

type ssse3_operation =
  | Abs_i8
  | Abs_i16
  | Abs_i32
  | Hadd_i16
  | Hadd_i32
  | Hadd_saturating_i16
  | Hsub_i16
  | Hsub_i32
  | Hsub_saturating_i16
  | Mulsign_i8
  | Mulsign_i16
  | Mulsign_i32
  | Shuffle_8
  | Alignr_i8 of int
  | Mul_unsigned_hadd_saturating_i8_to_i16

type sse41_operation =
  | Round_scalar_f64 of float_rounding
  | Blend_16 of int
  | Blend_32 of int
  | Blend_64 of int
  | Blendv_8
  | Blendv_32
  | Blendv_64
  | Cmpeq_i64
  | I8_sx_i16
  | I8_sx_i32
  | I8_sx_i64
  | I16_sx_i32
  | I16_sx_i64
  | I32_sx_i64
  | I8_zx_i16
  | I8_zx_i32
  | I8_zx_i64
  | I16_zx_i32
  | I16_zx_i64
  | I32_zx_i64
  | Dp_f32 of int
  | Dp_f64 of int
  | Extract_i8 of int
  | Extract_i16 of int
  | Extract_i32 of int
  | Extract_i64 of int
  | Insert_i8 of int
  | Insert_i16 of int
  | Insert_i32 of int
  | Insert_i64 of int
  | Max_i8
  | Max_i32
  | Max_unsigned_i16
  | Max_unsigned_i32
  | Min_i8
  | Min_i32
  | Min_unsigned_i16
  | Min_unsigned_i32
  | Round_f64 of float_rounding
  | Round_f32 of float_rounding
  | Multi_sad_unsigned_i8 of int
  | Minpos_unsigned_i16
  | Mullo_i32

type sse42_operation =
  | Cmpgt_i64
  | Cmpestrm of int
  | Cmpestra of int
  | Cmpestrc of int
  | Cmpestri of int
  | Cmpestro of int
  | Cmpestrs of int
  | Cmpestrz of int
  | Cmpistrm of int
  | Cmpistra of int
  | Cmpistrc of int
  | Cmpistri of int
  | Cmpistro of int
  | Cmpistrs of int
  | Cmpistrz of int
  | Crc32_64

type operation =
  | CLMUL of clmul_operation
  | BMI2 of bmi2_operation
  | SSE of sse_operation
  | SSE2 of sse2_operation
  | SSE3 of sse3_operation
  | SSSE3 of ssse3_operation
  | SSE41 of sse41_operation
  | SSE42 of sse42_operation

let equal_operation_clmul l r =
  match l, r with Clmul_64 l, Clmul_64 r -> Int.equal l r

let equal_operation_bmi2 l r =
  match l, r with
  | Deposit_64, Deposit_64 -> true
  | Extract_64, Extract_64 -> true
  | (Deposit_64 | Extract_64), _ -> false

let equal_operation_sse l r =
  match l, r with
  | Add_f32, Add_f32
  | Sub_f32, Sub_f32
  | Mul_f32, Mul_f32
  | Div_f32, Div_f32
  | Max_f32, Max_f32
  | Min_f32, Min_f32
  | Rcp_f32, Rcp_f32
  | Sqrt_f32, Sqrt_f32
  | Rsqrt_f32, Rsqrt_f32
  | High_64_to_low_64, High_64_to_low_64
  | Low_64_to_high_64, Low_64_to_high_64
  | Interleave_high_32, Interleave_high_32
  | Interleave_low_32, Interleave_low_32
  | Movemask_32, Movemask_32 ->
    true
  | Cmp_f32 l, Cmp_f32 r when float_condition_equal l r -> true
  | Shuffle_32 l, Shuffle_32 r when Int.equal l r -> true
  | ( ( Add_f32 | Sub_f32 | Mul_f32 | Div_f32 | Max_f32 | Min_f32 | Rcp_f32
      | Sqrt_f32 | Rsqrt_f32 | High_64_to_low_64 | Low_64_to_high_64
      | Interleave_high_32 | Interleave_low_32 | Movemask_32 | Cmp_f32 _
      | Shuffle_32 _ ),
      _ ) ->
    false

let equal_operation_sse2 l r =
  match l, r with
  | Min_scalar_f64, Min_scalar_f64
  | Max_scalar_f64, Max_scalar_f64
  | Sqrt_scalar_f64, Sqrt_scalar_f64
  | Cast_scalar_f64_i64, Cast_scalar_f64_i64
  | Sqrt_f64, Sqrt_f64
  | Add_i8, Add_i8
  | Add_i16, Add_i16
  | Add_i32, Add_i32
  | Add_i64, Add_i64
  | Add_f64, Add_f64
  | Add_saturating_unsigned_i8, Add_saturating_unsigned_i8
  | Add_saturating_unsigned_i16, Add_saturating_unsigned_i16
  | Add_saturating_i8, Add_saturating_i8
  | Add_saturating_i16, Add_saturating_i16
  | Sub_i8, Sub_i8
  | Sub_i16, Sub_i16
  | Sub_i32, Sub_i32
  | Sub_i64, Sub_i64
  | Sub_f64, Sub_f64
  | Sub_saturating_unsigned_i8, Sub_saturating_unsigned_i8
  | Sub_saturating_unsigned_i16, Sub_saturating_unsigned_i16
  | Sub_saturating_i8, Sub_saturating_i8
  | Sub_saturating_i16, Sub_saturating_i16
  | Max_unsigned_i8, Max_unsigned_i8
  | Max_i16, Max_i16
  | Max_f64, Max_f64
  | Min_unsigned_i8, Min_unsigned_i8
  | Mulhi_i16, Mulhi_i16
  | Mulhi_unsigned_i16, Mulhi_unsigned_i16
  | Mullo_i16, Mullo_i16
  | Mul_hadd_i16_to_i32, Mul_hadd_i16_to_i32
  | Min_i16, Min_i16
  | Min_f64, Min_f64
  | Mul_f64, Mul_f64
  | Div_f64, Div_f64
  | Avg_unsigned_i8, Avg_unsigned_i8
  | Avg_unsigned_i16, Avg_unsigned_i16
  | SAD_unsigned_i8, SAD_unsigned_i8
  | And_bits, And_bits
  | Andnot_bits, Andnot_bits
  | Or_bits, Or_bits
  | Xor_bits, Xor_bits
  | Movemask_8, Movemask_8
  | Movemask_64, Movemask_64
  | Cmpeq_i8, Cmpeq_i8
  | Cmpeq_i16, Cmpeq_i16
  | Cmpeq_i32, Cmpeq_i32
  | Cmpgt_i8, Cmpgt_i8
  | Cmpgt_i16, Cmpgt_i16
  | Cmpgt_i32, Cmpgt_i32
  | I32_to_f64, I32_to_f64
  | I32_to_f32, I32_to_f32
  | F64_to_i32, F64_to_i32
  | F64_to_f32, F64_to_f32
  | F32_to_i32, F32_to_i32
  | F32_to_f64, F32_to_f64
  | I16_to_i8, I16_to_i8
  | I32_to_i16, I32_to_i16
  | I16_to_unsigned_i8, I16_to_unsigned_i8
  | I32_to_unsigned_i16, I32_to_unsigned_i16
  | SLL_i16, SLL_i16
  | SLL_i32, SLL_i32
  | SLL_i64, SLL_i64
  | SRL_i16, SRL_i16
  | SRL_i32, SRL_i32
  | SRL_i64, SRL_i64
  | SRA_i16, SRA_i16
  | SRA_i32, SRA_i32
  | Interleave_high_8, Interleave_high_8
  | Interleave_high_16, Interleave_high_16
  | Interleave_high_64, Interleave_high_64
  | Interleave_low_8, Interleave_low_8
  | Interleave_low_16, Interleave_low_16
  | Interleave_low_64, Interleave_low_64 ->
    true
  | SLLi_i16 l, SLLi_i16 r
  | SLLi_i32 l, SLLi_i32 r
  | SLLi_i64 l, SLLi_i64 r
  | SRLi_i16 l, SRLi_i16 r
  | SRLi_i32 l, SRLi_i32 r
  | SRLi_i64 l, SRLi_i64 r
  | SRAi_i16 l, SRAi_i16 r
  | SRAi_i32 l, SRAi_i32 r
  | Shift_left_bytes l, Shift_left_bytes r
  | Shift_right_bytes l, Shift_right_bytes r
  | Shuffle_64 l, Shuffle_64 r
  | Shuffle_high_16 l, Shuffle_high_16 r
  | Shuffle_low_16 l, Shuffle_low_16 r
    when Int.equal l r ->
    true
  | Cmp_f64 l, Cmp_f64 r when float_condition_equal l r -> true
  | ( ( Add_i8 | Add_i16 | Add_i32 | Add_i64 | Add_f64 | Min_scalar_f64
      | Max_scalar_f64 | Cast_scalar_f64_i64 | Sqrt_scalar_f64 | Sqrt_f64
      | Add_saturating_unsigned_i8 | Add_saturating_unsigned_i16
      | Add_saturating_i8 | Add_saturating_i16 | Sub_i8 | Sub_i16 | Sub_i32
      | Sub_i64 | Sub_f64 | Sub_saturating_unsigned_i8
      | Sub_saturating_unsigned_i16 | Sub_saturating_i8 | Sub_saturating_i16
      | Max_unsigned_i8 | Max_i16 | Max_f64 | Min_unsigned_i8 | Min_i16
      | Min_f64 | Mul_f64 | Div_f64 | And_bits | Andnot_bits | Or_bits
      | Xor_bits | Movemask_8 | Movemask_64 | Cmpeq_i8 | Cmpeq_i16 | Cmpeq_i32
      | Cmpgt_i8 | Cmpgt_i16 | Cmpgt_i32 | I32_to_f64 | I32_to_f32 | F64_to_i32
      | F64_to_f32 | F32_to_i32 | F32_to_f64 | SLL_i16 | SLL_i32 | SLL_i64
      | SRL_i16 | SRL_i32 | SRL_i64 | SRA_i16 | SRA_i32 | I16_to_i8 | I32_to_i16
      | I16_to_unsigned_i8 | I32_to_unsigned_i16 | Avg_unsigned_i8
      | Avg_unsigned_i16 | SAD_unsigned_i8 | Interleave_high_8
      | Interleave_high_16 | Interleave_high_64 | Interleave_low_8
      | Interleave_low_16 | Interleave_low_64 | SLLi_i16 _ | SLLi_i32 _
      | SLLi_i64 _ | SRLi_i16 _ | SRLi_i32 _ | SRLi_i64 _ | SRAi_i16 _
      | SRAi_i32 _ | Shift_left_bytes _ | Shift_right_bytes _ | Cmp_f64 _
      | Shuffle_64 _ | Shuffle_high_16 _ | Shuffle_low_16 _ | Mulhi_i16
      | Mulhi_unsigned_i16 | Mullo_i16 | Mul_hadd_i16_to_i32 ),
      _ ) ->
    false

let equal_operation_sse3 l r =
  match l, r with
  | Addsub_f32, Addsub_f32
  | Addsub_f64, Addsub_f64
  | Hadd_f32, Hadd_f32
  | Hadd_f64, Hadd_f64
  | Hsub_f32, Hsub_f32
  | Hsub_f64, Hsub_f64
  | Dup_low_64, Dup_low_64
  | Dup_odd_32, Dup_odd_32
  | Dup_even_32, Dup_even_32 ->
    true
  | ( ( Addsub_f32 | Addsub_f64 | Hadd_f32 | Hadd_f64 | Hsub_f32 | Hsub_f64
      | Dup_low_64 | Dup_odd_32 | Dup_even_32 ),
      _ ) ->
    false

let equal_operation_ssse3 l r =
  match l, r with
  | Abs_i8, Abs_i8
  | Abs_i16, Abs_i16
  | Abs_i32, Abs_i32
  | Hadd_i16, Hadd_i16
  | Hadd_i32, Hadd_i32
  | Hadd_saturating_i16, Hadd_saturating_i16
  | Hsub_i16, Hsub_i16
  | Hsub_i32, Hsub_i32
  | Hsub_saturating_i16, Hsub_saturating_i16
  | Mulsign_i8, Mulsign_i8
  | Mulsign_i16, Mulsign_i16
  | Mulsign_i32, Mulsign_i32
  | ( Mul_unsigned_hadd_saturating_i8_to_i16,
      Mul_unsigned_hadd_saturating_i8_to_i16 )
  | Shuffle_8, Shuffle_8 ->
    true
  | Alignr_i8 l, Alignr_i8 r when Int.equal l r -> true
  | ( ( Abs_i8 | Abs_i16 | Abs_i32 | Hadd_i16 | Hadd_i32 | Hadd_saturating_i16
      | Hsub_i16 | Hsub_i32 | Hsub_saturating_i16 | Mulsign_i8 | Mulsign_i16
      | Mulsign_i32 | Shuffle_8 | Alignr_i8 _
      | Mul_unsigned_hadd_saturating_i8_to_i16 ),
      _ ) ->
    false

let equal_operation_sse41 l r =
  match l, r with
  | Blendv_8, Blendv_8
  | Blendv_32, Blendv_32
  | Blendv_64, Blendv_64
  | Cmpeq_i64, Cmpeq_i64
  | I8_sx_i16, I8_sx_i16
  | I8_sx_i32, I8_sx_i32
  | I8_sx_i64, I8_sx_i64
  | I16_sx_i32, I16_sx_i32
  | I16_sx_i64, I16_sx_i64
  | I32_sx_i64, I32_sx_i64
  | I8_zx_i16, I8_zx_i16
  | I8_zx_i32, I8_zx_i32
  | I8_zx_i64, I8_zx_i64
  | I16_zx_i32, I16_zx_i32
  | I16_zx_i64, I16_zx_i64
  | I32_zx_i64, I32_zx_i64
  | Max_i8, Max_i8
  | Max_i32, Max_i32
  | Max_unsigned_i16, Max_unsigned_i16
  | Max_unsigned_i32, Max_unsigned_i32
  | Min_i8, Min_i8
  | Min_i32, Min_i32
  | Min_unsigned_i16, Min_unsigned_i16
  | Min_unsigned_i32, Min_unsigned_i32
  | Mullo_i32, Mullo_i32
  | Minpos_unsigned_i16, Minpos_unsigned_i16 ->
    true
  | Blend_16 l, Blend_16 r
  | Blend_32 l, Blend_32 r
  | Blend_64 l, Blend_64 r
  | Dp_f32 l, Dp_f32 r
  | Dp_f64 l, Dp_f64 r
  | Extract_i8 l, Extract_i8 r
  | Extract_i16 l, Extract_i16 r
  | Extract_i32 l, Extract_i32 r
  | Extract_i64 l, Extract_i64 r
  | Insert_i8 l, Insert_i8 r
  | Insert_i16 l, Insert_i16 r
  | Insert_i32 l, Insert_i32 r
  | Insert_i64 l, Insert_i64 r
  | Multi_sad_unsigned_i8 l, Multi_sad_unsigned_i8 r
    when Int.equal l r ->
    true
  | Round_scalar_f64 l, Round_scalar_f64 r
  | Round_f64 l, Round_f64 r
  | Round_f32 l, Round_f32 r
    when float_rounding_equal l r ->
    true
  | ( ( Multi_sad_unsigned_i8 _ | Blendv_8 | Blendv_32 | Blendv_64 | Cmpeq_i64
      | I8_sx_i16 | I8_sx_i32 | I8_sx_i64 | I16_sx_i32 | I16_sx_i64 | I32_sx_i64
      | I8_zx_i16 | I8_zx_i32 | I8_zx_i64 | I16_zx_i32 | I16_zx_i64 | I32_zx_i64
      | Max_i8 | Max_i32 | Max_unsigned_i16 | Max_unsigned_i32 | Min_i8
      | Min_i32 | Min_unsigned_i16 | Min_unsigned_i32 | Minpos_unsigned_i16
      | Blend_16 _ | Blend_32 _ | Blend_64 _ | Dp_f32 _ | Dp_f64 _ | Mullo_i32
      | Extract_i8 _ | Extract_i16 _ | Extract_i32 _ | Extract_i64 _
      | Insert_i8 _ | Insert_i16 _ | Insert_i32 _ | Insert_i64 _ | Round_f64 _
      | Round_scalar_f64 _ | Round_f32 _ ),
      _ ) ->
    false

let equal_operation_sse42 l r =
  match l, r with
  | Cmpgt_i64, Cmpgt_i64 | Crc32_64, Crc32_64 -> true
  | Cmpestrm l, Cmpestrm r
  | Cmpestra l, Cmpestra r
  | Cmpestrc l, Cmpestrc r
  | Cmpestri l, Cmpestri r
  | Cmpestro l, Cmpestro r
  | Cmpestrs l, Cmpestrs r
  | Cmpestrz l, Cmpestrz r
  | Cmpistrm l, Cmpistrm r
  | Cmpistra l, Cmpistra r
  | Cmpistrc l, Cmpistrc r
  | Cmpistri l, Cmpistri r
  | Cmpistro l, Cmpistro r
  | Cmpistrs l, Cmpistrs r
  | Cmpistrz l, Cmpistrz r
    when Int.equal l r ->
    true
  | ( ( Cmpgt_i64 | Crc32_64 | Cmpestrm _ | Cmpestra _ | Cmpestrc _ | Cmpestro _
      | Cmpestrs _ | Cmpestrz _ | Cmpistrm _ | Cmpistra _ | Cmpistrc _
      | Cmpestri _ | Cmpistri _ | Cmpistro _ | Cmpistrs _ | Cmpistrz _ ),
      _ ) ->
    false

let equal_operation l r =
  match l, r with
  | CLMUL l, CLMUL r -> equal_operation_clmul l r
  | BMI2 l, BMI2 r -> equal_operation_bmi2 l r
  | SSE l, SSE r -> equal_operation_sse l r
  | SSE2 l, SSE2 r -> equal_operation_sse2 l r
  | SSE3 l, SSE3 r -> equal_operation_sse3 l r
  | SSSE3 l, SSSE3 r -> equal_operation_ssse3 l r
  | SSE41 l, SSE41 r -> equal_operation_sse41 l r
  | SSE42 l, SSE42 r -> equal_operation_sse42 l r
  | ( (CLMUL _ | BMI2 _ | SSE _ | SSE2 _ | SSE3 _ | SSSE3 _ | SSE41 _ | SSE42 _),
      _ ) ->
    false

let print_float_condition ppf = function
  | EQf -> pp_print_string ppf "eq"
  | LTf -> pp_print_string ppf "lt"
  | LEf -> pp_print_string ppf "le"
  | UNORDf -> pp_print_string ppf "unord"
  | NEQf -> pp_print_string ppf "neq"
  | NLTf -> pp_print_string ppf "nlt"
  | NLEf -> pp_print_string ppf "nle"
  | ORDf -> pp_print_string ppf "ord"

let print_float_rounding ppf = function
  | RoundUp -> pp_print_string ppf "up"
  | RoundDown -> pp_print_string ppf "down"
  | RoundNearest -> pp_print_string ppf "nearest"
  | RoundTruncate -> pp_print_string ppf "truncate"
  | RoundCurrent -> pp_print_string ppf "current"

let print_operation_clmul printreg op ppf arg =
  match op with
  | Clmul_64 i ->
    fprintf ppf "clmul_64[%d] %a %a" i printreg arg.(0) printreg arg.(1)

let print_operation_bmi2 printreg op ppf arg =
  match op with
  | Extract_64 ->
    fprintf ppf "extract_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Deposit_64 ->
    fprintf ppf "deposit_64 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation_sse printreg op ppf arg =
  match op with
  | Cmp_f32 i ->
    fprintf ppf "cmp_f32[%a] %a %a" print_float_condition i printreg arg.(0)
      printreg arg.(1)
  | Add_f32 -> fprintf ppf "add_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_f32 -> fprintf ppf "sub_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Mul_f32 -> fprintf ppf "mul_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Div_f32 -> fprintf ppf "div_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_f32 -> fprintf ppf "max_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_f32 -> fprintf ppf "min_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Rcp_f32 -> fprintf ppf "rcp_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Sqrt_f32 -> fprintf ppf "sqrt_f32 %a" printreg arg.(0)
  | Rsqrt_f32 -> fprintf ppf "rsqrt_f32 %a" printreg arg.(0)
  | Movemask_32 -> fprintf ppf "movemask_32 %a" printreg arg.(0)
  | Shuffle_32 i ->
    fprintf ppf "shuffle_32[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | High_64_to_low_64 ->
    fprintf ppf "high_64_to_low_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Low_64_to_high_64 ->
    fprintf ppf "low_64_to_high_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_high_32 ->
    fprintf ppf "interleave_high_32 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_low_32 ->
    fprintf ppf "interleave_low_32 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation_sse2 printreg op ppf arg =
  match op with
  | Sqrt_scalar_f64 -> fprintf ppf "sqrt_scalar_f64 %a" printreg arg.(0)
  | Min_scalar_f64 ->
    fprintf ppf "min_scalar_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_scalar_f64 ->
    fprintf ppf "max_scalar_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Sqrt_f64 -> fprintf ppf "sqrt_f64 %a" printreg arg.(0)
  | Add_i8 -> fprintf ppf "add_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Add_i16 -> fprintf ppf "add_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Add_i32 -> fprintf ppf "add_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Add_i64 -> fprintf ppf "add_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | Add_f64 -> fprintf ppf "add_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Add_saturating_i8 ->
    fprintf ppf "add_saturating_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Add_saturating_i16 ->
    fprintf ppf "add_saturating_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Add_saturating_unsigned_i8 ->
    fprintf ppf "add_saturating_unsigned_i8 %a %a" printreg arg.(0) printreg
      arg.(1)
  | Add_saturating_unsigned_i16 ->
    fprintf ppf "add_saturating_unsigned_i16 %a %a" printreg arg.(0) printreg
      arg.(1)
  | Sub_i8 -> fprintf ppf "sub_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_i16 -> fprintf ppf "sub_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_i32 -> fprintf ppf "sub_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_i64 -> fprintf ppf "sub_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_f64 -> fprintf ppf "sub_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_saturating_i8 ->
    fprintf ppf "sub_saturating_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_saturating_i16 ->
    fprintf ppf "sub_saturating_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_saturating_unsigned_i8 ->
    fprintf ppf "sub_saturating_unsigned_i8 %a %a" printreg arg.(0) printreg
      arg.(1)
  | Sub_saturating_unsigned_i16 ->
    fprintf ppf "sub_saturating_unsigned_i16 %a %a" printreg arg.(0) printreg
      arg.(1)
  | Max_unsigned_i8 ->
    fprintf ppf "max_unsigned_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_i16 -> fprintf ppf "max_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_f64 -> fprintf ppf "max_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_unsigned_i8 ->
    fprintf ppf "min_unsigned_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_i16 -> fprintf ppf "min_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_f64 -> fprintf ppf "min_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Mul_f64 -> fprintf ppf "mul_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Div_f64 -> fprintf ppf "div_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Avg_unsigned_i8 ->
    fprintf ppf "avg_unsigned_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Avg_unsigned_i16 ->
    fprintf ppf "avg_unsigned_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | SAD_unsigned_i8 ->
    fprintf ppf "sad_unsigned_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Mulhi_i16 -> fprintf ppf "mulhi_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Mulhi_unsigned_i16 ->
    fprintf ppf "mulhi_unsigned_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Mullo_i16 -> fprintf ppf "mullo_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Mul_hadd_i16_to_i32 ->
    fprintf ppf "mul_hadd_i16_to_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | And_bits -> fprintf ppf "and_bits %a %a" printreg arg.(0) printreg arg.(1)
  | Andnot_bits ->
    fprintf ppf "andnot_bits %a %a" printreg arg.(0) printreg arg.(1)
  | Or_bits -> fprintf ppf "or_bits %a %a" printreg arg.(0) printreg arg.(1)
  | Xor_bits -> fprintf ppf "xor_bits %a %a" printreg arg.(0) printreg arg.(1)
  | Movemask_8 -> fprintf ppf "movemask_8 %a" printreg arg.(0)
  | Movemask_64 -> fprintf ppf "movemask_64 %a" printreg arg.(0)
  | Cmpeq_i8 -> fprintf ppf "cmpeq_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Cmpeq_i16 -> fprintf ppf "cmpeq_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Cmpeq_i32 -> fprintf ppf "cmpeq_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Cmpgt_i8 -> fprintf ppf "cmpgt_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Cmpgt_i16 -> fprintf ppf "cmpgt_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Cmpgt_i32 -> fprintf ppf "cmpgt_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Cast_scalar_f64_i64 ->
    fprintf ppf "cast_scalar_f64_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | I32_to_f64 ->
    fprintf ppf "i32_to_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | I32_to_f32 ->
    fprintf ppf "i32_to_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | F64_to_i32 ->
    fprintf ppf "f64_to_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | F64_to_f32 ->
    fprintf ppf "f64_to_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | F32_to_i32 ->
    fprintf ppf "f32_to_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | F32_to_f64 ->
    fprintf ppf "f32_to_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | I16_to_i8 -> fprintf ppf "i16_to_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | I32_to_i16 ->
    fprintf ppf "i32_to_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | I16_to_unsigned_i8 ->
    fprintf ppf "i16_to_unsigned_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | I32_to_unsigned_i16 ->
    fprintf ppf "i32_to_unsigned_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | SLL_i16 -> fprintf ppf "sll_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | SLL_i32 -> fprintf ppf "sll_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | SLL_i64 -> fprintf ppf "sll_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | SRL_i16 -> fprintf ppf "srl_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | SRL_i32 -> fprintf ppf "srl_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | SRL_i64 -> fprintf ppf "srl_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | SRA_i16 -> fprintf ppf "sra_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | SRA_i32 -> fprintf ppf "sra_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_high_8 ->
    fprintf ppf "interleave_high_8 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_high_16 ->
    fprintf ppf "interleave_high_16 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_high_64 ->
    fprintf ppf "interleave_high_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_low_8 ->
    fprintf ppf "interleave_low_8 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_low_16 ->
    fprintf ppf "interleave_low_16 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_low_64 ->
    fprintf ppf "interleave_low_64 %a %a" printreg arg.(0) printreg arg.(1)
  | SLLi_i16 i -> fprintf ppf "slli_i16[%d] %a" i printreg arg.(0)
  | SLLi_i32 i -> fprintf ppf "slli_i32[%d] %a" i printreg arg.(0)
  | SLLi_i64 i -> fprintf ppf "slli_i64[%d] %a" i printreg arg.(0)
  | SRLi_i16 i -> fprintf ppf "srli_i16[%d] %a" i printreg arg.(0)
  | SRLi_i32 i -> fprintf ppf "srli_i32[%d] %a" i printreg arg.(0)
  | SRLi_i64 i -> fprintf ppf "srli_i64[%d] %a" i printreg arg.(0)
  | SRAi_i16 i -> fprintf ppf "srai_i16[%d] %a" i printreg arg.(0)
  | SRAi_i32 i -> fprintf ppf "srai_i32[%d] %a" i printreg arg.(0)
  | Shift_left_bytes i ->
    fprintf ppf "shift_left_bytes[%d] %a" i printreg arg.(0)
  | Shift_right_bytes i ->
    fprintf ppf "shift_right_bytes[%d] %a" i printreg arg.(0)
  | Cmp_f64 i ->
    fprintf ppf "cmp_f64[%a] %a %a" print_float_condition i printreg arg.(0)
      printreg arg.(1)
  | Shuffle_64 i ->
    fprintf ppf "shuffle_64[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Shuffle_high_16 i ->
    fprintf ppf "shuffle_high_16[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Shuffle_low_16 i ->
    fprintf ppf "shuffle_low_16[%d] %a %a" i printreg arg.(0) printreg arg.(1)

let print_operation_sse3 printreg op ppf arg =
  match op with
  | Addsub_f32 ->
    fprintf ppf "addsub_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Addsub_f64 ->
    fprintf ppf "addsub_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Hadd_f32 -> fprintf ppf "hadd_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Hadd_f64 -> fprintf ppf "hadd_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Hsub_f32 -> fprintf ppf "hsub_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Hsub_f64 -> fprintf ppf "hsub_f64 %a %a" printreg arg.(0) printreg arg.(1)
  | Dup_low_64 ->
    fprintf ppf "dup_low_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Dup_odd_32 ->
    fprintf ppf "dup_odd_32 %a %a" printreg arg.(0) printreg arg.(1)
  | Dup_even_32 ->
    fprintf ppf "dup_even_32 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation_ssse3 printreg op ppf arg =
  match op with
  | Abs_i8 -> fprintf ppf "abs_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Abs_i16 -> fprintf ppf "abs_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Abs_i32 -> fprintf ppf "abs_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Hadd_i16 -> fprintf ppf "hadd_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Hadd_i32 -> fprintf ppf "hadd_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Hadd_saturating_i16 ->
    fprintf ppf "hadd_saturating_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Hsub_i16 -> fprintf ppf "hsub_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Hsub_i32 -> fprintf ppf "hsub_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Hsub_saturating_i16 ->
    fprintf ppf "hsub_saturating_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Mulsign_i8 ->
    fprintf ppf "mulsign_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Mulsign_i16 ->
    fprintf ppf "mulsign_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Mulsign_i32 ->
    fprintf ppf "mulsign_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Shuffle_8 -> fprintf ppf "shuffle_8 %a %a" printreg arg.(0) printreg arg.(1)
  | Alignr_i8 i ->
    fprintf ppf "alignr_i8[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Mul_unsigned_hadd_saturating_i8_to_i16 ->
    fprintf ppf "mul_unsigned_hadd_saturating_i8_to_i16 %a %a" printreg arg.(0)
      printreg arg.(1)

let print_operation_sse41 printreg op ppf arg =
  match op with
  | Blend_16 i ->
    fprintf ppf "blend_16[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Blend_32 i ->
    fprintf ppf "blend_32[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Blend_64 i ->
    fprintf ppf "blend_64[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Blendv_8 -> fprintf ppf "blendv_8 %a %a" printreg arg.(0) printreg arg.(1)
  | Blendv_32 -> fprintf ppf "blendv_32 %a %a" printreg arg.(0) printreg arg.(1)
  | Blendv_64 -> fprintf ppf "blendv_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Cmpeq_i64 -> fprintf ppf "cmpeq_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | I8_sx_i16 -> fprintf ppf "i8_sx_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | I8_sx_i32 -> fprintf ppf "i8_sx_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | I8_sx_i64 -> fprintf ppf "i8_sx_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | I16_sx_i32 ->
    fprintf ppf "i16_sx_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | I16_sx_i64 ->
    fprintf ppf "i16_sx_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | I32_sx_i64 ->
    fprintf ppf "i32_sx_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | I8_zx_i16 -> fprintf ppf "I8_zx_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | I8_zx_i32 -> fprintf ppf "I8_zx_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | I8_zx_i64 -> fprintf ppf "I8_zx_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | I16_zx_i32 ->
    fprintf ppf "I16_zx_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | I16_zx_i64 ->
    fprintf ppf "I16_zx_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | I32_zx_i64 ->
    fprintf ppf "I32_zx_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_i8 -> fprintf ppf "max_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_i32 -> fprintf ppf "max_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_unsigned_i16 ->
    fprintf ppf "max_unsigned_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_unsigned_i32 ->
    fprintf ppf "max_unsigned_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_i8 -> fprintf ppf "min_i8 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_i32 -> fprintf ppf "min_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_unsigned_i16 ->
    fprintf ppf "min_unsigned_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_unsigned_i32 ->
    fprintf ppf "min_unsigned_i32 %a %a" printreg arg.(0) printreg arg.(1)
  | Dp_f32 i ->
    fprintf ppf "dp_f32[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Dp_f64 i ->
    fprintf ppf "dp_f64[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Extract_i8 i -> fprintf ppf "extract_i8[%d] %a" i printreg arg.(0)
  | Extract_i16 i -> fprintf ppf "extract_i16[%d] %a" i printreg arg.(0)
  | Extract_i32 i -> fprintf ppf "extract_i32[%d] %a" i printreg arg.(0)
  | Extract_i64 i -> fprintf ppf "extract_i64[%d] %a" i printreg arg.(0)
  | Insert_i8 i -> fprintf ppf "insert_i8[%d] %a" i printreg arg.(0)
  | Insert_i16 i -> fprintf ppf "insert_i16[%d] %a" i printreg arg.(0)
  | Insert_i32 i -> fprintf ppf "insert_i32[%d] %a" i printreg arg.(0)
  | Insert_i64 i -> fprintf ppf "insert_i64[%d] %a" i printreg arg.(0)
  | Round_scalar_f64 i ->
    fprintf ppf "round_scalar_f64[%a] %a %a" print_float_rounding i printreg
      arg.(0) printreg arg.(1)
  | Round_f64 i ->
    fprintf ppf "round_f64[%a] %a %a" print_float_rounding i printreg arg.(0)
      printreg arg.(1)
  | Round_f32 i ->
    fprintf ppf "round_f32[%a] %a %a" print_float_rounding i printreg arg.(0)
      printreg arg.(1)
  | Multi_sad_unsigned_i8 i ->
    fprintf ppf "multi_sad_unsigned_i8[%d] %a %a" i printreg arg.(0) printreg
      arg.(1)
  | Minpos_unsigned_i16 ->
    fprintf ppf "minpos_unsigned_i16 %a %a" printreg arg.(0) printreg arg.(1)
  | Mullo_i32 -> fprintf ppf "mullo_i32 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation_sse42 printreg op ppf arg =
  match op with
  | Cmpestrm i ->
    fprintf ppf "cmpestrm[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpestra i ->
    fprintf ppf "cmpestra[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpestrc i ->
    fprintf ppf "cmpestrc[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpestri i ->
    fprintf ppf "cmpestri[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpestro i ->
    fprintf ppf "cmpestro[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpestrs i ->
    fprintf ppf "cmpestrs[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpestrz i ->
    fprintf ppf "cmpestrz[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpistrm i ->
    fprintf ppf "cmpistrm[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpistra i ->
    fprintf ppf "cmpistra[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpistrc i ->
    fprintf ppf "cmpistrc[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpistri i ->
    fprintf ppf "cmpistri[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpistro i ->
    fprintf ppf "cmpistro[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpistrs i ->
    fprintf ppf "cmpistrs[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpistrz i ->
    fprintf ppf "cmpistrz[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Cmpgt_i64 -> fprintf ppf "cmpgt_i64 %a %a" printreg arg.(0) printreg arg.(1)
  | Crc32_64 -> fprintf ppf "crc32_64 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation printreg op ppf arg =
  match op with
  | CLMUL op -> print_operation_clmul printreg op ppf arg
  | BMI2 op -> print_operation_bmi2 printreg op ppf arg
  | SSE op -> print_operation_sse printreg op ppf arg
  | SSE2 op -> print_operation_sse2 printreg op ppf arg
  | SSE3 op -> print_operation_sse3 printreg op ppf arg
  | SSSE3 op -> print_operation_ssse3 printreg op ppf arg
  | SSE41 op -> print_operation_sse41 printreg op ppf arg
  | SSE42 op -> print_operation_sse42 printreg op ppf arg

let class_of_operation_clmul = function Clmul_64 _ -> Pure

let class_of_operation_bmi2 = function Deposit_64 | Extract_64 -> Pure

let class_of_operation_sse = function
  | Cmp_f32 _ | Add_f32 | Sub_f32 | Mul_f32 | Div_f32 | Max_f32 | Min_f32
  | Rcp_f32 | Sqrt_f32 | Rsqrt_f32 | High_64_to_low_64 | Low_64_to_high_64
  | Interleave_high_32 | Interleave_low_32 | Movemask_32 | Shuffle_32 _ ->
    Pure

let class_of_operation_sse2 = function
  | Add_i8 | Add_i16 | Add_i32 | Add_i64 | Add_f64 | Add_saturating_i8
  | Cast_scalar_f64_i64 | Min_scalar_f64 | Max_scalar_f64 | Sqrt_scalar_f64
  | Sqrt_f64 | Add_saturating_i16 | Add_saturating_unsigned_i8
  | Add_saturating_unsigned_i16 | Sub_i8 | Sub_i16 | Sub_i32 | Sub_i64 | Sub_f64
  | Sub_saturating_i8 | Sub_saturating_i16 | Sub_saturating_unsigned_i8
  | Sub_saturating_unsigned_i16 | Max_unsigned_i8 | Max_i16 | Max_f64
  | Min_unsigned_i8 | Min_i16 | Min_f64 | Mul_f64 | Div_f64 | Avg_unsigned_i8
  | Avg_unsigned_i16 | SAD_unsigned_i8 | Mulhi_i16 | Mulhi_unsigned_i16
  | Mullo_i16 | Mul_hadd_i16_to_i32 | And_bits | Andnot_bits | Or_bits
  | Xor_bits | Movemask_8 | Movemask_64 | Shift_left_bytes _
  | Shift_right_bytes _ | Cmpeq_i8 | Cmpeq_i16 | Cmpeq_i32 | Cmpgt_i8
  | Cmpgt_i16 | Cmpgt_i32 | Cmp_f64 _ | I32_to_f64 | I32_to_f32 | F64_to_i32
  | F64_to_f32 | F32_to_i32 | F32_to_f64 | I16_to_i8 | I32_to_i16
  | I16_to_unsigned_i8 | I32_to_unsigned_i16 | SLL_i16 | SLL_i32 | SLL_i64
  | SRL_i16 | SRL_i32 | SRL_i64 | SRA_i16 | SRA_i32 | SLLi_i16 _ | SLLi_i32 _
  | SLLi_i64 _ | SRLi_i16 _ | SRLi_i32 _ | SRLi_i64 _ | SRAi_i16 _ | SRAi_i32 _
  | Shuffle_64 _ | Shuffle_high_16 _ | Shuffle_low_16 _ | Interleave_high_8
  | Interleave_high_16 | Interleave_high_64 | Interleave_low_8
  | Interleave_low_16 | Interleave_low_64 ->
    Pure

let class_of_operation_sse3 = function
  | Addsub_f32 | Addsub_f64 | Hadd_f32 | Hadd_f64 | Hsub_f32 | Hsub_f64
  | Dup_low_64 | Dup_odd_32 | Dup_even_32 ->
    Pure

let class_of_operation_ssse3 = function
  | Abs_i8 | Abs_i16 | Abs_i32 | Hadd_i16 | Hadd_i32 | Hadd_saturating_i16
  | Hsub_i16 | Hsub_i32 | Hsub_saturating_i16 | Mulsign_i8 | Mulsign_i16
  | Mulsign_i32 | Shuffle_8 | Alignr_i8 _
  | Mul_unsigned_hadd_saturating_i8_to_i16 ->
    Pure

let class_of_operation_sse41 = function
  | Blend_16 _ | Blend_32 _ | Blend_64 _ | Blendv_8 | Blendv_32 | Blendv_64
  | Cmpeq_i64 | I8_sx_i16 | I8_sx_i32 | I8_sx_i64 | I16_sx_i32 | I16_sx_i64
  | I32_sx_i64 | I8_zx_i16 | I8_zx_i32 | I8_zx_i64 | I16_zx_i32 | I16_zx_i64
  | I32_zx_i64 | Dp_f32 _ | Dp_f64 _ | Extract_i8 _ | Extract_i16 _
  | Extract_i32 _ | Extract_i64 _ | Insert_i8 _ | Insert_i16 _ | Insert_i32 _
  | Insert_i64 _ | Max_i8 | Max_i32 | Max_unsigned_i16 | Max_unsigned_i32
  | Min_i8 | Min_i32 | Min_unsigned_i16 | Min_unsigned_i32 | Round_f64 _
  | Round_scalar_f64 _ | Round_f32 _ | Multi_sad_unsigned_i8 _
  | Minpos_unsigned_i16 | Mullo_i32 ->
    Pure

let class_of_operation_sse42 = function
  | Cmpgt_i64 | Cmpestrm _ | Cmpestra _ | Cmpestrc _ | Cmpestri _ | Cmpestro _
  | Cmpestrs _ | Cmpestrz _ | Cmpistrm _ | Cmpistra _ | Cmpistrc _ | Cmpistri _
  | Cmpistro _ | Cmpistrs _ | Cmpistrz _ | Crc32_64 ->
    Pure

let class_of_operation op =
  match op with
  | CLMUL op -> class_of_operation_clmul op
  | BMI2 op -> class_of_operation_bmi2 op
  | SSE op -> class_of_operation_sse op
  | SSE2 op -> class_of_operation_sse2 op
  | SSE3 op -> class_of_operation_sse3 op
  | SSSE3 op -> class_of_operation_ssse3 op
  | SSE41 op -> class_of_operation_sse41 op
  | SSE42 op -> class_of_operation_sse42 op

let is_pure op = match class_of_operation op with Pure -> true
