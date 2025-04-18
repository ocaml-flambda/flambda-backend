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

open! Int_replace_polymorphic_compare

(* SIMD instruction selection for AMD64 *)

open Arch
open Amd64_simd_instrs

type error = Bad_immediate of string

exception Error of error

module Seq = Simd.Seq

let instr instr ?i args = Some (Simd.Instruction { instr; imm = i }, args)

let seq seq ?i args = Some (Simd.Sequence { seq; imm = i }, args)

let bad_immediate fmt =
  Format.kasprintf (fun msg -> raise (Error (Bad_immediate msg))) fmt

(* Assumes untagged int *)
let extract_constant args name ~max =
  match args with
  | Cmm.Cconst_int (i, _) :: args ->
    if i < 0 || i > max
    then
      bad_immediate "Immediate for %s must be in range [0,%d] (got %d)" name max
        i;
    i, args
  | []
  | Cmm.(
      ( Cconst_float _
      | Cconst_natint (_, _)
      | Cconst_float32 (_, _)
      | Cconst_vec128 (_, _)
      | Cconst_symbol (_, _)
      | Cvar _
      | Clet (_, _, _)
      | Cphantom_let (_, _, _)
      | Ctuple _
      | Cop (_, _, _)
      | Csequence (_, _)
      | Cifthenelse (_, _, _, _, _, _)
      | Cswitch (_, _, _, _)
      | Ccatch (_, _, _)
      | Cexit (_, _, _) ))
    :: _ ->
    bad_immediate "Did not get integer immediate for %s" name

let int_of_float_rounding : X86_ast.rounding -> int = function
  | RoundNearest -> 0x8
  | RoundDown -> 0x9
  | RoundUp -> 0xA
  | RoundTruncate -> 0xB
  | RoundCurrent -> 0xC

let check_float_rounding = function
  (* Starts at 8, as these rounding modes also imply _MM_FROUND_NO_EXC (0x8) *)
  | 0x8 | 0x9 | 0xA | 0xB | 0xC -> ()
  | i -> bad_immediate "Invalid float rounding immediate: %d" i

let select_operation_clmul op args =
  if not (Arch.Extension.enabled CLMUL)
  then None
  else
    match op with
    | "caml_clmul_int64x2" ->
      let i, args = extract_constant args ~max:31 op in
      instr pclmulqdq ~i args
    | _ -> None

let select_operation_bmi2 op args =
  if not (Arch.Extension.enabled BMI2)
  then None
  else
    match op with
    | "caml_bmi2_int64_extract_bits" -> instr pext_r64_r64_r64m64 args
    | "caml_bmi2_int64_deposit_bits" -> instr pdep_r64_r64_r64m64 args
    | _ -> None

let select_operation_sse op args =
  match op with
  | "caml_sse_float32_sqrt" | "sqrtf" -> instr sqrtss args
  | "caml_simd_float32_max" | "caml_sse_float32_max" -> instr maxss args
  | "caml_simd_float32_min" | "caml_sse_float32_min" -> instr minss args
  | "caml_sse_cast_float32_int64" | "caml_simd_cast_float32_int64" ->
    instr cvtss2si_r64_Xm32 args
  | "caml_sse_float32x4_cmp" ->
    let i, args = extract_constant args ~max:7 op in
    instr cmpps ~i args
  | "caml_sse_float32x4_add" -> instr addps args
  | "caml_sse_float32x4_sub" -> instr subps args
  | "caml_sse_float32x4_mul" -> instr mulps args
  | "caml_sse_float32x4_div" -> instr divps args
  | "caml_sse_float32x4_max" -> instr maxps args
  | "caml_sse_float32x4_min" -> instr minps args
  | "caml_sse_float32x4_rcp" -> instr rcpps args
  | "caml_sse_float32x4_rsqrt" -> instr rsqrtps args
  | "caml_sse_float32x4_sqrt" -> instr sqrtps args
  | "caml_sse_vec128_high_64_to_low_64" -> instr movhlps args
  | "caml_sse_vec128_low_64_to_high_64" -> instr movlhps args
  | "caml_sse_vec128_interleave_high_32" -> instr unpckhps args
  | "caml_simd_vec128_interleave_low_32" | "caml_sse_vec128_interleave_low_32"
    ->
    instr unpcklps args
  | "caml_sse_vec128_movemask_32" -> instr movmskps args
  | "caml_sse_vec128_shuffle_32" ->
    let i, args = extract_constant args ~max:0xff op in
    instr shufps ~i args
  | _ -> None

let select_operation_sse2 op args =
  match op with
  | "caml_sse2_float64_sqrt" | "sqrt" -> instr sqrtsd args
  | "caml_sse2_float64_max" -> instr maxsd args
  | "caml_sse2_float64_min" -> instr minsd args
  | "caml_sse2_cast_float64_int64" -> instr cvtsd2si_r64_Xm64 args
  | "caml_sse2_float64x2_sqrt" -> instr sqrtpd args
  | "caml_sse2_int8x16_add" -> instr paddb args
  | "caml_sse2_int16x8_add" -> instr paddw args
  | "caml_sse2_int32x4_add" -> instr paddd args
  | "caml_simd_int64x2_add" | "caml_sse2_int64x2_add" -> instr paddq args
  | "caml_sse2_float64x2_add" -> instr addpd args
  | "caml_sse2_int8x16_add_saturating" -> instr paddsb args
  | "caml_sse2_int16x8_add_saturating" -> instr paddsw args
  | "caml_sse2_int8x16_add_saturating_unsigned" -> instr paddusb args
  | "caml_sse2_int16x8_add_saturating_unsigned" -> instr paddusw args
  | "caml_sse2_int8x16_sub" -> instr psubb args
  | "caml_sse2_int16x8_sub" -> instr psubw args
  | "caml_sse2_int32x4_sub" -> instr psubd args
  | "caml_simd_int64x2_sub" | "caml_sse2_int64x2_sub" ->
    instr psubq_X_Xm128 args
  | "caml_sse2_float64x2_sub" -> instr subpd args
  | "caml_sse2_int8x16_sub_saturating" -> instr psubsb args
  | "caml_sse2_int16x8_sub_saturating" -> instr psubsw args
  | "caml_sse2_int8x16_sub_saturating_unsigned" -> instr psubusb args
  | "caml_sse2_int16x8_sub_saturating_unsigned" -> instr psubusw args
  | "caml_sse2_int8x16_max_unsigned" -> instr pmaxub_X_Xm128 args
  | "caml_sse2_int16x8_max" -> instr pmaxsw_X_Xm128 args
  | "caml_sse2_float64x2_max" -> instr maxpd args
  | "caml_sse2_int8x16_min_unsigned" -> instr pminub_X_Xm128 args
  | "caml_sse2_int16x8_min" -> instr pminsw_X_Xm128 args
  | "caml_sse2_float64x2_min" -> instr minpd args
  | "caml_sse2_float64x2_mul" -> instr mulpd args
  | "caml_sse2_float64x2_div" -> instr divpd args
  | "caml_sse2_vec128_and" -> instr pand args
  | "caml_sse2_vec128_andnot" -> instr pandn args
  | "caml_sse2_vec128_or" -> instr por args
  | "caml_sse2_vec128_xor" -> instr pxor args
  | "caml_sse2_vec128_movemask_8" -> instr pmovmskb_r64_X args
  | "caml_sse2_vec128_movemask_64" -> instr movmskpd args
  | "caml_sse2_vec128_shift_left_bytes" ->
    let i, args = extract_constant args ~max:15 op in
    instr pslldq ~i args
  | "caml_sse2_vec128_shift_right_bytes" ->
    let i, args = extract_constant args ~max:15 op in
    instr psrldq ~i args
  | "caml_sse2_int8x16_cmpeq" -> instr pcmpeqb args
  | "caml_sse2_int16x8_cmpeq" -> instr pcmpeqw args
  | "caml_sse2_int32x4_cmpeq" -> instr pcmpeqd args
  | "caml_sse2_int8x16_cmpgt" -> instr pcmpgtb args
  | "caml_sse2_int16x8_cmpgt" -> instr pcmpgtw args
  | "caml_sse2_int32x4_cmpgt" -> instr pcmpgtd args
  | "caml_sse2_float64x2_cmp" ->
    let i, args = extract_constant args ~max:0x1f op in
    instr cmppd ~i args
  | "caml_sse2_cvt_int32x4_float64x2" -> instr cvtdq2pd args
  | "caml_sse2_cvt_int32x4_float32x4" -> instr cvtdq2ps args
  | "caml_sse2_cvt_float64x2_int32x4" -> instr cvtpd2dq args
  | "caml_sse2_cvt_float64x2_float32x4" -> instr cvtpd2ps args
  | "caml_sse2_cvt_float32x4_int32x4" -> instr cvtps2dq args
  | "caml_sse2_cvt_float32x4_float64x2" -> instr cvtps2pd args
  | "caml_sse2_cvt_int16x8_int8x16_saturating" -> instr packsswb args
  | "caml_sse2_cvt_int32x4_int16x8_saturating" -> instr packssdw args
  | "caml_sse2_cvt_int16x8_int8x16_saturating_unsigned" -> instr packuswb args
  | "caml_sse2_cvt_int32x4_int16x8_saturating_unsigned" -> instr packusdw args
  | "caml_sse2_int8x16_avg_unsigned" -> instr pavgb_X_Xm128 args
  | "caml_sse2_int16x8_avg_unsigned" -> instr pavgw_X_Xm128 args
  | "caml_sse2_int8x16_sad_unsigned" -> instr psadbw_X_Xm128 args
  | "caml_sse2_int16x8_sll" -> instr psllw_X_Xm128 args
  | "caml_sse2_int32x4_sll" -> instr pslld_X_Xm128 args
  | "caml_sse2_int64x2_sll" -> instr psllq_X_Xm128 args
  | "caml_sse2_int16x8_srl" -> instr psrlw_X_Xm128 args
  | "caml_sse2_int32x4_srl" -> instr psrld_X_Xm128 args
  | "caml_sse2_int64x2_srl" -> instr psrlq_X_Xm128 args
  | "caml_sse2_int16x8_sra" -> instr psraw_X_Xm128 args
  | "caml_sse2_int32x4_sra" -> instr psrad_X_Xm128 args
  | "caml_sse2_int16x8_slli" ->
    let i, args = extract_constant args ~max:15 op in
    instr psllw_X ~i args
  | "caml_sse2_int32x4_slli" ->
    let i, args = extract_constant args ~max:31 op in
    instr pslld_X ~i args
  | "caml_sse2_int64x2_slli" ->
    let i, args = extract_constant args ~max:63 op in
    instr psllq_X ~i args
  | "caml_sse2_int16x8_srli" ->
    let i, args = extract_constant args ~max:15 op in
    instr psrlw_X ~i args
  | "caml_sse2_int32x4_srli" ->
    let i, args = extract_constant args ~max:31 op in
    instr psrld_X ~i args
  | "caml_sse2_int64x2_srli" ->
    let i, args = extract_constant args ~max:63 op in
    instr psrlq_X ~i args
  | "caml_sse2_int16x8_srai" ->
    let i, args = extract_constant args ~max:15 op in
    instr psraw_X ~i args
  | "caml_sse2_int32x4_srai" ->
    let i, args = extract_constant args ~max:31 op in
    instr psrad_X ~i args
  | "caml_sse2_vec128_shuffle_64" ->
    let i, args = extract_constant args ~max:3 op in
    instr shufpd ~i args
  | "caml_sse2_vec128_shuffle_high_16" ->
    let i, args = extract_constant args ~max:255 op in
    instr pshufhw ~i args
  | "caml_sse2_vec128_shuffle_low_16" ->
    let i, args = extract_constant args ~max:255 op in
    instr pshuflw ~i args
  | "caml_sse2_vec128_interleave_high_8" -> instr punpckhbw args
  | "caml_sse2_vec128_interleave_low_8" -> instr punpcklbw args
  | "caml_sse2_vec128_interleave_high_16" -> instr punpckhwd args
  | "caml_sse2_vec128_interleave_low_16" -> instr punpcklwd args
  | "caml_simd_vec128_interleave_high_64"
  | "caml_sse2_vec128_interleave_high_64" ->
    instr punpckhqdq args
  | "caml_simd_vec128_interleave_low_64" | "caml_sse2_vec128_interleave_low_64"
    ->
    instr punpcklqdq args
  | "caml_sse2_int16x8_mul_high" -> instr pmulhw args
  | "caml_sse2_int16x8_mul_high_unsigned" -> instr pmulhuw_X_Xm128 args
  | "caml_sse2_int16x8_mul_low" -> instr pmullw args
  | "caml_sse2_int16x8_mul_hadd_int32x4" -> instr pmaddwd args
  | _ -> None

let select_operation_sse3 op args =
  if not (Arch.Extension.enabled SSE3)
  then None
  else
    match op with
    | "caml_sse3_float32x4_addsub" -> instr addsubps args
    | "caml_sse3_float64x2_addsub" -> instr addsubpd args
    | "caml_sse3_float32x4_hadd" -> instr haddps args
    | "caml_sse3_float64x2_hadd" -> instr haddpd args
    | "caml_sse3_float32x4_hsub" -> instr hsubps args
    | "caml_sse3_float64x2_hsub" -> instr hsubpd args
    | "caml_sse3_vec128_dup_low_64" -> instr movddup args
    | "caml_sse3_vec128_dup_odd_32" -> instr movshdup args
    | "caml_sse3_vec128_dup_even_32" -> instr movsldup args
    | _ -> None

let select_operation_ssse3 op args =
  if not (Arch.Extension.enabled SSSE3)
  then None
  else
    match op with
    | "caml_ssse3_int8x16_abs" -> instr pabsb_X_Xm128 args
    | "caml_ssse3_int16x8_abs" -> instr pabsw_X_Xm128 args
    | "caml_ssse3_int32x4_abs" -> instr pabsd_X_Xm128 args
    | "caml_ssse3_int16x8_hadd" -> instr phaddw_X_Xm128 args
    | "caml_ssse3_int32x4_hadd" -> instr phaddd_X_Xm128 args
    | "caml_ssse3_int16x8_hadd_saturating" -> instr phaddsw_X_Xm128 args
    | "caml_ssse3_int16x8_hsub" -> instr phsubw_X_Xm128 args
    | "caml_ssse3_int32x4_hsub" -> instr phsubd_X_Xm128 args
    | "caml_ssse3_int16x8_hsub_saturating" -> instr phsubsw_X_Xm128 args
    | "caml_ssse3_int8x16_mulsign" -> instr psignb_X_Xm128 args
    | "caml_ssse3_int16x8_mulsign" -> instr psignw_X_Xm128 args
    | "caml_ssse3_int32x4_mulsign" -> instr psignd_X_Xm128 args
    | "caml_ssse3_vec128_shuffle_8" -> instr pshufb_X_Xm128 args
    | "caml_ssse3_vec128_align_right_bytes" ->
      let i, args = extract_constant args ~max:31 op in
      instr palignr_X_Xm128 ~i args
    | "caml_ssse3_int8x16_mul_unsigned_hadd_saturating_int16x8" ->
      instr pmaddubsw_X_Xm128 args
    | _ -> None

let select_operation_sse41 op args =
  if not (Arch.Extension.enabled SSE4_1)
  then None
  else
    match op with
    | "caml_sse41_vec128_blend_16" ->
      let i, args = extract_constant args ~max:255 op in
      instr pblendw ~i args
    | "caml_sse41_vec128_blend_32" ->
      let i, args = extract_constant args ~max:15 op in
      instr blendps ~i args
    | "caml_sse41_vec128_blend_64" ->
      let i, args = extract_constant args ~max:3 op in
      instr blendpd ~i args
    | "caml_sse41_vec128_blendv_8" -> instr pblendvb args
    | "caml_sse41_vec128_blendv_32" -> instr blendvps args
    | "caml_sse41_vec128_blendv_64" -> instr blendvpd args
    | "caml_sse41_int64x2_cmpeq" -> instr pcmpeqq args
    | "caml_sse41_cvtsx_int8x16_int16x8" -> instr pmovsxbw args
    | "caml_sse41_cvtsx_int8x16_int32x4" -> instr pmovsxbd args
    | "caml_sse41_cvtsx_int8x16_int64x2" -> instr pmovsxbq args
    | "caml_sse41_cvtsx_int16x8_int32x4" -> instr pmovsxwd args
    | "caml_sse41_cvtsx_int16x8_int64x2" -> instr pmovsxwq args
    | "caml_sse41_cvtsx_int32x4_int64x2" -> instr pmovsxdq args
    | "caml_sse41_cvtzx_int8x16_int16x8" -> instr pmovzxbw args
    | "caml_sse41_cvtzx_int8x16_int32x4" -> instr pmovzxbd args
    | "caml_sse41_cvtzx_int8x16_int64x2" -> instr pmovzxbq args
    | "caml_sse41_cvtzx_int16x8_int32x4" -> instr pmovzxwd args
    | "caml_sse41_cvtzx_int16x8_int64x2" -> instr pmovzxwq args
    | "caml_sse41_cvtzx_int32x4_int64x2" -> instr pmovzxdq args
    | "caml_sse41_float32x4_dp" ->
      let i, args = extract_constant args ~max:255 op in
      instr dpps ~i args
    | "caml_sse41_float64x2_dp" ->
      let i, args = extract_constant args ~max:255 op in
      instr dppd ~i args
    | "caml_sse41_int8x16_extract" ->
      let i, args = extract_constant args ~max:15 op in
      instr pextrb ~i args
    | "caml_sse41_int16x8_extract" ->
      let i, args = extract_constant args ~max:7 op in
      instr pextrw_r64m16_X ~i args
    | "caml_sse41_int32x4_extract" ->
      let i, args = extract_constant args ~max:3 op in
      instr pextrd ~i args
    | "caml_sse41_int64x2_extract" ->
      let i, args = extract_constant args ~max:1 op in
      instr pextrq ~i args
    | "caml_sse41_int8x16_insert" ->
      let i, args = extract_constant args ~max:15 op in
      instr pinsrb ~i args
    | "caml_sse41_int16x8_insert" ->
      let i, args = extract_constant args ~max:7 op in
      instr pinsrw_X_r32m16 ~i args
    | "caml_sse41_int32x4_insert" ->
      let i, args = extract_constant args ~max:3 op in
      instr pinsrd ~i args
    | "caml_sse41_int64x2_insert" ->
      let i, args = extract_constant args ~max:1 op in
      instr pinsrq ~i args
    | "caml_sse41_float32x4_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      instr roundps ~i args
    | "caml_sse41_float64x2_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      instr roundpd ~i args
    | "caml_sse41_float64_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      instr roundsd ~i args
    | "caml_sse41_float32_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      instr roundss ~i args
    | "caml_simd_float32_round_current" ->
      instr roundss ~i:(int_of_float_rounding RoundCurrent) args
    | "caml_simd_float32_round_neg_inf" ->
      instr roundss ~i:(int_of_float_rounding RoundDown) args
    | "caml_simd_float32_round_pos_inf" ->
      instr roundss ~i:(int_of_float_rounding RoundUp) args
    | "caml_simd_float32_round_towards_zero" ->
      instr roundss ~i:(int_of_float_rounding RoundTruncate) args
    | "caml_sse41_int8x16_max" -> instr pmaxsb args
    | "caml_sse41_int32x4_max" -> instr pmaxsd args
    | "caml_sse41_int16x8_max_unsigned" -> instr pmaxuw args
    | "caml_sse41_int32x4_max_unsigned" -> instr pmaxud args
    | "caml_sse41_int8x16_min" -> instr pminsb args
    | "caml_sse41_int32x4_min" -> instr pminsd args
    | "caml_sse41_int16x8_min_unsigned" -> instr pminuw args
    | "caml_sse41_int32x4_min_unsigned" -> instr pminud args
    | "caml_sse41_int8x16_multi_sad_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr mpsadbw ~i args
    | "caml_sse41_int16x8_minpos_unsigned" -> instr phminposuw args
    | "caml_sse41_int32x4_mul_low" -> instr pmulld args
    | _ -> None

let select_operation_sse42 op args =
  if not (Arch.Extension.enabled SSE4_2)
  then None
  else
    match op with
    | "caml_sse42_int64x2_cmpgt" -> instr pcmpgtq args
    | "caml_sse42_int64_crc" | "caml_sse42_int_untagged_crc" ->
      instr crc32_r64_r64m64 args
    | "caml_sse42_vec128_cmpestrm" ->
      let i, args = extract_constant args ~max:127 op in
      instr pcmpestrm ~i args
    | "caml_sse42_vec128_cmpestra" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpestra ~i args
    | "caml_sse42_vec128_cmpestrc" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpestrc ~i args
    | "caml_sse42_vec128_cmpestri" ->
      let i, args = extract_constant args ~max:127 op in
      instr pcmpestri ~i args
    | "caml_sse42_vec128_cmpestro" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpestro ~i args
    | "caml_sse42_vec128_cmpestrs" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpestrs ~i args
    | "caml_sse42_vec128_cmpestrz" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpestrz ~i args
    | "caml_sse42_vec128_cmpistrm" ->
      let i, args = extract_constant args ~max:127 op in
      instr pcmpistrm ~i args
    | "caml_sse42_vec128_cmpistra" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpistra ~i args
    | "caml_sse42_vec128_cmpistrc" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpistrc ~i args
    | "caml_sse42_vec128_cmpistri" ->
      let i, args = extract_constant args ~max:127 op in
      instr pcmpistri ~i args
    | "caml_sse42_vec128_cmpistro" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpistro ~i args
    | "caml_sse42_vec128_cmpistrs" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpistrs ~i args
    | "caml_sse42_vec128_cmpistrz" ->
      let i, args = extract_constant args ~max:127 op in
      seq Seq.pcmpistrz ~i args
    | _ -> None

let select_simd_instr op args =
  let or_else try_ opt =
    match opt with Some x -> Some x | None -> try_ op args
  in
  None
  |> or_else select_operation_clmul
  |> or_else select_operation_bmi2
  |> or_else select_operation_sse
  |> or_else select_operation_sse2
  |> or_else select_operation_sse3
  |> or_else select_operation_ssse3
  |> or_else select_operation_sse41
  |> or_else select_operation_sse42

let select_operation_cfg op args =
  select_simd_instr op args
  |> Option.map (fun (op, args) -> Operation.Specific (Isimd op), args)

let pseudoregs_for_mem_operation (op : Simd.Mem.operation) arg res =
  match op with
  | SSE2 Add_f64
  | SSE2 Sub_f64
  | SSE2 Mul_f64
  | SSE2 Div_f64
  | SSE Add_f32
  | SSE Sub_f32
  | SSE Mul_f32
  | SSE Div_f32 ->
    [| res.(0); arg.(1) |], res

let pseudoregs_for_instr (simd : Simd.instr) arg_regs res_regs =
  let rax = Proc.phys_reg Int 0 in
  let rcx = Proc.phys_reg Int 5 in
  let rdx = Proc.phys_reg Int 4 in
  let xmm0v () = Proc.phys_reg Vec128 100 in
  Array.iteri
    (fun i (arg : Simd.arg) ->
      match Simd.loc_is_pinned arg.loc with
      | Some RAX -> arg_regs.(i) <- rax
      | Some RCX -> arg_regs.(i) <- rcx
      | Some RDX -> arg_regs.(i) <- rdx
      | Some XMM0 -> arg_regs.(i) <- xmm0v ()
      | None -> ())
    simd.args;
  (match simd.res with
  | First_arg ->
    assert (not (Reg.is_preassigned arg_regs.(0)));
    arg_regs.(0) <- res_regs.(0)
  | Res { loc; _ } -> (
    match Simd.loc_is_pinned loc with
    | Some RAX -> res_regs.(0) <- rax
    | Some RCX -> res_regs.(0) <- rcx
    | Some RDX -> res_regs.(0) <- rdx
    | Some XMM0 -> res_regs.(0) <- xmm0v ()
    | None -> ()));
  arg_regs, res_regs

let pseudoregs_for_operation (simd : Simd.operation) arg res =
  let arg_regs = Array.copy arg in
  let res_regs = Array.copy res in
  match simd with
  | Instruction { instr; _ } -> pseudoregs_for_instr instr arg_regs res_regs
  | Sequence { seq; _ } -> (
    match seq.id with
    | Sqrtss | Sqrtsd | Roundss | Roundsd | Pcmpestra | Pcmpestrc | Pcmpestro
    | Pcmpestrs | Pcmpestrz | Pcmpistra | Pcmpistrc | Pcmpistro | Pcmpistrs
    | Pcmpistrz ->
      pseudoregs_for_instr seq.instr arg_regs res_regs)

(* Error report *)

let report_error ppf = function
  | Bad_immediate msg -> Format.pp_print_string ppf msg

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

(* Vectorize operations *)

let vector_width_in_bits = 128

(* CR-soon gyorsh: [vectorize_operation] is too long, refactor / split up. *)
let vectorize_operation (width_type : Vectorize_utils.Width_in_bits.t)
    ~arg_count ~res_count ~alignment_in_bytes (cfg_ops : Operation.t list) :
    Vectorize_utils.Vectorized_instruction.t list option =
  (* Assumes cfg_ops are isomorphic *)
  let instr instr =
    Operation.Specific (Isimd (Instruction { instr; imm = None }))
  in
  let width_in_bits = Vectorize_utils.Width_in_bits.to_int width_type in
  let length = List.length cfg_ops in
  assert (length * width_in_bits = vector_width_in_bits);
  let vector_width_in_bytes = vector_width_in_bits / 8 in
  let is_aligned_to_vector_width () =
    match alignment_in_bytes with
    | None -> Misc.fatal_error "Unexpected memory operation"
    | Some alignment_in_bytes ->
      alignment_in_bytes mod vector_width_in_bytes = 0
      && alignment_in_bytes / vector_width_in_bytes > 1
  in
  let vec128_chunk () : Cmm.memory_chunk =
    if is_aligned_to_vector_width ()
    then Onetwentyeight_aligned
    else Onetwentyeight_unaligned
  in
  let same_width memory_chunk =
    Vectorize_utils.Width_in_bits.equal width_type
      (Vectorize_utils.Width_in_bits.of_memory_chunk memory_chunk)
  in
  let make_default ~arg_count ~res_count operation :
      Vectorize_utils.Vectorized_instruction.t list option =
    Some
      [ Vectorize_utils.Vectorized_instruction.make_default ~arg_count
          ~res_count operation ]
  in
  let create_const_vec consts =
    let lows, highs = Misc.Stdlib.List.split_at (length / 2) consts in
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
    Operation.Const_vec128 { high = pack_int64 highs; low = pack_int64 lows }
    |> make_default ~arg_count:0 ~res_count:1
  in
  let add_op =
    let op =
      match width_type with
      | W128 -> assert false
      | W64 -> paddq
      | W32 -> paddd
      | W16 -> paddw
      | W8 -> paddb
    in
    Some (instr op)
  in
  let mul_op =
    match width_type with
    | W128 -> None
    | W64 -> None
    | W32 -> Some (instr pmulld)
    | W16 -> Some (instr pmullw)
    | W8 -> None
  in
  let vectorize_intop (intop : Operation.integer_operation) =
    match intop with
    | Iadd -> Option.bind add_op (make_default ~arg_count ~res_count)
    | Isub ->
      (match width_type with
      | W128 -> assert false
      | W64 -> psubq_X_Xm128
      | W32 -> psubd
      | W16 -> psubw
      | W8 -> psubb)
      |> instr
      |> make_default ~arg_count ~res_count
    | Imul -> Option.bind mul_op (make_default ~arg_count ~res_count)
    | Imulh { signed } -> (
      match width_type with
      | W128 -> None
      | W64 -> None
      | W32 -> None
      | W16 ->
        if signed
        then instr pmulhw |> make_default ~arg_count ~res_count
        else instr pmulhuw_X_Xm128 |> make_default ~arg_count ~res_count
      | W8 -> None)
    | Iand -> instr pand |> make_default ~arg_count ~res_count
    | Ior -> instr por |> make_default ~arg_count ~res_count
    | Ixor -> instr pxor |> make_default ~arg_count ~res_count
    | Ilsl ->
      let op =
        match width_type with
        | W128 -> assert false
        | W64 -> psllq_X_Xm128
        | W32 -> pslld_X_Xm128
        | W16 -> psllw_X_Xm128
        | W8 -> assert false
      in
      instr op |> make_default ~arg_count ~res_count
    | Ilsr ->
      let op =
        match width_type with
        | W128 -> assert false
        | W64 -> psrlq_X_Xm128
        | W32 -> psrld_X_Xm128
        | W16 -> psrlw_X_Xm128
        | W8 -> assert false
      in
      instr op |> make_default ~arg_count ~res_count
    | Iasr ->
      let op =
        match width_type with
        | W128 -> assert false
        | W64 -> None
        | W32 -> Some psrad_X_Xm128
        | W16 -> Some psraw_X_Xm128
        | W8 -> None
      in
      Option.bind op (fun op -> instr op |> make_default ~arg_count ~res_count)
    | Icomp (Isigned intcomp) -> (
      match intcomp with
      | Ceq ->
        let op =
          match width_type with
          | W128 -> assert false
          | W64 -> pcmpeqq
          | W32 -> pcmpeqd
          | W16 -> pcmpeqw
          | W8 -> pcmpeqb
        in
        instr op |> make_default ~arg_count ~res_count
      | Cgt ->
        let op =
          match width_type with
          | W128 -> assert false
          | W64 -> pcmpgtq
          | W32 -> pcmpgtd
          | W16 -> pcmpgtw
          | W8 -> pcmpgtb
        in
        instr op |> make_default ~arg_count ~res_count
      | Cne | Clt | Cle | Cge ->
        None
        (* These instructions seem to not have a simd counterpart yet, could
           also implement as a combination of other instructions if needed in
           the future *))
    | Idiv | Imod | Iclz _ | Ictz _ | Ipopcnt | Icomp (Iunsigned _) -> None
  in
  match List.hd cfg_ops with
  | Move -> Operation.Move |> make_default ~arg_count ~res_count
  | Const_int _ ->
    let extract_const_int (op : Operation.t) =
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
  | Load { memory_chunk; addressing_mode; mutability; is_atomic } ->
    if not (same_width memory_chunk)
    then None
    else
      let num_args_addressing = Arch.num_args_addressing addressing_mode in
      assert (arg_count = num_args_addressing && res_count = 1);
      let operation =
        Operation.Load
          { memory_chunk = vec128_chunk ();
            addressing_mode;
            mutability;
            is_atomic
          }
      in
      Some
        [ { operation;
            arguments =
              Array.init num_args_addressing (fun i ->
                  Vectorize_utils.Vectorized_instruction.Original i);
            results = [| Result 0 |]
          } ]
  | Store (memory_chunk, addressing_mode, is_assignment) ->
    if not (same_width memory_chunk)
    then None
    else
      let num_args_addressing = Arch.num_args_addressing addressing_mode in
      assert (arg_count = num_args_addressing + 1 && res_count = 0);
      let operation =
        Operation.Store (vec128_chunk (), addressing_mode, is_assignment)
      in
      Some
        [ { operation;
            arguments =
              Array.append
                [| Vectorize_utils.Vectorized_instruction.Argument 0 |]
                (Array.init num_args_addressing (fun i ->
                     Vectorize_utils.Vectorized_instruction.Original (i + 1)));
            results = [||]
          } ]
  | Intop intop -> vectorize_intop intop
  | Intop_imm (intop, _) -> (
    let extract_intop_imm_int (op : Operation.t) =
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
        const_instruction.results.(0)
          <- Vectorize_utils.Vectorized_instruction.New_Vec128 0;
        intop_instruction.arguments.(1)
          <- Vectorize_utils.Vectorized_instruction.New_Vec128 0;
        Some [const_instruction; intop_instruction])
      else None
    | _ -> None)
  | Specific op -> (
    match op with
    | Ilea addressing_mode -> (
      let extract_scale_displ (op : Operation.t) =
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
          | Imfence | Ipause | Ipackf32 | Isimd _ | Isimd_mem _ | Iprefetch _
          | Icldemote _ ->
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
        { Vectorize_utils.Vectorized_instruction.operation = Move;
          arguments = [| arg |];
          results = [| res |]
        }
      in
      let make_binary_operation arg_0 arg_1 res operation =
        { Vectorize_utils.Vectorized_instruction.operation;
          arguments = [| arg_0; arg_1 |];
          results = [| res |]
        }
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
              make_const (New_Vec128 0) displs;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) add ]
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
              make_const (New_Vec128 0) displs;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) add ]
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
              make_const (New_Vec128 0) scales;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) mul;
              make_const (New_Vec128 1) displs;
              make_binary_operation (Result 0) (New_Vec128 1) (Result 0) add ]
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
              make_const (New_Vec128 0) scales;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) mul;
              make_binary_operation (Result 0) (Argument 0) (Result 0) add;
              make_const (New_Vec128 1) displs;
              make_binary_operation (Result 0) (New_Vec128 1) (Result 0) add ]
        | _ -> None)
      | Ibased _ -> None)
    | Isextend32 -> (
      match width_type with
      | W128 -> None
      | W64 -> instr pmovsxdq |> make_default ~arg_count ~res_count
      | W32 ->
        None
        (* If the upper bits of the original register containing the smaller
           register is determined to be unused without relying on this file,
           these can also be vectorized to be a move *)
      | W16 -> None
      | W8 -> None)
    | Izextend32 -> (
      match width_type with
      | W128 -> None
      | W64 -> instr pmovzxdq |> make_default ~arg_count ~res_count
      | W32 -> None (* See previous comment *)
      | W16 -> None
      | W8 -> None)
    | Istore_int (_n, addressing_mode, is_assignment) -> (
      if not (Vectorize_utils.Width_in_bits.equal width_type W64)
      then None
      else
        let extract_store_int_imm (op : Operation.t) =
          match op with
          | Specific (Istore_int (n, _addr, _is_assign)) -> Int64.of_nativeint n
          | Specific
              ( Ifloatarithmem _ | Ioffset_loc _ | Iprefetch _ | Icldemote _
              | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence | Ipause
              | Ipackf32 | Isimd _ | Isimd_mem _ | Ilea _ | Ibswap _
              | Isextend32 | Izextend32 )
          | Intop_imm _ | Move | Load _ | Store _ | Intop _ | Alloc _
          | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_int _
          | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
          | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _
          | Probe_is_enabled _ | Opaque | Begin_region | End_region
          | Name_for_debugger _ | Dls_get | Poll ->
            assert false
        in
        let consts = List.map extract_store_int_imm cfg_ops in
        match create_const_vec consts with
        | None -> None
        | Some [const_instruction] ->
          let num_args_addressing = Arch.num_args_addressing addressing_mode in
          assert (arg_count = num_args_addressing);
          assert (res_count = 0);
          assert (Array.length const_instruction.results = 1);
          let new_reg = Vectorize_utils.Vectorized_instruction.New_Vec128 0 in
          const_instruction.results.(0) <- new_reg;
          let address_args =
            Array.init num_args_addressing (fun i ->
                Vectorize_utils.Vectorized_instruction.Original i)
          in
          let store_operation =
            Operation.Store
              (Onetwentyeight_unaligned, addressing_mode, is_assignment)
          in
          let store_instruction : Vectorize_utils.Vectorized_instruction.t =
            { operation = store_operation;
              arguments = Array.append [| new_reg |] address_args;
              results = [||]
            }
          in
          Some [const_instruction; store_instruction]
        | Some _ -> None)
    | Ifloatarithmem (float_width, float_op, addressing_mode) ->
      let float_width_in_bits : Vectorize_utils.Width_in_bits.t =
        match float_width with Float64 -> W64 | Float32 -> W32
      in
      assert (Vectorize_utils.Width_in_bits.equal float_width_in_bits width_type);
      let num_args_addressing = Arch.num_args_addressing addressing_mode in
      assert (arg_count = 1 + num_args_addressing);
      assert (res_count = 1);
      let results = [| Vectorize_utils.Vectorized_instruction.Result 0 |] in
      let address_args =
        Array.init num_args_addressing (fun i ->
            Vectorize_utils.Vectorized_instruction.Original (i + 1))
      in
      if is_aligned_to_vector_width ()
      then
        let sse_op : Simd.Mem.operation =
          match float_width, float_op with
          | Float64, Ifloatadd -> SSE2 Add_f64
          | Float64, Ifloatsub -> SSE2 Sub_f64
          | Float64, Ifloatmul -> SSE2 Mul_f64
          | Float64, Ifloatdiv -> SSE2 Div_f64
          | Float32, Ifloatadd -> SSE Add_f32
          | Float32, Ifloatsub -> SSE Sub_f32
          | Float32, Ifloatmul -> SSE Mul_f32
          | Float32, Ifloatdiv -> SSE Div_f32
        in
        Some
          [ { operation =
                Operation.Specific (Isimd_mem (sse_op, addressing_mode));
              arguments = Array.append results address_args;
              results
            } ]
      else
        (* Emit a load followed by an arithmetic operation, effectively
           reverting the decision from Arch.selection. It will probably not be
           beneficial with 128-bit accesses. *)
        let op =
          match float_width, float_op with
          | Float64, Ifloatadd -> addpd
          | Float64, Ifloatsub -> subpd
          | Float64, Ifloatmul -> mulpd
          | Float64, Ifloatdiv -> divpd
          | Float32, Ifloatadd -> addps
          | Float32, Ifloatsub -> subps
          | Float32, Ifloatmul -> mulps
          | Float32, Ifloatdiv -> divps
        in
        let new_reg =
          [| Vectorize_utils.Vectorized_instruction.New_Vec128 0 |]
        in
        let load : Vectorize_utils.Vectorized_instruction.t =
          { operation =
              Operation.Load
                { memory_chunk = vec128_chunk ();
                  addressing_mode;
                  mutability = Mutable;
                  is_atomic = false
                };
            arguments = address_args;
            results = new_reg
          }
        in
        let arith : Vectorize_utils.Vectorized_instruction.t =
          { operation = instr op;
            arguments = Array.append results new_reg;
            results
          }
        in
        Some [load; arith]
    | Isimd_mem _ ->
      Misc.fatal_error "Unexpected simd operation with memory arguments"
    | Ioffset_loc _ | Ibswap _ | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence
    | Ipause | Ipackf32 | Isimd _ | Iprefetch _ | Icldemote _ ->
      None)
  | Alloc _ | Reinterpret_cast _ | Static_cast _ | Spill | Reload
  | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
  | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
  | Opaque | Begin_region | End_region | Name_for_debugger _ | Dls_get | Poll ->
    None
