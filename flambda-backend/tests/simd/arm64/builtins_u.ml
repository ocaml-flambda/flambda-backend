(* !!!

Should be kept in sync with builtins.ml.
CR-someday mslater: with layout polymorphism, the tests could be functorized.

!!! *)

type nonrec int8x16 = int8x16#
type nonrec int16x8 = int16x8#
type nonrec int32x4 = int32x4#
type nonrec int64x2 = int64x2#
type nonrec float32x4 = float32x4#
type nonrec float64x2 = float64x2#

module Float64 = struct
  type t = float

  external max : t -> t -> t = "" "caml_sse2_float64_max"
    [@@noalloc] [@@builtin] [@@unboxed]

  external min : t -> t -> t = "" "caml_sse2_float64_min"
    [@@noalloc] [@@builtin] [@@unboxed]

  external sqrt : t -> t = "" "caml_sse2_float64_sqrt"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "" "caml_sse41_float64_round"
    [@@noalloc] [@@builtin]
end

module Float32x4 = struct
  type t = float32x4

  external cmp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_sse_float32x4_cmp"
    [@@noalloc] [@@builtin]

  external movemask_32 : (int32x4[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse_vec128_movemask_32"
    [@@noalloc] [@@builtin]

  external add : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_float32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_float32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_float32x4_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_float32x4_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_float32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_float32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external rcp : t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_rcp"
    [@@noalloc] [@@unboxed] [@@builtin]

  external rsqrt : t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_rsqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse2_cvt_float32x4_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float64x2 : t -> float64x2
    = "caml_vec128_unreachable" "caml_sse2_cvt_float32x4_float64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_addsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float32x4_dp"
    [@@noalloc] [@@builtin]

  external round : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float32x4_round"
    [@@noalloc] [@@builtin]
end

module Float64x2 = struct
  type t = float64x2

  (* Math *)

  external cmp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_float64x2_cmp"
    [@@noalloc] [@@builtin]

  external movemask_64 : (int64x2[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_64"
    [@@noalloc] [@@builtin]

  external add : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse2_cvt_float64x2_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float32x4 : t -> float32x4
    = "caml_vec128_unreachable" "caml_sse2_cvt_float64x2_float32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_addsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float64x2_dp"
    [@@noalloc] [@@builtin]

  external round : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float64x2_round"
    [@@noalloc] [@@builtin]
end

module Int64x2 = struct
  type t = int64x2

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int64x2_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse42_int64x2_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_sll"
    [@@noalloc] [@@unboxed] [@@builtin]

  external srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_srl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int64x2_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int64x2_srli"
    [@@noalloc] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int64x2_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int64x2_insert"
    [@@noalloc] [@@builtin]
end

module Int32x4 = struct
  type t = int32x4

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int32x4_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int32x4_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sll"
    [@@noalloc] [@@unboxed] [@@builtin]

  external srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_srl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sra : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sra"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int32x4_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int32x4_srli"
    [@@noalloc] [@@builtin]

  external srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int32x4_srai"
    [@@noalloc] [@@builtin]

  external cvt_f64 : t -> float64x2
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_float64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_f32 : t -> float32x4
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_float32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int32x4_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mulsign : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_mulsign"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int32x4_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int32x4_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_si16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_int16x8_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_su16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_int16x8_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_mul_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int32x4_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int32x4_insert"
    [@@noalloc] [@@builtin]
end

module Int16x8 = struct
  type t = int16x8

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_add_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_add_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_sub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_sub_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int16x8_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int16x8_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_si8 : t -> t -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_cvt_int16x8_int8x16_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_su8 : t -> t -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_cvt_int16x8_int8x16_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int16x8_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int16x8_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int16x8_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int16x8_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hadd_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mulsign : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_mulsign"
    [@@noalloc] [@@unboxed] [@@builtin]

  external avgu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_avg_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minposu : t -> t
    = "caml_vec128_unreachable" "caml_sse41_int16x8_minpos_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_high : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_high"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_high_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_high_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_hadd_i32 : t -> t -> int32x4
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_hadd_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse41_int16x8_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int16x8_insert"
    [@@noalloc] [@@builtin]

  external sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sll"
    [@@noalloc] [@@unboxed] [@@builtin]

  external srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_srl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sra : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sra"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int16x8_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int16x8_srli"
    [@@noalloc] [@@builtin]

  external srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int16x8_srai"
    [@@noalloc] [@@builtin]
end

module Int8x16 = struct
  type t = int8x16

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_add_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_add_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_sub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_sub_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int8x16_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int8x16_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int8x16_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mulsign : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int8x16_mulsign"
    [@@noalloc] [@@unboxed] [@@builtin]

  external avgu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_avg_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sadu : t -> t -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_int8x16_sad_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external msadu :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int8x16_multi_sad_unsigned"
    [@@noalloc] [@@builtin]

  external mul_unsigned_hadd_saturating_i16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_ssse3_int8x16_mul_unsigned_hadd_saturating_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse41_int8x16_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int8x16_insert"
    [@@noalloc] [@@builtin]
end

module SSE_Util = struct
  type t = int32x4

  external high_64_to_low_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_high_64_to_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_64_to_high_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_low_64_to_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_interleave_high_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_interleave_low_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external shuffle_32 :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse_vec128_shuffle_32"
    [@@noalloc] [@@builtin]

  external movemask_32 : (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse_vec128_movemask_32"
    [@@noalloc] [@@builtin]
end

module SSE2_Util = struct
  external _and : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external andnot : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_andnot"
    [@@noalloc] [@@unboxed] [@@builtin]

  external _or : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external xor : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  external movemask_8 : (int8x16[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_8"
    [@@noalloc] [@@builtin]

  external movemask_64 : (int64x2[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_64"
    [@@noalloc] [@@builtin]

  external shift_left_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shift_left_bytes"
    [@@noalloc] [@@builtin]

  external shift_right_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shift_right_bytes"
    [@@noalloc] [@@builtin]

  external shuffle_64 :
    (int[@untagged]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_64"
    [@@noalloc] [@@builtin]

  external shuffle_high_16 :
    (int[@untagged]) -> (int16x8[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_high_16"
    [@@noalloc] [@@builtin]

  external shuffle_low_16 :
    (int[@untagged]) -> (int16x8[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_low_16"
    [@@noalloc] [@@builtin]

  external interleave_high_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_16"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_16"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module SSE3_Util = struct
  external dup_low_64 : int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse3_vec128_dup_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_odd_32 : int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_sse3_vec128_dup_odd_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_even_32 : int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_sse3_vec128_dup_even_32"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module SSSE3_Util = struct
  external shuffle_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_ssse3_vec128_shuffle_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external align_right_bytes :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_ssse3_vec128_align_right_bytes"
    [@@noalloc] [@@builtin]
end

module SSE41_Util = struct
  external blend_16 :
    (int[@untagged]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_16"
    [@@noalloc] [@@builtin]

  external blend_32 :
    (int[@untagged]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_32"
    [@@noalloc] [@@builtin]

  external blend_64 :
    (int[@untagged]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_64"
    [@@noalloc] [@@builtin]

  external blendv_8 : int8x16 -> int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external blendv_32 : int32x4 -> int32x4 -> int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external blendv_64 : int64x2 -> int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_64"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module SSE42_String = struct
  (* These also work with int16x8s, given the 16-bit char encoding immediate
     bit *)

  external cmpestrm :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int8x16[@unboxed]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrm"
    [@@noalloc] [@@builtin]

  external cmpestra :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestra"
    [@@noalloc] [@@builtin]

  external cmpestrc :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrc"
    [@@noalloc] [@@builtin]

  external cmpestri :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestri"
    [@@noalloc] [@@builtin]

  external cmpestro :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestro"
    [@@noalloc] [@@builtin]

  external cmpestrs :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrs"
    [@@noalloc] [@@builtin]

  external cmpestrz :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) ->
    (int[@untagged]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrz"
    [@@noalloc] [@@builtin]

  external cmpistrm :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrm"
    [@@noalloc] [@@builtin]

  external cmpistra :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistra"
    [@@noalloc] [@@builtin]

  external cmpistrc :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrc"
    [@@noalloc] [@@builtin]

  external cmpistri :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistri"
    [@@noalloc] [@@builtin]

  external cmpistro :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistro"
    [@@noalloc] [@@builtin]

  external cmpistrs :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrs"
    [@@noalloc] [@@builtin]

  external cmpistrz :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int[@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrz"
    [@@noalloc] [@@builtin]
end
