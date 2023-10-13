
#include <caml/memory.h>
#include <caml/simd.h>
#include <smmintrin.h>
#include <assert.h>

int64_t vec128_low_int64(__m128i v)
{
  return _mm_extract_epi64(v, 0);
}

int64_t vec128_high_int64(__m128i v)
{
  return _mm_extract_epi64(v, 1);
}

__m128i vec128_of_int64s(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}

CAMLprim value boxed_combine(value v0, value v1)
{
  CAMLparam2(v0, v1);

  __m128i l = Vec128_vali(v0);
  __m128i r = Vec128_vali(v1);
  __m128i result = _mm_add_epi64(l, r);

  CAMLreturn(caml_copy_vec128i(result));
}

__m128i lots_of_vectors(
  __m128i v0, __m128i v1, __m128i v2, __m128i v3,
  __m128i v4, __m128i v5, __m128i v6, __m128i v7,
  __m128i v8, __m128i v9, __m128i v10, __m128i v11,
  __m128i v12, __m128i v13, __m128i v14, __m128i v15)
{
  __m128i x0 = _mm_add_epi64(v0, v1);
  __m128i x1 = _mm_add_epi64(v2, v3);
  __m128i x2 = _mm_add_epi64(v4, v5);
  __m128i x3 = _mm_add_epi64(v6, v7);
  __m128i x4 = _mm_add_epi64(v8, v9);
  __m128i x5 = _mm_add_epi64(v10, v11);
  __m128i x6 = _mm_add_epi64(v12, v13);
  __m128i x7 = _mm_add_epi64(v14, v15);
  __m128i y0 = _mm_add_epi64(x0, x1);
  __m128i y1 = _mm_add_epi64(x2, x3);
  __m128i y2 = _mm_add_epi64(x4, x5);
  __m128i y3 = _mm_add_epi64(x6, x7);
  __m128i z0 = _mm_add_epi64(y0, y1);
  __m128i z1 = _mm_add_epi64(y2, y3);
  return _mm_add_epi64(z0, z1);
}

__m128i vectors_and_floats(
  __m128i v0, double f0, __m128i v1, double f1,
  __m128i v2, double f2, __m128i v3, double f3,
  double f4, __m128i v4, __m128i v5, double f5,
  double f6, __m128i v6, __m128i v7, double f7,
  double f8, double f9, __m128i v8, __m128i v9,
  __m128i v10, double f10, double f11, double f12)
{
  __m128i x0 = _mm_add_epi64(v0, v1);
  __m128i x1 = _mm_add_epi64(v2, v3);
  __m128i x2 = _mm_add_epi64(v4, v5);
  __m128i x3 = _mm_add_epi64(v6, v7);
  __m128i x4 = _mm_add_epi64(v8, v9);
  __m128i y0 = _mm_add_epi64(x0, x1);
  __m128i y1 = _mm_add_epi64(x2, x3);
  __m128i y2 = _mm_add_epi64(v10, x4);
  __m128i z0 = _mm_add_epi64(y0, y1);
  __m128i z = _mm_add_epi64(z0, y2);
  double f = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10 + f11 + f12;
  return vec128_of_int64s((int64_t)f, vec128_low_int64(z) + vec128_high_int64(z));
}

__m128i vectors_and_floats_and_ints(
  __m128i v0, double f0, __m128i v1, int64_t i0,
  __m128i v2, double f1, __m128i v3, int64_t i1,
  int64_t i2, __m128i v4, __m128i v5, double f2,
  double f3, __m128i v6, __m128i v7, int64_t i3,
  int64_t i4, double f4, __m128i v8, __m128i v9,
  __m128i v10, int64_t i5, int64_t i6, double f5)
{
  __m128i x0 = _mm_add_epi64(v0, v1);
  __m128i x1 = _mm_add_epi64(v2, v3);
  __m128i x2 = _mm_add_epi64(v4, v5);
  __m128i x3 = _mm_add_epi64(v6, v7);
  __m128i x4 = _mm_add_epi64(v8, v9);
  __m128i y0 = _mm_add_epi64(x0, x1);
  __m128i y1 = _mm_add_epi64(x2, x3);
  __m128i y2 = _mm_add_epi64(v10, x4);
  __m128i z0 = _mm_add_epi64(y0, y1);
  __m128i z = _mm_add_epi64(z0, y2);
  double f = f0 + f1 + f2 + f3 + f4 + f5;
  int64_t i = i0 + i1 + i2 + i3 + i4 + i5 + i6;
  return vec128_of_int64s((int64_t)f + i, vec128_low_int64(z) + vec128_high_int64(z));
}

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_vec128_unreachable);

BUILTIN(caml_sse2_float64_add);
BUILTIN(caml_sse2_float64_sub);
BUILTIN(caml_sse2_float64_mul);
BUILTIN(caml_sse2_float64_div);
BUILTIN(caml_sse2_float64_max);
BUILTIN(caml_sse2_float64_min);
BUILTIN(caml_sse2_float64_sqrt);
BUILTIN(caml_sse41_float64_round);

BUILTIN(caml_vec128_cast);

BUILTIN(caml_float32x4_low_of_float);
BUILTIN(caml_float32x4_low_to_float);
BUILTIN(caml_float32x4_const1);
BUILTIN(caml_float32x4_const4);

BUILTIN(caml_float64x2_low_of_float);
BUILTIN(caml_float64x2_low_to_float);
BUILTIN(caml_float64x2_const1);
BUILTIN(caml_float64x2_const2);

BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_int64x2_const1);
BUILTIN(caml_int64x2_const2);

BUILTIN(caml_int32x4_low_of_int32);
BUILTIN(caml_int32x4_low_to_int32);
BUILTIN(caml_int32x4_const1);
BUILTIN(caml_int32x4_const4);

BUILTIN(caml_int16x8_low_of_int);
BUILTIN(caml_int16x8_low_to_int);
BUILTIN(caml_int16x8_const1);
BUILTIN(caml_int16x8_const8);

BUILTIN(caml_int8x16_low_of_int);
BUILTIN(caml_int8x16_low_to_int);
BUILTIN(caml_int8x16_const1);
BUILTIN(caml_int8x16_const16);

BUILTIN(caml_sse_float32x4_cmp);
BUILTIN(caml_sse_float32x4_add);
BUILTIN(caml_sse_float32x4_sub);
BUILTIN(caml_sse_float32x4_mul);
BUILTIN(caml_sse_float32x4_div);
BUILTIN(caml_sse_float32x4_max);
BUILTIN(caml_sse_float32x4_min);
BUILTIN(caml_sse_float32x4_rcp);
BUILTIN(caml_sse_float32x4_rsqrt);
BUILTIN(caml_sse_float32x4_sqrt);
BUILTIN(caml_sse_vec128_high_64_to_low_64);
BUILTIN(caml_sse_vec128_low_64_to_high_64);
BUILTIN(caml_sse_vec128_interleave_high_32);
BUILTIN(caml_sse_vec128_interleave_low_32);
BUILTIN(caml_sse_vec128_shuffle_32);
BUILTIN(caml_sse_vec128_movemask_32);

BUILTIN(caml_sse2_int8x16_add);
BUILTIN(caml_sse2_int16x8_add);
BUILTIN(caml_sse2_int32x4_add);
BUILTIN(caml_sse2_int64x2_add);
BUILTIN(caml_sse2_float64x2_add);
BUILTIN(caml_sse2_int8x16_add_saturating);
BUILTIN(caml_sse2_int16x8_add_saturating);
BUILTIN(caml_sse2_int8x16_add_saturating_unsigned);
BUILTIN(caml_sse2_int16x8_add_saturating_unsigned);
BUILTIN(caml_sse2_int8x16_sub);
BUILTIN(caml_sse2_int16x8_sub);
BUILTIN(caml_sse2_int32x4_sub);
BUILTIN(caml_sse2_int64x2_sub);
BUILTIN(caml_sse2_float64x2_sub);
BUILTIN(caml_sse2_int8x16_sub_saturating);
BUILTIN(caml_sse2_int16x8_sub_saturating);
BUILTIN(caml_sse2_int8x16_sub_saturating_unsigned);
BUILTIN(caml_sse2_int16x8_sub_saturating_unsigned);
BUILTIN(caml_sse2_int8x16_max_unsigned);
BUILTIN(caml_sse2_int16x8_max);
BUILTIN(caml_sse2_float64x2_max);
BUILTIN(caml_sse2_int8x16_min_unsigned);
BUILTIN(caml_sse2_int16x8_min);
BUILTIN(caml_sse2_float64x2_min);
BUILTIN(caml_sse2_float64x2_mul);
BUILTIN(caml_sse2_float64x2_div);
BUILTIN(caml_sse2_float64x2_sqrt);
BUILTIN(caml_sse2_vec128_and);
BUILTIN(caml_sse2_vec128_andnot);
BUILTIN(caml_sse2_vec128_or);
BUILTIN(caml_sse2_vec128_xor);
BUILTIN(caml_sse2_vec128_movemask_8);
BUILTIN(caml_sse2_vec128_movemask_64);
BUILTIN(caml_sse2_vec128_shift_left_bytes);
BUILTIN(caml_sse2_vec128_shift_right_bytes);
BUILTIN(caml_sse2_int8x16_cmpeq);
BUILTIN(caml_sse2_int16x8_cmpeq);
BUILTIN(caml_sse2_int32x4_cmpeq);
BUILTIN(caml_sse2_int8x16_cmpgt);
BUILTIN(caml_sse2_int16x8_cmpgt);
BUILTIN(caml_sse2_int32x4_cmpgt);
BUILTIN(caml_sse2_float64x2_cmp);
BUILTIN(caml_sse2_cvt_int32x4_float64x2);
BUILTIN(caml_sse2_cvt_int32x4_float32x4);
BUILTIN(caml_sse2_cvt_float64x2_int32x4);
BUILTIN(caml_sse2_cvt_float64x2_float32x4);
BUILTIN(caml_sse2_cvt_float32x4_int32x4);
BUILTIN(caml_sse2_cvt_float32x4_float64x2);
BUILTIN(caml_sse2_int16x8_sll);
BUILTIN(caml_sse2_int32x4_sll);
BUILTIN(caml_sse2_int64x2_sll);
BUILTIN(caml_sse2_int16x8_srl);
BUILTIN(caml_sse2_int32x4_srl);
BUILTIN(caml_sse2_int64x2_srl);
BUILTIN(caml_sse2_int16x8_sra);
BUILTIN(caml_sse2_int32x4_sra);
BUILTIN(caml_sse2_int16x8_slli);
BUILTIN(caml_sse2_int32x4_slli);
BUILTIN(caml_sse2_int64x2_slli);
BUILTIN(caml_sse2_int16x8_srli);
BUILTIN(caml_sse2_int32x4_srli);
BUILTIN(caml_sse2_int64x2_srli);
BUILTIN(caml_sse2_int16x8_srai);
BUILTIN(caml_sse2_int32x4_srai);
BUILTIN(caml_sse2_vec128_shuffle_64);
BUILTIN(caml_sse2_vec128_shuffle_high_16);
BUILTIN(caml_sse2_vec128_shuffle_low_16);
BUILTIN(caml_sse2_vec128_interleave_high_8);
BUILTIN(caml_sse2_vec128_interleave_low_8);
BUILTIN(caml_sse2_vec128_interleave_high_16);
BUILTIN(caml_sse2_vec128_interleave_low_16);
BUILTIN(caml_sse2_vec128_interleave_high_64);
BUILTIN(caml_sse2_vec128_interleave_low_64);
BUILTIN(caml_sse2_int8x16_avg_unsigned);
BUILTIN(caml_sse2_int16x8_avg_unsigned);
BUILTIN(caml_sse2_int8x16_sad_unsigned);
BUILTIN(caml_sse2_cvt_int16x8_int8x16_saturating);
BUILTIN(caml_sse2_cvt_int32x4_int16x8_saturating);
BUILTIN(caml_sse2_cvt_int16x8_int8x16_saturating_unsigned);
BUILTIN(caml_sse2_cvt_int32x4_int16x8_saturating_unsigned);

BUILTIN(caml_sse3_float32x4_addsub);
BUILTIN(caml_sse3_float64x2_addsub);
BUILTIN(caml_sse3_float32x4_hadd);
BUILTIN(caml_sse3_float64x2_hadd);
BUILTIN(caml_sse3_float32x4_hsub);
BUILTIN(caml_sse3_float64x2_hsub);
BUILTIN(caml_sse3_vec128_dup_low_64);
BUILTIN(caml_sse3_vec128_dup_odd_32);
BUILTIN(caml_sse3_vec128_dup_even_32);

BUILTIN(caml_ssse3_int8x16_abs);
BUILTIN(caml_ssse3_int16x8_abs);
BUILTIN(caml_ssse3_int32x4_abs);
BUILTIN(caml_ssse3_int16x8_hadd);
BUILTIN(caml_ssse3_int32x4_hadd);
BUILTIN(caml_ssse3_int16x8_hadd_saturating);
BUILTIN(caml_ssse3_int16x8_hsub);
BUILTIN(caml_ssse3_int32x4_hsub);
BUILTIN(caml_ssse3_int16x8_hsub_saturating);
BUILTIN(caml_ssse3_int8x16_mulsign);
BUILTIN(caml_ssse3_int16x8_mulsign);
BUILTIN(caml_ssse3_int32x4_mulsign);
BUILTIN(caml_ssse3_vec128_shuffle_8);
BUILTIN(caml_ssse3_vec128_align_right_bytes);

BUILTIN(caml_sse41_vec128_blend_16);
BUILTIN(caml_sse41_vec128_blend_32);
BUILTIN(caml_sse41_vec128_blend_64);
BUILTIN(caml_sse41_vec128_blendv_8);
BUILTIN(caml_sse41_vec128_blendv_32);
BUILTIN(caml_sse41_vec128_blendv_64);
BUILTIN(caml_sse41_int64x2_cmpeq);
BUILTIN(caml_sse41_cvtsx_int8x16_int16x8);
BUILTIN(caml_sse41_cvtsx_int8x16_int32x4);
BUILTIN(caml_sse41_cvtsx_int8x16_int64x2);
BUILTIN(caml_sse41_cvtsx_int16x8_int32x4);
BUILTIN(caml_sse41_cvtsx_int16x8_int64x2);
BUILTIN(caml_sse41_cvtsx_int32x4_int64x2);
BUILTIN(caml_sse41_cvtzx_int8x16_int16x8);
BUILTIN(caml_sse41_cvtzx_int8x16_int32x4);
BUILTIN(caml_sse41_cvtzx_int8x16_int64x2);
BUILTIN(caml_sse41_cvtzx_int16x8_int32x4);
BUILTIN(caml_sse41_cvtzx_int16x8_int64x2);
BUILTIN(caml_sse41_cvtzx_int32x4_int64x2);
BUILTIN(caml_sse41_float32x4_dp);
BUILTIN(caml_sse41_float64x2_dp);
BUILTIN(caml_sse41_int8x16_extract);
BUILTIN(caml_sse41_int16x8_extract);
BUILTIN(caml_sse41_int32x4_extract);
BUILTIN(caml_sse41_int64x2_extract);
BUILTIN(caml_sse41_int8x16_insert);
BUILTIN(caml_sse41_int16x8_insert);
BUILTIN(caml_sse41_int32x4_insert);
BUILTIN(caml_sse41_int64x2_insert);
BUILTIN(caml_sse41_float32x4_round);
BUILTIN(caml_sse41_float64x2_round);
BUILTIN(caml_sse41_int8x16_max);
BUILTIN(caml_sse41_int32x4_max);
BUILTIN(caml_sse41_int16x8_max_unsigned);
BUILTIN(caml_sse41_int32x4_max_unsigned);
BUILTIN(caml_sse41_int8x16_min);
BUILTIN(caml_sse41_int32x4_min);
BUILTIN(caml_sse41_int16x8_min_unsigned);
BUILTIN(caml_sse41_int32x4_min_unsigned);
BUILTIN(caml_sse41_int8x16_multi_sad_unsigned);
BUILTIN(caml_sse41_int16x8_minpos_unsigned);

BUILTIN(caml_sse42_int64x2_cmpgt);
BUILTIN(caml_sse42_vec128_cmpestrm);
BUILTIN(caml_sse42_vec128_cmpestra);
BUILTIN(caml_sse42_vec128_cmpestrc);
BUILTIN(caml_sse42_vec128_cmpestri);
BUILTIN(caml_sse42_vec128_cmpestro);
BUILTIN(caml_sse42_vec128_cmpestrs);
BUILTIN(caml_sse42_vec128_cmpestrz);
BUILTIN(caml_sse42_vec128_cmpistrm);
BUILTIN(caml_sse42_vec128_cmpistra);
BUILTIN(caml_sse42_vec128_cmpistrc);
BUILTIN(caml_sse42_vec128_cmpistri);
BUILTIN(caml_sse42_vec128_cmpistro);
BUILTIN(caml_sse42_vec128_cmpistrs);
BUILTIN(caml_sse42_vec128_cmpistrz);

#include <float.h>
#include <math.h>

// Int32

int32_t uint32_max(int32_t l, int32_t r) {
  uint32_t ul = (uint32_t)l;
  uint32_t ur = (uint32_t)r;
  return ul > ur ? l : r;
}
int32_t uint32_min(int32_t l, int32_t r) {
  uint32_t ul = (uint32_t)l;
  uint32_t ur = (uint32_t)r;
  return ul < ur ? l : r;
}
int64_t int32_si16(int64_t i) {
  int32_t x = (int32_t)i;
  return x > INT16_MAX ? INT16_MAX : (x < INT16_MIN ? INT16_MIN : x);
}
int64_t int32_su16(int64_t i) {
  int32_t x = (int32_t)i;
  return x > UINT16_MAX ? UINT16_MAX : (x < 0 ? 0 : x);
}

// Int16

int64_t int16_max(int64_t l, int64_t r) {
  int16_t ul = (int16_t)l;
  int16_t ur = (int16_t)r;
  return ul > ur ? l : r;
}
int64_t int16_min(int64_t l, int64_t r) {
  int16_t ul = (int16_t)l;
  int16_t ur = (int16_t)r;
  return ul < ur ? l : r;
}
int64_t int16_maxu(int64_t l, int64_t r) {
  uint16_t ul = (uint16_t)l;
  uint16_t ur = (uint16_t)r;
  return ul > ur ? l : r;
}
int64_t int16_minu(int64_t l, int64_t r) {
  uint16_t ul = (uint16_t)l;
  uint16_t ur = (uint16_t)r;
  return ul < ur ? l : r;
}
int64_t int16_add(int64_t l, int64_t r) {
  return (int16_t)l + (int16_t)r;
}
int64_t int16_sub(int64_t l, int64_t r) {
  return (int16_t)l - (int16_t)r;
}
int64_t int16_abs(int64_t i) {
  int16_t x = i;
  return x < 0 ? -x : x;
}
int64_t int16_adds(int64_t l, int64_t r) {
  int16_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > INT16_MAX) return INT16_MAX;
  if(sum < INT16_MIN) return INT16_MIN;
  return sum;
}
int64_t int16_subs(int64_t l, int64_t r) {
  int16_t x = l, y = r;
  int32_t diff = x - y;
  if(diff > INT16_MAX) return INT16_MAX;
  if(diff < INT16_MIN) return INT16_MIN;
  return diff;
}
int64_t int16_addsu(int64_t l, int64_t r) {
  uint16_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > UINT16_MAX) return UINT16_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int16_subsu(int64_t l, int64_t r) {
  uint16_t x = l, y = r;
  int32_t sum = (int32_t)x - (int32_t)y;
  if(sum > UINT16_MAX) return UINT16_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int16_mulsign(int64_t l, int64_t r) {
  int16_t x = l, y = r;
  return y == 0 ? 0 : y > 0 ? x : -x;
}
int64_t int16_cmpeq(int64_t l, int64_t r) {
  if((int16_t)l == (int16_t)r) return 0xffff;
  return 0;
}
int64_t int16_cmpgt(int64_t l, int64_t r) {
  if((int16_t)l > (int16_t)r) return 0xffff;
  return 0;
}
int32_t int16_sxi32(int64_t x) {
  return (int32_t)(int16_t)x;
}
int32_t int16_zxi32(int64_t x) {
  return (uint32_t)(uint16_t)x;
}
int64_t int16_sxi64(int64_t x) {
  return (int64_t)(int16_t)x;
}
int64_t int16_zxi64(int64_t x) {
  return (uint64_t)(uint16_t)x;
}
int64_t int16_logand(int64_t l, int64_t r) {
  return (int16_t)l & (int16_t)r;
}
int64_t int16_shift_left(int64_t x, int64_t shift) {
  return (int16_t)x << shift;
}
int64_t int16_shift_right(int64_t x, int64_t shift) {
  return (int16_t)x >> shift;
}
int64_t int16_shift_right_logical(int64_t x, int64_t shift) {
  return (uint16_t)(int16_t)x >> shift;
}
int64_t int16_avgu(int64_t l, int64_t r) {
  uint16_t x = (uint16_t)(int16_t)l;
  uint16_t y = (uint16_t)(int16_t)r;
  return (x + y + 1) >> 1;
}
int64_t int16_si8(int64_t i) {
  int16_t x = (int16_t)i;
  return x > INT8_MAX ? INT8_MAX : (x < INT8_MIN ? INT8_MIN : x);
}
int64_t int16_su8(int64_t i) {
  int16_t x = (int16_t)i;
  return x > UINT8_MAX ? UINT8_MAX : (x < 0 ? 0 : x);
}

// Int8

int64_t int8_max(int64_t l, int64_t r) {
  int8_t ul = (int8_t)l;
  int8_t ur = (int8_t)r;
  return ul > ur ? l : r;
}
int64_t int8_min(int64_t l, int64_t r) {
  int8_t ul = (int8_t)l;
  int8_t ur = (int8_t)r;
  return ul < ur ? l : r;
}
int64_t int8_maxu(int64_t l, int64_t r) {
  uint8_t ul = (uint8_t)l;
  uint8_t ur = (uint8_t)r;
  return ul > ur ? l : r;
}
int64_t int8_minu(int64_t l, int64_t r) {
  uint8_t ul = (uint8_t)l;
  uint8_t ur = (uint8_t)r;
  return ul < ur ? l : r;
}
int64_t int8_add(int64_t l, int64_t r) {
  return (int8_t)l + (int8_t)r;
}
int64_t int8_sub(int64_t l, int64_t r) {
  return (int8_t)l - (int8_t)r;
}
int64_t int8_abs(int64_t i) {
  int8_t x = i;
  return x < 0 ? -x : x;
}
int64_t int8_adds(int64_t l, int64_t r) {
  int8_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > INT8_MAX) return INT8_MAX;
  if(sum < INT8_MIN) return INT8_MIN;
  return sum;
}
int64_t int8_subs(int64_t l, int64_t r) {
  int8_t x = l, y = r;
  int32_t diff = x - y;
  if(diff > INT8_MAX) return INT8_MAX;
  if(diff < INT8_MIN) return INT8_MIN;
  return diff;
}
int64_t int8_addsu(int64_t l, int64_t r) {
  uint8_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > UINT8_MAX) return UINT8_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int8_subsu(int64_t l, int64_t r) {
  uint8_t x = l, y = r;
  int32_t sum = (int32_t)x - (int32_t)y;
  if(sum > UINT8_MAX) return UINT8_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int8_mulsign(int64_t l, int64_t r) {
  int8_t x = l, y = r;
  return y == 0 ? 0 : y > 0 ? x : -x;
}
int64_t int8_cmpeq(int64_t l, int64_t r) {
  if((int8_t)l == (int8_t)r) return 0xff;
  return 0;
}
int64_t int8_cmpgt(int64_t l, int64_t r) {
  if((int8_t)l > (int8_t)r) return 0xff;
  return 0;
}
int64_t int8_sxi16(int64_t x) {
  return (int64_t)(int16_t)(int8_t)x;
}
int64_t int8_zxi16(int64_t x) {
  return (uint64_t)(uint16_t)(uint8_t)x;
}
int32_t int8_sxi32(int64_t x) {
  return (int32_t)(int8_t)x;
}
int32_t int8_zxi32(int64_t x) {
  return (uint32_t)(uint8_t)x;
}
int64_t int8_sxi64(int64_t x) {
  return (int64_t)(int8_t)x;
}
int64_t int8_zxi64(int64_t x) {
  return (uint64_t)(uint8_t)x;
}
int64_t int8_avgu(int64_t l, int64_t r) {
  uint8_t x = (uint8_t)(int8_t)l;
  uint8_t y = (uint8_t)(int8_t)r;
  return (x + y + 1) >> 1;
}
int64_t int8_diffu(int64_t l, int64_t r) {
  uint8_t x = (uint8_t)(int8_t)l;
  uint8_t y = (uint8_t)(int8_t)r;
  return x > y ? x - y : y - x;
}

// Float64

double float64_round(double f) {
  __m128d v = _mm_set1_pd(f);
  return _mm_cvtsd_f64(_mm_round_pd(v, 0x8));
}
double float64_sqrt(double f) {
  __m128d v = _mm_set1_pd(f);
  return _mm_cvtsd_f64(_mm_sqrt_pd(v));
}
double float64_min(double l, double r) {
  __m128d lv = _mm_set1_pd(l);
  __m128d rv = _mm_set1_pd(r);
  return _mm_cvtsd_f64(_mm_min_pd(lv, rv));
}
double float64_max(double l, double r) {
  __m128d lv = _mm_set1_pd(l);
  __m128d rv = _mm_set1_pd(r);
  return _mm_cvtsd_f64(_mm_max_pd(lv, rv));
}


// Float32

int32_t int32_of_float(float f) {
  return *(int32_t*)&f;
}
float float_of_int32(int32_t i) {
  return *(float*)&i;
}

int32_t float32_zero(value unit) { return int32_of_float(0.0f); }
int32_t float32_neg_zero(value unit) { return int32_of_float(-0.0f); }
int32_t float32_one(value unit) { return int32_of_float(1.0f); }
int32_t float32_neg_one(value unit) { return int32_of_float(-1.0f); }
int32_t float32_nan(value unit) { return int32_of_float(NAN); }
int32_t float32_neg_infinity(value unit) { return int32_of_float(-INFINITY); }
int32_t float32_infinity(value unit) { return int32_of_float(INFINITY); }
int32_t float32_maxv(value unit) { return int32_of_float(FLT_MAX); }
int32_t float32_minv(value unit) { return int32_of_float(FLT_MIN); }
value float32_eq(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) == float_of_int32(r)); }
value float32_lt(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) < float_of_int32(r)); }
value float32_le(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) <= float_of_int32(r)); }
value float32_ne(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) != float_of_int32(r)); }
value float32_nle(int32_t l, int32_t r) { return Val_bool(!(float_of_int32(l) <= float_of_int32(r))); }
value float32_nlt(int32_t l, int32_t r) { return Val_bool(!(float_of_int32(l) < float_of_int32(r))); }
value float32_ord(int32_t l, int32_t r) { return Val_bool(!(isnan(float_of_int32(l)) || isnan(float_of_int32(r)))); }
value float32_uord(int32_t l, int32_t r) { return Val_bool(isnan(float_of_int32(l)) || isnan(float_of_int32(r))); }

#define FLOAT32_BINOP(name, intrin)              \
  int32_t float32_##name(int32_t l, int32_t r) { \
    __m128 vl = _mm_set1_ps(float_of_int32(l));  \
    __m128 vr = _mm_set1_ps(float_of_int32(r));  \
    return _mm_extract_ps(intrin(vl, vr), 0);    \
  }

#define FLOAT32_UNOP(name, intrin)             \
  int32_t float32_##name(int32_t f) {          \
    __m128 v = _mm_set1_ps(float_of_int32(f)); \
    return _mm_extract_ps(intrin(v), 0);       \
  }

FLOAT32_BINOP(add, _mm_add_ps);
FLOAT32_BINOP(sub, _mm_sub_ps);
FLOAT32_BINOP(mul, _mm_mul_ps);
FLOAT32_BINOP(div, _mm_div_ps);
FLOAT32_BINOP(min, _mm_min_ps);
FLOAT32_BINOP(max, _mm_max_ps);

FLOAT32_UNOP(sqrt, _mm_sqrt_ps);
FLOAT32_UNOP(rcp, _mm_rcp_ps);
FLOAT32_UNOP(rsqrt, _mm_rsqrt_ps);
FLOAT32_UNOP(cvt_i32, _mm_cvtps_epi32);

int32_t float32_round(int32_t f) {
  __m128 v = _mm_set1_ps(float_of_int32(f));
  return _mm_extract_ps(_mm_round_ps(v, 0x8), 0);
}
