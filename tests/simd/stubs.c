
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <smmintrin.h>
#include <emmintrin.h>
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
  CAMLlocal1(res);

  __m128i l = _mm_loadu_si128((__m128i*)v0);
  __m128i r = _mm_loadu_si128((__m128i*)v1);
  __m128i result = _mm_add_epi64(l, r);
  res = caml_alloc_small(2, Abstract_tag);
  _mm_storeu_si128((__m128i*)res, result);

  CAMLreturn(res);
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

#include <float.h>
#include <math.h>

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
