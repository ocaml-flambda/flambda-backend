
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <smmintrin.h>
#include <emmintrin.h>

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
