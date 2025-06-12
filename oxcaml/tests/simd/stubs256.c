
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/simd.h>
#include <caml/callback.h>
#include <immintrin.h>
#include <assert.h>

int64_t vec256_first_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 3);
}

int64_t vec256_second_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 2);
}

int64_t vec256_third_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 1);
}

int64_t vec256_fourth_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 0);
}

__m256i vec256_of_int64s(int64_t w0, int64_t w1, int64_t w2, int64_t w3)
{
    return _mm256_set_epi64x(w0, w1, w2, w3);
}

value boxed_combine256(value v0, value v1)
{
    CAMLparam2(v0, v1);

    __m256i l = Vec256_vali(v0);
    __m256i r = Vec256_vali(v1);
    __m256i result = _mm256_add_epi64(l, r);

    CAMLreturn(caml_copy_vec256i(result));
}

__m256i lots_of_vectors256(
    __m256i v0, __m256i v1, __m256i v2, __m256i v3,
    __m256i v4, __m256i v5, __m256i v6, __m256i v7,
    __m256i v8, __m256i v9, __m256i v10, __m256i v11,
    __m256i v12, __m256i v13, __m256i v14, __m256i v15)
{
    __m256i x0 = _mm256_add_epi64(v0, v1);
    __m256i x1 = _mm256_add_epi64(v2, v3);
    __m256i x2 = _mm256_add_epi64(v4, v5);
    __m256i x3 = _mm256_add_epi64(v6, v7);
    __m256i x4 = _mm256_add_epi64(v8, v9);
    __m256i x5 = _mm256_add_epi64(v10, v11);
    __m256i x6 = _mm256_add_epi64(v12, v13);
    __m256i x7 = _mm256_add_epi64(v14, v15);
    __m256i y0 = _mm256_add_epi64(x0, x1);
    __m256i y1 = _mm256_add_epi64(x2, x3);
    __m256i y2 = _mm256_add_epi64(x4, x5);
    __m256i y3 = _mm256_add_epi64(x6, x7);
    __m256i z0 = _mm256_add_epi64(y0, y1);
    __m256i z1 = _mm256_add_epi64(y2, y3);
    return _mm256_add_epi64(z0, z1);
}

__m256i vectors_and_floats256(
    __m256i v0, double f0, __m256i v1, double f1,
    __m256i v2, double f2, __m256i v3, double f3,
    double f4, __m256i v4, __m256i v5, double f5,
    double f6, __m256i v6, __m256i v7, double f7,
    double f8, double f9, __m256i v8, __m256i v9,
    __m256i v10, double f10, double f11, double f12)
{
    __m256i x0 = _mm256_add_epi64(v0, v1);
    __m256i x1 = _mm256_add_epi64(v2, v3);
    __m256i x2 = _mm256_add_epi64(v4, v5);
    __m256i x3 = _mm256_add_epi64(v6, v7);
    __m256i x4 = _mm256_add_epi64(v8, v9);
    __m256i y0 = _mm256_add_epi64(x0, x1);
    __m256i y1 = _mm256_add_epi64(x2, x3);
    __m256i y2 = _mm256_add_epi64(v10, x4);
    __m256i z0 = _mm256_add_epi64(y0, y1);
    __m256i z = _mm256_add_epi64(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10 + f11 + f12;
    return vec256_of_int64s((int64_t)f, vec256_first_int64(z) + vec256_second_int64(z),
                            vec256_third_int64(z), vec256_fourth_int64(z));
}

__m256i vectors_and_floats_and_ints256(
    __m256i v0, double f0, __m256i v1, int64_t i0,
    __m256i v2, double f1, __m256i v3, int64_t i1,
    int64_t i2, __m256i v4, __m256i v5, double f2,
    double f3, __m256i v6, __m256i v7, int64_t i3,
    int64_t i4, double f4, __m256i v8, __m256i v9,
    __m256i v10, int64_t i5, int64_t i6, double f5)
{
    __m256i x0 = _mm256_add_epi64(v0, v1);
    __m256i x1 = _mm256_add_epi64(v2, v3);
    __m256i x2 = _mm256_add_epi64(v4, v5);
    __m256i x3 = _mm256_add_epi64(v6, v7);
    __m256i x4 = _mm256_add_epi64(v8, v9);
    __m256i y0 = _mm256_add_epi64(x0, x1);
    __m256i y1 = _mm256_add_epi64(x2, x3);
    __m256i y2 = _mm256_add_epi64(v10, x4);
    __m256i z0 = _mm256_add_epi64(y0, y1);
    __m256i z = _mm256_add_epi64(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5;
    int64_t i = i0 + i1 + i2 + i3 + i4 + i5 + i6;
    return vec256_of_int64s((int64_t)f + i, vec256_first_int64(z) + vec256_second_int64(z),
                            vec256_third_int64(z), vec256_fourth_int64(z));
}

double vector_and_then_stack_floats(
    __attribute__((unused)) __m256i v0,
    double f0, double f1, double f2, double f3,
    double f4, double f5, double f6, double f7)
{
    return f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7;
}
