
#include <assert.h>
#include <immintrin.h>

#define BUILTIN(name) void name(void) { assert(0); }

BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_simd_vec128_interleave_high_64);
BUILTIN(caml_simd_vec128_interleave_low_64);

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
