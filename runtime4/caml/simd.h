/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 2023 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Utilities for boxing & unboxing SIMD vectors in user C code. */

#ifndef CAML_SIMD_H
#define CAML_SIMD_H

// SIMD is only supported on 64-bit targets
#define Words_per_vec128 2
#define Words_per_vec256 4

#include "mlvalues.h"
#include "alloc.h"

#if defined(_M_IX86_FP) || defined(__SSE2__) || defined(__SSE3__) || \
    defined(__SSSE3__) || defined(__SSE4_1__) || defined(__SSE4_2__)
#define ARCH_SSE2
#endif

#if defined(__AVX__) || defined(__AVX2__)
#define ARCH_AVX
#endif

#ifdef ARCH_SSE2
#include <emmintrin.h>

#define Vec128_val(v) _mm_loadu_ps((const float *)Bp_val(v))
#define Vec128_vald(v) _mm_loadu_pd((const double *)Bp_val(v))
#define Vec128_vali(v) _mm_loadu_si128((const __m128i *)Bp_val(v))
#define Store_vec128_val(v, x) _mm_storeu_ps((float *)Bp_val(v), x)
#define Store_vec128_vald(v, x) _mm_storeu_pd((double *)Bp_val(v), x)
#define Store_vec128_vali(v, x) _mm_storeu_si128((__m128i *)Bp_val(v), x)

Caml_inline value caml_copy_vec128(__m128 v) {
    value res = caml_alloc_small(Words_per_vec128, Abstract_tag);
    Store_vec128_val(res, v);
    return res;
}

Caml_inline value caml_copy_vec128i(__m128i v) {
    value res = caml_alloc_small(Words_per_vec128, Abstract_tag);
    Store_vec128_vali(res, v);
    return res;
}

Caml_inline value caml_copy_vec128d(__m128d v) {
    value res = caml_alloc_small(Words_per_vec128, Abstract_tag);
    Store_vec128_vald(res, v);
    return res;
}

#endif /* ARCH_SSE2 */

#ifdef ARCH_AVX
#include <immintrin.h>

#define Vec256_val(v) _mm256_loadu_ps((const float *)Bp_val(v))
#define Vec256_vald(v) _mm256_loadu_pd((const double *)Bp_val(v))
#define Vec256_vali(v) _mm256_loadu_si256((const __m256i *)Bp_val(v))
#define Store_vec256_val(v, x) _mm256_storeu_ps((float *)Bp_val(v), x)
#define Store_vec256_vald(v, x) _mm256_storeu_pd((double *)Bp_val(v), x)
#define Store_vec256_vali(v, x) _mm256_storeu_si256((__m256i *)Bp_val(v), x)

Caml_inline value caml_copy_vec256(__m256 v) {
    value res = caml_alloc_small(Words_per_vec256, Abstract_tag);
    Store_vec256_val(res, v);
    return res;
}

Caml_inline value caml_copy_vec256i(__m256i v) {
    value res = caml_alloc_small(Words_per_vec256, Abstract_tag);
    Store_vec256_vali(res, v);
    return res;
}

Caml_inline value caml_copy_vec256d(__m256d v) {
    value res = caml_alloc_small(Words_per_vec256, Abstract_tag);
    Store_vec256_vald(res, v);
    return res;
}

#endif /* ARCH_AVX */

#ifdef __ARM_NEON
#include <arm_neon.h>

#define Vec128_val(v) vld1q_f32((const float *)Bp_val(v))
#define Vec128_vald(v) vld1q_f64((const double *)Bp_val(v))
#define Vec128_vali(v) vldrq_p128((const poly128_t *)Bp_val(v))
#define Store_vec128_val(v, x) vst1q_f32((float *)Bp_val(v), x)
#define Store_vec128_vald(v, x) vst1q_f64((double *)Bp_val(v), x)
#define Store_vec128_vali(v, x) vstrq_p128((poly128_t *)Bp_val(v), x)

Caml_inline value caml_copy_vec128(float32x4_t v) {
    value res = caml_alloc_small(Words_per_vec128, Abstract_tag);
    Store_vec128_val(res, v);
    return res;
}

Caml_inline value caml_copy_vec128i(poly128_t v) {
    value res = caml_alloc_small(Words_per_vec128, Abstract_tag);
    Store_vec128_vali(res, v);
    return res;
}

Caml_inline value caml_copy_vec128d(float64x2_t v) {
    value res = caml_alloc_small(Words_per_vec128, Abstract_tag);
    Store_vec128_vald(res, v);
    return res;
}

#endif /* __ARM_NEON */

#endif /* CAML_SIMD_H */
