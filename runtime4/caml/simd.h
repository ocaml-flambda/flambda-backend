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

/* SIMD vector instruction support */

#ifndef CAML_SIMD_H
#define CAML_SIMD_H

#include "mlvalues.h"

#if defined(_M_IX86_FP) || defined(__SSE2__) || defined(__SSE3__) || \
    defined(__SSSE3__) || defined(__SSE4_1__) || defined(__SSE4_2__)
#define ARCH_SSE2
#endif

#if defined(__AVX__) || defined(__AVX2__)
#define ARCH_AVX
#endif

#ifdef ARCH_SSE2
#include <emmintrin.h>

typedef __m128 simd_poly128_t;
typedef __m128 simd_float32x4_t;
typedef __m128d simd_float64x2_t;
typedef __m128i simd_int128_t;
typedef __m128i simd_int64x2_t;

#define Vec128_val(v)  _mm_loadu_ps((const float*)Bp_val(v))
#define Vec128_vald(v) _mm_loadu_pd((const double*)Bp_val(v))
#define Vec128_vali(v) _mm_loadu_si128((const __m128i*)Bp_val(v))
#define Store_vec128_val(v,x)  _mm_storeu_ps((float*)Bp_val(v), x)
#define Store_vec128_vald(v,x) _mm_storeu_pd((double*)Bp_val(v), x)
#define Store_vec128_vali(v,x) _mm_storeu_si128((__m128i*)Bp_val(v), x)

#else /* ARCH_SSE2 */

#ifdef __ARM_NEON
#include <arm_neon.h>

typedef poly128_t simd_poly128_t;
typedef float32x4_t simd_float32x4_t;
typedef float64x2_t simd_float64x2_t;
typedef poly128_t simd_int128_t;
typedef int64x2_t simd_int64x2_t;

#define Vec128_val(v)  vld1q_f32((const float*)Bp_val(v))
#define Vec128_vald(v) vld1q_f64((const double*)Bp_val(v))
#define Vec128_vali(v) vldrq_p128((const simd_int128_t*)Bp_val(v))
#define Store_vec128_val(v,x)  vst1q_f32((float*)Bp_val(v), x)
#define Store_vec128_vald(v,x) vst1q_f64((double*)Bp_val(v), x)
#define Store_vec128_vali(v,x) vstrq_p128((simd_int128_t*)Bp_val(v), x)

#else /* __ARM_NEON */
#error "Target not supported"
#endif /* __ARM_NEON */
#endif /* ARCH_SSE2 */

CAMLextern value caml_copy_vec128(simd_float32x4_t);
CAMLextern value caml_copy_vec128i(simd_int128_t);
CAMLextern value caml_copy_vec128d(simd_float64x2_t);

#endif /* CAML_SIMD_H */
