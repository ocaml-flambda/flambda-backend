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

#define Vec128_val(v)  _mm_loadu_ps((const float*)Bp_val(v))
#define Vec128_vald(v) _mm_loadu_pd((const double*)Bp_val(v))
#define Vec128_vali(v) _mm_loadu_si128((const __m128i*)Bp_val(v))
#define Store_vec128_val(v,x)  _mm_storeu_ps((float*)Bp_val(v), x)
#define Store_vec128_vald(v,x) _mm_storeu_pd((double*)Bp_val(v), x)
#define Store_vec128_vali(v,x) _mm_storeu_si128((__m128i*)Bp_val(v), x)

CAMLextern value caml_copy_vec128(__m128);
CAMLextern value caml_copy_vec128i(__m128i);
CAMLextern value caml_copy_vec128d(__m128d);
#endif

#endif /* CAML_SIMD_H */
