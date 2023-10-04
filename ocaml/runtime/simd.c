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

#define CAML_INTERNALS

#include "caml/alloc.h"
#include "caml/simd.h"

#ifdef ARCH_SSE2

CAMLexport value caml_copy_vec128(__m128 v) {
    value res = caml_alloc_small(2, Abstract_tag);
    Store_vec128_val(res, v);
    return res;
}

CAMLexport value caml_copy_vec128i(__m128i v) {
    value res = caml_alloc_small(2, Abstract_tag);
    Store_vec128_vali(res, v);
    return res;
}

CAMLexport value caml_copy_vec128d(__m128d v) {
    value res = caml_alloc_small(2, Abstract_tag);
    Store_vec128_vald(res, v);
    return res;
}

#endif
