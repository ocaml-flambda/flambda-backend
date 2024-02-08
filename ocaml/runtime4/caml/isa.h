/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 2024 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_ISA_H
#define CAML_ISA_H

#include "mlvalues.h"

#define CAML_REQUIRE_ARCH_POPCNT \
    CAMLweakdef intnat caml_arch_popcnt;

#define CAML_REQUIRE_ARCH_PREFETCHW \
    CAMLweakdef intnat caml_arch_prefetchw;

#define CAML_REQUIRE_ARCH_PREFETCHWT1 \
    CAMLweakdef intnat caml_arch_prefetchwt1;

#define CAML_REQUIRE_ARCH_SSE3 \
    CAMLweakdef intnat caml_arch_sse3;

#define CAML_REQUIRE_ARCH_SSSE3 \
    CAMLweakdef intnat caml_arch_ssse3;

#define CAML_REQUIRE_ARCH_SSE4_1 \
    CAMLweakdef intnat caml_arch_sse4_1;

#define CAML_REQUIRE_ARCH_SSE4_2 \
    CAMLweakdef intnat caml_arch_sse4_2;

#define CAML_REQUIRE_ARCH_CLMUL \
    CAMLweakdef intnat caml_arch_clmul;

#define CAML_REQUIRE_ARCH_LZCNT \
    CAMLweakdef intnat caml_arch_lzcnt;

#define CAML_REQUIRE_ARCH_BMI \
    CAMLweakdef intnat caml_arch_bmi;

#define CAML_REQUIRE_ARCH_BMI2 \
    CAMLweakdef intnat caml_arch_bmi2;

CAMLextern void caml_assert_arch_extensions(void);

#ifdef CAML_INTERNALS
extern uintnat caml_skip_arch_extension_check;
#endif

#endif /* CAML_ISA_H */
