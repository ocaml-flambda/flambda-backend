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

#include "misc.h"

// This file is included by mlvalues.h, so should be visible
// to all user-defined C bindings. When compiling C stubs to
// x86_64/ELF, this detects which ISA extensions are enabled and
// requests the OCaml runtime to check for support at startup.

#if defined __x86_64__ && defined __ELF__

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

#ifdef __POPCNT__
CAML_REQUIRE_ARCH_POPCNT
#endif

#ifdef __PRFCHW__
CAML_REQUIRE_ARCH_PREFETCHW
#endif

#ifdef __PREFETCHWT1__
CAML_REQUIRE_ARCH_PREFETCHWT1
#endif

#ifdef __SSE3__
CAML_REQUIRE_ARCH_SSE3
#endif

#ifdef __SSSE3__
CAML_REQUIRE_ARCH_SSSE3
#endif

#ifdef __SSE4_1__
CAML_REQUIRE_ARCH_SSE4_1
#endif

#ifdef __SSE4_2__
CAML_REQUIRE_ARCH_SSE4_2
#endif

#ifdef __PCLMUL__
CAML_REQUIRE_ARCH_CLMUL
#endif

#ifdef __LZCNT__
CAML_REQUIRE_ARCH_LZCNT
#endif

#ifdef __BMI__
CAML_REQUIRE_ARCH_BMI
#endif

#ifdef __BMI2__
CAML_REQUIRE_ARCH_BMI2
#endif

#endif

#ifdef CAML_INTERNALS
CAMLextern void caml_assert_arch_extensions(void);
extern uintnat caml_skip_arch_extension_check;
#endif

#endif /* CAML_ISA_H */
