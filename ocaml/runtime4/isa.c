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

#define CAML_INTERNALS

#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/isa.h"

uintnat caml_skip_arch_extension_check = 0;

// Weak symbols are only supported in ELF

#if defined __x86_64__ && defined __ELF__

// Must be kept in sync with amd64/emit.mlp

// We declare each architecture flag as a separate weak symbol.
// These symbols are not defined (i.e. are not assigned an address)
// in this compilation unit. They compile to [.weak symbol_name].
//
// Other compilation units may define the symbol by associating
// its name with a label [symbol_name: ...].
//
// If multiple compilation units define the symbol, the linker
// will choose one arbitrarily, and the symbol's address will
// point into that unit. If the symbols are never defined,
// their address will be NULL. This allows us to check for
// their presence in the final executable below.

CAMLweakdef extern intnat caml_arch_popcnt;
CAMLweakdef extern intnat caml_arch_prefetchw;
CAMLweakdef extern intnat caml_arch_prefetchwt1;
CAMLweakdef extern intnat caml_arch_sse3;
CAMLweakdef extern intnat caml_arch_ssse3;
CAMLweakdef extern intnat caml_arch_sse4_1;
CAMLweakdef extern intnat caml_arch_sse4_2;
CAMLweakdef extern intnat caml_arch_clmul;
CAMLweakdef extern intnat caml_arch_lzcnt;
CAMLweakdef extern intnat caml_arch_bmi;
CAMLweakdef extern intnat caml_arch_bmi2;

// CPUID with EAX = 1, result in ECX

#define SSE3_BIT         (1 << 0)
#define PCLMULQDQ_BIT    (1 << 1)
#define SSSE3_BIT        (1 << 9)
#define SSSE4_1_BIT      (1 << 19)
#define SSSE4_2_BIT      (1 << 20)
#define POPCNT_BIT       (1 << 23)

// CPUID with EAX = 0x80000001, result in ECX

#define LZCNT_BIT     (1 << 5)
#define PREFETCHW_BIT (1 << 8)

// CPUID with EAX = 7, ECX = 0, result in EBX

#define BMI_BIT            (1 << 3)
#define BMI2_BIT           (1 << 8)

// CPUID with EAX = 7, ECX = 0, result in ECX

#define PREFETCHWT1_BIT    (1 << 0)

//////////////////////////////////////////////////////////////////////////

static void caml_cpuid(int info[4], int eax, int ecx) {
#ifdef _MSC_VER
    __cpuidex((int *)info, eax, ecx);
#else
    asm volatile (
        "cpuid"
        : "=a" (info[0]), "=b" (info[1]), "=c" (info[2]), "=d" (info[3])
        : "a" (eax), "c" (ecx)
    );
#endif
}

CAMLexport void caml_assert_arch_extensions(void) {

    if(caml_skip_arch_extension_check) return;

    int info[4];
    caml_cpuid(info, 1, 0);

    // If a weak symbol is not defined, its address is NULL

    if(&caml_arch_popcnt) {
        if(!(info[2] & POPCNT_BIT)) {
            caml_fatal_error("Binary compiled with -fpopcnt, but this CPU lacks support.");
        }
    }
    if(&caml_arch_sse3) {
        if(!(info[2] & SSE3_BIT)) {
            caml_fatal_error("Binary compiled with -fsse3, but this CPU lacks support.");
        }
    }
    if(&caml_arch_ssse3) {
        if(!(info[2] & SSSE3_BIT)) {
            caml_fatal_error("Binary compiled with -fssse3, but this CPU lacks support.");
        }
    }
    if(&caml_arch_sse4_1) {
        if(!(info[2] & SSSE4_1_BIT)) {
            caml_fatal_error("Binary compiled with -fsse41, but this CPU lacks support.");
        }
    }
    if(&caml_arch_sse4_2) {
        if(!(info[2] & SSSE4_2_BIT)) {
            caml_fatal_error("Binary compiled with -fsse42, but this CPU lacks support.");
        }
    }
    if(&caml_arch_clmul) {
        if(!(info[2] & PCLMULQDQ_BIT)) {
            caml_fatal_error("Binary compiled with -fpclmul, but this CPU lacks support.");
        }
    }

    caml_cpuid(info, 0x80000001, 0);

    if(&caml_arch_prefetchw) {
        if(!(info[2] & PREFETCHW_BIT)) {
            caml_fatal_error("Binary compiled with -fprefetchw, but this CPU lacks support.");
        }
    }
    if(&caml_arch_lzcnt) {
        if(!(info[2] & LZCNT_BIT)) {
            caml_fatal_error("Binary compiled with -flzcnt, but this CPU lacks support.");
        }
    }

    caml_cpuid(info, 7, 0);

    if(&caml_arch_bmi) {
        if(!(info[1] & BMI_BIT)) {
            caml_fatal_error("Binary compiled with -fbmi, but this CPU lacks support.");
        }
    }
    if(&caml_arch_bmi2) {
        if(!(info[1] & BMI2_BIT)) {
            caml_fatal_error("Binary compiled with -fbmi2, but this CPU lacks support.");
        }
    }
    if(&caml_arch_prefetchwt1) {
        if(!(info[2] & PREFETCHWT1_BIT)) {
            caml_fatal_error("Binary compiled with -fprefetchwt1, but this CPU lacks support.");
        }
    }
}

#else // No extensions for other architectures are currently available.

CAMLexport void caml_assert_arch_extensions(void) {}

#endif
