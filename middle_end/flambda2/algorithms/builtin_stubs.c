#include "caml/mlvalues.h"
#include "caml/fail.h"

#if defined(_MSC_VER)
#include <intrin.h>
#endif

// These are replaced with clz instructions by flambda2.
//
// Use weak symbols in order to allow compiler-libs and ocaml_intrinsics_kernel
// to be shared dependencies of the same program.

CAMLweakdef intnat caml_int_clz_tagged_to_untagged(value i) {
    // Do not use Long_val(v1) conversion, instead preserving the tag.
    // It guarantees that the input to builtin_clz / _BitScanReverse is
    // non-zero, to guard against versions that are undefined for input 0. The
    // tag does not change the number of leading zeros.
#if defined(__GNUC__) || defined(__clang__)
    #if SIZEOF_PTR == SIZEOF_INT
    return __builtin_clz(i);
    #elif SIZEOF_PTR == SIZEOF_LONG
    return __builtin_clzl(i);
    #elif SIZEOF_PTR == SIZEOF_LONGLONG
    return __builtin_clzll(i);
    #else
    #error "No builtin clz function available"
    #endif
#elif defined(_MSC_VER)
    unsigned long r = 0;
    #ifdef SIZEOF_PTR == 8
    _BitScanReverse64(&r, i);
    r ^= 63;
    #elif SIZEOF_PTR == 4
    _BitScanReverse(&r, i);
    r ^= 31;
    #else
    #error "No builtin bsr function available"
    #endif
    return r;
#else
    #error "Unsupported compiler"
#endif
}

CAMLweakdef value caml_int_clz_tagged_to_tagged(value i) {
    return Val_int(caml_int_clz_tagged_to_untagged(i));
}
