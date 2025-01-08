#include "caml/mlvalues.h"
#include "caml/fail.h"

#if defined(_MSC_VER)
#include <intrin.h>
#endif

intnat caml_int_clz_tagged_to_untagged(value i) {
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

value caml_int_clz_tagged_to_tagged(value i) {
    return Val_int(caml_int_clz_tagged_to_untagged(i));
}
