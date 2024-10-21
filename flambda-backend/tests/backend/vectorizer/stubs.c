#include <assert.h>

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_sse2_int64x2_add);
BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_sse_vec128_high_64_to_low_64);
BUILTIN(caml_sse_vec128_low_64_to_high_64);
