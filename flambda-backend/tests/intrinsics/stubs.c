#include <assert.h>

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_csel_value);
BUILTIN(caml_csel_int64_unboxed);
