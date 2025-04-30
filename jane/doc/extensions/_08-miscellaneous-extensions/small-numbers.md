---
layout: documentation-page
collectionName: Miscellaneous extensions
title: Small Numbers
---

# Small Numbers

The small numbers extension adds `float32`, `int16`, and `int8` types to OxCaml.
Currently, only `float32` (single-precision IEEE float) is implemented.

## Float32

When small numbers are enabled, the following float32 types are available:

```
float32
float32#
float32 array
float32# array
```

Literals use the `s` suffix:

```
1.0s  : float32
#1.0s : float32#
```

Pattern matching on `float32`s is not supported.

### Operations

Operations on 32-bit floats are available via the `Stdlib_stable.Float32` and
`Stdlib_stable.Float32_u` libraries, which provide `Base`-like APIs.

### Representation

The boxed `float32` type is encoded as a custom block with similar semantics to
`int32`.  Similarly, `float32 array` is a typical OxCaml array containing boxed
elements.

The `float32#` type is unboxed:

- Function arguments and returns of type `float32#` are passed using
  floating-point registers.

- Record fields of type `float32#` are not boxed, but each take up one word of
  space.  Using float32 records requires the mixed blocks extension, which is
  also enabled by default.

- Arrays of type `float32# array` contain tightly packed unboxed float32
  elements.  The array itself is a custom block with similar semantics to
  `int32# array`.

Like floats, compiler optimizations allow boxed float32s to remain unboxed while
being manipulated within the scope of a function.

### C ABI

Both boxed and unboxed float32s may be passed to C stubs.  The OxCaml runtime
provides helper functions for working with float32s.

```ocaml
external float32_stub : (float32[@unboxed]) -> (float32[@unboxed]) =
  "boxed_float32_stub" "unboxed_float32_stub"

external float32_hash_stub : float32# -> float32# =
  "boxed_float32_stub" "unboxed_float32_stub"

(* ... *)
```
```c
#include <caml/float32.h>

float unboxed_float32_stub(float v) {
  return v;
}

value boxed_float32_stub(value v) {
  return caml_copy_float32(unboxed_float32_stub(Float32_val(v)));
}
```

## Int8 / Int16

Coming soon.
