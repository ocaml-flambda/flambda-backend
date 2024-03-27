
# SIMD in flambda2

## Vector types

<!-- CR mslater: more docs -->

## Intrinsics

<!-- CR mslater: more docs -->

## Load/Store

Unlike intrinsics, SIMD loads and stores are represented as flambda2-visible primitives.

- `string`, `bytes`: `caml_{string,bytes}_{getu128,setu128}{u}` map to `MOVUPD`, an unaligned 128-bit vector load/store.
  The primitives can operate on all 128-bit vector types.
  The safe primitives raise `Invalid_argument` if any part of the vector is not within the array bounds; the `u` suffix omits this check.
  Aligned load/store is not available because these values may be moved by the GC.

- `bigstring`: `caml_bigstring_{get,set}{u}128{u}` map to `MOVAPD` or `MOVUPD`.
  The primitives can operate on all 128-bit vector types.
  The prefix `u` indicates an unaligned operation (`MOVUPD`), and the suffix `u` omits bounds checking.
  Aligned load/store is available because bigstrings are allocated by `malloc`.

- `float array`, `floatarray`, `float# array`: the corresponding primitives take an index in `float`s and are required to operate on `float64x2`s.
  The address is computed as `array + index * 8`; the safe primitives bounds-check against `0, length - 1`.
  The primitives on `float array` are only available when the float array optimization is enabled.
  Aligned load/store is not available because these values may be moved by the GC.

- `nativeint# array`, `int64# array`: the corresponding primitives take an index in `nativeint`s/`int64`s and are required to operate on `int64x2`s.
  The address is computed as `array + index * 8`; the safe primitives bounds-check against `0, length - 1`.
  The primitives on `nativeint# array` are only available in 64-bit mode.
  Aligned load/store is not available because these values may be moved by the GC.

- `int32# array`: the corresponding primitives take an index in `int32`s and are required to operate on `int32x4`s.
  The address is computed as `array + index * 4`; the safe primitives bounds-check against `0, length - 3`.
  Aligned load/store is not available because these values may be moved by the GC.

- `%immediate64 array`: the corresponding primitives take an index in immediates, and are required to operate on `int64x2`s.
  The primitives can operate on all `('a : immediate64) array`s and are only available in 64-bit mode.
  The address is computed as `array + index * 8`; the safe primitives bounds-check against `0, length - 1`.
  Aligned load/store is not available because these values may be moved by the GC.
  Load/store directly reads/writes two 64-bit **tagged** values. The "safe" primitives do not check for proper tagging,
  so are not to be exposed to users as "safe."
