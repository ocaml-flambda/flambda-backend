# Non-modal bounds

## Externality

The externality axis records whether all a type's values may safely be ignored
by the GC.  This may be because they are OCaml "immediates" (values represented
by a tagged integer), because they are unboxed types like `float#` or `int32#`,
or because they are allocated elsewhere.

The axis has three possible values, with `external_ < external64 < internal`.
* `external_` means that all values of the type are safely ignored by the
  GC.
* `external64` means that all values of the type are safely ignored by the GC
  _on 64-bit systems_. The only 32-bit target currently supported by the OCaml
  compiler is bytecode. Note that, although JavaScript and WASM are 32-bit
  platforms and the compiler goes through bytecode to reach them, they still
  count as 64-bit systems for the purpose of this axis because of their unique
  data models.
* `internal` means values of the type may need to be scanned.

The compiler uses the externality axis for certain runtime optimizations. In
particular, updating a mutable reference to a type that is `external_` can skip
the write barrier (i.e., it does not need a call to `caml_modify`).

In the future, we plan to make externality a mode, rather than just a property
of types.

## Nullability

The nullability axis records whether `NULL` (the machine word 0) is a possible
value of a type, and is used to support the non-allocating option `'a or_null`
type. The axis has two possible values, with `non_null < maybe_null`. A type may
be `non_null` if none of its values are `NULL`.

The kind of values with `NULL` added as a possibility is written
`value_or_null`. The more common `value` kind is an abbreviation for
`value_or_null mod non_null`.

Types that don't have `NULL` as a possible value are
compatible with `or_null`, a non-allocating option type that is built into
OxCaml.  Its definition is:
```ocaml
type ('a : value) or_null : value_or_null =
  | Null
  | This of 'a
```

## Separability

CR reisenberg: TODO
