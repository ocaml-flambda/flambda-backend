# Introduction to Uniqueness

See also the full feature [reference](reference.md) and [common pitfalls](pitfalls.md).
In this document, we use the new [syntax for modes](../modes/syntax.md),
which we expect to be stable soonish.

The `unique` mode designates values that have only a single reference pointing
to them. For example, an externally allocated data structure might support a
`free` operation:

```ocaml
val free : t @ unique -> unit
```

The compiler guarantees that this operation will be sound: as the argument is
`unique`, there can be no further references to it that could lead to a
use-after-free. The compiler tracks the uniqueness of values and marks values
which have more than one reference pointing to them with the `aliased` mode:

```ocaml
let duplicate : t -> t * t @ aliased = fun t -> t, t
```

When a `unique` value is captured in a closure, this closure can be invoked at
most once: if the closure was invoked more often, it could not use the value
uniquely each time. Values that contain closures that can be invoked at most
once are at mode `once`, while values with closures that can be invoked
arbitrarily often are at mode `many`:

```ocaml
let delay_free : t @ unique -> (unit -> unit) @ once = fun t -> fun () -> free t
```

## Modalities and Mode Crossing

These modes form two mode axis: the _uniqueness_ of a value is either `unique`
or `aliased`, while the _affinity_ of a value is `once` or `many`. Similar to
[locality](../local/intro.md), uniqueness and affinity are deep properties. If a
value is at mode `unique` then all of its children are also `unique`.

We make an exception to this rule for record fields that are annotated with a
_modality_. Analogous to global fields, a record field that is annotated as `@@
aliased`, can contain `aliased` values even if the record itself is `unique`.
However, a read from such a field always returns an `aliased` value. If a record
field is annotated by `@@ many`, then it can only store `many` values even if
the record itself is `once`. In return, a read of that field always yields a
`many` value.

Analogous to the `globalize` function, it is possible to cast between the modes:

```ocaml
val alias : 'a @ unique -> 'a @ aliased
val linearize : 'a @ many -> 'a @ once
```

However, the other directions are disallowed in general: if a value is `aliased`
there is no way to get it back to mode `unique`; similarly a `once` value can
not be made `many`.

We make an exception to this rule for values of special types to which these
modes do not apply. For example, `int`s and other immediates can be used as
`unique` and `many`, no matter their mode. Similarly, most values that don't
contain functions can be used as `many`. For example, an `int list @ once
aliased` can be used as `many` but not as `unique`.

## Checking for Uniqueness

The compiler performs a sophisticated analysis to determine which values are
aliased and which ones are unique. For example, it is fine to match on a value
and then use it:

```ocaml
let okay t =
  match t with
  | Con { field } -> free t
```

But it would not be fine to use (parts of) `t` twice:

```ocaml
let bad t =
  match t with
  | Con { field } ->
    free_field field;
    free t
```

In the `bad` function above, the `free t` assumes that it gets all of `t`
uniquely, but this is not true if the `field` as been freed previously. However,
we can use parts of `t` twice if these uses happen in different branches:

```ocaml
let okay t =
  match t with
  | Con { field } ->
    if flip_coin ()
    then free_field field
    else free t
```

or if the uses both accept `aliased` arguments:

```ocaml
val store : t @ aliased -> unit

let okay t =
  match t with
  | Con { field } ->
    store_field field;
    store t
```

## Current syntax

Until the new [syntax for modes](../modes/syntax.md) is stable, our syntax for
uniqueness will be slightly different from the description above. Currently, you
would mark an argument or return value in a type signature as unique by
prepending it with `unique_` and as once by prepending it with `once_`. You can
not mark arguments or return values as aliased or many. To mark a record field
as `@@ aliased`, you would prepend `aliased_` and to mark it as `@@ many`, you
would prepend `many_`.

For more details, read [the reference](./reference.md).
