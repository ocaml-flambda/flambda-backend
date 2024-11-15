# Introduction to Uniqueness

See also the full feature [reference](reference.md) and [common pitfalls](pitfalls.md).
In this document, we use the new [syntax for modes](../modes/syntax.md).

The `unique` mode designates values that have only a single reference pointing
to them. If an operation takes a `unique` argument, it will consume the only
reference to the value. For example, an externally allocated data structure
might support a `free` operation:

```ocaml
val free : t @ unique -> unit
```

Normally, a `free` operation would be dangerous since any remaining references
can lead to a use-after-free segfault. But with the `unique` mode, the compiler
guarantees that no further references remain. To provide this guarantee, the
compiler tracks the uniqueness of values and marks values which have more than
one reference pointing to them with the `aliased` mode:

```ocaml
let duplicate : t -> t * t @ aliased = fun t -> t, t
```

When a `unique` value is consumed in a closure, this closure can be invoked at
most once: if the closure was invoked more often, it could not use the value
uniquely each time. Such closures are at mode `once`, while closures that can be
invoked arbitrarily often are at mode `many`:

```ocaml
let delay_free : t @ unique -> (unit -> unit) @ once = fun t -> fun () -> free t
```

## Modalities

These modes form two mode axes: the _uniqueness_ of a value is either `unique`
or `aliased`, while the _affinity_ of a value is `once` or `many`. Similar to
[locality](../local/intro.md), uniqueness and affinity are deep properties. If a
value is at mode `unique` then all of its children are also `unique`. If a value
is `once` then all of the closures it contains are also at mode `once`.

We make an exception to this rule for record fields that are annotated with a
_modality_. Analogous to global fields, a record field that is annotated as `@@
aliased`, can contain `aliased` values even if the record itself is `unique`.
However, a read from such a field always returns an `aliased` value. If a record
field is annotated by `@@ many`, then it can only store `many` values even if
the record itself is `once`. In return, a read of that field always yields a
`many` value.

This is especially useful if we want to ensure the uniqueness of a data
structure (eg. to be able to free it later), but can not guarantee that the
elements will always be unique. Rather than copying the elements upon insertion
to make them unique, we wrap them in a record with the `aliased` modality:

```ocaml
type 'a aliased = { a : 'a @@ aliased } [@@unboxed]

val cons : 'a @ aliased -> 'a aliased list @ unique -> 'a aliased list @ unique
```

## Mode Crossing

You can always cast a value to a mode that affords fewer guarantees: You can use
a `unique` value like an `aliased` value, and a `many` value like a `once`
value. However, the other directions are disallowed in general: if a value is
`aliased` there is no way to get it back to mode `unique`; similarly a `once`
value can not be made `many`.

We make an exception to this rule for values of those types that cross these
modes. For example, `int`s and other immediates can be used as `unique` and
`many`, no matter their mode. For example:

```ocaml
type delayed_free = { id : int; callback : unit -> unit }

let get_id : delayed_free @ once -> int @ many = fun t -> t.id
```

We are working on a feature which will make it possible to use a value as `many`
if its type prevents it from containing a function. For example, you will then
be able to use an `int list @ once aliased` as `many` (but not as `unique`).

## Checking for Uniqueness

The compiler performs a sophisticated analysis to determine which values are
aliased and which ones are unique. For example, it is fine to match on a unique
value and then use it:

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

In the `bad` function above, `free_field` assumes that it gets the only
reference to `field`. But `field` can still be referenced from `t`, which is
itself passed to `free`. However, we are allowed to use parts of `t` twice if
these uses happen in different branches:

```ocaml
let okay t =
  match t with
  | Con { field } ->
    if flip_coin ()
    then free_field field
    else free t
```

or if the uses both accept `aliased` arguments (eg. by calling a function that
only stores its argument somewhere):

```ocaml
val store : t @ aliased -> unit

let okay t =
  match t with
  | Con { field } ->
    store_field field;
    store t
```

For more details, read [the reference](./reference.md).
