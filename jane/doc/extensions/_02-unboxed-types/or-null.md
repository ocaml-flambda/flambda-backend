---
layout: documentation-page
collectionName: Unboxed types
title: Or null
---

# Adding null to types

This document describes the way we can add a null value to types, thus granting a
non-allocating version of `option`. This extension builds on the *layouts*
system described in the [main document for unboxed types](../intro). Before
reading this document, you may wish to read up through the
[layouts](../intro#layouts) section of the main document.

# Adding null

Now that layouts provide a way to segment the world of types, we can
leverage this feature to provide an `option` type -- dubbed `or_null`
-- that requires no allocation. This section describes how it all
works.

## The `value_or_null` layout

The key observation powering `or_null` is that no ordinary OCaml value equals the
word 0. A valid pointer will always be non-null, and an immediate (represented
as a tagged integer) will always have its bottom bit set. Thus, all are
different from 0. Therefore, we can safely update the garbage collector not to
traverse null pointers.

We thus want `t or_null` to be just like `t`, except now with a meaning
for 0. There is still a small problem, though: we cannot ever have `t or_null
or_null`, as that gives *two* meanings for 0. We thus say that the argument
to `or_null` must be a *non-null type*; that is, it has room for 0. However, `t
or_null` is a *maybe-null type*; that is, it uses 0 in its representation.

We call the legacy OCaml non-null layout `value` and the new maybe-null layout
`value_or_null`.

Here is the layout structure:

```
              any
               |
         value_or_null
               |
             value
```

Note that `value_or_null` is above `value`. This is because anything
that can be done with a maybe-null type can be done with a non-null one, but
non-null types have an extra capability (they can be the argument to `or_null`);
this is thus the usual case where a subtype has an extra capability over the
supertype.

The layout `value_or_null` is *concrete*: we can compile
a function that manipulates `value_or_null`s.

Along with the kind-system changes above, the following type will be in the initial
environment:

```ocaml
type ('a : value) or_null : value_or_null
```

The with-kinds feature allows `'a or_null` to mode-cross the same way that
`'a` does -- this permits, e.g. `int or_null` to still be subject to
optimizations around immediates.

The `or_null` type has constructors `This` and `Null` (usable for both
construction and pattern-matching), subject to the usual type-based
disambiguation.

## Defaults

In the diagram above, the `value_or_null` layout includes types that do contain
null, whereas the `value` layout is the one that includes all of today's
types, and is the default for abstract types and type variables. Why?

Whatever default we choose for abstract types will also
determine the default for type variables due to backwards compatibility.
Choosing non-null would mean that all pre-existing OCaml types are valid
parameters for `'a or_null`, whereas choosing maybe-null allows `or_null`
to be a type argument.

Most types in OCaml programs lack arguments, and those
that do are typically container types like `'a list`, which we can modify
preemptively. Thus, we default abstract types to non-null to reduce the
annotation burden on programmers.

## Arrays

The float array optimization demands that all types in OCaml are *separable*:
either all elements of that type are float values, or none of them are. But
`float or_null` is not separable: as possible values, it has all floating-point
numbers, plus the null pointer. Therefore, `float or_null array`s must be forbidden.

This can be done by either declaring `float` to be with-null or by requiring array
elements to be non-null. Since non-null is the default, the former would break legacy
code, so we choose the latter. We introduce the `any_non_null` layout for array elements.

## Examples

### A safe, non-allocating `hd`

```ocaml
(* in list.ml *)
let hd' = function
  | [] -> Null
  | x :: _ -> This x
;;

(* in list.mli *)
val hd' : 'a t -> 'a or_null
(* ['a] has layout [value] by default*)

(* somewhere else *)
let f xs default = match List.hd' xs with
  | Null -> default
  | This x -> x
;;
(* we'll infer [f : 'a list -> 'a -> 'a] *)
```

Note that `'a list` can be modified to hold `or_null` values, and the specialized
`hd'` function will still work.
