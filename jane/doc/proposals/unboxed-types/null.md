# Adding null to types

This document proposes a way we can add a null value to types, thus granting a
non-allocating version of `option`. This proposal builds on the *layouts*
system described in the [main proposal for unboxed types](index.md). Before
reading this document, you may wish to read up through the
[layouts](index.md#layouts) section of the main proposal.

# Adding null

Now that layouts provide a way to segment the world of types, we can
leverage this feature to provide an `option` type -- dubbed `or_null`
-- that requires no allocation. This section describes how it all
works.

## The `non_null_value` layout

The key observation powering `or_null` is that ordinary OCaml values are
never equal to the word 0. A pointer will always be non-null (and thus different
from 0), and a tagged integer always has its bottom bit set. Because 0 can
never be a valid pointer, we can safely update the garbage collector not to
traverse null pointers.

We thus want `t or_null` to be just like `t`, except now with a meaning
for 0. There is still a small problem, though: we cannot ever have `t or_null
or_null`, as that gives *two* meanings for 0. We thus say that the argument
to `or_null` must be a *non-null type*; that is, it has room for 0. However, `t
or_null` is a *with-null type*; that is, it uses 0 in its representation.

Here is the layout structure:

```
              any
               |
             value
               |
         non_null_value
```

Note that `value` is above `non_null_value`. This is because anything
that can be done with a with-null type can be done with a non-null one, but
non-null types have an extra capability (they can be the argument to `or_null`);
this is thus the usual case where a subtype has an extra capability over the
supertype.

Supporting non-null immediates follows naturally: as explained more fully in the
page on [kinds](kinds.md), `immediate` is just `value` with the ability to
mode-cross, and so the little layout lattice above powers a kind like

```
kind non_null_immediate = 
  non_null_value & always global & always external & always unique & always many
```

Indeed, this definition will be part of the initial environment.

The layout `non_null_value` is *concrete*: we can compile
a function that manipulates `non_null_value`s.

Along with the kind-system changes above, the following type will be in the initial
environment:

```ocaml
type ('a : non_null_value) or_null : value
```

In addition, we will allow `'a or_null` to mode-cross the same way that
`'a` does -- effectively allowing `or_null` to work nicely with `immediate`s.
This can be implemented by "looking through" the
`or_null` type constructor during mode-cross-checking. (The alternative would be to
have some degree of kind polymorphism, where we have
`type ('a : non_null_value & 'modes) or_null : value & 'modes`
or similar, but looking through
`or_null` is easy enough so that we can avoid this extra complexity.)
This permits, e.g. `int or_null` to still be subject to
optimizations around immediates.

The `or_null` type has constructors `Some` and `None` (usable for both
construction and pattern-matching), subject to the usual type-based
disambiguation.

## The choice of meaning of `value`.

In the diagram above, the `value` layout includes types that contain null,
whereas the `non_null_value` layout is the one that includes all of today's
types. This choice -- to make a *new* layout have the short name -- derives
from our expectation that most annotations will prefer to denote the superlayout,
not the sublayout. Here is an example:

```ocaml
type ('element : value, 'index : immediate) flexible_array
```

where `flexible_array` can work with a non-`int` index type. Such a storage
structure likely does not care whether its elements support 0 or not. Yet, if we
used `value` to mean the layout without null, then it would be impossible to store
a `widget or_null` in the `flexible_array`. We thus believe that `or_null` will
be easier to use if users' `value` accepts null -- even
though our proposed `value` is distinct from the layout of legacy types.

Users can always write the `non_null`
versions if they choose to. Note that because of the way layout inference works,
the choice between these layouts matters only for abstract types. For example,
the following still works fine:

```ocaml
type ('a : value) pair_with_default = { contents : 'a or_null; default : 'a }
```

It works because the `: value` annotation gives only the upper bound, and thus
we actually infer `'a : non_null_value`, even though the user did not write that.

## Sub-typing

Because any valid `t` is also a valid `t or_null`, we have a natural sub-typing
relationship: `t` is a sub-type of `t or_null` for all `t` (such that `t
or_null` is well formed). Note that this relationship is entirely notionally
separate from the sub-layout relationships described above. Here, we're talking
about term-level conversions such as in

```ocaml
let f (x : int) = (x :> int or_null)
```

## Examples

### A safe, non-allocating `hd`

```ocaml
(* in list.mli *)
val hd' : 'a t -> 'a or_null
  (* we'll infer that ['a] has layout [non_null_value] *)

(* somewhere else *)
let f xs default = match List.hd' xs with
| None -> default
| Some x -> x
  (* we'll infer [f : ('a : non_null_value). 'a List.t -> 'a -> 'a] *)
```

Note that nothing changes about `List.t` itself here. This means that
a data structure can be designed to hold `value`s (that might be null) but that
individual functions over that structure can be specialized to work with
`non_null_value`s.