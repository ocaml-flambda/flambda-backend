# The kind system

Oxcaml includes a system of _kinds_, which classify types. Roughly, the kind
system adds an extra level to the type system: terms are classified by types,
and types are classified by kinds. Kinds capture properties of types along
several different dimensions. For example, kinds can be used to identify which
types have values that are passed in floating point registers, or are safely
ignored by the garbage collector.

The kind of a type has three components: the _layout_, the _modal bounds_, and
the _non-modal bounds_. The layout describes the shape of the data at runtime,
and is used to support unboxed types. The modal bounds describe how different
types interact with our mode system. In particular, some types don't have
interesting interactions with some modes, so values of these types can safely
ignore those modal axes. The non-modal bounds capture a grab-bag of other
properties.

Kinds are related by a _sub-kinding_ relation, described in more detail
below. This allows to use a type with a more precise kind where a type with a
less precise kind is expected.

This page describes the kind system at a high level, and contains complete
details for the non-modal bounds. It does not exhaustively describe the possible
layouts (which are documented on the [unboxed types
page](unboxed-types/index.md)) or the modal axes (which are documented on the
[modes page]()), but does explain how those components appear in kinds.

CR ccasinghino: add links to modes documentation after moving it here.

# The basic structure of kinds

Basic kinds have the form:

```
<layout> mod <bounds>
```

For example, the type `(int -> int) option` has the kind `value mod
contended`. Here, the layout `value` indicates that members of this type have
the shape of normal OCaml data, and the bound `contended` indicates that they
can safely ignore the "contention" access (it is safe to treat them as if they
were created by the current thread, even if they were not).

The "bounds" portion of the kind often has multiple components, and includes
both modal and non-modal bounds. For example, the type `int -> int` has a kind
with two modal bounds, `value mod aliased contended`.  The type `int` has all
the bounds:

```
value mod external_ global contended portable aliased many unyielding
```

This kind indicates that `int` mode crosses on all six of our modal axes. The
non-modal bound `external_` captures the property that `int`s can safely be
ignored by the OCaml garbage collector.

# The meaning of kinds

In additional to `value`, Oxcaml supports layouts like `float64` (unboxed
floating point numbers that are passed in SIMD registers), `bits64`
and `bits32` (for types represented by unboxed/untagged integers) and product
layouts like `float64 & bits32` (an unboxed pair that is passed in two
registers). More detail on layouts and the unboxed types language feature can be
found [here](unboxed-types/index.md).

Modal bounds all correspond to modal axes, which are described in more detail in
the [modes documentation](). The logic for which types can cross on which axes is
specific to each axis, often involving both the semantic meaning of the mode and
details of the implementation of related features in the OCaml runtime. See the
documentation for each mode to understand which types cross on its axis.

Formally, these are called modal _bounds_ because the represent upper or lower
bounds on the appropriate modal axes. For _future_ modal axes (like portability
and linearity), the kind records an upper bound on the mode of values of this
type. For example, `int` is `mod portable` because if you have an `int` that is
`nonportable`, it's safe to treat it as `portable`.  For _past_ modal axes
(like contention and uniqueness), the kind records a lower bound on
_expectations_.  For example, `int` is `mod contended` because in a place where
an `uncontended` value is expected, it's still safe to use a `contended` int.

Why do past and future modal axes get different treatment in kinds? This is
covered in the "Advanced Topics" section below, but isn't essential to
understand for day-to-day use of the system.

# Subkinding

There is a partial order on kinds, which we'll write `k1 <= k2`. The
relationship `k1 <= k2` holds when `k2` tells us less about a type than `k1`.
Thus, it is always safe to use a type of kind `k1` where a type of kind `k2` is
expected.  There is a maximum kind, written `any` (which, somewhat confusingly,
is also the name of the maximum layout).

As an example, `value mod portable <= value`. This means that if we know a type
is represented as a normal OCaml value and mode crosses on the portability axis,
it's safe to use the type where we just need a regular OCaml type but don't care
how it interacts with portability. This relation forms a partial order because
some kinds are unrelated, like `float64 mod contended` and `bits64 mod
contended`, or `value mod portable` and `value mod aliased`.

If you want to get nerdy about it, each individual piece of a kind (the layout
and each possible axis of bounds) is a join semilattice, and the order we're
talking about here is the one corresponding to the product join semilattice that
combines all these components. The order for each component is described in the
documentation for that component.

# Kinds in the syntax

You can write kind annotations in several places. In type declarations, they can appear
both on the type being declared itself, and on the type parameters.  E.g.,:
```ocaml
type ('a : <kind1>) t : <kind2>
```
Kind annotations can also appear anywhere a type variable or underscore is legal
in a type expression, as in:
```ocaml
let foo (x : ('a : value mod portable)) (y : (_ : bits32 & float64)) = ...
```
And on locally abstract types:
```ocaml
let foo (type (a : value mod portable) (b : bits32 & float64)) (x : a) (y : b)
  = ...
```

It's legal to omit the `mod ...` portion of the kind when you only want to
specify the layout (as we did in a couple of these examples). The bounds of
unspecified axes are the maximum on those axes.

What is the maximum on each axis? It's the bound that tells us the least about
the type. Note that for the "past" modes, this order is reversed from the order
on the mode itself (that is, `value mod contended <= value mod uncontended`).
This is due to the meaning of these kinds as covered
[above](#the-meaning-of-kinds).

Note that the meaning of a kind annotation on a type declaration differs
depending on whether the type declaration is abstract.  For an abstract type declaration
like `type t : value mod portable`, the kind annotation will be used as the
exact kind of the type `t`.

For a type declaration that has a RHS, the kind annotation is just a check. For
example, consider this type declaration:
```ocaml
type t : value = (int -> int) option
```
This declaration is allowed because `(int -> int) option` does have the kind
value (due to subkinding, see the previous section).  But the type system treats
`t` and `(int -> int) option` as complete equal types, and therefore still knows
the more precise kind `value mod contended` for `t` despite the annotation.

On the other hand, the following declaration is rejected because the kind of
`(int -> int) option` is not less than or equal to `value mod portable`:
```
type t : value mod portable = (int -> int) option
```

## Kind abbreviations

Kinds can get long, so we have built-in abbreviations for several common kinds,
and there is a mechanism to define aliases of your own.  The builtin
abbreviations include kinds like "immediate", which is secretly shorthand for
the kind we showed for `int` above.  Here is a list of the built-in abbreviations:

| Alias            | Meaning                                                                  |
|------------------|--------------------------------------------------------------------------|
| `immediate`      | `value mod external_ global contended portable aliased many unyielding`  |
| `immediate64`    | `value mod external64 global contended portable aliased many unyielding` |
| `immutable_data` | `value mod contended portable many unyielding`                           |
| `mutable_data`   | `value mod portable many unyielding`                                     |

It's allowed to extend an alias with an additional bounds.  For example,
`mutable_data mod contended` is equivalent `immutable_data`.

User-defined kind abbreviations may be added with `kind_abbrev_`, which is legal
in structures and signatures.  For example, we could have this alias:
```ocaml
kind_abbrev_ portable_value_pair = (value & value) mod portable
```

# Inclusion and variance

This is accepted:

```ocaml
module M1 : sig
  type t : value
end = struct
  type t = int
end
```

This makes sense because the kind of `int` is `immediate`, which is a subkind
of `value`. Even though users of `M1.t` will be expecting a `value`, the `immediate`
they get works great. Thus, the kinds of type declarations are *covariant* in the module
inclusion check: a module type `S1` is included in `S2` when the kind of a type `t`
in `S1` is included in the kind of `t` in `S2`.

Similarly, this is accepted:

```ocaml
module M2 : sig
  type ('a : immediate) t
end = struct
  type ('a : value) t
end
```

This makes sense because users of `M2.t` are required to supply an `immediate`; even
though the definition of `M2.t` expects a `value`, the `immediate` it gets works great.
Thus, the kinds of type declaration arguments are *contravariant* in the module
inclusion check: a module type `S1` is included in `S2` when the kind of the argument
to a type `t` in `S2` is included in the kind of that argument in `S1`.

Contravariance in type arguments allows us to have

```ocaml
module Array : sig
  type ('a : any) t = 'a array
  (* ... *)
end
```

and still pass `Array` to functors expecting a `type 'a t`, which assumes `('a :
value)`.

Relatedly, a `with type` constraint on a signature can fill in a type with one
that has a more specific kind.  E.g., this is legal:
```ocaml
module type S_any = sig
  type t : any
end

module type S_imm = S_any with type t = int
```

This can be particularly useful for common signatures that might be implemented
by types with any kind (e.g., `Sexpable`).

# With kinds

Sometimes the kind of a type constructor depends on the kinds of the types that
instantiate its parameters.  For example, the type `'a list` can mode cross on
the portability axis if `'a` does.

We could have a `list` whose kind is restricted to work on types that more cross
on the portability axis, to record this fact, as in:
```ocaml
type ('a : value mod portable) portable_list : value mod portable
```
But it would be annoying to have many different list types, and we certainly
don't want to restrict the normal `list` type to work only on a subset of
`value`s.

The solution to this problem is "with kinds": kinds that record dependencies on
types.  The actual kind of list in our.
```ocaml
type 'a list : value mod contended portable many with 'a
```

CR ccasinghino: I'm giving up on explaining with-kinds here for now, mainly
because I'm not familiar enough with the the latest version of the syntax.

# Non-modal kind axes

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

The syntax for this axis is special, mainly to keep the system backwards
compatible. The nullability bound is not written as part of the bounds that
follow `mod`, but rather is implicit in the part of the kind that comes before
`mod`. In particular, `value` by itself really means the thing you would
otherwise write as `value mod non_null`. This reflects the fact that, for all
classic OCaml types, `NULL` is not a possible value.

The kind of values with `NULL` added as a possibility is written
`value_or_null`.

Types that don't have `NULL` as a possible value are
compatible with `or_null`, a non-allocating option type that is built into
Oxcaml.  Its definition is:
```ocaml
type ('a : value) or_null : value_or_null =
  | Null
  | This of 'a
```

Like value, the syntax for the kind `any` implicitly adds a `mod non_null`
bound. If you want the `any` kind without this bound, you can write
`any_or_null`.

# Advanced topics

## Why are past and future modal bounds different?

Historically, we thought of modal bounds in kinds as being upper bounds on the
mode of values of the type. This turns out to be the correct meaning only for
the future axes. For past axes, modal bounds in kinds are instead _lower bounds_
on _expected modes_.

This distinction only matters for modal axes with at least three values.
Consider the contention axis, where `uncontended < shared < contended`.  What
does a kind like `value mod shared` mean?

Our answer is that it's the kind of a type like:
```ocaml
type 'a t = { shared : 'a @@ shared } [@@unboxed]
```
That is, the meaning of `value mod shared` is related to types using `shared` as
a modality.

The type `t` is a "box" containing something we know is shared. How do you use
this type? In short, this is a box that knows that its contents are `shared`,
even when the box itself is `uncontended`. If we have a `t @ uncontended`, the
mode of `t.shared` is `shared`. But if we have a `t @ contended`, the mode of
`t.shared` must still be `contended` - this `t` may have come from another
thread, and for thread safety its contents must now be treated as contended
regardless of their mode when they went into the box.

So, for this type, we don't care about the difference between `uncontended` and
`shared`, but we do care about the difference between `shared` and `contended`.
If we thought of `mod shared` as representing a upper bound on the mode, that
would suggest we can treat `contended` things as `shared`, which we've just seen
is wrong.

Instead, for past axes, a modal bound is _lower bound_  on expectations: `mod
shared` means that even if you're expecting to get an `uncontended` thing of
this type, a `shared` thing is just as good.

## Kind inference

CR ccasinghino: I'm imagining the "with kinds" section above to be a practical
introduction to why we have with kinds and how to write them. It probably should
not cover the details of how we compute the kind of type. Is it worth explaining
that down here?
