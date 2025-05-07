---
layout: documentation-page
collectionName: Kinds
title: Intro
---

# The kind system

OxCaml includes a system of _kinds_, which classify types. Roughly, the kind
system adds an extra level to the type system: terms are classified by types,
and types are classified by kinds. This page provides an overview of the kind
system, but several other pages flesh out the details:

* [Syntax for kind annotations](../syntax)
* [The non-modal bounds](../non-modal)
* [How to compute the kinds of a type](../types)

Kinds capture properties of types along
several different dimensions. For example, kinds can be used to identify which
types have values that are passed in floating point registers, or are safely
ignored by the garbage collector.

The kind of a type has four components: the _layout_, the _modal bounds_, the
_with-bounds_, and the _non-modal bounds_. The layout describes the shape of the
data at runtime, and is used to support unboxed types. The modal bounds describe
how different types interact with our mode system. In particular, some types
don't have interesting interactions with some modes, so values of these types
can safely ignore those modal axes. However, container types (among other
parameterized types) have modal bounds that depend on the bounds of the element
type; this dependency is captured in the with-bounds. The non-modal bounds
capture a grab-bag of other properties.

Kinds are related by a _sub-kinding_ relation, described in more detail
below. This allows to use a type with a more precise kind where a type with a
less precise kind is expected.

This page describes the kind system at a high level, and contains complete
details for the non-modal bounds. It does not exhaustively describe the possible
layouts (which are documented on the [unboxed types
page](../unboxed-types/index)) or the modal axes (which are documented on the
[modes page](../modes/intro)), but does explain how those components appear in
kinds, including how the modal bounds are affected by the with-bounds.

CR ccasinghino: add links to modes documentation after moving it here.

CR reisenberg: Where should/do we document mode crossing? There is some text
in proposals/unboxed-types/kinds.md that might be useful.

# The basic structure of kinds

Basic kinds have the form:

```
<layout> mod <bounds>
```

For example, the type `(int -> int) option` has the kind `value mod contended
immutable non_float`. Here, the layout `value` indicates that members of this
type have the shape of normal OCaml data, and the
bounds `contended immutable` indicates that they can safely ignore the
*contention* (any value of this type can be treated as if it were created in the
current thread) and *visibility* axes (no part of this type is mutable, and so
read/write protections do not matter). The `non_float` bound says that no value
that has type `(int -> int) option` is a pointer to a floating-point number
block.

As we see in this example, the *bounds* portion of the kind often has multiple
components, and includes both modal and non-modal bounds. The type `int` has all
the bounds:

```
value mod global contended portable aliased many unyielding immutable stateless
          non_float external_
```

This kind indicates that `int` mode crosses on all eight of our modal axes. In
addition, `int`s are not `float`s and they do not need to be garbage-collected
(they are `external_` to the garbage collector).

# The meaning of kinds

In additional to `value`, OxCaml supports layouts like `float64` (unboxed
floating point numbers that are passed in SIMD registers), `bits64`
and `bits32` (for types represented by unboxed/untagged integers) and product
layouts like `float64 & bits32` (an unboxed pair that is passed in two
registers). More detail on layouts and the unboxed types language feature can be
found [here](../unboxed-types/index).

Modal bounds all correspond to modal axes, which are described in more detail in
the [modes documentation](../modes/intro). The logic for which types can cross
on which axes is specific to each axis, often involving both the semantic
meaning of the mode and details of the implementation of related features in the
OxCaml runtime. See the documentation for each mode to understand which types
cross on its axis.

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

The non-modal bounds encode several different properties; see the section
"Non-modal bounds" below. They are called bounds because each non-modal axis
still supports a sub-kinding relationship, where a type of a more specific kind
can be used in place of a variable of a less specific kind.

# Subkinding

There is a partial order on kinds, which we'll write `k1 <= k2`. The
relationship `k1 <= k2` holds when `k2` tells us less about a type than `k1`.
Thus, it is always safe to use a type of kind `k1` where a type of kind `k2` is
expected.  There is a maximum kind, written `any`: this is the maximum layout
but with no bounds.

As an example, `value mod portable <= value`. This means that if we know a type
is represented as a normal OCaml value and mode crosses on the portability axis,
it's safe to use the type where we just need a regular OCaml type but don't care
how it interacts with portability. This relation forms a partial order because
some kinds are unrelated, like `float64 mod contended` and `bits64 mod
contended`, or `value mod portable` and `value mod aliased`.

Adding bounds to a kind always makes the kind more specific, or lower. That is,
for any kind `k`, `k mod <bounds> <= k`.

Along the future modal axes, a lower mode leads to a lower kind. So `value mod
stateless <= value mod observing`, and bounding by the maximum mode has no
effect. However, along the past modal axes, a _higher_ mode leads to a lower
kind. So `value mod contended <= value mod sharing` and bounding by the minimum
mode has no effect. We can think of the past axes as flipped, when used in a
kind. This is because `value mod contended` is more restrictive than `value mod
sharing` (the former contains types that do not care at all about the value of
the contention axis, while the latter contains types that still care about the
distinction between `contended` and `sharing`/`uncontended`), and so we must
flip these past axes (somewhat unfortunately).

If you want to get nerdy about it, each individual piece of a kind (the layout
and each possible axis of bounds) is a join semilattice, and the order we're
talking about here is the one corresponding to the product join semilattice that
combines all these components. The order for each component is described in the
documentation for that component.

# Inclusion and variance

This is accepted:

```ocaml
module M1 : sig
  type t : value
end = struct
  type t = int
end
```

This makes sense because the kind of `int` is `immediate`, which is a subkind of
`value`. Even though users of `M1.t` will be expecting a `value`, the
`immediate` they get works great. Thus, the kinds of type declarations are
*covariant* in the module inclusion check: a module type `S1` is included in
`S2` when the kind of a type `t` in `S1` is included in the kind of `t` in `S2`.

Similarly, this is accepted:

```ocaml
module M2 : sig
  type ('a : immediate) t
end = struct
  type ('a : value) t
end
```

This makes sense because users of `M2.t` are required to supply an `immediate`;
even though the definition of `M2.t` expects a `value`, the `immediate` it gets
works great.  Thus, the kinds of type declaration arguments are *contravariant*
in the module inclusion check: a module type `S1` is included in `S2` when the
kind of the argument to a type `t` in `S2` is included in the kind of that
argument in `S1`.

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
that has a more specific kind. For example, this is legal:
```ocaml
module type S_any = sig
  type t : any
end

module type S_imm = S_any with type t = int
```

This can be particularly useful for common signatures that might be implemented
by types with any kind (e.g., `Sexpable`).

# With-bounds

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

The solution to this problem is the with-bounds: a with-bound in a kind of a
type makes that type not mode-cross whenever the with-bound also does not
mode-cross. Add a bound to the `mod` section always *lowers* a kind, while
adding a with-bound always *raises* a kind.

Here is the full kind of `'a list`:

```ocaml
type 'a list
  : value mod contended portable many immutable stateless unyielding with 'a
```

also written (see our [syntax page](../syntax) for details on `immutable_data`)

```ocaml
type 'a list : immutable_data with 'a
```

We can think of this as saying that the `list` type itself is safe to cross all
those axes, but still contains data of type `'a`. Because modes are deep,
allowing `'a list` to mode-cross just because the `list` structure itself can
mode-cross would be wrong: the elements would cross along with the list! We thus
state in the with-bounds that `'a list` contains `'a` -- that's the intuition
behind the `with` syntax.

Looking at examples of `list`, we would have `int list : immutable_data`
(because `int` mode-crosses everything) but `(int -> int) ref list : value`,
because `(int -> int) ref` mode-crosses nothing.

## Modalities in with-bounds

Fields in a record or constructor can contain *modalities*, as described in our
[modes documentation](../modes/intro). To get maximal mode-crossing, these
modalities need to be reflected in the with-bounds as well. For example, we can
have

```ocaml
type 'a portended = Portend of 'a @@ portable contended
```

The kind of this is `immutable_data with 'a @@ portable contended`. The
modalities in the with-bound indicate that we know `'a` must be portable and
contended, and thus it does not matter whether the type substituted for `'a`
crosses these modes. So, for example, `int ref portended` still crosses to
`contended`, because the `@@ contended` protects the `with int ref @@ portable
contended` from affecting the contention axis.

## Kind equivalence

Now that we have with-bounds, we can see that there is a rich equivalence
relation on kinds. For example, `value mod portable with int` is equivalent to
`value mod portable`, because `int` mode-crosses portability. Similarly, `value
mod portable with (int -> int) @@ portable` is also equivalent to `value mod
portable`, because the `@@ portable` says that the with-bound cannot affect
portability. Another example is that `value mod portable with (int -> int)` is
equivalent to `value`, because `int -> int` does *not* cross portability.

The OxCaml compiler aggressively *normalizes* kinds to find a minimal kind that
is equivalent to an original one. This normalization procedure is also used
during kind-checking to tell whether one kind is a subkind of another.

An interesting case in normalization is around abstract types. If we have `type
'a abstract` and we have a kind `value mod portable with (int -> int) abstract`,
can we normalize to `value`? We cannot know without knowing what `abstract`
might become. Perhaps we substitute it for a type that ignores its type
argument; then `value mod portable with (int -> int) abstract` is equivalent to
`value mod portable`. On the other hand, perhaps we substitute it for a type
that stores its argument. Then `value mod portable with (int -> int) abstract`
is equivalent to `value`. Accordingly, you might see abstract types appear in
with-bounds, because we are unable to normalize them away until we actually
learn what type is being used.

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

## Existential types in with-bounds

Consider the type

```ocaml
type t = K : 'a * ('a -> int) -> t
```

What should the kind of this type be? Naively, we just put all the fields
in a type in its with-bounds, meaning we'd infer

```ocaml
type t : immutable_data with 'a with ('a -> int)
```

But this would be bad: that `'a` is not in scope. Instead, we recognize that
we will never learn more about `'a` (e.g. by substitutions), and thus that
the kind of `t` should just consider `'a` to be any old `value`. In this case,
then, we can produce the kind `value` for t.

However, what about this case:

```ocaml
type 'a abstract
type t = K : 'a abstract -> t
```

Now what can we do? We want to say that `'a` is like any old `value`, but what
concretely does that look like? Our solution is to invent a way to say "any old
`value`", which we write `(type : value)`. So the kind of this `t` is
`immutable_data with (type : value) abstract`. This way, if we later learn that
`abstract` ignores its argument, we can get a kind of `immutable_data`.  If
`abstract` stores its argument, we can get a kind of `value`.

The type `(type : <<kind>>)` can actually be used anywhere a type can be
written, but the type is uninhabited and useful only in the context of a
with-bound.
