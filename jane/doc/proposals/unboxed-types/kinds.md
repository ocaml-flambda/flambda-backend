# Kinds

**Mostly superseded by the main [kinds documentation](../../extensions/kinds/index.md).**

This page describes a kind system for OCaml. Kinds are used to control
both runtime layout and the ability for certain values to *mode-cross*.
To learn more about layouts, see the [introduction to layouts](index.md).
This page assumes a passing familiarity with layouts, although it is not
necessary to read all of the details from the other page.

## Motivation: mode crossing

In a language without modes (such as [`local`](../../extensions/stack/intro.md)
or [`sync`](../modes/data-race-freedom.md)), classifying a type by its [layout](index.md) would
be enough. However, our experience with local types suggest that users will
enjoy the ability to control whether types can mode-cross, and kinds are
a natural fit for expressing this property of a type.

As an example, let's consider locals. Every expression is either `local` or `global`.
Though actually, it's slightly subtler: because `global` is a submode of `local`
(written `global <: local`), every expression is `local`, while some are additionally
`global`. A `global` expression has an advantage in that it is allowed to escape
from functions and be stored in the heap. On the other hand, expressions that
are `local` only lack these abilities. Such expressions can be allocated on a
stack instead of on the usual heap. We can consider all expressions to be `local`
because we can always safely forget that an expression is allowed to escape.

However, for some types, it is safe to say that any expression of that type is
actually `global`. The most natural example is `int`. Because `int`s are stored
directly -- an `int` is never allocated on the heap -- we can say that all
`int` expressions are `global`. This allows us to return `local` `int`s from
a non-`local` function, because `int`s are really always global.

At the time of writing (May 2023), *all* immediate types are always global.
We say that all immediate types *mode-cross* along the locality axis. However,
this is sometimes undesirable. For example, we might want a declaration like

```ocaml
val with_file : filename -> (local_ handle -> 'r) -> 'r
```

The `with_file` function opens a file, passes the file's handle to a continuation
function, and then closes the file when the called function is done. However,
it would be an error if that function stored the file handle somewhere for later
access -- the file would be closed by then. So the author of this function puts
a `local_` annotation to say that the `handle` should not be allowed to escape.

Yet the designer of this API may want to use an `int` as the underlying representation
of a `handle`. If no extra information is necessary to store, then we don't want
to have to allocate a `handle`; we want it to be immediate. And yet we do *not*
want it to mode-cross.

We thus wish to separate out the notion of "this is not a pointer" from
"this is allowed to mode-cross on the locality axis". This page describes
a mechanism to do this by allowing a type's kind to give its mode-crossing
information, along with its memory representation information. To wit, the
API above might include

```ocaml
type handle : value & always external = private int
```

This declaration says that `handle` has the `value` layout, is always
external (in other words, can mode-cross along the externality axis), but
says nothing about mode-crossing along the locality axis. Accordingly,
a `handle` cannot mode-cross from `local` to `global`.

One might wonder where `external` came from there. In the framework here,
the interesting quality of `immediate` is that it is `external`: it does not
need to be scanned by the garbage collector. Thus any `external` expression
can be optimized just like `immediate`s are today. Accordingly, the actual
definition of `immediate` is

```ocaml
kind immediate = 
  value & always external & always global & always unique & always many
```

This describes types that are stored like `value`s, never need to be scanned
by the GC, and can mode-cross across the locality, uniqueness, and linearity
modes.

Possible addition (not to be implemented for now): An alternative way
of writing the above could be

```ocaml
type handle : immediate with always local = private int
```

This approach takes an existing kind -- `immediate` -- and modifies it to drop the `always
global` descriptor. Using `always local` is a bit silly because everything is always
local, but we think the syntax works nicely as a counterpoint to `always global`.

Note that `with` here is meant to evoke record-update syntax (which
replaces a value), not module-type-update syntax (which adds
constraints to a type).

