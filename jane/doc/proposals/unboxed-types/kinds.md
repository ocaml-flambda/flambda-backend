# Kinds

This page describes a kind system for OCaml. Kinds are used to control
both runtime layout and the ability for certain values to *mode-cross*.
To learn more about layouts, see the [introduction to layouts](index.md).
This page assumes a passing familiarity with layouts, although it is not
necessary to read all of the details from the other page.

## Motivation: mode crossing

In a language without modes (such as [`local`](../../extensions/local/intro.md)
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

## A kind is a set of descriptors

We now describe more formally how this all works.

A *kind* is a set of *descriptors*, where each descriptor specifies some
aspect of a type. A descriptor is one of the following:

* a layout
* a modality

For now, all modalities can be phrased as upper bounds on modes; they
are written as the keyword `always` followed by a mode.

Because there are many mode axes (such as locality and externality), there
can be many descriptors forming a kind.

All mode axes have a submode relation with a top mode. (All other modes in the
same axis are submodes of the top mode.) If a mode upper bound is omitted, we
assume the upper bound is the top mode for that axis.

A kind is written as a `&`-separated list of descriptors. Order does not
matter. It is an error if a kind specifies
more than one choice for a given *descriptor category*, where a descriptor
category is either *layout* or one of the mode axes. The layout of a kind
may never be omitted.

An optional alternative syntax allows `kind with descriptors`, where
the descriptors given override those in the kind.

According to this description, all layouts are automatically kinds (with
no mode upper bounds).

We also allow the use of a kind abbreviation in place of a descriptor; it
is expanded to mean the set of descriptors it abbreviates.

Here is the BNF syntax:

```
kind ::= descriptors
     |   kind `with` descriptors  (* optional extension; not for now *)

descriptors ::= descriptor
            |   descriptor '&' descriptors

descriptor ::= layout
           |   `always` mode
           |   [ module-path '.' ] kind-name

kind-name ::= lowercase-ident

mode ::= `local`
     |   `global`
     |   `internal`
     |   `external`
     |   `external64`
```

The BNF for layouts is defined in the [syntax](syntax.md) page.
We expect more modes to be introduced over time.

Because layouts can be used as kinds, layouts and kinds share a
namespace. It is thus an error for a structure to include a layout
and a kind of the same name.

## The externality mode axis

The [locality mode axis is well
described](../../extensions/local/intro.md). However, this page newly introduces
the *externality* axis. It is arranged like this:

```
external < external64 < internal
```

An *internal* value is one that the garbage collector must scan: it
may contain managed pointers. An *external* value is one that must not
contain managed pointers. An *external64* value must not contain managed
pointers on 64-bit machines, but is allowed to contain managed pointers
on other platforms.

The submoding relationship above works because a value either contains
managed pointers or not, and it is always safe to forget a restriction
on a value.

The compiler uses the externality axis to perform certain
optimizations, such as choosing not to scan external values. This is
the key property of immediates needed for efficient
optimization. (That is, without this notion of externality, the
compiler would not have a reason to track immediacy.)  An important
part of this design is to separate out the locality mode-crossing
feature of `immediate` from the `external`-based optimization.

## Subkinding

Kinds enjoy a subkinding relation which is simply the pointwise subsumption
relation on all descriptors. We can thus write `any` as the top kind, because
using a layout as a kind assumes no non-trivial upper bounds for any mode axis.

## Kind abbreviations

Because kinds can be complex, we introduce the ability to define kind abbreviations.
These can appear in both signatures and structures, and look like this:

```
specification +::= `kind` kind-name `=` kind

definition +::= `kind` kind-name `=` kind
```

Note that kind abbreviations may *not* be abstract; you must supply
the expansion in both the signature and the structure.

Because users can define kind abbreviations, it is thus allowed to
shadow the pre-defined kinds.

## Pre-defined kinds

The [introduction to layouts](index.md) defines a number of pre-defined
layouts. Here, we add some nuance to these definitions. Specifically,
we redefine `immediate` and `immediate64` as refinments on `value`:

```ocaml
kind immediate = value & always global & always external
kind immediate64 = value & always external64
```

## Kinds of user-defined types

Every user-defined type is assigned a kind, according to the following
algorithm. If a case does not describe the handling of a kind annotation in
the declaration (e.g. the `k` in type `t : k = ...`), then it is checked as
an upper bound of the kind given by the algorithm here.

There are two classes of modality: *inferred* modalities and *requested*
modalities. Inferred modalities are ones aggressively inferred on type
declarations whenever possible. Requested ones are added to new type declarations
only by request (except on non-`private` abbreviations, which are expanded during
type-checking). Right now, we have this breakdown:

* Inferred modalities: `always external`, `always external64`
* Requested modalities: `always global`

The logic here is that externality is really about powering an optimization.
Users generally don't really care whether a type is external; they just want
the compiler to perform the optimization when it is safe to do so. On the other
hand, locality can affect the correctness properties of an API (for example, around
scoped file handles). Accordingly, if a user wants a new type to be `always global`,
they must ask for it.

This is slightly backward-incompatible with the treatment that preceded modal kinds: in
the initial implementation of locals, `type t = private int` has an upper bound of
`global` along the locality axis, whereas the design described here assigns no upper bound
(that is, `always local`).

(Looking forward, we expect `always sync` to be inferred while `always many` and
`always unique` will be requested.)

Recall here that all layouts can also be treated as kinds, but with no mode
upper bounds.

* A fully abstract type (a definition with no `=` signs) is assigned the
kind written in a kind annotation. If there is no kind annotation, the abstract
type gets kind `value`.

* A boxed record type gets the kind `value`.

    * If all fields are `always global`, the record can be annotated with `always global`.
    (Otherwise, doing so is an error.)

    * If all fields have layout `void`, the record will be inferred to be `always external`.

* A record type declared with `#{ ... }` gets the kind
`lay1 * ... * lay_n`, where `lay1 ... lay_n` are
the layouts of the types of the fields of the record, in the order of appearance
in the declaration. In addition, if all the fields' types are `always m` (for any
mode `m`), then the record may also be `always m` (either inferred or requested by annotation).

* A record declared with `[@@unboxed]` gets the same layout as its field. It inherits
all inferred modalities and may be annotated with requested modalities, according to
the modalities on its field.

* A variant type with all constant constructors gets the kind `value & always external`.
A constructor is constant if all of its fields have layout `void`. (This statement
is also true of constructors with no fields.) It may also be annotated `always global`
to get that modality by request.

* A variant type with any non-constant constructors gets the kind `value`.

* A variant type declared with `#( ... )` gets the kind
`lay1 + ... + lay_n`, where `lay_i` is the product
of the layouts of the fields of the `i`th constructor.
In addition, if all the constructors' fields' types are `always m` (for any
mode `m`), then the variant may also be `always m` (either inferred or requested
by annotation).

* An extensible variant has kind `value`.

* Regardless of the shape of the type declaration, any type variables
get their kinds inferred from usage sites. An unconstrained layout
on a variable defaults to `value`, while an unconstrained mode descriptor
defaults to be the appropriate top mode.

## Kinds of types in the type algebra

Types formed from the type algebra are also assigned kinds, according to
the following algorithm.

* All type variables have a kind. As described more on the [inference page](inference.md),
flexible type variables infer their kinds, while rigid type variables must be assigned
a kind when they are brought into scope. A rigid type variable brought into scope
without a kind annotation defaults to kind `value`.

* A function type `ty1 -> ty2` has kind `value` without constraining the kinds of
either `ty1` or `ty2`.

* A tuple `ty1 * ... * tyn` (where n >= 2) has kind `value` without constraining the
kinds of any of the `tyi`.

* An unboxed tuple `#( ty1 * ... * tyn )` (where n >= 2) has a layout `lay1 * ... * lay_n`,
where each `lay_i` is the layout of type `tyi`. Furthermore, for any mode `m`, if all of
the `tyi` are `always m`, then the unboxed tuple is `always m` as well.  This is the same
as the rule for unboxed records.

* An applied type constructor `(ty1, ..., tyn) t` (where n >= 0) has the kind assigned to `t` at its
declaration. Each of the `tyi` is constrained to be a subkind of the kind on the corresponding
type parameter to `t`.

* An object type `< f1 : ty1; ...; fn : tyn >` has kind `value`. Each `tyi` is constrained to
have a kind that is a subkind of `value`.

* A polymorphic variant type ``[ `K1 of tys1 | ... | `Kn of tysn ]``, where all
constructors are constant constructors, has kind `immediate`. As above, a constant
constructor is one where all of its fields have layout `void`. A polymorphic variant
with at least one non-constant constructor has kind `value`. No constraint is put on
the kinds of any of the types.

* A polymorphic type `'a. ty` has the kind of the inner `ty`.

* A first-class module type `(module S)` has kind `value`.

* `ref`, `array` and `lazy_t` are all primitive types. They have kind `value`.
Their type arguments have kind `value` for now, but we expect this to relax
as we develop more features.

## Kind checking

We must make sure that types are well-kinded. This is straightforward:
we require that each type variable is instantiated with a type whose kind
is a subkind of the kind of the type variable, and that each type abbreviation's
right-hand side is a subkinds of the kind declared on the abbreviation.

When computing whether one module type is a subtype of another, we use
the subkinding relation on user-defined types. For now, we require that
the kind on each type parameter to a user-defined type to equal the
corresponding kind in the other signature, but this may be relaxed to allow
contravariance of type parameters (which should be sound).

Inference of kinds is described on the page on [inference](inference.md).

## Open questions

Should we allow `value; always global; always external` or should we require
`value with always global; always external`? That is, we could say that the
descriptors can only be after a `with`. This makes is more canonical, but makes
it slightly awkward if we ever want to update the layout descriptor of a kind
(because most people will think that only modes can go after the `with`).

## Design notes

Simple cases must be simple.
Do we need abstract modes? Maybe. But not today.
We probably will with mode polymorphism.
So the design should be extensible to abstract modes.
This will mean we have to name the axes. In a new "mode axis" namespace. Urgh.
No abstract mode axes! You can't even rename them. They are utterly fixed.
We need `with` syntax.
We definitely want abstract layouts.
Do we want abstract / redefinable kinds? I'm sure we do.
Kinds and layouts will share a namespace
  So you can say blah : value or blah : bits32 * bits32.
Or equivalently: each layout is also a kind, where all the modal fields
  are boring.
Is * an operator on kinds, or on layouts? I think: layouts.
Actually, maybe we allow only kind abbreviations for now, while
  we allow proper abstract layouts. That seems like a reasonable first step.
We have `k with ...` syntax to update bits of a kind.

Of the initial layouts we've imagined:
* value is still a layout: the GC treats values differently from other layouts.
* immediate64 is now a kind: immediate64 = value; always external64
* immediate is now a kind: immediate = value; always global; always external.
* the others are all still layouts

Use ; or ,? It's kind-of a tuple, but it's unordered. And it uses `with`
syntax. It really should be ;.

Use braces? Nah.