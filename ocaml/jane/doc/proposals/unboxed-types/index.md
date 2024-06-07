# Unboxed types

## Preamble

This document describes an upcoming feature we (the compilers team) are
planning. This is all subject to change as we gain experience during
implementation. The features described here will become available incrementally;
this proposal paints only the final picture. We are still working out a more
detailed implementation plan, which will be posted and linked in due course.

This document presents the core parts of the proposal; other aspects are
discussed in these companion documents. Read this main proposal
first.

* [Kinds](kinds.md)
* [Abstract layouts](abstract.md)
* [Surface syntax of unboxed types](syntax.md)
* [Adding null to types](null.md)
* [Type inference concerns](inference.md)
* [Extensions, alternatives, and bad ideas](extensions.md)

Parts of this proposal have been upstreamed to be discussed by the wider OCaml community,
as an [OCaml RFC](https://github.com/ocaml/RFCs/pull/34).

OCaml currently has two attributes that this proposal may easily be confused
with:

* We can mark arguments to external calls as `[@unboxed]`, as [documented in the
  manual](https://v2.ocaml.org/manual/intfc.html#s%3AC-cheaper-call). This
  proposal essentially takes this idea and expands it to be usable within OCaml
  itself, instead of just at the foreign function interface boundary.
* We can mark some types as `[@@unboxed]`, as briefly described in a bullet
  point in [this manual
  section](https://v2.ocaml.org/manual/attributes.html#ss:builtin-attributes).
  This attribute, applicable to a type holding one field of payload (either a
  one-field record or a one-constructor variant with one field), means that no
  extra box is allocated when building values of that type. Declaring an
  `[@@unboxed]` type is equivalent to declaring an unboxed record or variant
  with one field. See the sections on unboxed records and variants, below.

## Motivation

Suppose you define this type in OCaml:

```ocaml
type t = { x : Int32.t; y : Int32.t }
```

Values of this type carry 8 bytes of actual data. However, in OCaml,
values of this type are represented as:

  - an 8-byte pointer

  - to a 24-byte block
    (8B header + 2x 8B fields)

  - pointing at two more 24-byte blocks
    (8B header + 8B `caml_int32_ops` + 4B data + 4B padding)

There are some efficiency gains to be had here.

## Principles

In this design, we are guided by the following high-level principles.
These principles are not incontrovertible, but any design that fails
to uphold these principles should be considered carefully.

**Backward compatibility**: The extension does not change the
correctness or performance of existing programs.

**Availability**: Unboxed types should be easy to use. That is,
it should not take much change to code to use unboxed types and
values.

## Unboxed primitives

This is a proposed extension to OCaml to allow the layout of data
types to be controlled by the programmer. This proposal won't affect
the type defined above, but will allow you to define better layouts.
With unboxed types, we can write the following:

```ocaml
type t = { x : int32#; y : int32# }
```

Here, the `x` and `y` fields have the unboxed type `int32#`, which
consumes only 4 bytes. Values of type `t` are still boxed: they are
represented as 8-byte pointers to a 16-byte block, consisting of an
8-byte header and two 4-byte fields. Despite containing unboxed
fields, the type t is an ordinary OCaml type: it can be passed to
normal functions, stored in normal data structures, and so on.

On the other hand, the new type `int32#` is unboxed. This means that
it can't be used to instantiate ordinary type parameters.  For
example, `fun (x : int32#) -> Fn.id x` will not work: `Fn.id` doesn't
work on types that are not represented by exactly one word. Similarly,
you can't have a `int32# list`, because a list expects its contents to
be represented by exactly one word. Getting this right is important
for, e.g., the garbage collector, which expects values in memory to
have a certain format.

Unboxed types, thus, require a trade-off: they are more compact,
but do not work as smoothly as normal boxed types.

The following types will be available in the initial environment, given with
example literals of that type:

```ocaml
#()  : unit#
#42b : int8#
#42s : int16#
#42l : int32#
#42L : int64#
#42n : nativeint#
#42. : float#
```

## Layouts

So, unboxed types mean that polymorphic definitions need to be
polymorphic over only a subset of the possible types (say, the boxed
ones). We do this by introducing *layouts*, which classify types in
the way that types classify values. (A layout is actually just
one component of a *kind*, which is more fully explored in the
[page on kinds](kinds.md).) The layout of a type determines
how many bytes of memory values of that type consume, how many and
what kind of registers are needed to pass such values to functions,
and whether locations holding such values need to be registered with
the GC.

Once we have layouts, the type of, say, `dup : 'a -> 'a * 'a` is a short form
of:

```ocaml
val dup : ('a : value). 'a -> 'a * 'a
```

The layout `value` is the layout of ordinary OCaml values (boxed or
immediate). This means that trying to use diag at type `int32# ->
int32# * int32#` causes a compilation error, as `int32#` is not of
layout `value`.

The following layouts are available, given with example types of that layout:

```ocaml
(* representable ones *)
string     : value
int        : immediate
int32      : immediate64   (* immediate on 64-bit machines only *)
nativeint# : word
int8#      : bits8
int16#     : bits16
int32#     : bits32
int64#     : bits64
unit#      : void
float#     : float64

(* unrepresentable one *)
any
```

* The key aspect of `value` is that values of `value` types can be scanned by
  the garbage collector. We thus say that `value` types are *scannable*.

* An `immediate` is a `value` that is represented directly, without indirection.
  Thus, an `immediate` is scannable, but because there is no indirection, we are
  not obliged to scan it. An `immediate` type is thus our first example of an
  *external* type -- one that is external to our garbage-collected heap.

* An `immediate64` is a type that is an `immediate` on 64-bit platforms, but is
  just an ordinary `value` on other platforms.

* A `word` takes up the same amount of space as a `value`, but a `word` is not
  known to be scannable (and thus must not be scanned by the GC). However, a
  `word` is always external; it is thus necessary *not* to scan `word` values.

* The `bits` layouts describe external, non-scannable values. On a 64-bit
  platform, `word` and `bits64` are the same, but we do not wish to bake
  this fact into the type system, and so the layouts remain separate.

* The `void` layout takes up 0 bits at runtime. It is useful to classify
  one-constructor, no-field types (including both `unit` and types like
  `('a, 'b) eq`). Accordingly, it is both scannable and external.

As well as the above primitive layouts, layouts may be combined using
the `*` and `+` operators. The
following are all equivalent at runtime (but not equivalent to the type checker):

```
(value * value) * value
value * (value * value)
value * value * value
```

There is a (transitive) subtyping relation between these layouts, as pictured
here:

```
           ----any---------------------------
          /     |      |        |            \
       value   void   word   float64     bits_8..64
         |
    immediate64
         |
     immediate
```

(It would be safe to add `immediate <= word`, as well, but we don't for now. See
[extensions](extensions.md).)  The subtyping relationship
between product and sum layouts is pointwise, and only between products/sums of
the same length. All products/sums are subtypes of `any`, which exists to serve
as the top of our sublayout lattice.

We might imagine that we could make, e.g. `(value * value) * value`
a subtype of `value * (value * value)` (and vice versa), but this
would play poorly with type inference. For example, if we had
`'l1 * 'l2 = value * value * value` (where `'l1` and `'l2` are unification
variables), how could we know how to proceed? Perhaps we could make
a subtyping relationship here, but only when there is an explicit check;
we leave this off the design for now.

Names used in layouts (both the alphanumeric names listed above and the infix
binary operators) live in a new layout namespace (distinct from all existing
namespaces) and are in scope in the initial environment. Layouts can be declared
to be abbreviations or abstract in modules, as described in
[abstract layouts](abstract.md).

Beyond the layouts introduced here, the ["adding null"](null.md) companion
proposal includes layouts `non_null_value` and `non_null_immediate`, and the
"Mutation" section below describes a layout former `*w`.

## Layout inference

As detailed in the [surface syntax](syntax.md) page, users may assign layouts to
types. The design in this document adheres to the following two principles
guiding layout inference:

1. Layout annotations on concrete types or flexible type variables declare upper bounds.
2. Layout annotations on abstract types (including `private` ones) are precise.

These principles are meant to extend current treatment of flexible variables
throughout language design. In particular, they agree with the treatment of
variance annotations, which are checked (but not necessarily precise) for types
with definitions but precise for fully abstract types.

For example, if we write

```ocaml
type ('a : immediate) is_immediate

type myint : value = int
```

then `myint is_immediate` is still accepted -- despite the `: value`
annotation. That is because `myint` is concrete: we know it is `int`.
So the `: value` annotation checks whether `int` is a `value` (it is) but is
otherwise ignored. Put another way, when checking `myint is_immediate`, we look
through the synonym `myint = int` to see that `int : immediate`, and all is
well.

The situation is similar with the annotation on `'a`. The `: immediate` puts an
upper bound on the layout for `'a`. Since nothing else constrains `'a`'s layout,
that becomes its layout, and then `is_immediate` requires its argument to have
an `immediate` layout.

By contrast, if we have

```ocaml
module M : sig
  type myint : value
end = struct
  type myint = int
end
```

then `M.myint is_immediate` will result in an error. The `myint` type here is
abstract, and thus its `: value` annotation is authoritative: `myint`'s best
layout is `value`. This similarly applies in e.g. `fun (type (a : immediate))
... ->` syntax and in `let f : ('a : immediate). 'a -> ...` syntax.

## Unboxed types and records with unboxed fields

All of the currently-existing types in OCaml have layout `value`. Some
also have layout `immediate`, such as `int` or `unit`. We're planning
to add unboxed versions of a few builtin types, such as `int32#` and
`float#`. This gives:

```ocaml
type float : value
type int32 : immediate64
type int : immediate
type float# : float64
type int32# : bits32
```

All of these can be passed and returned from functions, but only the
`value` types (including `immediate` ones) can be stored in, say,
lists. Note that the existing `+.`, etc. primitives won't work on
`float#`, because they have the wrong type. New ones for `float#` will
be made available. (Same for `int32#`, etc.)

Unboxed types will be allowed in record fields:

```
type t = {
  foo : int;
  bar : float#;
  ...
}
```

The OCaml runtime forbids mixing safe-for-GC fields and unsafe-for-GC
fields in a single block. See the mixed-block restriction, below.

## Layout flexibility

A key aspect of the design of unboxed types is that we must know
the representation of a type before we can ever manipulate a value
of that type. For example, check out `hard` here:

```ocaml
let hard : ('a : any). 'a -> 'a = fun x -> x
```

This function is impossible to compile to native code.
The reason is that the compiled `hard` function must
take an argument somehow and then return it somehow. Yet, if we don't
know the representation of that argument, we cannot do this! Does the
argument fit in one register or two? Is it a pointer (that the GC would
have to be aware of) or not? We cannot know: the argument's layout is
`any`.

Nevertheless, we have learned time and time again that polytypic programming
is important. We thus offer [layout flexibility](flexibility.md),
described on another page. However, as that page describes, there are
(compile-time) costs to using `any` layouts; these costs trigger in two
scenarios:

1. when a bound variable's type has layout `any`
2. when an argument passed to a function has a type with layout `any`

More details in the page about [layout flexibility](flexibility.md).

These problems do not come up, however, in using `any` layouts in type
declarations. For example, the following declaration is fine:

```ocaml
type ('a : any) pair = { first : 'a; second : 'a }
```

The above restrictions imply that any construction of `pair` or pattern-match on
`pair` would work only for instantiations that have known representations, but
we can suffice with just one, flexible type declaration.

## Adding a box

Because of the restrictions around the usage of unboxed types
(both that they cannot work with standard polymorphism and the
restriction just above about mixing representations in record
types), we sometimes want to box an unboxed type. The types
we have seen so far have obvious boxed cousins, but we will soon
see the ability to make custom unboxed types. We thus introduce

```
type ('a : any) box

val box : ('a : any). 'a -> 'a box
val unbox : ('a : any). 'a box -> 'a
```

These definitions are magical in a number of ways:

* It is best to think of the type `box` as describing an infinite
family of types, where the member of the family is chosen
by the layout of the boxed type. (The family is infinite because of
the `*` and `+` layouts, which can be used to create new layouts.)

* It would be impossible to define the `box` function without magic, because
it runs afoul of the restriction around function arguments.
A consequence of this design is that the `box` function cannot
be abstracted over: a definition like `let mybox (x : ('a : any)) = box x`
runs into trouble, because it binds a variable of a type whose layout
is unknown.

* Because the function `box` is really a family of functions, keyed
by the layout of its argument, it is not allowed to use `box` without
choosing a layout. The simplest way to explain this is to say that `box`
must always appear fully applied. In this way, it might be helpful to
think of `box` more as a keyword than a function. But we could, in theory,
allow something like `let box_float : ('a : float64). 'a -> 'a box = box`,
where `box` is unapplied (but specialized). However, there is little
advantage to doing so, and it's harder to explain to users, so let's
not: just say `box` must always appear applied. (Note: GHC has a similar
restriction around similar constructs.)

* There is additional magic around various forms of unboxed types,
as described below.

## Arrays

Going beyond storing just one unboxed value in a memory box, we also
want to be able to store many unboxed values in an array. Accordingly,
we extend the existing `array` to take a type argument of `any`,
as if `array` were declared with

```ocaml
type ('a : any) array
```

Just like we need magical value-level primitives `box` and `unbox` to deal
with `box`, we will also need similar primitives to deal with arrays
of unboxed values. This proposal does not spell out the entire API for this
feature, but we will work it out during the implementation. Regardless
of the API details, the `array` type must have a similar layout-indexed
magic as `box`, though `array` could conceivably use a different memory
layout as `box` does. In particular, the memory format of a boxed variant
(as described below in the section on "Unboxed variants") has a variable length,
making it impossible to pack into an `array`. Thus `array` may choose to use
a different memory format as `box` in order to allow for indexing.

Note that extraction of an element from an array of unboxed values (e.g.
with `get`) requires
copying the element. There are two ways a user might want to get an
unboxed value, via the following hypothetical API:

    get : 'a array -> int -> 'a
    get_boxed : 'a array -> int -> 'a box

If the type `'a` is unboxed, the `get` function copies the value from
the memory allocated as part of the array into registers (or other
destination). To avoid this copying, one might imagine `get_boxed`, intended
to return a pointer to the existing allocated memory within the array.
However, `get_boxed` cannot do this: an `'a box` is expected to be a pointer
to a block with a header, and an element within an array lacks this header.
Instead, we might imagine

```ocaml
get_element : 'a array -> int -> 'a element
```

where `element` is a new type that represents a 1-element slice of an
allocated array. The `get_element` function would indeed just return
a pointer, but it requires yet another magical type `element`, represented
by a pointer to the middle of a block (really, a pointer to the beginning
of the block and an offset).

We do not explore this aspect of the design further in this document.
Instead, this consideration of `get_boxed` and `get_element` serves to
explain why there is no loss of run-time efficiency if the memory format used
by `box` and the memory format used by `array` are completely different.

## Unboxed tuples

In addition to new primitive types, like `int32#`, this proposal also
includes user-defined unboxed
types. The simplest of these is unboxed tuples:

```ocaml
#( int * float * string )
```

This is an unboxed tuple, and is passed around as three separate
values.  Passing it to a function requires three registers.
The layout of an unboxed tuple is the product of the layout of its
components. In particular, the layout of `#( string * string )` is
`value * value`, not `value`, and so it cannot be stored in, say, a
list.

Constructing and pattern-matching against an unboxed tuple requires
a `#`: `let ubx : #(int * float#) = #(5, #4.)`. Note that parentheses
are required for construction and pattern-matching unboxed tuples.

Naturally, boxing an unboxed tuple yields a boxed tuple. We thus add
the following rules:

* For all unboxed tuples `#(ty1 * ty2 * ...)`,
`#(ty1 * ty2 * ...) box = (ty1 * ty2 * ...)`. The `...` is meant to
denote that this works for any arity, including 0. (Left undetermined:
what to do about unary unboxed tuples.)

* The syntax `(e1, e2, ...)` continues to work for boxed tuples.

### The mixed-block restriction

The current OCaml runtime has the following restriction:

* For every block of memory, one of the following must hold:

    1. All words in the block (other than the header) may be scanned
    by the garbage collector. That is, every word in the block is either
    a pointer to GC-managed memory or has its bottom bit tagged. This is a
    scannable block.

    2. No words in the block are pointers to GC-managed memory. This is an
    external block.

This is also described at in [Real World OCaml](https://dev.realworldocaml.org/runtime-memory-layout.html).

We call this rule the *mixed-block restriction*, and we call types
whose memory layout is mixed as *mixed-block types*. We imagine that
we will be able to lift this restriction in a future release of the
runtime, but we do not expect to do this as part of our initial rollout
of unboxed types. We thus refer to this restriction in several places
in this specification so that readers might imagine a future improvement.

We can test for the mixed-block restriction by examining a type's layout.
All primitive layouts obey the mixed-block restriction: each is either
scannable or external. A composite layout (that is, formed with `*` and
`+`) is scannable if all of its
components are scannable, and it is external if all of its components
are external. Then, any layout that is either scannable or external
meets the mixed-block restriction.

The first consequence of the mixed-block restriction is already apparent:
we cannot store, say, an unboxed tuple `#(string * float#)` in a memory
block, as that block would be mixed. Its layout is value * float64, which
is neither scannable nor external.

We thus have the following restriction on mixed-block types: `box` may
never be called on a mixed-block type. Equivalently, the layout of the argument
to `box` must be either scannable or external.
(This is the essense of
the mixed-block restriction.) However, because `box` is implicitly
used by constructors of boxed types -- such as a boxed tuple -- this
restriction applies more widely than code literally calling `box`. For
example, the following would be rejected:

```ocaml
let bad : (string * float#) = "hi", #4.
```

The problem is the implicit call to `box` in the definition, which would
expand to

```ocaml
box #("hi", #4.)
```

Note that the mixed-block restriction never requires looking directly at
*types*; instead, a mixed-block type can be identified by looking only at
its *layout*. This is in keeping with our understanding of `box` as a
family of functions, indexed by layout. The members of the family corresponding
to mixed-block types simply do not exist.

## Unboxed records

Unboxed records are in theory just as straightforward as unboxed tuples, but
have some extra complexity just to handle the various ways that types are
defined and used. Suppose you write:

```ocaml
type t = { x : int32#; y : int32# }
```

Sometimes we want to refer to `t` as a boxed type, to store it in
lists and other standard data structures. Sometimes we want to refer
to `t` as an unboxed type, to avoid extra allocations (in particular,
to inline it into other records). The language never infers which is
which (boxing and allocation is always explicit in this design), so we
need different syntax for both.

To make this all work, we consider a declaration like the one for
`t` above to actually declare *two* types. To wit, we desugar the
above syntax to these declarations:

```ocaml
type t# = #{ x : int32#; y : int32# }
type t = t# box
```

That is, we have a primitive notion of an unboxed record, called
`t#`, and then we define `t` just as `t# box`. (The `#{` syntax is
also available to users, if they want to just directly declare an
unboxed record.)

The layout of an unboxed record, like `t#`, is the product of the layouts
of the fields. So, for our example, the layout of `t#` will be `bits32 * bits32`,
because the layout of `int32#` is `bits32`. The tuple is ordered according
to the order of fields as listed in the declaration, even though the order of
such fields usually does not matter.

To make this backward compatible (and at all pleasant to program in),
we add a little more magic to `box`:

* In the syntax `e.x` (where `e` is an arbitrary expression and `x`
is a path), if `e` has type `ty box` (for some `ty`), then we treat
the expression as if it were projecting from `ty` after unboxing.

* Along similar lines, we allow record-creation and pattern-matching
syntax `{ ... }` to create and match against `box`.

We might imagine an alternative design without automatically creating
unboxed record definitions in this way, but that design does not uphold
the **Availability** principle as well as the current design.

### Construction

We use a `#` in the syntax for creating unboxed records:

```ocaml
type q = #{ x : int; y : int32# }
let g : q -> q = fun #{ x; y } as r -> #{ x = x + r.x; y }
```

(If we didn't have the special treatment for `box`, we wouldn't need to
differentiate construction syntax, as we can infer that the record is unboxed
from the field labels, as we do for other record types. We could also
release a first version that requires `#{` and then see if users want to
be able to avoid the `#`. If we drop the `#` requirement, we could simply
default to boxed, unless the type system disambiguation mechanism specifically
selects for unboxed.)

Type aliases do not automatically bind an unboxed version; rather
it is a property of the record type declaration that creates the
extra unboxed definition. However, if you create a transparent alias
like

```ocaml
type u = t = { x : int32; y : int32 }
```

then we translate this to become

```ocaml
type u# = t# = #{ x : int32; y : int32 }
type u = u# box   (* equivalent to type u = t# box *)
```

Naturally, you can also directly alias an unboxed type:

```ocaml
type u = t#
```

Similarly, an abstract type abstracts only the boxed type:

```ocaml
module M : sig
  type t
end = struct
  type t = { x : int32; y : int32 }
end
```

defines `M.t` but not `M.t#`. If you want to export both
an unboxed and boxed version of an abstract type, you can with

```ocaml
module M : sig
  type unboxed_t : value * value
  type t = unboxed_t box
end = struct
  type t = { x : int32; y : int32 }
  type unboxed_t = t#
end
```

We might imagine allowing module signatures to include `type t#`
when they also include `type t`, but as we see here, this feature
is not strictly necessary.

### `[@@unboxed]` records

The declaration

```ocaml
type t = { field : ty } [@@unboxed]
```

is equivalent to writing

```ocaml
type t = #{ field : ty }
```

with the exception that construction and pattern-matching
do not require the `#` prefix. Accordingly, the layout of
the newly declared type (`t` in the example) is always the
same as the layout of the field type.

### Recursion

Because the layout of an unboxed record comprises the layouts of
its fields, unboxed records cannot normally be recursive. For example,
this would be rejected:

```ocaml
type t = #{ f1 : int; f2 : t }
```

because its layout would be infinite. Unboxed types *may* be mutually
recursive with boxed records, though, as this breaks the cycle (boxed
records are `value`s).

An exception is made for a record with one field, like this:

```ocaml
type t = #{ f : t }
```

This declaration is useless -- no values have this type -- but the layout
is at least finite. If nothing constrains the layout of the type, it is
defaulted to `value`.

### Field names and disambiguation

If we have a

```ocaml
type t = { x : int; y : int }
```

and write a function

```ocaml
let f t = t.x + t.y
```

what type do we infer for `f`? Of course, we want to infer `t -> int`, but in
the presence of unboxed types, we might imagine inferring `t# -> int`, as `t.x`
is a valid way of projecting out the `x` field of both `t` and `t#`. Yet
inferring `t# -> int` would be terribly non-backward-compatible.

We thus effectively have the boxed projection shadow the unboxed one. That is,
when we spot `t.x` (for a `t` of as-yet-unknown type), we assume that the
projection is from the boxed type `t`, not the unboxed type `t#`. If a user
wants to project from the unboxed type, they can disambiguate using a prefix
`#`, like `t.#x`. Thus, if we have

```ocaml
let g t = t.#x + t.y
```

we infer `g : t# -> int`, though inference is non-principal. Users could also
naturally write

```ocaml
let g t = t.#x + t.#y
```

which supports principled type inference.

To be clear, the `#` mark is just used for disambiguation. The following also
works:

```ocaml
let g (t : t#) = t.x + t.y
```

There is no ambiguity here, and thus the `#` mark is not needed on record
projections.

Although this section discusses only record projections, the same idea applies
to record construction and pattern matches: the field of the boxed record
shadows the field of the unboxed record, though the latter can be written with a
`#` prefix (or can be discovered by type-directed disambiguation).

### Mutation

There are several concerns that arise when thinking about mutable fields and
unboxed types; this section lays out the scenarios and how they are treated.
We assume the following declarations:

```ocaml
type point = { x : int32#; y : int32# }
type mut_point = { mutable x : int32#; mutable y : int32#; identity : int }
```

1. **Mutable unboxed field in a boxed record**. Example:

    ```ocaml
    type t = { radius : int32#; mutable center : point# }
    ```

    A mutable unboxed field can be updated with `<-`, analogously to a mutable
    boxed field. However, updating an unboxed field might take multiple separate
    writes to memory. Accordingly, there is the possibility of data races when
    doing so: one thread might be writing to a field word-by-word while another
    thread is reading it, and the reading thread might witness an inconsistent
    state. (For records with existential variables and unboxed sums, this
    inconsistent state might lead to a segmentation fault; for other types, the
    problem might arise only as an unexpected result.) We call this undesirable
    action *tearing*.

    To help the programmer avoid tears, an update of a mutable
    unboxed field is sometimes disallowed. We surely want to disallow such an
    update if it is not type safe. We also want to disallow such an update if it
    could violate abstraction: perhaps some abstract record type internally
    maintains an invariant, and a torn record might not support the invariant.

    Some cases are easy: we definitely want to support mutation of `float#`s, and
    we definitely want to disallow mutation of wide unboxed variants (where we
    might update the tag and the fields in separate writes). What about wide
    unboxed records? It depends. Consider the type `t` in this section, and
    imagine `type s = { mutable circle : t# }`. Should we allow
    `s.circle <- ...`? Our answer, in essence: if a type is concrete, then allow
    the update. After all, a concrete type cannot be maintaining any invariants
    (or if it is, the programmer is responsible). So, if we have the definition
    of `t#`, then `s.circle <- ...` is allowed. However, if we do not have the
    definition of `t#`, then `s.circle <- ...` is disallowed.

    Yet this design -- based on the concreteness of a type -- is disappointing,
    as it bases the choice of allowing mutable updates on whether or not an
    interface exposes a representation. Instead, we would want to be able to
    write an abstract type yet which allows a potentially-tearing update.

    We thus introduce a new layout `*w` (the "w" stands for "writable"). (We
    will use `*w` in our notation here, but the surface syntax is different and
    defined below.) `*w` is like `*` (and `l1 *w l2 <= l1' * l2'` iff `l1 <=
    l1'` and `l2 <= l2'`), but a product built with `*w` is allowed in a mutable
    update, even if it is potentially non-atomic. The inferred layout of a most
    (see below) product types (like `t#`) will be built with `*w`, not `*`. Yet
    if a type is exported abstractly from a module, the module signature will
    have to specify the layout of that type; most users will write a layout
    using `*`, thus protecting values of that type from getting torn.

    If a user wants to write a layout with `*w`, they can do so like this:

    ```ocaml
    type t : bits32 * (bits32 * bits32) [@writable]
    ```

    The `[@writable]` attribute transforms all `*`s in a layout to be `*w`s.

    Despite most product types being inferred to have a `*w` layout, product
    types with existential variables are inferred to have a `*` layout, as
    tearing an existential could be type-unsafe. Here is an example:

    ```ocaml
    type ex_int = K : 'a * ('a -> int) -> ex_int
    ```

    The layout of `#ex_int` is `value * value` (the first components is `value`
    because there is no annotation on `'a` suggesting otherwise), with the
    non-writable `*`.

    Putting this all together, we define the following predicate to test for
    writeability:

    ```
    writable : concrete_layout -> bool
    writable(l1 * l2) = false
    writable(l1 + l2) = false
    writable(l1 *w l2) = writable(l1) && writable(l2)
    writable(_) = true
    ```

    The update of a mutable field is thus allowed if *either*:

    1. The layout of the field fits in a 64-bit word, *or*
    2. The layout of the field satisfies the `writable` predicate above.

    The first possibility -- for word-sized layouts -- is because a type whose
    values fit in a 64-bit word have no threat of getting torn, and thus
    can be updated safely. We choose *64-bit* words (not 32-bit ones) because
    32-bit OCaml does not support multicore, and so no possibility of tearing
    exists.

    The restriction described here is called the *tearing restriction*.

    The tearing restriction allows updates of our `mutable center` field,
    for two reasons: the type `point#` is concrete and thus has layout
    `bits32 *w bits32`, with `*w`, and a `point#` fits in a 64-bit word.

    A user who wishes to update a non-atomic mutable field (with their own plan
    for synchronization between threads) may do so with the `[@non_atomic]`
    attribute, like so:

    ```ocaml
    type point = { x : int32#; y : int32# }
    module Circle : sig
      type t : bits32 * (bits32 * bits32)
    end = struct
      type t = #{ radius : int32#; mutable center : point# }
    end

    type t = { mutable circle : Circle.t#; color : Color.t }
    let f (t : t) = t.circle <- #{ ... } [@non_atomic]
    ```

    The design in this section upholds the following principle:

    * Even in the presence of data races, a programmer can never observe an
      intermediate state of a single assignment (unless they write `[@non_atomic]`).

    **Optional.** It may be convenient for users to get a warning when they
    declare a mutable field that falls afoul of the tearing restriction and
    thus requires `[@non_atomic]` on *every* update.

2. **Mutable field in an unboxed record**. Example:

    ```ocaml
    fun (pt : mut_point#) -> (pt.x <- pt.x + #1l); pt
    ```

    Mutation within an unboxed record would be  unusual, because unboxed values have no
    real notion of identity. That is, an update-in-place appears to be
    indistinguishable from a functional update (that is, `{pt with x = pt.x +
    #1l}`). By its unboxed nature, an unboxed value cannot be aliased, and so the
    mutation will not propagate.

    Here is an example:

    ```ocaml
    fun (pt : mut_point#) ->
      let boxed = box pt in
      pt.x <- pt.x + #1l;
      (unbox boxed).x = pt.x
    ```

    This function would always return `false`, because the point stored in the
    box is a *copy* of `pt`, not a reference to `pt`. In other words, unboxed
    values are passed by *value*, not by *reference*. Accordingly, every time an
    unboxed value is passed to or returned from a function, it is copied.

    Because of the potential for bugs around mutability within unboxed records,
    this proposal disallows it: in an unboxed record, any `mutable` keywords are
    ignored. Accordingly, the example above is rejected: `pt.x <- ...` is
    disallowed, because `pt` is an unboxed record.

    This section can be summarized easily:

    * Fields of unboxed records are never mutable.

3. **Fields within a mutable unboxed record within a boxed record.** Example:
    if we have `t : t` (where the type `t` is from mutation case 1, above), then
    `t.center.x` is a field within a mutable unboxed record within a boxed
    record.

    The case of such a field is interesting in that an unboxed record placed
    within a boxed record *does* have a notion of identity: it lives at a
    particular place in the heap, and the surrounding boxed record might indeed
    be aliased. Thus mutation in this nested case does make good sense, and we
    surely want to support it.

    We thus introduce a new syntax for updates of individual fields of a
    mutable unboxed record: We write e.g. `t.(.center.x) <- ...`. Note the
    parentheses: they denote that we're updating the `x` sub-field within the
    mutable `center`. The parentheses are required here, as is the leading
    `.`. (The leading `.` within the parentheses disambiguates the syntax with
    array access.)

    The rule is this: Consider the list of field accesses in a parenthesized
    field assignment. The first field must be mutable, and all fields in the
    list must be unboxed.

    By the first field being mutable, we know that the record directly before
    the open-parenthesis is boxed, because unboxed records do not have mutable
    fields.

    This allows `t.(.center.x) <- #3l` even though `x` is not
    declared as `mutable`. This does not break abstraction: the type of `center`
    must already be concrete in order to write this, and so a user can always
    write `t.center <- {t.center with x = #3l}` instead (modulo the
    tearing restriction). In effect, `t.(.center.x) <- #3l` can be seen as
    syntactic sugar for `t.center <- #{ t.center with x = #3l }`, but allowed
    even if `t.center` is too wide for the tearing restriction.

    Beyond just mutable fields in boxed records, this new syntax form extends to
    elements of mutable arrays. So if we have a `#pt array` named `arr`, users
    could write `arr.(.(3).x) <- #4l` to update one field of an element of the
    array.

### Nesting

According to the descriptions above, the `#` prefix on types is
*not* recursive. That is, the contents of a `t#` are the same as
the contents of a `t` -- it's just that the `t#` is laid out in
memory differently. Let's explore the consequences of this aspect
of the design by example:

Consider the definitions (let's assume a 64-bit machine)

```ocaml
type t0 = { x : int32; y : int16 }
type t1 = { x : int32#; y : int16# }
type t2 = { t1 : t1; z : int16# }
type t3 = { t1 : t1#; z : int16# }
type t4 = #{ t1 : t1#; z : int16# }
```

The points below suggest a calling convention around passing arguments
in registers. These comments are meant to apply both to passing arguments
and returning values, and are up to discussion with back-end folks.

1. A value of type `t0` will be a pointer to a block of memory with 3 words: a
header, a pointer to a boxed `int32` for `x`, and a word-sized immediate for
`y`. It is passed in one register (the pointer).

2. A value of type `#t0` will comprise two words of memory: a pointer for `x`,
and an immediate for `y`; it is passed in two registers to a function.

3. A value of type `t1` will be a pointer to a block of memory with 2 words:
a header and a word containing `x` and `y` packed together (precise layout
within the word to be determined later, and likely machine-dependent).
It is passed in one register (the pointer).

4. A value of type `t1#` will comprise one word of memory, containing
`x` and `y` packed together. When passed to a function, it will be
passed as *two* registers, one for each component.

5. A value of type `t2` does not exist: it would be pointer to a *mixed* block,
containing a pointer to a `t1` structure and a non-GC word for `z`.

   **Alternative:** (We do not plan to do this.) A value of type `t2` is a
pointer to a block of memory with three words: a header, a pointer to a `t1`
structure (as described above), and a tagged immediate word, 16 bits of which
store `z`. (In this design, a `z : int64#` would be rejected because there is no
room for the tag.)

6. A value of type `t2#` comprises two words: one is a pointer to a `t1`
structure, and one contains `z`. (The `z` word does *not* need to be tagged,
as the GC will know not to look at it.) It may *not* be stored in
memory; it is passed in two registers.

7. A value of type `t3` is a pointer to a block of memory with 2 words:
a header and one packed word containing 48 bits of `t1` and 16 bits of
`z`. It is passed in one register.

8. A value of type `t3#` comprises one packed word in memory, containing
all of `x`, `y`, and `z`, with no tag bit. It is passed in three registers,
one each for `x`, `y`, and `z`.

9. The type `t4` is utterly identical to the type `t3#`.

10. The type `t4#` does not exist; it is an error, as the name `t4#` is
unbound.

The key takeaway here is that if a programmer wants tight packing, they have to
specify the `#` in the definition of their types. Note that there is no way to
get from the definition of `t0` or `t2` to a tightly packed representation; use
`t1` or `t3` instead!

A separate takeaway here is that memory layout in our design is *associative*:
the memory layout does not depend on how the type definitions are
structured. This is in contrast to C `struct`s, where each sub-`struct` must be
appropriately aligned and padded. For example, the C translation of this example
is

```c
struct t3 {
  struct t1 { int32 x; int16 y; } t1;
  int16 z;
};
```

Yet this would take 2 words in memory, as `t1` would be padded in order to be
32-bit aligned.

### Further memory-layout concerns

We wish for unboxed records (and tuples, to which this discussion equally
applies) to be packed as tightly as possible when stored in memory. (This dense
packing does not apply when an unboxed record is stored in local variables, as
it may be more efficient to store components in separate registers.)

This section of the proposal describes a possible approach to tight memory
packing that supports reordering. This section is more hypothetical than others,
and we type theorists defer to back-end experts if they disagree here. The
section ends with a few user-facing design conclusions.

The key example is

```ocaml
type t5 = { t1 : t1#; a : int; z : int16# }
```

Note that now, we have an `a` between the `t1#` and the `int16#`.

A bad idea would be to tightly pack the `int` against the `t1#`, like this (not
to scale):

    63       31        0 63       31       0
    xxxxxxxxxyyyyyaaaaaa aaaaaaaaaaaaaaazzzz

Note that the 64 bits of `a` are spread across *two* words. This would make
operations on `a` very expensive. Just say "no"!

Instead, we insist that `a` is word-aligned. We might thus imagine

    63       31        0 63       31       0 63        31        0
    xxxxxxxxxyyyyy000000 aaaaaaaaaaaaaaaaaaa zzzzz0000000000000000

where `0` denotes padding. That works, but it's wasteful. Instead, we do

    63       31        0 63       31       0
    xxxxxxxxxyyyyyzzzzzz aaaaaaaaaaaaaaaaaaa

Everything is aligned nicely, and there's no waste. The only drawback is that
we have *reordered*.

In general, we reserve the right to reorder components in an unboxed tuple or
record in order to reduce padding. With demand, we could imagine introducing
a way for programmers to request that we do not reorder. (An unboxed type that
does not participate in reordering would have a different layout from one that
does participate in reordering, meaning essentially a new layout former, such
as `&`, analogous to `*`.)

However, the reordering is *not* encoded in the layout system. Imagine now

```ocaml
type t6 = { t1 : t1#; z : int16#; a : int }
```

This `t6` is the same as `t5`, but with fields in a different order. We have

```ocaml
t5 : (bits32 * bits16) * immediate * bits16
t6 : (bits32 * bits16) * bits16 * immediate
```

Accordingly, a type variable that ranges over `t5` would not also be able to
range over `t6`: the two have *different* layouts. We could imagine a very
fancy layout equivalence relation that would detect the reordering here and
say that `t5`'s layout equals `t6`'s; we choose not to do this, for several
reasons:

* Encoding reordering in the layout system potentially constrains us in the
  future, in case we wish to do fewer reorderings.
* This significantly complicates layouts, for little perceived benefit.
* Different architectures may benefit from different reorderings; we do not want
  the type system to depend on the architecture.

Accordingly, the reordering of physical layout is mostly undetectable by users:
they just get a more compact running program. The way to detect the reordering
is by either inspecting memory manually (of course) or by sending unboxed
records through a foreign function. In order to support foreign functions, we
will add an interaction with `ocamlc` that produces a C header that offers
functions that extract and set the various fields of an unboxed tuple. Foreign
code could then use this header to be sure that it interacts with our reordering
algorithm correctly.

#### Backward compatibility

Existing OCaml programs may have foreign interfaces that rely on a certain
ordering of record fields. The reordering story need not disrupt this. To wit,
we promise that, as we work out the details of reordering, we *never* reorder
fields in records (or tuples) where all fields have layout `value`. (This
includes *all* types expressible before our change.)

Going further, and imagining possibly lifting the mixed-block restriction, we
can imagine the following rule:

* (Hypothetical) In any record or tuple type, its fields are ordered in memory
  with all `value`s first, in the order given in the declaration, followed by
  non-`value`s, in an implementation-defined order.

This rule actually contradicts the layout of `t5` above, which would put `a` (a
`value`, because `immediate`s are `value`s) first. However, we make no promises
about the hypothetical rule and include it here just as a possible way forward.

## Unboxed variants

Unboxed variants pose a particular challenge, because the boxed
representation is quite different than the unboxed representation.

Consider

```ocaml
type t = K1 of int * bool | K2 of float# * int32# | K3 of int32# | K4
```

An unboxed representation of this would have to essentially be like a C
union with a tag. This particular `t#` would use registers as follows for
argument passing:

```ocaml
bits2         (* tag information *)
immediate     (* for the int *)
immediate     (* for the bool *)
float64       (* for the float# *)
bits32        (* for both int32#s, shared across constructors *)
```

No matter which variant is chosen, this register format works. This is important
because the layout of an unboxed type must describe its register
requirements, regardless of what constructor is used. We thus use a new
layout former to describe unboxed variants, `+`. That is, the layout of `t` would
be `(immediate * immediate) + (float64 * bits32) + bits32 + void`.
By specifying only the
layouts of the variants' fields -- not the actual register format itself -- we leave
abstract the actual algorithm for determining the register format.
(See "Aside" below for discussion on this point.)

One challenge in working with unboxed variants is that it may be hard for the
programmer to predict the size of the unboxed variant. That is, a programmer
might have a variant `v` and then think that `#v` will be more performant than
`v`. However, `#v` might be wider than even the widest single variant of `v` and
thus actually be *less* efficient. (Of course, this is always true: we should
always test before assuming that e.g. unboxing improves performance.) As we
implement, we should keep in mind producing a mechanism where programmers can
discover the memory layout of an unboxed variant, so they can make an informed
decision as to its usage.

### Boxing

In contrast to the fixed register format above, a boxed variant needs only
enough memory to store the fields for one particular constructor. That's because
boxed variants get allocated specifically for one constructor -- there is no
requirement that all variants have the same size and layout.

Despite the challenges here, `box` can still work to convert an unboxed variant
to a boxed one: `box` simply understands the `+` layout form to mean
alternatives,
just as variants have always worked.

We thus extend the treatment of unboxed records to work analogously
for unboxed variants. That is, we treat a definition of a boxed variant

```ocaml
type t = K1 of ty1 | K2 of ty2 | ...
```

to really mean

```ocaml
type t# = #( K1 of ty1 | K2 of ty2 | ... )
type t = t# box
```

Just like with records, the layout of an unboxed variant like `t#` comprises
the layouts of the constructors, but combined with `+`, not `*`. The handling
of recursion is the same as with records: most uses of recursion will lead to
infinite layouts, which are rejected. If the layout is finite but unconstrained,
it is defaulted to `value`.

We then additionally add magic to `box` to make this change transparent to users:

* A boxed unboxed variant may be constructed and pattern-matched against as
if the box were not there. Following our example, `K1` and `K2` could construct
values of type `t`, and values of type `t` could be matched against constructors
`K1` and `K2`.

Depending on how an unboxed variant ends up in memory, it has one of three possible
representations:

* If an unboxed variant is `box`ed, then it has the same representation as
  today's variants: a block including a tag and the fields of
  the particular constructor (only).

* If an unboxed variant is part of a boxed product (i.e. record, tuple, or array),
  then it takes up exactly as much space as needed to store the tag and the
  widest constructor. Here is the example:

    ```ocaml
    type t = K1 of int * string * string | K2 of bool * bool | K3 of string
    type t2 = { f1 : float; f2 : t#; f3 : string }
    ```

    A value of type `t2` will be a pointer to a block arranged as follows:

    ```
    word 0: header
    word 1: f1, a pointer to a boxed float
    word 2: tag for f2
    word 3: K1's int (immediate) or K2's bool (immediate) or K3's string (pointer)
    word 4: K1's string (pointer) or K2's bool (immediate)
    word 5: K1's string (pointer)
    word 6: f3, a pointer to a string
    ```

    Note that the `t#` takes up 4 words, regardless of the choice of
    constructor. This fixed-width representation is needed in order to give `f3`
    a fixed offset from the beginning of the record, which makes accesses of
    `f3` more efficient. Imagining a variable-width encoding requires examining
    the tag of the variant in order to address later fields; this becomes
    untenable if a record has multiple inlined variants. (We can think of `t#`
    here more as an inlined variant than an unboxed one.)

* If an unboxed variant is part of an outer variant, we essentially inline the
  inner unboxed variant. The section on "Nesting", below, covers this case.

### Constructor names and disambiguation

Just as we have done for record fields, we consider the constructor for the
boxed variant to shadow the constructor for the unboxed one. That is, writing
`K1 blah` will construct a `t`. However, if we already know that the expected
type of `K1 blah` is `t#`, then we use type-directed disambiguation to discover
that the `K1` is meant to refer to the unboxed variant, not the boxed
one. Echoing the design for record fields, we can use a `#` prefix to
disambiguate manually: `#K1 blah` unambiguously creates a `t#`.

### `[@@unboxed]` variants

Just like with records, an `[@@unboxed]` variant is equivalent to a normal
unboxed variant. For example, the declaration

```ocaml
type t = K of ty [@@unboxed]
```

is equivalent to

```ocaml
type t = #( K of ty )
```

Accordingly, the layout of the newly declared type (e.g. `t`) is the same
as the layout of the field of the constructor. The same holds true if the
constructor has an inlined record.

### Nesting

We can naturally nest unboxed variants, just like we did with unboxed records.
Just as before, the `#` mark is *not* recursive. Also just as before, we can
gain extra efficiency by cleverly packing one variant inside of another. Let's
explore an example:

```ocaml
type t = K1 of int * bool | K2 of float# * int32# | K3 of int32# | K4
  (* as above *)
type t2 = K2_1 of int32# | K2_2 of float# * float# | K2_3 of t#
```

The `t` is unedited from above, but the `t2` definition contains a `t#`.

We imagine the following register format for `t2`:

```ocaml
bits3          (* for the combined tag *)
immediate      (* for K1's int *)
immediate      (* for K1's bool *)
float64        (* for K2's float# and K2_2's first float# *)
float64        (* for K2_2's second float# *)
bits32         (* for K2's, K3's, and K2_1's int32#s *)
```

A few observations to note about this format:

* There is no part of this arrangement that matches the arrangement for `t`;
indeed, it is not necessary to efficiently get from a `t2` to a `t`: when the
user asked to unbox `t` in the definition of `t2`, they gave up on this
possibility.
* If the `t#` component of `K2_3` is, say, passed to another function, its
  components will have to be copied into new registers, so that the function can
  extract the pieces it needs.
* If the `t#` component of `K2_3` is, say, matched against, the matcher can
  simply remember an offset when looking at the tag. In our case, we might
  imagine that tags `0` and `1` correspond to `K2_1` and `K2_2`, with tags `2`
  through `5` corresponding to the constructors of `t`. Then, a match on `t`
  would simply subtract 2 from the tag before comparing against `t`'s
  constructors.

Note that the description above around boxing applies to nested variants, too,
because the layout of `t2#` is

```ocaml
bits32 + (float64 * float64) + (immediate * immediate) +
  (float64 * bits32) + bits32 + void
  ```

. Each summand is interpreted as a variant by `box`, and thus the in-memory
representation does not need to take any extra space. Spelling this out, a
`t2# box` would be either be the tagged immediate for `0` (representing
`K2_3 K4`) or a pointer to a block containing a header and the following fields

    tag  |  fields                         | represents
    ===================================================
      0  |  bits32                         |  K2_1 x
      1  |  float64 ; float64              |  K2_2 (x, y)
      2  |  immediate ; immediate          |  K2_3 (K1 (x, y))
      3  |  float64 ; bits32               |  K2_3 (K2 (x, y))
      4  |  bits32                         |  K2_3 (K3 x)

As in the discussions above about low-level details: it's all
subject to change. The key observation here is that unboxing a variant
effectively inlines it within another variant. Put another way, we want the
concrete use of memory and registers for a type to be unaffected by the choice
of how the type is structured (that is, ignore the associativity choices of `+`,
much like we have ignored the associativity choices of `*`).

### Aside: Why we have a fresh layout for unboxed variants

The section above describes a fresh layout constructor `+` for unboxed
variants. However, in memory, an unboxed variant is laid out just like
an unboxed tuple, so we could, in theory, use the same layout constructor
for both. Under this idea, the `t` example above would have layout
`immediate * immediate * float * int32 * int2`.

However, we do not adopt this design for (at least) these reasons:

1. Encoding the unboxed-variant layout algorithm in the type system
seems fragile in the face of possible future improvements/changes. Maybe
we can come up with a better way of packing certain fields in the future,
and it would thus be a shame if such a change broke layout checking.

2. An unboxed product type of that kind can reasonably be the
type of a mutable record field. However, the variant case
can't. That's because racing writes of an unboxed variant type
risk memory safety -- the fields from one constructor might
win the race, whilst the tag from the other constructor wins.

3. The most natural representation for including an
unboxed sum type within a boxed sum type is actually to add
aditional constructors to the boxed sum type -- dual to how
unboxing a product into a product adds additional fields. In
other words, we would store the tag of the nested unboxed sum
type as part of the tag of the enclosing boxed sum type.

That representation also gives an additional excuse for not
allowing mutable unboxed variant fields.

Note that the kind of representation described above is still
available using GADTs to make the tag fields explicit. For example:

```ocaml
type foo = { a : int;
             unused1 : unit;
             unused2 : unit; }

type bar = { b : float;
             c : string;
             unused : unit; }

type baz = { d : string;
             e : string;
             f : string; }

type ('a : value * value * value) tag =
  | Foo : foo# tag
  | Bar : bar# tag
  | Baz : baz# tag

type t =
   T : { g : float;
         tag : 'a tag
         data : 'a } -> t
```