# Type inference for unboxed types

This page describes type inference concerns when working with unboxed types and
kinds. This document assumes the reader is familiar with the [main
unboxed-types proposal](index.md), the extension for [adding null](null.md),
and how layouts are components of [kinds](kinds.md).

# Polymorphism, abstraction and type inference for unboxed types

The design notes below here are about the details of making unboxed
types play nicely with the rest of the type system.

The key problem is that we must now somehow figure out the kinds
of all type variables, including those in type declarations and
in value descriptions. The **Availability** principle suggests that
we should infer kinds as far as is possible. Yet the **Backward
compatibility** principle tells us that we should prefer the
`value` kind over other kinds when there is a choice.

We thus operate with the following general design:

* The kind of a *rigid* variable is itself rigid: it must be supplied
at the type variable's binding site.

* The kind of a *flexible* (unification) variable is itself flexible:
it can be inferred from usage sites.
If we still do not know the kind of a variable when we have finished
processing a compilation unit, we default it to `value`.

(Why wait for the whole compilation unit? Because we might get critical
information in an mli file, seen only at the very end of an entire file.)

We retain principal *types*, but we don't have principal *kinds*:
for any expression, there's a best type for any given kind, but
there's no best kind (or at least, the best kind isn't always
compilable, because we cannot compile functions that operate over
values of types of layout `any`).

Programmers may also specify a kind, using the familiar `:` syntax
for type ascriptions. A kind may be given wherever a type appears.
For example, these declarations are accepted:

```ocaml
type ('a : value, 'b : immediate) t = Foo of 'a * 'b
val fn : ('a : immediate) . 'a ref -> 'a -> unit
val fn2 : 'a ref -> ('a : immediate) -> unit   (* not on declaration of 'a *)
val fn3 : unit as (_ : immediate) -> unit      (* awkward syntax for an ascription on type, not variable *)
type ('a : bits32 * bits32) t2 = #{ x : #float; stuff : 'a }
val fn4 : 'a t2 -> 'a t2    (* kind of 'a is inferred *)

type t5 : bits32   (* kind ascription on an abstract type *)

val fn5 : 'a -> 'a    (* 'a is assumed to have layout `value` *)
```

It may seem a little weird here to allow abstraction over types of
kind `bits32`. After all, there's only one such type (`#int32`).
However, this ability becomes valuable when combined with abstract
types: two different modules can expose two different abstract types
`M.id`, `N.id`, both representing opaque 32-bit identifiers. By
exposing them as abstract types of layout `#int32`, these modules can
advertise the fact that the types are represented in four bytes
(allowing clients of these modules to store them in a memory-efficient
way), yet still prevent clients from mixing up IDs from different
modules or doing arithmetic on IDs.

## Defaults

When inferring the kind of a type, we must consider what we use as the
default layout. In general, we want make a choice low in the lattice for
covariant positions and high in the lattice for contravariant positions, as
these defaults will allow for more expressiveness. That is, if we say
`type 'a t` (with no `=`), a low-in-the-lattice layout for `t` means that `t`
can be used in more contexts, while a high-in-the-lattice layout for `'a` means
that `t` can be applied to more types. With this in mind, we will use the
following defaults:

```ocaml
type t : <<here>>                 (* default: non_null_value *)
type ('a : <<here>>) t            (* default: non_null_value *)
fun (type (a : <<here>>)) -> ...  (* default: value *)
let f : ('a : <<here>>). ...      (* default: value *)
```

Abstract types `t` should have a default layout of `non_null_value`, as this is
both low in the lattice and also the layout of any type declared with record,
variant, extensible variant, or object type definition. It thus seems likely
that the default will work with both usages of `t` and the definition of `t`.

A parameter to an abstract type `'a` in `type 'a t` (ditto the type variables in
`type ('a, 'b) t`) also defaults to have layout `non_null_value` -- even though
this choice is low in our lattice. The reason we must do this is to maintain
backwards compatibility. Consider the following module:

```ocaml
module M : sig
  type 'a t
end = struct
  type 'a t = 'a
end
```

If the default for `'a` were anything different from the default for `t`, this
definition would not be accepted. This is unfortunate, because our other
principle of availability is sacrificed in order to maintain backward
compatibility here: defaulting `'a : non_null_value` means that e.g. `string
or_null Widget.t` is disallowed for a `Widget` module declared with no kind
annotations. (This particular choice of default is hard, and may be revisited;
defaulting `'a : any` is much more compelling here, but we would lose the
ability to write `M` here without annotations.)

In rigid type variables introduced in function types, we default to `value`, as
doing so seems like a happy compromise. Going higher in the lattice (i.e. `any`)
makes functions too hard to define (e.g. `let id : 'a. 'a -> 'a = fun x -> x` is
rejected), and going lower (i.e. `non_null_value`) makes functions too hard to
call. This, too, may end up revisited in the light of experience.

### Recursive unboxed types

The default above for type declarations applies only to fully abstract types;
if the type has a manifest or a kind (that is, if the declaration has
at least one `=`), then we can learn the layout from the type's definition.
There is one exception to this rule, however: recursive `[@@unboxed]` types.
For example:

```ocaml
type loopy = { field : loopy } [@@unboxed]
```

Here, any layout would be correct. (We can imagine more complex cases,
including with mutual recursion.) In this case, we default to `non_null_value`,
just like we do with an abstract type. This has the advantage of being
fully backward compatible. We considered the possibility of requiring a layout
annotation here, but that would break some existing code. We also considered
the possibility of assigning `any` to this type (we can't build one at runtime,
after all), but then e.g. `loopy list` would be newly rejected, and so this
this choice is also not backward compatible.