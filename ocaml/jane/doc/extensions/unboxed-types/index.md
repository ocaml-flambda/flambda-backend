# Unboxed types (implemented)

[unboxed types proposal]: ../../proposals/unboxed-types/index.md
[kind annotation]: ../../proposals/unboxed-types/syntax.md#kind-annotations

This page documents the fragment of the [unboxed types
proposal][] that is implemented. Watch this page
for updates on what's available.

This page is intended to be a brief overview; the details are in the [unboxed types
proposal][].

**Warning:** This is all early days for unboxed types. Expect plenty of papercuts. That
said, we have confidence that these features are safe to use. Annoying, but safe.

# Layouts

Every type is now classified by a *layout*, much like how every expression is classified
by a *type*. There is a small fixed set of layouts:

* `value` is the layout occupied by almost every OCaml type. Every type you have ever
  conceived of before reading this description is a `value`.
* `immediate` is a sublayout of `value`, describing those values that are represented
  without a pointer. That is, `int : immediate`, as well as `private` and `[@@unboxed]`
  wrappers around `int`.  `immediate` is a sublayout of `value`, and so you can use an
  `immediate` type wherever a `value` is expected, without any explicit conversion
  necessary. Types declared with `[@@immediate]` have layout `immediate`.
* `immediate64` is a variant of `immediate` that is like `immediate` on 64-bit platforms
  and like `value` on all other platforms (like JavaScript). In the sublayout relation,
  `immediate < immediate64 < value`. Types declared with `[@@immediate64]` have layout
  `immediate64`.
* `float64` is the layout of the `float#` unboxed float type.
* `bits32` is the layout of the `int32#` unboxed int32 type.
* `bits64` is the layout of the `int64#` unboxed int64 type.
* `word` is the layout of the `nativeint#` unboxed nativeint type.
* `any` is a layout that is the superlayout of all other layouts.  It doesn't correspond
  to a specific runtime representation. More information [below](#the-any-layout).

Over time, we'll be adding more layouts here.

# Layout annotation

You can annotate type variables of type declarations with a layout, like this:

```ocaml
type ('a : immediate) t1
type ('a : float64) t2
type ('a : immediate, 'b : bits32) t3
```

If you do not annotate a type variable, we use layout inference to figure out the layout,
though we know layout inference is incomplete in certain complicated scenarios with
mutually recursive type definitions. If layout inference does not work, you will get an
error asking you to write a layout annotation; we will never infer an incorrect layout. If
layout inference does not fix the layout of a type variable, it is defaulted to have
layout `value`.

You can also mark types as non-`value`s using the following syntax:

```ocaml
type t4 : float64
type t5 : value  (* redundant, but you can do it if you like *)
type t6 : bits64 = int64# (* redundant since the layout can be deduced from the rhs *)
```

A type declared with no `=` signs (often in a signature) and
no layout information defaults to layout `value`. Types with `=` signs deduce their layout
from their right-hand sides.

Annotations can also be used within type expressions:

```ocaml
module type S = sig
  (* An annotation at binding sites sets the layout of the universal variable.
     Unannotated variables have layout `value`. *)
  val f1 : ('a : float64) ('b : immediate) 'c. 'a -> 'b -> 'c

  (* As shown here, annotation can't be placed directly on arbitrary types.
     [(int : immediate)] would be invalid syntax. The same can be achieved with
     [(int as (_ : immediate))]. *)
  val f2 : (int as ('a : immediate)) -> ('a : value) -> 'a
end

(* Note that annotations are always treated as upper bounds.
   The following is valid: *)
type ('a : immediate) t
type ('a : value) t2 = 'a t
(* ^ This will get typed as [type ('a : immediate) t2 = 'a t] *)

(* Here are a few more places where you can write annotations *)
let f3 (type a : immediate): a -> a = fun x -> x
let f4 (type (a : immediate) (b : float64) c) (x : a) (y: b) (z: c) = x
let f5 x = (x : (_ : immediate))
let f6: type (a: bits32). a -> a = fun x -> x
```

Reference [the relevant section of the design proposal][kind annotation] for the full syntax.
The complete annotation design is not yet implemented and the syntax should be read
with `kind ::= layout-name` for now. It also provides reasoning around some design
decisions and contains additional examples.

# Unboxed numbers

We now have `float#`, `int32#`, `int64#`, and `nativeint#` types.
They are the types for unboxed numbers. These all are stored
without pointers; working with them does not cause any allocation.
Each unboxed numeric type has its own layout: `float# : float64`, `int32# : bits32`,
`int64# : bits64`, and `nativeint# : word`.

Using layouts, you can usefully make a synonym of `float#` (or any of
the other unboxed types) that has layout
`float64`, for example with `module M : sig type t : float64 ... end = struct type t =
float# ... end`.

Each numeric type has its own library for working with it: `float_u`,
`int32_u`, `int64_u`, and `nativeint_u`. (Outside of Jane Street, these will be
modules in the `janestreet_shims` library.)

* Unboxed constants are written with a prepended `#`.
  Examples include:

    * `#3.14  (* : float# *)`
    * `-#0.5  (* : float# *)`
    * `#1e9   (* : float# *)`
    * `#123l  (* : int32# *)`
    * `-#456L (* : int64# *)`
    * `#789n  (* : nativeint# *)`

* Unboxed numbers can be stored in local variables, passed to functions, returned from
  functions, and have limited support in records (details below).

* Unboxed numbers may *not* appear...
   * ... top-level in a module (e.g. you cannot have `val pi : float#`)
   * ... in a tuple (e.g. you cannot have `int32# * int32#`)
   * ... as a field of a constructor (e.g. you cannot have `| K of int64#` or `| K of {
     x : nativeint# }`)
   * ... as a field of a polymorphic variant constructor (e.g. you cannot have ``[ `K of
     float# ]``)
   * ... anywhere near the class system (no `int64#` methods or class variables or even
     parameters in constructors)

* You can make records containing all `float#`s. (This applies only to `float#`s, not
  other unboxed numbers.) Every field must be a `float#` (or
  actually have layout `float64`). No exceptions. This does *not* work for inline or
  `[@@unboxed]` records.

* Existing types all expect `value` arguments. Thus for basically any `t`, you cannot
  write `float# t` or `int64# t`.
  This includes obvious candidates like `float# array` or `float# option`.

* Existing ppxs expect to work with `value` types. Accordingly, using `deriving` with
  types that involve unboxed numbers will lead to inscrutable type errors, and other ppxs
  (such as e.g. `match%optional`) will fall over when given unboxed numbers. These
  failures will lead to hard-to-understand errors from the compiler, never unsafe code.

# The `any` layout

If all we know about a type is that its layout is
`any`, we cannot execute code using that type.

For example, it's fine to write this function type:

```ocaml
val f : ('a : any). 'a -> 'a (* valid as a type signature *)
```
> (See the [previous section](#layout-annotation) to learn more about the layout annotation used here)

But it's not possible to implement a function of that type:

```ocaml
let f (type a : any) (x : a) = x (* rejected by the compiler *)
```

This is because the compiler doesn't know how to work with data of a type
(calling convention, etc.) without knowing its concrete layout:

```
Error: This pattern matches values of type a
      but a pattern was expected which matches values of type
        ('a : '_representable_layout_1)
      The layout of a is any, because
        of the annotation on the abstract type declaration for a.
      But the layout of a must be representable, because
        it's the type of a function argument.
```

The main use case for layout `any` in its current form is with module
types. For example:

```ocaml
module type S = sig
  type t : any

  val add : t -> t -> t
  val one : unit -> t
  val print : t -> unit
end

module M1 : S with type t = float# = struct
  type t = float#

  let add x y = Float_u.add x y
  let one () = #1.
  let print t = Printf.printf "%f" (Float_u.to_float t)
end

module M2 : S with type t = int = struct
  type t = int

  let add x y = x + y
  let one () = 1
  let print t = Printf.printf "%d" t
end
```

Here by defining module type `S` with layout `any` and using `with` constraints, we can
reason about modules with similar shapes but that operate on different layouts. This removes code
duplication and can aid ppxs in supporting unboxed types.

# `[@layout_poly]` attribute

The attribute enables support for **limited layout polymorphism on external
`%`-primitives**. This is possible because these primitives are always inlined at every
use site. We can thus specialize the function implementation based on the layout
information at each site.

With a `[@layout-poly]` external declaration like this:

```ocaml
external[@layout_poly] opaque_identity : ('a : any). 'a -> 'a = "%opaque"
```

It means that `opaque_identity` can operate on any concrete layout and have all of these
types:

```ocaml
opaque_identity : ('a : float64). 'a -> 'a
opaque_identity : ('a : value). 'a -> 'a
opaque_identity : ('a : bits64). 'a -> 'a
...
```

The attribute changes the meaning of the layout annotation `(_ : any)` and turns `'a` into
a layout polymorphic type variable.

As a consequence of the specialization happening at every use site, this limited layout
polymorphic behavior does not propagate:

```ocaml
let f = opaque_identity
```

Here `f` can have one and only one of the types listed above:

```ocaml
let _ = f #1.
(* or *)
let _ = f 100
(* but NOT BOTH *)
```

The current implementation also restricts all layout polymorphic type variables to have
the same layout:

```ocaml
external[@layout_poly] magic : ('a : any) ('b : any). 'a -> 'b = "%obj_magic"

let f1 : int32# -> int32# = magic;; (* ok *)
let f2 : float# -> float# = magic;; (* ok *)
let f3 : float# -> int32# = magic;; (* error *)
```

This feature is conceptually similar to `[@local_opt]` for modes and would be useful for
array access primitives.

Here's the list of primitives that currently support `[@layout_poly]`:

* `%identity`
* `%opaque`
* `%obj_magic`
* `%ignore`
* `%revapply`
* `%apply`
* `%array_safe_get`
* `%array_safe_set`
* `%array_unsafe_get`
* `%array_unsafe_set`