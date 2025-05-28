---
layout: documentation-page
collectionName: Unboxed types
title: Intro
---

The "unboxed types" extension provides users with additional control over the
way their data is represented in memory and registers. These new types have
different *layouts*, which is part of their [kind](../../kinds/intro), to
distinguish them from normal OCaml types.

This page gives a comprehensive overview of the extension.  Unboxed types are
still in active development, with new features being added frequently, and the
documentation here is kept up to date.

# Layouts

Every type is now classified by a *layout*, much like how every expression is classified
by a *type*. The type system knows about a collection of fixed *base* layouts:

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
* `float32` is the layout of the `float32#` unboxed 32-bit float type.
* `bits32` is the layout of the `int32#` unboxed int32 type.
* `bits64` is the layout of the `int64#` unboxed int64 type.
* `vec128` is the layout of 128-bit unboxed SIMD vector types.
* `word` is the layout of the `nativeint#` unboxed nativeint type.
* `any` is a layout that is the superlayout of all other layouts.  It doesn't correspond
  to a specific runtime representation. More information [below](#the-any-layout).

* `value_or_null` is a superlayout of `value` including normal OCaml values
  and null pointers. Unless `-extension-universe alpha` is set, it is displayed
  as `value` and can't be used in jkind annotations.

* `any_non_null` is a sublayout of `any` forbidding null pointers. Unless
  `-extension-universe alpha` is set, it is displayed as `any`.
  Additionally, `any` jkind annotations are interpreted as `any_non_null` for
  backwards compatibility for definitions using arrays.

The type system also supports one *composite* layout: unboxed products:
* `l1 & l2 & ... & lk` is the layout of unboxed products where the first element
  of the project has layout `l1`, the second has layout `l2`, and so on.

Over time, we'll be adding more layouts here.

## Layout annotation

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

The full syntax can be found in the [documentation for kinds](../../kinds/syntax).
The complete annotation design is not yet implemented and the syntax should be
read with `kind ::= layout-name` for now. It also provides reasoning around some
design decisions and contains additional examples.


## Layouts in module inclusion

Layouts are part of kinds, and therefore work just like kinds for the purposes
of module inclusion. See the [kinds documentation](../../kinds/intro#inclusion-and-variance) for more.

# Unboxed numbers

We now have `float#`, `int32#`, `int64#`, `nativeint#`, and unboxed 128-bit vector types.
They are the types for unboxed numbers. These all are stored
without pointers; working with them does not cause any allocation.

Most unboxed numeric types have their own layout: `float# : float64`, `int32# : bits32`,
`int64# : bits64`, `nativeint# : word`.

All of the 128-bit vectors have the same layout: `float32x4# : vec128`,
`float64x2# : vec128`, `int8x16# : vec128`, `int16x2# : vec128`, `int32x4# : vec128`,
and `int64x2# : vec128`.

Using layouts, you can usefully make a synonym of `float#` (or any of
the other unboxed types) that has layout
`float64`, for example with `module M : sig type t : float64 ... end = struct type t =
float# ... end`.

Each numeric type has its own library for working with it: `float_u`,
`int32_u`, `int64_u`, and `nativeint_u` (all in the `janestreet_shims` library).

* Unboxed constants are written with a prepended `#`.
  There is no literal syntax for unboxed vectors: use the `Ocaml_simd_sse` library instead.
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

* Unboxed numbers may be stored in structures, with
  [some restrictions](#using-unboxed-types-in-structures). Additionally, blocks
  in which every field is a `float` or has layout `float64` are represented as
  flat float arrays. Unboxed numbers may *not* be stored in inline or
  `[@@unboxed]` records.

* With a few specific exceptions (documented below), existing types all expect
  `value` arguments. Thus for basically any `t`, you cannot write `float# t` or
  `int64# t`.  This includes natural candidates like `float#
  option`.

* Existing ppxs expect to work with `value` types. Accordingly, using `deriving` with
  types that involve unboxed numbers will lead to inscrutable type errors, and other ppxs
  (such as e.g. `match%optional`) will fall over when given unboxed numbers. These
  failures will lead to hard-to-understand errors from the compiler, never unsafe code.

## Unboxed options

We now have `type 'a or_null : value_or_null`, the type of unboxed options.
It has constructors `Null` and `This v`. See the [`or_null` document](../or-null)
for more details.

# Unboxed products

The unboxed product layout describes types that work like normal products (e.g.,
tuples or records), but which are represented without a box.

In stock OCaml, a tuple is a pointer to a block containing the elements of the tuple. If
you pass a tuple to a function, it is passed by reference in one register. The
function can access the tuple's elements through the pointer. Records and
their fields are treated similarly. By contrast, an
unboxed product does not refer to a block at all. When used as a function
argument or return type, its elements are passed separately in their own
registers, with no indirection (or on the call stack, if the product has more
elements than there are available registers).

Currently, types that have unboxed product layouts are *unboxed tuples* and
*unboxed records*.

Unboxed tuples are written `#(...)`, and may have labels just like normal tuples.
So, for example, you can write:
```ocaml
module Flipper : sig
  val flip : #(int * float# * lbl:string) -> #(lbl:string * float# * int)
end = struct
  let flip #(x,y,~lbl:z) = #(~lbl:z,y,x)
end
```

Unboxed records are defined, constructed, and matched on like normal records, but with
a leading hash. Fields are projected with `.#`. For example:
```ocaml
type t = #{ f : float# ; s : string }
let inc #{ f ; s } = #{ f = Float_u.add f #1.0 ; s }
let get_s t = t.#s
```

The field names of unboxed records occupy a different namespace from the
field names of "normal" (including `[@@unboxed]`) records.

Unboxed tuples and records may be nested within other unboxed tuples and records.
There are no limitations on the layouts of the elements of unboxed tuples, but the fields
of unboxed records must be representable.

*Limitations and future plans*:
* Unboxed products may not currently placed in blocks.
  We plan to lift this restriction in the near future.
* Unboxed record fields may not be mutable.
  We plan to allow mutating unboxed records within boxed records
  (the design will differ from boxed record mutability, as unboxed types don't have the
  same notion of identity).
* Unboxed record fields must be representable.
  We plan to lift this restriction in the future.
* We plan to add other types with unboxed product layouts (e.g., interior pointers).

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

<!-- This heading is referred to by name in a link to an HTML anchor below.
     If you rename it, please also update that link.
-->
# `[@layout_poly]` attribute

The attribute enables support for **limited layout polymorphism on external
`%`-primitives**. This is possible because these primitives are always inlined at every
use site. We can thus specialize the function implementation based on the layout
information at each site.

With a `[@layout_poly]` external declaration like this:

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

This feature is conceptually similar to `[@local_opt]` for modes and is useful for
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
* `%array_size`

# Arrays of unboxed elements

Arrays can store elements of any layout. You can think of `array` as having been declared as:

```ocaml
type ('a : any) array = (* ... *)
```

Array elements are packed according to their width. For example, arrays of
elements whose layout is `bits32` store two elements per word.

You can use normal array syntax for constructing such an array:

```ocaml
let array = [| #2l |]
```

Array primitives must be declared with `[@layout_poly]` to be usable with arrays of unboxed elements.

```ocaml
module Array = struct
  external[@layout_poly] get : ('a : any). 'a array -> int -> 'a = "%array_safe_get"
end

let first_elem () = array.(0)
```

(The above relies on the fact that array projection syntax desugars to a call to whatever `Array.get` is in scope.)

A limited set of primitives may be bound as `[@layout_poly]`;
[see the earlier section](#layout_poly-attribute) for more information.

## Runtime representation

| Array                                          | Tag                | Layout of data                                               |
|----------------------------------              |--------------------|--------------------------------------------------------------|
| `('a : float64) array`                         | `Double_array_tag` | 64 bits per element                                          |
| `('a : bits64) array`                          | `Custom_tag`       | reserved custom block word, followed by 64 bits per element  |
| `('a : float32) array`, `('a : bits32) array`  | `Custom_tag`       | reserved custom block word, followed by 32 bits per element  |
| `('a : vec128) array`                          | `Custom_tag`       | reserved custom block word, followed by 128 bits per element |

The reserved custom block word is the standard custom block field that stores a
pointer to the record of custom operations, like polymorphic equality and
comparison. For unboxed 32-bit element types, like `int32#` and `float32#`, the
custom operations pointer is different for odd-length arrays and even-length
arrays.

Odd-length arrays of 32-bit element type have 32 bits of padding at the end.
The contents of this padding is unspecified, and it is not guaranteed that
the padding value will be preserved by the generated code or the runtime.

# Using unboxed types in structures

Unboxed types can usually be put in structures, though there are some restrictions.

These structures may contain unboxed types:

  * Records
  * Constructors

Unboxed numbers can't be put in these structures:

  * Constructors with inline record fields
  * Exceptions
  * Extensible variant constructors
  * Top-level fields of modules
  * Tuples

There aren't fundamental issues with the structures that lack support. They will
just take some work to implement.

Here's an example of a record with an unboxed field. We call such a record
a "mixed record", and it is represented at runtime by a "mixed block".

```ocaml
type t =
  { str : string;
    i : int;
    f : float#;
  }
```

## The "mixed block" representation

The runtime representation of mixed blocks is slightly different than normal
OCaml blocks. These differences are present to accomodate the garbage collector,
which must scan the fields with layout `value`, but not the fields containing
unboxed types.

To enable this, the header word of mixed blocks remembers how many elements of
the block are values, with a maximum of 254. The compiler _reorders_ the fields
of your block so that all the values are first, and the GC knows to stop
scanning after it has seen that number of fields.

For example, consider this record type:
```ocaml
type t =
  { w : float#;
    x : string;
    y : int64#;
    z : int
  }
```

The compiler will represent this type with a block where the fields are in the
order `x`, `z`, `w`, `y`.

The reordering is invisible to source-level ocaml programs that don't use unsafe
features, but can be relevant when writing C bindings or OCaml code that depends
on the runtime representation of values. It is stable in the sense that it never
changes the relative order of two values, or of two non-values.  Immediates
count as values for this purpose (they are always moved to the prefix).

There is a special case for for records that consist solely of `float` and
`float#` fields. The "flat float record optimization" applies to any such
record&mdash;all of the fields are stored flat, even the `float` ones that will
require boxing upon projection. The fields are also not reordered. This special
case exists to provide a better migration story for all-`float` records to which
the flat float record optimization currently applies.

Blocks may contain unboxed products, in which case the products are "flattened"
to become individual fields of the block, and reordered to accomodate the mixed
block representation.  For example, consider this record type:

```ocaml
type t =
  { a : float#;
    b : #(w:float# * #(x:string * y:int64#) * z:(int * int));
    c : bool }
```
This is represented as a block with six fields, and the fields appear the order
`x`, `z`, `c`, `a`, `w`, `y`.

## Generic operations aren't supported

Some operations built in to the OCaml runtime aren't supported for structures
containing unboxed types.

These operations aren't supported:

  * polymorphic comparison and equality
  * polymorphic hash
  * marshaling

These operations raise an exception at runtime, similar to how polymorphic
comparison raises when called on a function.

You should use ppx-derived versions of these operations instead.

## Depending on the layout of mixed blocks

The implementation of field layout in a mixed block is not finalized. For example, we'd like for int32 fields to be packed efficiently (two to a word) on 64 bit platforms. Currently that's not the case: each one takes up a word.

As a result, code that depends on the way mixed blocks are represented in memory
(e.g., via C bindings) may need updates in the future. To help manage this,
OxCaml provides mechanisms to assert your code depends on the current
representation. The mechanism depends on whether you are writing C bindings
or (unsafe) OCaml code.

Note also that, while unboxed types are generally considered an "upstream
compatible" (because they can be erased while preserving behavior), depending on
the exact representation of mixed blocks is not. Thus, use of these mechanism is
also a sign that your code may need a custom mechanism if it is intended to work
both in OxCaml and upstream OCaml.

### In C bindings

To ensure that your C code will need to be updated when the layout changes, use
the `Assert_mixed_block_layout_v#` family of macros. For example,

```
Assert_mixed_block_layout_v3;
```

Write the above in statement context, i.e. either at the top-level of a file or
within a function.

### In OCaml code

Users who write OCaml code that depends on the layout of mixed blocks (via
`Obj.magic` or similar) should instead include a reference in the relevant
modules to `Stdlib_upstream_compatible.mixed_block_layout_v#`. For example:
```
let _ = Stdlib_upstream_compatible.mixed_block_layout_v3
```

### Example

Here's a full example. Say you're writing C bindings against this OCaml type:

```ocaml
(** foo.ml *)
type t =
  { x : int32#;
    y : int32#;
  }
```

Here is the recommend way to access fields:

```c
Assert_mixed_block_layout_v3;
#define Foo_t_x(foo) (*(int32_t*)&Field(foo, 0))
#define Foo_t_y(foo) (*(int32_t*)&Field(foo, 1))
```

### Future changes and history

We will bump the version number if make changes to the layout of mixed
blocks. For example, it will be bumped if:

  * We change what word half the int32 is stored in
  * We start packing int32s more efficiently

When we bump the version, the C assertion for the previous version will fail at
compile time, and the OCaml definition for the previous version will be removed
from the standard library. This alerts maintainers of code using these
mechanisms to consider whether that code needs updates.

Version history:

- `v1`: initial implementation;
- `v2`: automatic reordering by the front- and middle-ends;
- `v3`: automatic flattening of nested unboxed records.
