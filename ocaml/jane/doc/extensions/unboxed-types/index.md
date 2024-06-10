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

Array operations must be declared with `[@layout_poly]` to be usable with arrays of unboxed elements.

```ocaml
module Array = struct
  external[@layout_poly] get : ('a : any). 'a array -> int -> 'a = "%array_safe_get"
end

let first_elem () = array.(0)
```

(The above relies on the fact that array projection syntax desugars to a call to whatever `Array.get` is in scope.)

## Runtime representation

| Array                            | Tag                | Layout of data                                              |
|----------------------------------|--------------------|-------------------------------------------------------------|
| `float# array`                   | `Double_array_tag` | 64 bits per element                                         |
| `int64# array`                   | `Custom_tag`       | reserved custom block word, followed by 64 bits per element |
| `float32# array`, `int32# array` | `Custom_tag`       | reserved custom block word, followed by 32 bits per element |

The reserved custom block word is the standard custom block field that stores a pointer to the
record of custom operations, like polymorphic equality and comparison. (For unboxed 32-bit element types,
it also tracks whether the array stores an odd or even number of elements.)

The above table is written about concrete types like `float#` and `int64#`, but actually holds
for all types of the relevant layout.

# Using unboxed types in structures

Unboxed types can usually be put in structures, though there are some restrictions.

These structures may contain unboxed types, but have some restrictions on field
orders:
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
a "mixed record".

```ocaml
type t =
  { str : string;
    i : int;
    f : float#;
  }
```

## Restrictions on field ordering

The below is written about record fields but equally applies to constructor
arguments.

Suppose a record contains any unboxed field `fld` whose layout is not `value`[^or-combination-of-values]. Then, the following restriction applies: All
fields occurring after `fld` in the record must be "flat", i.e. the GC can
skip looking at them. The only options for flat fields are immediates (i.e. things
represented as ints at runtime) and other unboxed numbers.

[^or-combination-of-values]: Technically, there are some non-value layouts that don't hit this restriction, like unboxed products and unboxed sums consisting only of values.

The following definition is rejected, as the boxed field `s : string` appears
after the unboxed float field `f`:

```ocaml
type t_rejected =
  { f : float#;
    s : string;
  }
  (* Error: Expected all flat fields after non-value field, f,
            but found boxed field, s. *)
```

The only relaxation of the above restriction is for records that consist
solely of `float` and `float#` fields. Any ordering of `float` and `float#`
fields is permitted. The "flat float record optimization" applies to any
such record&mdash;all of the fields are stored flat, even the `float` ones
that will require boxing upon projection. The ordering restriction is relaxed
in this case to provide a better migration story for all-`float` records
to which the flat float record optimization currently applies.

```ocaml
type t_flat_float =
  { x1 : float;
    x2 : float#;
    x3 : float;
  }
```

The ordering restriction has to do with the "mixed block" runtime
representation. Read on for more detail about that.

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

## Runtime representation: mixed blocks

As a general principle: The compiler should not change the user-specified
field ordering when deciding the runtime representation.

Abiding by this principle allows you to write C bindings and
predict hardware cache performance.

A structure containing unboxed types is represented at runtime as a "mixed
block". A mixed block always consists of fields the GC can-or-must scan followed by
fields the GC can-or-must skip[^can-or-must]. The garbage collector must be kept
informed of which fields of the block it should scan. A portion of the header
word is reserved to track the length of the prefix of the block that should be
scanned by the garbage collector.

[^can-or-must]: "Can-or-must" is a bit of a mouthful, but it captures the right nuance. Pointer values *must* be scanned, unboxed number fields *must* be skipped, and immediate values *can* be scanned or skipped.

The ordering constraint on structure fields is a reflection of the same
ordering restriction in the runtime representation. 

## C bindings for mixed blocks

The implementation of field layout in a mixed block is not finalized. For example, we'd like for int32 fields to be packed efficiently (two to a word) on 64 bit platforms. Currently that's not the case: each one takes up a word.

Users who write C bindings might want to be notified when we change this layout. To ensure that your code will need to be updated when the layout changes, use the `Assert_mixed_block_layout_v#` family of macros. For example,

```
Assert_mixed_block_layout_v1;
```

Write the above in statement context, i.e. either at the top-level of a file or
within a function.

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
Assert_mixed_block_layout_v1;
#define Foo_t_x(foo) (*(int32_t*)&Field(foo, 0))
#define Foo_t_y(foo) (*(int32_t*)&Field(foo, 1))
```

We would bump the version number in either of these cases, which would prompt you to think about the code:

  * We change what word half the int32 is stored in
  * We start packing int32s more efficiently
