---
layout: documentation-page
collectionName: Unboxed types
title: Block indices
---

# Block indices

This document describes the language feature and implementation for explicit
_indices_ into a block. Before reading this document, you may wish to read up
through the [layouts](../intro#layouts) section of the main document.

```ocaml
open Stdlib_beta

type pt = { x : int; y : int }
type line = { p : pt#; q : pt# } (* flatly contains four ints *)

let i : (line, int) idx_imm = (.q.#y) (* an index to the second y-coord of a line *)
let get_coord (line : line) (i : (line, int) idx_imm) : int =
  Idx_imm.unsafe_get line i
```

If the index path is to a mutable element (it is in an array or mutable record
field), then it is an `idx_mut`.

```ocaml
let first_x : (pt# array, int) idx_mut = (.(0).#x)
let inc_coord (pts : 'a) (i : ('a, int) idx_mut) =
  Idx_mut.unsafe_set pts i (Idx_mut.unsafe_get pts i + 1)
```

# Overview

A block index is an opaque, explicit index to an element. The language feature
includes these types in the predef:

```ocaml
type ('a, 'b : any) idx_imm : bits64
type ('a, 'b : any) idx_mut : bits64
```

For `('a, 'b) idx_imm` and `('a, 'b) idx_mut`, we refer to `'a` as the "base
type" and `'b` as the "element type."

A block index represents the position of an element type within the base type.

For example, `(.q.#y) : (line, int) idx_imm` in the example above represents
the position of this int in a `line`:
```
                           v
{ p = #{ x; y }; q = #{ x; y } }
```


Accordingly, block indices can be used to read and write within blocks,
polymorphically in the actual type of the block. This can be done via the
`Idx_imm.unsafe_get`, `Idx_mut.unsafe_get`, and `Idx_mut.unsafe_set` functions
in `Stdlib_beta`.

Syntax is added for block index creation, e.g. `(.foo.#bar)`, which consists of
one "block access" (a record field, array index, iarray index, or another block
index) and zero or more "unboxed accesses" (currently, just an unboxed record
field).

If the block access is mutable (mutable record fields, arrays, mutable block
index), then an `idx_mut` is created, and if the block access is immutable
(immutable record fields, immutable arrays, immutable block indices), then an
`idx_imm` is created.

# Use cases

## Allow polymorphic APIs to support fine-grained access

```ocaml
module Stack : sig
 type 'a t
 val empty : 'a t
 val push : 'a t -> 'a -> unit
 val pop : 'a t -> 'a
 val update_top : 'a t -> ('a, 'b) idx_mut -> 'b -> unit
 (* [update_top] normally isn't possible without exposing the representation.
    One could use ['a t -> ('a -> unit) -> unit], but the closure is less
    efficient. *)
end

type pt = { x : int; y : int }
type line = { mutable p : pt#; mutable q : pt# }

let drop_last_to_y_axis (s : line Stack.t) =
 update_top s &.q.y 0
```

## Implement ``interior pointers''

By packing the base type parameter, block indices can be used to implement
pointers into a block. (There is ongoing work to create a standardized library
for interior pointers, but we include this example for illustrative purposes.)

```ocaml
type 'a iptr = P : #('base, ('base, 'a) idx_imm) -> 'a iptr [@@unboxed]
type 'a mptr = P : #('base, ('base, 'a) idx_mut) -> 'a mptr [@@unboxed]
```

# Edge cases and limitations

1. Indices to flat float arrays cannot be taken: the type parameter must be
   `non_float`.
2. Indices to `[@@unboxed]` records cannot be taken.
3. An index to a `float` into a flattened float record has an element type
   `float#`.
4. Indices to some records containing both values and non-values, and occupying
   over 2^16 bytes, cannot be created. See "Representation of block indices" for
   details.

# Representation of block indices

Consider the following type:

```ocaml
type a = #{ s : string; i : int64# }
type b = #{ i : int64#; a : a; s : string }
type c = { mutable b : b; s : string }
```
The layout of `c` has the shape
`((b_i64, (a_string, a_i64), b_string), c_string)`,
whose representation differs between the native and bytecode compilers.

In the native code compiler, it gets reordered (a stable two-color sort that
puts values before non-values):
```
   a_string b_string c_string b_i64 a_i64
b  ^^^^^^^^^^^^^^^^^          ^^^^^^^^^^^
a  ^^^^^^^^                         ^^^^^
```

In the bytecode compiler, unboxed records are actually boxed, and not
reordered.

Acccordingly, block indices have also two different representations. In the
native compiler, they are the offset into the block and the gap between the
values and non-values of the pointed-to payload, both in bytes. In the
bytecode compiler, block indices are represented as a sequence of field
positions.

| Idx in `c` | Native repr. | Bytecode repr. |
|------------|--------------|----------------|
| `(.b)` | offset 0, gap 8 | { 0 } |
| `(.s)` | offset 16, gap 0 | { 1 } |
| `(.b.#i)` | offset 24, gap 0 | { 0; 0 } |
| `(.b.#a)` | offset 0, gap 24 | { 0; 1 } |
| `(.b.#s)` | offset 8, gap 0 | { 0; 2 } |
| `(.b.#a.#s)` | offset 0, gap 0 | { 0; 1; 0 } |
| `(.b.#a.#i)` | offset 32, gap 0 | { 0; 1; 1 } |


In-memory representation:
- In the native compiler, the offset and gap are packed into
  a single `bits64`. There are two subcases:
  * The index is to product containing both values and non-values. In this
    case, the offset is the lower 48 bits and the gap is the upper 16 bits.
  * The index is to all values/non-values. In this case, all 64 bits are used
    for the offset.
- In the bytecode compiler, the field positions are stored as tagged integers
  in single block with tag 0.
  * Unboxed record fields in the index into singleton unboxed records are
    _not_ included in the list of positions, as singleton unboxed records are
    erased during translation to lambda.

For a visualization of the native representation of block indices, and the
implementation of deepening, see below.

## Native implementation of index deepening

We show compare the "layout view" of deepening with the "native representation"
view of deepening. Note that the layout does not account for reordering while
the native representation does. When the below refers to what is "left" and
"right" of the element, this refers to the layout, *not* the native
representation.

![All values or flats](assets/all_values_or_flats.png)
![Mixed to all flats](assets/mixed_to_all_flats.png)
![Mixed to all values](assets/mixed_to_all_values.png)
![Mixed to mixed](assets/mixed_to_mixed.png)
