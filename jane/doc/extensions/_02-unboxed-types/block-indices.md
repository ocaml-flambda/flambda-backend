---
layout: documentation-page
collectionName: Unboxed types
title: Block indices
---

# Block indices

This document describes the language feature and implementation for explicit
_indices_ into a block. Before reading this document, you may wish to read up
through the [layouts](../intro#layouts) section of the main document.

As a quick example:
```ocaml
open Stdlib_beta

type pt = { x : int; y : int }
type line = { p : pt#; q : pt# }

(* Creating an immutable block index into a nested record *)
let mk_idx () : (line, int) idx_imm = (.q.#y)

let get_coord (line : line) (i : (line, int) idx_imm) : int =
  (* Equivalent to [line.q.#y] *)
  Idx_imm.unsafe_get line i

(* Creating a block index into an array *)
let first_x (): (pt# array, int) idx_mut = (.(0).#x)

let inc_coord (pts : 'a) (i : ('a, int) idx_mut) =
  (* Equivalent to [pts.(i) <- pts.(i) + 1] when [i] is in bounds and [pts] is
     an [int array]. But this function could also be used update a mutable
     record containing an [int]. *)
  Idx_mut.unsafe_set pts i (Idx_mut.unsafe_get pts i + 1)
```

# Overview

A block index is an opaque, explicit index to an element. The language feature
includes these predefined types:

```ocaml
type ('a, 'b : any) idx_imm : bits64
type ('a, 'b : any) idx_mut : bits64
```

Given `('a, 'b) idx_imm` or `('a, 'b) idx_mut`, we refer to `'a` as the "base
type" and `'b` as the "element type." A block index thus represents the position
of an element type within the base type. For example,
`(.q.#y) : (line, int) idx_imm` in the example above represents the position of
this `int` in a `line`:
```
                           v
{ p = #{ x; y }; q = #{ x; y } }
```

**Index creation.** Block index creation uses the syntax `(.foo.#bar)`.
Specifically, it consists of one "block access" followed by zero or more
"unboxed accesses" within parentheses.

Block accesses take the following forms:
- `.foo` (record field)
- `.(i)` (array index)
- `.:(i)` (iarray index)
- `.idx_imm(idx)` (immutable block index)
- `.idx_mut(idx)` (mutable block index)

Unboxed accesses take the following forms:
- `.#bar` (unboxed record field)

**Index mutability.** If the block access is mutable (mutable record fields,
arrays, and mutable block indices), then an `idx_mut` is created, and if the
block access is immutable (immutable record fields, immutable arrays, and
immutable block indices), then an `idx_imm` is created.

**Using indices.** Naturally, block indices can be used to read and write
within blocks. This can be done via the `Idx_imm.unsafe_get`,
`Idx_mut.unsafe_get`, and `Idx_mut.unsafe_set` functions in `Stdlib_beta`.

_The key advantage of block indices is that these accessor functions are
polymorphic in both the base type and element type._ Index reading roughly
(ignoring mutability, layouts, modes) has the type signature
`'a -> ('a, 'b) idx -> 'b`, and index writing roughly has the type
signature `'a -> ('a, 'b) idx -> 'b -> unit`.

**Index deepening.** Block indices themselves are included as a type of block
access so that indices can be _deepened_. For example, given
`idx : ('a, pt#) idx_imm`, one may obtain
`(.idx_imm(idx).#y) : ('a, int) idx_imm`.

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

(* ... *)

type pt = { x : int; y : int }
type line = { mutable p : pt#; mutable q : pt# }

let drop_last_to_y_axis (s : line Stack.t) =
  Stack.update_top s (.q.#y) 0
```

## Implement "interior pointers"

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
the native representation does. When the below refers to the "left" and
"right" of the element, this refers to the layout, *not* the native
representation.

`o1` and `g1` refer to the offset and gap of the index before deepening (light
orange), while `o2` and `g2` refer to the offset and gap of the index after
deepening (blue). Recall that indices to all values/non-values have no gap.

![All values or flats](assets/all_values_or_flats.png =500x)
![Mixed to all flats](assets/mixed_to_all_flats.png =500x)
![Mixed to all values](assets/mixed_to_all_values.png =500x)
![Mixed to mixed](assets/mixed_to_mixed.png =500x)
