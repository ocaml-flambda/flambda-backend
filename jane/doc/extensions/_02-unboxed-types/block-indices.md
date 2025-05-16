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

A block index is an opaque, explicit index to an element. The language feature includes these types in the predef:

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

In fact, in the native compiler, this block index is physically equivalent to
`#24L`, as `q`'s `y` occurs 24 bytes from the start of the record.

Accordingly, block indices can be used to read and write within blocks, polymorphically in the actual type of the block. This can be done via the functions bound in
these modules in `Stdlib_beta` (some details elided, e.g. the particular primitives bound by externals):
```ocaml
module Idx_imm : sig
  type ('a, 'b : any) t = ('a, 'b) idx_imm

  external unsafe_get
    : 'a ('b : any). ('a[@local_opt]) -> ('a, 'b) idx_imm -> ('b[@local_opt])
end

module Idx_mut : sig
  type ('a, 'b : any) t = ('a, 'b) idx_mut

  external unsafe_get
    : 'a ('b : any). ('a[@local_opt]) -> ('a, 'b) idx_mut -> ('b[@local_opt])

  external unsafe_set
    : 'a ('b : any). 'a @ local -> ('a, 'b) idx_mut -> 'b -> unit
end
```

Syntax is added for block index creation, e.g. `(.foo.#bar)`, which consists of
one "block access" (a record field, array index, iarray index, or another block
index) and zero or more "unboxed accesses" (currently, just an unboxed record
field).

If the block access is mutable (mutable record fields, arrays, mutable block
index), then an `idx_mut` is created, and if the block access is immutable
(immutable record fields, immutable arrays, immutable block indices), then an
`idx_imm` is created.

### Terminology



### Block index creation examples

- `(.

# Indices to records and arrays with special representations

- Indices to flat float arrays cannot be taken.
- An index to a float into a flattened float record has an element type `float#`.

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
implementation of deepening, see the below.

![All values or flats](assets/all_values_or_flats.png)
![Mixed to all flats](assets/mixed_to_all_flats.png)
![Mixed to all values](assets/mixed_to_all_values.png)
![Mixed to mixed](assets/mixed_to_mixed.png)
