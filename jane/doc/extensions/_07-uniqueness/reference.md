---
layout: documentation-page
collectionName: Uniqueness
title: Reference
---

# Uniqueness Reference

The goal of this document is to be a reasonably complete reference to the
uniqueness mode. For a gentler introduction, see [the introduction](../intro).
It will grow over time as we add more features such as overwriting and
borrowing.

## More details on uniqueness checking

### Mixing unique and aliased uses

Continuing the example from [the introduction](../intro), we can have both a
unique and an aliased access to a value, as long as those occur in separate
branches:

```ocaml
val free : t @ unique -> unit
val store : t @ aliased -> unit

let okay t =
  if flip_coin ()
  then free t
  else (store t; store t)
```

This might be surprising coming from a language that uses ownership like Rust.
In Rust, each value needs to have exactly one owner which is responsible for its
deallocation. But in OCaml, we can rely on the GC for deallocation which makes
it possible to give up ownership of the value in the second branch.

In particular, this enables a programming pattern to create data structures in
two phases: In the first phase, the data structure is unique and we can mutate
it safely and efficiently. In the second phase, we relinquish the uniqueness and
the data structure becomes an ordinary OCaml value which can be shared freely.

### Unique uses of different fields

The uniqueness analysis makes it possible to consume some fields of an
allocation without losing access to the allocation or its other fields. For
example, you might write:

```ocaml
let okay r =
  free r.field1;
  match r with
  | { field2; _ } -> free field2
```

This is fine: we free the first field of `r`, then pattern match on `r` itself
(which was not affected by the free of its child) and then free the second
child. While `r` technically still keeps a reference to `r.field1`, it is never
dereferenced after the free.

However, it would not be fine to free the first field and then free `r` itself:

```ocaml
let bad r =
  free r.field1;
  match r with
  | { field2; _ } -> free r
```

The problem here is that the compiler can not determine whether `free r` might
dereference `r.field1` again. As such, it would be unsafe to free `r.field1`.
The uniqueness analysis carefully tracks which children of an allocation have
already been consumed. This tracking also works with aliases, such as
renamings:

```ocaml
let okay r =
  let x = r in
  free x.field1;
  match r with
  | { field2; _ } -> free field2
```

However, which children have been consumed is not tracked through more
complicated aliases, such as function calls:

```ocaml
let bad r =
  let x = Fun.id r in
  free x.field1;
  match r with
  | { field2; _ } -> free field2
```

or new allocations:

```ocaml
let bad r =
  let x = { field1 = r.field1; field2 = r.field2 } in
  free x.field1;
  match r with
  | { field2; _ } -> free field2
```

## Matching on tuples

In OCaml, it is possible to simultaneously match on multiple values:

```ocaml
match x, y, z with
| p, q, r -> ...
```

There is in fact no special syntax for this: as parentheses are optional in
tuples, the above is actually a match on a single value, the tuple `(x, y, z)`,
against a single pattern, the pattern `(p, q, r)`. Normally, this would mean
that `x`, `y` and `z` have the same mode. However, just as for other modes, we
check this code as if it was special syntax and the modes of `x`, `y`, `z` may
be unrelated.

Furthermore, we track aliasing through tuples that are immediately matched on:

```ocaml
let okay x y z =
  match x, y, z with
  | p, q, r -> free x.field1; free p.field2
```

Here we free both the first and the second field of `x`. However, this changes
when the tuple is explicitly named:

```ocaml
let bad x y z =
  match x, y, z with
  | p, q, r as t -> free x.field1; free p.field2
```

In this case, the OCaml compiler may actually allocate a tuple `(x, y, z)` at
runtime and we now consider the value `x` to be consumed by the tuple. That
makes it impossible to free its first field, which can now be referenced through
the new allocation `t`.
