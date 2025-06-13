---
layout: documentation-page
collectionName: Modes
title: Reference
---

The goal of this document is to be a reasonably complete reference to the mode system in
OxCaml.

<!-- CR zqian: For a gentler introduction, see [the introduction](../intro). -->

The mode system in the compiler tracks various properties of values, so that certain
performance-enhancing operations can be performed safely. For example:
- Locality tracks escaping. See [the local allocations
  reference](../../stack-allocation/reference)
- Uniqueness and linearity tracks aliasing. See [the uniqueness reference](../../uniqueness/reference)
- Portability and contention tracks inter-thread sharing.
    <!-- CR zqian: reference for portability and contention -->

# Lazy
`lazy e` contains a thunk that evaluates `e`, as well as a mutable cell to store the
result of `e`. Upon construction, the mode of `lazy e` cannot be stronger than `e`. For
example, if `e` is `nonportable`, then `lazy e` cannot be `portable`. Upon destruction
(forcing a lazy value), the result cannot be stronger than the mode of lazy value. For
example, forcing a `nonportable` lazy value cannot give a `portable` result. Additionally,
forcing a lazy value involves accessing the mutable cell and thus requires the lazy value
to be `uncontended`.

Currently, the above rules don't apply to the locality axis, because both the result and
the lazy value are heap-allocated, so they are always `global`.

Additionally, upon construction, the comonadic fragment of `lazy e` cannot be stronger
than the thunk. The thunk is checked as `fun () -> e`, potentially closing over variables,
which weakens its comonadic fragment. This rule doesn't apply to several axes:
- The thunk is always heap-allocated so always `global`.
- Since the thunk is only evaluated if the lazy value is `uncontended`, one can construct
a lazy value at `portable` even if the thunk is `nonportable` (e.g., closing over
`uncontended` or `nonportable` values). For example, the following is allowed:
```ocaml
let r = ref 0 in
let l @ portable = lazy (r := 42) in
```
- Since the thunk runs at most once even if the lazy value is forced multiple times, one
can construct the lazy value at `many` even if the thunk is `once` (e.g., closing over
`unique` or `once` values). For example, the following is allowed:
```ocaml
let r = { x = 0 } in
let l @ many = lazy (overwrite_ r with { x = 42 })
```

# Exception
Currently the exception type `exn` crosses portability and contention. To make
that safe, exception constructors are assigned a portability. Such a constructor
is portable iff all of its arguments cross portability and contention. A
portable function cannot close over a nonportable constructor, whether for
constructing or matching exceptions.

In the following example, `Foo` is nonportable because its argument doesn't
cross contention; similarly, `Bar` is nonportable because its argument doesn't
cross portability. As a result, neither of `foo` and `bar` can be marked as
portable.
```ocaml
exception Foo of int ref
exception Bar of unit -> unit

let (foo @ portable) () =
    try () with
    Foo _ -> ()

let (bar @ portable) () =
    try () with
    Bar _ -> ()
```

In the following example, `Baz` is nonportable, but `foo` doesn't close over
`Baz` and can be portable.
```ocaml
let (foo @ portable) () =
    let module M = struct
        exception Baz of int ref * (unit -> unit)
    end in
    raise (M.Baz (ref 42, fun () -> ()))
```
