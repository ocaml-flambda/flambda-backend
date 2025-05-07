---
layout: documentation-page
collectionName: Stack allocation
title: Intro
---

# Introduction to Stack Allocations

See also the full feature [reference](../reference) and
[common pitfalls](../pitfalls).

This page describes how OCaml sometimes allocates values on a stack,
as opposed to its usual behavior of allocating on the heap.
This helps performance in a couple of ways: first, the same few hot
cache lines are constantly reused, so the cache footprint is lower than
usual. More importantly, stack allocations will never trigger a GC,
and so they're safe to use in low-latency code that must currently be
zero-alloc.

Because of these advantages, values are allocated on a stack whenever
possible. Of course, not all values can be allocated on a stack: a value that is
used beyond the scope of its introduction must be on the heap. Accordingly,
the compiler uses the _locality_ of a value to determine where it will be
allocated: _local_ values go on the stack, while _global_ ones must go on the
heap.

Though type inference will infer where stack allocation is possible, it is
often wise to annotate places where you wish to enforce locality and stack
allocation. This can be done by labeling an allocation as a `stack_` allocation:

```ocaml
let x2 = stack_ { foo; bar } in
...
```

However, for this to be safe, stack-allocated values must not be used after
their stack frame is freed. This is ensured by the type-checker as follows.
A stack frame is represented as a _region_ at compile time, and each
stack-allocated value lives in the surrounding region (usually a function body).
Stack-allocated values are not allowed to escape their region. If they do,
you'll see error messages:

```ocaml
let foo () =
  let thing = stack_ { foo; bar } in
  thing
  ^^^^^
Error: This value escapes its region
```

Most allocations in OCaml can be stack-allocated: tuples, records, variants,
closures, boxed numbers, etc. Stack allocations are also possible from C stubs,
although this requires code changes to use the new `caml_alloc_local` instead of
`caml_alloc`. A few types of allocation cannot be stack-allocated, though,
including first-class modules, classes and objects, and exceptions. The contents
of mutable fields (inside `ref`s, `array`s and mutable record fields) also
cannot be stack-allocated.

The `stack_` keyword works shallowly: it only forces the immediately following allocation
 to be on stack. Putting it before an expression that is not an allocation (such as a
 complete function application) leads to a type error.

## Local parameters

Generally, OCaml functions can do whatever they like with their
arguments: use them, return them, capture them in closures or store
them in globals, etc. This is a problem when trying to pass around
stack-allocated values, since we need to guarantee they do not
escape.

The remedy is that we allow the `local_` keyword to appear on
function parameters:

```ocaml
let f (local_ x) = ...
```

A local parameter is a promise by a function not to let a particular
argument escape its region. In the body of f, you'll get a type error
if x escapes, but when calling f you can freely pass stack-allocated values as
the argument. This promise is visible in the type of f:

```ocaml
val f : local_ 'a -> ...
```

The function f may be equally be called with stack- or
heap-allocated values: the `local_` annotation places obligations only on the
definition of f, not its uses.

<!-- CR zqian: factor the generic mode stuff into a dedicated document. -->

Even if you're not interested in performance benefits, local
parameters are a useful new tool for structuring APIs. For instance,
consider a function that accepts a callback, to which it passes some
mutable value:

```ocaml
let uses_callback ~f =
  let tbl = Foo.Table.create () in
  fill_table tbl;
  let result = f tbl in
  add_table_to_global_registry tbl;
  result
```

Part of the contract of `uses_callback` is that it expects `f` not to
capture its argument: unexpected results could ensue if `f` stored a
reference to this table somewhere, and it was later used and modified
after it was added to the global registry. Using `local_`
annotations allows this constraint to be made explicit and checked at
compile time, by giving `uses_callback` the signature:

```ocaml
val uses_callback : f:(local_ int Foo.Table.t -> 'a) -> 'a
```

## Inference

The examples above use the `stack_` keyword to mark stack
allocations. In fact, this is not necessary, and the compiler will use
stack allocations by default where possible.

The only effect of the keyword on an allocation is to change the
behavior for escaping values: if the allocated value looks like it escapes
and therefore cannot be stack-allocated, then without the keyword
the compiler will allocate this value on the GC heap as usual, while
with the keyword it will instead report an error.

Inference can even determine whether parameters are local, which is
useful for helper functions. It's less useful for top-level functions,
though, as whether their parameters are local is generally forced by
their signature in the mli file, where no inference is performed.

Inference does not work across files: if you want e.g. to pass a local
argument to a function in another module, you'll need to explicitly
mark the local parameter in the other module's mli.

## More control

There are a number of other features that allow more precise control
over which values are stack-allocated, including:

  - **Stack-allocated closures**

    ```ocaml
    let f  = stack_ (fun a b c -> ...)
    ```

    defines a function `f` whose closure is itself stack-allocated.

  - **Local-returning functions**

    ```ocaml
    let f a = exclave_
      ...
    ```

    defines a function `f` which puts its stack-allocated values in its
    caller's region.

  - **Global fields**

    ```ocaml
    type 'a t = { global_ g : 'a }
    ```

    defines a record type `t` whose `g` field is always known to be
    heap-allocated (and may freely escape regions), even though the record
    itself may be stack-allocated.

For more details, read [the reference](../reference).
