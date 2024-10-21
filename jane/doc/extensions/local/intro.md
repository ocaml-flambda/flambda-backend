# Introduction to Local Allocations

See also the full feature [reference](reference.md) and [common pitfalls](pitfalls.md).

Instead of allocating values normally on the GC heap, local
allocations allow you to stack-allocate values using the new `local_`
keyword:

```ocaml
let local_ x = { foo; bar } in
...
```

or equivalently, by putting the keyword on the expression itself:

```ocaml
let x = local_ { foo; bar } in
...
```

These values live on a separate stack, and are popped off at the end
of the _region_. Generally, the region ends when the surrounding
function returns, although read [the reference](reference.md) for more
details.

This helps performance in a couple of ways: first, the same few hot
cache lines are constantly reused, so the cache footprint is lower than
usual. More importantly, local allocations will never trigger a GC,
and so they're safe to use in low-latency code that must currently be
zero-alloc.

However, for this to be safe, local allocations must genuinely be
local. Since the memory they occupy is reused quickly, we must ensure
that no dangling references to them escape. This is checked by the
type-checker, and you'll see new error messages if local values leak:

```ocaml
# let local_ thing = { foo; bar } in
  some_global := thing;;
                 ^^^^^
Error: This value escapes its region
```

Most of the types of allocation that OCaml does can be locally
allocated: tuples, records, variants, closures, boxed numbers,
etc. Local allocations are also possible from C stubs, although this
requires code changes to use the new `caml_alloc_local` instead of
`caml_alloc`. A few types of allocation cannot be locally allocated,
though, including first-class modules, classes and objects, and
exceptions. The contents of mutable fields (inside `ref`s, `array`s
and mutable record fields) also cannot be locally allocated.


## Local parameters

Generally, OCaml functions can do whatever they like with their
arguments: use them, return them, capture them in closures or store
them in globals, etc. This is a problem when trying to pass around
locally-allocated values, since we need to guarantee they do not
escape.

The remedy is that we allow the `local_` keyword to also appear on
function parameters:

```ocaml
let f (local_ x) = ...
```

A local parameter is a promise by a function not to let a particular
argument escape its region. In the body of f, you'll get a type error
if x escapes, but when calling f you can freely pass local values as
the argument. This promise is visible in the type of f:

```ocaml
val f : local_ 'a -> ...
```

The function f may be equally be called with locally-allocated or
GC-heap values: the `local_` annotation places obligations only on the
definition of f, not its uses.

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

The examples above use the `local_` keyword to mark local
allocations. In fact, this is not necessary, and the compiler will use
local allocations by default where possible.

The only effect of the keyword on e.g. a let binding is to change the
behavior for escaping values: if the bound value looks like it escapes
and therefore cannot be locally allocated, then without the keyword
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
over which values are locally allocated, including:

  - **Local closures**

    ```ocaml
    let local_ f a b c = ...
    ```

    defines a function `f` whose closure is itself locally allocated.

  - **Local-returning functions**

    ```ocaml
    let f a = exclave_
      ...
    ```

    defines a function `f` which returns local allocations into its
    caller's region.

  - **Global fields**

    ```ocaml
    type 'a t = { global_ g : 'a }
    ```

    defines a record type `t` whose `g` field is always known to be on
    the GC heap (and may therefore freely escape regions), even though
    the record itself may be locally allocated.

For more details, read [the reference](./reference.md).
