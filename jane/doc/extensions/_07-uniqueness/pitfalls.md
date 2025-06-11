---
layout: documentation-page
collectionName: Uniqueness
title: Pitfalls
---

# Some Pitfalls of Uniqueness

This document outlines some common pitfalls that may come up when trying out
uniqueness, as well as some suggested workarounds. Over time, this list may grow
(as experience discovers new things that go wrong) or shrink (as new
compiler versions ameliorate some issues).

If you want an introduction to uniqueness, see the [introduction](../intro).

## Annotating unique return values

When the compiler infers uniqueness, it will only mark values as `unique` if
they are actually used as `unique`. For example, given a smart constructor:

```ocaml
let mk () = { x = 1 }
```

The result is not unique by default. This helps the compiler identify static
allocations: as long as `mk ()` returns an `aliased` value, the compiler can
lift out the allocation so that `mk ()` returns the same pointer on each
invocation. However, you might want to annotate this allocation as `unique`:

```ocaml
let mk () = ({ x = 1 } : _ @ unique)
```

Unfortunately, this will not work: while the allocation is now indeed `unique`,
the compiler immediately casts it to `aliased` so that `mk` still returns an
`aliased` value. The only way to reliably change the return mode of `mk` to be
`unique` is through a type signature:

```ocaml
let mk : unit -> t @ unique = fun () -> { x = 1 }
```

Most times, it will not be necessary to manually annotate a return value as
`unique`. While the compiler defaults the return mode to `aliased`, it will make
the return mode `unique` if there is any unique use of the result of `mk ()`:

```ocaml
let use () = free (mk ())
```

This code typechecks even if `mk` is not annotated, because, now that a need has
arisen, the compiler has inferred the return mode of `mk` to be `unique`.
However, this analysis happens on a per module basis. If we abstract `mk` into
its own module we will get a mode error:

```ocaml
module Mk = struct
  let mk () = { x = 1 }
end

let use () = free (Mk.mk ())
                  ^^^^^^^^^^
Error: This value is "aliased" but expected to be "unique".
```

When abstracting functions that return unique values into a module, we need to
provide an explicit mode annotation in the signature.

## Threading unique values

While unique values allow safe mutation, they require a coding style that is
much closer to purely functional code than mutating code. For example, you might
imagine a function to set values in unique arrays:

```ocaml
val set : 'a array @ unique -> int -> 'a -> unit
```

which you would use like this:

```ocaml
let set_all_zero arr =
  for i = 0 to Unique_array.size arr do
    Unique_array.set arr i 0
  done
```

But this does not work! Similar to how closures closing over unique values can
only be invoked once, a for-loop may not close over unique values at all:

```ocaml
3 |     Unique_array.set arr i 0
                         ^^^
Error: This value is "aliased" but expected to be "unique".
  Hint: This identifier cannot be used uniquely,
  because it was defined outside of the for-loop.
```

Indeed, we can not use the same array `arr` more than once uniquely. Instead,
the `set` function needs to return the new array:

```ocaml
val set : 'a array @ unique -> int -> 'a -> 'a array @ unique
```

This is a crucial change: since `set` consumes a `unique` argument, this removes
the only reference we have to the old array. To be able to use the `array`
afterwards, you need `set` to return another reference. Then we can write:

```ocaml
let set_all_zero arr =
  let rec loop idx arr =
    if idx == 0 then arr
    else loop (idx - 1) (Unique_array.set arr idx 0)
  in
  let size, arr = Unique_array.size arr in
  loop size arr
```

We are planning a new feature called _exclusive mutable_ that will allow you to
use unique values in a more imperative code style similar to the first example.
