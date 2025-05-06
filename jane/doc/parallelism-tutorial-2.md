# Parallelism Tutorial II

In part one, we learned how to parallelize a computation using `fork_join` and parallel sequences.
However, we only had one way to share mutable data structures between `portable` functions: atomics.

Wrapping mutable state in an `Atomic.t` can be reasonable approach, but parallel programs often require other concurrency primitives, such as locks.
This is the purpose of the _Capsule API_, which lets us associate a mutable data structure with a particular lock.

## Capsules

A _capsule_ is a collection of mutable data identified by a unique _key_.
To create a capsule, we first ask for its key:

```ocaml
$ let (P key) = Capsule.create ()

val key : $k Capsule.Key.t
```

Keys have an interesting type: the parameter `$k` represents a brand-new type that's specific to this key.[^existential]
We will use this particular `'k` to identify different pieces of our capsule, allowing them to be used in concert.
Hence, we'll refer to our capsule as capsule `'k`.

Keys may be converted to locks, which give multiple domains mutually exclusive access to the contents of a capsule.
The most common kind of lock is a mutex:

```ocaml
$ let mutex = Capsule.Mutex.create key

val mutex : 'k Capsule.Mutex.t
```

Note that creating a mutex consumes the key, which cannot be used afterward.
This behavior relies on the _uniqueness_ mode axis, which we won't discuss in detail here.

Mutexes cross portability and contention, so they can be freely shared across any number of `portable` functions without becoming `contended`.
For example, we can use the same mutex in both branches of `fork_join`:

```ocaml
let lock (par : Parallel.t) =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let (), () =
    Parallel.fork_join2 par
        (fun _par -> Capsule.Mutex.with_lock mutex (fun _ -> ()))
        (fun _par -> Capsule.Mutex.with_lock mutex (fun _ -> ()))
  in ()
```

But we're getting ahead of ourselves&mdash;we first need to create some mutable state that lives in capsule ``k`.
Let's encapsulate a fresh `int ref`:

```ocaml
$ let ref = Capsule.Data.create (fun () -> ref 0)

val ref : (int ref, '_weak1) Capsule.Data.t
```

The resulting type has a weak parameter, indicating that `ref` lives in some particular capsule, but we don't yet know exactly which one.
When we use `ref` in conjunction with `mutex`, the compiler will learn that this parameter is specifically ``k`.

The type `Capsule.Data.t` also crosses portability and contention, so we can share `ref` across portable functions without it becoming contended.
However, we still need to enforce that only one function can manipulate the contents of `ref` at a time, so to access `ref`, we must lock the mutex.

```ocaml
let lock (par : Parallel.t) =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let ref = Capsule.Data.create (fun () -> ref 0) in
  let (), () =
    Parallel.fork_join2 par
        (fun _par -> Capsule.Mutex.with_lock mutex
            (fun password -> Capsule.Data.iter ref ~password ~f:(fun ref -> ref := !ref + 1)))
        (fun _par -> Capsule.Mutex.with_lock mutex
            (fun password -> Capsule.Data.iter ref ~password ~f:(fun ref -> ref := !ref + 1)))
  in ()
```

Let's unpack exactly what's going on here.
First, `with_lock` locks the mutex, assuring that we have exclusive access to capsule ``k`.
Second, we receive the associated capsule's `password`, which has type `'k Capsule.Password.t`.
Finally, we use the password to call `iter`, which provides uncontended access to our mutable reference.

This example could also be implemented with an `int Atomic.t`, but capsules are a much more flexible abstraction.
For example, we're not forced to use a mutex, or even a lock at all&mdash;using uniqueness, we can manipulate a capsule via its key.
Further, we can dynamically create new mutable state inside `'k`, and it will automatically share the locking scheme.
In the following sections, we'll explore two uses of capsules to protect a larger collection of mutable data.

## Sorting

<!-- an array in a capsule -->

<!-- sequential quicksort -->

<!-- slices -->

<!-- parallel arrays -->

<!-- parallel quicksort -->

## Image Processing

<!-- the shared mode -->

<!-- example of shared capsule data -->

<!-- keys and uniqueness -->

<!-- image blur with parallel arrays -->

[existential]: Also known as an existential type.