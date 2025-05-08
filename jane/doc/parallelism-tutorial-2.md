# Parallelism Tutorial II

In part one, we learned how to parallelize a computation using `fork_join` and parallel sequences.
However, we only had one way to share mutable data structures between `portable` functions: atomics.

Wrapping mutable state in an `Atomic.t` can be reasonable approach, but parallel programs often require other concurrency primitives, such as locks.
This is the purpose of the _Capsule API_, which lets us associate a mutable data structure with a particular lock.

_This tutorial uses the "expert" capsule API, which may be found in `Portable.Capsule.Expert`.
 A more ergonomic but slightly less expressive API will be provided later on._

## Capsules

A _capsule_ is a collection of mutable state protected by a certain abstraction boundary.
Data that lives in a capsule is represented by the type `('a, 'k) Capsule.Data.t`.
For example, to create a fresh `int ref` that lives in a capsule:

```ocaml
let ref : (int ref, 'k) Capsule.Data.t = Capsule.Data.create (fun () -> ref 0)
```

The result has type `(int ref, 'k) Capsule.Data.t`, which we may interpret as a pointer to an `int ref` that lives in the capsule `'k`.
The type `'k` is an [existential type](https://dev.realworldocaml.org/gadts.html) used to identify various pieces of capsule `'k`.
That is, if two capsule pointers have the same `'k`, we know they point into the same capsule.

Capsule data crosses portability and contention, so it may be shared across any number of `portable` functions without becoming `contended`.
For example, we could use `ref` at `uncontended` in both branches of a fork-join expression.

```ocaml
let fork_join parallel =
  let ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ -> (ref : _ @@ uncontended))
    (fun _ -> (ref : _ @@ uncontended))
;;
```

To prevent races, the capsule API assures that only one domain at a time can dereference a capsule pointer.
There are three mechanisms for doing so, each of which builds on the former.

## Accesses

The first mechanism is _access_.
A value of type `'k Capsule.Access.t` indicates that we are currently executing within the capsule `'k`.
When `'k` is the current capsule, we know that no other domains can access `'k`, so it is safe to dereference pointers into `'k`.

```ocaml
let increment ~(access : 'k Capsule.Access.t) (ref : (int ref, 'k) Capsule.Data.t) =
  let ref = Capsule.Data.unwrap ~access ref in
  ref := !ref + 1
;;
```

Accesses **do not** cross contention, so they cannot be freely shared between portable functions.
Intuitively, a portable function may run in a different capsule, so it must not be allowed to see capsule `'k`.
This property prohibits data races, as we can see with fork/join:

```ocaml
let parallel_increment parallel ~(access : 'k Capsule.Access.t) =
  let ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ -> increment ~access ref)
(*                       ^^^^^^                            *)
(* This value is contended but expected to be uncontended. *)
    (fun _ -> increment ~access ref)
;;
```

So, how do we obtain access to capsule `'k`?
Well, we can always request access to the capsule associated with the initial domain:

```ocaml
let increment_initial () =
  let initial_ref = Capsule.Data.create (fun () -> ref 0) in
  increment initial_ref ~access:Capsule.Access.initial
;;
```

Because `Access.initial` is `nonportable`, we know it can never be used outside of the initial domain.
In fact, all code executes within _some_ capsule, so we can also ask for access to the current capsule.

```ocaml
let increment_current () =
  let (P access) = Capsule.current () in
  let current_ref = Data.create (fun () -> ref 0) in
  increment current_ref ~access
;;
```

However, we can't know whether we're currently inside a particular preexisting capsule, so `Capsule.current` returns a _packed_ access.
Unpacking `access` generates a fresh capsule type `'k` that's distinct from all other capsules.

Therefore, we also need a way to request access to a particular capsule `'k`.
This is the purpose of the second mechanism: _passwords_.

## Passwords

A value of type `'k Capsule.Password.t` represents permission to enter the capsule `'k`.
Given a password, we can request an access:

```ocaml
let increment ~(password : 'k Capsule.Password.t) (ref : (int ref, 'k) Capsule.Data.t) =
  Capsule.access ~password (fun access ->
    let ref = Capsule.Data.unwrap ~access ref in
    ref := !ref + 1)
;;
```

Passwords **do** cross contention, so they can be freely shared across portable functions.
Naively, that would introduce races, but passwords are always [_local_](https://blog.janestreet.com/oxidizing-ocaml-locality/).
That means we still can't transfer a password between domains: fork-join requires global functions, which cannot capture a local password.

<!-- CR-someday mslater: this is actually enforced by the yielding axis; at some
     point we will allow local fork/join and this will be wrong. -->

```ocaml
let parallel_increment parallel ~(password : 'k Capsule.Password.t @@ local) =
  let ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ -> increment ~password ref)
(*                       ^^^^^^^^                                                      *)
(* The value password is local, so cannot be used inside a function that might escape. *)
    (fun _ -> increment ~password ref)
  [@nontail]
;;
```

Semantically, the primary use of passwords is requesting access, since that's what lets you unwrap capsule data.
However, `Capsule.Data` also provides a variety of convenience functions for operating on capsule data _in situ_.
These functions require passwords, since they do not assume that the current capsules is the correct one.

```ocaml
val map
  :  password:'k Capsule.Password.t @ local
  -> f:('a -> 'b) @ local once portable
  -> ('a, 'k) Capsule.Data.t
  -> ('b, 'k) Capsule.Data.t
```

For example, `Capsule.Data.map` runs the function `f` in the capsule `'k`, providing access to the encapsulated `'a` and returning an encapsulated `'b`.
Passwords can also provide more flexibility than accesses, since they may be captured by `local portable` functions.
We'll see an example of when this can be useful later on.

However, we still haven't explained how to get a password!
That brings us to the third mechanism: _keys_.

## Keys

A value of type `'k Capsule.Key.t` is, in some sense, the capsule `'k` itself.
Creating a key is equivalent to creating a capsule:

```ocaml
let (P (key : _ Capsule.Key.t)) = Capsule.create ()
```

Like we saw with `Capsule.current`, creation returns a packed key, and unpacking it gives us a `'k Capsule.Key.t` for a brand-new capsule `'k`.
We can then use the key to get a password:

```ocaml
let increment_fresh () =
  let (P key) = Capsule.create () in
  let fresh_ref = Capsule.Data.create (fun () -> ref 0) in
  Capsule.Key.with_password key ~f:(fun password ->
    increment ~password fresh_ref)
;;
```

Like passwords, keys cross contention.
However, keys don't have to be local&mdash;what makes them safe is _uniqueness_.

Uniqueness is another mode axis that describes whether there are multiple references to a value.
When a value has the `unique` mode, we know it is the only reference to its contents.
The default mode on the uniqueness axis is `aliased`, which means other references may exist.

Hence, `Capsule.Key.with_password` requires a unique key: if we can show that no other domain knows about this key, we gain permission to access the associated capsule.
Like the other two mechanisms, we still can't share a key across fork/join:

```ocaml
let parallel_keys parallel ~(access : 'k Capsule.Access.t) =
  let (P key) = Capsule.create () in
  Parallel.fork_join2 parallel
    (fun _ -> Capsule.Key.with_password key ~f:(fun _ -> ()) |> ignore)
    (fun _ -> Capsule.Key.with_password key ~f:(fun _ -> ()) |> ignore)
(*                                      ^^^                         *)
(* This value is used here, but it is already being used as unique. *)
;;
```

<!-- summarize, then introduce locks -->

## Locks

## Sorting

<!-- an array in a capsule -->

<!-- sequential quicksort -->

<!-- slices -->

<!-- parallel arrays -->

<!-- parallel quicksort -->

## Image Processing

<!-- the shared mode -->

<!-- example of shared capsule data -->

<!-- aliased keys -->

<!-- image blur with parallel arrays -->
