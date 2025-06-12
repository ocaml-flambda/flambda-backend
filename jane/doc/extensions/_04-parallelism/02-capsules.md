---
layout: documentation-page
collectionName: Parallelism
title: Capsules
---

<style>
.table {
    width: fit-content;
    margin-left: auto;
    margin-right: auto;
    margin-bottom: 20px;
    border: 1px solid black;
    border-collapse: collapse;
    td, th {
      font-size: 0.95em;
      border: 1px solid black;
      padding-left: 6px;
      padding-right: 6px;
      padding-top: 5px;
      padding-bottom: 3px;
    }
    th {
      text-align: center;
      border-bottom: 2px solid black;
    }
}
</style>

# Capsules

_This page introduces the ["expert" capsule
API](https://github.com/janestreet/basement/blob/master/src/capsule.mli).  A
more ergonomic, work-in-progress API is available in
[`Portable.Capsule`](https://github.com/janestreet/portable/blob/master/kernel/src/capsule_intf.ml)._

A _capsule_ is a collection of mutable state accessible via a particular API
that prohibits data races.  Capsules may be associated with _locks_, which allow
their mutable state to be shared across domains.

## Data

Data that lives in a capsule is represented by the type `('a, 'k)
Capsule.Data.t`.  For example, to create a fresh `int ref` that lives in a
capsule:

```ocaml
let capsule_ref = Capsule.Data.create (fun () -> ref 0)
```

The encapsulated reference has type `(int ref, 'k) Capsule.Data.t`, which we may
interpret as a pointer to an `int ref` that lives in capsule `'k`.

The type parameter `'k` is known as a *capsule brand*, and uniquely identifies a
capsule at compile time.  Given two capsule pointers with the same `'k`, we know
they point into the same capsule, so they may be merged:

```ocaml
val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
```

However, if you create capsule data at top level, you'll see a slightly
different type:

```ocaml
$ let capsule_ref = Capsule.Data.create (fun () -> ref 0);;

> val capsule_ref : (int ref, '_weak1) Capsule.Data.t = <abstr>
```

Here, the brand is _weakly polymorphic_.  Later on, the compiler will infer that
the brand is that of a particular capsule `'k`&mdash;it just doesn't know which
one yet.

Encapsulated data crosses portability and contention.  That means we can share a
`Capsule.Data.t` across any number of `portable` functions without it becoming
`contended`.  For example, we could use `capsule_ref` at `uncontended` in both
branches of a fork/join expression.

```ocaml
let fork_join parallel =
  let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ -> (capsule_ref : _ @ uncontended))
    (fun _ -> (capsule_ref : _ @ uncontended))
;;
```

To prevent races, the capsule API assures that only one domain at a time can
dereference a capsule pointer.  There are three mechanisms for doing so, each of
which builds on the former.

## Accesses

The first mechanism is _access_.  A value of type `'k Capsule.Access.t`
indicates that `'k` is the _current capsule_.  When `'k` is the current capsule,
we know that no other domains can access `'k`, so it is safe to dereference
pointers into `'k`.

```ocaml
let increment ~(access : 'k Capsule.Access.t) capsule_ref =
  let ref = Capsule.Data.unwrap ~access capsule_ref in
  ref := !ref + 1
;;
```

Accesses **do not** cross contention, and `unwrap` requires an `uncontended`
access.  Hence, accesses cannot be freely shared between `portable`
functions&mdash;semantically, calling a `portable` function changes the current
capsule, so we can no longer examine data in `'k`.  This property prohibits data
races, as we can see with fork/join:

```ocaml
let parallel_increment parallel ~(access : 'k Capsule.Access.t) =
  let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ -> increment ~access capsule_ref)
(*                       ^^^^^^                            *)
(* This value is contended but expected to be uncontended. *)
    (fun _ -> increment ~access capsule_ref)
;;
```

So, how do we obtain access to a capsule?  The simplest way is to use the
top-level access associated with the initial domain:

```ocaml
let (initial : Capsule.initial Capsule.Access.t) = Capsule.initial
```

Accesses don't cross contention, so only `nonportable` functions can capture an
`uncontended` reference to `initial`.  Then, since top-level `nonportable`
functions always execute on the initial domain, they may manipulate data in the
initial capsule.

```ocaml
let increment_initial () =
  let initial_ref = Capsule.Data.create (fun () -> ref 0) in
  increment initial_ref ~access:Capsule.Access.initial
;;
```

More generally, all functions execute in _some_ capsule, though not necessarily
the initial one.  We can always ask for access to the current capsule, which
gives it a name:

```ocaml
let increment_current () =
  let (P access) = Capsule.current () in
  let current_ref = Capsule.Data.create (fun () -> ref 0) in
  increment current_ref ~access
;;
```

However, we don't know whether the current capsule has the same brand as any
preexisting capsule.  `Capsule.current` returns a _packed_ access, and unpacking
the result creates a fresh `'k` that's distinct from the brand of all other
capsules.  That means we can never access data from capsules other than the
current capsule.

```ocaml
let increment_other (ref : (int ref, 'k) Capsule.Data.t) =
  let (P access) = Capsule.current () in
  increment ref ~access
(*               ^^^^^^                                  *)
(* This expression has type (int ref, 'k) Capsule.Data.t *)
(* but an expression was expected of type                *)
(* (int ref, $k) Capsule.Data.t                          *)
```

_Note the capsule brand is printed as `$k`, indicating it was generated by
locally unpacking `access`._

Providing access to other capsules is the purpose of the second mechanism:
_passwords_.

## Passwords

A value of type `'k Capsule.Password.t` represents permission to access `'k` by
making it the current capsule.  Given a password, we can request an access:

```ocaml
let increment ~(password : 'k Capsule.Password.t @ local) capsule_ref =
  Capsule.access ~password (fun access ->
    let ref = Capsule.Data.unwrap ~access capsule_ref in
    ref := !ref + 1)
  [@nontail]
;;
```

Passwords **do** cross contention, so they can be freely shared between
`portable` functions.  Naively, that would introduce races, but passwords are
also always [`local`](../../stack-allocation/intro).
That means we still can't transfer a password across domains: fork/join requires
`global` functions, which cannot capture a `local` password.

<!-- CR-soon mslater: this is actually enforced by the yielding axis; at some
     point we will allow local fork/join and this will be wrong. -->

```ocaml
let parallel_increment parallel
      ~(password : 'k Capsule.Password.t @ local) =
  let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ -> increment ~password capsule_ref)
(*                       ^^^^^^^^                          *)
(* The value password is local, so cannot be used inside a *)
(* function that might escape.                             *)
    (fun _ -> increment ~password capsule_ref)
  [@nontail]
;;
```

The primary use of passwords is requesting access, since that's what lets you
unwrap capsule data.  However, `Capsule.Data` also provides a variety of
convenience functions for operating on capsule data _in situ_.  These functions
require passwords, since they do not assume that the current capsule is the
correct one.

```ocaml
val map
  :  password:'k Capsule.Password.t @ local
  -> f:('a -> 'b) @ local once portable
  -> ('a, 'k) Capsule.Data.t
  -> ('b, 'k) Capsule.Data.t
```

For example, `Capsule.Data.map` runs the function `f` with `'k` as the current
capsule, providing access to the encapsulated `'a` and returning an encapsulated
`'b`.  Passwords can also provide more flexibility than accesses, since they may
be captured by `local portable` functions.

However, we still haven't explained how to get a password!  That brings us to
the third mechanism: _keys_.

## Keys

A value of type `'k Capsule.Key.t` is, in some sense, the capsule `'k` itself.
Creating a key is equivalent to creating a capsule:

```ocaml
let (P (key : _ Capsule.Key.t)) = Capsule.create ()
```

Like we saw with `Capsule.current`, creation returns a packed key, so unpacking
it gives us a `'k Capsule.Key.t` for a brand-new capsule `'k`.  We can then use
the key to get a password:

```ocaml
let increment_fresh () =
  let (P key) = Capsule.create () in
  let fresh_ref = Capsule.Data.create (fun () -> ref 0) in
  Capsule.Key.with_password key ~f:(fun password ->
    increment ~password fresh_ref)
;;
```

Like passwords, keys cross contention.  However, keys need not be
`local`&mdash;to prohibit races, keys rely on _uniqueness_.

Uniqueness is a modal axis that describes whether there exist multiple
references to a value.  When a value has the `unique` mode, we know it is the
only reference to its contents.  The default uniqueness mode is `aliased`, which
means other references may exist.

`Capsule.Key.with_password` requires a unique key: if we can show that there are
no other references to `key`, we gain permission to access the associated
capsule.  Therefore, as with the other two mechanisms, we still can't share a
key across fork/join:

```ocaml
let parallel_keys parallel =
  let (P key) = Capsule.create () in
  let _ = Parallel.fork_join2 parallel
    (fun _ -> Capsule.Key.with_password key ~f:(fun _ -> ()))
    (fun _ -> Capsule.Key.with_password key ~f:(fun _ -> ()))
(*                                      ^^^                *)
(* This value is used here, but it is already being used   *)
(* as unique.                                              *)
  in ()
;;
```

However, unlike a password or an access, a key may be _moved_ into one of the
branches:

```ocaml
let parallel_key parallel =
  let (P key) = Capsule.create () in
  let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ ->
      Capsule.Key.with_password key
        ~f:(fun password -> increment ~password capsule_ref)
      |> ignore)
    (fun _ -> ())
;;
```

Here, the first task captures `key` uniquely, so we know the parent domain has
renounced access to the associated capsule.  It's therefore perfectly safe to
run the first task on another domain.

Furthermore, _destroying_ a key allows us to merge the associated capsule into
the current capsule.  Concretely, passing a `'k` key to `Key.destroy` consumes
it, so we know it can never be used again.  We get back a `'k` access,
indicating that `'k` is part of the current capsule.

```ocaml
let merge_fresh () =
  let (P key) = Capsule.create () in
  let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
  let access = Capsule.Key.destroy key in
  let ref = Capsule.Data.unwrap ~access capsule_ref in
  ref := !ref + 1
;;
```

These three abstractions&mdash;access, password, and key&mdash;provide three
distinct approaches to arbitrating capsule access.  Because each type's safety
properties rely on a different mode axis, they enable different parallel
programming patterns.  In summary:

Type            | Represents                              | Provides                | Safety Axis
----------------|-----------------------------------------|-------------------------|------------
`'k Access.t`   | Proof that `'k` is the current capsule. | Access to data in `'k`. | Contention
`'k Password.t` | Permission to enter capsule `'k`.       | `'k Access.t`           | Locality
`'k Key.t`      | The capsule `'k` itself.                | `'k Password.t`         | Uniqueness
{: .table }

## Locks

At long last, we can get to the ultimate point of capsules: sharing mutable
state across domains.

All abstractions covered so far rule out data races _statically_&mdash;one way
or another, they require that state is never actually shared.  Fortunately, we
now have all the tools needed to define _locks_: at runtime, a `'k
Capsule.Mutex.t` will enforce that only one domain at a time can access `'k`.

To create a mutex for capsule `'k`, we consume its key:

```ocaml
let parallel_mutexes parallel =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  (* ... *)
;;
```

Intuitively, `mutex` now represents the capsule `'k`, since it contains the `'k`
key.  We can think of a mutex as a _dynamically unique_ key: whenever we lock
the mutex, we gain exclusive access to `'k`.  Therefore, locking a mutex
produces a password.

```ocaml
let parallel_mutexes parallel =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
    Parallel.fork_join2 parallel
    (fun _ ->
      Capsule.Mutex.with_lock mutex
        ~f:(fun password -> increment ~password capsule_ref))
    (fun _ ->
      Capsule.Mutex.with_lock mutex
        ~f:(fun password -> increment ~password capsule_ref))
;;
```

As you might expect, mutexes cross contention and are neither `local` nor
`unique`.  In fact, mutexes have no restrictions at all, so may be freely shared
between `portable` functions, fork/join tasks, and domains.

## Reader-Writer Locks

_Coming Soon_
