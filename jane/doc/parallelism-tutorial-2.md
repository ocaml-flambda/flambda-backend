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
let increment ~(access : 'k Capsule.Access.t) ref =
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
let increment ~(password : 'k Capsule.Password.t @@ local) ref =
  Capsule.access ~password (fun access ->
    let ref = Capsule.Data.unwrap ~access ref in
    ref := !ref + 1)
  [@nontail]
;;
```

Passwords **do** cross contention, so they can be freely shared between portable functions.
Naively, that would introduce races, but passwords are also always [_local_](https://blog.janestreet.com/oxidizing-ocaml-locality/).
That means we still can't transfer a password across domains: fork-join requires global functions, which cannot capture a local password.

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

Like we saw with `Capsule.current`, creation returns a packed key, so unpacking it gives us a `'k Capsule.Key.t` for a brand-new capsule `'k`.
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
However, keys need not be local&mdash;to prohibit races, keys rely on _uniqueness_.

Uniqueness is another mode axis that describes whether there exist multiple references to a value.
When a value has the `unique` mode, we know it is the only reference to its contents.
The default uniqueness mode is `aliased`, which means other references may exist.

`Capsule.Key.with_password` requires a unique key: if we can show that there are no other references to `key`, we gain permission to access the associated capsule.
Therefore, as with the other two mechanisms, we still can't share a key across fork/join:

```ocaml
let parallel_keys parallel =
  let (P key) = Capsule.create () in
  Parallel.fork_join2 parallel
    (fun _ ->
      Capsule.Key.with_password key ~f:(fun _ -> ()) |> ignore)
    (fun _ ->
      Capsule.Key.with_password key ~f:(fun _ -> ()) |> ignore)
(*                              ^^^                                 *)
(* This value is used here, but it is already being used as unique. *)
;;
```

However, unlike a password or an access, a key may be _moved_ into one of the branches:

```ocaml
let parallel_key parallel =
  let (P key) = Capsule.create () in
  let ref = Capsule.Data.create (fun () -> ref 0) in
  Parallel.fork_join2 parallel
    (fun _ ->
      Capsule.Key.with_password key ~f:(fun password -> increment ~password ref)
      |> ignore)
    (fun _ -> ())
;;
```

Here, the first task captures `key` uniquely, so we know the parent domain has renounced access to the associated capsule.
It's therefore perfectly safe to run the first task on another domain.

Furthermore, _destroying_ a key allows us to merge the associated capsule into the current capsule.
Concretely, passing a `'k` key to `Key.destroy` consumes it, so we know it can never be used again.
We get back a `'k` access, indicating that `'k` is part of the current capsule.

```ocaml
let merge_fresh () =
  let (P key) = Capsule.create () in
  let ref = Capsule.Data.create (fun () -> ref 0) in
  let access = Capsule.Key.destroy key in
  let ref = Capsule.Data.unwrap ~access ref in
  ref := !ref + 1
;;
```

These three abstractions&mdash;access, password, and key&mdash;provide three distinct approaches to arbitrating capsule access.
Because each type's safety properties rely on a different mode axis, they enable different parallel programming patterns.
In summary:

Type | Represents | Provides | Safety Axis
-----|------------|----------|--------
`'k Access.t` | Proof that `'k` is the current capsule. | Access to data in `'k`. | Contention
`'k Password.t` | Permission to enter capsule `'k`. | `'k Access.t` | Locality
`'k Key.t` | The capsule `'k` itself. | `'k Password.t` | Uniqueness

## Locks

At long last, we can get to the ultimate point of capsules: sharing mutable state across domains.

All abstractions covered so far rule out data races _statically_&mdash;one way or another, they require that state is never actually shared.
Fortunately, we now have all the tools needed to define _locks_: at runtime, a `'k Capsule.Mutex.t` will enforce that only one domain at a time can access `'k`.

To create a mutex for capsule `'k`, we consume its key:

```ocaml
let parallel_mutexes parallel =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  (* ... *)
;;
```

Intuitively, `mutex` now represents the capsule `'k`, since it contains the `'k` key.
We can think of a mutex as a _dynamically unique_ key: whenever we lock the mutex, we gain exclusive access to `'k`.
Therefore, locking a mutex produces a password.

```ocaml
let parallel_mutexes parallel =
  let (P key) = Capsule.create () in
  let mutex = Capsule.Mutex.create key in
  let ref = Capsule.Data.create (fun () -> ref 0) in
    Parallel.fork_join2 parallel
    (fun _ ->
      Capsule.Mutex.with_lock mutex ~f:(fun password -> increment ~password ref))
    (fun _ ->
      Capsule.Mutex.with_lock mutex ~f:(fun password -> increment ~password ref))
;;
```

As you might expect, mutexes cross contention and are neither `local` nor `unique`.
In fact, mutexes have no restrictions at all, so may be freely shared between portable functions, fork/join tasks, and domains.

## Sorting

Now that we can share mutable state between parallel tasks, we can speed up a broader class of algorithms.
For example, let's explore how we might parallelize sorting a mutable array.

We'll make use of two more pieces of the `Parallel` library: _parallel arrays_ and _slices_.

```ocaml
module Par_array = Parallel.Arrays.Array
module Slice = Parallel.Arrays.Array.Slice
```

A parallel array is just like an array, but with a couple restrictions that make it safe to operate upon in parallel.
In this example, we'll assume the array elements cross portability and contention (and for simplicity, are `int`s).

A slice is a `local` view of a parallel array.
Intuitively, a slice _borrows_ a segment of the array, only allowing access a contiguous subset of its indices.
Using slices, we can implement a standard sequential quicksort:

```ocaml
let swap slice ~i ~j =
  let temp = Slice.get slice i in
  Slice.set slice i (Slice.get slice j);
  Slice.set slice j temp
;;

let partition slice =
  let length = Slice.length slice in
  let pivot = Random.int length in
  swap slice ~i:pivot ~j:(length - 1);
  let pivot = Slice.get slice (length - 1) in
  let store = ref 0 in
  for i = 0 to length - 2 do
    if Slice.get slice i <= pivot
    then (
      swap slice ~i ~j:!store;
      Int.incr store)
  done;
  swap slice ~i:!store ~j:(length - 1);
  !store
;;

let rec quicksort slice =
  if Slice.length slice > 1
  then (
    let pivot = partition slice in
    let left = Slice.sub slice ~i:0 ~j:pivot in
    let right = Slice.sub slice ~i:pivot ~j:(Slice.length slice) in
    quicksort left;
    quicksort right [@nontail])
;;
```

The sequential implementation has reasonable performance&mdash;it sorts 10,000 random integers in 2.3 milliseconds in our benchmark setup.
However, there's a clear opportunity for parallelism: recursively sorting `left` and `right` are independent tasks, so they could be run in parallel.
We can try adding a fork/join, but it won't quite work:

```ocaml
let rec quicksort parallel slice =
  if Slice.length slice > 1
  then (
    let pivot = partition slice in
    let left = Slice.sub slice ~i:0 ~j:pivot in
    let right = Slice.sub slice ~i:pivot ~j:(Slice.length slice) in
    let (), () =
      Parallel.fork_join2
        parallel
        (fun parallel -> quicksort parallel left)
(*                                          ^^^^                                   *)
(* The value left is local, so cannot be used inside a function that might escape. *)
        (fun parallel -> quicksort parallel right)
    in
    ())
;;
```

There are actually two issues here: slices are local, so we can't close over them in a parallel task, but even if we could, they would become contended, meaning we can't read or write their contents.
Instead, we can use a specialized fork/join for slices:

```ocaml
let rec quicksort parallel slice =
  if Slice.length slice > 1
  then (
    let pivot = partition slice in
    let (), () =
      Slice.fork_join2
        parallel
        ~pivot
        slice
        (fun parallel left -> quicksort parallel left)
        (fun parallel right -> quicksort parallel right)
    in
    ())
;;
```

The function `Slice.fork_join2` requires an uncontended slice, splits it at an index, and provides the two halves to two parallel tasks at uncontended.
Since each task can only access a separate portion of the array&mdash;and the slices are local, so don't escape&mdash;this is safe.

To run our parallel quicksort, we need to get an implementation of parallelism from a scheduler.
For example, using `Parallel_scheduler_work_stealing`:

```ocaml
let quicksort ~scheduler ~mutex array =
    let monitor = Parallel.Monitor.create_root () in
    Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
      let array = Par_array.of_array array in
(*                                   ^^^^^                 *)
(* This value is contended but expected to be uncontended. *)
      quicksort parallel (Slice.slice array) [@nontail])
  ;;
```

There's one last problem: capturing an existing array in `schedule` causes it to become contended.
To fix this, we will instead provide an encapsulated array.

```ocaml
let quicksort ~scheduler ~mutex array =
  let monitor = Parallel.Monitor.create_root () in
  Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
    Capsule.Mutex.with_lock mutex ~f:(fun password ->
      Capsule.Data.iter array ~password ~f:(fun array ->
        let array = Par_array.of_array array in
        quicksort parallel (Slice.slice array) [@nontail])
      [@nontail])
    [@nontail])
;;
```

Finally, we may benchmark our parallel implementation on various numbers of domains:

Domains | Time (ms)
--------|------
Sequential | 2.36
1 | 2.78
2 | 1.70
4 | 1.09
8 | 0.78
16 | 0.77

Although we observe a speedup using up to 16 domains, quicksort only admits a limited amount of parallelism&mdash;eventually, the cost of sequentially partitioning the array dominates the runtime.
For this reason, other algorithms (such as merge-sort) are often preferable in the parallel setting.

## Image Processing

Another common application of parallelism is for _data parallel_ tasks, where we want to perform the same independent operation on a collection of data.
For example, let's attempt to parallelize blurring an image.

We'll start by defining a simple (greyscale) image type:

```ocaml
type t : value mod portable

val load : string -> t
val of_array : float array -> width:int -> height:int -> t

val width : t @ contended -> int
val height : t @ contended -> int

val get : t -> x:int -> y:int -> float
val set : t -> x:int -> y:int -> float -> unit
```

An image is really just an array plus a width and height, but this interface has a couple notable features:

- `value mod portable` indicates that images cross portability. This is safe since an image does not contain functions.
- `width` and `height` allow a contended image. This is safe since the width and height are immutable properties of the image.

To create a blurred copy of an image, we want each pixel in the result to contain the average of a 9x9 box of pixels in the input, centered at this pixel.
We'll use the following function to compute this average at coordinates `x,y`:

```ocaml
let blur_at image ~x ~y =
  let width = Image.width image in
  let height = Image.height image in
  let acc = ref 0. in
  for i = -4 to 4 do
    for j = -4 to 4 do
      let x = Int.clamp_exn (x + i) ~min:0 ~max:(width - 1) in
      let y = Int.clamp_exn (y + j) ~min:0 ~max:(height - 1) in
      acc := !acc +. Image.get image ~x ~y
    done
  done;
  !acc /. 121.
;;
```

Introducing parallelism is easy: we just need to run this function for each output pixel.
We will do so via `Par_array.init`, where each index corresponds to one pixel.

Since we want to use the input image in multiple parallel tasks, we'll need to provide it in a capsule:

```ocaml
let filter ~scheduler ~mutex image =
  let monitor = Parallel.Monitor.create_root () in
  Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
    (* Note [project] produces a contended image *)
    let width = Image.width (Capsule.Data.project image) in
    let height = Image.height (Capsule.Data.project image) in
    let data =
      Par_array.init parallel (width * height) ~f:(fun i ->
        let x = i % width in
        let y = i / width in
        Capsule.Mutex.with_lock mutex ~f:(fun password ->
          Capsule.access ~password ~f:(fun access ->
            blur_at (Capsule.Data.unwrap image ~access) ~x ~y)))
    in
    Image.of_array (Par_array.to_array data) ~width ~height)
;;
```

Now, if we benchmark this implementation...

Domains | Time (ms)
--------|------
1 | 309
2 | 705
4 | 812
8 | 977

...we'll find that it gets slower with more domains!
That's because we've only allowed one domain at a time to access the input image, destroying any opportunity for parallelism.

Fortunately, we know that all domains only _read_ the input image, so it should be safe for them to do so simultaneously.
However, we can't just allow `Image.get` to read from a contended image&mdash;in general, one other domain could be writing to it.

Hence, we need a third mode on the contention axis: _shared_, which falls in between contended and uncontended.
The shared mode indicates that _all_ references to a value are either shared or contended, so we may read its mutable contents in parallel.
Let's make `get` allow a shared image:

```ocaml
val get : t @ shared -> int -> int -> float
```

Now, how do we obtain a shared reference to the input image?
The shared mode is closely related to keys: if a unique key indicates that we have exclusive access to a capsule, an _aliased_ key indicates that _nobody_ has exclusive access to the capsule.
This property is enforced by the uniqueness axis, since unlike with contention, an aliased reference precludes the existence of any unique references.
Hence, an aliased key can provide shared access to the contents of a capsule.

Instead of protecting the input image with a mutex, we can instead use an aliased key.
The existence of this key means we can never again write to the input, but that's perfectly fine here.

```ocaml

let filter ~scheduler ~key image =
  let monitor = Parallel.Monitor.create_root () in
  Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
    let width = Image.width (Capsule.Data.project image) in
    let height = Image.height (Capsule.Data.project image) in
    let data =
      Parallel_array.init parallel (width * height) ~f:(fun i ->
        let x = i % width in
        let y = i / width in
        Capsule.Key.access_shared key ~f:(fun access ->
          blur_at (Capsule.Data.unwrap_shared image ~access) ~x ~y))
    in
    Image.of_array (Parallel_array.to_array data) ~width ~height)
;;
```

The `Capsule.Key.access_shared` function provides us with an `'k Access.t @ shared`, which we can then pass to `Capsule.Data.unwrap_shared` to get our desired `Image.t @ shared`.
Now, our filter's performance scales fairly close to linearly with additional domains:

Domains | Time (ms)
--------|------
1 | 287
2 | 150
4 | 81
8 | 51

