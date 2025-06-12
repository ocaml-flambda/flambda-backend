---
layout: documentation-page
collectionName: Tutorials
title: Introduction to parallelism, Part 2
---

<style>
.table {
    width: fit-content;
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

# Parallelism Tutorial: Part 2

The [first parallelism tutorial](../01-intro-to-parallelism-part-1) introduced the
contention and portability mode axes, showcasing their use in fork/join
parallelism and parallel sequences.  However, it only covered one way to share
mutable data between `portable` functions:
[atomics](https://github.com/janestreet/portable/blob/master/kernel/src/atomic.mli).
In this tutorial, we'll see how
[_capsules_](https://github.com/janestreet/portable/blob/master/kernel/src/capsule_intf.ml)
and [_parallel
arrays_](https://github.com/janestreet/parallel/blob/with-extensions/arrays/parallel_arrays_intf.ml)
can be used parallelize programs that operate on more complex mutable data.

## Capsules

_This tutorial uses the ["expert" capsule
API](https://github.com/janestreet/basement/blob/master/src/capsule.mli), which
is explained in more detail [here](../../parallelism/02-capsules).  For a brief
overview, read on._

Wrapping mutable state in an `Atomic.t` can be a reasonable approach, but
parallel programs often require other concurrency primitives, such as locks.
This is the purpose of the capsule API, which lets us associate a collection of
mutable state with a particular lock.

### Branding

The capsule API introduces several types that have a special type parameter
`'k`.  This parameter is called the *capsule brand*, and it uniquely identifies
a capsule at compile time.  Concretely, we'll use the following types:

- `('a, 'k) Capsule.Data.t`
- `'k Capsule.Key.t`
- `'k Capsule.Mutex.t`

All values that share a particular brand `'k` are associated with capsule `'k`.
For example, we will be able to use a `'k Capsule.Mutex.t` to access data
branded with the same `'k`.

### Data

To represent mutable state that lives in a capsule, we create a `('a, 'k)
Capsule.Data.t`.  This type can be thought of as a pointer to a value of type
`'a` that is protected by capsule `'k`.

```ocaml
let capsule_ref = Capsule.Data.create (fun () -> ref 0)
```

Even though the capsule may contain mutable state, encapsulated data crosses
portability and contention.  That means we can freely share this pointer between
`portable` functions without it becoming `contended`.  To prevent races, the
rest of the capsule API limits when we can dereference encapsulated data.

### Keys

Capsules can be associated with _keys_&mdash;for example, the key for capsule
`'k` has type `'k Capsule.Key.t`.  When we create a capsule, we receive its key:

```ocaml
let (P key) = Capsule.create () in (* ... *)
```

Note that `Capsule.create` returns a "packed" key: its brand is
[existential](https://dev.realworldocaml.org/gadts.html), so unpacking the
result produces a fresh `'k` distinct from all other capsule brands.

Keys are protected by [_uniqueness_](../../uniqueness/intro),
which is another modal axis that tracks whether there exist multiple references
to a value.  Given a `unique` key (as opposed to `aliased`), we know the current
thread holds the only reference to the key, so may manipulate the contents of
the associated capsule.

Given a unique key, we can request the _password_ for its capsule, which lets us
access the data therein.  For example, we can use `Capsule.Data.iter` to
increment our reference:

```ocaml
let (P key) = Capsule.create () in
let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
Capsule.Key.with_password key ~f:(fun password ->
  Capsule.Data.iter capsule_ref ~password ~f:(fun ref ->
    ref := !ref + 1
  ))
```

### Locks

If `unique` keys were the only way to get a password, we still couldn't let
multiple domains trade off access to a capsule.  This is the purpose of locks,
the most common of which is the _mutex_.  To create a mutex for a capsule, we
consume its key, which cannot be used again:

```ocaml
let (P key) = Capsule.create () in
let mutex = Capsule.Mutex.create key in (* ... *)
```

Mutexes also cross portability and contention, so may be freely shared across
`portable` functions and assumed to be `uncontended`.  Now, to get a password,
we can lock the mutex, indicating that our domain has exclusive access to the
capsule.  In this way, a mutex is like a _dynamically unique_ key&mdash;the
mutex itself may be `aliased`, but only one domain can have the lock.

```ocaml
let (P key) = Capsule.create () in
let mutex = Capsule.Mutex.create key in
let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
Capsule.Mutex.with_lock mutex ~f:(fun password ->
  Capsule.Data.iter capsule_ref ~password ~f:(fun ref ->
    ref := !ref + 1
  ))
```

With mutexes, we have the tools required to safely share mutable data structures
between parallel tasks.

## Sorting

Now that we can share mutable state between tasks, we can speed up a broader
class of algorithms.  For example, let's explore how we might parallelize
sorting a mutable array.

We'll make use of two more pieces of the `Parallel` library: _parallel arrays_
and _slices_.

```ocaml
module Par_array = Parallel.Arrays.Array
module Slice = Parallel.Arrays.Array.Slice
```

A parallel array is just like an array, but with a couple restrictions that make
it safe to operate upon in parallel.  Here, we'll use `int` as the element
type&mdash;it crosses portability and contention, simplifying the example.

A slice is a `local` view of (part of) a parallel array.  Intuitively, a slice
_borrows_ a segment of the array, only allowing access to a contiguous subset of
its indices.  Using slices, we can implement a standard sequential quicksort:

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
    let length = Slice.length slice in
    let left = Slice.sub slice ~i:0 ~j:pivot in
    let right = Slice.sub slice ~i:pivot ~j:length in
    quicksort left;
    quicksort right [@nontail])
;;
```

The sequential implementation has decent performance&mdash;it sorts 10,000
random integers in 2.3 milliseconds in our
[benchmark](https://github.com/janestreet/parallel/blob/with-extensions/example/sort.ml).
However, there's a clear opportunity for parallelism: recursively sorting `left`
and `right` are independent tasks, so they could be run in parallel.  We can try
adding a fork/join, but it won't quite work:

```ocaml
let rec quicksort parallel slice =
  if Slice.length slice > 1
  then (
    let pivot = partition slice in
    let length = Slice.length slice in
    let left = Slice.sub slice ~i:0 ~j:pivot in
    let right = Slice.sub slice ~i:pivot ~j:length in
    let (), () =
      Parallel.fork_join2
        parallel
        (fun parallel -> quicksort parallel left)
(*                                          ^^^^        *)
(* The value left is local, so cannot be used inside a  *)
(* function that might escape.                          *)
        (fun parallel -> quicksort parallel right)
    in
    ())
;;
```

There are actually two issues here: slices are `local`, so we can't close over
them in a parallel task, but even if we could, they would become `contended`,
meaning we can't read or write their contents.  Instead, we can use a
specialized fork/join for slices:

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

The function `Slice.fork_join2` requires an `uncontended` slice, splits it at an
index, and provides the two halves to two parallel tasks at `uncontended`.
Since each task can only access a separate portion of the array&mdash;and the
slices are `local`, so don't escape&mdash;this is safe.

To run our parallel quicksort, we need to get an implementation of parallelism
from a scheduler.  For example, using `Parallel_scheduler_work_stealing`:

```ocaml
let quicksort ~scheduler ~mutex array =
    let monitor = Parallel.Monitor.create_root () in
    Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
        let array = Par_array.of_array array in
(*                                     ^^^^^               *)
(* This value is contended but expected to be uncontended. *)
        quicksort parallel (Slice.slice array) [@nontail])
  ;;
```

There's one last problem: capturing an existing array in `schedule` causes it to
become `contended`.  Instead, we will operate on an encapsulated array, which
assures that our caller is not mutating it in parallel.

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

Finally, we may benchmark our parallel implementation on various numbers of
domains:

Domains    | Time (ms)
-----------|----------
Sequential | 2.36
1          | 2.78
2          | 1.70
4          | 1.09
8          | 0.78
{: .table }

Our code runs faster given more domains, but quicksort only admits a limited
amount of parallelism&mdash;eventually, the cost of sequentially partitioning
the array dominates the runtime.  For this reason, other algorithms (such as
merge-sort) are often preferable in the parallel setting.

## Image Processing

Another common application of parallelism is for _data parallel_ tasks, where we
want to perform the same independent operation on a collection of data.  For
example, let's attempt to parallelize blurring an image.

We'll start by defining a simple (greyscale) image type:

```ocaml
type t : mutable_data

val load : string -> t
val of_array : float array -> width:int -> height:int -> t

val width : t @ contended -> int
val height : t @ contended -> int

val get : t -> x:int -> y:int -> float
val set : t -> x:int -> y:int -> float -> unit
```

An image is really just an array plus a width and height, but this interface has
a couple notable features:

- `mutable_data` indicates that images cross portability (among other
  axes). This is safe since an image does not contain functions.
- `width` and `height` allow a `contended` image. This is safe since the width
  and height are immutable properties of the image.

To create a blurred copy of an image, we want each pixel in the result to
contain the average of a 9x9 box of pixels in the input, centered at this pixel.
We'll use the following function to compute this average at coordinates `x,y`:

```ocaml
let blur_at image ~x ~y =
  let width = Image.width image in
  let height = Image.height image in
  let acc = ref 0. in
  let radius = 4 in
  for i = -radius to radius do
    for j = -radius to radius do
      let x =
        Int.clamp_exn (x + i) ~min:0 ~max:(width - 1)
      in
      let y =
        Int.clamp_exn (y + j) ~min:0 ~max:(height - 1)
      in
      acc := !acc +. Image.get image ~x ~y
    done
  done;
  !acc /. Float.of_int ((2 * radius + 1) * (2 * radius + 1))
;;
```

Introducing parallelism is easy: we just need to run this function for each
output pixel.  We will do so via `Par_array.init`, where each index corresponds
to one pixel.

Since we want to share the input image across multiple parallel tasks, we'll
need to provide it in a capsule.  For simplicity, we also make use of
`Capsule.access`, which lets us _unwrap_ the encapsulated image before passing
it to `blur_at`.  This pattern is explained in more detail in the [capsules
page](../../parallelism/02-capsules).

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
            let image = Capsule.Data.unwrap image ~access in
            blur_at image ~x ~y)))
    in
    Image.of_array (Par_array.to_array data) ~width ~height)
;;
```

Now, if we benchmark this implementation...

Domains | Time (ms)
--------|----------
1       | 309
2       | 705
4       | 812
8       | 977
{: .table }

...we'll find that it gets slower with more domains!  That's because our mutex
only allows one domain at a time to access the input image, destroying any
opportunity for parallelism.

Fortunately, we know that all domains only _read_ the input image, so it should
be safe for them to do so simultaneously.  However, we can't just allow
`Image.get` to read from a `contended` image&mdash;in general, up to one other
domain could be writing to it.

Hence, we need a third mode on the contention axis: `shared`, which falls in
between `contended` and `uncontended`.  The shared mode indicates that _all_
references to a value are either `shared` or `contended`, so we may read its
mutable contents in parallel.  Let's make `get` allow a shared image:

```ocaml
val get : t @ shared -> x:int -> y:int -> float
```

Now, how do we obtain a shared reference to the input image?  The shared mode is
closely related to keys: if a `unique` key indicates that we have exclusive
access to a capsule, an `aliased` key indicates that _nobody_ has exclusive
access to the capsule.  This property is enforced by the uniqueness axis, since
unlike contention, an `aliased` reference precludes the existence of any
`unique` references.  Hence, an `aliased` key can provide `shared` access to the
contents of a capsule.

Instead of protecting the input image with a mutex, we can instead use an
`aliased` key.  The existence of this key means we can never again write to the
input, but that's perfectly fine here.

```ocaml
let filter ~scheduler ~key image =
  let monitor = Parallel.Monitor.create_root () in
  Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:(fun parallel ->
    let width = Image.width (Capsule.Data.project image) in
    let height = Image.height (Capsule.Data.project image) in
    let pixels = width * height in
    let data =
      Parallel_array.init parallel pixels ~f:(fun i ->
        let x = i % width in
        let y = i / width in
        Capsule.Key.access_shared key ~f:(fun access ->
          let image =
            Capsule.Data.unwrap_shared image ~access
          in
          blur_at image ~x ~y))
    in
    Parallel_array.to_array data
    |> Image.of_array ~width ~height)
;;
```

The function `Capsule.Key.access_shared` takes an `aliased` key and provides us
with an `'k Access.t @ shared`.  We may then pass the access to
`Capsule.Data.unwrap_shared` to get our desired `Image.t @ shared`.

Now, our filter's performance scales close to linearly with additional domains:

Domains | Time (ms)
--------|----------
1       | 287
2       | 150
4       | 81
8       | 51
{: .table }

## Further Reading

Capsules, keys, and mutexes let us manipulate mutable state across parallel
tasks.  However, some data access patterns still can't be expressed with these
abstractions alone.

For example, if we needed to preserve the mutability of our input image, we
could instead protect its capsule with a reader-writer lock.  The [capsules
page](../../parallelism/02-capsules) discusses several further interfaces.
