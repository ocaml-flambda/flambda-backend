---
layout: documentation-page
collectionName: Tutorials
title: Introduction to parallelism, Part 1
---

<!-- FIXME: These should be covered by the stylesheet -->
<style>
  blockquote {
    margin-bottom: 1rem;
    padding: 0rem 2rem;
  }

  table {
    margin-bottom: 1rem;
  }

  td {
    padding: 0.2rem;
  }
</style>

# Simple Parallelism Without Data Races

OCaml 5 unleashed parallel programming on the OCaml ecosystem. As is often the
case when something is unleashed, this has quite a bit of potential to cause
chaos. Race conditions lead to bugs that are easy to write, hard to replicate,
and tricky to eliminate. But it's hard to argue with the potential performance
gains. What might the language do to help us get those speedups and spend less
time debugging nondeterministic behavior?

OxCaml provides an infrastructure for multicore OCaml programming that exploits
the full power of parallelism while _guaranteeing_ freedom from an especially
pernicious threat, that of the _data race._ That infrastructure includes many
parts, from common primitives like atomics and locks to more exotic features
like modes and capsules. Fortunately, many parallel programs only need some of
this machinery, and following this tutorial, you'll see how far you can get with
just two modes and the `Parallel.fork_join2` function.

## Multicore OCaml in a nutshell

OCaml 5 provides parallelism for otherwise perfectly ordinary OCaml code. The
program is divided into _domains,_ each of which executes in a separate OS
thread. Domains are allowed to pass values around freely to each other, so a
function may at any time be accessing values allocated by other domains—and thus
other domains may be accessing those values in parallel.

The great thing about multicore OCaml is that there is total flexibility to
structure the program without concern for who may be touching what at any time.
The problem of course is that there is total flexibility to structure the program
without concern for who may be touching what at any time. Race conditions may
abound, of which data races are especially concerning. OxCaml's data-race
freedom extensions retain the ability to run code in parallel and intermix data
between parallel domains, yet they add enough guardrails to eliminate data
races entirely.

## What is a data race?

A data race is a race condition in which two accesses to the same memory
location conflict. Specifically, there are four elements of the crime:

1. Code running in parallel, which is to say in two different domains
2. A memory location that may be accessed by both domains simultaneously
3. At least one of the accesses is a write
4. The location isn't atomic

The last condition is a carve-out for specially synchronized memory operations
provided by `Atomic.t`, which we'll cover [later on](#atomics). Any “normal”
mutable OCaml data can lead to a data race: references, `mutable` fields, and
`array`s are all possible culprits.

So, for example:

```ocaml
type record = { mutable field : int }

let reference : int ref = ref 1
let record : record = { field = 2 }
let arr : int array = [| 3; 4 |]
```

All three of these values are possible sources of data races, say if the
following two functions were run in two different domains:

```ocaml
let fun1 () =
  reference := 2;
  record.field <- 3;
  arr.(0) <- 4;
  print_int arr.(1)

let fun2 () =
  reference := 3; (* Data race: write vs. write *)
  print_int record.field; (* Data race: read vs. write *)
  arr.(0) <- arr.(0) + 1; (* Two data races! *)
  print_int arr.(1) (* This one's okay: read vs. read *)
```

## Why are data races bad?

Every race condition is a bug, but data races are especially insidious.
Obviously, the outcome is unpredictable, since we don't know which side will
“win the race,” but the problems don't end there. Both the compiler and the CPU
routinely reorder instructions for performance, and they do so assuming there
are no data races. As a consequence, in a program with data races, it's
difficult if not impossible to reason about the order in which things happen.

As an example, imagine we have two global variables:

```ocaml
let price_of_gold = ref 0.0
let initialised = ref false
```

Now consider two pieces of code running in parallel. One is some startup code
we'll call domain A:

```ocaml
(* ... *)
price_of_gold := calculate_price_of_gold ();
initialised := true
(* ... *)
```

The other, domain B, accesses the variables:

```ocaml
(* ... *)
if !initialised then
  if !price_of_gold < really_good_price_for_gold then
    buy_lots_of_gold ()
(* ... *)
```

What happens? Well, since they're running in parallel, we don't know exactly.
There is of course an intuitive way to reason it out: at any given point in
time, each domain will be partway done running, we just never know exactly which
domain gets to go next. So maybe domain A sets `price_of_gold`, and then domain
B reads `initialised` and sees that it's `false`, and then domain A sets
`initialised` to `true`. Or maybe domain A sets both variables before domain B
checks `initialised`, and then if gold is cheap, domain B heads to the market.

The one thing we want _not_ to happen is for domain B to see that
`!initialised` is `true` and yet find that `price_of_gold` is still stuck at
its initial value of `0.0`, thus causing us to `buy_lots_of_gold ()` no matter
the price. Our intuitive method reassures us that this cannot happen: domain A
doesn't set `initialised` to `true` until it's already done setting
`price_of_gold` to something sensible.

Unfortunately, our intuitive reasoning is wrong: this code very much can end up
calling `buy_lots_of_gold ()` despite `price_of_gold` being high. Since the
assignments to `price_of_gold` and `initialised` happen in the same domain and
neither is a fancy [atomic] operation, they can be reordered so that
`initialised` is set first, _while `price_of_gold` is still zero,_ leading
domain B to make an expensive mistake.

[atomic]: #atomics

It turns out our intuition assumes what the literature calls _sequential
consistency:_ essentially, the principle that a program can be fully understood
by taking the actions that each domain is going to perform and thinking about
how those actions might be _interleaved_ but not _reordered._ Maybe domain A
does all of its work first, or maybe domain B does, or maybe they go back and
forth. But in the end, if sequential consistency holds, the result will be as if
you ran all of those actions in _some_ sequence where each domain's actions
happen in order.

This principle is so essential that it's hard to imagine reasoning about a
multicore program without it. Without sequential consistency, there is no one
state of the machine. Each domain has its own view of memory reflecting a
hodgepodge of writes that other domains have performed, and since those writes
can be observed out of order, “impossible” states can occur. In our example,
if domain B observes the write to `initialised` first, it sees a state where
`initialised` is `true` and `price_of_gold` is still `0.0`. So having sequential
consistency would rule out that bad state.

Alas, OCaml doesn't
guarantee sequential consistency for this code—because of the data races.
There are two data races, to be specific: each of `price_of_gold` and
`initialised` is being modified by domain A and read by domain B in parallel.
When there is a data race, the OCaml memory model throws up its hands and says
“sorry, no sequential consistency.”[^bounded-in-space-and-time]

[^bounded-in-space-and-time]: If you come from a C/C++ or Java background, you
    may be interested to know that data races are still _considerably_ less
    catastrophic in OCaml than in other languages, where the repercussions are
    “unbounded in space and time,” which is as scary as it sounds. For details,
    see [this paper][bounding data races].

[bounding data races]: https://kcsrk.info/papers/pldi18-memory.pdf

The good news is that the inverse is also true: any program _without_ data races
_is_ sequentially consistent. *Data-race freedom gives us back the power to
reason
intuitively about our code, no matter how buggy it might get.* In our example,
that means that once we fix the data races (easy to do with `Atomic.t`), we know
our bug is gone: sequential consistency means that it's impossible for domain
B to observe a state where `initialised` is `true` and `price_of_gold` is
`0.0`.[^price-zero]

[^price-zero]: Assuming, of course, `calculate_price_of_gold ()` doesn't
    return `0.0`. The compiler can't catch everything.

The even _better_ news is that, in code using OxCaml's extensions, the data
races in `price_of_gold` and `initialised` would _never have compiled to begin
with._ There's a tradeoff, of course: just as OCaml's type safety means having
to understand the type system, the system for data-race freedom has a learning
curve. This tutorial aims to get you moving with an approach that, if it fits
your use case, aims to achieve real speedups without much fuss. You'll still
need to convince the compiler that your code has no data races, but we'll walk
through the rules and even some [shortcuts](#niceties) for common special
cases.

# Fork/join parallelism

Parallelism has so many architectures and paradigms and frameworks that “let's
parallelise this code” isn't so much a decision as the first of many decisions.
Each application is different—a relational database server needs sophisticated
synchronisation mechanisms that a batch processor may not. For this tutorial,
we're looking at an ideal, yet realistic, situation:

  * We have a large computation that we will run and get a single result out.
  * This computation can be split into independent tasks, ideally as many tasks
    as we have capacity for parallelism.
  * In the tradition of functional programming, we perform this computation by
    producing values rather than doing in-place mutation. (Later in the
    tutorial, we'll see that [atomics](#atomics) can accommodate imperative
    algorithms as well, though care is required.)

These assumptions allow us to use _fork/join parallelism_, a system that
provides parallelism without the application having to do anything with locks or
other parallel machinery. You write your algorithm and we work out the
scheduling, synchronising, blocking, etc.
Of course, OxCaml is ever watchful and will insist on being convinced there are
no data races, so you'll still have to understand the
fundamentals of data-race freedom. The [next tutorial] will build on
the basics by
covering more primitive operations that exercise more of the system.

[next tutorial]: ../02-intro-to-parallelism-part-2

## A trivial example

OxCaml's [`parallel` library] provides fork/join parallelism with the `fork_join*`
functions. For example, a very expensive way to add four
integers (we promise the other examples are more substantial) would be this:

[`parallel` library]: https://github.com/janestreet/parallel

<a id="code-add4"></a>
```ocaml
let add4 (par : Parallel.t) a b c d =
  let a_plus_b, c_plus_d =
    Parallel.fork_join2 par
      (fun _par -> a + b)
      (fun _par -> c + d)
  in
  a_plus_b + c_plus_d
```

The call to `Parallel.fork_join2` will schedule the calculations of `a + b` and
`c + d` as independent *tasks*, returning both once they're both done, and then
`add4` finishes by adding the results from the tasks. The `par` argument
parameterizes `fork_join2` (and, in turn, `add4`) by a particular implementation
of parallelism. It is also passed to the tasks so that they can spawn sub-tasks.

To run `add4`, we need to get our hands on a *scheduler,* a component that takes
in all the tasks that we want to run, decides how to dole them out into domains,
and tracks who is waiting for what to be computed. Each scheduler is provided by
a library. For this tutorial, we'll use `parallel.scheduler.work_stealing`,
which implements the popular [work-stealing] strategy.

[work-stealing]: https://en.wikipedia.org/wiki/Work_stealing

Here's some test code to get you started. The details of `run_one_test` aren't
important for this tutorial, so feel free to copy-and-paste and forget, though
a real program will want to be more thoughtful (see the [`parallel` library]
for details).

```ocaml
let test_add4 par = add4 par 1 10 100 1000

let run_one_test ~(f : Parallel.t -> 'a) : 'a =
  let module Scheduler = Parallel_scheduler_work_stealing in
  let scheduler = Scheduler.create () in
  let monitor = Parallel.Monitor.create_root () in
  let result = Scheduler.schedule scheduler ~monitor ~f in
  Scheduler.stop scheduler;
  result
;;

let () =
  let result = run_one_test ~f:test_add4 in
  Printf.printf "result: %d\n" result
```

```
result: 1111
```

This uses a work-stealing scheduler, but you can also use the `parallel`
library's own `Parallel.Scheduler.Sequential`, which simply runs everything on
the primary domain. This is handy for testing or debugging when you want to
eliminate nondeterminism. To do so, simply replace
`Parallel_scheduler_work_stealing` with `Parallel.Scheduler.Sequential` in
`run_one_test`.

## Averaging over binary trees

Now for something more substantial. Suppose we're working with binary trees, and
we want to take an average over all the values in the tree. Here's a basic
implementation (note that we've made use of the new [labeled tuples] feature):

[labeled tuples]: ../../miscellaneous-extensions/labeled-tuples

<a id="code-average"></a>
```ocaml
module Tree = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a t * 'a t
end

let average (tree : float Tree.t) =
  let rec total tree : (total:float * count:int) =
    match tree with
    | Tree.Leaf x -> ~total:x, ~count:1
    | Tree.Node (l, r) ->
      let ~total:total_l, ~count:count_l = total l in
      let ~total:total_r, ~count:count_r = total r in
      ( ~total:(total_l +. total_r),
        ~count:(count_l + count_r) )
  in
  let ~total, ~count = total tree in
  total /. (count |> Float.of_int)
;;

let test_tree = Tree.Node (Leaf 3.0, Leaf 4.0)
```

We can use `fork_join2` to parallelize `average`:

<a id="listing-average_par"></a>
```ocaml
let average_par (par : Parallel.t) tree =
  let rec total par tree : total:float * count:int =
    match tree with
    | Tree.Leaf x -> ~total:x, ~count:1
    | Tree.Node (l, r) ->
      let ( (~total:total_l, ~count:count_l),
            (~total:total_r, ~count:count_r) ) =
        Parallel.fork_join2 par
          (fun par -> total par l)
          (fun par -> total par r)
      in
      ( ~total:(total_l +. total_r),
        ~count:(count_l + count_r) )
  in
  let ~total, ~count = total par tree in
  total /. (count |> Float.of_int)
;;
```

Note that we don't have to worry about unbalanced trees: the work-stealing
algorithm dynamically adapts whenever tasks are unevenly distributed among cores.

## Trees of records

We're not limited to working with simple `float`s, of course. Suppose we add a
module `Thing` earlier in the file:

<a id="listing-thing-impl"></a>
```ocaml
module Thing = struct
  module Mood = struct
    type t =
      | Happy
      | Neutral
      | Sad
  end

  type t =
    { price : float
    ; mutable mood : Mood.t
    }

  let create ~price ~mood = { price; mood }
  let price { price; _ } = price
  let mood { mood; _ } = mood
  let cheer_up t = t.mood <- Happy
end
```

All we have to do to sum over the prices in a `Thing.t Tree.t` is change the
`Tree.Leaf` case:

```diff
  let rec total par tree : total:float * count:int =
    match tree with
-   | Tree.Leaf x -> ~total:x, ~count:1
+   | Tree.Leaf x -> ~total:(Thing.price x), ~count:1
    | Tree.Node (l, r) ->
      let ( (~total:total_l, ~count:count_l),
          (~total:total_r, ~count:count_r) )=
```

And of course you'll need a new test tree:

```
let test_tree =
  Tree.Node
    ( Leaf (Thing.create ~price:3.0 ~mood:Happy)
    , Leaf (Thing.create ~price:4.0 ~mood:Sad) )
```

## The compiler gets worried about data races

So far, so good. But something annoying happens if we introduce an abstraction
barrier. Let's move `Thing` into its own file. `thing.mli` is very simple:

```ocaml
type t

module Mood : sig
  type t =
    | Happy
    | Neutral
    | Sad
end

val create : price:float -> mood:Mood.t -> t
val price : t -> float
val mood : t -> Mood.t
val cheer_up : t -> unit
```

But now we get an error from the compiler:

```
The value total is nonportable, so cannot be used inside a
function that is portable.
```

We'll get into exactly what `portable` and `nonportable` are soon enough. For
the moment, just know that they are _modes_. Where types describe the exact
shape of a value, a mode describes something more circumstantial like what's
safe to do with the value. We'll say that something “has mode `portable`,” or
say “with mode `portable`” or “at mode `portable`,” or simply that something “is
`portable`,” all meaning the same thing.

Now, let's confront a more basic question: Why are things breaking just because
we moved some code? We can get closer to the answer by insisting that `total` is
`portable`:

```diff
 let average_par (par : Parallel.t) tree =
-  let rec total par tree =
+  let rec (total @ portable) par tree =
     match tree with
```

```
The value Thing.price is nonportable, so cannot be used inside a function that
is portable.
```

What's going on is that our declaration for `Thing.price` left out crucial
information that the compiler was previously able to infer, back when `Thing`
was in the same module. We can take the compiler's suggestion easily enough by
explaining that `price` is `portable` in `thing.mli`:

```ocaml
val price : t -> float @@ portable
```

But the compiler isn't satisfied. Now it complains about the `l` in
`total par l` (which is still as it was [before]):

[before]: #listing-average_par

```
This value is contended but expected to be uncontended.
```

(Again, `contended` and `uncontended` will be covered soon.)
Now the solution is much less obvious, but as we'll see, it again comes down to
missing information on `price`:

```ocaml
val price : t @ contended -> float @@ portable
```

(The precedences here are such that the `t` argument is `contended` and the
whole function, namely `price`, is `portable`.)

Once you're familiar with the `portable` and `contended` modes, you'll see that
this can be read as saying:

> The `price` function is safe to call from any domain, and it won't produce a
> data race even if its argument might be mutated in parallel.

Why should we believe this is true? Well, remember, `price` is simply

```ocaml
let price { price; _ } = price
```

It does nothing but project an _immutable_ field out of a record. And as we
covered in [defining a data race](#what-is-a-data-race), rule 3 is that in order
for two accesses to produce a data race, one of them must be a write—and an
immutable field can't be modified[^write-on-initialization]. Hence `price` can't
produce a data race no matter what is happening in parallel. The situation
changes if we make `price` mutable:

```diff
   type t =
-    { price : float
+    { mutable price : float
     ; mutable mood : Mood.t
     }

```

```
Values do not match:
  val price : t -> float @@ portable
is not included in
  val price : t @ contended -> float @@ portable
```

The type that the compiler now wants to give `price` says essentially “this
is only safe to call if no other domain can access the argument.” But
`Parallel.fork_join2` can't make that promise, since it's scheduling tasks in
parallel. (If this all seems vague at the moment, we'll make it systematic when
we detail how `portable` and `contended` work.)

Giving `price` the right type is enough to let `average_par` go through: from
there, the compiler infers that our calls to `Parallel.fork_join2` won't produce
data races, and in turn that `average_par` is again data-race free.

If you modify our test code to run `average_par` on the new `test_tree`, you'll
find yourself needing to make one more change to `thing.mli`:

```ocaml
val create : price:float -> mood:Mood.t -> t @ portable
  @@ portable
```

This says somewhat tediously that `create` both _returns_ something `portable`
and _is itself_ `portable`. Fortunately, we'll see in the section on
[niceties](#niceties) that both of these can be made implicit.

Adding `portable` and `contended` to your .mli files can take some work, but
over the next few sections we'll cover what exactly the modes mean, what the
most important rules for using them are, and some workarounds and shortcuts. In
return, these two modes suffice to run even large amounts of code in fork/join
style without fear of data races.

## How `portable` and `contended` work

Firstly, `portable` and `contended` are _modes_. Like a type, a mode describes
something about a name in an OCaml program. But whereas a type describes the
*value* associated with a name, a mode instead describes the value's
*circumstances.* This could be where it is in memory, who has access to it, or
what can be done with it. If you've worked with [stack allocation], you've
already encountered the `local` mode.

[stack allocation]: ../../stack-allocation/intro

The `portable` mode is the one you'll see most often, but it will be easier to
understand once we've covered `contended` and `uncontended`, so we begin there.

### The `contended` and `uncontended` modes

We said [before](#what-is-a-data-race) that a data race needs four things.

1. Code running in parallel, which is to say in two different domains
2. A memory location that may be accessed by both domains simultaneously
3. At least one of the accesses is a write
4. The location isn't atomic

The `contended` mode and its opposite, `uncontended`, prevent data races by
ensuring that this is impossible. They institute two key rules:

> <a id="rule-contended-parallel"></a>
> **Rule 1.** If two or more domains may access the same value, at most one of
> them may consider the value `uncontended`. The others must consider it
> `contended`.

> <a id="rule-contended-mutable"></a>
> **Rule 2.** Reading or modifying a `mutable` field is not allowed if the
> record is `contended`. The same goes for any element of a `contended` `array`.

Taken together, these two rules guarantee data-race freedom: it's not allowed
for two domains to access the same location in memory if either of them could
possibly modify it.

Let's see what these rules mean for our `thing.ml` from
[before](#listing-thing-impl):

```ocaml
type t = {
  price : float;
  mutable mood : Mood.t
}
```

We can force an argument to have a particular mode using the `@` syntax.

```ocaml
let price (t @ contended) =
  t.price (* ok: [price] is immutable *)
let cheer_up (t @ contended) =
  t.mood <- Happy (* error! *)
```

```
Error: This value is contended but expected to be
uncontended.
```

This is of course [rule 2](#rule-contended-mutable). To see why this has to be
an error, consider that since `t` is `contended`, there may be another domain
trying to mutate it in parallel:

```ocaml
let bum_out t =
  t.mood <- Sad (* cue ominous music: a data race lurks! *)
```

So we can't allow the access in `cheer_up` unless we flag the argument as
`uncontended`:

```ocaml
let cheer_up (t @ uncontended) =
  t.mood <- Happy (* ok: [t] is [uncontended] *)
```

Note that you can also let the compiler infer that `t` is `uncontended` (as we
did for `bum_out`). Much like with types, adding explicit modes is often useful
either to make your intentions clear to other humans or nail down the ultimate
cause of a compiler error. Also like with types, these explicit declarations are
checked, so you can't use them to sneak unsafe code by the compiler
(accidentally or otherwise).

Note that rule 2 forbids even _reading_ the mutable state:

```ocaml
let mood (t @ contended) = t.mood (* error! *)
```

```
Error: This value is contended but expected to be shared or
uncontended.
```

This is dangerous for the same reason `cheer_up` was: someone else could be
running `bum_out` in parallel, producing a data race.[^shared]

[^shared]: You may have noticed the `shared` in the error message here. The
    `shared` mode lies in between `contended` and `uncontended` in that it
    allows reading but not writing mutable fields. It's less common than the
    others but it comes in handy for things like read/write locks.

Adding `uncontended` signals to the compiler that a data race is possible: if
`cheer_up` and `bum_out` can be called with the same argument in parallel, we
have a data race. Enter [rule 1](#rule-contended-parallel), which simply says
_it is not permissible_ to do so. Unlike rule 2, there's no one place where rule
1 is enforced—really, it's an invariant of the whole system (language, runtime,
and core libraries)—but let's see one example of trying to break it. First, make
sure `cheer_up` and `bum_out` have the right signatures in `thing.mli`:

```ocaml
val cheer_up : t -> unit @@ portable
val bum_out : t -> unit @@ portable
```

They both take their argument `uncontended`, so it would certainly be a shame
if they could be called in parallel:

```ocaml
let beat_the_system par =
  let t = { price = 42.0; mood = Neutral } in
  let (), () =
    Parallel.fork_join2 par
      (fun _par -> cheer_up t)
      (fun _par -> bum_out t)
  in
  ()
```

Fortunately:

```
Error: This value is contended but expected to be
uncontended.
```

When we cover the `portable` mode [next], we'll be able to explain precisely
what's going on here, but for the moment suffice it to say that the type of
`fork_join2` forces the two tasks to obey rule 1: since the tasks may be running
in separate domains, they are forbidden to access `t` as `uncontended`.

[next]: #the-portable-and-nonportable-modes

You may worry that the different modes on `price` and `mood` prevent writing
sensible code like

```ocaml
let price_and_mood (t @ uncontended) = price t, mood t
```

Can we really treat `t` as both `contended` (to call `price`) and `uncontended`
(to call `mood`)? Actually, we can:

<a id="rule-contended-submode"></a>
> **Rule 3.** An `uncontended` value may be used as though it is `contended`.

This is entirely consistent with rules 1 and 2: having an `uncontended` value
gives strictly more power than having a `contended` one, so it's always safe to
forget we have that power. The upshot is that if a function takes a `contended`
parameter then the caller is _allowed_ to pass a `contended` value but it is not
required to. On the other hand, `uncontended` is a constraint that _must_ be
met.[^local-means-allowed]

[^local-means-allowed]: If you've worked with `local` and `global`, you may know
    they have a similar relationship: if a function takes a `local` argument,
    you're _allowed_ to pass something `local` but not required, whereas
    `global` is a hard requirement.

Finally, recall that our running example wants to fork/join over an entire tree
of `Thing.t`s, so we should consider what happens when the `Thing.t` is in a
bigger data structure.

> <a id="rule-contended-deep"></a>
> **Rule 4.** Any component of a `contended` value is `contended`.

In particular, every field of a `contended` record, every element of a
`contended` tuple or `array`, and every argument
of every constructor of a `contended` variant is `contended`. And of course this
applies recursively, so the components of _those_ components are also
`contended`. Accordingly, we say that the `contended` mode is _deep._ It's easy
to see what goes wrong if we let anyone treat, say, a field as `uncontended`
when its record is `contended`:

<a id="code-cheer_up_sneakily"></a>
```ocaml
type t_in_a_trenchcoat = {
  inner_t : t;
}

let cheer_up_sneakily (t_in_a_trenchcoat @ contended) =
  let t @ uncontended =
    t_in_a_trenchcoat.inner_t (* error: rule 4 *)
  in
  cheer_up t (* cue the ominous music again *)
```

If not for rule 4, we could also write `bum_out_sneakily` and then call
`cheer_up_sneakily` and `bum_out_sneakily` on the same argument in parallel,
causing the same data race we've been trying to avoid all along.

In fact, rule 4 is pretty well required to make [rule 1] work at all. The
interested reader is invited to work out why the text of rule 1 demands that,
for instance, we say that `t_in_a_trenchcoat.inner_t` is `contended` when
`t_in_a_trenchcoat` is `contended`.

[rule 1]: #rule-contended-parallel

Now, you may be surprised to see that projecting a field from a `contended`
record gives a `contended` value. After all, we have the function:

```ocaml
let price t = t.price
```

and we stated that its type is:

```ocaml
val price : t @ contended -> float @@ portable
```

Shouldn't the returned value be `contended`? In other circumstances it would: if
you try changing the `float` in the definition of `t` to a `float ref` then
you'll find you indeed have to add `contended` to the return type of `price`.
We'll cover the details in the section on [mode crossing], but the short answer
is that since `float`s have no mutable parts, no data race can arise from them,
so the compiler simply ignores `contended` and `uncontended` on `float`s (and
you can make use of this for your own deeply immutable data as well).

[mode crossing]: #mode-crossing

### The `portable` and `nonportable` modes

As we said before, [rule 1](#rule-contended-parallel) and [rule
2](#rule-contended-mutable) of `contended` give us data-race freedom—assuming we
can enforce them, that is. The compiler's type checker can just raise an error
when rule 2 is violated, but rule 1 is a bit squishier. To reiterate:

> **Rule 1 of `contended`.** If two or more domains may access the same value,
> at most one of them may consider the value `uncontended`. The others must
> consider it `contended`.

We've seen that `Parallel.fork_join2` provides parallelism while enforcing this
rule. How does it manage that? Let's look again at our attempt to sneak a data
race by it, adding a few things for illustration:

```ocaml
let beat_the_system par =
  let t @ uncontended = { price = 42.0; mood = Neutral }
  cheer_up t; (* line A *)
  let (), () =
    Parallel.fork_join2 par
      (fun _par -> cheer_up t) (* line B *)
      (fun _par -> bum_out t) (* line C *)
  in
  ()
```

Firstly, the new annotation on `t` should be uncontroversial: we just created
`t`, so clearly there aren't any parallel accesses at all, much less
`uncontended` accesses. Accordingly, the access on line A is fine: it sees `t`
as `uncontended` and it is. On the other hand, the access from line B is clearly
bad: that code might[^or-same-domain] be running in parallel (in particular, in
parallel with line C), and it's still assuming `t` is `uncontended` (as is line
C).

[^or-same-domain]: We say “might” because `Parallel.fork_join2` may choose not
    to run the tasks in parallel. The compiler, as usual, has to be pessimistic.

In summary, the arguments to `fork_join2`

1. need to be designated as “this might run in any domain,” and they
2. should not be allowed to consider `t` to be `uncontended`.

As you might have guessed, this is exactly what the `portable` mode is for. Both
arguments to `fork_join2` are required to be `portable`, and we have two rules:

> <a id="rule-portable-safety"></a>
> **Rule 1.** Only a `portable` value is safe to access from outside the domain
> that created it.

> <a id="rule-portable-closure"></a>
> **Rule 2.** If a `portable` function refers to a value outside of its own
> definition, then (a) that value must be `portable`, and (b) the value is
> treated as `contended`.

(So far it looks like `portable` is only relevant for functions, but when we get
to [rule 4](#rule-portable-deep), we'll see that records and arrays can also
fail to be `portable`.)

Why does rule 2 say “outside of its own definition”? Well, remember what we
said about the `t` defined in `beat_the_system` above: it was just now created,
so we knew that it was `uncontended`. Similarly, the following function is
`portable`:

```ocaml
let (factorial @ portable) i =
  let a @ uncontended = ref 1 in
  let rec (loop @ nonportable) i =
    if i > 1 then (a := !a * i; loop (i - 1))
  in
  loop i;
  !a
```

As before, `a` is `uncontended` because it was just created. We can have
`factorial` access `a` because it's allowed to treat things _inside_ its
definition as `uncontended`. (Note that a `ref` is just a record whose only
field is `mutable`, so `!a` requires `uncontended` as always.) On the
other hand, if we try and mark `loop` as `portable`, the compiler sees that `a`
is defined _outside_ of `loop`, so `a := !a * i` gets hit with the familiar

```
This value is contended but expected to be uncontended.
```

because rule 2b insists that it treat `a` as `contended`. Fortunately, `loop`
doesn't need to be `portable`: since it's defined inside `functorial`, it's
safe for `functorial` to call `loop` even though `loop` isn't `portable`.
(Remember, rule 1 says we can't call `loop` from _outside the domain that
created it._ Since `factorial` is `portable`, that could be any domain, but
nonetheless its whole body executes in one consistent domain.)

Interestingly, we actually _can_ make `loop` portable:

```ocaml
let (factorial' @ portable) i =
  let a @ uncontended = ref 1 in
  let rec (loop' @ portable) (a @ uncontended) i =
    if i > 1 then (a := !a * i; loop' a (i - 1))
  in
  loop' a i;
  !a
```

A function's parameters aren't defined “outside the function” for the
purposes of rule 2, so `loop'` is in fact allowed to use `a` as `uncontended`.
This may seem a bit arbitrary: in both cases, the inner function says “I need a
value called `a` that's an `int ref` at mode `uncontended`,” but somehow rule 2
only cares when the `a` is something from an outer scope rather than a
parameter. The difference is in the types:

```ocaml
val loop : int -> unit
val loop' : int ref @ uncontended -> int -> unit @@ portable
```

By expressing the requirement on `a` in its type, `loop'` makes its caller take
on the responsibility of providing an `uncontended` value. In contrast, `loop`
advertises nothing—or rather, its being `nonportable` advertises that it
requires _some unknown number of `uncontended` values._ The caller can't hope
to provide that, so in general you can't call a `nonportable` function unless
you know you can access all the same `uncontended` values that it can (which is
to say, unless you know you're in the same domain).

Of course, making `loop` `portable` raises a question: Can we now use
`fork_join2` to parallelize it? Obviously the answer had better be “no,”
since it would be a whole rat's nest of data races, but the reason it falls
down is a bit subtle. It is instructive, though, so the interested reader is
encouraged to try it as an exercise: if `loop'` tries to call itself via
`Parallel.fork_join2`, what error does the compiler raise and why?[^answer-loop]

[^answer-loop]: Answer to the exercise: If you put `loop' a (i - 1)` into a call
    to `Parallel.fork_join2`, the compiler complains that `a` is `contended` but
    expected to be `uncontended`. Crucially, even though `loop'` is `portable`,
    _the argument to `fork_join2`_ is what ultimately needs to be `portable`,
    and so _it_ still can't access `a` at `uncontended`. Of course, it could
    instead create a fresh reference and pass that into `loop'`, but then it
    would genuinely not be a data race (admittedly it would also not be useful).

Just as it's safe to forget a value's privileged `uncontended` status and
downgrade it to `contended`, there's no danger in treating something `portable`
as if it's `nonportable`:

> **Rule 3.** A `portable` value may be treated as `nonportable`.

And also for similar reasons as before, we need `portable` to be deep the way
`contended` is:

<a id="rule-portable-deep"></a>
> **Rule 4.** Every component of a `portable` value must be `portable`.

You may have been wondering why rules 1 and 2a restrict all values, when really
it's only functions that we care about being `portable` or
not.[^other-code-types] This is the reason: many values can _contain_ functions,
and if it weren't for rule 4, we could use, say, a record to smuggle the
function around (in other words, the record could be a function [in a
trenchcoat]). So we have to forbid _all accesses_ of `nonportable` values from
other domains, not just function calls. The good news is that many types (in
fact, most types) obviously _can't_ cause a problem—you'll never make an `(int *
string option) list` that contains _any_ function, much less one that will cause
a data race—and if the compiler is aware that a type `t` can't have a function
in it, it can ignore portability requirements altogether for values of `t`.
We'll cover the specifics when we get to [mode crossing].

[^other-code-types]: Note that some other types are secretly function types,
    notably `Lazy.t`, so `portable` is also a concern for them.

[in a trenchcoat]: #code-cheer_up_sneakily
[mode crossing]: #mode-crossing

We can summarize the rules of `portable` and `contended` like so:

|        | `contended` | `portable` |
|--------|-------------|------------|
| Rule 1 | No parallel `uncontended` accesses | Only `portable` values may cross domains |
| Rule 2 | No accessing `contended` `mutable` state | A `portable` function only sees `portable` `contended` values |
| Rule 3 | Can treat `uncontended` as `contended` | Can treat `portable` as `nonportable` |
| Rule 4 | Everything in a `contended` value is `contended` | Everything in a `portable` value must be `portable` |

# Niceties

As with most systems, data-race freedom involves a tradeoff between safety and
convenience. Fortunately there are powerful ways to keep things convenient in
common cases.

## Defaulting to `portable`

If you need to add `@@ portable` to one function in an .mli, you'll usually soon
find yourself adding `@@ portable` to most or all of them. In such a case, you
can “portabilize” the whole .mli at once simply by adding `@@ portable` at the
top. Similarly you can have `@@ portable` as the first thing in a `sig` to make
each function `portable`. In either case, you can always say `@@ nonportable` to
opt out an individual function.

So for example, we can take our erstwhile `thing.mli` with the changes we've
made to date (adding one more function for illustrative purposes):

```ocaml
type t

module Mood : sig
  type t =
    | Happy
    | Neutral
    | Sad
end

val create : price:float -> mood:Mood.t -> t @ portable
  @@ portable
val price : t @ contended -> float @@ portable
val mood : t -> Mood.t @@ portable
val cheer_up : t -> unit @@ portable
val bum_out : t -> unit @@ portable
val do_something_involving_shared_state : unit -> unit
```

And change the default to `portable`:

```ocaml
@@ portable

type t

module Mood : sig
  type t =
    | Happy
    | Neutral
    | Sad
end

val create : price:float -> mood:Mood.t -> t @ portable
val price : t @ contended -> float
val mood : t -> Mood.t
val cheer_up : t -> unit
val bum_out : t -> unit
val do_something_involving_shared_state : unit -> unit
  @@ nonportable
```

## Mode crossing

In discussing why [`portable` is deep], we mentioned that very few types care
about `portable`, since the values of most types aren't functions and don't
contain functions. Similarly, if a type `t` doesn't have any `mutable` fields or
mutable arrays, having a `contended` or `uncontended` `t` makes no difference.

[`portable` is deep]: #rule-portable-deep

Fortunately, the compiler can be made aware that a type “doesn't care” about
`portable` or `contended`:

```ocaml
type float_and_int : immutable_data = float * int
```

The `immutable_data` here is called a _kind._ Just as the value `(1.2, 3)` has
the type `float_and_int`, in turn the type `float_and_int` has the kind
`immutable_data`. Also like types, kinds are inferred where possible, so in
fact the `: immutable_data` in this case is redundant. However, in an .mli
they are often useful.

The three most important kinds for data-race freedom are `immutable_data`,
`mutable_data`, and `value`. Some kinds constrain what types they describe, and
in return, any type with such a kind gets to _cross_ certain modes, in essence
ignoring those modes. In summary (this table isn't nearly exhaustive—see the
[documentation on kinds] for many more modes and what kinds cross them):

[documentation on kinds]: ../../kinds/intro

| Kind | Requirements | Crosses |
| ---- | ------------ | ------- |
| `immutable_data` | no functions or mutable fields, deeply | portability, contention |
| `mutable_data` | no functions, deeply | portability |
| `value` | none | none |

(Note that unboxed types like `int64#` can't have any of these kinds, since
they have different kinds that express how they're represented in memory and
in registers. Nonetheless, they are all immutable data and thus cross both
portability and contention.)

A type crossing portability means that `portable` and `nonportable` are
irrelevant for that type: a `nonportable` value can be used as though it were
`portable`. The same goes for contention with `contended` and `uncontended`.

```ocaml
let always_portable (a : float_and_int @ nonportable)
  : float_and_int @ portable
  =
  let a' @ portable =
    a (* ok because [float_and_int] crosses portability *)
  in
  a'
```

In fact, we've already seen mode crossing in action. We mentioned before when
talking about [rule 4 of `contended`](#rule-contended-deep) that when
`Thing.price` projects the `price` field out of a `contended` `Thing.t`, the
only reason it gets to return an `uncontended` `float` is that `float` crosses
contention.

We can make things more convenient with `Thing.t` as well. Recall that its
definition is simply:

```ocaml
type t =
  { price : float
  ; mutable mood : Mood.t
  }
```

This clearly falls under `mutable_data`, which crosses portability. So we can
go into `thing.mli` and make a few changes. First, we can change the type:

```diff
-type t
+type t : mutable_data
```

Now we can simplify the definition of `create`:

```diff
-val create : price:float -> mood:Mood.t -> t @ portable
+val create : price:float -> mood:Mood.t -> t
```

(At one point it also had a `@@ portable` but we took it out by [defaulting
to `portable`].)

[defaulting to `portable`]: #defaulting-to-portable

Since `Thing.t` is now `mutable_data`, every `Thing.t` is implicitly assumed to
be `portable`.

Any type variable can be given a kind, so we can write a version of
`always_portable` that works for _any_ `mutable_data` type:

```ocaml
let always_portable' (a : ('a : mutable_data) @ nonportable)
  : 'a @ portable
  =
  let a' @ portable =
    a (* ok because ['a] crosses portability *)
  in
  a'
```

The type of `always_portable'` is:

```ocaml
val always_portable'
   : ('a : mutable_data) @ nonportable
  -> 'a @ portable
```

We can read this as “Given any type `'a` of kind
`mutable_data`, this function takes a `nonportable` `'a` and returns a
`portable` `'a`.”

## Atomics

We've seen that `mutable` fields can cause quite some trouble: they require
`uncontended` access, which can make functions not `portable`, and generally
they very much get in the way. In some cases, however, you _may_ (with
significant caveats) be able to swap out `mutable` for `Atomic.t`. Let's do
that with our `thing.ml`:

```ocaml
module Thing = struct
  module Mood = struct
    type t =
      | Happy
      | Neutral
      | Sad
  end

  type t : immutable_data =
    { price : float
    ; mood : Mood.t Atomic.t
    }

  let create ~price ~mood =
    { price; mood = Atomic.make mood }
  let price { price; _ } = price
  let mood { mood; _ } = Atomic.get mood
  let cheer_up { mood; _ } = Atomic.set mood Happy
  let bum_out { mood; _ } = Atomic.set mood Sad
end
```

As you can see, there's a bit of syntactic overhead, but in return we get to
access `mood` even if a `Thing.t` is `contended`, and in fact we can mark `t` as
`immutable_data` so that it ignores `contended` and `uncontended` altogether
(that is, it [crosses contention]). Do go over the documentation in the
[`Atomic` module], as it has many useful operations,
from `compare_exchange` to atomic logical bitwise XOR. Also, note that we're
using Core's `Atomic` here rather than the `Atomic` from OxCaml's standard
library, which hews closer to the upstream OCaml standard library and doesn't
support mode crossing.

[crosses contention]: #mode-crossing
[`Atomic` module]: https://github.com/janestreet/portable/blob/master/kernel/src/atomic.mli

An `Atomic.t` can be handy outside of a record as well, in cases where you would
otherwise use a `ref` to hold mutable state. For example, rather than return
the total and count from our fork/join tasks, we can keep the running total and
count in atomics:

```ocaml
let average_par_running (par : Parallel.t) tree =
  let total = Atomic.make 0.0 in
  let count = Atomic.make 0 in
  let rec go par tree =
    match tree with
    | Tree.Leaf x ->
      Atomic.update total
        ~pure_f:(fun total -> total +. Thing.price x);
      Atomic.incr count
    | Tree.Node (l, r) ->
      let (), () =
        Parallel.fork_join2 par
          (fun par -> go par l)
          (fun par -> go par r)
      in
      ()
  in
  go par tree;
  Atomic.get total /. (Atomic.get count |> Float.of_int)
```

If we kept `total` and `count` in `ref`s, then `go` would not be able to access
them. (Exercise: What rules combine to stop us? Remember, a `ref` is just a
record whose only field is `mutable`. Don't rely on rule 1 of `contended` or
rule 1 of `portable` for your answer—those only tell us that we _must be
stopped,_ not what actually stops us.[^answer-go-ref])

[^answer-go-ref]: Answer to the exercise: Rule 2 of `portable` says that `go`
    can only access `total` and `count` at mode `contended`, and rule 2 of
    `contended` then says that `go` can't read or write the value at either
    (since that value is stored as a `mutable` field of a record).

Now for the caveats: Firstly, there are performance penalties, since an
`Atomic.t` is a pointer and atomic operations are more expensive. Secondly,
`Atomic.t` frees us from concerns about data races, but it absolutely does not
prevent race conditions in general. There are few guarantees about the order in
which the updates to `total` and `count` occur: we know only that they all
happen before the outermost `fork_join2` returns.

Of course, in this case what saves us is that it doesn't matter what order the
updates happen in: addition is commutative. However, now that we've made `mood`
atomic as well, suppose we wanted to take the average of all the nodes whose
`mood` is `Happy`? Nothing in the system stops us now: if someone is calling
`bum_out` on the entire tree in parallel, then our average will reflect an
unpredictable number of those changes. The only way to stop _that_ is to use
something more sophisticated like a lock over the whole tree, which grants a
function `uncontended` access while the lock is held (which is safe because of
course only one domain can hold the lock). See the [capsule API] for details.

[capsule API]: ../../parallelism/02-capsules

The good news is that data-race freedom guarantees that even buggy programs can
be reasoned about intuitively. See [Why are data races bad?].

[Why are data races bad?]: #why-are-data-races-bad

## Immutable arrays

Another possible way to avoid getting tangled in `contended` is to change your
`array`s into `iarray`s. [Rule 2 of `contended`](#rule-contended-mutable)
forbids accessing an element of a `contended` `array`—that's specifically a
`contended` value of the OCaml type `array`. Since an `iarray`'s elements are
immutable, they're exempt from the rule (just like immutable fields of a
record). Obviously, your code may not be able to make this change easily.
However, since `iarray`s are a new feature, it's worth checking whether your
`array`s are ever actually mutated. If not, changing them to `iarray`s means
accesses no longer have to be `uncontended`—and possibly even that your record
can be `immutable_data`.

# Parallel sequences

Our very first example added exactly four integers, and generally we've been
assuming that our data is in a shape that makes it obvious how to parallelize
(or at least makes _one_ strategy obvious). But of course real data isn't so
convenient:

```ocaml
let add_many (arr : int iarray) =
  Iarray.fold arr ~init:0 ~f:(fun a b -> a + b)
```

(We'll be using immutable arrays a lot, since mutable arrays are miserable for
parallelism. A mutable array is essentially nothing but a series of `mutable`
fields, [requiring `uncontended` access] to do almost anything.)

[requiring `uncontended` access]: #rule-contended-mutable

Parallelizing this directly is possible but fussy even in this simple case. The
`Parallel.Sequence` module makes it nearly trivial:

```ocaml
let add_many_par par arr =
  let seq = Parallel.Sequence.of_iarray arr in
  Parallel.Sequence.reduce par seq ~f:(fun a b -> a + b)
  |> Option.value ~default:0
```

We first need to convert from `iarray` to [`Parallel.Sequence.t`], a general
sequence type supporting a rich selection of parallel operations. Among them is
a parallel `reduce`, which operates much like an unordered `fold`.

[`Parallel.Sequence.t`]: https://github.com/janestreet/parallel/blob/with-extensions/sequence/parallel_sequence_intf.ml

Just like `Parallel.fork_join2`, we can use parallel sequences in a nested
manner. Suppose that rather than a binary tree we have an n-ary tree:

```ocaml
module Tree = struct
  type 'a t =
    | Leaf of 'a
    | Nodes of 'a t iarray
end
```

The sequential code isn't much different from [before]:

[before]: #code-average

```ocaml
let average tree =
  let rec total tree =
    match tree with
    | Tree.Leaf x -> ~total:(Thing.price x), ~count:1
    | Tree.Nodes arr ->
      let totals_and_counts =
        Iarray.map arr ~f:(fun subtree -> total subtree)
      in
      Iarray.fold
        totals_and_counts
        ~init:(~total:0.0, ~count:0)
        ~f:(fun (~total:total_acc, ~count:count_acc)
                (~total, ~count) ->
              ( ~total:(total +. total_acc),
                ~count:(count + count_acc) ))
  in
  let ~total, ~count = total tree in
  total /. (count |> Float.of_int)
;;
```

Naturally, we now use `Iarray.map` to recurse on subnodes and `Iarray.fold` to
combine the results. To parallelize, we still need the
`Parallel.Sequence.of_iarray`, but now `Parallel.Sequence.fold'` combines
the `map` and `reduce` operations:

```ocaml
let average_par (par : Parallel.t) tree =
  let rec (total @ portable) par tree =
    match tree with
    | Tree.Leaf x -> ~total:(Thing.price x), ~count:1
    | Tree.Node arr ->
      let seq = Parallel.Sequence.of_iarray arr in
      Parallel.Sequence.fold' par
        seq
        ~f:(fun par subtree -> total par subtree)
        ~init:(~total:0.0, ~count:0)
        ~combine:(fun _par (~total, ~count)
                           (~total:total2, ~count:count2) ->
                    ( ~total:(total +. total2),
                      ~count:(count + count2) ))
      [@nontail]
  in
  let ~total, ~count = total par tree in
  total /. (count |> Float.of_int)
```

(We use `fold'` rather than `fold` to get the version that passes `par` down
into our `f` and `combine` functions.)

Another approach would be to convert the entire tree into a
`Parallel.Sequence.t` at once rather than rely on nested parallelism. This is
possible using `Parallel.Sequence.unfold`, though it does take more work,
requiring a definition of an unfolding state with an operation that splits it in
half.
