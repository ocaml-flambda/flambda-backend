# Parallelism Tutorial

# Introduction

## What is a data race?

## Why are data races bad?

Every race condition is a bug, but data races are especially insidious. Both the
compiler and the CPU routinely reorder instructions for performance, and they do
so assuming there are no data races. As a consequence, in a program with data
races, it's difficult if not impossible to reason about the order in which
things happen.

As an example, imagine we have two global variables:

```ocaml
let price_of_gold = ref 0.0
let initialised = ref false
```

Now consider two pieces of code running in parallel. One is some startup code
we'll call thread A:

```ocaml
(* ... *)
price_of_gold := calculate_price_of_gold ();
initialised := true
(* ... *)
```

The other, thread B, accesses the variables:

```ocaml
(* ... *)
if !initialised then
  if !price_of_gold < really_good_price_for_gold then
    buy_lots_of_gold ()
(* ... *)
```

What happens? Well, since they're running in parallel, we don't know exactly.
There is of course an intuitive way to reason it out: at any given point in
time, each thread will be partway done running, we just never know exactly which
thread gets to go next. So maybe thread A sets `price_of_gold`, and then thread
B reads `initialised` and sees that it's `false`, and then thread A sets
`initialised` to `true`. Or maybe thread A sets both variables before thread B
checks `initialised`, and then if gold is cheap, thread B heads to the market.

The one thing we want _not_ to happen is for thread B to see that
`!initialised` is `true` and yet find that `price_of_gold` is still stuck at
its initial value of `0.0`, thus causing us to `buy_lots_of_gold ()` no matter
the price. Our intuitive method reassures us that this cannot happen: thread `A`
doesn't set `initialised` to `true` until it's already done setting
`price_of_gold` to something sensible.

Unfortunately, our intuitive reasoning is wrong: this code very much can end up
calling `buy_lots_of_gold ()` despite `price_of_gold` being high. Since the
assignments to `price_of_gold` and `initialised` happen in the same thread and
neither is a fancy [atomic] operation, they can be reordered so that
`initialised` is set first, leading thread `B` to make an expensive mistake.

[atomic]: #atomics

It turns out our intuition assumes what the literature calls
_sequential consistency:_ essentially, the principle that things happen in order
they're written, and parallelism just means we're not sure which thread will do
its next thing. This principle is so essential that it's hard to imagine
reasoning about a multicore program without it.

Of course, what's happened here is two data races: each of `price_of_gold` and
`initialised` is being modified by thread A and read by thread B in parallel.
So the moral of the story is that data races violate sequential consistency.

The good news is that the inverse is also true: any program without data races
is sequentially consistent. Data-race freedom gives us back the power to reason
intuitively about our code, no matter how buggy it might get. And in our
example, we can prove that replacing our `ref`s with `Atomic.t`s fixes our bug:
sequential consistency means that it's impossible for thread B to observe a
state where `initialised` is `true` and `price_of_gold` is `0.0`.[^price-zero]

[^price-zero]: Assuming, of course, `calculate_price_of_gold ()` doesn't
return `0.0`. The compiler can't catch everything.

# Fork/join parallelism

Any problem that can be neatly subdivided is a good candidate for
_fork/join parallelism,_ as provided by the `parallel` library’s `fork_join*`
functions. For example, a very expensive way to add four integers (I promise the
other examples are more substantial) would be this:

<a id="code-add4"></a>
```ocaml
let add4 (par : Parallel.t) a b c d =
  let a_plus_b, c_plus_d =
    Parallel.fork_join2 par (fun _par -> a + b) (fun _par -> c + d)
  in
  a_plus_b + c_plus_d
```

The call to `Parallel.fork_join2` will schedule the calculations of `a + b` and
`c + d` as independent *tasks*, returning both once they're both done, and then
`add4` finishes by adding the results from the tasks. The `par` argument
parameterizes `fork_join2` (and, in turn, `add4`) by a particular implementation
of parallelism. It is also passed to the tasks so that they can spawn sub-tasks.

To run `add4`, we need to get our hands on a *scheduler*. Each scheduler is
provided by a library and determines the policy by which tasks get doled out and
run in domains. For this tutorial, we'll use `parallel_scheduler_work_stealing`,
which implements the popular [work-stealing] strategy.

[work-stealing]: https://en.wikipedia.org/wiki/Work_stealing

```ocaml
let test_add4 par = add4 par 1 10 100 1000

let%expect_test "add4 in parallel" =
  let module Scheduler = Parallel_scheduler_work_stealing in
  let scheduler = Scheduler.create () in
  let monitor = Parallel.Monitor.create_root () in
  let result = Scheduler.schedule scheduler ~monitor ~f:test_add4 in
  Scheduler.stop scheduler;
  print_s [%message (result : int)];
  [%expect {| (result 1111) |}];
;;
```

This creates a work-stealing scheduler, along with a _monitor_ to manage
exceptions. Then it tells the scheduler to run the `test_add4` function before
shutting down the scheduler. (Naturally, a real program will want to keep the
monitor and scheduler around longer!)

You can also use the `parallel` library's own `Parallel.Scheduler.Sequential`,
which simply runs everything on the primary domain. This is handy for testing or
debugging when you want to eliminate nondeterminism. To do so, simply replace
`Parallel_scheduler_work_stealing` with `Parallel.Scheduler.Sequential`.

Now for something more substantial. Suppose we're working with binary trees, and
we want to take an average over all the values in the tree. Here's a basic
implementation:

<a id="code-average"></a>
```ocaml
module Tree = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a t * 'a t
end

let average (tree : float Tree.t) =
  let rec total tree =
    match tree with
    | Tree.Leaf x -> ~total:x, ~count:1
    | Tree.Node (l, r) ->
      let ~total:total_l, ~count:count_l = total l in
      let ~total:total_r, ~count:count_r = total r in
      ~total:(total_l +. total_r), ~count:(count_l + count_r)
  in
  let ~total, ~count = total tree in
  total /. (count |> Float.of_int)
;;
```

(Note that we've made use of the new [labeled tuples] feature for the values
returned by `total`.)

[labeled tuples]: https://tyconmismatch.com/papers/ml2024_labeled_tuples.pdf

We can use `fork_join2` to parallelize `average`:

<a id="listing-average_par"></a>
```ocaml
let average_par (par : Parallel.t) tree =
  let rec total par tree =
    match tree with
    | Tree.Leaf x -> ~total:x, ~count:1
    | Tree.Node (l, r) ->
      let (~total:total_l, ~count:count_l), (~total:total_r, ~count:count_r) =
        Parallel.fork_join2
          par
          (fun par -> total par l)
          (fun par -> total par r)
      in
      ~total:(total_l +. total_r), ~count:(count_l + count_r)
  in
  let ~total, ~count = total par tree in
  total /. (count |> Float.of_int)
;;
```

Note that we don't have to worry about unbalanced trees: the work-stealing
algorithm dynamically adapts whenever tasks are unevenly distributed among cores.

We're not limited to working with simple `float`s, of course. Suppose we add a
module `Thing` earlier in the file: **[Would be nice to think of something
better than `Thing` here.]**

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
end
```

All we have to do to sum over the prices in a `Thing.t Tree.t` is change the
`Tree.Leaf` case:

```diff
  let rec total par tree =
    match tree with
-   | Tree.Leaf x -> ~total:x, ~count:1
+   | Tree.Leaf x -> ~total:(Thing.price x), ~count:1
    | Tree.Node (l, r) ->
      let (~total:total_l, ~count:count_l), (~total:total_r, ~count:count_r) =
```

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
```

But now we get an error from the compiler:

```
The value Thing.price is nonportable, so cannot be used inside a function that
is portable.
```

Why are things breaking just because we moved some code? As it turns out, it's
because our declaration for `price` left out crucial information that the
compiler was previously able to infer. We can take the compiler's suggestion
easily enough by explaining that `price` is `portable`:

```ocaml
val price : t -> float @@ portable
```

But the compiler isn't satisfied. Now it complains about the `l` in
`total par l` (which is still as it was [before]):

[before]: #listing-average_par

```
This value is contended but expected to be uncontended.
```

Now the solution is much less obvious, but as we'll see, it again comes down to
missing information on `price`:

```ocaml
val price : t @ contended -> float @@ portable
```

Once you're familiar with the `portable` and `contended` modes, you'll see that
this can be read as saying:

> The `price` function is safe to call from any domain, and it won't produce a
> data race even if its argument is mutated in parallel.

That's enough to let `average_par_things` go through: from there, the compiler
infers the safety properties that `Parallel.fork_join2` demands.

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
what can be done with it. If you've seen `@ local` **(link to doc)**, you've
already encountered the `local` mode.

The `portable` mode is the one you'll see most often, but it will be easier to
understand once we've covered `contended` and `uncontended`, so we begin there.

### The `contended` and `uncontended` modes

A data race requires two things:

- Two domains can access the same value.
- One of those domains can modify that value, and the other can either observe
  that modification or attempt its own.

The `contended` mode and its opposite, `uncontended`, prevent data races by
ensuring that this is impossible. To do so, they institute two key rules:

> <a id="rule-contended-parallel"></a>
> **Rule 1.** If multiple accesses of the same value may occur in parallel, at
> most one of them may consider the value `uncontended`. The others must
> consider it `contended`.

> <a id="rule-contended-mutable"></a>
> **Rule 2.** Reading or modifying a `mutable` field is not allowed if the
> record is `contended`. The same goes for any element of a `contended` array
> (unless it's an `iarray`, since those are immutable).

Taken together, these two rules guarantee data-race freedom. However, rule 1
requires quite a bit of machinery to enforce, including other modes such as
`portable` as well as the annotations on the types of parallelism APIs like
`fork_join`.

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
let price (t @ contended) = t.price (* ok: [price] is immutable *)
let cheer_up (t @ contended) = t.mood <- Happy (* error! *)
```

```
Error: This value is contended but expected to be uncontended.
```

This is of course [rule 2](#rule-contended-mutable). To see why this has to be
an error, consider that since `t` is `contended`, there may be another domain
trying to mutate it in parallel:

```ocaml
let bum_out t = t.mood <- Sad (* cue ominous music: a data race lurks! *)
```

So we can't allow the access in `cheer_up` unless we flag the argument as
`uncontended`:

```ocaml
let cheer_up (t @ uncontended) = t.mood <- Happy (* ok: [t] is [uncontended] *)
```

Note that you can also let the compiler infer that `t` is `uncontended` (as we
did for `bum_out`). Much like with types, adding explicit modes is often useful
either to make your intentions clear to other humans or nail down the ultimate
cause of a compiler error.

Note that rule 2 forbids even _reading_ the mutable state:

```ocaml
let mood (t @ contended) = t.mood (* error! *)
```

```
Error: This value is contended but expected to be shared or uncontended.
```

This is dangerous for the same reason `cheer_up` was: someone else could be
running `bum_out` in parallel, producing a data race.[^shared]

[^shared]: You may have noticed the `shared` in the error message here. The
`shared` mode lies in between `contended` and `uncontended` in that it allows
reading but not writing mutable fields. It's less common than the others but it
comes in handy for things like read/write locks.

Adding `uncontended` signals to the compiler that a data race is possible: if
`cheer_up` and `bum_out` can be called with the same argument in parallel, we
have a race. Enter [rule 1](#rule-contended-parallel), which simply says _it is
not permissible_ to do so. Unlike rule 2, there's no one place where rule 1 is
enforced—really, it's an invariant of the whole system (language, runtime, and
core libraries)—but let's see one example of trying to break it:

```ocaml
let beat_the_system par =
  let t = { price = 42.0; mood = Neutral }
  let (), () =
    Parallel.fork_join2 par (fun _par -> cheer_up t) (fun _par -> bum_out t)
  in
  ()
```

```
Error: This value is contended but expected to be uncontended.
```

When we cover the `portable` mode [next], we'll be able to explain precisely
what's going on here, but for the moment suffice it to say that the type of
`fork_join2` forces the two tasks to obey rule 1.

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
required to. (You may be aware that `local` is the same way.) On the other hand,
`uncontended` (like `global`) is a constraint that _must_ be met.

Finally, recall that our running example wants to fork/join over an entire tree
of `Thing.t`s, so we should consider what happens when the `Thing.t` is in a
bigger data structure.

> <a id="rule-contended-deep"></a>
> **Rule 4.** Any component of a `contended` value is `contended`.

In particular, every field of a `contended` record, every element of a
`contended` tuple or array **[note: make this `array` if we make the
corresponding change elsewhere]** (as before, `iarray`s are exempt), and every
argument
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
  let t @ uncontended = t_in_a_trenchcoat.inner_t (* error: rule 4 *) in
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

### The `portable` and `nonportable` modes

As we said before, [rule 1](#rule-contended-parallel) and [rule
2](#rule-contended-mutable) of `contended` give us data-race freedom—assuming we
can enforce them, that is. The compiler's type checker can just raise an error
when rule 2 is violated, but rule 1 is a bit squishier. To reiterate:

> **Rule 1 of `contended`.** If multiple accesses of the same value may occur
> in parallel, at most one of them may consider the value `uncontended`. The
> others must consider it `contended`.

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
`Parallel.fork_join2`, what error does the compiler raise and why?

Just as it's safe to forget a value's privileged `uncontended` status and
downgrade it to `contended`, there's no danger in treating something `portable`
as if it's `nonportable`:

> **Rule 3.** A `portable` value may be treated as `nonportable`.

And also for similar reasons as before, we need `portable` to be deep the way
`contended` is:

<a id="rule-portable-deep"></a>
> **Rule 4.** Every component of a `portable` value must be `portable`.

You may have been wondering why rule 2 talks about accessing values rather than
calling functions, even though `portable` really only cares about
functions[^other-code-types], and this is the main reason: the record could be a
function [in a trenchcoat]. (The other, more subtle, reason is that a value of
type `'a` could still end up being a function.) So we have to forbid _all
accesses_ of `nonportable` values from other domains, not just function calls.
The good news is that many types (in fact, most types) obviously _can't_ cause
a problem—you'll never make an `(int * string option) list` that contains a
function, much less one that will cause a data race—and if the compiler is
aware that a type `t` can't have a function in it, it can ignore portability
requirements altogether for values of `t`. We'll cover the specifics when we get
to [mode crossing].

[^other-code-types]: Note that some other types are secretly function types,
notably including `Lazy.t`, so `portable` is also a concern for them.

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

So for example, we can take this .mli:

```ocaml
type t

val create : unit -> t @@ portable
val combine : t -> t -> t @@ portable
val do_something_involving_shared_state : t -> unit
```

And change the default to `portable`:

```ocaml
@@ portable

type t

val create : unit -> t
val combine : t -> t -> t
val do_something_involving_shared_state : t -> unit @@ nonportable
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
in return types with those kinds get to cross certain modes. In summary (this
table isn't nearly exhaustive—in particular, there are many modes we haven't
covered, and `immutable_data` and `mutable_data` cross some of them as well):

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
let always_portable : int_or_string @ nonportable -> int_or_string @ portable =
 fun a ->
  let a' @ portable = a in (* ok because [int_or_string] crosses portability *)
  a'
```

(We have to write this slightly awkwardly to specify the returned mode of a
function.)

In fact, we've already seen mode crossing in action. Let's look at the full type
of `Parallel.fork_join2` as `parallel_kernel.mli` writes it:

```ocaml
val fork_join2
  :  t @ local
  -> (t @ local -> 'a @ contended portable) @ portable
  -> (t @ local -> 'b @ contended portable) @ portable
  -> 'a * 'b @ contended portable
```

We've wrestled before with the fact that both functions have to be `portable`,
but they also have to _return_ values that are both `contended` and `portable`!
(Exercise: What goes wrong if they're allowed to return something `uncontended`?
How about `nonportable`?) Why didn't we worry about this before? Well, let's
look again at the code we were writing:

```ocaml
  let average_things_par (par : Parallel.t) tree =
    let rec total par tree =
      match tree with
      | Tree.Leaf x -> ~total:(Thing.price x), ~count:1
      | Tree.Node (l, r) ->
        let (~total:total_l, ~count:count_l), (~total:total_r, ~count:count_r) =
          Parallel.fork_join2
            par
            (fun par -> total par l)
            (fun par -> total par r)
        in
        ~total:(total_l +. total_r), ~count:(count_l + count_r)
    in
    let ~total, ~count = total par tree in
    total /. (count |> Float.of_int)
  ;;
```

The type of `total` is this:

```ocaml
Parallel.t @ local -> Thing.t Tree.t @ contended -> total:float * count:int
```

Crucially, the returned type—which becomes the type returned by the arguments to
`fork_join2`—is `total:float * count:int`. The basic `float` and `int` types are
`immutable_data`, and any pair of `immutable_data` types is also
`immutable_data`, so this type is `immutable_data` as well (in fact, if not for
the labels it would be the same type as our `float_and_int`). So even though we
wrote our code with no concern for whether the returned `total` and `count`
could be accessed in parallel, thanks to the types involved, we didn't have to
worry.

Any type variable can be given a kind, so we can write a version of
`always_portable` that works for _any_ `mutable_data` type:

```ocaml
let always_portable' : ('a : mutable_data) @ nonportable -> 'a @ portable =
 fun a ->
  let a' @ portable = a in
  a'
```

The type of `always_portable'` can be read “Given any type `'a` of kind
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

  let create ~price ~mood = { price; mood = Atomic.make mood }
  let price { price; _ } = price
  let mood { mood; _ } = Atomic.get mood
  let cheer_up { mood; _ } = Atomic.set mood Happy
  let bum_out { mood; _ } = Atomic.set mood Sad
end
```

As you can see, there's a bit of syntactic overhead, but in return we get to
access `mood` even if a `Thing.t` is `contended`, and in fact we can mark `t` as
`immutable_data` so that it ignores `contended` and `uncontended` altogether
(that is, it [crosses contention]). Do go over the documentation for the `Atomic`
module **(XX link to documentation)**, as it has many useful operations from
`compare_exchange` to atomic logical bitwise XOR.

[crosses contention]: #mode-crossing

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
      Atomic.update total ~pure_f:(fun total -> total +. Thing.price x);
      Atomic.incr count
    | Tree.Node (l, r) ->
      let (), () =
        Parallel.fork_join2 par (fun par -> go par l) (fun par -> go par r)
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
stopped,_ not what actually stops us.)

Now for the caveats: Firstly, there are performance penalties, since an
`Atomic.t` is a pointer and atomic operations are more expensive. Secondly,
`Atomic.t` frees us from concerns about data races, but it absolutely does not
prevent race _conditions_. There are few guarantees about the order in which the
updates to `total` and `count` occur: we know only that they all happen before
the outermost `fork_join2` returns.

Of course, in this case what saves us is that it doesn't matter what order the
updates happen in: addition is commutative. However, now that we've made `mood`
atomic as well, suppose we wanted to take the average of all the nodes whose
`mood` is `Happy`? Nothing in the system stops us now: if someone is calling
`bum_out` on the entire tree in parallel, then our average will reflect an
unpredictable number of those changes. The only way to stop _that_ is to use
something more sophisticated like a lock over the whole tree, which grants a
function `uncontended` access while the lock is held. That's what the capsule
API is for, but it's beyond the scope of this tutorial.

The good news is that data-race freedom guarantees that even buggy programs can
be reasoned about intuitively. See [Why are data races bad?].

[Why are data races bad?]: #why-are-data-races-bad

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

We first need to convert from `iarray` to `Parallel.Sequence.t`, a general
sequence type supporting a rich selection of parallel operations. Among them is
a parallel `reduce`, which operates much like an unordered `fold`.

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
        ~f:(fun (~total:total_acc, ~count:count_acc) (~total, ~count) ->
          ~total:(total +. total_acc), ~count:(count + count_acc))
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
      Parallel.Sequence.fold'
        par
        seq
        ~f:(fun par subtree -> total par subtree)
        ~init:(~total:0.0, ~count:0)
        ~combine:(fun _par (~total, ~count) (~total:total2, ~count:count2) ->
          ~total:(total +. total2), ~count:(count + count2))
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
