# Parallelism Tutorial

# Introduction

# Here’s a program we’re sad isn’t parallel

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

# But what if fork/join?

We can achieve fork/join parallelism using the `parallel` library’s `fork_join*`
functions. For example, a very expensive way to add four integers would be this:

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
which implements the popular [work-stealing] strategy. You can also use the
`parallel` library's own `Parallel.Scheduler.Sequential`, which simply runs
everything on the primary domain. This is handy if you want to test parallel
code without switching to a multicore-enabled runtime.

[work-stealing]: https://en.wikipedia.org/wiki/Work_stealing

```ocaml
let test_add4 par = add4 par 1 10 100 1000

let%expect_test "add4 in parallel" =
  let scheduler = Parallel_scheduler_work_stealing.create () in
  let monitor = Parallel.Monitor.create_root () in
  let result =
    Parallel_scheduler_work_stealing.schedule scheduler ~monitor ~f:test_add4
  in
  Parallel_scheduler_work_stealing.stop scheduler;
  print_s [%message (result : int)];
  [%expect {| (result 1111) |}];
;;
```

This creates a work-stealing scheduler, along with a _monitor_ to manage
exceptions. Then it tells the scheduler to run the `test_add4` function before
shutting down the scheduler. (Naturally, a real program will want to keep the
monitor and scheduler around longer!) To test using the sequential scheduler
instead, we would simply replace `Parallel_scheduler_work_stealing` with
`Parallel.Scheduler.Sequential`.

We can use `fork_join2` to parallelize `average`:

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
submodule `Thing` earlier in the file: **[Would be nice to think of something
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

```ocaml
  let average_things_par (par : Parallel.t) tree =
    let rec total par tree =
      match tree with
      | Tree.Leaf x -> ~total:(Thing.price x), ~count:1 (* <== new code *)
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

So far, so good. But something annoying happens if we introduce an abstraction
barrier. Lets move `Thing` into its own module. `thing.mli` is very simple:

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
`total par l`:

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

<!--

What's going on here? Well, the arguments to `Parallel.fork_join2` must be
`portable`, which is to say, safe to call from any domain. We'll get into
precisely what this means [later], but an important aspect is that a `portable`
function can only call other `portable` functions.[^param-loophole] So for `(fun
par -> total par l)` to be `portable`, `total` must be `portable`, and thus
since `total` calls `Thing.price`, that has to be `portable` as well. When we
defined `price` in the same module and let the compiler infer its type, the
`portable` got inferred with it, but now that we're writing the type out
ourselves we need to include it:

```ocaml
val price : t -> float @@ portable
```

[later]: #the-portable-and-nonportable-modes

[^param-loophole]: This is a slight exaggeration. See [rule 2] of the `portable`
mode for the precise definition.

[rule 2]: #rule-portable-access

But the compiler isn't satisfied. Now it complains about the `l` in
`total par l`:

```
This value is contended but expected to be uncontended.
```

This is telling us that the compiler suspects a data race: it believes that
someone may be able to modify `l` in parallel with this code, and that
`total` may race if that happens. Hence there are two possible solutions:

1. Convince the compiler that `l` can't be modified in parallel.
2. Convince the compiler that `total` doesn't race even if `l` is modified in
   parallel.

In the language of our error message, option 1 means making the value (namely
`l`) `uncontended` and option 2 means changing the expectation so that `l`
can remain `contended`.

In this case it turns out we can go for option 2: even if the value `l` is
modified in parallel, we're safe anyway. We'll make this precise when we meet
[the contended and uncontended modes], but a key insight is that nothing here
can actually produce a data race. All we're doing is walking over an immutable
data structure and reading an immutable field. Someone else might go changing
all the moods, but we won't notice. You wouldn't know that by looking at
`thing.mli`, though: `price` could be a mutable field like `mood`, and then
calling `price` could risk a data race. So we need to give `price` a type that
expresses that it doesn't produce data races:

```ocaml
val price : t @ contended -> float @@ portable
```

[the contended and uncontended modes]: #the-contended-and-uncontended-modes

(Note the different roles of `@` and `@@` here: the `@` puts a mode on a
           bothered to give a particularly good error or handle the Not_found
           case from env.
function's argument or return type, and the `@@` puts a mode on the entire
`val` declaration.)

This says that `price` is safe to call even if its argument might be getting
messed with at this very moment. Notably, you can't do the same with `mood`:

```ocaml
val mood : t @ contended -> Mood.t @@ portable
```

```
Values do not match:
  val mood : t -> Mood.t @@ portable
is not included in
  val mood : t @ contended -> Mood.t @@ portable
The type t -> Mood.t is not compatible with the type
  t @ contended -> Mood.t
```

Now that we've modified `price`, that's enough to convince the compiler that
`average_par` is safe: if the `x` in `Thing.price x` is allowed to be
`contended`, that lets the parameter of `total` be `contended` (this will be
[rule 3] of the `contended` mode), and in turn our long-suffering `l` is
allowed to be `contended`, fixing the error.

[rule 3]: #rule-contended-deep

Now that we've seen that mutable fields make `contended` go wrong, you might
wonder what makes a function not `portable` (besides other functions not being
`portable`). One surefire way is to access any kind of global state:

```ocaml
let annoying_bit_of_global_state = ref 0

let price { price; _ } =
  incr annoying_bit_of_global_state;
  price
;;
```

This innocuous-looking change is enough to ruin our claim that `price` is
`portable`:

```
Values do not match:
  val price : t -> float
is not included in
  val price : t -> float @@ portable
The second is portable and the first is nonportable.
```

This is, of course, entirely correct given what we said at the outset: a
`portable` function is one that's safe to call from any domain, but calling
this version of `price` twice in parallel clearly produces a data race. As
we'll soon see, what's going on is that for `price` to be `portable`, it has to
see `annoying_bit_of_global_state` as `contended`, which disallows accessing it
at all (since a `ref` is just a record with a single mutable field).
-->

## A brief primer on `portable` and `contended`

Firstly, `portable` and `contended` are _modes_. Like a type, a mode describes
something about a name in an OCaml program. But whereas a type describes the
*value* associated with a name, a mode instead describes the value's
*circumstances.* This could be where it is in memory, who has access to it, or
what can be done with it. If you've seen `@ local` **(link to doc)** or the
older syntax `local_`, you've already encountered the `local` mode.

<!-- you may know that a `local` value is
limited in how long it can be accessed (typically not after the current function
returns). It happens that `local` is a mode, and s `portable` mode has a
similar gatekeeping task:

> Only a `portable` value may pass from one domain to another.
-->

<!--
The `portable` and `contended` modes can broadly be described like this:

> Only a `portable` value is allowed to be accessed from other domains.

> A `contended` value may currently be accessible from some other domain.

These may sound similar but there are crucial differences. Portability is about
*what is allowed:* something that's `portable` _can_ be passed from one domain to
another. In contrast, contention is about *what has happened:* something that's
`contended` _may have been_ passed to another domain that could be accessing it.
So, for instance, you can imagine

**\[Probably want examples here.\]**

We can already work out a few things that should be true of the `contended` mode.
For one thing, if another domain can access a record, then surely it can access
any of its fields. It should be the case then that if a record is `contended`
then so are its fields, and it is indeed so. The same goes for the arguments in a
`contended` variant, the elements of a `contended` array, and so forth. We say
the `contended` mode is _deep_. In fact, all modes are deep in this way, though
they all come with different “escape hatches.” **\[That makes it sound dirty like
`Obj.magic` when all I'm trying to get at here is things like `ATomic.t` for
`contended` and modalities for modes where they're meaningful.\]**

Because another domain might see it, a `contended` value is subject to data
races: I might read some field while you're writing to it, or you might read that
field while I'm writing to it, or we both might try to write to the same field at
once. We're safe to read _immutable_ fields, but reading _or_ writing any
`mutable` part is dangerous **\[elaborate or
point to earlier example\]**. This motivates the central invariant of data-race
freedom:

> **Golden rule.** A `mutable` field of a `contended` record must not be read or
> written to.

**\[This next graf feels unnecessary here. Maybe cover mutable data later, when
we discuss capsules and locks?\]**

The *keyword* `mutable` is important here. Certain types like `Atomic.t` provide
mutability without risking data races, and while an immutable field of type `int
Atomic.t` **\[point to example?\]** is a “mutable field” in a sense, it's exempt
from the golden rule. **\[This next bit should be split out into an example:\]**
An `int ref`, on the other hand, does *not* help, since it’s actually a record
with a `mutable` field. As we've mentioned, modes are deep, so if a record with
an immutable `int ref` field is `contended`, you can access the `int ref` itself
but you can neither read it nor write to it.

Now that we've covered `contended`, we can specify the key property of the
`portable` mode:

> A `portable` function accesses all values from outside its definition at
> `contended` mode.

By “at `contended` mode” we mean something a bit subtle here.

-->

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
`portable` as well as the types of parallelism APIs like `fork_join`.

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

> **Rule 3.** An `uncontended` value may be used as though it is `contended`.

This is entirely consistent with rules 1 and 2: having an `uncontended` value
gives strictly more power than having a `contended` one, so it's always safe to
forget we have that power. The upshot is that if a function takes a `contended`
parameter then the caller is _allowed_ to pass a `contended` value but it is not
required to. (You may be aware that `local` is the same way.) On the other hand,
`uncontended` (like `global`) is a constraint that _must_ be met.

<!--
Finally, recall that our running example wants to fork/join over an entire tree
of `Thing.t`s, so we should consider what happens when the `Thing.t` is in a
bigger data structure. We clearly need to be weary:

```ocaml
type t_in_a_trenchcoat = {
  inner_t : t;
}

let cheer_up_sneakily (t_in_a_trenchcoat @ contended) =
  let t = t_in_a_trenchcoat.inner_t (* ok: can access immutable field ... *) in
  cheer_up t (* cue the ominous music again? *)
```


[Commenting out. This isn't a textbook. Let's not bother teaching the reader how
they could have invented rule 4.

Nothing we've seen so far explicitly tells us that `t_in_a_trenchcoad.inner_t`
is `contended`, so one might worry that we've found a way to smuggle a data race
by the compiler. After all, we can also write `bum_out_sneakily`, and what stops
us from calling them both in parallel on the same `t_in_a_trenchcoat` (and hence
the same `inner_t`)? Fortunately, the compiler is not so easily fooled:

```
Error: This value is contended but expected to be uncontended.
```

We can work out exactly what rule we need by considering rule 1. To reiterate:

> **Rule 1.** If multiple accesses of the same value may occur in parallel, at
> most one of them may consider the value `uncontended`. The others must
> consider it `contended`.

We can't possibly let `t_in_a_trenchcoat.inner_t` be `uncontended` when
`t_in_a_trenchcoat` is `contended`, since then _everyone_ 
-->

Finally, recall that our running example wants to fork/join over an entire tree
of `Thing.t`s, so we should consider what happens when the `Thing.t` is in a
bigger data structure. For this we have two closely-related rules. First:

> <a id="rule-contended-deep"></a>
> **Rule 4a.** On access, any component of a `contended` value is `contended`.

In particular, every field of a `contended` record, every element of a
`contended` tuple or array (as before, `iarray`s are exempt), and every argument
of every constructor of a `contended` variant is `contended`. We say that the
`contended` mode is _deep._ It's easy to see what goes wrong if we let anyone
treat, say, a field as `uncontended` when its record is `contended`:

<a id="code-cheer_up_sneakily"></a>
```ocaml
type t_in_a_trenchcoat = {
  inner_t : t;
}

let cheer_up_sneakily (t_in_a_trenchcoat @ contended) =
  let t @ uncontended = t_in_a_trenchcoat.inner_t (* error: rule 4a *) in
  cheer_up t (* cue the ominous music again *)
```

If not for rule 4a, we could also write `bum_out_sneakily` and then call
`cheer_up_sneakily` and `bum_out_sneakily` on the same argument in parallel,
causing the same data race we've been trying to avoid all along.

In fact, rule 4a is pretty well required to make [rule 1] work at all. The
interested reader is invited to work out why the text of rule 1 demands that,
for instance, we say that `t_in_a_trenchcoat.inner_t` is `contended` when
`t_in_a_trenchcoat` is `contended`.

[rule 1]: #rule-contended-parallel

Finally, rule 4a told us about accessing data but we also need a rule about
constructing it:

**[On reflection, I think I'd rather get rid of rule 4b and just describe how
`uncontended` and modalities work. It isn't really a Rule the way the others
are.]**

> **Rule 4b.** On construction, every component of a `uncontended` value must be
> `uncontended`, unless the type declares the component to be `@@ contended`.

<!--
If rule 4a makes `contended` deep, rule 4b makes `uncontended` “deep by default.”
We saw in [`cheer_up_sneakily`] that it's _always_ dangerous for a `contended`
record to contain an `uncontended` field. Going the other direction is fine,
though. Consider:

[`cheer_up_sneakily`]: #code-cheer_up_sneakily

```ocaml
module State = struct
  type t = {
    mutable most_expensive_thing : Thing.t
    mutable my_favorite_thing : Thing.t
  }
end
```

If I have an `uncontended` `State.t`, that means no one else is trying to access
it in parallel, so it's safe for me to read or write from either field. If I had
a `contended` `Thing.t`, could I set `most_expensive_thing` to it? Morally, this
seems fine: it means that it's safe for me to read and write the fields of the
state but
-->

If rule 4a makes `contended` deep, rule 4b makes `uncontended` “deep by default.”
We saw in [`cheer_up_sneakily`] that it's _always_ dangerous for a `contended`
record to contain an `uncontended` field. Going the other direction is fine,
though, as that just means we know we have exclusive access to the record, say,
but someone else might be messing with something _inside_ one of the fields. We
express this possibility like so:

[`cheer_up_sneakily`]: #code-cheer_up_sneakily

```ocaml
module State = struct
  type t = {
    mutable most_expensive_thing : Thing.t @@ contended
    mutable my_favorite_thing : Thing.t
  }
end
```

The `@@ contended` is called a _modality_ and it lets the field have a different
mode than the record containing it. (You may already know the `@@ global`
modality.) Effectively, `most_expensive_thing` is _always_ `contended`, whether
or not the containing `state` record is, but `my_favorite_thing` has the same
mode as the whole `state` as usual.

This means we can _never_ access the `mood` of `most_expensive_thing`:

```ocaml
let most_expensive_mood (state @ uncontended) =
  state.most_expensive_thing.mood
```

```
Error: This value is contended but expected to be uncontended.
```

In return, we can set `most_expensive_thing` to something `contended`:

```ocaml
let set_most_expensive_thing (state @ uncontended) (thing @ contended) =
  state.most_expensive_thing <- thing
```

We can't do that with `my_favorite_thing`, since `uncontended` is deep by
default:

```ocaml
let set_my_favorite_thing (state @ uncontended) (thing @ contended) =
  state.my_favorite_thing <- thing
```

```
Error: This value is contended but expected to be uncontended.
```

Note that there is actually an `@@ uncontended` modality but it's not useful—if
the record is `contended`, the field has to be `contended` no matter what (see
[rule 4a]), so the modality doesn't change anything there. Fortunately, by rule
3, you can _always_ put an `uncontended` value in a `contended` record, so the
modality isn't even necessary there.

[rule 4a]: #rule-contended-deep

<!--
Note that there is also an `@@ uncontended` modality but it doesn't actually
change anything: you might hope that it would let you access a field of a
`contended` record as `uncontended`, but rule 4a forbids that outright; and you
might hope that it would let you construct a `contended` record from an
`uncontended` field value, but rule 3 already lets you do that by simply
treating the value as `contended`.
-->


### The `portable` and `nonportable` modes

As we said before, [rule 1] and [rule 2] of `contended` give us data-race
freedom—assuming we can enforce them, that is. The compiler's type checker can
just raise an error when rule 2 is violated, but rule 1 is a bit squishier. To
reiterate:

> **Rule 1 of `contended`.** If multiple accesses of the same value may occur
> in parallel, at most one of them may consider the value `uncontended`. The
> others must consider it `contended`.

[rule 1]: #rule-contended-parallel
[rule 2]: #rule-contended-mutable

<!-- [This is making it hard rather than easy]

We've seen that `Parallel.fork_join2` provides parallelism while enforcing this
rule. How does it manage that? Let's look again at our attempt to sneak a data
race by it, adding a few things for illustration:[^data-races-are-really-bad]

```ocaml
let beat_the_system par =
  let t @ uncontended = { price = 42.0; mood = Neutral }
  cheer_up t; (* line A *)
  let (), () =
    Parallel.fork_join2 par
      (fun _par -> cheer_up t) (* line B *)
      (fun _par -> bum_out t)
  in
  cheer_up t (* line C *)
```

[^data-races-are-really-bad]: You may think that the additional calls to
`cheer_up` make the data race benign: surely in the end `mood t` will always be
`Happy`? Unfortunately, data races aren't so well-behaved: it's entirely
possible for the writes to get reordered so that `Sad` is the final value.

The call to `cheer_up` on line A is clearly fine: we've just created `t` so it's
obviously uncontended. The call on line C is 

**[Exercise idea: change `fork_join2` to `fork_but_don't_join2`. Should line C
now be an error? (In the sense of causing a rule 1 violation?) No, because there
are other domains but they all consider `t` uncontended.]**

Where exactly does this code do something wrong? At first, `t` is surely
uncontended (that is, its `uncontended` designation is accurate): it's just been
created, so no one else can be accessing it in parallel. We could add a call to
`cheer_up` there if we wanted. But the call to `cheer_up t` in the argument to
`fork_join2` is a problem because _that_ code _may[^or-same-domain] run in
another domain._
-->

**[I can't tell whether this is belaboring the point a bit. The arguments to
`fork_join2` shouldn't assume things are `uncontended`. Do I need to spell
things out this much?]**

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

<!--

As with [`contended`], we can identify two elements of the crime:

[`contended`]: #the-contended-and-uncontended-modes

1. Some code may be run in any domain.
2. That code assumes that something is `uncontended`.

And as before, we can synthesize these into two rules:
-->

As you might have guessed, this is exactly what the `portable` mode is for. Both
arguments to `fork_join2` are required to be `portable`, and we have two rules:

> **Rule 1.** Only a `portable` value is safe to access from outside the domain
> that created it.

> <a id="rule-portable-access"></a>
> **Rule 2.** If a `portable` function refers to a value outside of its own
> definition, then (a) that value must be `portable`, and (b) the value
> is treated as `contended`.


**[Seems worth observing somewhere that `portable` and `contended` codify the
maxim that “thread-safe code doesn't have uncontrolled access to shared
state,” but I'm not sure where that goes. Maybe it's more cute than helpful.]**

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
field is `mutable` field, so `!a` requires `uncontended` as always.) On the
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

```
val loop : int -> unit @@ nonportable
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

> **Rule 4a.** Every component of a `portable` value must be `portable`.

You may have been wondering why rule 2 talks about accessing values rather than
calling functions, even though `portable` really only cares about
functions[^other-code-types], and this is the main reason: the record could be a
function [in a trenchcoat]. (The other, more subtle, reason is that a value of
type `'a` could still end up being a function.) So we have to forbid _all
accesses_ of `nonportable` values from other domains, not just function calls.
The good news is that many types (in fact, most types) obviously _can't_ cause
a problem—you'll never make an `(int * string option) list` that contains a
function, much less one that will cause a data race—and if the compiler is
aware, it can ignore portability requirements altogether for values of a given
type. We'll cover the specifics when we get to [mode crossing].

[^other-code-types]: Note that some other types are secretly function types,
especially `Lazy.t`, so `portable` is also a concern for them.

[in a trenchcoat]: #code-cheer_up_sneakily
[mode crossing]: #mode-crossing

Lastly, `nonportable` is “deep by default” the way `uncontended` is, with
a `@@ portable` modality:

**[Oh right, I still need to remove the access/construction distinction from
the `contended` discussion, as well as rule 4b. Also this 4b should go.
Actually this 4b is ridiculous.]**

> **Rule 4b.** On construction, every component of a `nonportable` value must be
> `portable` if the type declares the component to be `@@ portable`.

<!--
Even though `portable` is concerned with functions, not data, both of these
rules refer to accessing values in general. There are two reasons for this:
datatypes like records and arrays can contain functions (that is, you
could hide a nonportable function [in a trenchcoat]); and an unknown type
like `'a` could turn out to be a function type. Both of these concerns push us
to forbid _all accesses_, not just function calls. However, in most cases
`portable` is only relevant for functions (and other code types like `Lazy.t`).
In fact, most datatypes are simply always `portable`, and the compiler will
ignore portability requirements for any such type it's aware of, as we'll see
when we cover [mode crossing].

[in a trenchcoat]: #code-cheer_up_sneakily
[mode crossing]: #mode-crossing
-->


<!-- ugh, let's try writing `uncontended` first ...
### The `contended` mode

The basic property of the `contended` mode is this:

> <a id="contended-basic-property"></a>
> A `contended` value may be accessible from another domain, and that domain may
> mutate it.

Because another domain might mutate it, a `contended` value is subject to data
races: I might read some field while you're writing to it, or you might read that
field while I'm writing to it, or we both might try to write to the same field at
once. We're safe to read _immutable_ fields, but reading _or_ writing any
`mutable` part is dangerous **\[elaborate or
point to earlier example\]**. This motivates the central invariant of data-race
freedom:

**\[Dunno that I'm totally sold on _Golden Rule_ as the name, but I do want
something a bit grandiose, since this really is the invariant that the whole
system revolves around.\]**

> <a id="golden-rule"></a>
> **Golden Rule.** A `mutable` field of a `contended` record must not be read or
> written to.

(Arrays have a similar rule: the elements of a `contended` array are off
limits, unless it's an `iarray` since those are immutable.)

So, suppose we have  **\[find less generic names\]**:

```ocaml
type t = {
  a : int;
  mutable b : int;
}
```

Then `a` is always available but, for `contended` values, `b` is not:

```ocaml
let f (t @ contended) =
  printf "%d\n" t.a; (* ok: [a] is immutable *)
  t.b <- t.b + 1; (* error: [b] is mutable *)
```

```
Error: This value is contended but expected to be uncontended.
```

As the error suggests, the opposite of `contended` is `uncontended`, which is
also a mode:

```ocaml
let f (t @ uncontended) =
  printf "%d\n" t.a; (* ok: [a] is immutable *)
  t.b <- t.b + 1; (* ok: [t] is uncontended *)
```

There is a third mode, `shared`, that allows reading mutable fields but not
writing to them. It's not needed as often[^uses-of-shared], so we won't use
it for this tutorial, but you may see it in error messages:

```ocaml
let f (t @ contended) =
  printf "%d\n" t.a; (* ok: [a] is immutable *)
  printf "%d\n" t.b; (* error: [b] is mutable *)
```

```
Error: This value is contended but expected to be shared.
```

Anything `uncontended` is automatically `shared`, so you can fix this error by
making the value `uncontended`.

[^uses-of-shared]: One place where `shared` is needed is when using read/write
locks: the write lock provides `uncontended` access but the read lock only gives
`shared`.

<!--
We need a few more rules to make sure that anything that would cause a data race
is `contended`. Firstly, while it's safe to access an immutable field of a
`contended` record, that field's value needs to be considered `contended` as
well. After all, some other domain might be accessing the same field, so we
can't allow either of them to think the field's value is `uncontended`.
- ->

In exchange for the restrictions on `contended` values, we get the privilege of
accessing a value from as many domains as we want, in parallel, so long as all
but one of them consider it `contended`. We've already seen some of the argument
for why this is safe: the [Golden Rule](#golden-rule) says that only a domain
that sees the value as `uncontended` can access the mutable field, and the basic
property of `contended` amounts to seeing the whole 

```ocaml
type t_in_a_trenchcoat = {
  inner_t : t;
}

let set_b (t_in_a_trenchcoat @ contended) b =
  let t = t_in_a_trenchcoat.inner_t (* ok: can access immutable field ... *) in
  t.b <- b (* error: ... but not allowed to treat it as [uncontended] *)
```

```
Error: This value is contended but expected to be uncontended.
```

If `set_b` were allowed, then we could call it with the same `t_in_a_trenchcoat`
in two different domains (after all, it's `contended`, so it should be safe for
both domains to access it) 


<!--
We need a few more rules to make sure that anything that would cause a data
race is `contended`. Firstly, if a record is `contended`, what does that say
about its fields? There may be many domains that can access the record, so long
as all (or all but one) consider it `contended`. We're allowed to read immutable
fields of `contended `
-->
<!-- Recall the basic property of `contended`:

> A `contended` value may be accessible from another domain, and that domain may
> mutate it.

Fundamentally, `foo @ contended` expresses _uncertainty:_ another domain _might_
be allowed to mutate `foo`, so it's unsafe to access any of `foo`'s mutable
fields. If we are uncertain about `foo`, then we must be _at least_ as uncertain
about `foo.bar`
- ->
<!--

It follows that we have this property for `uncontended`:

> An `uncontended` value cannot be mutated by any other domain.

Notice that the `uncontended` mode conveys _certainty:_ no other domain can
mutate the value. Conversely, a `contended` value is one we _don't_ have
guarantees about.

Now
- ->
<!--

We need a few more rules to make sure that anything that would cause a data
race is `contended`. For example, if a record is `contended`, what does that say
about its fields?

```ocaml
type t2 = {
  always_the_same_t : t;
  mutable a_different_t_each_time : t;
}

let g (t2 @ contended) new_b =
  printf "%d\n" t2.always_the_same_t.a; (* ok: immutable is always safe *)
  (* printf "%d\n" t2.a_different_t_each_time.a; (* error: mutable field *) *)
  t2.always_the_same_t.b <- new_b (* ??? *)
```

If `g` were allowed, we'd be able to take a value `t2` and call `g t2 1` in one
domain and `g t2 2` in another domain in parallel. That's a data race, so in
fact `g` is forbidden:

```
This value is contended but expected to be uncontended.
```

To see the rule that we need here, consider again the basic property of the
`contended` mode:

> A `contended` value may be accessible from another domain, and that domain may
> mutate it.

Clearly, if `t2` is accessible from another domain, then so is
`t2.always_the_same_t`. The only solution is to have `t2.always_the_same_t` be
`contended` whenever `t2` is.[^what-about-may-mutate] We do this by making the
`contended` mode _deep:_

<!--
The Golden Rule says that `t2`'s mutable field is inaccessible, but it lets
`t2.always_the_same_t` through. Suppose we have another function:

```ocaml
let g' (t2 @ uncontended) =
  t2.always_the_same_t.b <- 99 (* cue the ominous music *)
```

If we call `g` and `g'` with the same `t2` on two different domains running in
parallel, we have a data race. Therefore either `g` or `g'` has an error. Recall
the basic property of the `contended` mode:

> A `contended` value may be accessible from another domain, and that domain may
> mutate it.

- ->

<!--
If I have `x : t1` and its mode is `contended`, then `x.a` is good but the
Golden Rule says `x.b` is an error. But what if I have `y : t2` with mode
`contended`?
Clearly `y.mutable_t1` is right out, and `y.immutable_t1` is okay on its own,
but what about `y.immutable_t1.b`? Clearly that _shouldn't_ work:

```ocaml
(* Meanwhile, on another domain ... *)
y.immutable_t1.b <- 99 (* cackles of evil intent are heard *)
```
Since `y` can be accessed by another domain, so can all of its fields, and that means
`y.immutable_t1.b` needs to be disallowed just as `y.mutable_t1` is. Fortunately, this
is exactly what happens:

**\[example session\]**

We accomplish this by making `contended` _deep:_
- ->

> <a id="contended-deep"></a>
> Every component of a `contended` value is `contended`.

In particular, every field of a `contended` record, every element of a
`contended` tuple or array[^immutable-arrays], and every argument of every
constructor of a `contended` variant is `contended`.

[^what-about-may-mutate-it]: We've glossed over the “and that domain may mutate
it” part of the basic property, but it doesn't change the outcome. If we let
`t2.always_the_same_t` be `uncontended` even though `t2` is `contended`, that
would let _every_ domain that can access `t2` mutate `t2.always_the_same_t`.

[^immutable-arrays]: For mutable arrays (that is, most arrays), this doesn't
matter much since the elements of a `contended` array can't be accessed to begin
with. However, immutable arrays can be accessed, and then we do care that the
elements are `contended`.

- ->

### The `portable` mode

Now that we've covered `contended`, we turn to `portable`. Its basic property is
this:

> Only a `portable` value is safe to access from outside the domain that created
> it.

<!--
At first blush, this seems similar to `contended`, but there's an important
shift in perspective: where `contended` is about _what has happened,_ `portable`
is about _what may happen in the future_. So a value might easily be `portable`
but not `contended`, say because we're _allowed_ to pass it to another domain
but we haven't done so _yet_. (Can we access the mutable fields of such a value?
Yes!)[^contended-nonportable]

[^contended-nonportable]: It's even allowed, but less exciting, to have a value
that's `contended` yet not `portable`. That just means we don't know very much
about that value: it _may_ have been shared with another domain, so we can't
access its mutable fields, and yet it's not known to be safe to pass between
domains, so we can't do that either.
- ->

This covers similar territory to `contended`, but there are important
differences: if a value isn't `portable` (in which case it has the `nonportable`
mode), then other domains can't access it _at all._ So something can be
`portable` and `uncontended`, meaning that value is _safe_ to share with other
domains, but either no other domain _currently_ has access or every other domain
sees it as `contended`. (If two domains see the same value as `uncontended` then we
have A Problem since the Golden Rule no longer protects us from data races.)

We haven't yet said what _makes_ a value `portable`. In the case of functions, this
is where the `contended` mode comes in:

> A `portable` function accesses all values outside its definition[^closures] at
> `contended` mode.

[^closures]: For the initiated, “all values outside its definition” means
every variable that the function closes over (which does include global variables).

<!- -
[Taking another pass at this: too much of it is making things sound difficult.]

By “at `contended` mode” we mean something a bit subtle here. Back when we discussed
what modes are (see [here](#a-brief-primer-on-portable-and-contended)), we said that
types describe values but modes describe _circumstances_. One consequence of this is
that a variable's mode can depend on how it's accessed: `x` might not be `contended`
when it's declared, but if a `portable` function mentions `x`, it “sees” a
`contended` value. We then say that the function accesses `x` “at `contended`
mode.” Note that this doesn't generally happen with types: if `x` is an `int`
then it's an `int` everywhere, and you can't just access it “at `float` type.”

We can illustrate what's going on by returning from our types from [the previous
section](#the-contended-mode). Here we explicitly give `x` the mode `uncontended`
(unsurprisingly the opposite of `contended`):

**\[I would really love to give `x` its type here but then I have to explain why
`@@` is needed rather than `@` for the type and this seems like not the time. \]**

```ocaml
let f (x @ uncontended) =
  (* Demonstrate that [x] is uncontended by reading and writing its mutable field *)
  x.b <- x.b + 1;
  let (g @ portable) () =
    let x @ uncontended = x (* error! *) in
    x.b <- x.b + 2 (* not very portable of us *)
  in
  g
```

```
This value is contended but expected to be uncontended.
```

If we wrote simply `x : t1` rather than `x @ uncontended`, then it would be surprising if
`let x : t1 = x` didn't work. However, in this case it's essential that it not work: `g`
being portable means it might be accessed by another domain, and that means it might _run_
on another domain. From that domain's perspective, `x` is very much accessible by another
domain, and that domain may mutate it (remember, `x` is `uncontended` there!), so data-race
freedom demands that `x` be considered `contended` so that the [Golden Rule](#golden-rule)
can apply (thus stopping our mischief on the following line).
- ->

It's worth drilling into what “at `contended` mode” means. The mode of a
variable often depends on where it occurs---[as we've said], modes are about
circumstances, so naturally the mode will change if the circumstances change.
For example:

[as we've said]: #a-brief-primer-on-portable-and-contended

```ocaml
let f (x @ uncontended) =
  (* These occurrences of [x] have mode [uncontended] *)
  x.b <- x.b + 1; (* ok: [x] is uncontended *)
  let (g @ portable) () =
    (* These occurrences of [x] have mode [contended] *)
    printf "%d\n" x.a; (* ok: we can access immutable parts *)
    x.b <- x.b + 2 (* error! *)
  in
  g
```

Since `f` defines `x` to be uncontended, its own accesses happen at
`uncontended` mode. When `g` accesses `x`, however, this is a `portable`
function accessing a value outside of its own definition, so the access happens
at `contended` mode, meaning `x.a` is allowed but `x.b` is not.


We need another invariant for `portable` to work, since it would do no good to
force a function to be `portable` if it can call a `nonportable` function
(again, the opposite of `portable`) to do its dirty work. In order to do so, of course, it
would need to access the `nonportable` function, so we can just forbid that:

> A `portable` function may only access a value outside its definition if that
> value is itself `portable`.

This is similar to the rule about `contended`, only rather than altering the variable's mode,
we forbid access altogether. Taken together, the rules for `portable` say that to access `x`
defined outside a `portable` function, `x` must already be `portable`, and it
gets accessed at mode `contended`.

Finally, we've covered `portable` functions, but the `portable` mode applies to non-function
types as well. However, non-function types are _almost_ always portable: there's no harm in
letting another domain access a string (or even something mutable, so long as the other domain
sees it as `contended`). The one exception is we can't let the user smuggle a `nonportable`
function into another domain by hiding it in another data structure:

> Every component of a `portable` value must be `portable`.

This makes the `nonportable` mode _deep_, in a similar way to `contended` (and
we use exactly the same definition for [_component_](#contended-deep) from
there). In fact, modes are often deep like this. **\[I could go into a whole
thing about modalities here, though I _definitely_ want to avoid the _word_
“modalities.” But it doesn't seem worthwhile.\]**
/ugh
--> 

# Mode crossing

**[Enough to cover `immutable_data` and `mutable_data`]**

# Atomics

# But what if parallel sequences?

```ocaml
module Tree = struct
  type 'a t =
    | Leaf of 'a
    | Nodes of 'a t iarray
end

let average tree =
  let rec total tree =
    match tree with
    | Tree.Leaf x -> ~total:x, ~count:1
    | Tree.Nodes arr ->
      let totals_and_counts = Iarray.map ~f:total arr in
      Iarray.fold
        totals_and_counts
        ~init:(~total:0.0, ~count:0)
        ~f:(fun (~total:total_acc, ~count:count_acc) (~total, ~count) ->
          ~total:(total +. total_acc), ~count:(count + count_acc))
  in
  let ~total, ~count = total tree in
  total /. (count |> Float.of_int)
;;

let average_par (par : Parallel.t) tree =
  let rec (total @ portable) par tree =
    match tree with
    | Tree.Leaf x -> ~total:x, ~count:1
    | Tree.Nodes arr ->
      let seq = Parallel.Sequence.of_iarray arr in
      let totals_and_counts = Parallel.Sequence.map' ~f:total seq in
      (match
          Parallel.Sequence.reduce
            par
            totals_and_counts
            ~f:(fun (~total:total_acc, ~count:count_acc) (~total, ~count) ->
              ~total:(total +. total_acc), ~count:(count + count_acc))
        with
        | None -> ~total:0.0, ~count:0
        | Some total_and_count -> total_and_count)
  in
  let ~total, ~count = total par tree in
  total /. (count |> Float.of_int)
;;
```

**[Also do version with `unfold` that parallelizes over the whole tree]**

# But what if capsules and locks?
