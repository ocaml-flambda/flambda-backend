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
of parallelism. It is also passed to the tasks, which is useful whenever a task
wants to spawn sub-tasks.

To run `add4`, we need to get our hands on a *scheduler*. Each scheduler is
provided by a library and determines the policy by which tasks get doled out and
run in domains. For this tutorial, we'll use `parallel_scheduler_queue`, which
maintains a pool of worker domains that pull tasks from a global queue. You can
also use `parallel_scheduler_sequential**, which is a trivial “scheduler” that
just runs everything on the primary domain. This is handy if you want to test
parallel code without switching to a multicore-enabled runtime.

**[This is a slightly old phrasing: probably we should show `average` working
fine until we split it across files]**

If we try to call **`[f]`** from our threads, however, we get an error from the
compiler:

**\[error\]**

What this is saying in essence is that the arguments to `Parallel.fork_join2`
must be *portable*, which is to say, declared to be safe to call from any
thread. Writing such a declaration is easy enough:

**\[naively modify program\]**

But, as usual, the compiler doesn't simply take us at our word:

**\[new error: the function isn’t actually portable\]**

## A brief primer on `portable` and `contended`

Like a type, a mode describes something about a name in an OCaml program. But
whereas a type describes the *value* associated with a name, a mode instead
describes the value's *circumstances.* This could be where it is in memory, who
has access to it, or what can be done with it. If you've seen `@ local` **(link
to doc)** or the older syntax `local_`, you've already encountered the `local`
mode.

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

> **Rule 1.** If multiple accesses of the same value may occur in parallel, at
> most one of them may consider the value `uncontended`. The others must
> consider it `contended`.

> **Rule 2.** Reading or modifying a `mutable` field is not allowed if the
> record is `contended`. The same goes for any element of a `contended` array
> (unless it's an `iarray`, since those are immutable).

Taken together, these two rules guarantee data-race freedom. However, rule 1
requires quite a bit of machinery to enforce, including other modes such as
`portable` as well as the types of parallelism APIs like `fork_join`.

Let's see what these rules mean for code. Rule 2 is easy because it's a simple
typing rule:

```ocaml
type t = {
  a : int;
  mutable b : int;
}

let f (t @ contended) =
  printf "%d\n" t.a; (* ok: [a] is immutable *)
  t.b <- t.b + 1 (* error: [b] is mutable *)
```

```
Error: This value is contended but expected to be uncontended.
```

Since `t` is `contended`, there may be another domain trying to mutate it in
parallel:

```ocaml
let g (t @ uncontended) =
  t.b <- 99 (* cue ominous music *)
```

So we can't allow the access in `f` unless we flag the argument as
`uncontended`:

```ocaml
let f (t @ uncontended) =
  printf "%d\n" t.a; (* ok: [a] is immutable *)
  t.b <- t.b + 1 (* ok: [t] is [uncontended] *)
```

Of course, this only helps if it is in fact impossible to call `f` and `g` with
the same argument in parallel. That's where rule 1 comes in. There's no one
place where rule 1 is enforced---really, it's an invariant of the whole system
(language, runtime, and core libraries)---but let's see one example of trying to
break it:

```ocaml
let () =
  let t = { a : 1; b : 2 } in
  let (), () = Parallel.fork_join2 par (fun _par -> f t) (fun _par -> g t) in
  ()
```

```
Error: This value is contended but expected to be uncontended.
```

When we cover the `portable` mode we'll be able to explain precisely what's
going on here, but for the moment suffice it to say that `fork_join2` requires
that it be safe to run `f t` and `g t` in parallel. Treating `t** as `uncontended**
in both tasks would violate rule 1.

**[Something to connect to:]**

> **Rule 3.** An `uncontended` value may be used as though it is `contended`.

In this way, `contended` is much like `local`: if a function takes a `contended`
parameter then the caller is _allowed_ to pass a `contended` value but it is not
required to. Similarly, `uncontended` is like `global`: it is a constraint that
_must_ be met.

**[Now a bit of explanation leading to ...]**

> **Rule 4.** Any component of a `contended` value is `contended`.

**[... which is easy to justify by rule 1.]**

**[Also:]**

#### The `shared` mode

**[Also mention `shared` in case people see it in errors.]**

### The `portable` and `nonportable` modes

Now that we've covered `contended`, we turn to `portable`. Like `contended`, it
has two main rules, namely a big-picture safety rule and a more concrete typing rule:

> **Rule 1.** Only a `portable` value is safe to access from outside the domain
> that created it.

> **Rule 2.** If a `portable` function refers to a value outside of its own
> definition[^closures], then (a) that value must be `portable`, and (b) the value
> is treated as `contended`.

[^closures]: For the initiated, “all values outside its definition” means
every variable that the function closes over (which does include global variables).


This covers similar territory to `contended`, but there are important
differences: if a value isn't `portable` (in which case it has the `nonportable`
mode), then other domains can't access it _at all._ So something can be
`portable` and `uncontended`, meaning that value is _safe_ to share with other
domains, but either no other domain _currently_ has access or every other domain
sees it as `contended`. (If two domains see the same value as `uncontended` then we
have A Problem since the Golden Rule no longer protects us from data races.**

**[more words here]**

> **Rule 3.** A `portable` value may be treated as `nonportable`.

> **Rule 4.** Every component of a `portable` value must be `portable`**.

**[Point out that this rule 4 is different from the rule 4 for `contended`]**

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

# But what if capsules and locks?
