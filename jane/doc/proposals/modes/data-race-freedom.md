# Data race freedom

We would like to provide a programming model for shared-memory
parallelism in OCaml that is less terrifying than the free-for-all you
get in languages like Java, C, C++ and Go. In particular, we intend to
statically prevent data races. We define a data race as two or more
threads concurrently accessing a non-atomic memory location where at
least one of the accesses is a write.

Data races are bad because their behavior is under-specified and hard
to reason about. They are generally a mistake caused by the programmer
accidentally sharing mutable state in a way they did not intend.

OCaml has unrestricted mutable state. Controlling that mutability is
at the heart of preventing data-races. We take a two-pronged approach:

- We place restrictions on how mutable state can be shared between
  threads. In general mutable state cannot be shared between
  threads. However, we also provide mechanisms to wrap mutable data up
  into isolated *capsules* allowing them to be shared between threads.

- We provide support for new restricted forms of mutation that cannot
  cause data-races:

  * We support reusing the space from one *immutable* value to create a
    second *immutable* value if there are no aliases remaining to the
    original value. Semantically this behaves as creating a fresh value
    however, unlike allocating fresh immutable values, it does not incur
    the cost of actually allocating.

  * We provide support for data that can only be mutated if there are
    no other active aliases to that data. Since there are no other
    active aliases, the mutation cannot cause a data race.

  * We add some mutable types that can only be mutated if you have
    exclusive access to an associated *key* value. Since the key is held
    uniquely there cannot be any data-races from these mutations. Keys
    can be managed using traditional methods of mutual exclusion e.g.
    locks.

These approaches rely on the ability to reason about whether a value
is held uniquely and whether it contains mutable state. We rely on
a system of *modes* to track these properties of values.

# Modes are good

Our support for stack allocation in OCaml is based around tracking
locality in the type system. We can generalize this tracking of
locality to a general concept of *mode*.

A mode is a property of value that is independent of its type. The
type system tracks the modes of variable bindings and expressions in
addition to their types.

We will use several axes of modes to support data-race freedom, in
addition to the existing axis of locality modes. Each axis describes
one aspect of a value; for example, the locality axis describes
whether the value can escape a region.

By default, a mode applies deeply to all the values reachable from the
original value. For example, a local `string list` is a local `list`
of local `string`s. Modes *do not* apply deeply through function
types. A `global` function's closure will only contain `global` values,
but a `global` function may accept `local` parameters or return `local`
results.

Since modes do not apply deeply through functions types, they are
annotated with the modes of their parameters and results. For example:
```ocaml
val foo : local 'a -> 'b -> local 'c
```

Record and constructor fields can be annotated with *modalities* that
allow a value at one mode to contain a value at a different mode. For
example, record fields can be annotated with `global`:
```ocaml
type t = { a : string; global b : string }
```
which means that the value in the field is always `global` --
even when the record itself is `local`.

Modes in each axis are ordered by their "strictness". For example,
`global` is more strict than `local` because the former requires the
the data to be allocated in heap, while the latter allows heap or
stack allocation. Values of one mode can always be used as values of a
less strict mode. For example, a `global` value can always be used as
a `local` value.

All modes can be inferred. Mode annotations are only required when
writing function types e.g. in `.mli` files. Explicit mode annotation
can still be used to ensure a specific mode, similar to explicit type
annotation.

As a preview, we are going to describing 5 mode axes in this document,
which have the following modes ordered by increasing strictness.

| Locality | Portability   | Uniqueness | Linearity  | Readonly  |
| -------- | ------------- | ---------- | ---------- | --------- |
| Local    | Nonportable   | Aliased    | Once       | Readonly  |
| Global   | Portable      | Exclusive  | Separate   | Readwrite |
|          |               | Unique     | Many       |           |

The *locality* axis tracks which values can leave regions. The *portability*
axis tracks which values are allowed to be shared with other
threads. The *uniqueness* axis tracks which values have aliases. The
*linearity* axis tracks functions that can only be run at most once. The
*readonly* axis tracks which values can have their mutable parts
mutated.

# Modal kinds and mutable fields

For any modality there are types which are unaffected by it. For
example, a global `int` is equivalent to a local `int`, because an
integer is immediately available and does not need extra allocation
on the stack or heap. We can use *kinds* to track such types and then
allow values of those types to freely change between the compatible
modes. For example, a function expecting a global `int` can be
passed a local `int` instead.

For each mode `m` there is a kind `always(m)` of types that can always
be considered to have that mode. `m` is an upper bound on the mode of
values of types with kind `always(m)`.

A value of a type of kind `always(m)` can be used at mode `m` regardless
of the original mode the value. This *mode-crossing* requires that the
type of the value be known at the point where the value is used --
similar to how record field disambiguation and constructor
disambiguation work.

We also use these modal kinds to allow records with mutable fields to
be created at different modes. For example, given the type definition:
```ocaml
type 'a t =
  { name : string;
    mutable data : 'a; }
```
an expression creating a `t`:
```ocaml
{ name = n; data = d }
```
creates a value at mode `m` if:

1. The mode of `n` is less than `m`.

2. The type of `d` is of kind `always(m)`.

This ensures that, not only is `d` of mode `m` but so is any value
that it might be replaced by through mutation. Key here is that, for
mutable fields, we examine only the kind of the type of the field,
not the mode on any expression we use to initialize the field.

# Mode polymorphism & new mode syntax

Already with just two modes (`local` and `global`) you sometimes need
to duplicate functions to get versions that work on values at either
mode. For example, `List.fold` could be given the following four
types:
```ocaml
val fold :
  'a list
  -> init:'b
  -> f:local ('b -> 'a -> 'b)
  -> 'b

val fold :
  local 'a list
  -> init:'b
  -> f:local ('b -> local 'a -> 'b)
  -> 'b

val fold :
  'a list
  -> init:local 'b
  -> f:local (local 'b -> 'a -> 'b)
  -> local 'b

val fold :
  local 'a list
  -> init:local 'b
  -> f:local (local 'b -> local 'a -> 'b)
  -> local 'b
```

This problem obviously gets worse as we add further modes. To address
this we intend to add support for mode polymorphism. Allowing
`List.fold` to be given a single general type of which all the above
types are instances.

Adding more modes and mode polymorphism starts to risk making the type
syntax over-complicated, especially for beginners that shouldn't have
to worry about modes initially. We intend to address this by making
two changes to our mode syntax. First, we are going to switch from
writing modes in front of types on arguments and returns:
```ocaml
val map : local 'a -> local 'b
```
to writing them after the type separated by an `@`:
```ocaml
val foo : 'a @ local -> ('b @ local)
```
Second we are going to allow the mode parts of a type to be
"unzipped", writing first the type without any modes and then a
skeleton of the type holding the modes:
```ocaml
val foo : 'a -> 'b @ local -> local
```

This allows the general type of `List.fold` to be written as something
like:
```ocaml
val fold :
  'a list
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b
  @
    'm
    -> init:'n
    -> f:local ('n -> 'm -> global 'n)
    -> 'n
```
We write `.` to stand for the default mode.

The precise details of mode polymorphism are not yet worked out --
there are a number of design decisions still to be made -- but we
don't expect there to be any fundamental difficulties.

# Don't share mutable data

The root cause of potential data races in OCaml is mutability. In
particular, `mutable` record fields and mutable arrays. To prevent
data races we need to place restrictions on this mutable data. To do
this we add a new `portable` mode. A value is `portable` if **it can be passed
between threads without risking introducing data races**. Values that
are not `portable` are at mode `nonportable`. An array or record with mutable
fields is always `nonportable`.

Naturally `portable` can be relaxed into `nonportable`: one can always forget
that a value can be safely passed between threads.

A function is `portable` if it only closes over other `portable` values. This
ensures that it can be safely sent to another thread. Note that a
`portable` function can still receive `nonportable` values via parameters and
return them as results. For example, `Array.concat` is a `portable`
function even though it manipulates mutable data: since it doesn't
close over `nonportable` values it is safe for it to be called in two
threads simultaneously.

We use `portable` to ensure that two threads never share raw mutable
state.  When spawning a new thread, we require the thread body to be
`portable`, so that no `nonportable` value is passed from the spawning thread
to the spawned thread:

```ocaml
val spawn : (unit -> 'a) -> 'a t
          @ portable (. -> .) -> .

val join : 'a t -> 'a
         @ . -> .
```

Similarly, any mechanism for communicating between threads,
e.g. channels, requires the transferred data to be `portable`:
```ocaml
val send : 'a channel -> 'a   -> unit
         @ . -> portable -> .

val recv : 'a channel -> 'a
         @ . -> portable
```

Note that any deeply immutable type is of kind `always(portable)` allowing
any value of such types to be passed between threads.

We allow a `portable` modality on record fields so that the fields will be
`portable` even if the record is `nonportable`. We obviously cannot allow a
`nonportable` modality, because a record with `nonportable` fields cannot itself
be `portable`.

The majority of toplevel functions will already be `portable` since people
try to avoid using global mutable state. Most others will become `portable`
by the addition of e.g. locking to allow them to be called from multiple
threads.  As such, it would be better in the long run to have `portable` be
the default mode for things in modules and require people to specify
which functions are `nonportable`. Since this would be a backward
incompatible change, and only affects the meaning of the default syntax,
we'll let the user choose the default via a command-line flag, with the
aim being to make the new state the default and eventually deprecate the
flag.

# Uniqueness

A motivating example for uniqueness mode is the following:
```ocaml
let r = {x = 3; y = 5} in
...
let s = {r with x = 7} in
...
```
This allocates fresh space for `s`. However, it is common that `r` will
never be referred to again after `s` is created, in which case we can
optimize the machine code so that `s` will skip allocating and
copying from `r`, but instead just reuse the space of `r`.

To support this optimization we add the following in-place record update
operation:
```ocaml
{overwrite r with x = 7}
```
that behaves as `{r with x = 7}` but reuses the space of `r`.  To
ensure that this operation is only used when it is safe, we introduce
the `unique` mode as part of a new *uniqueness* mode axis.

A value is `unique` if **it is not aliased with any other value in the
whole system**. More precisely:
```ocaml
e == f
```
is always `false` if the expression `e` returns a unique value. Note
that in the expression `x == x`, `x` cannot be given the unique mode
because it is used twice. Values which aren't `unique` are at mode
`aliased`.

Only values at mode `unique` can be used with in-place record
update.  Consider the following example:
```ocaml
let r = {x = 3; y = 5} in
let s = {overwrite r with x = 7} in
r
```
which gives a type error:
```ocaml
Line 2, characters 22-23:
2 | let s = {overwrite r with x = 7} in
                       ^
Error: r is used uniquely here so cannot be used twice. It will be used
  again at:
Line 3, characters 3-4:
3 | r
    ^
```
There are two occurrences of `r`, so `r` will be a `aliased` value at
these occurrences. As a result, `r` cannot be used with in-place record
update. It would be unsafe in the sense that `r.x` is observed to be
mutated while being an immutable field.

Note that while the updated value reuses the space of the old value,
semantically they are still two distinct values. In particular, they
could have different types. Consider the following example:
```ocaml
type 'a foo = {x : int; y : 'a}

let r = {x = 3; y = "foo"} in
let s = {overwrite r with y = 5} in
...
```
where `r` is of type `string foo` and `s` of type `int foo`, while
sharing the same memory space. Such an update that changes not only
the value but also the type is called a *strong update*. It is
obviously only safe if `r` is never used again after the update,
otherwise `r.y` might be read as `string` while actually being an
`int`.

A useful example of strong update is the in-place version of
`List.map` (using a list type defined as a record type):
```ocaml
type 'a list =
  | Cons of { hd : 'a; tl : 'a list }
  | Nil

let rec map f (unique_ xs) =
  match xs with
  | Nil -> xs
  | Cons ({hd;tl} as xs) -> Cons { overwrite xs with hd = f hd; tl = map f tl }
```

Note that using the `overwrite` syntax also requires that any values
being written into the record are global. For instance,
```ocaml
{ overwrite r with s = msg }
```
requires that `msg` be `global`. This is because, even if `r` is
`local`, it's possible that it was allocated on the heap, and the
runtime does not support pointers from the heap to the stack.

By default a value being unique means that everything reachable from
it is also unique. For example, the following is an error because the
explicit annotation enforces the tuple to be `unique`, which means
everything reachable from the tuple must be `unique`. However, the
first element of the tuple is not `unique` because it is aliased with
another value (namely in the second component of the tuple), and
similar for the second component.

```ocaml
let dup x = unique (x, x)

[%%expect{|
Line 1, characters 24-25:
1 | let dup x = unique (x, x)
                            ^
Error: x is used uniquely here so cannot be used twice. It will be used again at:
Line 1, characters 21-22:
1 | let dup x = unique (x, x)
                         ^
|}]
```

An `aliased` modality can be applied to record fields, so that the field
will be `aliased` regardless of the mode of the record. One particular
application is the following, where `'a aliased` would represent some
`aliased` `'a` regardless of the mode of the `'a aliased`.
```ocaml
type 'a aliased = {aliased foo : 'a } [@@unboxed]
```

Note that a `unique` modality on record fields is not permitted as it
breaches mode safety. The result of reading from such a field could
not be unique because the value would still be aliased in the shared
record from which it was read.

`unique` is more strict than `aliased` as it restricts the number of
aliases.  One can always forget this extra bit of information about a
value, and relax a value of `unique` into a value of `aliased`.  In the
following, `x` is `unique` but relaxed to `aliased` which allows it to
be aliased twice (by the tuple); the resulting tuple is `aliased` as
well of course.
```ocaml
let dup (unique x) = (x, x)
```

# Borrowing

One might wish to use a `unique` value as `aliased` for a short
duration. For example,
```ocaml
type point = { x : float; y : float }

let total p = p.x +. p.y

let averagize p =
  let avg = (total p) /. 2.0 in
  { overwrite p with x = avg; y = avg }
```
This would be an error because `p` has two occurrences but the second
occurrence requires that it be unique. However, it is clear that by
the time of the second occurrence `p` will in fact be unique again.

To support such code we introduce *borrowing*. Borrowing allows
`unique` values to be `aliased` for the duration of a region, and
recovers them to `unique` afterwards. To borrow the variable `x` you
use the syntax `&x`. So that `averagize` becomes:
```ocaml
let averagize p =
  let avg = (total &p) /. 2.0 in
  { overwrite p with x = avg; y = avg }
```

The `&x` syntax creates an implicit region and treats `&x` as `aliased`
and `local` for the duration of this region. The `local` mode prevents
the value from escaping the region, which allows `x` to be considered
`unique` again after the region has ended.

The implicit regions take one of the following forms -- using `{...}` to
indicate where the implicit region would be:

- Surrounding a function call:
```ocaml
 let foo x =
    let y = {bar &x} in
    unique x
 ```

- Around a `let` binding:
```ocaml
let foo x =
   {let y = &x in
   bar y y
   }
```

The implicit region for a use of `&x` will be the nearest enclosing potential
region. For example,
```ocaml
let foo x y =
    let z = bar x ({baz &y}) in
    unique y
```

Since a borrowed value is `aliased`, there may be multiple borrows
within the same (implicit) region. In the following example, `x` is
borrowed twice in the same region, both of which would be used as
`aliased` by `bar`.
```ocaml
let foo x =
  let y = {bar &x &x} in
  unique x
```

The original unique value is not accessible uniquely in the borrowing
region. The following example is an error because `x` is borrowed as `y`
for the whole `let` binding, and would not be accessible uniquely for
the call to `baz`.
```ocaml
let foo x =
  let y = &x in
  let z = bar y in
  baz (unique x)
```

# Exclusivity

Consider the following program:
```ocaml
let foo x =
  ...
  let x = {overwrite x with a = "hello"} in
  ...
  x

let bar x =
  ...
  let x = {overwrite x with b = "goodbye"} in
  ...
  x

let process x =
  ...
  let x = foo x in
  ...
  let x =
    if p then begin
      ...
      let x = bar x in
      ...
    end else begin
      x
    end
  in
  ...
  unique x
```
The `process` function operates on a unique value `x`, using `foo`
and `bar` which update `x` in place. Since these in place updates
require `x` to be unique, `x` must be threaded through the body of `process`
passing it in and getting it out of each call that modifies it. This passing
around of `x` can become cumbersome for large programs.

As an alternative we propose `exclusively mutable` record fields:
```ocaml
type t =
  { exclusively mutable a : string
  ; exclusively mutable b : string }
```

Such fields can be mutated using the usual syntax
```ocaml
x.a <- "hello"
```
but only if the value being mutated (`x`) is *exclusive*. Unlike
ordinary `mutable` fields, records with `exclusively mutable` fields can
be `portable`.

The `exclusive` mode is closely related to the `unique` mode. They both
restrict how values can be used to limit how they can be aliased. The
difference comes in how they treat borrowing. Values can be borrowed as
`exclusive` but values can never be borrowed as `unique`.

More precisely, a value being `exclusive` requires that **it is not
aliased with other values that are active in the current region**. It
might be aliased with other values, but those values will not be used in
the current region.

For instance, our example above can be rewritten as:
```ocaml
let foo x =
  ...
  x.a <- "hello";
  ...

let bar x =
  ...
  x.b <- "goodbye";
  ...

let process x =
  ...
  foo &x;
  ...
  if p then begin
    ...
    bar &x;
    ...
  end;
  ...
  unique x
```
Here, the `foo` and `bar` functions require that their parameter be
`exclusive`.

Note that the `&x` passed to `foo` and the `&x` passed to `bar` are
aliases to each other; however, the first `x` is active only during
the region around the call to `foo`, whilst the second `x` is active
only during the region around the call to `bar`. This means that,
whilst they aren't `unique`, these borrowed values are `exclusive`.

Having a value exclusively is not a strong enough condition to
`overwrite` it with another value -- you might have borrowed the value
from elsewhere and when your borrow ends the original owner will reclaim
it and continue to use it. This is why we need to have both `exclusive`
and `unique` modes.

Much like `unique` values, the compiler checks values used as
`exclusive` are not aliased. For example, the following code:
```ocaml
let dup x = exclusive (x, x)
```
is an error:
```ocaml
Line 1, characters 27-28:
1 | let dup x = exclusive (x, x)
                              ^
Error: x is used exclusively here so cannot be used twice. It will be used
  again at:
Line 1, characters 24-25:
1 | let dup x = exclusive (x, x)
                           ^
```

A borrowed value is `exclusive` as long as the following conditions
hold:
1. The original value is `unique` or `exclusive`.
2. The original value is not borrowed or used elsewhere in the implicit
   region of the borrow.

For example, the following is an error:
```ocaml
let foo (exclusive x) y =
  x.a <- y.a;

let bar x =
  ...;
  foo &x &x
```
because `foo` requires its first parameter exclusively, but it is
borrowed twice within the same implicit region.

Unlike ordinary `mutable` fields, there is no possibility of data races
when mutating `exclusively mutable` fields. There is also no possibility
of unexpected aliasing: if you are reading the value of an `exclusively
mutable` field then no other piece of code can be updating it. Combined
with the fact that borrows are syntactically explicit (thus signalling
the possibility of mutation), you get much of the same benefits as
immutability when reasoning about your code.

# Closures

## Accessing unique values from closures

Consider the following code:
```ocaml
let foo x =
  let bar () =
    if String.equal x.a "foo" then
      { overwrite x with a = 42 }
    else
      { overwrite x with a = 24 }
  in
  bar
```
The function `bar` clearly cannot run more than once. It strongly
updates `x`, and running it the second time would be reading the
integer `x.a` as a string, which is unsafe. To be safe, we forbid any
function closing over unique values to be called more than once.

To this end, we introduce the `once` mode. This mode **prevents
additional aliases of the value from being created**. For example, the
following is an error, because the value `x` while being `once` is
referred to twice.
```ocaml
let dup (once x) = (x, x)
```
This ensures that, for example, a `once` function can only be called a
single time.

Values that are not `once` will be at mode `many`. `many` is stricter
than `once`: it is safe to forget that a value can be used multiple
times, and instead use it only once.

In addition, a `many` modality can apply to record fields, so that the
fields will be `many` regardless of the mode of the record.

`once` and `unique` are closely related: a value being `once` means it
cannot have more aliases created in the *future*, a value being
`unique` means it has had no aliases created in the *past*. In
particular, a function parameter being `unique` is a restriction on
the caller, whilst a function parameter being `once` is a restriction
on the callee.

Putting all of this together, we would infer the following type for
`foo`:

```ocaml
val foo : t -> (unit -> t)
          @ unique -> once
```

This says that `foo` takes a `t` it is allowed to destroy and returns
a function that can be called at most once.

As another example, `Deferred.bind` could probably have the following type
to indicate that the `f` parameter will be called only once:
```ocaml
val bind : 'a t -> f:('a -> 'b t) -> 'b t
          @ . -> once -> .
```
which means `f` can close over `unique` values and strongly update
them. By making a stricter requirement on `bind` -- that it can call
`f` at most once -- we give its caller the freedom to pass in more
possible functions.

Functions that close over `once` variables are themselves at mode
`once`. In the following example, `bar` must be called at most
once to ensure that `f` is called at most once.
```ocaml
let foo (once f) =
  let bar () =
    f 42
  in
  bar
```

Since `once` is about restricting functions, concrete data types
that don't contain functions are `always(many)`.

## Accessing exclusive values from closures

There are three different ways that a local function can access an
exclusive value from its surrounding environment.

1. Putting the value in the function's closure, and then using the
   value directly from the closure in the body of the function. We
   shall refer to this as *consuming* the value.

2. Borrowing the value, putting the borrowed value in the closure, and
   then re-borrowing the value whenever it is used in the body of the
   function. We shall refer to this as *borrowing* the value.

3. Putting the value in the function's closure, and then borrowing the
   value whenever it is used in the body of the function. We shall
   refer to this as *moving* the value.

Each of these ways places different restrictions on the function and
on the uses of the value inside the function.

### Consuming the value

Consider the following code:
```ocaml
type t =
  { exclusively mutable a : int }

let set t x = t.a <- x

let updater t =
  (fun x -> set t x)
```
The `updater` function returns a closure that uses `t` exclusively
without borrowing it. That can only be done once and so this closure
will be created at mode `once`, much like a function that uses a value
from its closure uniquely.

The types of these functions will be:
```ocaml
val set : t -> int -> unit
         @ local exclusive -> . -> .

val updater : t -> (int -> unit)
             @ unique -> once
```

### Borrowing the value

Consider the following example,
```ocaml
type t =
  { exclusively mutable a : int }

let get t = t.a

let set t x = t.a <- x

let foo t g =
  let () =
    let bar f =
      let old = get &t in
      set &t (old + 1);
      f ();
      assert (get &t = old + 1)
    in
    g bar
  in
  unique t
```
The `bar` function closes over the `t` value exclusively. The
assertion should be guaranteed to pass: since `t` is exclusive there
should be no other aliases to it, so the call to `f ()` should not be
able to change `t`'s value.

Since each use of `t` within `bar` is a borrow, we also consider the
definition of `bar` itself to borrow `t`. That means that `bar` must
be local.  This condition is not sufficient to ensure that `t` can be
used exclusively in `bar`. Consider the following potential use of
`foo`:
```ocaml
let bad t =
  foo t (fun bar -> bar (fun () -> bar (fun () -> ())))
```
This code would cause the assertion in `bar` to fail, because the
`f()` call in `bar` would in fact call `bar` itself, resulting in `t`
being unexpectedly mutated.

To be safe, at most one instance of `bar` can be running
simultaneously. To ensure this we add a `separate` mode that sits
between `once` and `many`. A value being at mode `separate` **prevents
additional aliases that are active in the current region from being
created**. This is ensured by preventing the creation of new aliases
to `separate` values, whilst still allowing `separate` values to be
borrowed.

A `separate` value can be borrowed as long as the following conditions
hold:

1. The borrowed value is `separate` or `once`.

2. The original value is not used elsewhere in the implicit region of
   the borrow.

This prevents the `bad` example from passing type-checking: the `bar`
parameter is `separate` and so it cannot be passed to itself. Whereas,
the following use of `foo` would be fine:
```ocaml
let good t =
  foo t
    (fun bar ->
       &bar (fun () -> ());
       &bar (fun () -> ()))
```
because the implicit regions of the two borrows do not overlap.

Pulling this all together, we get these inferred types from this example:
```ocaml
val bar : (unit -> unit) -> unit
         @ local separate (local once (. -> .) -> .)

val foo : t -> (((unit -> unit) -> unit) -> unit) -> t
         @ unique
           -> local once (local separate (local once (. -> .) -> .) -> .)
           -> unique

val good : t -> t
          @ unique -> unique
```

Similar to the relationship between `once` and `unique`, `separate` is
dual to `exclusive` in that a value being `separate` poses a restriction
on code *using* the value, while a value being `exclusive` poses
restriction on code *providing* the value. For example, `List.iter`
could have type:
```ocaml
val iter : 'a t -> f:('a -> unit) -> unit
          @ . -> local separate -> .
```
to indicate that the `f` parameter is only run one-at-a-time and
doesn't escape. This allows `f` to close over some `exclusive` values
By posing stricter requirements on `iter`, we give more freedom to
`f`.

Note that functions closing over `separate` values are themselves
`separate`.

### Moving the value

Consider the following example:
```ocaml
type t =
  { exclusively mutable a : int }

let get t = t.a

let set t x = t.a <- x

let foo t =
  let global bar f =
    let old = get &t in
    set &t (old + 1);
    f ();
    assert (get &t = old + 1)
  in
  bar
```
This is similar to the example from the previous section, except
here `bar` is returned from `foo` and `t` is not used outside of
`bar` again. This requires that we *move* `t` into `bar`, which
is indicated by the user by the `global` annotation on the definition
of `bar`.

We get the following inferred types for `bar` and `foo`:
```ocaml
val bar : (unit -> unit) -> unit
          @ separate (local once (. -> .) -> .)

val foo : t -> ((unit -> unit) -> unit)
          unique t -> separate (local once (. -> .) -> .)
```

Moving an exclusive value into a closure produces a closure at mode
`separate`, since at most one instance of the function can be run
simultaneously. This prevents code like the following:
```ocaml
let bad t =
  let bar = foo t in
  bar (fun () -> bar (fun () -> ()))
```

# Keys and locks

Uniqueness and exclusivity provide forms of mutation that cannot race,
allowing the data that is mutated to be safely aliased between
threads. However, they require the user to carefully manage aliases
to all the mutated data and to convey that information to the type
checker.  This could quickly become cumbersome as one needs to
carefully keep the `exclusive`/`unique` mode when passing around
values.

Moreover, some programs fundamentally cannot be directly expressed
using just exclusive and unique. Consider the following example of
constructing a circular graph:
```ocaml
type node =
  { data : int;
    mutable next : node option }

let create_loop () =
  let n0 = {data = 42; next = None} in
  let n1 = {data = 24; next = Some n0} in
  n0.next <- n1;
  n0
```
The above would not work if we rely on `exclusively mutable` instead of
`mutable`.  When performing `n0.next <- n1`, `n0` cannot be `exclusive`
because it is aliased as `n1.next`.

However, we can provide some additional APIs built around `exclusive`
and `unique` that enable data-race free mutation without requiring the
mutated data itself to be `exclusive` or `unique`.

The core of these APIs is the `Key.t` type:
```ocaml
module Key : sig

  type 'k t : void & always(portable)

  type packed = Key : 'k t -> packed [@@unboxed]

  val create : unit -> unique packed

end
```

The essential invariant that the above API ensures is that for each type
`k` there is at most one value of type `k Key.t`. This property is
maintained by the `create` function returning an existential type. That
means that each call to `create` returns a key for a fresh `k` type.

A *key* is a form of capability. For a given `k`, there can be only one
`k Key.t` at `exclusive` mode at any point in time. If there is a `k
Key.t` at `aliased` mode then there are no `k Key.t`s at mode `exclusive`
simultaneously.

`'k t` is of the `void` layout, which means it consumes no space and
contains no information at run-time. It is also `always(portable)` so can
always be safely passed to another thread.

## Safe boxes

The first API using keys is `Safe_box`. A `Safe_box.t` is a piece of
mutable state guarded by a key. It can only be written to if you have
the key exclusively. That prevents data races without requiring the user
to keep track of aliases to the `Safe_box.t` itself.

The core of its API is as follows:
```ocaml
module Safe_box : sig

  type ('a, 'k) t : always(portable)

  val create : 'a -> ('a, 'k) t
               @ portable -> .

  val read : ('a, 'k) t -> 'k Key.t -> 'a
             @ . -> local -> portable

  val write : ('a, 'k) t -> 'k Key.t -> 'a -> unit
              @ . -> local exclusive -> portable -> .

end
```

To `read` the safe-box one must provide the associated key. To `write`
to the safe-box, one must provide the key exclusively. They both only
require the key locally, which allows one to use borrowed keys to
access safe-boxes.

The `create_loop` example can now be rewritten as
```ocaml
type 'k node =
  { data : int;
    next : ('k node option, 'k) Safe_box.t }

let create_loop key =
  let n0 = {data = 42; next = Safe_box.create None} in
  let n1 = {data = 24; next = Safe_box.create (Some n0)} in
  Safe_box.write n0.next key n1;
  n0
```
Note that the keys for `n0.next` and `n1.next` are not specified at
`create` but later inferred upon `read`/`write`. In practice, we would
protect the whole graph with a single key, by which all `Safe_box.t`s
within are parameterized.

## Unique safe boxes

We can also have a data-race free reference type that holds unique
values:
```ocaml
module Unique_safebox : sig

  type ('a, 'k) t : always(portable)

  val create : 'a -> ('a, 'k) t
               @ unique portable -> .

  val exchange : ('a, 'k) t -> 'k Key.t -> 'a -> 'a
                 @ . -> local exclusive -> unique portable -> unique portable

end
```
This interface is guaranteed to preserve the uniqueness of the value
because you can only swap the value out of the box, you cannot read
it. It is also guaranteed to be data-race free because you can only
swap out the contents if you have exclusive access to the key that
protects the safebox.

## Unique atomic

OCaml comes with an atomic reference type `Atomic.t`. This type will
be considered `portable` since racing on an atomic reference is not
considered a data-race. We can additionally add a `Unique_atomic.t`
that holds unique values:
```ocaml
module Unique_atomic : sig

  type 'a t : always(portable)

  val make : 'a -> 'a t
             @ portable unique -> .

  val exchange : 'a t -> 'a -> 'a
                @ . -> portable unique -> portable unique

end
```
There is no `get` operation, since that would allow the unique value
to become aliased. Note that the `exchange` operation must be atomic
to ensure the uniqueness of the result.

## Mutex

We can rely on mutual exclusion to ensure uniqueness. The following
mutex interface is a convenient way to support this:
```ocaml
module Mutex : sig

  type 'k t

  val create : unit -> 'k t

  val acquire : 'k t -> 'k Key.t
               @ . -> unique

  val release : 'k t -> 'k Key.t -> unit
               @ . -> unique -> .

end
```

The idea is that a mutex protects a `unique` key. One can `acquire` the
protected `unique` key, and then perform actions enabled by that key
(e.g. write to a `Safe_box.t`). When one is finished with the key, they
can `release` it back into the protection of the `Mutex`. Note that
`create` creates the mutex in the acquired state.

The `Mutex.t` is associated with a particular key type `'k`. That means
to `acquire` the intended `'k Key.t` one must be using the corresponding
`Mutex.t`, and this is ensured by the type checker.  This is a helpful
improvement over the traditional mutex interface where one can easily
acquire and release the wrong mutex without being warned by type system
at all.

We can give a spinlock-based implementation of `Mutex` using
`Unique_atomic.t`:
```ocaml
type 'k t = 'k Key.t option Atomic_unique_ref.t

let create () =
  Unique_atomic.create None

let rec acquire t =
  match Unique_atomic.exchange k None with
  | None -> acquire t
  | Some key -> key

let release t key =
  ignore (Unique_atomic.exchange t (Some key))
```
which shows that `Mutex` doesn't add any additional expressivity. In
practice however we would implement `Mutex.t` on top of a traditional
mutex implementation.

As a side-note, recall that `'k Key.t` is of `void` layout and so
consumes no space at runtime. As a result, `'k Key.t option` is simply
`bool` at runtime just like a traditional mutex: the extra safety
brought by `'k Key.t` is free.

We can define a useful `with_` combinator as:
```ocaml
let with_ t f =
  let key = acquire t in
  let res = f &key in
  release t key
  res
```
which has type:
```ocaml
val with_ : 'k t -> ('k Key.t -> 'a) -> 'a
            @ . -> local once (local exclusive -> .) -> .
```

# Capsules
The requirement that values shared between threads be `portable` essentially
forbids threads from sharing mutable (i.e. `nonportable`) values. This improves
safety at the cost of expressivity. To restore the expressivity of shared
mutable values but in a safe manner, we introduce capsules.

A *capsule* is a container holding some, possibly `nonportable`, data. Only
`portable` data is allowed to pass into or out of a capsule. This ensures
that any `nonportable` data within the capsule is entirely isolated from the
world outside the capsule. There is one capsule associated with each
key, and access to the capsule is guarded by that key. This ensures that
any `nonportable` data inside the capsule can only be accessed while holding
that key . That means it is safe to pass capsules between threads
without risking data-races.

Capsules are accessed via `Capsule.t`s. These are pointers into a capsule
from the outside world. They have the following interface:
```ocaml
module Capsule : sig

  type (+'a, 'k) t : always(portable)

  val pure : 'a -> ('a, 'k) t
            @ portable -> .

  val apply : 'k Key.t -> ('a -> 'b, 'k) t -> ('a, 'k) t -> ('b, 'k) t
              @ local exclusive -> . -> . -> .

  val extract : 'k Key.t -> ('a, 'k) t -> ('a -> 'b) -> 'b
               @ local exclusive -> . -> local once portable (. -> portable) -> portable

  val destroy : 'k Key.t -> ('a, 'k) t -> 'a
               @ unique -> . -> .

end
```

`portable` data can move freely between capsules, so all `portable` data can be
 thought of as being in all capsules simultaneously. `pure` makes such
 data available as a `Capsule.t`.

`apply` takes a `Capsule.t` pointing to a function and a `Capsule.t`
pointing to a value within the same capsule and runs the function on
that value returning a `Capsule.t` pointing to the result.

`extract` takes a `portable` function that returns a `portable` value, and a
`Capsule.t` pointing to some data, and runs the function on the data and
returns the result. Since the result must be `portable` it does not need to
be wrapped in a `Capsule.t`. This allows the user take `portable` data out
of a capsule.

`destroy` consumes a `key`, destroying the associated capsule. A
`Capsule.t` is also provided and the data from it is returned,
essentially moving it from the destroyed capsule to the outside world.

Note that `apply` and `extract` both require exclusive access to the key
associated with the capsule. This ensures that only one thread can be
accessing the mutable data in a capsule at any one time.

We can define a few more useful combinators:
```ocaml
let map k f x =
  apply k (pure f) x

let inject k f x =
  map k f (pure x)

let iter k f x =
  ignore (map k f x)
```
with signatures:
```ocaml
val map : 'k Key.t -> ('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t
           @ local exclusive -> portable -> . -> .

val inject : 'k Key.t -> ('a -> 'b) -> 'a -> ('b, 'k) t
              @ local exclusive -> portable -> portable -> .

val iter : 'k Key.t -> ('a -> unit) -> ('a, 'k) t -> unit
            @ local exclusive -> portable -> . -> .
```

As an example, below we encapsulate a mutable `Hashtbl.t`. The resulting
`'k Capsule_table.t` is `portable` so can be passed between threads.

```ocaml
module Capsule_table : sig

  type 'k t : always(portable)

  val create : 'k Key.t -> 'k t
              @ local exclusive -> .

  val add_exn : 'k Key.t -> 'k t -> int -> string -> unit
               @ local exclusive -> . -> . -> . -> .

  val find : 'k Key.t -> 'k t -> int -> string
               @ local exclusive -> . -> . -> .

end = struct

  type 'k t = ((int, string) Hashtbl.t, 'k) Capsule.t

  let create key =
    Capsule.inject key (fun () -> Hashtbl.create ()) ()

  let add_exn key t i data =
    Capsule.iter key (fun t -> Hashtbl.add t ~key:i ~data) t

  let find key t i =
    Capsule.extract key (fun t -> Hashtbl.find t i) t

end
```

Recall that we can use a mutex to dynamically protect a key. Below we extend
the above example to a type for a hash table protected by a lock:
```ocaml
module Locked_table = struct

  type t =
    Table :
      { table : 'k Capsule_table.t;
        mutex : 'k Mutex.t; } -> t

  let create () =
    let Key key = Key.create () in
    let table = Capsule_table.create &key in
    let mutex = Mutex.create () in
    Mutex.release mutex key;
    { table; mutex }

  let add_exn { table; mutex } i data =
    Mutex.with_ mutex (fun key ->
      Capsule_table.add_exn key table i data)

  let find { table; mutex } i =
    Mutex.with_ mutex (fun key ->
      Capsule_table.find key table i)

end
```

# Read-only capsule access

Capsules allow us to wrap nonportable mutable data and ensure that it is only
accessed by one thread at a time. However, this is stricter than is
necessary to avoid data races. Multiple threads can safely read from the
nonportable mutable state in a capsule as long as no thread can be
simultaneously writing to it.

To support this kind of access we add another mode: `readonly`. A
`readonly` value can contain nonportable mutable state, but that state cannot
be written to. A value that is not `readonly` is at mode
`readwrite`. `readwrite` is more strict than `readonly`.

There are two restrictions on `readonly` values:

1. Writing to a `mutable` record field requires that the record
   being written to be `readwrite`.

2. Functions can only be applied at mode `readwrite`.

For example, the following:
```ocaml
let set t =
  (readonly t).contents <- 5
```
is an error:
```ocaml
Line 2, characters 6-32:
1 |   (readonly t).contents <- 5
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot mutate readonly value
```

There is a `readonly` modality that allows `readwrite` data to point to
`readonly` data. Since `portable` values contain no nonportable mutable data, the
`portable` modality on record fields also gives `readwrite` values even if
the record is `readonly`. Similarly, `always(portable)` implies
`always(readwrite)`.

With this mode we can add the following additional operations to the
`Capsule` API:
```ocaml
val map_readonly : 'k Key.t -> ('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t
           @ local -> portable (readonly -> .) -> . -> .

val read : 'k Key.t -> ('a, 'k) t -> ('a -> 'b) -> 'b
             @ local -> . -> portable (readonly -> portable) -> portable

val freeze : 'k Key.t -> ('a, 'k) t -> 'a
             @ . -> . -> readonly
```
which require the capsule's key, but do not require it exclusively.

Using these it is possible to create a version of `Locked_hashtbl` that
uses a multiple-reader-single-writer lock to allow multiple threads to
read from the hash table simultaneously. However, it does require
changing the interface of the underlying `Hashtbl` module to indicate
which operations only read from the mutable state in the hash table.
