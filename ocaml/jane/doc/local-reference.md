# Local Allocations Reference

The goal of this document is to be a reasonably complete reference to local
allocations in OCaml. For a gentler introduction, see [the
introduction](local-intro.md).

When local allocations are enabled, the
compiler may locally allocate some values, placing them on a stack rather than
the garbage collected heap. Instead of waiting for the next GC, the memory used
by locally allocated values is reclaimed when their _region_ (see below) ends, and
can be immediately reused. Whether the compiler locally allocates certain values
is controlled using a new keyword currently spelled `local_`, whose effects in
expressions, patterns and types are explained below.


## Local expressions and allocation

The `local_` keyword may be placed on an expression to indicate that
allocations in that expression should be locally allocated:

```ocaml
let abc = local_ [a; b; c] in
...
```

Here, the three cons cells of the list `[a; b; c]` will all be locally
allocated.

Equivalently, the keyword `local_` may precede the pattern in a `let`:

```ocaml
let local_ abc = [a; b; c] in
...
```

Locally allocated values may reference global (that is, GC-allocated or
constant) values, but global values may not reference local ones. In the
example above, any or all of `a`, `b` and `c` may themselves be locally
allocated.

It is valid for an expression annotated `local_` to still yield a global value.
For instance, if there is a global `x : int list` in scope, then this is
allowed:

```ocaml
let l = local_ if n > 0 then n :: x else x in
...
```

Here, if `n > 0`, then `l` will be a locally-allocated cons cell. However, if
`n <= 0`, then `l` will be `x`, which is global. In other words, the `local_`
keyword on an expression permits but does not oblige that expression to locally
allocate its result.

Most OCaml types can be locally allocated, including records, variants,
polymorphic variants, closures, boxed numbers and strings. However, certain
values cannot be locally allocated, and will always be on the GC heap,
including:

  - Modules (including first-class modules)

  - Exceptions
    (Technically, values of type `exn` can be locally allocated, but only global ones may be raised)

  - Classes and objects

In addition, any value that is to be put into a mutable field (for example
inside a `ref`, an `array` or a mutable record) cannot be locally allocated.


## Inference

In fact, the allocations of the examples above will be locally
allocated even without the `local_` keyword, if it is safe to do so
(and the `-extension local` flag is enabled). The presence of the
keyword on an expression only affects what happens if the value
escapes (e.g. is stored into a global hash table) and therefore cannot
be locally allocated. With the keyword, an error will be reported,
while without the keyword the allocations will occur on the GC heap as
usual.

Inference does not cross file boundaries. If local annotations subject to
inference appear in the type of a module (e.g. since they can appear in
function types, see below) then inference will resolve them according to what
appears in the `.mli`. If there is no `.mli` file, then inference will always
choose `global` for anything that can be accessed from another file.

Local annotations (or the lack thereof) in the mli don't affect inference
within the ml. In the below example, the `~foo` parameter is inferred to
be local internally to `A`, so `foo:(Some x)` can be constructed locally.

```ocaml
(* in a.mli *)
val f1 : foo:local_ int option -> unit
val f2 : int -> unit

(* in a.ml *)
let f1 ~foo:_ = ()
let f2 x = f1 ~foo:(Some x) (* [Some x] is stack allocated *)
```

<!-- See Note [Inference affects allocation in mli-less files] in [ocaml/testsuite/tests/typing-local/alloc_arg_with_mli.ml]
     in the flambda-backend Git repo. The ensuing paragraph is related to that
     note; we can remove this comment when the note is resolved.
-->
However, a missing mli *does* affect inference within the ml. As a conservative rule of thumb,
function arguments in an mli-less file will be heap-allocated unless the function parameter or argument
is annotated with `local_`. This is due to an implementation detail of the type-checker and
is not fundamental, but for now, it's yet another reason to prefer writing mlis.

```ocaml
(* in a.ml; a.mli is missing *)
let f1 ~foo:_ = ()
let f2 x = f1 ~foo:(Some x) (* [Some x] is heap allocated *)
```

## Regions

Every local allocation takes places inside a _region_, which is a block of code
(usually a function body, but see below).  At the end of a region, all of its
local allocations are freed.

Regions may nest, for instance when one function calls another. Local
allocations always occur in the innermost (most recent) region.

We say that a value _escapes_ a region if it is still referenced beyond the end
of that region. The job of the type-checker is to ensure that locally allocated
values do not escape the region they were allocated in.

"Region" is a wider concept than "scope", and locally-allocated variables can
outlive their scope. For example:

```ocaml
let f () =
  let local_ counter =
    let local_ r = ref 42 in
    incr r;
    r
  in
  ...
```

The locally-allocated reference `r` is allocated inside the definition of
`counter`. This value outlives the scope of `r` (it is bound to the variable
`counter` and may later be used in the code marked `...`). However, the
type-checker ensures that it does not outlive the region in which it is
allocated, which is the entire body of `f`.

As well as function bodies, a region is also placed around:

  - Loop bodies (`while` and `for`)
  - Lazy expressions (`lazy ...`)
  - Module bindings (`let x = ...` at module level, including in sub-modules)

Module bindings are wrapped in regions to enforce the rule (as mentioned above)
that modules never contain locally-allocated values.

Additionally, it is possible to write functions that do *not* have
a region around their body, which is useful to write functions that
return locally-allocated values. See "Local-returning functions" below.

### Runtime behavior

At runtime, local allocations do not allocate on the C stack, but on a
separately-allocated stack that follows the same layout as the OCaml
minor heap. In particular, this allows local-returning functions
without the need to copy returned values.

The beginning of a region records the stack pointer of this local
stack, and the end of the region resets the stack pointer to this
value.


### Variables and regions

To spot escaping local allocations, the type checker internally tracks whether
each variable is:

  - **Global**: must be a global value. These variables are allowed to freely
    cross region boundaries, as normal OCaml values.

  - **Local**: may be a locally-allocated value. These variables are restricted
    from crossing region boundaries.

As described above, whether a given variable is global or local is inferred by
the type-checker, although the `local_` keyword may be used to specify it.

Additionally, local variables are further subdivided into two cases:

  - **Outer-region local**: may be a locally-allocated value, but only from an outer
    region and not from the current one.

  - **Any-region local**: may be a locally-allocated value, even one allocated
    during the current region.

For instance:

```ocaml
let f () =
  let local_ outer = ref 42 in
  let g () =
    let local_ inner = ref 42 in
    ??
  in
  ...
```

At the point marked `??` inside `g`, both `outer` and `inner` are
locally-allocated values. However, only `inner` is any-region local, having been
allocated in `g`'s region. The value `outer` is instead outer-region local: it
is locally allocated but from a region other than `g`'s own.

So, if we replace `??` with `inner`, we see an error:

    Error: This local value escapes its region

However, if we replace `??` with `outer`, the compiler will accept it: the
value `outer`, while locally allocated, was definitely not locally allocated
_during g_, and there is therefore no problem allowing it to escape `g`'s
region.

(This is quite subtle, and there is an additional wrinkle: how does the
compiler know that it is safe to still refer to `outer` from within the closure
`g`? See "Closures" below for more details)


## Function types and local arguments

Function types now accept the `local_` keyword in both argument and return
positions, leading to four distinct types of function:

    a -> b
    local_ a -> b
    a -> local_ b
    local_ a -> local_ b

In argument positions, `local_` indicates that the function may be passed
locally-allocated values. As always, the local_ keyword does not *require*
a locally-allocated value, and you may pass global values to such functions. In
effect, a function of type `local_ a -> b` is a function accepting `a`
and returning `b` that promises not to capture any reference to its argument.

In return positions, `local_` indicates that the function may return
locally-allocated values. A function of type `local_ a -> local_ b` promises
not to capture any reference to its argument except possibly in its return
value.

A function with a local argument can be defined by annotating the argument as
`local_`:

```ocaml
let f (local_ x) = ...
```

Inside the definition of `f`, the argument `x` is outer-region local: that is,
while it may be locally allocated, it is known not to have been allocated during
`f` itself, and thus may safely be returned from `f`. For example:

```ocaml
# let f1 (local_ x : int list) = [1; 2; 3]
val f1 : local_ int list -> int list

# let f2 (local_ x : int list) = x
val f2 : local_ int list -> local_ int list

# let f3 (local_ x : int list) = (42 :: x)
                                        ^
Error: This value escapes its region
```

In the above, `f1` returns a global `int list`, while `f2` returns a local one.
`f2` is allowed to return the local value `x` despite the ending of the
function's region, because the value `x` is known to come from outside that
region.

In contrast, `f3` is an error. The value `42 :: x` must be locally allocated (as
it refers to a local value `x`), and it is locally allocated from within the
region of `f3`. When this region ends, the any-region local value `42 :: x` is
not allowed to escape it.

It is possible to write functions like `f3` that return
locally-allocated values, but this requires explicit annotation, as it
would otherwise be easy to do by mistake.  See "Local-returning
functions" below.

Like local variables, inference can determine whether function arguments are
local. However, note that for arguments of exported functions to be local, the
`local_` keyword must appear in their declarations in the corresponding `.mli`
file.


## Closures

Like most other values, closures can be locally allocated. In particular, this
happens when a closure closes over local values from an outer scope: since
global values cannot refer to local values, all such closures _must_ be locally
allocated.

Consider again the example from "Variables and regions" above:

```ocaml
let f () =
  let local_ outer = ref 42 in
  let g () =
    let local_ inner = ref 42 in
    outer
  in
  ...
```

Here, since `g` refers to the local value `outer`, the closure `g` must itself
be locally allocated. (As always, this is deduced by inference, and an explicit
`local_` annotation on `g` is not needed).

This then means that `g` is not allowed to escape its region, i.e. the body of
`f`. `f` may call `g` but may not return the closure. This guarantees that `g`
will only run before `f` has ended, which is what makes it safe to refer to
`outer` from within `g`.

Higher-order functions should usually mark their function arguments as
`local_`, to allow local closures to be passed in. For instance, consider the
following function for computing the length of a list:

```ocaml
let length xs =
  let local_ count = ref 0 in
  List.iter xs ~f:(fun () -> incr count);
  !count
```

With the standard type of `List.iter`, this results in a type error:

```ocaml
  List.iter xs ~f:(fun () -> incr count);
                                  ^^^^^
Error: The value count is local, so cannot be used inside a closure that might escape
```

The standard type of `List.iter` is as follows:

```ocaml
val iter : 'a list -> f:('a -> unit) -> unit
```

This type places no restrictions on the use of `f`, allowing `iter` to capture
or otherwise leak its argument `f`. It is therefore not safe to pass a local
closure to such a function, hence the error.

Instead, `List.iter` and similar functions should be given the following type:

```ocaml
val iter : 'a list -> f:local_ ('a -> unit) -> unit
```

This type carries the additional promise that `iter` does not capture its `f`
argument, allowing local closures to be passed. With this type, the above
`length` function is accepted.

Note that the function `f` here _is_ allowed to capture its argument,
and there are no restrictions on what may be done with the list
elements themselves. To specify that `f` may _not_ capture its
argument, the type of iter would have to be:

```ocaml
val iter : 'a list -> f:local_ (local_ 'a -> unit) -> unit
```

The two occurrences of `local_` are independent: the first is a promise
by `iter` not to capture `f`, while the second is a requirement by
`iter` to be given an `f` that does not itself capture.



## Tail calls

Usually, a function's region lasts for the entire body of that function,
cleaning up local allocations at the very end. This story gets more complicated
if the function ends in a tail call, however, as such functions need to clean
up their stack frame before the tail call in order to ensure that
tail-recursive loops use only constant space.

Therefore, when a function ends in a tail call, that function's region ends:

  - after the arguments to the tail call have been evaluated

  - but before control is transferred to the callee.

This early ending of the region introduces some restrictions, as values used in
tail calls then count as escaping the region. In particular, any-region local values
may not be passed to tail calls:

```ocaml
let f1 () =
  let local_ r = ref 42 in
  some_func r
            ^
Error: This local value escapes its region
  Hint: This argument cannot be local, because this is a tail call
```

and any-region local closures may not be tail-called:

```ocaml
let f2 () =
  let local_ g () = 42 in
  g ()
  ^
Error: This local value escapes its region
  Hint: This function cannot be local, because this is a tail call
```

In both cases, if tail recursion is not necessary, then the issue can be
resolved by moving the call so that it is not syntactically a tail call:

```ocaml
let f1 () =
  let local_ r = ref 42 in
  let res = some_func r in
  res

let f2 () =
  let local_ g () = 42 in
  let res = g () in
  res
```

or by annotating the call with the `[@nontail]` attribute, that
prevents it from being a tail call:

```ocaml
let f1 () =
  let local_ r = ref 42 in
  some_func r [@nontail]

let f2 () =
  let local_ g () = 42 in
  g () [@nontail]
```


This change means that the locally allocated values (`r` and `g`)
will not be freed until after the call has returned.

Note that values which are outer-region local rather than any-region local (that
is, local values that were passed into this region from outside) may be used in
tail calls, as the early closing of the region does not affect them:

```ocaml
let f3 (local_ x) =
  some_func x
```

Here, even though the region of `f3` ends before the call to `some_func`, the
value `x` remains available.



## Local-returning functions

The region around the body of a function prevents local allocations inside that
function from escaping. Occasionally, it is useful to write a function that
allows local allocations to escape, which can be done by explicitly marking
such functions.

This is useful particularly for constructor functions of abstract types. For
instance, consider this code that uses an `int ref` as a counter:

```ocaml
let f () =
  let counter = ref 0 in
  ...
  let n = !counter in
  incr counter;
  ...
```

Here, inference will detect that `counter` does not escape and will allocate
the reference locally. However, this changes if we try to abstract out
`counter` to its own module:

```ocaml
module Counter = struct
  type t = int ref

  let make () =
    ref 0

  let next c =
    let x = !c in
    incr c;
    x
end

let f () =
  let counter = Counter.make () in
  ...
  let n = Counter.next counter in
  ...
```

In this code, the counter will *not* be allocated locally. The reason is the
`Counter.make` function: the allocation of `ref 0` escapes the region of
`Counter.make`, and the compiler will therefore not allow it to be locally
allocated. This remains the case no matter how many local_ annotations we write
inside `f`: the issue is the definition of `make`, not its uses.

To allow the counter to be locally allocated, we need to specify that
`Counter.make` may return local allocations. This can be done by wrapping the
entire body of `make` with the `local_` keyword:

```ocaml
let make () = local_
  ref 0
```

The `local_` keyword around a function body like this specifies not only that
the allocation of the `ref` should be local, but more importantly that the
function `make` *should not have its own region*.

Instead, local allocations during `make` are considered part of `f`s region,
and will only be cleaned up when that region ends. Local allocations are
allocated as always in the nearest enclosing region. However if the current
function is a local-returning function, then the nearest enclosing region will
be the caller's (or that of the caller's caller, etc., if the caller is also
local-returning).

## Exclave
In the previous section, we discussed that a function can return local values
without having its own region. Consequently, it operates within the caller's
region. This approach, however, has certain disadvantages. Consider the
following example:

```
let f (local_ x) = local_
  let local_ y = (complex computation on x) in
  if y then local_ None
  else local_ (Some x)
```

The function `f` allocates memory within the caller's region to store
intermediate and temporary data for the complex computation. This allocation
remains in the region even after `f` returns and is released only when the
program exits the caller's region. To allow temporary allocations to be released
upon the function's return, we can rewrite the example as follows:


```
let f (local_ x) =
  let local_ y = (complex computation on x) in
  if y then exclave_ None
  else exclave_ Some x
```

The new primitive `exclave_` terminates the current region early and executes
the subsequent code in the outer region. In this example, the function `f`
has a region where the allocation for the complex computation occurs.
This region is terminated by `exclave_`, releasing all temporary allocations.
Both `None` and `Some x` are considered "local" relative to the
outer region and are allowed to escape. In summary, we have temporary
allocations on the stack that are promptly released and result allocations on
the stack that can escape.


Here is another example in which the stack usage can be improved asymptotically
by applying `exclave_`:


```
let rec maybe_length p l = local_
  match l with
  | [] -> Some 0
  | x :: xs ->
      if p x then None
      else begin
        match maybe_length p xs with
        | None -> None
        | Some count ->
            Some (count + 1)
      end
```

This function is intended to have the type:

```
val maybe_length : ('a -> bool) -> 'a list -> local_ int option
```

This function computes the length of the list. The predicate can return `false`,
in which case the result of the entire function would be `None`. It is designed
not to allocate heap memory, instead using the stack for all `Some` allocations.
However, it will currently use O(N) stack space because all allocations occur in
the original caller's stack frame. To improve its space usage, we remove the
`local_` annotation (so the function has its own region), and wrap `Some
(count + 1)` inside `exclave_` to release the region before the allocation:

```ocaml
let rec maybe_length p l =
  match l with
  | [] -> Some 0
  | x :: xs ->
      if p x then None
      else begin
        match maybe_length p xs with
        | None -> None
        | Some count ->
            exclave_ Some (count + 1)
      end
```

Now the function uses O(1) stack space.


`exclave_` terminates the current region, so local values from that region
cannot be used inside `exclave_`. For example, the following code produces an
error because `x` would escape its region:


```
  let local_ x = "hello" in
  exclave_ (
    let local_ y = "world" in
    local_ (x ^ y)
  )
```

Similarly, `exclave_` can only appear at the tail position of a region since
one cannot re-enter a terminated region. The following code is an error for this
reason:


```
  let local_ x = "hello" in
  exclave_ (
    let local_ y = "world" in
    ()
  );
  local_ (x ^ "world")
```
## Records and mutability

For any given variable, the type-checker checks only whether that variable is
local or global, and generally does not separately track parts of the variable.
For instance, the following code yields an error, even though `x` and `y` are
both global:

```ocaml
let f () =
  let local_ packed = (x, y) in
  let x', y' = packed in
  x'
```

Here, the `packed` values is treated as local, and the type-checker then
conservatively assumes that `x'` and `y'` may also be local (since they are
extracted from `packed`), and so cannot safely be returned.

Similarly, a variable `local_ x` of type `string list` means a local
list of local strings, and none of these strings can be safely
returned from a function like `f`.

This can be overridden for record types, by annotating some fields with
`global_`:

```ocaml
type ('a, 'b) t = { global_ foo : 'a; bar: 'b }

let f () =
  let local_ packed = {foo=x; bar=y} in
  let {foo; bar} = packed in
  foo
```

Here, the `foo` field of any value of type `_ t` is always known to be global,
and so can be returned from a function. When constructing such a record, the
`foo` field must therefore be a global value, so trying to fill it with a local
value will result in an escape error, even if the record being constructed is
itself local.

In particular, by defining:

```ocaml
type 'a glob = { global_ contents: 'a } [@@unboxed]
```

then a variable `local_ x` of type `string glob list` is a local list
of global strings, and while the list itself cannot be returned out of
a region, the `contents` field of any of its elements can.

The same overriding can be used on constructor arguments. To imitate the example
for record fields:

    type ('a, 'b) t = Foo of global_ 'a * 'b

    let f () =
      let local_ packed = Foo (x, y) in
      match packed with
      | Foo (foo, bar) -> foo

### Mutability

Mutable fields are always `global_`, including array elements. That is, while
you may create local `ref`s or arrays, their contents must always be global.

This restriction may be lifted somewhat in future: the tricky part is that
naively permitting mutability might allow an older local mutable value to be
mutated to point to a younger one, creating a dangling reference to an escaping
value when the younger one's region ends.


## Curried functions

The function type constructor in OCaml is right-associative, so that these are
equal types:

```ocaml
string -> string -> string
string -> (string -> string)
```

These both describe a two-argument function which is curried, and therefore may
be partially applied to the first argument, yielding a closure that accepts the
second.

The situation is more complicated when `local_` is involved. The following two
types are *not* equivalent:

```ocaml
local_ string -> string -> string
local_ string -> (string -> string)
```

The former is a two-argument function which accepts as its first argument
a local string. Like all two-argument functions, it may be partially applied to
a single argument yielding a closure that accepts the second. However, since
this closure closes over the first local argument, it must necessarily be local
itself. Thus, if applied to a single argument, this function in fact returns
a _local_ closure, making its type equal to the following:

```ocaml
local_ string -> local_ (string -> string)
```

By contrast, the type `local_ string -> (string -> string)` means a function
that accepts a local string but returns a global function. Necessarily, this
global function cannot refer to the local string that was passed, so this
cannot be an ordinary two-argument function. (It could be something like `fun
s -> print s; fun x -> x`, however)

In general, in a curried function type `... -> ... -> ...` (without
parentheses), then after the first use of `local_`, all arrow types except the
last will implicitly be given `local_` return types, enabling the expected
partial application behavior.

Finally, this transformation applies also to types marked with the `local_`
keyword. For instance, the following type:

```ocaml
local_ (a -> b -> c -> d) -> e -> f -> g
```

is read as:

```ocaml
local_ (a -> local_ (b -> local_ (c -> d))) -> local_ (e -> local_ (f -> g))
```

Note the implicit `local_` both in the returned `e -> f` closure (as described
above), and also in the type of the `b -> c` argument. The propagation of
`local_` into the function argument is necessary to allow a locally-allocated
function (which would have type `a -> local_ (b -> local_ (c -> d))`) to be
passed as an argument. Functions are different than other types in that, because
of currying, a locally-allocated function has a different type than a
globally-allocated one.


### Currying of local closures

Suppose we are inside the definition of a function, and there is in scope
a local value `counter` of type `int ref`. Then of the following two
seemingly-identical definitions, the first is accepted and the second is
rejected:

```ocaml
let local_ f : int -> int -> int = fun a b -> a + b + !counter in
...

let f : int -> int -> int = local_ fun a b -> a + b + !counter in
...
```

Both define a closure which accepts two integers and returns an integer. The
closure must be local, since it refers to the local value `counter`. In the
former definition, the type of the function appears under the `local_` keyword,
as as described above is interpreted as:

```ocaml
int -> local_ (int -> int)
```

This is the correct type for this function: if we partially apply it to
a single argument, the resulting closure will still be local, as it refers to
the original function which refers to `counter`. By contrast, in the latter
definition the type of the function is outside the `local_` keyword as is
interpreted as normal as:

```ocaml
int -> (int -> int)
```

This is not the correct type for this function: it states that partially
applying it to a single argument will yield a global closure, which is not the
case here. For this reason, this version is rejected. It would be accepted if
written as follows:

```ocaml
let f : int -> local_ (int -> int) = local_ fun a b -> a + b + !counter in
...
```


## Special case typing of tuple matching

As mentioned above, the type-checker generally does not separately track
the local or global status of parts of a value, but rather tracks this
only once per variable or expression. There is one exception to this
rule, as follows.

In OCaml, it is possible to simultaneously match on multiple values:

```ocaml
match x, y, z with
| p, q, r -> ...
```

There is in fact no special syntax for this: as parentheses are
optional in tuples, the above is actually a match on a single value,
the tuple `(x, y, z)`, against a single pattern, the pattern `(p, q,
r)`.

Applying the usual rule that an expression is either treated as
entirely local or entirely global would mean that `p`, `q` and `r`
would all be local if any of `x`, `y` and `z` are. This is
counter-intuitive, as the syntax above is usually thought of as a
multiple-value match, rather than a match on a single tuple value. For
this reason, the type-checker independently tracks whether the parts of
this tuple are local or global.

The same logic applies to simultaneous binding of multiple values:

```ocaml
let a, b, c =
  ...
  x, y, z
```

Again, there is no actual syntax for this in OCaml: that's a binding
of the single value `(x, y, z)` against the single pattern `(a, b,
c)`. Since it's usually thought of as the simultaneous binding of
several variables, the type-checker treats it as such rather than
making all of `a`,`b` and `c` local if any of `x`, `y` and `z` are.


## Primitive definitions

Allocations in OCaml functions must either be local or global, as these are
compiled separately. A different option is available for `%`-primitives exported
by the stdlib, however, as these are guaranteed to be inlined at every use
site. Unlike ordinary functions, these primitives may be used to make both
local and global allocations, which is why `ref` worked for both local and
global in various examples above.

In the interface for the stdlib (and as re-exported by Base), this feature is
enabled by use of the `[@local_opt]` annotation on `external` declarations. For
example, we have the following:

```ocaml
external id : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
```

This declaration means that `id` can have either of the following types:

```ocaml
id : local_ 'a -> local_ a
id : 'a -> 'a
```

Notice that the two `[@local_opt]`s act in unison: either both `local_`s are
present or neither is. This allows for a limited form of mode-polymorphism for
`external`s (only). Nothing checks that the locality ascriptions are sound,
though, so use this feature with much caution. In the case of `id`, all is well,
but if the two `[@local_opt]`s did not act in unison (that is, they varied
independently), it would not be: `id : local_ 'a -> 'a` allows a local value to
escape.
