---
layout: documentation-page
collectionName: Stack allocation
title: Reference
---
# Stack Allocations Reference

The goal of this document is to be a reasonably complete reference to stack
allocations in OCaml. For a gentler introduction, see [the
introduction](../intro).

The stack allocations language extension allows the compiler to
stack-allocate some values, placing them on a stack rather than the
garbage collected heap. Instead of waiting for the next GC, the memory
used by stack-allocated values is reclaimed when their stack frame
is popped, and can be immediately reused. Whether the compiler
stack-allocates certain values is inferred or controlled from new keywords
`stack_` and `local_`, whose effects are explained below.

## Stack allocation

The `stack_` keyword may be placed on an allocation to indicate that
it should be stack-allocated:

```ocaml
let abc = stack_ (42, 24) in
...
```

Here, the tuple cell will be stack-allocated. The `stack_` keyword works shallowly: it
only forces the immediately following allocation to be on stack. In the following
example, the outer tuple is guaranteed to be on stack, while the inner one is not
(although likely to be due to optimization).
```ocaml
let abc = stack_ (42, (24, 42)) in
...
```

Placing `stack_` on an expression that is not an allocation is meaningless and
causes a type error:
```ocaml
let f = ref (stack_ `Foo)
                    ^^^^
Error: This expression is not an allocation site.
```

Most OCaml types can be stack-allocated, including records, variants,
polymorphic variants, closures, boxed numbers and strings. However, certain
values cannot be stack-allocated, and will always be on the GC heap,
including:

  - Modules (including first-class modules)

  - Exceptions
    (Technically, values of type `exn` can be locally allocated, but only global
    ones may be raised)

  - Classes and Objects

### Runtime behavior

At runtime, stack allocations do not take place on the function call stack, but on a
separately-allocated stack that follows the same layout as the OCaml
minor heap. In particular, this enables local-returning functions
without the need to copy returned values. See "Use `exclave_` to return a local value"
below for more details.

The runtime records the stack pointer when entering a new stack frame,
and leaving that stack frame resets the stack pointer to that value.

## Regions

Every stack allocation takes place inside a stack frame and is freed when the
stack frame is reclaimed. For this to be safe, stack-allocated values cannot be used
after their stack frame is freed. This property is guaranteed at
compile-time by the type checker as follows.

A function body defines a _region_: a contiguous stretch of code, all of whose
stack allocations go into the same stack frame.
A stack-allocated value lives in the region it's allocated in. We say
the value is _local_ to the region it lives in. A heap-allocated value is
_global_.

We say that a value _escapes_ a region if it is still referenced beyond the end
of that region. The type-checker guarantees that local values do not escape
their region. Since stack-allocated values live in the region representing the
stack frame containing the allocation, they are guaranteed to be never
referenced after their stack frame is freed.

Global values can escape all regions. As a result, they may not reference local
values, since that will make the local values escape regions, which breaks the
guarantee. Local values may reference global ones.

### Weakening

A global value can be weakened to local, effectively "forgetting" that it
can escape regions. For instance, if there is a global `x : int list` in
scope, then this is allowed:

```ocaml
let l = if n > 0 then stack_ (n :: x) else x in
...
```

Here, if `n > 0`, then `l` will be a stack-allocated cons cell and thus local.
However, if `n <= 0`, then `l` will be `x`, which is global. In that second case, `x` is
implicitly weakened to local (because both branches of an `if` must have the
same locality), making the whole
expression local.

You can also use the `local_` keyword to explicitly weaken a value to local. For
example:

```ocaml
let l = local_ if n > 0 then n :: x else x in
...
```

The `local_` keyword doesn't force stack allocation. However, it does weaken `l`
to local, which prevents `l` from escaping the current region, and as a result
the compiler will optimize `n :: x` to be stack-allocated in the current
region. However, users may wish to use `stack_` to ensure stack allocation,
as refactoring code can make an allocation that was previously on the stack
silently move to the heap.

### Region vs. Scope

*Region* is a wider concept than *scope*, and stack-allocated variables can
outlive their scope. For example:

```ocaml
let f () =
  let counter =
    let r = stack_ (ref 42) in
    incr r;
    r
  in
  ...
```

The stack-allocated reference `r` is allocated inside the definition of
`counter`. This value outlives the scope of `r` (it is bound to the variable
`counter` and may later be used in the code marked `...`). However, the
type-checker ensures that it does not outlive the region that it lives in,
which is the entire body of `f`.

### Other regions

As well as function bodies, a region is also placed around:

  - Loop bodies (`while` and `for`)
  - Lazy expressions (`lazy ...`)
  - Module bindings (`let x = ...;;` at module level, including in sub-modules)

Module bindings are wrapped in regions to enforce the rule (as mentioned above)
that modules never contain local values.

Additionally, it is possible to write functions whose region ends before the
function does, which is useful to write functions that return
stack-allocated values. See "Use `exclave_` to return a local value" below.

### Nested regions

Regions nest, for instance when defining a local function. Stack-allocated
values always live in the inner-most region, since at runtime one can
allocate only in the current stack frame. There is an outermost region,
including an entire file. This region never ends and is where global (heap-allocated)
values live.

One subtlety is that we wish to treat `local` variables from the current region
differently than `local` variables from an enclosing region. Local from outside
the current region is called *outer-local*.

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

At the point marked `??` inside `g`, both `outer` and `inner` are local values,
but they live in different regions: `inner` lives in the `g` region, while `outer`
lives in the `f` region and thus is outer-local.

So, if we replace `??` with `inner`, we see an error:

    Error: This local value escapes its region

However, if we replace `??` with `outer`, the compiler will accept it: the
value `outer`, while being local, was definitely not local to the region of
`g`, and there is therefore no problem allowing it to escape the `g` region.

(This is quite subtle, and there is an additional wrinkle: how does the
compiler know that it is safe to still refer to `outer` from within the closure
`g`? See "Closures" below for more details)

This situation also arises with local parameters. For example:

```ocaml
let f (local_ x) =
  let local_ y = 3 :: x in
  ??
```

Both `x` and `y` are local and cannot, in general, escape a region. However, filling `??`
in with `x` (but not `y`) is allowed. This is because we know that `x` lives outside of
the `f` region and therefore will continue to exist after `f` ends. In contrast, `y` is a cons cell
in the `f` region, which will be destroyed after `f` ends.

## Inference

In fact, allocations will be on stack even without `stack_`, if it is safe to do
so. The presence of the keyword on an allocation only affects what
happens if the allocated value escapes (e.g. is stored into a global hash table)
and therefore cannot be stack-allocated. With the keyword, an error
will be reported, while without the keyword the allocations will occur
on the GC heap as usual. Similarly, whether a value is global or local (and
hence whether certain allocation can be optimized to be on stack) is inferred by
the type-checker, although the `local_` keyword may be used to specify it.

Inference does not cross file boundaries. If local annotations subject to
inference appear in the type of a module (e.g. since they can appear in
function types, see below) then inference will resolve them according to what
appears in the mli. If there is no mli file, then inference will always
choose global for anything that can be accessed from another file.

Local annotations (or the lack thereof) in the mli don't affect inference
within the ml. In the below example, the `~foo` parameter is inferred to
be local internally to `a.ml`, so `foo:(Some x)` can be stack-allocated.

```ocaml
(* in a.mli *)
val f1 : foo:local_ int option -> unit
val f2 : int -> unit

(* in a.ml *)
let f1 ~foo:_ = ()
let f2 x = f1 ~foo:(Some x) (* [Some x] is stack allocated *)
```

<!-- See Note [Inference affects allocation in mli-less files] in [testsuite/tests/typing-local/alloc_arg_with_mli.ml]
     in the flambda-backend Git repo. The ensuing paragraph is related to that
     note; we can remove this comment when the note is resolved.
-->
However, a missing mli *does* affect inference within the ml file. As a conservative
rule of thumb, function arguments in an mli-less file will default to global
unless the function parameter or argument is annotated with `local_`. This is
due to an implementation detail of the type-checker and is not fundamental, but
for now, it's yet another reason to prefer writing mlis.

```ocaml
(* in a.ml; a.mli is missing *)
let f1 ~foo:_ = ()
let f2 x = f1 ~foo:(Some x) (* [Some x] is heap allocated *)
```

## Function types and local arguments

Function types now accept the `local_` keyword in both argument and return
positions, leading to four distinct types of function:

    a -> b
    local_ a -> b
    a -> local_ b
    local_ a -> local_ b

In all cases, the `local_` annotation means "local to the call site's surrounding
region" , or equivalently "outer-local to the function's region".

In argument positions, `local_` indicates that the function may be passed
local values. As always, the `local_` keyword does not *require*
a local value, and you may pass global values to such functions. (This is an
example of the fact that global values can always be weakened to local ones.) In
effect, a function of type `local_ a -> b` is a function accepting `a`
and returning `b` that promises not to capture any reference to its argument.

In return positions, `local_` indicates that the function may return values that
are local (See "Use `exclave_` to return a local value" below). A function of
type `local_ a -> local_ b` promises not to capture any reference to its
argument except possibly in its return value.

A function with a local argument can be defined by annotating the argument as
`local_`:

```ocaml
let f (local_ x) = ...
```

As we saw above, inside the definition of `f`, the argument `x` is outer-local: that is,
while it may be stack-allocated, it is known not to have been allocated during
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

In contrast, `f3` is an error. The value `42 :: x` refers to a local value `x`,
which means it cannot be global. Therefore, it must be stack-allocated, and it
 is allocated within the region of `f3`. When this region ends, the any-region
local value `42 :: x` is not allowed to escape it.

It is possible to write functions like `f3` that return stack-allocated
values, but this requires an explicit annotation, as it could otherwise be done
unintentionally.  See "Use `exclave_` to return a local value" below.

Like local variables, inference can determine whether function arguments are
local. However, note that for arguments of exported functions to be local, the
`local_` keyword must appear in their declarations in the corresponding mli
file.


## Closures

Like most other values, closures can be stack-allocated. In particular, this
happens when a closure closes over local values: since global values cannot
refer to local values, all such closures cannot be global and _must_ be
stack-allocated.

Consider again the example from "Variables and regions" above:

```ocaml
let f () =
  let outer = stack_ (ref 42) in
  let g () =
    let inner = stack_ (ref 42) in
    outer
  in
  ...
```

Here, since `g` refers to the local value `outer`, the closure `g` must itself
be stack-allocated. (As always, this is deduced by inference, and an explicit
`stack_` annotation on `g` is not needed.)

This then means that `g` is not allowed to escape its region (the body of
`f`). `f` may call `g` but may not return the closure. This guarantees that `g`
will only run before `f` has ended, which is what makes it safe to refer to
`outer` from within `g`.

Higher-order functions should usually mark their function arguments as
`local_`, to allow local closures to be passed in. For instance, consider the
following function for computing the length of a list:

```ocaml
let length xs =
  let count = stack_ (ref 0) in
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
and local values are available until at the very end. This story gets more complicated
if the function ends in a tail call, however, as such functions need to clean
up their stack frame before the tail call in order to ensure that
tail-recursive loops use only constant space.

Therefore, when a function ends in a tail call, that function's region ends:

  - after the arguments to the tail call have been evaluated

  - but before control is transferred to the callee.

This early ending of the region introduces some restrictions, as values used in
tail calls then count as escaping the region. In particular, local
values may not be passed to tail calls:

```ocaml
let f1 () =
  let r = stack_ (ref 42) in
  some_func r
            ^
Error: This local value escapes its region
  Hint: This argument cannot be local, because this is a tail call
```

and local closures may not be tail-called:

```ocaml
let f2 () =
  let g = stack_ (fun () -> 42) in
  g ()
  ^
Error: This local value escapes its region
  Hint: This function cannot be local, because this is a tail call
```

In both cases, if tail recursion is not necessary, then the issue can be
resolved by moving the call so that it is not syntactically a tail call:

```ocaml
let f1 () =
  let r = stack_ (ref 42) in
  let res = some_func r in
  res

let f2 () =
  let g = stack_ (fun () -> 42) in
  let res = g () in
  res
```

or by annotating the call with the `[@nontail]` attribute, which
prevents it from being a tail call:

```ocaml
let f1 () =
  let r = stack_ (ref 42) in
  some_func r [@nontail]

let f2 () =
  let g = stack_ (fun () -> 42) in
  g () [@nontail]
```

These changes make the local values (`r` and `g`) stay available until after
the call has returned.

Note that values which are outer-local (see "Nested regions") may be used in
tail calls, as the early closing of the region does not affect them:

```ocaml
let f3 (local_ x) =
  some_func x
```

Here, even though the region of `f3` ends before the call to `some_func`, the
value `x` remains available.

## Use `exclave_` to return a local value

The region around the body of a function prevents local values inside that
function from escaping. Occasionally, it is useful to write a function that
allocates and returns a value in the caller's region. For instance, consider
this code that uses an `int ref` as a counter:

```ocaml
let f () =
  let counter = ref 0 in
  ...
  let n = !counter in
  incr counter;
  ...
```

Here, inference will detect that `counter` does not escape and will stack-allocate
the reference (assuming that `incr` takes its argument `local_`ly). However, this changes if we try to abstract out
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

In this code, the counter will *not* be stack-allocated. The reason is the
`Counter.make` function: the allocation of `ref 0` escapes the region of
`Counter.make`, and the compiler will therefore not allow it to be
stack-allocated. This remains the case no matter how many `local_` annotations we
write inside `f`: the issue is the definition of `make`, not its uses.

To allow the counter to be stack-allocated, we need to make `Counter.make` end
its region early so that it can allocate its return value in the caller's
region. This can be done with `exclave_`:

```ocaml
let make () = exclave_
  ref 0
```

The keyword `exclave_` terminates the current region and executes the subsequent
code in the outer region. Therefore, `ref 0` is executed in the `f` region, which
allows its stack-allocation. The allocation will only be cleaned up when the
region of `f` ends.

## Delaying exclaves

In the previous section, the example function exits its own region immediately,
which allows allocating and returning in the caller's region. This approach,
however, has certain disadvantages. Consider the following example:

```ocaml
let f (local_ x) = exclave_
  let local_ y = (complex computation on x) in
  if y then None
  else (Some x)
```
The function `f` allocates memory within the caller's region to store
intermediate and temporary data for the complex computation. This allocation
remains in the region even after `f` returns and is released only when the
program exits the caller's region. To allow temporary allocations to be released
upon the function's return, we delay `exclave_` as follows:

```ocaml
let f (local_ x) =
  let local_ y = (complex computation on x) in
  if y then exclave_ None
  else exclave_ Some x
```

In this example, the function `f` has a region where the allocation for the
complex computation occurs. This region is terminated by `exclave_`, releasing
all temporary allocations. Both `None` and `Some x` are allocated in the
caller's stack frame and are allowed to be returned. In summary, the
temporary allocations in the `f` region are promptly released, and the result
allocated in the caller's region is returned.

Here is another example in which the stack usage can be improved asymptotically
by delaying `exclave_`:
```ocaml
let rec maybe_length p l = exclave_
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

```ocaml
val maybe_length : ('a -> bool) -> 'a list -> local_ int option
```
It is designed not to allocate heap memory by using the stack for all `Some`
allocations. However, it will currently use O(N) stack space because all
allocations occur in the original caller's stack frame. To improve its space
usage, we delay the `exclave_` annotation until returning the result:
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
error:
```ocaml
  let local_ x = "hello" in
  exclave_ (
    let local_ y = "world" in
    local_ (x ^ y)
  )
```

Similarly, `exclave_` can only appear at the tail position of a region since one
cannot re-enter a terminated region. The following code is an error for this
reason:
```ocaml
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
  let packed = stack_ (x, y) in
  let x', y' = packed in
  x'
```

Here, the `packed` values is treated as local, and the type-checker then
conservatively assumes that `x'` and `y'` may also be local (since they are
extracted from `packed`), and so cannot safely be returned.

Similarly, a local value of type `string list` means a local
list of local strings, and none of these strings can be safely
returned from a function like `f`.

This can be overridden for record types, by annotating some fields with
`global_`:

```ocaml
type ('a, 'b) t = { global_ foo : 'a; bar: 'b }

let f () =
  let packed = stack_ {foo=x; bar=y} in
  let {foo; bar} = packed in
  foo
```

Here, the `foo` field of any value of type `_ t` is always known to be global,
and so can be returned from a function. When constructing such a record, the
`foo` field must therefore be assigned a global value, so trying to fill it with a local
value will result in an escape error, even if the record being constructed is
itself local.

In particular, by defining:

```ocaml
type 'a t = { global_ global : 'a } [@@unboxed]
```

then a local value of type `string t list` is a local list of global
strings, and while the list itself cannot be returned out of a region, the
global field of any of its elements can. For convenience, `base` provides
this as the type `Modes.Global.t`.

The same overriding can be used on constructor arguments. To imitate the example
for record fields:
```ocaml
type ('a, 'b) t = Foo of global_ 'a * 'b

let f () =
  let packed = stack_ (Foo (x, y)) in
  match packed with
  | Foo (foo, bar) -> foo
```

### Mutability

Mutable fields are always `global_`, including array elements. That is, while
you may create local `ref`s or arrays, their contents must always be global.
Should you need to put a local value into one of these
places, you may want to check out
[`ppx_globalize`](https://github.com/janestreet/ppx_globalize).

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
`local_` into the function argument is necessary to allow a stack-allocated
function (which would have type `a -> local_ (b -> local_ (c -> d))`) to be
passed as an argument. Functions are different than other types in that, because
of currying, a stack-allocated function has a different type than a
heap-allocated one.

### Currying of local closures

Suppose we are inside the definition of a function, and there is in scope
a local value `counter` of type `int ref`. Then of the following two
seemingly-identical definitions, the first is accepted and the second is
rejected:

```ocaml
let local_ f : int -> int -> int = fun a b -> a + b + !counter in
...

let g : int -> int -> int = stack_ fun a b -> a + b + !counter in
...
```

Both define a closure which accepts two integers and returns an integer. The
closure must be local, since it refers to the local value `counter`. In the
definition of `f`, the type of the function appears under the `local_` keyword,
and as described above is interpreted as:

```ocaml
int -> local_ (int -> int)
```

This is the correct type for this function: if we partially apply it to
a single argument, the resulting closure will still be local, as it refers to
the original function which refers to `counter`. By contrast, in the
definition of `g`, the type of the function is outside the `stack_` keyword as is
interpreted as normal as:

```ocaml
int -> (int -> int)
```

This is not the correct type for this function: it states that partially
applying it to a single argument will yield a global closure, which is not the
case here. For this reason, this version is rejected. It would be accepted if
written as follows:

```ocaml
let g : int -> local_ (int -> int) = stack_ fun a b -> a + b + !counter in
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


## Primitive declarations
### Mode polymorphism

A limited form of mode polymorphism is available for primivites, defined
with `external`.

In the interface for the stdlib (and as re-exported by Base), this feature is
enabled by use of the `[@local_opt]` annotation on `external` declarations. For
example, we have the following:

```ocaml
external id : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
```

This declaration means that `id` can have either of the following types:

```ocaml
id : local_ 'a -> local_ 'a
id : 'a -> 'a
```

Notice that the two `[@local_opt]`s act in unison: either both `local_`s are
present or neither is. Nothing checks that the locality ascriptions are sound,
though, so use this feature with much caution. In the case of `id`, all is well,
but if the two `[@local_opt]`s did not act in unison (that is, they varied
independently), it would not be: `id : local_ 'a -> 'a` allows a local value to
escape.

### Stack allocation

Primitives defined within the compiler (with names starting with `%`) are
inlined at every use site, and can have different runtime behavior (such as
allocation) at each use site. For example, primitives that return allocated
values will allocate the value on stack if declared to be local-returning:

```ocaml
external ref_stack : 'a -> local_ 'a ref = "%makemutable"
external ref_heap : 'a -> 'a ref = "%makemutable"

let r_stack = ref_stack "hello" in
let r_heap = ref_heap "hello" in
let r_error = stack_ (ref_heap "hello") in
...
```

In this example, `r_stack` will always be on stack even without `stack_`
annotation; `r_heap` will always be on heap; and `r_error` will trigger type
error. We can further use the `[@local_opt]` attribute to declare an allocation
polymorphic `ref`:

```ocaml
external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
```

which allocates the cell on the GC heap or on the stack, depending on the
inferred mode.
