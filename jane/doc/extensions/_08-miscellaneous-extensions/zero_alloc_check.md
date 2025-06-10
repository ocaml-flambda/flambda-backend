---
layout: documentation-page
collectionName: Miscellaneous extensions
title: Zero_alloc checker
---

# Compile-time checking of non-allocating functions

This page describes support for `[@zero_alloc]` attributes on functions.  These
annotations are checked statically at compile time. If the check passes, it guarantees
that at runtime there will be no allocations on any execution of the function, including
in the callees.

For example, the check succeeds on `add` and fails on `add64`:

```ocaml
let[@zero_alloc] add b x y = if b then x + y else x
let[@zero_alloc] add64 b (x:int64) (y:int64) = if b then Int64.add x y else x
```
The check forbids allocations on the OCaml heap and other events that may trigger garbage
collection. Local allocations are allowed. For example, the check succeeds on `local_pair`
but fails on `pair`:


```ocaml
let[@zero_alloc] local_pair x y  = stack_ (x,y)
let[@zero_alloc] pair x y  = (x,y)
```

The check is conservative in the sense that it may fail even if the function never
allocates. In particular, all indirect calls are considered
to be allocating.  For example, the check fails on the following:

```ocaml
let[@zero_alloc] rec iter f l =
  match l with
  | [] -> ()
  | hd::tl -> f hd; iter f tl
```

A caller of `iter` can be shown to be non-allocating, for example
the check succeeds when `sum` is compiled with `-O3` and `iter` is inlined:
```ocaml
let[@zero_alloc opt] sum l =
  let local_ c = ref 0 in
  iter (fun n -> c := add (no_overflow !c n) !c n) l;
  !c
```

Note the use of `opt` payload to mark this function as `zero_alloc` in optimized builds
only.

There is no need to annotate all callees of a `zero_alloc` function for the check to pass,
and in fact the callees might not pass the check, as the above example of `iter` shows.
The compiler analyzes all callees to determine their allocation behavior, but only checks
that functions annotated with `@zero_alloc` do not allocate. Summary of allocation
behavior of all functions is stored in `.cmx` file and used to analyze callers from other
compilation units.

## How to enable the check?

The check is enabled by default for `[@zero_alloc]` attributes without `opt` payload.
These annotation should only be used on functions that pass the check in all builds,
even unoptimized builds.

For optimized builds, `-zero-alloc-check all` compiler flag enables
checking of `[@zero_alloc opt]` annotations in addition to the default ones.

## Relaxed vs strict annotation

Our goal is to make the analysis precise enough to be useful in practice. Consider for
example the following function from `core_kernel/iobuf/src/iobuf_expert.ml`:

```ocaml
let[@zero_alloc] reinitialize_of_bigstring t ~pos ~len buf =
    let str_len = Bigstring.length buf in
    if pos < 0 || pos > str_len
    then
      raise_s
        [%message
          "Expert.reinitialize_of_bigstring got invalid pos" (pos : int) (str_len : int)];
    let max_len = str_len - pos in
    if len < 0 || len > max_len
    then
      raise_s
        [%message
          "Expert.reinitialize_of_bigstring got invalid len" (len : int) (max_len : int)];
    let lo = pos in
    let hi = pos + len in
    unsafe_reinitialize t ~lo_min:lo ~lo ~hi ~hi_max:hi buf
  ;;
```

This function allocates on the error paths.  However, such allocations rarely matter for
hot code performance.  Therefore, by default, the check uses a "relaxed" meaning of
`zero_alloc` that ignores allocations on paths that lead to an exceptional return from the
function. The check succeeds on this example.

If we change the annotation of `reinitialize_of_bigstring` from `[@zero_alloc]` to
`[@zero_alloc strict]`, the check will fail with the following error:

```
File "core_kernel/iobuf/src/iobuf_expert.ml", line 1948, characters 7-17:
Error: Annotation check for zero_alloc strict failed on function Iobuf_expert.reinitialize_of_bigstring (camlIobuf_expert__reinitialize_of_bigstring_9401)

```
The "strict" meaning ensures no allocation on any paths.

### Treatment of `raise_notrace` vs `raise` with a backtrace

The relaxed meaning of `@zero_alloc` only applies to exceptions raised with a
backtrace. Our reasoning is that `raise_notrace` can be used for normal control flow,
whereas `raise` with a backtrace is expensive enough to only be performed on error paths
that can be ignored in `zero_alloc` code. If the code is compiled without `-g` flag or
with backtraces disabled, all `raise` are treated as `raise_notrace`.  In that case, the
check will fail on all "relaxed" annotations that are not also "strict".

## Annotation `assume`

Sometimes, the "relaxed" meaning above is not sufficiently permissive.
For example, consider a function that allocates a designated value for
error, rather than raising an exception.  The check of "relaxed"
annotation will fail. If we don't care about allocation in the error
case, we can annotate the function that produces the error value with
`[@zero_alloc assume]`.  For example, the check succeeds for `test`:

```ocaml
let[@cold][@zero_alloc assume] log_error msg n = print_string msg; Error n

let check ~limit l =
  match l with
  | None -> log_error "empty" 0
  | Some n -> if n > limit then log_error "over" n else Ok ()

let[@zero_alloc] test ~limit ~default l =
  match check ~limit l with
  | Ok _ -> l
  | Error n -> default
```

### What if `assume` is not enough?
A common pattern is to perform error handling in an exception handler
and return an error code instead of reraising. For example:
```ocaml
let[@zero_alloc] g = ...

let[@cold][@zero_alloc assume] handle_exn exn =
  print_s [%message "something went wrong" ~_:(exn:exn)];
  Answer.Option.none

let[@zero_alloc] f () =
  match g () with
  | exception exn -> handle_exn exn
  | answer -> Answer.Option.some (answer * 2)

```

The check of `f` fails in this example, even when the check of `g` succeeds.  The check of
`g` ignores allocations in `g` on a path that returns from `g` by raising an exception,
but `f` catches the exception and returns normally.  As a result, these allocations in `g`
appear on a path to a normal return from `f` and the check of `f` fails.  It is not enough
to annotate `handle_exn` with `[@zero_alloc assume]` because it still permits
`handle_exn` to return normally to `f` and continue executing
within the "zero alloc" scope of `f`.

To address it, `handle_exn` can be annotated with `[@zero_alloc assume error]`.  The
intended meaning of `assume error` is to exit the scope of "zero_alloc" code due to an
error case in which we do not care about allocations. With `assume error`, the check
ignores all allocations on paths that go through `handle_exn` and treats
`handle_exn` as if it gets "stuck" (does not return normally and does not raise).
Note that allocations after a call to `handle_exn` are also ignored, in addition to
allocation leading up to the call and within `handle_exn` itself.

### Where can I use "assume"?

It is possible to place `[@zero_alloc ..]` annotation on function definitions and
let-bindings of functions (as in the example above).

It is also possible to annotate function applications
with `[@zero_alloc assume ..]`. In the above example, instead of annotating
`log_error` definition, we can annotate specific call sites:

```ocaml
let check ~limit l =
  match l with
  | None -> log_error "empty" 0
  | Some n -> if n > limit then (log_error[@zero_alloc assume]) "over" n else Ok ()
```

It is also possible to annotate applications
of `external` functions with `[@zero_alloc assume ..]`.

Functions that are not `cold` and may be inlined can also be annotated with
`[@zero_alloc assume ..]` (this was not the case in early versions of the checker).

It is not yet possible to annotate arbitrary sub-expressions or code blocks.

### `assume_unless_opt` vs `opt` vs `assume`

A function annotated with `[@zero_alloc assume]` is never checked.

A function annotated with `[@zero_alloc opt]` is not checked in a non-optimized build.
This is inconvenient because the signature of such a function cannot be annotated with
`[@zero_alloc]` and its callers will often fail the check in a non-optimized build too,
and would also need to be annotated with `[zero_alloc opt]` and therefore defer their
checking to optimized builds.

The payload `assume_unless_opt` can be used to ensure that a function is `zero_alloc` in
an optimized build and allow its callers to treat it as non-allocating even in
non-optimized builds. This allows for those callers to be checked as much as possible in
non-optimized builds, to detect as early as possible if an allocation is introduced
elsewhere.

By default, `[@zero_alloc assume_unless_opt]` is treated as
`[@zero_alloc assume]` on function applications and implementations.

If `-zero-alloc-check all` compiler flag is used, then `[@zero_alloc assume_unless_opt]`
is treated as `[@zero_alloc]` on function implementation,
and ignored on application.

## Use in signatures

Signature items may be annotated `zero_alloc`, as in:
```ocaml
val[@zero_alloc] f : int -> int
```
Such annotations have two effects:

- In modules implementing such a signature, the corresponding function is checked for
  `zero_alloc`. The implementation of the function need not be annotated explicitly
  with `zero_alloc` (this was not the case in early versions of the checker).
- The `zero_alloc` analysis of `f`'s callers can use the fact `f` is
  zero alloc.  This does not rely on inlining, so `f` is reliably
  known to be zero alloc even when compiling an application of it in a
  different library.

### Legal payloads and module typing

The payload of a such an annotation may contain the `strict` and `opt`
directives (as well as a new `arity` directive, more on which below).  If `opt`
is present, the information will only be available to clients in optimized
builds.  Implementing modules must annotate the corresponding function with a
`zero_alloc` attribute that is at least as strong, so, for example, this
typechecks:
```ocaml
module M : sig
  val f[@zero_alloc] : int -> int
end = struct
  let f[@zero_alloc strict] x = x
end
```
However, if the signature had `strict` and the struct did not, this would be
rejected by the typechecker.

It is legal to satisfy a signature's `zero_alloc` requirement with a `zero_alloc
assume` annotation in the implementation.

### Arity

The compiler will infer the arity of a declaration with a `[@zero_alloc]`
attribute by counting the number of arrows in the type.  Applications of this
function will only be considered `zero_alloc` when fully applied.  For
example, if a library declares a function `f`:
```ocaml
val[@zero_alloc] f : 'a -> 'a -> 'a
```
Then, in code that uses this library, the `zero_alloc` analysis will know that
applications of `f` to two arguments do not allocate. Applications of `f` to any
other number of arguments will still be considered allocating (unless
cross-module inlining is available and the compiler can see from `f`'s
definition that it also does not allocate in those cases).

Sometimes it is necessary to override the arity inferred by the compiler here.
For example, the compiler will reject this signature because the `[@zero_alloc]`
attribute is present on a declaration without any arrows in its type:
```ocaml
type t = int -> int
val[@zero_alloc] f : t
```
This behavior can be overridden by specifying the arity manually:
```ocaml
type t = int -> int
val[@zero_alloc arity 1] f : t
```
There are also cases where arrows are present but is still desirable to override
the arity, as here:
```ocaml
module M : sig
  val[@zero_alloc arity 1] f : int -> int -> int
end = struct
  let[@zero_alloc] f x =
    if x = 42 then fun y -> (y,y) else fun y -> (y,y+1)
end
```

## Attribute `[@@noalloc]` vs `[@zero_alloc]`

The check of `[@zero_alloc]` annotations on OCaml functions relies on `[@@noalloc]`
annotations of any `external` declarations they refer to in order to determine the
allocation behavior of the corresponding external functions. Without `[@@noalloc]`, the
check conservatively assumes that external code allocates. The check treats `[@@noalloc]`
attribute using the "strict" meaning, as if the external declaration is annotated with
`[@zero_alloc assume strict]`.

There is currently no way to annotate external declarations with
`[@zero_alloc assume]` for the "relaxed" meaning of the check.  A
workaround is to annotate the call sites with `[@zero_alloc assume]`.
An alternative workaround is to define a wrapper OCaml function for
the `external` and annotate the wrapper with `[@zero_alloc assume][@inline always]`
to make sure it is always inlined and has no
impact on the generate code.

Note that the presence of `[@@noalloc]` affects the generated code whereas `[@zero_alloc]`
is only used for checking and error reporting at compile time.

## Interaction with safepoints in OCaml 5

Currently, the check ignores poll instructions (otherwise all loops would be considered
allocating when poll insertion is enabled). Therefore, `[@zero_alloc]` is not sufficient
to ensure the absence of context switches.

## Interaction with inlining and other optimizations

The check is very sensitive to the optimization level and other compilation flags.  This
is by design and not expected to change.  To give the most accurate results, the check for
allocations is performed late in the compilation process, after all the optimizations such
as inlining, specialization, static allocation and unboxing. The result of the check can
sometimes be surprising and counter-intuitive at the source code level.

## Dynamic vs static checking

`Expect_test_helpers_cre.require_no_allocation` counts the exact
number of words allocated in a particular program execution at
runtime, whereas `[@zero_alloc]` check considers all possible
executions without ever running the program and may over-approximate
the number of allocations.  The two approaches are complementary.
We recommend using both.

## What is the overhead?

When the check is enabled, the overhead on build time is low. It is just one additional
lightweight pass on each compilation unit.

The size of `.cmx` files can increase slightly. Preliminary measurements on the compiler
distribution itself showed less than `0.5%` increase with flambda2. Current
format is naive and can be optimized if this becomes a bottleneck.

There is no runtime overhead!

There is no change to generated code as a result of enabling the check or adding
`[@zero_alloc]` annotations on functions, except the described below.


## Dead code elimination

The presence of `zero_alloc` annotation can prevent dead code elimination.

Consider for example the function `foo`:
``` ocaml
let[@zero_alloc strict] foo t =
  if debug then log "foo" t [%sexp_of: _ t];
  bar t
```
We expect this function to pass the check when `debug` is statically set to `false`, and
fail otherwise.

If `foo` is not exposed in the interface and gets inlined into all call sites, the optimizer
can remove the definition of `foo`. The presence of `zero_alloc` annotation delayes the elimination of `foo` so that the compiler can perform the check after all other optimizations.
The presence of the function may prevent some other
optimizations from triggering.

## How do I figure out why the check fails on my function?

When the check fails, the compiler reports all allocations and other expressions such as
indirect calls that lead to the failure of the check.  The source locations of the offending
allocations is shown in the error messages, including inlining information.

For example, compiling this (contrived) example

```ocaml
let[@inline never] bar x = [x+1;x+x]

let[@zero_alloc] foo b x f =
  let y =
    match bar x with
    | [c] -> f c
    | [a;_] ->
      [%probe "test" (print_int x)];
      (a, a)
    | _ -> (x,x)
  in
  Dep.inline_always y
```
fails with an error message that looks like this:
```bash
File "test.ml", line 3, characters 5-15:
3 | let[@zero_alloc] foo b x f =
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function Test.foo (camlTest__foo_3_6_code)

File "test.ml", line 5, characters 10-15:
5 |     match bar x with
              ^^^^^
Error: called function may allocate (direct call camlTest__bar_2_5_code)

File "test.ml", line 6, characters 13-16:
6 |     | [c] -> f c
                 ^^^
Error: called function may allocate (indirect call)

File "test.ml", line 8, characters 6-35:
8 |       [%probe "test" (print_int x)];
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: expression may allocate
       (probe test handler camlTest__probe_handler_test_4_7_code)

File "test.ml", line 9, characters 6-12:
9 |       (a, a)
          ^^^^^^
Error: allocation of 24 bytes

File "test.ml", line 10, characters 11-16:
10 |     | _ -> (x,x)
                ^^^^^
File "test.ml", line 12, characters 2-21:
12 |   Dep.inline_always y
       ^^^^^^^^^^^^^^^^^^^
Error: called function may allocate (external call to caml_make_vect) (test.ml:12,2--21;dep.ml:2,2--38)
```

If we remove `[@inline never]` annotation from `bar`, the compiler can optimize some but
not all allocations in `foo`.  The check will fail and show the remaining allocations::

```bash
File "test.ml", line 3, characters 5-15:
3 | let[@zero_alloc] foo b x f =
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function Test.foo (camlTest__foo_3_6_code)

File "test.ml", line 8, characters 6-35:
8 |       [%probe "test" (print_int x)];
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: expression may allocate
       (probe test handler camlTest__probe_handler_test_4_7_code)

File "test.ml", line 9, characters 6-12:
9 |       (a, a)
          ^^^^^^
Error: allocation of 24 bytes

File "test.ml", line 12, characters 2-21:
12 |   Dep.inline_always y
       ^^^^^^^^^^^^^^^^^^^
Error: called function may allocate (external call to caml_make_vect) (test.ml:12,2--21;dep.ml:2,2--38)

```

Compilation flag `-zero-alloc-checker-details-cutoff n` controls how many allocations are listed
in these error messages:

- positive `n` : print at most `n` witnesses
- `n` is `0`: do not print any witnesses
- negative `n` : print all witnesses (default)

The default setting is convenient when adding annotations or editing annotated code.  The
other settings have lower memory overhead and are convenient for testing the analysis.

The flag can be passed in the usual way via `dune` settings.  For example,
to turn off detailed messages in a particular library, add `(ocamlopt_flags (:standard
-zero-alloc-checker-details-cutoff 0))` to the `library` stanza in `dune` file.

When the check fails, it is a compile error (not a warning) and compilation stops. The
error message will show all functions that failed the check in the same compilation unit
(not just the first function that failed the check).

## Other ways to control the analysis

The following settings are mostly for testing and debugging of the analysis.

The check can be controlled by the compiler flag `-zero-alloc-check` with the following
arguments:

* `default` : check attributes without `opt` payload (default)
* `check all` : covers both `opt` and default payloads and is intended to be used for
      optimized builds
* `none` : disable the check
* `opt` : only check attributes with `opt` payload and is intended for debugging

The flag can be passed to `dune` build as usual for the entire project or on a
per-library basis. For example, to disable regular and `opt` checks:
`(ocamlopt_flags (:standard -zero-alloc-check none)`

The check can also be controled on a per-file basis using the top-level attribute
`[@@@zero_alloc check]`, `[@@@zero_alloc check_none]`, `[@@@zero_alloc check_all]`,
`[@@@zero_alloc check_opt]`.

If information about dependencies is not available (i.e., a
build without cross module / library inlining or with `-opaque`), the dependencies will be
conservatively assumed to allocate and the check may fail.

Details of the analysis can be printed using `-dzero-alloc` compiler flag for debugging.

The analysis can be disabled using `-disable-zero-alloc-checker` flag.
This disables computation of function summaries (unlike
`-zero-alloc-check none` that disables checking of annotations). With
`-disable-zero-alloc-checker` flag, the checker will conservatively
assume that all functions may allocate, so if there are any
annotations, compilation will fail unless `-zero-alloc-check none` is
also passed.

Compilation flag `-disable-precise-zero-alloc-checker`
disables computation of summaries only for recursive functions
and functions whose definition appears in the generated code before
the definitions of all their dependencies.  This is an escape hatch
for situations of unexpectedly high overhead of the analysis on
specific files, for example due to precise analysis of recursive
functions. It will cause all uses of these functions to be
conservatively treated and likely fail the check.
`-zero-alloc-checker-join` compilation flag provides a more fine-grained
control of precision.

## Annotation `[@@@zero_alloc all]` and `[@zero_alloc ignore]`

Some files consist mostly of non-allocating code. To reduce the annotation burden, we
provide a top-level annotation `[@@@zero_alloc all]`. It requires that all function in the
compilation unit are not allocating.  It is equivalent to adding `[@zero_alloc]`
annotation to all functions in the file.
To opt out individual function, use
`[@zero_alloc ignore]`. For example:

```ocaml
[@@@zero_alloc all]
let[@zero_alloc ignore] make x y = (x,y)
let fst (x,y) = x
let snd (x,y) = y
```

Note that `[@@@zero_alloc all]` does not cover the top-level effects (and any other module
initialization code that ends up in the compiler-generated `entry` function).

 For example, the following file passes the check:
```ocaml
[@@@zero_alloc all]
let () = Printf.printf "Hello world!"
```
but this one fails:
```ocaml
[@@@zero_alloc all]
let[@inline never] test () = Printf.printf "Hello world!"
let () = test ()
```

More precisely, `[@@@zero_alloc all]` applies to all function symbols
emitted for the compilation unit, except the `entry` function (and
some compiler-generated wrapper functions). As such, it will apply to
inner functions but may not apply to some top level functions
optimized away by the compiler as discussed above.

`[@@@zero_alloc all]` and `[@zero_alloc ignore]` are somewhat more experimental than
other annotations. Their scope is global within a compilation unit is global and
they should appear at the top of the file before any other (otherwise
their effect is not well-defined).

### Annotation `[@@@zero_alloc all]` vs `[@@@zero_alloc check]`

`[@@@zero_alloc check]` turns on the checking of `zero_alloc` annotations on functions,
whereas `[@@@zero_alloc all]` requires that all functions in the file are `zero_alloc`.

## Syntactic limitation

The `[@zero_alloc]` annotation can only appear on functions, not values.
For example, if we add `[@zero_alloc]` attribute to `recvfrom` function  defined in
`lib/zero_udp/src/zero_udp.ml` as follows:
```ocaml
let[@zero_alloc] recvfrom = Zero_unix.recvfrom
```
the compiler prints a warning:
```
File "lib/zero_udp/src/zero_udp.ml", line 42, characters 5-15:
Error (warning 53 [misplaced-attribute]): the "zero_alloc" attribute cannot appear in this context
```

A workaround is to add arguments explicitly:
```ocaml
let[@zero_alloc] recvfrom a b = Zero_unix.recvfrom a b
```

This limitation is not specific to `[@alloc]`. Other function-level attributes like
`[@inline]` and `[@specialise]` have the same limitation.


## Limitations

Summary of known limitations

- We currently do not allow `[@zero_alloc]` annotations on external function declarations.
  Applications of external functions can be annotated with `[@zero_alloc assume]`.
- Annotations cannot be placed on arbitrary sub-expressions.
- Annotations on functions that would otherwise be optimized away may inhibit
  optimizations. The presence of annotations currently prevents the functions from being
  dead code eliminated, which in turn may prevent other code from being optimized
  and also increases the size of generated code.
- Functions annotated with `[@zero_alloc assume]` are not analyzed at all. As a result,
  the summary of such a function can be more conservative than if the function was
  actually analyzed. For example, if the function does not allocate at all on any path,
  but assumed to be `zero_alloc` using the relaxed semantics.
- Inlining functions annotated with `assume` may cause the `zero_alloc` check of the caller to
  fail when the inlined body contains `try..with` and the exception handler
  may return not via a `raise`. A workaround is to annotate the exception handler with
  `[@zero_alloc assume error]`.
- Conservative handling of higher order function arguments, for example in `List.iter`.
Another common example where the check fails is `Map.find_exn` and `Hashtbl.mem` from
`base` if the underlying comparator and hash functions are not annotated with `[@zero_alloc]`
in the signature.
- The check may fail on non-allocating functions that manipulate arrays due to flat float
optimization, but unfortunately, the error message is not helpful.
- Missing scoping of top-level `[@@@zero_alloc ...]` annotations.
- A function that does not allocate may fail the check if the analysis
  does not have access to summaries of functions in other libraries
  and thus any calls to such functions are conservatively considered
  as allocating.
- Syntactic limitation of `[@zero_alloc ..]` attribute: can only appear on functions, not
  values.
- Safepoints are considered non-allocating.
