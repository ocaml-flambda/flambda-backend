#  Some Pitfalls of Local Allocations

This document outlines some common pitfalls that may come up when
trying out local allocations in a new code base, as well as some
suggested workarounds. Over time, this list may grow (as experience
discovers new things that go wrong) or shrink (as we deploy new
compiler versions that ameliorate some issues).

If you want an introduction to local allocations, see the [introduction](local-intro.md).

## Tail calls

Many OCaml functions just happen to end in a tail call, even those
that are not intentionally tail-recursive. To preserve the
constant-space property of tail calls, the compiler applies special
rules around local allocations in tail calls (see [the
reference](./local-reference.md)).

If this causes a problem for calls that just happen to be in tail
position, the easiest workaround is to prevent them from being
treated as tail calls by moving them, replacing:

```ocaml
func arg1 arg2
```

with

```ocaml
let res = func arg1 arg2 in res
```

or by annotating them with `[@nontail]`:

```ocaml
func arg1 arg2 [@nontail]
```

With this version, local values used in `func arg1 arg2` will be freed
after `func` returns.

## Partial applications with local parameters

To enable the use of local allocations with higher-order functions, a
necessary step is to add local annotations to function types,
particularly those of higher-order functions. For instance, an
unlabeled `iter` function may become:

```ocaml
val iter : local_ ('a -> unit) -> 'a t -> unit
```

thus allowing locally-allocated closures to be used as the first
parameter.

However, this is unfortunately not an entirely backwards-compatible
change. The problem is that partial applications of `iter` functions
with the new type are themselves locally allocated, because they close
over the possibly-local `f`. This means in particular that partial
applications will no longer be accepted as module-level definitions:

```ocaml
let print_each_foo = iter print_foo
```

The fix in these cases is to expand the partial application to a full
application by introducing extra arguments:

```ocaml
let print_each_foo x = iter print_foo x
```

Note that this pitfall does not apply to the final parameter of a
function. So a labeled `iter` function with a type like:
```ocaml
val iter : 'a t -> f:local_ ('a -> unit) -> unit
```
can be partially-applied without issue:
```ocaml
let print_each_foo = iter ~f:print_foo
```
This is another reason to prefer putting `~f` parameters as the final
parameter of functions.

## Typing of (@@) and (|>)

The typechecking of (@@) and (|>) changed slightly with the local
allocations typechecker, in order to allow them to work with both
local and nonlocal arguments. The major difference is that:

    f x @@ y
    y |> f x
    f x y

are now all typechecked in exactly the same way. Previously, the
first two were typechecked differently, as an application of an
operator to the expressions `f x` and `y`, rather than a single
application with two arguments.

This affects which expressions are in "argument position", which can
have a subtle effect on when optional arguments are given their
default values. If this affects you (which is extremely rare), you
will see type errors involving optional parameters, and you can
restore the old behaviour by removing the use of `(@@)` or `(|>)` and
parenthesizing their subexpressions. That is, the old typing behaviour
of `f x @@ y` is available as:

    (f x) y
