#  Some Pitfalls of Local Allocations

This document outlines some common pitfalls that may come up when
trying out local allocations in a new codebase, as well as some
suggested workarounds. Over time, this list may grow (as experience
discovers new things that go wrong) or shrink (as we deploy new
compiler versions that ameliorate some issues).


## Tail calls

Many OCaml functions just happen to end in a tail call, even those
that are not intentionally tail-recursive. To preserve the
constant-space property of tail calls, the compiler applies special
rules around local allocations in tail calls (see [the
reference](./local-reference.md)).

If this causes a problem for calls that just happen to be in tail
position, the easiest workaround is to prevent them from being
treated as tail calls by moving them, replacing:

    func arg1 arg2

with

    let res = func arg1 arg2 in res

With this version, local values used in `fun arg1 arg2` will be freed
after `func` returns.

## Partial applications with local parameters

To enable the use of local allocations with higher-order functions, a
necessary step is to add local annotations to function types,
particularly those of higher-order functions. For instance, an `iter`
function may become:

    val iter : 'a list -> f:local_ ('a -> unit) -> unit

thus allowing locally-allocated closures `f` to be used.

However, this is unfortunately not an entirely backwards-compatible
change. The problem is that partial applications of `iter` functions
with the new type are themselves locally allocated, because they close
over the possibly-local `f`. This means in particular that partial
applications will no longer be accepted as module-level definitions:

    let print_each_foo = iter ~f:(print_foo)

The fix in these cases is to expand the partial application to a full
application by introducing extra arguments:

    let print_each_foo x = iter ~f:(print_foo) x

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
