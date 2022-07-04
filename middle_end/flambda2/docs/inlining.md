# Inlining in flambda2

## Introduction

The inliner is an important optimization that allows the user to write near
zero-cost abstractions easily.
Inlining occurs on function calls. Upon seeing a call the inliner can decide to
*inline* it, meaning remove the call and replace it by the body of the function
being called. Any information about the arguments of the call can then be used
to simplify the body. Thus, the inliner can indirectly remove allocations, unused
code and simplify some operations.
The hard part of inlining is on deciding what is worth inlining and what isn't.
This document will go through the inner work of the inliner in Flambda2.

Flambda2's inliner must be:
- *Predictable*: the user should never be surprised by a decision taken by the
  inliner, and as a corollary any decision must be explainable.
- *Fast*: The inliner should be fairly efficient.


## Cost_metric

Two metrics are computed for all expressions going through the simplifier. The
first one, the code size, is a rough approximation of the size of the generated
code. The second one, the removed_operations is a set of counters tracking the
number of operations removed during by simplifying this expression.

### Code size

The code size is computed in an arbitrary unit. It is not meant to be exact,
only to be correlated with the actual size of the generated code.
Given an expression the code size is computed as follows:
- If it is `Switch`, then the size is 5 times the number of arms
- If it is `Apply_cont`, the size is 0 if there is no trap,  5 otherwise
- If it is `Apply` then the size depends on the type of calls. Direct calls have
  a smaller size than indirect calls, which has a smaller size than allocating
  extcalls.
- `Let` has the size of its body plus the size of its expression
- The size of a `Prim` depends on the specific primitive
- Finally the size of a set of closures corresponds to the sum of the size of
  its closures, to which is added the size of an allocation and 2 if the closure
  has arity 1, 3 otherwise. The sum of the size of it closures is involved in
  the computation as if we were to inline a function where a set of closures was
  defined then we will have to also copy the body of those closures.


### Removed operations

While simplifying an expression the simplifier might remove some operations. As
all instructions are not made equal in regard to performance, it's generally a
good idea to give a small bonus to functions that when inline removes
known-to-be costly expressions.

The `Removed_operations` metrics are tracking the number of function calls,
allocations, primitives and branches that were removed as well as the number
of indirect calls converted to direct calls and polymorphic comparisons specialised
into non-polymorphic comparisons while simplifying the expressions.


## Inlining arguments.

The behavior of the inliner is controlled by a variety of command line flags.

- **flambda2-inline-max-depth**: Maximum depth of search of inlining opportunities
  inside inlined functions.
- **flambda2-inline-max-rec-depth**: Maximum number of times a recursive function
  can be unrolled
- **flambda2-inline-call-cost**, **-flambda2-inline-alloc-cost**,
  **-flambda2-inline-prim-cost**, **-flambda2-inline-branch-cost**,
  **-flambda2-inline-poly-compare-cost**, **_flambda2-inline-indirect-call-cost**:
  Costs of making a direct call, an allocation, calling a primitive, taking a
  branch, doing a polymorphic comparison and calling an indirect function. These
  costs are used to evaluate the removed operations.
- **-flambda2-inline-small-function-size**, **-flambda2-inline-large-function-size**:
  Respectively the maximum code size under which a function is considered to be
  "small" and the minimum code size over which a function is considered to be
  "large".
- **-flambda2-inline-threshold**: "Aggressiveness" of inlining, more on it later.

Binary flags (not explicit, only the one impacting the inliner):
- **-(no-)flambda2-expert-can-inline-recursive-functions**: Turn on
  (or off) the inlining of recursive functions
- **-(no-)flambda2-speculative-inlining-only-if-arguments-useful**:
  If unset, do not consider functions where arguments are unknown
  for inlining
- **-(no-)flambda2-expert-fallback-inlining-heuristic**:Allow inlining of functions
  whose bodies contain closures.

The user can either set these flags one by one or use one of the predefined
optimization levels `-O1`, `-O2` and `-O3` (from less aggressive to most
aggressive).

## Inlining heuristics

Flambda2's inliner can take inlining decisions both on function declarations and
on function calls.

### Function declaration decision

Decisions on function declarations are used to classify the function in one the
three following categories:
- Small function that must be inlined
- Large function that should never be inlined
- Function that might be inlined (or speculatively inlinable).

The classification is done by using the code size of the function and comparing
it to the size threshold for small functions and large functions. Functors are
always classified as "might be inlined". Inlining annotations of the function are
respected: functions annotated by `[@@inline never]` will never be inlined, by
`[@@inlined always]` will always be inlined and by `[@@inlined available]` will
be inlined speculatively.

Functions deemed not inlinable on their definition will not be exported in the
cmx for their compilation unit.


### Function call site decision

The first thing that the inliner does on a call site is to try and retrieve the
`Code_metadata.t` associated to the callee. This type contains any information
about the function but its code. This includes the decision made on the function
declaration.
The inliner will now follow several steps:
1. Check if this call should be inlined (or not) because of an annotation.
It will then first inspect the annotation (if it exists) present on the call.
Calls annotated by `[@inlined never]` are never inlined and by `[@inlined always]`
or `[@inlined hint]` are always inlined, In the case where the user requested the
function to be inlined but the `Code.t` of the function is missing -- meaning
that this function was deemed too large to be inlined by the decision on its
definition, or that the call was indirect -- a warning is emitted. One way to
fix such a situation is to add a `[@inlined hint]` annotation on the definition
of the function. In the case of an `[@unrolled n]` annotation the inliner will
try and inline the recursive call.

2. Check if inlining this call would be coherent under the current set of
inlining arguments
The inliner checks the current inlining depth against the maximum inlining
depth. If it is over it then this call will not be inlined.
A similar check is done for recursive functions: the `rec_info` of the function
is retrieved and compared to the maximum recursive depth.

3. Check the decision made on the function declaration
If the decision on the definition was to always inline this function, the
function is inlined. If it was to never inline it then it is not.

4. Check if the environment allows for speculative inlining to happen.

5. Check if at least one of the function arguments has useful information.
   The meaning of useful information is pretty wide, it could go from "has a
   specific value" to "we have some idea of its shape".
   Intuitively it makes sense not to inline a function when nothing his known
   about its argument in this case: if it was especially important to inline
   then it should have been annotated, and if the inliner didn't deem it
   worthy of being inlined on its call site then it's likely that nothing
   will change if we know nothing about its arguments.
  
6. Do some speculative inlining.
   The inliner is left with function that might, or might not, be inlined.
   The inliner will start by inlining the call and by simplifying it while
   prohibiting any speculative inlining from occurring. `cost_metrics` of
   the inlined call are retrieved and are evaluated using the following
   formula: $D = \text{code_size} - \text{sum} \text{removed_operations}$.
   If $D$ is below the inlining threshold the call is inlined otherwise it
   isn't.
  

Once the heuristics decided to inline a call, the simplifier will resume its
work starting from the inlined body.
 
## Inlining predictably

### Problem

The behavior of the inliner is conditioned on the values of the inlining
arguments set by the user. Depending on their precise values, the inliner might
decide to inline a function in one case and not at all in the second. For the
inliner to be predictable to the user it is vital for him to have a good mental
model about the set of arguments used by the inliner on the different functions,
especially when several files are involved.


Let's suppose that we got two files and that `-O3` inlines more than `-O1`.

```ocaml
(* File A.ml, compiled with -O1 *)
let foo x =
  let .... in
  let fox () = ...
  in
  let x = C.bar ... in
  ...

(* File B.ml, compiled with -O3 *)
let baz () =
  A.foo ....
```
When compiling B.ml Flambda2 might inline the call to A.foo. If that
is the case the content of B.ml will look like

```ocaml
let baz () =
  let .... in
  let fox () = ...
  in
  let x = C.bar ... in
  ...
```
Flambda2 will then use the optimization parameters -O3 to inline the call
to C.bar as well as the definition to fox.

This can be considered as an error: both of these constructs were defined
in a file compiled with a different set of optimization parameters and kind
of magically appeared inside B after some optimization. They should still be
optimized with -O1, and not with -O3.

Flambda2 considers that any code coming through (either directly defined or
coming from an inlined functions) a file implemented with weak inlining
settings can't be inlined with a more aggressive set of parameters.

Now let's take another example:

```ocaml
(* File A.ml, compiled with -inlining-max-depth 2 *)
let fn_4 x = ...
let fn_3 x = fn_4 x
let fn_2 x = fn_3 x
let fn_1 x = fn_2 x
let bar x = fn_1 x

(* File B.ml, compiled with -inlining-max-depth 2 *)
let baz () = A.bar ()
```

Suppose that when compiling `A.ml` Flambda2 decided to inline the calls to `fn_1`
inside `bar` and then the call ti `fn_2` inside `bar`. The newly created call to
`fn_3` inside `bar` has now an inlining depth of `2`.
Later on, when `B.ml` will be compiled, let's suppose that Flambda2 decided to
inline `A.bar`. The code of `B.ml` is now similar to:

```ocaml
(* File A.ml, compiled with -inlining-max-depth 2 *)
let fn_4 x = ...
let fn_3 x = fn_4 x
let bar x = fn_3 x (* fn_3 has an inlining depth of 1*)

(* File B.ml, compiled with -inlining-max-depth 2 *)
let baz () = A.fn_3 () (* fn_3 has an inlining depth of 2 coming from the inlining
                         of foo in A.ml
                         This call to A.fn_3 is coming from inlining A.baz with
                         an inlining depth of 1*)
```

We consider that the inlining depth of the call to `A.fn_3` should really be 3 and
thus that no further inlining should happen on it in our case.
To understand why, let's suppose that we kept 1 as its inlining depth. Then
if Flambda2 were to inline `A.fn_3` a call to `A.fn_4` would appear. This call
to `A.fn_4` would have required an inlining depth of at least 3 to appear if we
were inlining from `A.ml` and thus Flambda2 inlined more aggressively than what
the user specified.


### Implementation

The `Inlining_state` module encapsulates a set of inlining arguments and an
inlining depth. All functions declarations and apply nodes are associated with
an instance of `Inlining_state.t`. Furthermore, there is also one inlining state
inside the downward environmentironment of the simplifier. At the start of the
compilation process this inlining state is initialized to have an inlining depth
of 0 and to inherit the set of inlining arguments specified by the user.
Any function declaration or apply node build by the simplifier is associated
with the inlining state present in the downward environment at the time of their
construction.
It is possible to *meet* two inlining states a and b, producing a new inlining
state where:
- the set of inlining args will inline at most as much as the one in a and the
  one in b
- the inlining depth is the sum of the one in a and the one in b.

Decisions on a function declaration are taken using the *meet* of the inlining
state from the environment and of the inlining state from the function itself. The same
reasoning applies for decisions on a call site but this time with the *meet* of
the inlining state from the environment and the apply node.

Lastly when a call is actually inlined we set the inlining state of the environment to
be the *meet* of the one already in the environment and the ones from the call and from
the function being called. The inlining depth is also incremented.
