# Exception-related control flow in the backend

## General concepts and description of the current state

Exceptions in the OCaml language are manipulated through two constructions:
the `raise` primitive, and the `try...with` structure (`match...with exception`
is considered here as syntactic sugar for and `try...with` around a regular
`match`).

In the native backend, this is implemented using trap frames on the stack, and
a global variable (now part of the `Domain_state` structure) to hold a pointer
to the current (or top) frame.
A trap frame is a small structure holding a pointer to the previous trap frame
and a code pointer, and it is stored on the regular stack, along with spilled
local variables..

The compilation of a `raise` primitive consists of storing
the exception being raised into the appropriate register, then loading the
current trap frame through the global variable, restoring the value of the current
trap frame to the previous value, restoring the value of the stack pointer to
just before (or just after, for backwards-growing stacks) the trap frame, and
finally jumping to the code pointer contained in the stack frame.

The compilation of a `try...with` structure will produce code that:
- Prefixes the handler's code with a label that can be used for computing its code
pointer.
- Prefixes the body's code with a set of instructions to allocate the trap frame.
This is subsumed in the `Linear` representation by the `Lpushtrap` instruction.
- Appends to the body a set of instructions to deallocate the trap frame.
This is subsumed in the `Linear` representation by the `Lpoptrap` instruction.

So before the `try...with` there is a given stack shape that we will call the
base stack, the body is executed with a stack shape that is the base stack plus
a trap frame, after the body (if the body doesn't raise) the execution resumes
with the base stack, and if the body raises execution goes to the handler, with
the base stack (and when the handler finishes execution flows to the same code
as after the body, with the base stack).

## Problematic cases

This all works well as long as all code paths go through the `Lpushtrap` and
`Lpoptrap` instructions in a well-parenthesized manner. However, this invariant
can be broken by the static exceptions used in the compiler representations
from Lambda and onwards. In particular, although it only occurs in specific
circumstences, it is allowed to have inside the body of a `try...with`
structure a sub-expression that raises a static exception whose handler is
outside the body of the `try...with`. This looks like a simple detail, as
the code that emits the traps simply needs to insert traps around these
static raising constructs, but this is actually also important for earlier
passes as some optimisations involving static exceptions that would be correct
if both the handler and the raise are in the same exception context become
incorrect if the contexts differ.

To deal with this, the relevant optimisations in the compiler carry a `try_depth`
argument that allows to check if the optimisation is allowed or not. This of
course means that someone writing a new optimisation without thinking about this
slightly uncommon pattern is bound to encounter the same problems again.

In flambda2, we chose to deal with this by computing during translation from
lambda what kind of (abstract) trap actions are associated with each static
raise (or `Apply_cont` in flambda2 terminology), so all transformation passes
on the flambda2 representation are forced to deal with the potential trap
context updates.

## Trap context and CFG representations

The main property needed for correct translation to machine code is that each
piece of code knows exactly what trap context it will be evaluated in.
This is to guarantee that access to stack-based pseudo-registers can be
compiled to a statically known offset from the stack pointer, even through
pieces of codes that modify the stack pointer (the trap instructions).

For the current implementation, this is fairly straightforward: all intermediate
representations except the very last ones (linear and the generated assembly)
have some kind of expression structure where the only way to change the trap
context is to enter or exit a trywith block.

But there have been some experiments (in particular the flambda2 project, but
not only) that use a representation of the code that is either explicitly
or implicitly a control-flow graph. For those representations, the ability
to re-order the nodes (or instruction blocks) of the graph is an important
property, and at least for flambda2 it's very easy to end up with a graph
for which there is no straightforward transformation back into a structured
expression form.

Consider the following OCaml program (it's fairly easy to end up with programs
looking like that after inlining):

```ocaml
let x =
  if cond then begin
    then_branch ();
    A a
  end else begin
    else_branch ();
    B b
  end
in
try
  match x with
  | A a ->
    a_branch ()
  | B b ->
    b_branch ()
with
| exn -> handler exn
```

We would like to rewrite this into (in pseudo-code):

```ocaml
let%trywith_handler h exn = handler exn in
let r =
  if cond the begin
    then_branch ();
    enter_trywith h;
    a_branch ()
  end else begin
    else_branch ();
    enter_trywith h;
    b_branch ()
  end
in
exit_trywith h;
r
```

The `enter_trywith` and `exit_trywith` constructions correspond to the pushtrap
and poptrap operations found in the bytecode and linear representations, but
there's no equivalent in the other intermediate representations.
It's easy for flambda2 (or other passes) to guarantee that the trap instructions
will be well-parenthesized, and also easy to guarantee that each piece of code
can only be executed under a single statically known trap context.

## This proposal: trap manipulations in the intermediate representations

Our main aim is for the native backend to be able to represent programs like the
one from above. We see three main changes that are needed for this to become
possible:

- First, the ability to have constructors for explicitly manipulating traps.
This was already proposed in pull request 1482, initially with instructions
that could be included in any sequence, which was rejected as lacking structure,
and later in the form that we're using in our prototype today: trap annotations
on static raise constructions. One of the advantages of this approach is that
checking for locality of static raises (i.e. that they do not cross a trap
boundary) can be done more easily, without carrying an extra parameter during
the traversal. This could simplify code in `Simplif` (although our prototype
does not propagate these changes so far, since those annotations are only
computed and carried starting from `Cmm`), in `Linearize`, and of course
flambda2 would be more complex too if we didn't have this concept.
One other advantage is that this introduces a strong invariant that the trap
context cannot change without entering a catch handler. It is not as strong as
we could have hoped for (for example, we do not have the property that every
expression has the same trap context at the start and end of its evaluation
anymore), but it will be useful in keeping additional trap context annotations
to a minimum.

- Second, the ability to declare exception handlers without pushing the
corresponding trap immediately. In our original pull request, this was done
by making the trywith constructs a sub-case of the catch construct, removing
the old trywith construct completely. In our current prototype, this is done
by adding a new 'kind' field to the trywith constructors, which can be
either `Regular` for normal trywith blocks, or `Delayed lbl` for the new
cases. The `lbl` argument allows later trap actions to refer to this handler.
This also enforces the invariant that only handlers declared specifically
as delayed can be used in the explicit trap actions, so when seeing a
regular trywith block it is guaranteed that all of its body will be executed
under a stack containing its handler.

- Third, trap context annotations on some constructors to simplify compilation.
In our prototype we don't have any annotations on Cmm, we compute the contexts
during `Selection` and use the computation to annotate delayed trywith
handlers, static catch handlers, and the context of the `next` field of
static catch instructions. Then in `Linear`, these contexts are used to
generate the correct code but not otherwise propagated (an earlier prototype
had all `Linear` instructions annotated with their context to check
consistency, this could easily be brought back if required).

## Prototypes

The approach described in this RFC is implemented as part of flambda2.
(https://github.com/ocaml-flambda/ocaml/tree/flambda2.0-stable)
There is also a stand-alone branch containing only the commits relevant to
this RFC at https://github.com/lthls/ocaml/tree/cmm_traps, based on a version
of trunk from October 2019.

## Other patterns that would benefit from this change

The following optimisations could be considered if this RFC is approved.
They have not been actually implemented though.

### Lifting of cold error paths from loops

```ocaml
while cond do
  let x = compute () in
  let y =
    try may_raise x with
    | exn ->
      expensive_debug_printing ();
      raise exn (* Or another exception *)
  in
  compute_again y;
done
```

Ideally we would like to lift the expensive, presumably cold handler out of the
loop to keep to loop code relatively small. With this proposal, the handler
can be defined out of the loop and only the trap instructions will remain
inside the loop. (This assumes that the handler does not depend on values
defined inside the loop, of course.)

### Placement of traps at strategic points

```ocaml
let b = condition () in
try
  if b then
    0
  else
    computation ()
with exn -> handler exn
```

In this example it would make sense to push the trywith block inside the `else`
branch. While here the result can be expressed with regular trywith blocks, the
analysis to determine whether it is allowed or not, and whether it would be
beneficial or not is much simpler using a representation with explicit traps
than with trywith blocks.

## Drawbacks, alternatives, unresolved questions

Drawbacks: Adds non trivial changes to a non trivial part of the compiler.

Alternatives: Unknown

Unresolved questions: The prototype has been used for several months and the few
issues that came up have been resolved. However, there is still space for
discussion around the implementation details like the form and place of the trap
and stack annotations.
