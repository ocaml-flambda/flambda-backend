# Translating Flambda2 to Cmm

One of the reason behind the flambda2 (in opposition to working on upgrading
flambda1), was to go from lambda to flambda and then directly to cmm, whereas
flambda1 goes from lambda to flambda to clambda and then to cmm, reusing the
existing translation from clambda to cmm. However, that gave very little control
to flambda in terms of unboxing, since most optimizations relating to unboxing
happens during the clambda to cmm translation.

The translation from flambda2 to cmm is currently named `un_cps` for historical
reasons (because it `un`does the `cps` form of flambda2), but it is expected to
change toward a less obscure name at one point.


## Overview

See [this file](term_language.md) for a description of the flamdba2 language,
and [the cmm doc](cmm.md) for an overview of the cmm language.

This section provides an overview of the main tasks of the `un_cps`
translation. First these "tasks" are listed, and then are explained in details,
together with their motivations and requirements. The technical aspect of how
these tasks are implemented will be discussed in the following section).

The main challenges of the translation are:
- translate some abstract concepts into concrete ones, for such things as
  tagging, boxing, "big" and "small" values (aka Int64 and Int32 depending on
  architecture), etc...
- translating the continuations of flambda2 into the control flow structures of
  the cmm language
- substituting some let-bindings from the flambda2, so as to generate (at least for
  arithmetic) more nested expressions which are better optimized by the
  construction function for cmm
- assigning unique offsets to all elements in set of closures (closures and env
  vars), while taking into account offsets assigned by other compilation units
- eliminate unused env vars for sets of closures, sometimes resulting in a set
  of closure that can be statically allocated
- try and avoid using a global mutable state, but still try and re-use most of
  the existing code as possible


### Low-level implementation of high-level concepts

The points here (tagging/unboxing, and big/small values) are actually
tasks that are also performed by cmmgen (because they are properties of the
differences in representation between middle-end and backend).

#### Tagging and boxing

As it is the turning point between the middle-end and the backend of the
compiler, some operations/concepts that are abstract in earlier passes need
to be made far more explicit.

First is to introduce all the concrete tagging/untagging and boxing/unboxing.
Flambda2 already distinguishes tagged and untagged integers as well as boxing
(in order to be able to reason about those and eliminate them when possible),
however those are still abstract at the flambda2 level, whereas at the Cmm level
and lower these operations are explicit, e.g.:
- cmm only operates on machine integers, so tagging explicitly has to be
  translated in terms of arithmetic (left shift and addition for tagging for
  instance)
- allocations still benefit from some builtin handling, but block headers
  are integers from cmm onwards, so concrete integer values for the headers
  must be computed (whereas before, it was only the tag info)
- field projection must be translated as a memory address computation
  (pointer of the value + offset of the field, which can depend on the integer
  size for the targeted architecture), and then a memory read
- similarly, field assignements must be expressed as memory address
  computation followed by a memory write

One good point compared to cmmgen is that we can assume that the flambda
simplifier will already have unboxed things that can be unboxed, so contrary to
`cmmgen`, `un_cps` does not need to do the work of trying to find unboxing
optimizations.

#### "Big" and "small" values

Another abstract concept which become concrete is to split "big" values and
fill "small" ones; indeed, from cmm onwards, all manipulation occurs on values
that fit into registers, hence values that do not fit *exactly* into one
register must be handled with special care in order to provide the expected
semantics.

"big" values are things that cmm need to manipulate explicitly, but which do not
fit in a single register; typically 64-bit integers on 32-bit archs.  Usually,
such values will not be manipulated direclty by cmm, and instead be handled by C
functions, but in some cases, such as when providing to, or getting back a raw
value from a C function, cmm need to be able to split one big value into
register-wide things, or to combine back parts of it into a single value.
Currently, this only applies to 64-bits integers when compiling for a 32-bit
architecture, but these manipulations may one day be used to handle unboxing of
ocaml values across functions.

"small" values are values which are strictly smaller than register, and whose
semantics usually need this smaller value space; typically, these are 32-bit
integers on 64-bit platforms.  For these values, `un_cps` need to choose one
encoding into 64-bits registers, and translate the abstract operations from
flambda2 on these types to concrete operations on registers in a way that
preserve the expected semantics.


### Translating away the continuations

Flambda2 makes heavy use of continuations in order to represent the control
flow of programs, which helps when inlining and manipulating the program AST.
However, Cmm uses simple sequential statements, and static raise/catch (which
amount to correctly scoped jumps). Continuations could very naturally translate
to static raise/catchs, but such jumps may incur some performance penalty in
some cases, thus it is not desirable to always translate continuations using
jumps. Instead, the translation will try and inline continuations that are
used exactly once, thus producing simple sequential code in those cases, and in
other case will use static raise as the translation is rather perfectly
adequate.


### Let-binding substitution

This is actually the most important optimization to perform when translating to
cmm. For various reasons, it is better to generate nested expressions in cmm
compared to generating A-normal forms; some of these reasons are:
- constructions functions for cmm code are made to optimize nested arithmetic
  computations (through constant propagation, re-ordering of commutative
  operations, and some algorithmic optimizations of complex operations). This is
  very important as it completely eliminates the cost of tagging/untagging of
  intermediate values in non-trivial arithmetic computations (e.g. in `(a+b)+c`,
  the tagging and untagging of `a+b` is eliminated during translation to cmm by
  these arithmetic optimizations).
- if-then-elses are much better translated from cmm to mach by selectgen when
  the condition of the if-then-else has at its top-level a builtin comparison
  operator (such as (=), (<), etc...). Indeed, in such cases the comparison can
  be directly translated as a processor comparison and then condition jump.
  Conversely, when this is not the case, the compiler pipeline will
  automatically add a (= 0) comparison, resulting in a few more instructions.

Thus `un_cps` does the following:
- remove pure and co-effectful-only expressions which are never used
- substitute pure and co-effectful-only expressions which are used exactly once
- if the option is set, substitute effectful expressions which are used exactly once

Some current limitations of let-binding substitution include:
- no substitution across let rec const:
```
let x = <some pure primitive> in
let rec cont k1 y = <some code here, without any reference to x> in
let z = x + 1 in
apply_cont k1 z
```
in such case, no substitution will take place.


### Closure offsets

Flambda2 makes abstract manipulations on elements of sets of closures, namely
closures and environment variables. Thus the same closure or env var can occur
in more than one set of closure (e.g. if some function is inlined in a set of
mutually recursive function, then that function's closure may now also occur in
this set of closures, in addition to its own set of closures). However, flambda2
relies on the fact that given a set of closures, it is possible to project a
closure (or env var) from it in a static manner[1]. This means that every
closure and env var must be assigned a unique offset so that in any set of
closures, a closure or env var will be found at its offset (it if belongs to the
set of closure). Additionally, some closures or env vars may have assigned offset
from other compilation units, since flambda2 can perform cross-module inlining.

We thus need an algorithm to assign unique offsets to all closures and env vars,
that also takes into account as input a series of constraints on already
assigned offsets. Note that this algorithm cannot be complete as some problems
may be unsatisfiable, for instance consider the following constraints: the set
of closures A contains closures f, g, and h. This may occur e.g. if f and g are
functions from another module which have been inlined in the definition of h.  f
and g are closures defined in other modules, each the only one in its own set of
closures, and thus both have an already fixed offset of `0`. In this case, it is
impossible to fit all three closures in A. For now, we have decided that on such
impossible cases, flambda2 should fail at compilation, and we'll take a closer
look at situations that might make this happen in order to see if it is really
interesting to support such cases. Actually, given that flambda2 currently does
not inline sets of closures across modules anymore, and thus all sharing are
restricted to the current module, there should not be any instance where
the offset computation can fail.

Currently, `un_cps` implements a greedy algorithm for filling out sets of
closures and assign offsets. It iterates over closures, assigning the lowest
offset that is free in all sets of closures where it appears, and then does the
same for all env vars. While not optimal as it may leave gaps, this should
result in the regular solution for when there is no sharing between sets of
closures (thus preserving the behaviour of Closure), behave well in cases
with reasonable sharing (mainly when sharing occurs between sets of closures
defined by the same module), and provide a best effort for exotic cases with
sharing across modules, all the while having reasonable complexity.


[1]: Actually, Flambda1 had a fallback that allowed dynamic projection, where
the projection would, at runtime, look at the set of closure in order to
determine which field to read, but we'd very much like not to have that in
flambda2.

## Concrete details

### Organization of the code and main control flow of the translation

`un_cps` translate flambda2 expressions using two traversals of an flambda2
body.

The first traverssal iterates over all sets of closures defined by the body.
This allows to assign offsets to all members of sets of closures (taking into
account constraints due to a potential sharing of closures of env vars beetween
sets of closures), using the code in `un_cps_closure.ml`.

After that, `un_cps` operates the second, and main traversal of the
expression, accumulating information (about continuations, substitutions, etc)
in an environment (defined in `un_cps_env.ml`), and then returning two
values: the resulting cmm expression, and a static result that contains all
the constants that should be pre-allocated (in comparison, `cmmgen` uses a global
reference to store that information during translation). This static result
(defined in `un_cps_result.ml`) will be grown by the translation, by either
`let_symbol` bindings or sets of closures bindings. Translations of flambda2
expression that can result in pre-allocation of static constants go through
the `un_cps_static.ml` file.

### Tagging and unboxing

In the code, this is one of the rather easy points. Flambda2 has a notion
of kinds, which offser enough information about the memory representation
of values, e.g. boxed integers are identified with their size, tagged integers
are differentiated from untagged integers, ... . Together with information
about what the cmm primitives expect (i.e. which primitive expect or return
a tagged integer, and which deal with untagged integers), this offers a
rather comprehensive way for `un_cps` to know when or where to tag/untag
things. See the following for more details:
- `middle_end/flambda/terms/flambda_primitive.ml` for the data about kinds
  consumed/returned by flambda primitives (particularly the
  `arg_kind_of_*_primitive` and `result_kind_of_*_primitive`)
- [the cmm doc](cmm.md) for info about the primitives in cmm
  (this is still a TODO)

Unboxing is also straightforward: as mentioneed earlier, flambda2 already
takes care of identifying and applying all the unboxing we want, so `un_cps`
only introduces the boxing and unboxing as needed, using the same information
as for tagging/untagging.

For the concrete implementation, `un_cps` mostly re-uses code from
`cmm_helpers`.

### "Big" and "small" values

We'll describe here the way "big" and "small" values are translated, since
the rest is relatively simple and can be understood directly from the code.

#### Int64 on 32-bit archs

Since in most cases, these are handled by C functions, the C convention is used
throughtou all the code: on a 32-bit arch, an Int64 is split into its lower
bits and its higher bits, and then carried on two registers.  Handling these
is relatively straightforward since the low-level operations are already
written in `cmm_helpers`.

A difference between `cmmgen` and `un_cps` is that flambda2 has explicit
box/unbox operations on these, and as such, where `cmmgen` is in control of the
boxing and thus most of the time generate calls to function that handle the
boxed version of Int64s, `un_cps` actually has to mostly deal with unboxed
Int64, and generate calls that handle the unboxed variant.

Currently, there may be some cases in `un_cps` that will raise a fatal error
because there was not yet an unboxed version of the required function in the
runtime.

#### Int32 on 64-bit archs

Here there is more choices to be made, since almost all operations on int32
under 64-bit archs are translated down to 64-bit arithmetic. There are a few
choices that can be made (only care about the lower 32-bits, zero-extend the
integer, ...), but currently we choose to sign-extend 32-bit integers.
Basically, a 32-bit int is represented as a 64-bit that *must* stay in the
range of 32-bits integers. Hence, a 32-bit arithmetic oepration will often be
translated as the corresponding 64-bit operation, followed by a sign-extension
(sign extension is composed of two shifts in cmm that should later in the
pipeline be simplified into a single sign-extension instruction on
architectures that support it). Some operations can avoid the sign-extension
as it can be demonstrated that the result will always be within the acceptable
range (division, modulo, etc...), but these were already implemented in
`cmm_helpers`.

### Continuations

As stated before, continuations are translated in one of two ways:
- non-recursive continuations that are used exactly once are inlined
- all other continuations are translated using static jump/catches

Inlined continuations are first stored untranslated (i.e. the handler of
the continuation is stored as a flambda2 expression), so that when it is
inlined, the translation of the expression can make use of substitutions.

Non-inline continuations (or jumps), are pretty straight-forward since cmm
has a notion of static jump and catch. The only thing to pay attention to
is to check the potential trap actions that can be associated to a continuation
call. But again, these are relatively simple to translate since cmm now has
a trap mechanism to mirror that of flambda2.

### Let-binding substitution

This is the most complex part of `un_cps` and also the part most sensitive
to changes (even innocuous changes in `un_cps` might break becasue of this).
The goal is to substitute let-bound variable's bodies during translation, because
it allows for more optimizations. There are two technical challengs
with this:
- keep the correct order of evaluation (with respect to effects and co-effects)
- yet still allow for some re-ordering of evaluation, such as pure computations
  across other computations (as long as it doesn't cross into a recursive
  continuations, in which case the expression might be valuated more than once).

Most of the code to deal with this is in the definition of the translation
environment in `un_cps_env`: the environment accumulates all let-bindings
(whether that can be substituted or not), since we have to be able to
substitute through a non-substitutable binding. A nice consequence of that is
that translating a let-binding can then be tail-rec, which is very useful.

Let-bindings are stored in the environment in a few structures:
- pure bindings are stored in a dedicated map
- all other bindings are stored in "stages". Intuitively a stage is a set of bindings
  that can all be commuted with each other, while keeping the same semantics. This
  means that a stage is either: a set of let-bindings which only have co-effects,
  or a single effectful binding.
These two structures are used to store enough information to perform substitution
if needed. Additionally, another map is used to relate all flambda2 variables to
a corresponding cmm variables. This mapping is used when `un_cps` decides not to
substitute a variable's body.

When a let-bound variable has to be translated, a special env lookup is used,
in order to determine whether the variable's body can be subtituted. On
variables that are bound to a pure expression, the dedicated map is used, if
the variable is marked as substitutable, else the generic mapping from flambda2
variables to cmm variables is used. On variables bound to a non-pure body, the
last and current stage is looked up, if the variable is in it and can be
substituted (i.e. has been marked as substitutable when inserting it in the
env[2]) the substitued body is returned, else the generic mapping is used. Is
the variable has been substituted, it is then removed from the current stage
and if this results in the current stage being empty, the previous stage is
restored as the current stage.

[2]: this is done by the caller of `Env.bind_variable`, on variables that are used
exactly once.

This process allows to correctly substitute let-bound variable's bodies when
adequate, but for variables not substituted, the let-binding information is
"only" in the env: when translating the let-binding, all that is done is add the
let-binding info in the env, thus if the variable's body is not substituted, it
simply stays in the env. Hence, the env needs to be "flushed" of its accumulated
let-bindings at some precise points to ensure that all let-bindings are correctly
translated and present in the generated cmm code. This presents two challenges:
- correctly identify all points where the env needs to be flushed (else variables
  may be unbound)
- the env must be treated as a somewhat linear type, more precisely an env can
  only be duplicated right after a flush and before any new variables are bound
  in it (else this may result in duplicated bindings being generated).

The points at which the env should be flushed can basically be understood as any
operations that cannot be expressed as a let-binding, so any control flow,
including non-inlined continuations, but excluding function calls (since these can
be seen as effectful let-bindings).

### Closure offsets

Closure offset computation is done in two steps. First is to accumulate all
information about sets of closures and members of these in ia mutable
structure: by iterating over all sets of closure sin the flamdba2 program body,
we build a record for each set of closures. These records contain a list of
closures and of env vars that belong to the set of closures; we will refer to
them as slots. A slot is a member of a set of closures, currently a slot is
occupied by either a closure or an env var.  Additionally, each slot record all
the sets of closures in which it appears (hence the need for a mutable data
structure, since there can be a lot of cycles). Additionally, earlier compilation
units are checked for closure and env vars that already have assigned offsets
(through the `Exported_offsets` interface).

Once that first pre-processing phase is done, offsets can be assigned to slots,
while taking care for no slot to overlap. This is currently done in a rather
simple and greedy manner: we first iterate over all closure slots, and for each
slot we find the first available offset where it can be placed (by looking at
all the sets of closures where it occurs). We then do the same for env vars slots.

As stated before, this algorithm has the nice property to be simple and rather
reasonable in terms of complexity. However, it might generate sub-optimal offset
assignments in some cases. Further improvements could be made by either using
heuristics to choose the order of offset assignment (e.g. first assign offsets to
slots that have the most or least occurrences, ...), or use/design an algorithm
to solve this problem optimally (for a notion of optimality yet to be determined).

Pending modification (see PR#269 in the flambda repo), each set of closures
will be checked to ensure that no closure slot occur after an env var slot,
and to compute the start of environment needed for the representation of closures
starting from ocaml 4.12. The check is done by keeping for each set the first
offset that is free after the last closure slot, and the first offset used by
an env var slot.

