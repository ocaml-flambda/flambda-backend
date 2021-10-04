# Value approximations in Flambda 2: the Flambda type system

## Introduction

Despite the name, the structures and algorithms for tracking value
approximations in Flambda 2 is closer to an abstract domain in the sense of
abstract interpretation than to a type system.

However, unlike the general approach used in static analysis based on
abstract interpretation, here we're interested in carrying information for
a single-pass simplification (or optimisation). This means that we forbid
ourselves from going over the same piece of code more than once, and this
means that we can't use the normal widening-based approach to fixpoint
computations. This makes analysis of loops a bit difficult, but on the other
hand we will not have to bother with widening operations on our abstract
domains.

## Overview of the abstract domains

### The concrete domain

Roughly speaking, we will abstract sets of environments, where an environment
is a map from program names (variables or symbols) to values. Values range
over the full set of runtime OCaml values, and we also manipulate variables
that range over basic types such as fixed-width integers or floating-point
values. Every variable or symbol has a kind (derived from the type-checking)
that restricts the set of allowed values for it; all kinds are disjoint,
although some kinds allow annotations that further restrict the set of
allowed values.

#### The Value kind

The Value kind represents OCaml values. It allows both tagged integers
(machine integers with their last bit set to 1, which encode 63-bit or 31-bit
integers, depending on the architecture), and pointers to memory blocks.
No tagged integer is a valid pointer, and no pointer is a valid tagged integer.

In our model, the concrete memory blocks are a bit more abstract than the
actual in-memory representations. In particular, we do not take the GC bits
of the header into account, and consider that all reserved tags encode
completely different sorts of memory regions.
So we end up with:
- Regular blocks. These have a tag (between 0 and some maximal allowed tag),
a size, and a number of fields equal to their size each containing a value of
kind Value.
- Boxed numbers. These are parametrised by a number kind, and contain a single
value of that kind. In the actual runtime, the boxed numbers of kind Float
will be represented with a Double tag, while the other numbers will use Custom
blocks, but in our model they behave in the same way.
- Arrays. We're not very interested in arrays, but we assume that
they have a length, a kind (that can be either Value or Float), and a number
of fields equal to their length of the corresponding kind. We consider that
arrays are disjoint from regular blocks, even though their actual runtime
representation might end up the same.
- Immutable strings.
- Closures. These are a bit tricky, as a single closure block can define
multiple distinct valid pointers (unlike the other blocks, which always
define a single valid pointer). So a closure block contains a number of
functions, and a number of environment entries. The functions are indexed by
a closure identifier (or closure_id), a unique name generated during
compilation of OCaml functions. The actual contents of the function are only
important for inlining, so we'll assume here that it's roughly a pointer to
some piece of code, without any more details. The environment entries are
indexed by a closure variable. Closure variables are not regular variables but
identifiers generated during compilation of functions. Each entry in the
environment contains a single value of kind Value.
- Unknown blocks. This regroups all the values that are allowed in OCaml that
our model doesn't track, such as abstract or custom values, objects, or mutable
strings.

The Value kind also allows sub-kinds:
- The Tagged_immediate sub-kind corresponds to tagged integers.
- The Boxed_number sub-kinds corresponds to (pointers to) boxed numbers of the
given kind.
- The Block sub-kinds corresponds to (pointers to) regular blocks of the given
tag and size, with sub-kinds specified for each field individually.

#### The naked number kinds

Our model has four basic number kinds:
- Float, for 64-bit floating-point numbers
- Int64, for 64-bit integers
- Int32, for 32-bit integers
- Nativeint, for integers that can be either 32 or 64-bit wide depending on the
target architecture.

In addition, we also have an Immediate kind, for integer values that need to
have a valid representation as a tagged integer but are manipulated directly.

### The abstract domains

As usual with abstract domains, we're building our domains by composing
simple ones. Each abstract domain implements a join operation, a meet operation,
and a check for Bottom. In addition, they implement various specific operations
that are only relevant for these domains (such as abstraction of arithmetic
operations for numerical domains).

#### Numerical domains

We have a number of base domains that abstract sets of values for number
kinds, using finite sets and a Top element. These domains allow us to
propagate constant values at a reasonably low cost. Our current implementation
does not allow the use of relational numerical domains, but could fit any
non-relational domain (such as intervals or congruences) easily.
The domain operations are the usual ones on finite sets.

#### The Immediates domain

This domain, which tracks OCaml-like integers (so 31-bits or 63-bits wide), is
similar to the other numerical domains, although in the actual implementation
some details differ to handle some interactions with the relational domain
that keeps track of the tag values of blocks and whether a value is an
integer or a pointer.

#### Function pointers

The domain for function pointers is roughly split into two parts. One part
is a non-relational domain that maps code IDs to actual code fragments
(and associated data such as calling conventions) or Top or Bottom,
while the other part is a relational domain tracking the specialisation
relation between code fragments (which code was generated by specialising
which other piece of code).
The domain operations on the relation part are the usual ones (when the
relation is viewed as a set of pairs, join is the set intersection and
meet is the set union).
On the non-relational domain, the join between two different code fragments
is the last common ancestor of these fragments according to the specialisation
relation (or Top if there isn't one), while the meet between two different
code fragments is Top (in practice we never need the meet operation here).

#### Product domains

We have several kinds of product domains, the main difference being how the
elements are indexed. For regular blocks, we use finite products indexed by
integers between 0 and a given size (excluding the size). For closures, we
use closure_ids and closure variables as indices.
All of these domains use the standard component-wise operations.
Some other domains will sometimes need to combine two elements of product
domains with different indices; in this case we will assume that the other
domain will first extend or restrict one of the elements in the appropriate
way before calling the product operation on matching indices.

#### Row_like, disjunction over products

The Row_like domains are used to represent non-constant variant values and
closures. The basic version of these domains is the dual of the product domains
(a family of indices, to each index is associated an element which is itself a
value of an underlying domain, but while the concretisation of a product element
is the set of maps from indices to concretisation of the elements, for Row_like
the concretisation is the set of pairs (index, elt) where index is one of the
indices and elt is in the concretisation of the abstract element associated to
index).
The operations on this domain are the standard ones (component-wise join and
meet).

Our domain has two additional features:
- Support for abstract values where the index is unknown (or partially known).
- Support for constraints (or extensions) on indices.

The first feature is used for variant-like blocks. Our indices here are pairs
of a tag and a block size. But in practice when reading the field of a block
there are cases where the size of the block is not known (and in some cases,
even the tag can be unknown, though we've modified the compiler to reduce these
cases to a minimum). To represent the constraint corresponding to these field
reads, we use partially known abstract values. This complicates a bit the
domain operations; in particular, as the element associated to a given tag
and size is a product of the given size, when the size is only given a lower
bound the corresponding element must represent products of every size bigger
than the bound. In practice this is represented by a single product of the
minimal allowed size, so this product must be extended when joining or meeting
with compatible indices of different sizes. This also means that reduction
becomes an issue (we want overlapping indices to be coherent), and that the
join algorithm cannot be complete (roughly speaking, joining an element with a
single known tag t but unknown size, with an element with a known size sz but
unknown tag, will result in an abstract element that allows values that have
both a tag different from t and a size different from sz).
However this seems to work reasonably well in practice.

The second feature will be described in a bit more details in the sections
about extensions and the meet algorithm for the toplevel domain.

In the case of closures, the set of indices is a pair containing on one side
the closure ID of the corresponding function, and on the other side the set
of closure elements (closure IDs and closure variables) present in the
associated closure. The elements associated to such an index are products
indexed by the set of closure elements (in this product, closure IDs are
associated to a function pointer abstraction, and closure variables to regular
value abstractions).
Disjunctions are less useful here, but in a few cases this could transform a
call to an unknown function (indirect call) into a switch that dispatches to
the right direct call (provided one can find a discriminant to switch on).

#### Variants

The variants domain is a simple disjunction between tagged integers and
regular blocks. Tagged integers are represented using the domain of immediates,
and blocks are represented using the version of Row_like made for regular
blocks.
As with the Row_like domain, extensions can be associated to each member
of the disjunction.

#### Relational domains for projections

We have various relational domains tracking projections.
Block projections relate variables that represent regular blocks with variables
that represent one of their fields, closure projections do the same for closures
and environment entries, and boxed numbers also define projections between the
boxed value and its unboxed contents.

#### Get_tag and Is_int relational domains

The Get_tag domain holds relational information between values, of the form
"Value x is a regular block and its tag is equal to value y".

The Is_int domain is similar, but the form of the relation is "Value y is a
boolean that is true if and only if value x is a tagged integer".

In both cases, the value x is of kind Value and y is of kind Immediate.

#### Aliases

The other relational domain we use is a domain for tracking aliases.
This domain only tracks must-alias relations (so we can't prove that two given
variables do not alias using this domain), so in practice it is an equivalence
relation on "Simple" terms (variables or symbols or constants).

#### The combined domain: Typing_env

Typing environments are conceptually a reduced product of the Aliases,
projections, Get_tag and Is_int domains, with a non-relational domain that maps
names (variables or symbols in Flambda 2) to a kind and a non-relational
abstraction for sets of values of this kind.
For non-Value kinds, the abstraction is the obvious one based on the relevant
numerical domain, and for Value kinds it is a domain that holds either a
Variant abstraction, a Closure abstraction (using the version of Row_like for
closures), a boxed number of a given kind using the corresponding numerical
domain, or Top (in practice there is also a small amount of support for
arrays and immutable strings). So fundamentally a disjunction of these domains,
but with sets of values that can fit several underlying domains abstracted
directly to Top.

## Implementation details

### Relational domains and reduction

In practice, the implementation of the main non-relational domain is entangled
with the relational domains. The Get_tag and Is_int domains are not kept
separately, but instead the map from names to abstractions for the Immediate
kind can either hold a regular, non-relational abstraction or an equation
that states what its relation is to other values. This allows an easy way to
detect when reduction is needed. The equation is currently stored in one way
only (from the variable corresponding to the tag or is_int boolean we can
recover the block or variant it is associated to, but not the reverse), but
it would be possible to store the relation at both points.

The projection domains are also integrated into the relevant domains. For
instance, in the Row_like domain for blocks, the data associated to each field
is either a regular abstraction or a named projection. In the case of the named
projection, the abstract value associated with the field is no longer part of
the Row_like structure, but instead can be recovered by looking up the abstract
value associated to the name of the field in the overall map.

As a concrete example, here is how the state of a simple program might be
represented:

```ocaml
(* Program *)
let bool = (* opaque boolean expression *)
let block = (bool, bool)
(* State of the program here *)
```

```
Basic product
Env: bool -> {0; 1}
     block -> [| tag0, size2 -> field0: {0; 1}, field1: {0; 1} |]
Proj: block --field0-> bool
      block --field1-> bool
```

```
Actual implementation
Env: bool -> {0; 1}
     block -> [| tag0, size2 -> field0: (= bool), field1: (= bool) |]
```

The main advantage of this representation is that it transforms the reduction
process from a global one (when you update the value associated to a variable,
you need to explore all the relational domains to see if any other variable
can be updated) to a local one.

The impact can be shown if we augment the previous example with the following
sequence:

```ocaml
if (fst block) then
  (* State 1 *)
  if (snd block) then
    code_for_true
  else
    (* State 2 *)
    assert false
else
  if (snd block) then
    assert false
  else
    code_for_false
```

```
Basic product
State 1:
Env: bool -> {0; 1}
     block -> [| tag0, size2 -> field0: {1}, field1: {0; 1} |]
Proj: block --field0-> bool
      block --field1-> bool
State 2:
Env: bool -> {0; 1}
     block -> [| tag0, size2 -> field0: {1}, field1: {0} |]
Proj: block --field0-> bool
      block --field1-> bool
```

```
Actual implementation
State 1:
Env: bool -> {1}
     block -> [| tag0, size2 -> field0: (= bool), field1: (= bool) |]
State 2:
Env: bool -> Bottom
     block -> [| tag0, size2 -> field0: (= bool), field1: (= bool) |]
```

With the basic product version, a reduction step is necessary to find out that
the case is unreachable. With our implementation, at each step the necessary
reduction is performed, exploring only the parts of the environment that are
strictly needed.

The aliases relation is also partly implemented using the same trick.
The environment can associate to any variable either an abstract element, as
above, or an alias equation. However, while projections are assymmetrical,
the alias relation is symmetrical and this causes various kinds of problems:
cycles, different but equivalent representations...
To get around these issues, we use a total order on names (based in part on
binding times, meaning the variables defined earlier will compare smaller than
variables defined later). This allows us to define the canonical element of
a set of aliases, and we maintain the invariant that canonical elements will
always have abstract elements associated to them in the environment (and not
an alias equation), and non-canonical elements will always have an alias
equation associated to them. We also keep a full representation of the
equivalence classes, as this means the alias equations do not need to be
systematically updated to point to the canonical element when it changes.

### Flambda types

From the previous section, it appears that in many contexts we manipulate
either a normal abstract element or an equality encoding a relation.
We have then made this a general property, and introduced the datatype of
Flambda types.

A type (in this context) is made of a kind, and either a type-lifted
abstract element or an equality (to a program name or constant).
A type-lifted abstract element is an element of the abstract domain for the
relevant kind, except that some sub-elements are replaced by types.
Which sub-elements are replaced by types reflects which relations we're keeping
track of: for finite sets, for instance, we still use the raw numbers as
elements of the set, and for the Row_like disjunctions, only the contents
of the fields (or closure projections) are types, neither the indices nor
the individual blocks of the disjunction.

### Existential variables

While a typing environment is an abstraction of the concrete environment
at a given point in the program, we found it very useful to allow the abstract
version to have a bigger domain than the concrete one. The extra variables
are quantified existentially in the concretisation function, and this allows us
to represent additional information that couldn't be represented in a strict
version of the typing environment.

Take the following program:

```ocaml
let block =
  let bool = Random.bool () in
  (bool, bool)
in
(* State here *)
if (fst block) then
  if (snd block) then
    code_for_true
  else
    assert false
else
  if (snd block) then
    assert false
  else
    code_for_false
```

With only program variables allowed, the environment at the commented point
would be:

```
Env: block -> [| tag0, size2 -> field0: {0; 1}, field1: {0; 1} |]
```

This would prevent us from correctly marking the assertions as unreachable.

However, with existential variables, we could get the following state:

```
Env: ∃ bool -> {0; 1}
     block -> [| tag0, size2 -> field0: (= bool), field1: (= bool) |]
```

This can then lead to precise simplification of the rest of the code.

Note that as usual with existential variables, it is very important that the
number of existential variables is kept in check.
We achieve this by only allowing normal variables to be transformed into
existential variables (when they leave their scope), and agressively prune
variables that are considered unrelated to the program variables.

### Join algorithm

Our join algorithm can be called on environments with different domains (due to
the existential variables), so we first need to decide on the domain of the
join result. In practice, all the variables bound in both inputs (existential
or not) are kept in the output, and variables bound in only one side are
introduced in the other side with a special abstract value that acts as Bottom
for most purposes except that it is not related to other variables, and does not
make the whole environment Bottom (if one of the non-existential variables ends
up having this special value or being related to one of them, then the whole
environment becomes Bottom). This special kind of value is sometimes called
Poison in other settings.
In practice this means that existential variables from one side only are
re-introduced in the result with roughly the same type as on the side they were
present in.

After that, the join algorithms for the various abstract domains are
straightforward. The main source of additional complexity occurs when
encoutering types that are equalities.
Roughly speaking, when joining two types, if none is an equality the join
is the result of the join of the corresponding abstract elements. If only one
is an equality, then we use the full environment in which the equality occurs
to reduce the equality to an abstract element, and join it with the abstract
element from the other side.
When the two sides are equalities, then we are in a case where it's possible
that we could infer a relation implied by both sides, so we check if both
variables are aliased (in the joined aliases domain) and if so return an
equality with the corresponding canonical element. Otherwise, we fall back to
the default case of reducing to abstract elements and returning the abstract
element that is the join of both inputs.

### Meet algorithm and extensions

The meet algorithm is rather important in our domains, as we rely on this
operation for all the transfer functions that add constraints to the
environment (conditional statements, sub-kind constraints coming from the
OCaml type system, projections...).

And because our domain is conceptually a reduced product, the meet operation
has to perform a reduction as well. Here is a simple example illustrating the
issue:

```ocaml
type 'a t = T of 'a
let block = T x in
(* Before *)
let T y = block in
(* After *)
```

```
Basic product:
Before:
Proj: block --field0-> x

Meet with:
Proj: block --field0-> y

After:
Proj: block --field0-> x
Proj: block --field0-> y
```

Here we want to also learn that x and y are aliases, which corresponds to
doing a reduction between the domain of projections and the domain of aliases.

So unlike the join algorithm, which is implemented as a traversal of the
abstract elements that directly returns the joined result, for the meet
algorithm a single traversal cannot perform all the necessary reduction steps.
Instead, we've introduced a concept of environment extensions. These extensions
encode extra constraints that need to be applied to the main environment, and
because of that although in theory an extension is just another environment,
in practice it is linked with the environment it is generated for.
This allows the actual representation of extensions to be more lightweight
than normal environments.

In practice, all the meet functions in our domains return both an abstract
element, that is the result of the direct traversal of the inputs, and an
extension. For domains that recursively call the meet functions of other
domains, the extensions are combined: either with a meet (for product-like
domains) or with a join (for disjunction-like domains).
At the toplevel meet, this extension must be applied, so a new meet is
called between the first result of the meet and the extension.
This could easily diverge, but we enforce convergence by not allowing
domains with infinite descending chains, and ensuring that the meet
algorithms never generate extensions that do not strictly constrain the
environment.

### Meet and disjunctions: extensions in the environment

Consider the following program:

```ocaml
type t = A | B
type either = Left of t | Right of t
let f x =
  let y =
    match x with
    | A -> Left A
    | B -> Right B
  in
  (* State here *)
```

What should be the environment there ? We know that y is either a Left block
or a Right block, but for the contents of these blocks we need to choose whether
we want to track that the contents of the blocks is always equal to x, or to
remember which constant can occur for each case. Depending on the following
code, both approaches could be useful, but we cannot represent both at the same
time with our domains as described so far.

To solve this issue (and a number of other ones), we've introduced extensions
that can be associated to some of the elements of the disjunction domains.
These extensions encode additional constraints that are known to be implied
by the concretisation of this particular element of the disjunction, but may
not be true in all cases.
During a meet, whenever we know that only one of the disjunction elements
remains possible (because we've proven all the other cases to be Bottom), we
can then add the extension from this element to the extension returned by the
normal meet algorithm.

In our example, the abstract element for y will be a disjunction between two
elements. One is a block abstraction of the Left constructor, with a single
field whose contents is {0} (for A), and an extension corresponding to the
constraint x = 0. The other is similar, but with Right and B.
When we later match on y, each branch can then be simplified with the
knowledge of both the contents of the block and the value of x.
If we match again on x instead, then the constraint in each branch will be
incompatible with one of the extensions, and so each branch will know that
y can only correspond to a single constructor (Left or Right depending on
the branch).

However, we did put a few limitations on this feature.
The first one is that we do not yet have extensions associated to our domains
for finite sets. This means that if we changed the example so that it doesn't
involve the type either anymore, then we can't represent the exact set of
possible values:

```ocaml
type t = A | B
let f x =
  let y =
    match x with
    | A -> (A, B)
    | B -> (B, A)
  in
  (* State here *)
```

The second limitation is that we only add these extensions in a limited number
of cases, mostly during the meet on Row_like structures (extensions returned by
the individual meets on elements are associated to the corresponding element
in the result in addition to being joined with the extensions from the other
elements to form the final extension).
It would be possible to also infer extensions during joins (some details of
our implementation of environments make this less complicated than it sounds),
but we currently do not do that. (This means that we cannot actually simplify
the earlier example completely, as the extensions would have to be inferred
during the join after the match expression.)

### Typing_env_level: improved join algorithm

To avoid the cost of a full join of the environments at each join point, we
divide our environments into levels. Each level contains both a full copy of
the environment at this point and a structure (similar to an extension, but
allowing new declarations for variables) that only contains the changes from
the previous level. Whenever the analysis reaches a potential branching point,
a new level is constructed out of the previous one.

When joining two (or several) branches, we actually get an additional
environment that is guaranteed to be an ancestor of all the branches.
This means that instead of joining the full environments from the branches,
we can join the extensions coming from all the levels that were created
after the split from the ancestor. The join of these extensions is then added
to the ancestor environment to produce the result (as if doing a meet).
This is also the step where the existential variables are introduced: the
levels track when the variables are defined, so each variable that is defined
after the ancestor gets turned into an existential variable as described in the
associated section.

In the case of Flambda 2, join points occur on continuation handlers, which
take parameters; there is some amount of heuristics to correctly handle those
parameters, which are defined at the very end of the branches (as aliases to
the continuation arguments) but are not, strictly speaking, present in the
ancestor. Since they correspond to real program variables they must not be
existentially quantified.

## Structure of the code

### Folders

All folders are prefixed by `middle_end/flambda/`, the common root for the
Flambda-related files.

`types/`: The root folder for the type-related code.
This contains the basic functions to access the high-level structure of the
Flambda types, along with the `Flambda_type` module which exports the
relevant parts of the type system to the others parts of Flambda.

`types/template/`: Holds `flambda_type.templ.ml`, used to generate
`flambda_type.ml`. As it is not a valid `.ml` file by itself, it has been put
in a separate directory so that dependency computations do not see it.

`types/basic/`: A few simple utility modules used in the rest of the typing
code. As an example, the `Or_unknown` and `Or_bottom` modules are defined here.

`types/kinds`: Description of the kinds. All variables and symbols (in the
terms) and all types have a kind associated to them. In addition to the kind
`Value`, which represents valid OCaml values, there are various kinds
corresponding to unboxed numbers.

`types/type_of_kind/`: Implementations for the main abstract domains (one for
each kind).

`types/structures/`: Helpers for building specific abstract domains out of
basic ones. The main examples are `Product`, used to build cartesian products
with parametric indices, and `Row_like`, used to build indexed disjunctions
(with parametric indices).

`types/env/`: Abstractions over mappings from variables (and symbols) to
values. The `Typing_env` module corresponds to the environments that will
be used during simplification.

### Recursive modules

Many of the modules are mutually recursive. The main recursive dependency
is that `Type_grammar` (the base module for types) depends on `Typing_env`.
This in turn makes all modules that depend on `Type_grammar` but are used
for defining `Typing_env` part of the recursive definition.
The reason why we need that recursion is that the types (of type
`Type_grammar.t`) describe the information we have about some value in
a given environment. Since this information is relational (either in the
form of aliases, such as "this value is equal to the contents of this
variable", or in the form of compound values, such as "this value is
constructed by combining these other values"), then a type is only valid in
a given environment, and so many functions for manipulating a type need
access to the environment in which it's used.

