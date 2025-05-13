---
layout: documentation-page
collectionName: Modes
title: Syntax
---

# Modes

A mode expression is simply a space-delimited list of modes.

```
mode ::= locality | uniqueness | linearity | portability | contention
       | yield | statefulness | visibility

(* these are the modal axes: *)
locality ::= `global` | `local`
uniqueness ::= `unique` | `aliased`
linearity ::= `many` | `once`
portability ::= `portable` | `nonportable`
contention ::= `uncontended` | `shared` | `contended`
yield ::= `unyielding` | `yielding`
statefulness ::= `stateless` | `observing` | `stateful`
visibility ::= `read_write` | `read` | `immutable`

modes ::= mode
      |  mode modes
```

For example:
```
local
local unique
```

Modes are in a dedicated namespace separated from variable names or type names,
which means you can continue to use `local` or `unique` as variable or type
names.

A mode expression actually contains a specification for each modal axis, whether
you have written a choice for that axis or not. For axes that are omitted, the
so-called *legacy* modes are used instead. The legacy modes are as follows:

```ocaml
global aliased many nonportable uncontended unyielding stateful read_write
```

It is an error to specify more than one mode along the same axis in one mode
expression.

To write a mode expression in program, it has to be prefixed by an `@` symbol.
It can appear in several places in a program as described below.

## Arrow types
```ocaml
a * b @ modes -> b @ modes
foo:a * b @ modes -> b @ modes
?foo:a * b @ modes -> b @ modes
```

One should be careful about the precedence of `@`. In the following example,
`modes` annotates `c`.
```ocaml
a -> b -> c @ modes
```

You can use parentheses to override precedence. In the
following example, `modes` annotates `b -> c`.
```ocaml
a -> (b -> c) @ modes
```

## Function parameters

The rule of thumb is: wherever a type constraint `x : ty` is allowed in a
function parameter, a similar mode constraint `x @ modes` or type-and-mode constraint `x :
ty @ modes` is allowed.

```ocaml
let foo ?(x : int @ modes = default) = ..
let foo ?x:((a, b) : int @ modes = default)
let foo ~(x : int @ modes) = ..
let foo ~x:((a, b) : int @ modes) = ..
let foo ((a, b) : int @ modes) = ..
```

Patterns that are not directly function parameters can’t have modes. For
example, the following is not allowed, because `x @ local` is not a function
parameter (but the first component of one).
```ocaml
let foo ((x @ local), y) = ..
```

Again, one should pay attention to the precedences. In the following example, the first
`modes` annotates `'b`, while the second annotates `x`.
```ocaml
let foo (x : 'a -> 'b @ modes) = ..
let foo (x : ('a -> 'b) @ modes) = ..
```

## Let bindings
The rule of thumb is: wherever a type constraint `pat : ty` is allowed, a similar
mode constraint `pat @ modes` and type-and-mode constraint `pat : ty @ modes` will be
allowed.
```ocaml
  let a : int @ modes = 42 in
  let (a, b) : int @ once portable = 42 in
```

## Functions and their body
You can specify the mode of the function itself:
```ocaml
let (foo @ modes) x y = ..
```
You can also specify the mode of the function body:
```ocaml
let foo x y @ modes = ..
let foo x y : ty @ modes = ..
fun x y @ modes -> ..
```
We don't support `fun x y : ty @ modes -> 42` due to a limitation in the
parser.

## Expressions
```ocaml
(expression : ty @ modes)
```
We don't support `(expression @ modes)` because `@` is already parsed as a binary operator.
However, you can write `(expression : _ @ modes)` if you do not want to constrain the type.

## Modules
Support for modules with modes is being worked on and not ready for wide adoption.
More documentation will come
as it becomes ready.

# Modalities

Similar to modes, a modality expression is simply a space-delimited list of
modalities.  As of this writing, every modality is also the name of a mode,
though it is conceivable we will have modalities other than modes in the future.

```
modalities ::= modes
```

<!-- CR reisenberg: This should be moved to a page about the semantics
of modalities, instead of here in the syntax page. But we don't have
such a page now, so it's here for the time being. -->

Modalities are used to describe the relationship between a container and an
element in that container; for example, if you have a record field `x` with
a `portable` modality, then `r.x` is `portable` even if `r` is `nonportable`.
We say that the `portable` modality applied to the `nonportable` record mode
produces the `portable` mode of the field. 

Modalities work differently on future axes vs. past axes. On a future axis, the
modality imposes an upper bound on the mode (thus always lowering that
mode). Thus applying the `portable` modality to a `nonportable` records yields a
`portable` field, because `portable < nonportable`. On a past axis, the modality
imposes a lower bound (thus always raising that mode). Accordingly, a
`contended` modality applied to an `uncontended` record yields a `contended`
field, because `uncontended < contended`.

Any axis left out of a modality expression is assumed to be the identity
modality. (When a modality is the identity, then the mode of a field is the same
as the mode of the record.) For future axes, this would be the top mode; for
past axes, this would be the bottom mode. These are the identity modalities:

```ocaml
local unique once nonportable uncontended unyielding stateless immutable
```

Note that a legacy mode might or might not be the same as the identity modality.

It is an error to specify more than one modality along the same axis in one modality
expression.

All modality expressions are prefixed by an `@@` symbol,
in one of several places in the syntax, as described below.

## Record fields
Record fields can have modalities:
```ocaml
type r = {x : string @@ modalities}
type r = {x : string @ modes -> string @ modes @@ modalities}
```

## Constructor fields
Constructor fields can have modalities:
```ocaml
type v =
  | K1 of string @@ modalities
  | K2 of string @@ modalities * int array
  | K3 of string @@ modalities * int array @@ modalities
  | K4 of (int -> int) @@ modalities   (* parentheses around functions are required even without modalities *)
  | K5 : string @@ modalities -> v
  | K6 : string @@ modalities * int array @@ modalities -> v
  | K7 of { x : string @@ modalities; y : string @@ modalities }
  | K8 : { x : string @@ modalities; y : string @@ modalities } -> v
```

## Signature items
A `val` signature item can have modalities:
```ocaml
val foo : string @@ modalities
val bar : string @ modes -> string @ modes @@ modalities
```

Similarly, so can an `external` signature item:
```ocaml
external id : 'a -> 'a @@ modalities = "%identity"
```

A signature can have default modalities that each item can override:
```ocaml
sig @@ portable
val foo : string (* have portable modality *)
val bar : string -> string @@ nonportable (* not have portable modality *)
end
```
These default modalities must be the first item in the signature.

An .mli file is like a signature, but we do not write the `sig` and the
`end`. Accordingly, you may put `@@ modalities` as the first item in an .mli
file.

## Kinds
Modality expressions can appear in [kinds](../kinds/intro), documented with the
kind syntax.

## Modules
Support for modules with modes is being worked on and not ready for wide adoption.
More documentation will come
as it becomes ready.

