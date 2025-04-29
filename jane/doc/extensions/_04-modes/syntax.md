---
layout: documentation-page
collectionName: Modes
title: Syntax
---

# Modes and modalities

A mode expression is a space-delimited list of modes.

```
mode ::= locality | uniqueness | linearity | portability | contention
       | yield | statefulness | visibility

locality ::= `global` | `local`
uniqueness ::= `unique` | `aliased`
linearity ::= `many` | `once`
portability ::= `portable` | `nonportable`
contention ::= `uncontended` | `shared` | `contended`
yield ::= `yielding` | `unyielding`
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

Similarly, a modality expression is a space-delimited list of modalities.
As of this writing, a modality is just a mode, though it is conceivable we will
have modalities other than modes in the future.

```
modalities ::= modes
```

# Where modes can appear

To write a mode expression in program, it has to be prefixed by an `@` symbol.
It can
appear in several places in a program as described below.

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

# Where modalities can appear

Similar to a mode expression, a modality expression has to be prefixed by an `@@` symbol,
in one of several places in the syntax.

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

<!-- CR reisenberg for zqian: There are other signature items that can have
modalities, according to the parser:
  include S @@ modalities
  module (M @@ modalities) : module_type   (* but this does not work with [rec] *)
  module M : module_type @@ modalities  (* this form *does* work with [rec] *)
  module (M @@ modalities) = M2
  module M = M2 @@ modalities
I don't exactly know what these mean, so I have not documented them.
-->

## Kinds
Modality expressions can appear in [kinds](../kinds/intro), documented with the
kind syntax.

## Modules
Support for modules with modes is being worked on and not ready for wide adoption.
More documentation will come
as it becomes ready.

