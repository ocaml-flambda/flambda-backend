# Modes and modalities
Currently a mode expression is simply a space-delimited list of modes.

```
mode := local | global | unique | shared | many | once | portable | nonportable
      | contended | noncontended | ..

modes := separated_nonempty_list(SPACE, mode)
```

For example:
```
local
local unique
```

Modes are in a dedicated namespace separated from variable names or type names,
which means you can continue to use `local` or `unique` as variable or type
names.

Currently a modality expression is simply a space-delimited list of modalities.

```
modality := local | global | ..
modalities := separated_nonempty_list(SPACE, modality)
```
Similarly, modalities are in a dedicated namespace.

# Where can they appear

To write a mode expression in program, it has to be prefixed by an `@` symbol.
Similarly, a modality expression has to be prefixed by an `@@` symbol. They can
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

## Function parameter
The rule of thumb is: wherever a type constraint `x : ty` is allowed, a similar
mode constraint `x @ modes` or type-and-mode constraint `x : ty @ modes` will be
allowed.
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
fun foo x y @ modes -> ..
```
We don't support `fun foo x y : ty @ modes -> ..`, because the parser can't
tell if the arrow is instead an arrow type.

## Expressions
```ocaml
(expression : ty @ local)
```
We don't support `(expression @ modes)` because `@` might be a binary operator.

## Record fields
Record fields can have modalities, for example:
```ocaml
type r = {x : string @@ modalities}
type r = {x : string @ modes -> string @ modes @@ modalities}
```

## Signature items
Signature items such as values can have modalities, for example:
```ocaml
val foo : string @@ modalities
val bar : string @ modes -> string @ modes @@ modalities
```

## Modules
Support for modules with modes is being worked on and not ready for compnay-wide adoption.
For the few use sites, the syntax should be self-explanatory. More documentation will come
as it becomes ready.
