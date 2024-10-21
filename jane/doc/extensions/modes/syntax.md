# Motivation
Previously we have a fixed set of mode names in the parser. Everytime we want to
add more modes we need to update the parser (and correspondingly ocamlformat).
This is too costy in the long term.

We introduce a new mode syntax that allows mode names (and mode expressions in
the future) to enjoy a dedicated name space. As a result, any identifier can be
used as a mode name.

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

Currently modalities have  the same syntax as modes:

```
modality := local | global | ..
modalities := separated_nonempty_list(SPACE, modality)
```

# Where can they appear

Mode expressions and modalities can appear after either `@` or `@@`. The former
is used in most cases, while the latter is used for disambiguition. For example,
in `val f : a -> b @ global`, the `@ global` could be annotating either `b` or
`f`. To avoid the ambiguity, we will write `val f : a -> b @ local @@ global`
where `local` is the mode of `b` and `global` is the modality on `f`.

## Function parameter
```ocaml
let foo ?(label_let_pattern = default) = ..
let foo ~(label_let_pattern) = ..
let foo (x @ modes) = ..
let foo (x : ty @@ modes) = ..
```
where `label_let_pattern` could be
```ocaml
x @ modes
x : ty @@ modes
x : a b c. a -> b -> c @@ modes
```
Patterns that are not directly function parameters can’t have modes. For
example:
```
let foo ((x, y) @ modes) = .. (error)
```

## Let bindings
```ocaml
let x @ modes = ..
let x : ty @@ modes = ..
let x : type a b. ty @@ modes = ..
let (foo @ modes) x y = ..
```

## Arrow types
```ocaml
a * b @ modes -> b @ modes
foo:a * b @ modes -> b @ modes
?foo:a * b @ modes -> b @ modes
```

## Record fields & Value descriptions
The two are similar in both semantics and syntax, so we will consider them
together.
```ocaml
type r = {x : string @@ global}
type r = {x : string @ local -> string @ local @@ global}

val foo : string -> string @ local @@ portable
```

## Expressions
```ocaml
(expression : _ @@ local)
```

# Future works
## Unzipped syntax
In the future we will support
```ocaml
type r = string -> string @@ local unique -> unique local
```
which corresponds to
```ocaml
type r = string @@ local unique -> string @@ unique local
```
This gets more complex when modalities are involved:
```ocaml
type r = { x : string -> string @@ (local -> unique) global}
```
which is equivalent to
```ocaml
type r = {x : string @ local -> string @ unique @@ global}
```
where `global` is a modality on the `x` field.

## Syntax sugar
Similar to
```ocaml
let foo x y : int = exp in
```
which unfolds to
```ocaml
let foo x y = exp : int in
```
We should allow
```ocaml
let foo x y @ local = exp in
```
unfold to
```ocaml
let foo x y = exp : _ @@ local in
```
