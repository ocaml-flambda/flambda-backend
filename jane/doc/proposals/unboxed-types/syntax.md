# Surface syntax for unboxed types

This document describes surface syntax for working with unboxed types,
part of the broader [unboxed types](index.md) proposal, which you may
wish to read first.

# Surface syntax

Everything here is based on a
starting point from the [OCaml manual](https://v2.ocaml.org/manual/) and uses
the [notation
conventions](https://v2.ocaml.org/manual/language.html#ss:notations) used in the
manual, with two additions:

1. When presented in a rule, literals are written in
backticks `` ` ``. (The manual uses a font-change for this distinction.)

2. We write `+::=` to denote adding a production to an existing non-terminal.
When we write a plain `::=`, this text includes all such productions.

## Additions to existing syntax

### Types

We now accept type names suffixed with a `#`, by adding extra logic to the
lexer. No space is allowed between the type name and the `#`. We previously
considered putting the `#` in prefix, but this conflicts with an existing
notation used with class types.

In addition to new unboxed type constants, we also have unboxed tuples,
as are easily added:

```
typexpr +::= `#` `(` typexpr { `*` typexpr }+ `)`
```

We also add new syntax to allow users to declare unboxed types explicitly, such
as the following examples:

* `type t1 = #{ f1 : int; f2 : int32# }`
* `type t2 = #( | K1 | K2 of int )`
* `type t2' = #( K1 | K2 of int )`

There are no ambiguities here, and so we just straightforwardly add these
productions:

```
record-decl +::= `#` `{` field-decl {`;` field-decl} [`;`] `}`
type-representation +::= `=` `#` `(` [`|`] constr-decl {`|` constr-decl} `)`
```

Of course, rather than explicitly declaring unboxed types, users may simply
refer to the unboxed version of boxed types.  That is, rather than declaring
`t1` as above, one could declare the boxed type:

```
type t1 = { f1 : int; f2 : int32# }
```

And use `t1#` where the unboxed type is desired.

### Expressions

We wish to add support for the following example expressions:

* `#K`, where `K` is a constructor (to disambiguate)
* `#{ field1 = expr; field2 }` for unboxed record construction
* `#{ e with field = expr }` for unboxed record update
* `#( expr1, expr2 )` for unboxed tuple construction
* `e.#field` for unboxed field projection (to disambiguate)
* `e.#field <- e2` for unboxed field update (to disambiguate)
* `e.(.f1.f2) <- e2` for deep field update

There is no conflict with the existing `object#method` syntax, because the first
four examples above have something other than a lowercase letter following the
`#`. The other examples are structurally distinct from `object#method`.

We thus add the following productions:

```
constant +::= `#` constr
         |    `#` integer-literal
         |    `#` float-literal
         |    `#` `(` `)`

expr +::= `#` `{` ... `}` (* copy the existing syntax for record construction *)
     |    `#` `{` ... `}` (* copy the existing syntax for record update *)
     |    `#` `(` expr { `,` expr }+ `)`
     |    expr `.` field-projection   (* replaces existing expr.field *)
     |    expr `.` field-projection `<-` expr  (* replaces existing field update *)
     |    expr `.(` { `.` field-projection }+ `)` `<-` expr

field-projection ::= field
                 |   `#` field
```

The `#K(e1, e2)` syntax is covered by the addition to `constant`.

One might worry about the lexing of the `.#` in `e.#field`, but this is OK: an
infix symbol cannot begin with `.#`, and so the `.` and `#` will be lexed separately.

### Patterns

We wish to add support for the following example patterns:

* `#K`, where `K` is a constructor
* `#{ field1 = pattern; field2 }` against unboxed records
* `#( pat1, pat2 )` against unboxed tuples
* `#3L` against unboxed literals

Only the first causes any potential trouble, because of the conflict with
the existing `#typeconstr` pattern. But this trouble is easily resolved, as
types begin with a lowercase letter and constructors begin with an uppercase
letter. The others can be straightforwardly added:

```
pattern +::= `#` constr pattern
        |    `#` `{` ... `}`  (* copy the existing syntax for record patterns *)
        |    `#` `(` pattern { `,` pattern }+ `)`
```

Note also the additions to `constant`, above.

### Kind annotations

We also add the ability to write annotations on type declarations and
type variables, describing their kind. The BNF for `kind` is in
the [kinds](kinds.md) document.

```
layout-name ::= lowercase-ident    (* new non-terminal *)

(* new non-terminal *)
layout ::= [ module-path `.` ] layout-name
       |   `(` layout `)`
       |   layout `*` layout
       |   layout `+` layout
       |   layout attribute

(* `kind` defined elsewhere *)

(* allow annotations on type variables *)
typexpr +::= `(` `'` ident `:` kind `)`
        |     typexpr `as` `(` `'` ident `:` kind `)`
        |     typexpr `as` `(` `_` `:` kind `)`

(* allow annotations on type declarations *)
typedef ::= [type-params] typeconstr-name [`:` kind] type-information

(* allow annotations when binding type variables to the left of a `.` *)
type-binder ::= `'` ident                      (* new non-terminal *)
            |   `(` `'` ident [`:` kind] `)`

poly-typexpr ::= typexpr
             |   {type-binder}+ `.` typexpr     (* modified *)

type-param ::= [ext-variance] type-binder       (* modified *)

(* allow annotations when binding locally abstract types *)
abstract-type-binder ::= typeconstr-name        (* new non-terminal *)
                     |   `(` typeconstr-name [`:` kind] `)`

(* the following replace additions in Chapter 10.4 *)
parameter +::= `(` `type` {abstract-type-binder}+ `)`
          |    `(` `type` typeconstr-name `:` kind `)`
let-binding +::= value-name `:` type {abstract-type-binder}+ `.` typexpr `=` expr
class-field +::= `method` [`private`] method-name `:` `type` {abstract-type-binder}+ `.` typexpr `=` expr
            |    `method!` [`private`] method-name `:` `type` {abstract-type-binder}+ `.` typexpr `=` expr
```

The two productions for `parameter` allow e.g. `fun (type a : immediate) (x : a)
-> ...` without extra parentheses around the `a : immediate`.

A kind annotation on a type variable (the `typexpr` productions) imposes a
constraint on the type: `'a : kind` is accepted if the layout of
`'a` is a subkind of the `kind`. The annotations on the declaration
forms declare the kind of the variable. Accordingly, if we have

```ocaml
let f : ('a : immediate). 'a -> 'a = ...
```

the type variable `'a` will have kind `immediate`. Here is another example:

```ocaml
type ('a : immediate) t
let f : ('a : value). 'a t -> 'a t   (* rejected *)
```

This last example would be rejected: the universal type variable `'a` is
declared to have kind `value`, and `value` is not a subkind of the
`immediate` required to be a parameter to `t`.

However, recall that type variables in type declarations are unification
variables, not universal variables. Thus, reusing the same `t`, the following
would be accepted:

```ocaml
type ('a : value) t2 = 'a t
```

Effectively, the `'a` unifies with a fresh type variable that has kind
`immediate`. With this declaration, usage sites of `t2` would still require
a type argument of kind `immediate`, *not* `value`. This is similar to how
types with `constraint`s work, where a type variable might be refined as we
interpret the type definition.

#### Kind annotations on arbitrary types

The grammar above does *not* allow kind annotations on arbitrary types:

```ocaml
type t = (int : immediate)  (* not allowed *)
```

Instead, it just allows kind annotations on type variables, including type
variables introduced with `as`. There are two reasons for this restriction:

* When reading a type annotation like `let f : (t:value ...`, we have a parsing
  challenge: are we parenthesizing the beginning of a function type whose
  argument is labeled `t` and has type `value`? or do we have a type `t` whose
  kind is `value`? Reading the remainder of the type will always disambiguate,
  based on whether the next token is a `)` or a `->`, but the potential
  confusion is still there. Here is a telling example:

    ```ocaml
    type t
    type t2
    type value
    let f1 : (t:value) -> t2
    let f2 : t:value -> t2
    ```

    Here, `f1` is a function from a `t` (which has kind `value`) to a `t2`.
    On the other hand, `f2` is a function from a `value` (with label `t`) to
    a `t2`. The parses are unambiguous. Yet human programmers may be easily
    confused.

* Kind annotations on non-type-variables are rarely useful. Non-type-variables
  have an ingrained kind, assigned when the type is declared (or by the type
  system, for built-in type formers). The one case where such an annotation
  might be useful is this:

    ```ocaml
    (* in some library *)
    type t = A | B | C

    (* in your code *)
    let performant_function : (t : immediate) -> ...
    ```

    With the type declaration for `t`, `t` is an enumeration and will have
    an `immediate` kind. Yet perhaps the library author decides to add
    `D of string` to `t`. Now your function, expecting to operate on an
    `immediate` loses performance. The `: immediate` annotation allows you
    to get an error if the library author ever decides to do this.

    This situation does actually seem like a real use-case, but even without
    the `(t : immediate)` syntax, we can handle this:

    ```ocaml
    let performant_function : t as (_ : immediate) -> ...
    ```

    It's a bit ugly, but seems better than the alternative, and should remain
    relatively rare.

If this restriction proves onerous, we can lift it.