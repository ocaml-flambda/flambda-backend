# Syntax for kind annotations

[overview]: index.md
[manual]: https://ocaml.org/manual/latest/language.html
[unboxed types]: ../unboxed-types/index.md
[nullability]: non-modal.md#nullability
[externality]: non-modal.md#externality

This page describes user-facing concerns about kind annotations. You may
want to read an [overview][] of the kind system first.

## Summary

You can write kind annotations in several places. In type declarations, they can
appear both on the type being declared itself, and on the type parameters.
For example:

```ocaml
type ('a : <kind1>) t : <kind2>
```

Kind annotations can also appear anywhere a type variable or underscore is legal
in a type expression, as in:

```ocaml
let foo (x : ('a : value mod portable)) (y : (_ : bits32 & float64)) = ...
```

And on locally abstract types:
```ocaml
let foo (type (a : value mod portable) (b : bits32 & float64)) (x : a) (y : b)
  = ...
  
let bar (type a : value mod portable) (x : a) = ... 
  (* no extra parentheses needed when there is only one locally abstract type *)
```

It's legal to omit the `mod ...` or `with ...` portion of the kind when you only
want to specify the layout (as we did in a couple of these examples). The bounds
of unspecified axes are the maximum on those axes.

## Syntax examples

Here are some example kinds:

```ocaml
value
any
value mod contended portable
float64 mod everything
value mod contended portable with 'a with 'b
immutable_data with 'a t with int t with 'a @@ portable with t2
               @@ contended unyielding
```

Here are some examples of where kinds can appear in OxCaml code:

```ocaml
val id_float64 : ('a : float64) -> 'a
val id_any : ('a : any). 'a -> 'a
val length_float64 : (_ : float64) array -> int
val ensure_t_is_immediate : t as (_ : immediate) -> unit

type 'a list : immutable_data with 'a
type ('a : any) array : mutable_data with 'a
type (!+'a : float64) t
type (_ : value, _ : float64) t

let f : ('a : float64). 'a -> 'a = ...
let f : type (a : float64). a -> a = ...
let f (type a : float64) (x : a) = ...
let f (type a b (c : float64)) (x : a) (y : (b, c) t) = ...
```

For full information about the syntax of kinds and where to use them,
please see the [syntax reference](#syntax-reference).

## Abbreviations

Kinds are built by starting with a layout and then adding _bounds_ (as described
in the [overview][]). However, some kinds are so commonplace that we provide
abbreviations in these cases. We expect to eventually allow users to write their
own kind abbreviations.

In all cases but one, the layouts described here are the basic layouts also
described in our [unboxed types][] documentation. The exception is around
`value`: the kind `value_or_null` is really the primitive layout here (with no
extra requirements), while the much more common `value` additionally requires
types not to admit any values represented by all `0` bits. (See also the
documentation on [nullability][].) We thus give the non-primitive `value`
the shorter name.

Beyond just kind abbreviations, we also have one bounds abbreviation:
`everything`.  Naturally, `everything` describes a maximal amount of mode
crossing.

The abbreviations defined in the language are as follows:

* `everything = global aliased many contended portable unyielding immutable
                stateless external_ non_null non_float`

    Values whose types have kinds that include `mod everything` do not...

    * ... allocate memory: this allows them to mode-cross to `global` and be
      `external_`.
    * ... contain functions: this allows them to mode-cross to `many portable
      unyielding stateless` (all of which only affect functions).
    * ... support mutation: this allows them to mode-cross to `contended
      immutable`.
    * ... equal the bit pattern comprising all `0`s: this allows a bound
      of `non_null`.
    * ... point anywhere: this allows mode-crossing to `aliased` (which would
      confuse update-in-place of the pointed-to memory, if there was any) and a
      bound of `non_float` (because the value is not a pointer to a
      floating-point block).

    Using `mod everything` is appropriate for simple data represented directly,
    like `int` or `float32#`.

* `any_non_null = any mod non_null`

* `value = value_or_null mod non_null`

    This is the kind of typical OCaml values, as they have been before OxCaml
    was invented. When a kind is unknown (but `any` would not be an appropriate
    choice), we default kinds to be `value`. This defaulting action is described
    in further detail in the sections below.

* `immediate = value mod everything`

    This is the kind of `int` and types like it (including enumerations and
    `bool`). It is the OxCaml equivalent of OCaml's `[@@immediate]` attribute.

* `immediate64 = value mod global aliased many contended portable unyielding
                 immutable stateless external64 non_float`

    This is just like `immediate`, but applies only on 64-bit machines. On a
    32-bit machine, value whose types are `immediate64` may be
    heap-allocated. This kind classifies the `Int63.t` type in `base`. (It is
    *not* the kind of `int32`, which is unconditionally heap-allocated on all
    platforms.)

    It is still safe for these values to mode-cross, even on 32-bit platforms,
    due to the following:

    * Stack allocation is not implemented on 32-bit platforms, so mode-crossing
      to `global` is safe.
    * Something storable in 64 bits without indirection cannot support
      update-in-place, so mode-crossing to `aliased` is safe.
    * Something storable in 64 bits without indirection cannot contain
      functions, so mode-crossing `many portable unyielding stateless` is safe.
    * Something storable in 64 bits without indirection cannot support mutation
      (much like other unboxed types), so mode-crossing `contended immutable` is
      safe.
    * Something storable in 64 bits without indirection will not be a pointer to
      a heap-allocated `float`, so `non_float` is correct.

    `external64` describes exactly this scenario: on a 64-bit machine, values
    of this type are never pointers to allocations managed by the OCaml garbage
    collector. This means that mutations of values of this type do not require a
    call to `caml_modify` (on 64-bit machines). See also the documentation on
    [externality][].

* `immutable_data =
     value mod many contended portable unyielding immutable stateless`

    This is a suitable kind for plain old data that is immutable. By "plain
    old data", we mean that values of types of this kind contain no pointers to
    functions. The type `string` has this kind.

* `mutable_data = value mod many portable unyielding stateless`

    This is a suitable kind for plain old data that may be mutable. The
    type `int ref` has this kind.

## Implications

Declaring that a kind crosses some axes implies that it also crosses others;
that is, there are *implications* between the kind axes.

CR reisenberg: Document implications, which I am not fully familiar with.

## Kind annotations on type declarations

When you create a new type with a `type` declaration, you must choose the kinds
of both the type itself and any of its parameters. There are three cases to
consider: determining the kind of a type parameter, determining the kind of an
abstract type (one with no `=` signs), and determining the kind of a defined
type.

### Kinds of type parameters

**Summary:** Type parameters without annotations are assumed to have a layout
other than `any` and assumed to be `non_null`; beyond that, inference chooses
the best possible kind. You can use a kind annotation to make a type parameter
of layout `any` or with `maybe_null`, but inference is still performed.

**Details:** We use inference to determine the kind of a type parameter.

The best kind for a
type parameter is the highest in the subkinding lattice: this allows for a type
to be applicable to as many type arguments as possible. For example, if we have
`type ('a : value) t_value` and `type ('a : immediate) t_imm`, then (because
`immediate <= value`) we can say both `string t_value` and `int t_value`, but we
cannot say `string t_imm`: the higher kind (`value`) is more permissive.

However, we cannot just allow the highest possible kind for a type parameter,
because of how that parameter is used in the definition of the type. For
example, if we have types `type ('a : value mod portable) t_port` and `type
('a : value mod contended) t_cond` and define

```ocaml
type 'a t = { x : 'a t_port; y : 'a t_cond }
```

we have to infer a kind for `'a`, but it has to be a kind lower than `value mod
portable` and lower than `value mod contended` because of the use of `'a`. We
will infer `'a : value mod portable contended`, the highest possible kind that's
still lower than both usages.

The only question, then, is where to start the inference from? That is, what is
default kind for a type parameter? It is tempting to say `any`, as that's the
top. However, this would not be backward compatible, at least in module
signatures, as we need the following very simple example to be accepted:

```ocaml
module M : sig
  type 'a t
end = struct
  type 'a t = 'a
end
```

The kind chosen for `'a t` in the signature is `value` (as described below), and
thus inferring `'a : any` in the signature (and then accepting the module
inclusion) would not be safe: it would allow us to smuggle, say, a `float64`
type where only `value`s are expected.

Accordingly, all type parameters are assumed to have a layout other than `any`
and also bounded by `non_null` -- unless you write a kind annotation saying
otherwise (like `type ('a : any) array`). However, even if you write a kind
annotation, we still perform inference. For example, if you write `type ('a :
any) t = K of 'a t_port`, then we'll infer `'a : value mod portable`. The
annotation is not ignored, but instead causes the initial assumed kind of `'a`
to be `any`, allowing that kind to be lowered by usages of `'a`.

If type inference ends and we still do not know what layout should be used for
the parameter (this can happen only without an annotation), we infer the
kind `value`.

Abstract types are treated identically to concrete ones; it's just that abstract
types have no right-hand side to use to perform inference, and so an unnanotated
type parameter in an abstract type will get kind `value`, according to the rules
described above.

### Kinds of abstract types

When you define an abstract type -- with no `=`-signs -- that type is assigned
kind `value`. This is the only backward-compatible choice. If you wish to
declare an abstract type of another kind, you may supply a kind annotation,
which then determines the final kind of the type; no further inference is
performed.

Note that this is true even when inference could, in theory, be cleverer, such
as in this example:

```ocaml
type ('a : float64) t1
and t2
and t3 = t2 t1
```

Here, we have enough information to infer that the abstract type `t2` should
have kind `float64`, given the usage of `t2` in the definition of `t3`. However,
this example is rejected by our rule that unannotated abstract types have kind
`value`.

### Kinds of concrete types

When a type declaration includes a right-hand side, that right-hand side is used
to compute the kind of the type, with details given in the [description of how
kinds are assigned to types](types.md). You may also write
a kind annotation on the type declaration, like this:

```ocaml
type t : value mod portable =
  | A
  | B of string
```

The annotation checks that the kind of `t` is less than `value mod portable`,
but the kind of `t` remains the one computed by examining the right-hand
side. If you want to make sure that `t` has kind `value mod portable` and no
lower, then you would have to export `t` abstractly from a module, and you can
use `value mod portable` as the kind annotation on the abstract export.

Using the right-hand side to compute the kind of the type is true for `private`
types as well as regular types, though we expect to change this to allow
a `private` type to declare that its kind is higher than strictly necessary.
(Making the kind higher essentially makes the type more abstract, leaking less
about its implementation.)

## Kind annotations on variables

Type variables come in two flavors in OCaml: rigid and unifiable. Rigid type
variables are explicitly bound before a `.`, as in `let f : 'a 'b. ... = ...` or
`val f : 'a 'b. ...` or `type t = { f : 'a 'b. ... }`. All other type variables
are unifiable, including those in type declarations.

A rigid type variable brought into scope without a kind annotation has kind
`value`. You may put a kind annotation at the binding site (before the `.`)
to change the kind.

A unifiable type variable has its kind inferred based on how it is used,
inferring the topmost kind consistent with its usage, with some exceptions:

* In a `val` or `external` declaration, we infer a layout other than `any`, and
we infer only kinds with `mod non_null`. To avoid these restrictions, use a
rigid type variable (with an explicit binding) instead. This exception is to
allow, for example, `val id : 'a -> 'a` to infer `'a : value`, necessary for
backward compatibility.

* In a `class` or `class type` declaration, all type variables have kind
`value`.

* A type variable in a `let` definition may have its layout determined *after*
the definition, in usage sites of the bound variable. This is similar to the way
the value restriction works via weak polymorphism. The bounds are _not_
determined afterwards.

## Examples around rigid vs unification variables

For an abstract type declaration
like `type t : value mod portable`, the kind annotation will be used as the
exact kind of the type `t`.

However, for a type declaration that describes what the type is (that is, has at
least one `=`), the kind annotation is just a check. For example, consider this
type declaration:

```ocaml
type t : value = (int -> int) option
```

This declaration is allowed because `(int -> int) option` does have the kind
`value` (due to subkinding).  But the type system treats `t` and `(int -> int)
option` as completely equal types, and therefore still knows the more precise
kind `value mod contended immutable non_float non_null` for `t` despite the
annotation.

On the other hand, the following declaration is rejected because the kind of
`(int -> int) option` is not less than or equal to `value mod portable`:

```ocaml
type t : value mod portable = (int -> int) option
```

There is a similar subtlety around unifiable type variables in `val`
declarations. Consider

```ocaml
val id : 'a -> 'a
```

The type checker assumes a default of `'a : value` here. If you want to change
that, you should write

```ocaml
val id : ('a : any). 'a -> 'a
```

Here, we have annotated the binding site of `'a` to say that it should have kind
`any`.  (The type `('a : any). 'a -> 'a` is a perfectly good type, and a
function of that type can be called on values of any type of any kind. However,
you will be unable to define a function of that type, because the compiler will
not know what register will hold the argument.) In contrast, writing

```ocaml
val id : ('a : any) -> 'a
```

does not do what you might think: it constrains a *usage site* of `'a`, stating
that `'a` must have a subkind of `any` -- but of course `value` *is* a subkind
of `any`, so the default behavior of choosing `value` is unaffected. That is,
the type `('a : any) -> 'a` is the same as just writing `'a -> 'a`.

Contrast further with

```ocaml
val id : ('a : float64) -> 'a
```

Here, we'll choose `'a` to have kind `float64`. This annotation works because
`float64` is not a superkind of `value`; the default kind does not apply.

The bottom line here: if you want to set the kind of a type variable, do it at a
binding site like `val f : ('a : <<here>>). ...` or `let f : ('a :
<<here>>). ...`.

## Syntax reference

In this reference, we use backticks to denote a literal keyword,
brackets to denote optional syntax, and braces with a trailing `+` to denote
lists containing one or more repetitions. Any nonterminal not defined here is
defined in the OCaml [manual][].

First we define the syntax for kinds:

```
kind ::= atomic-kind [ `mod` mod-bounds ] [ with-bounds ]
     |   `(` kind `)`
     |   kind `&` kind

atomic-kind ::= layout 
            |   kind_abbreviation

layout ::= `any`
       |   `value_or_null`
       |   `float64`
       |   `float32`
       |   `word`
       |   `bits64`
       |   `bits32`
       |   `vec128`

kind_abbreviation ::= `any_non_null`
                  |   `value`
                  |   `immediate`
                  |   `immediate64`
                  |   `immutable_data`
                  |   `mutable_data`

mod-bounds ::= `everything`
           |   { mod-bound }+
           
mod-bound ::= modality
          |   externality
          |   nullability
          |   separability
          
externality ::= `external_`
          |     `external64`
          |     `internal`
          
nullability ::= `non_null`
          |     `maybe_null`
          
separability ::= `non_float`
             |   `separable`
             |   `non_separable`

with-bounds ::= { `with` field_type }+

field_type ::= typexpr [ @@ modalities ]
```

Please see other OxCaml documentation for details on the syntax for
[`modality`](../modes/syntax.md);
[typexpr](https://ocaml.org/manual/latest/types.html) is defined in the OCaml
manual.

Kind annotations are allowed at several places in the syntax. 
As the syntax rules below extend the syntax of OCaml, we use `+::=` to denote
additions to definitions in the OCaml [manual][]; in contrast, we use `::=` to
denote a replacement of the definition in the [manual][].

```
typexpr +::= `(` `'` ident `:` kind `)`
        |    `(` `_` `:` kind `)`
        |     typexpr `as` `(` `'` ident `:` kind `)`
        |     typexpr `as` `(` `_` `:` kind `)`

(* allow a kind annotation when defining a type *)
typedef ::= [type-params] typeconstr-name [ `:` kind ] type-information  (* modified *)

(* allow a kind annotation when binding type variables to the left of a `.` *)
type-binder ::= `'` ident                      (* new non-terminal *)
            |   `(` `'` ident `:` kind `)`

poly-typexpr ::= typexpr
             |   { type-binder }+ `.` typexpr     (* modified *)

(* allow kind annotations on parameters in type definitions *)
type-params ::= type-param-name
               (* modified, also correcting a few bugs in the manual *)
            |   `(` type-param-list `)`

type-param-list ::= type-param [ `,` type-param-list ]

type-param ::= type-param-name [ `:` kind ]

type-param-name ::= [ ext-variance ] `'` ident
                |   [ ext-variance ] `_`

(* allow annotations when binding locally abstract types *)
abstract-type-binder ::= typeconstr-name        (* new non-terminal *)
                     |   `(` typeconstr-name `:` kind `)`

(* the following replace additions in Chapter 10.4 to use
   abstract-type-binder *)
parameter +::= `(` `type` { abstract-type-binder }+ `)`
          |    `(` `type` typeconstr-name `:` kind `)`
      (* no need for extra parentheses in the case where there is only one
         locally abstract type *)

let-binding +::=
  value-name `:` type { abstract-type-binder }+ `.` typexpr `=` expr

class-field +::=
    `method` [ `private` ] method-name `:`
      `type` { abstract-type-binder }+ `.` typexpr `=` expr
  | `method!` [ `private` ] method-name `:`
      `type` { abstract-type-binder }+ `.` typexpr `=` expr
```
