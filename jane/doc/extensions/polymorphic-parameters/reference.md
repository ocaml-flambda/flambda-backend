# Polymorphic parameters

The *polymorphic parameters* extension allows you to have function
parameters with polymorphic types. For example, a function could
have a type like:
```ocaml
val f : ('a. 'a -> 'a list) -> int list
```

As a practical example, let's consider the creation functions from
`ppx_typed_fields`. Given a record definition:
```ocaml
type t =
  { a : string
  ; b : int
  }
```
`ppx_typed_fields` gives you a type representing the fields indexed by
their type:
```ocaml
type 'a field =
  | A : string field
  | B : int field
```
A useful function to provide for these types is one that can create a
`t` if given a function that returns a value for each of the fields. You
might try to write that function like so:
```ocaml
let create f =
  { a = f A; b = f B }
```
but the compiler will complain about this:
```ocaml
Line 3, characters 21-22:
3 |     { a = f A; b = f B };;
                         ^
Error: This expression has type int field
       but an expression was expected of type string field
       Type int is not compatible with type string
```
The issue is that you need to apply `f` to both a `string field` and
an `int field`: you need it to be *polymorphic*.

There are existing ways to work around this issue -- polymorphic
record fields, first-class modules and polymorphic methods -- but they
all require defining a fresh type to contain `f`. These work-arounds
require additional code at each call site to wrap `f` in the associated
type.

With polymorphic parameters, you can simply annotate `f` as
polymorphic:
```ocaml
let create (f : 'a. 'a field -> 'a) =
  { a = f A; b = f B }
```
which gives `create` the following type:
```ocaml
val create : ('a. 'a field -> 'a) -> t
```

It can be called on a suitable function directly:
```ocaml
let forty_two (type a) : a field -> a = function
  | A -> "forty two"
  | B -> 42

let r = create forty_two
```

## Limitations

All polymorphic parameter require an annotation. Without an annotation
OCaml will assume that the parameter is monomorphic.

Rather than annotating `f` directly, you can also annotate `create` as
a whole:
```ocaml
let create : ('a. 'a field -> 'a) -> t =
  fun f -> { a = f A; b = f B }
```
or rely on the expected type propagated from a function application:
```ocaml
let with_creator (c : ('a. 'a field -> 'a) -> t) =
  c forty_two

let r = with_creator (fun f -> { a = f A; b = f B })
```

Type variables in OCaml always stand for monomorphic types. You cannot
instantiate a type like `'a -> 'b` to `('a. 'a field -> 'a) -> t`. In
practice, this means that functions like `apply`:
```ocaml
let apply f x = f x
```
cannot be applied to functions with polymorphic parameters:
```ocaml
Line 1, characters 6-12:
1 | apply create forty_two;;
          ^^^^^^
Error: This expression has type ('a. 'a field -> 'a) -> t
       but an expression was expected of type 'b -> 'c
       The universal variable 'a would escape its scope
```

Similarly, applying a function with polymorphic parameters requires
knowing the type of the function. Without type information, OCaml will
assume that the function has type `'a -> 'b`. The rules for knowing
the type of the function are the same as those for record field
disambiguation. For example,
```ocaml
let with_create_mono c =
  c forty_two
```
will get the type:
```ocaml
val with_create_mono : (('a field -> 'a) -> 'b) -> 'b
```
because OCaml has assumed that the parameter of `c` is monomorphic,
which prevents it from being applied to `create`:
```ocaml
Line 1, characters 17-23:
1 | with_create_mono create;;
                     ^^^^^^
Error: This expression has type ('a. 'a field -> 'a) -> t
       but an expression was expected of type ('b field -> 'b) -> 'c
       The universal variable 'a would escape its scope
```

Note that these limitations only apply to parameters with genuinely
polymorphic parameters. Given a type like:
```ocaml
type 'a t = int
```
the type `('a. 'a t) -> string` is completely equivalent to `int ->
string`.