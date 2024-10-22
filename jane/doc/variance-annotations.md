# Variance Annotations in OCaml

(This was originally published to the Jane Street [Tech
Blog](https://blog.janestreet.com/a-and-a/))

OCaml has subtyping, which is a binary relation on types that says
roughly, if type `t1` is a subtype of type `t2`, then any value of
type `t1` can be used anywhere a value of type `t2` was expected.  In
OCaml, subtyping arises from polymorphic variants.  For example, ``[
`Foo ]`` is a subtype of ``[ `Foo | `Bar ]``.  Why?  Because if you
have a piece of code that knows how to deal with `` `Foo`` and ``
`Bar``, well then surely it can deal with just `` `Foo``.

You can ask OCaml to check subtyping using the syntax `(e : t1 :>
t2)`.  E.g.:

    let f x = (x : [ `Foo ] :> [ `Foo | `Bar ])

This syntax actually does two things:

- it verifies that ``[ `Foo ]`` is a subtype of ``[ `Foo | `Bar ]``
- it causes the type of the entire `:>` expression to be ``[ `Foo |
  `Bar ]``.

Just as the OCaml typechecker has rules for deciding when a let
expression or a function call typechecks, it has rules for deciding
when `(e : t1 :> t2)` typechecks.  The subtyping rule for polymorphic
variant types is clear -- one polymorphic variant `t1` is a subtype of
another polymorphic variant `t2` if every constructor in `t1` also
appears in `t2` (and with the same type argument).  OCaml then has
rules to extend the subtyping relation to more complex types.
E.g. for tuples, the rule is:

    if t1 :> t1' and t2 :> t2'
    then (t1, t2) :> (t1', t2')

For example:

    let f x = 
      (x :  [ `Foo        ]   [ `Baz        ] 
         :> [ `Foo | `Bar ] * [ `Baz | `Bap ])

The rule for subtyping on tuples makes intuitive sense if you think
about what code could do with a tuple -- it can take apart the pieces
and look at them.  So, if a tuple has fewer kinds of values in both
its first and second components, then code dealing with the tuple
would still be fine.

For arrow types, the subtyping rule is:

     if ta' :> ta and tr :> tr' 
     then ta -> tr :> ta' -> tr'

For example:

    let f x =
      (x :  [ `Foo | `Bar ] -> [ `Baz        ]
         :> [ `Foo        ] -> [ `Baz | `Bap ])

Again, the rule makes sense if you think about what code can do with a
function that it has.  It can feed the function arguments, and observe
the results.  So, if a function can accept more kinds of inputs or
returns fewer kinds of outputs, then the code dealing with the
function would still be fine.

For types in OCaml like tuple and arrow, you can use variance
annotations, `+` and `-`, to state the essence of the subtyping rule
for the type -- namely the direction of subtyping needed on the
component types in order to deduce subtyping on the compound type.
For tuple and arrow types, you can write:

    type (+'a, +'b) t = 'a * 'b
    type (-'a, +'b) t = 'a -> 'b

If you don't write the `+` and `-`, OCaml will infer them for you.
So, why do you need to write them at all?  Because module interfaces
are designed to express the contract between implementor and user of a
module, and because the variance of a type affects which programs
using that type are type correct.  For example, suppose you have the
following:

    module M : sig
      type ('a, 'b) t
    end = struct
      type ('a, 'b) t = 'a * 'b
    end

Should the following typecheck or not?

    let f x = 
      (x :  ([ `Foo        ], [ `Baz        ]) M.t
         :> ([ `Foo | `Bar ], [ `Baz | `Bap ]) M.t)

If you know that `('a, 'b) M.t = 'a * 'b`, then yes, it should type
check.  But the whole point of an interface is that a user only knows
what the interface says.  And it does not say that `('a, 'b) M.t =
'a * 'b`.  So in fact it does not type check.

Variance annotations allow you to expose the subtyping properties of
your type in an interface, without exposing the representation.  For
example, you can say:

    module M : sig
      type (+'a, +'b) t
    end = struct
      type ('a, 'b) t = 'a * 'b
    end

This will give enough information to the OCaml type checker to
typecheck uses of `M.t` for subtyping so that the following will type
check.

    let f x =
      (x :  ([ `Foo        ], [ `Baz        ]) M.t 
         :> ([ `Foo | `Bar ], [ `Baz | `Bap ]) M.t)

When you use variance annotations in an interface, OCaml will check
that the implementation matches the interface, as always.  For example
the following will fail to typecheck:

    module M : sig
      type (+'a, +'b) t
    end = struct
      type ('a, 'b) t = 'a -> 'b
    end

Whereas the following will typecheck:

    module M : sig
      type (-'a, +'b) t
    end = struct
      type ('a, 'b) t = 'a -> 'b
    end