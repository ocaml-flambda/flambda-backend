---
layout: documentation-page
collectionName: Templates
title: Intro
---

# OCaml with Templates

## Polymorphism and its Limitations

OxCaml comes with _mode_ and _kind_ systems which enable many of its key features, 
including stack allocation, unboxed types, and data-race-free parallelism. However, there 
is still work to be done, and while these systems will eventually be comparable in their
expressivity to the type system, they are at present notably constrained in their capacity
for polymorphism.

For example, consider the identity function:

```ocaml
let id : 'a. 'a -> 'a = fun x -> x
```

In upstream OCaml, this is about as polymorphic as you can get! I can call `id x` on any
value `x`, regardless of its type, and get the same value back out.

But in OxCaml, things are more complex. If my value is stack-allocated, then it will be at 
mode `local`, so my identity function had better not do anything funny like stick it in a 
global `ref`, and it should return the same stack-allocated value at mode `local`, so we
actually need a different signature for that:

```ocaml
let id : 'a. 'a @ local -> 'a @ local = fun x -> x
```

We have submoding, so I can pass a heap-allocated value at mode `global` to this function,
but the resulting value will always be at mode `local`. To preserve its mode, we must use
our original function, which may equivalently be written as:

```ocaml
let id : 'a. 'a @ global -> 'a @ global = fun x -> x
```

## Templates

The implementation of these two functions is obviously identical, so it would be nice if
we could just write it once, but get two copies of the function with different modes.
Conveniently, we've written a 
[preprocessor](https://dune.readthedocs.io/en/stable/reference/preprocessing-spec.html#using-ppx-rewriters) 
to do exactly that: `ppx_template`. Much like templates in C++, this allows us to define 
the function once, polymorphic over some mode variable, while the compiler will 
instantiate the template for each value of the variable. Thus our two identity functions 
can be written as:

```ocaml
let%template[@mode m = (global, local)] id
  : 'a. 'a @ m -> 'a @ m
  =
  fun x -> x
;;
```

This yields:

```ocaml
let id : 'a. 'a @ global -> 'a @ global = fun x -> x
and id__local : 'a. 'a @ local -> 'a @ local = fun x -> x
```

There are a few pieces to unpack here. First, in order to introduce any template
polymorphism, onne must be inside of a `%template` extension node. The syntax for these is
relatively generous. In general, `%template` may follow the keyword introducing any
structure or signature item, such as `let` in the case above, `val`, `type`, `module`, or
`module type`. One can also group any number of structure or signature items using
`[%%template]` syntax. In fact, the above is equivalent to:

```ocaml
[%%template let[@mode m = (global, local)] id
  : 'a. 'a @ m -> 'a @ m
  =
  fun x -> x
;;]
```

In signatures, `[%%template: ...]` (with a colon) must be used instead.

Next, the `[@mode]` attribute introduces a variable `m` and specifies that it ranges over
the `global` and `local` modes. Unlike C++, we have no
[SFINAE](https://en.cppreference.com/w/cpp/language/sfinae.html); all instances of a given
polymorphic declaration or definition are generated eagerly. Like `%template`, it may
immediately follow the keyword introducing a structure or signature item (e.g. `let`), or
it may follow the item as a whole, though this form requires two `@` symbols:

```ocaml
let%template id: 'a. 'a @ m -> 'a @ m = fun x -> x
[@@mode m = (global, local)]
```

The `[%%template ...]` extension may also appear as an expression, such as in
`[%template fun x -> x]`, and can generally be attached to keywords as well, such as in
`fun%template x -> x`. The `[%template: ...]` extension may also appear as a type, such as
in `[%template: 'a option]`.

In the fullness of time, with first-class mode polymorphism, we anticipate that the
definition of id will look more like:

```ocaml
let id : 'a. 'a @ 'm -> 'a @ 'm = fun x -> x
```

## Instantiation

To call our localized identity function, we could of course spell out its
machine-generated, or "mangled" name, `id__local`. However, this can quickly become
cumbersome for complex templates, and we strongly discourage relying on the implementation
of the mangling scheme. Instead, one may again use the `[@mode]` attribute to
"instantiate" one of its monomorphisms, such as `id [@mode local]`. The ppx has a notion
of "default" modes, such that `id [@mode global]` is equivalent to simply `id`. This
currently requires being inside a `%template` extension, but we hope to soon lift this
restriction, allowing one to instantiate templates in arbitrary code:

```ocaml
let f x = (id [@mode global]) x
let g y = exclave_ (id [@mode local]) y
```

If we find ourselves inside of another polymorphic template, we may refer to any mode
variables in scope as well:


```ocaml
let%template[@mode m = (global, local)] f x =
  (id [@mode m]) x [@exclave_if_local m]
;;
```

## Kinds

In addition to mode polymorphism, another feature of `ppx_template` (and indeed its
original motivation) is its support for kind polymorphism. If, say, we wanted to implement
and identity function that worked for both `value`s and unboxed pairs of `value`s, we can
define this via `ppx_template`, this time using the `[@kind]` attribute to introduce a
kind variable:

```ocaml
let%template[@kind k = (value, value & value)] id
  : ('a : k). 'a -> 'a
  =
  fun x -> x
;;
```

Likewise, we could instantiate this template using the `[@kind]` attribute, such as
`id [@kind value & value]`. The ppx again has a notion of "default" kinds, such that
`id [@kind value]` is equivalent to `id`.

Eventually, we will be able to write (and, crucially, compile) an identity function for
any kind, which may look something like

```ocaml
let id : ('a : any). 'a -> 'a = fun x -> x
```

## Modalities

The OxCaml team has prepared Jane Street's standard `Base` and `Core` libraries for use in
a multicore context, largely by exposing the functions within as having the `portable`
modality. However, things are not always so straightforward. One case that stands out is
that of functors; much like functions, a functor whose input module contains `portable`
functions may produce an output module containing `portable` functions, but a functor
whose input is `nonportable` generally produces `nonportable` output. Very little code is
portable yet, but this will change over time, so we must support both cases.

With `ppx_template`, we might express such a functor like so:

```ocaml
module%template F (_ : sig @@ p
    include I
  end) : sig @@ p
  include O
end
[@@modality p = (nonportable, portable)]
```

However this is quite verbose, so we added a special `%template.portable` extension as
shorthand:

```ocaml
module%template.portable F (_ : I) : O
```

The ultimate syntax for this will likely be similar to that of other forms of mode and
modality polymorphism.

## Floating Attributes

A common pattern is to template all items in a given structure or signature over the same
modes, kinds, and/or modalities. However, repeating the same template parameters can
become verbose and duplicative. To simplify this case, we use the floating attributes
`[@@@mode.default]`, `[@@@kind.default]`, and `[@@@modality.default]`. These are
equivalent to applying the corresponding `[@mode]`, `[@kind]`, and `[@modality]`
attributes to all subsequent items in the structure or signature. Here's an example from
`Base.Float`:

```ocaml
[%%template:
[@@@mode.default m = (global, local)]

val min_inan : t @ m -> t @ m -> t @ m
val max_inan : t @ m -> t @ m -> t @ m]
```

This is functionally equivalent to:

```ocaml
val%template min_inan : t @ m -> t @ m -> t @ m
[@@mode m = (global, local)]

val%template max_inan : t @ m -> t @ m -> t @ m
[@@mode m = (global, local)]
```

