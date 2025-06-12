---
layout: documentation-page
collectionName: Templates
title: Example
---

# A Worked Example

## Layout-polymorphic floats

Let's say we wanted to write a layout-polymorphic version of `Float`, that is, one which
calls functions from `Float` for `float`s with kind `value`, and functions from `Float_u`
for `float#`s with kind `float64`. There are a few different ways to go about this, and
different approaches might make sense depending on the use case, so we try to be flexible
enough to support them.

### Whole module

Perhaps the simplest way to do this is to simply bind the two modules to mangled names
blessed by the PPX:

```ocaml
open! Core

(* Just rebinding the names to work with the PPX.
   [%template] here is just shorthand for [struct ... end],
   with the PPX enabled. Another totally acceptable way to
   write this is [open%template struct ... end]. *)
open
  [%template
  (* alternatively, ... = Float [@@kind value] *)
  module [@kind value] Float = Float
  module [@kind float64] Float = Float_u]
```

This aliases `Float` to `Float` and `Float__float64` to `Float_u`.
From here, we can refer to our aliases using the `[@kind]` attribute, making it easy to
give our module a custom interface if we so desire:

```ocaml
module%template [@kind k = (value, float64)] Float : sig
  type t : k

  val round_up : t -> t
  val round_down : t -> t
  val iround_up_exn : t -> int
  val iround_down_exn : t -> int
end =
  Float
  [@kind k]
```

The name `k` here is arbitrary; it just seemed nice and generic for an example.

### Each binding

Now, this is cool, but in practice it might actually be more convenient to have
kind-polymorphic bindings for each type and function in the module, rather than for the
module itself. At present, for an expression like `M.f [@kind k]`, the PPX will always
mangle `f` rather than `M`, though we could make this configurable if it ends up being
very inconvenient in practice.

In any case, for the sake of example, let's see how we might transform a
kind-polymorphic module binding into a module full of kind-polymorphic bindings.

Here's the interface:

```ocaml
[%%template:
[@@@kind.default k = (value, float64)]

type float : k

val round_up : (float[@kind k]) -> (float[@kind k])
val round_down : (float[@kind k]) -> (float[@kind k])
val iround_up_exn : (float[@kind k]) -> int
val iround_down_exn : (float[@kind k]) -> int]
```

And now for the implementation, in terms of our modules defined above:

```ocaml
(* Unfortunately, this is not valid syntax:
   [type t = (M[@kind k]).t [@@kind k = k]]

   Some ideas for working around this are below - kind of
   like the module equivalent of
   [let open M [@kind k] in ...]. *)
[%%template
[@@@kind.default k = (value, float64)]

open Float [@kind k]

type float = t

let round_up = round_up
let round_down = round_up
let iround_up_exn = iround_up_exn
let iround_down_exn = iround_down_exn]
```

### Advanced example

A common pattern is to define some module type `S` and then a number of functions with
a signature along the lines of `(module S with type t = 'a) -> 'a -> _`. For example, one
way to define layout-polymorphic `Float` operations without this PPX would be as follows:

```ocaml
module type S1 = sig
  type t : any

  val round_up : t -> t
  val round_down : t -> t
  val iround_up_exn : t -> int
  val iround_down_exn : t -> int
end

type ('a : any) module1 = (module S1 with type t = 'a)

val round_up1 : ('a : any). 'a module1 -> 'a -> 'a
val round_down1 : ('a : any). 'a module1 -> 'a -> 'a
val iround_up1_exn : ('a : any). 'a module1 -> 'a -> int
val iround_down1_exn : ('a : any). 'a module1 -> 'a -> int
```

```ocaml
module type S1 = sig
  type t : any

  val round_up : t -> t
  val round_down : t -> t
  val iround_up_exn : t -> int
  val iround_down_exn : t -> int
end

type ('a : any) module1 = (module S1 with type t = 'a)

let round_up1 (type a : any) ((module M) : a module1) =
  M.round_up
;;

let round_down1 (type a : any) ((module M) : a module1) =
  M.round_down
;;

let iround_up1_exn (type a : any) ((module M) : a module1) =
  M.iround_up_exn
;;

let iround_down1_exn (type a : any) ((module M) : a module1)
  =
  M.iround_down_exn
;;
```

However, with ppx_template, it's easy to directly generate multiple copies of the bindings
in a way that emulates polymorphism over layouts:

```ocaml
[%%template:
[@@@kind.default k = (value, float64)]

module type S0 = sig
  type t : k

  include S1 with type t := t
end

module M : S0 [@kind k] with type t = (float[@kind k])]
```

We might as well provide layout-polymorphic versions of the functions from `S1` while
we're at it:

```ocaml
[%%template:
[@@@kind.default k = (value, float64)]

type module0 =
  ((module S0 with type t = (float[@kind k]))[@kind k])

val module0 : (module0[@kind k])
val round_up0 : (float[@kind k]) -> (float[@kind k])
val round_down0 : (float[@kind k]) -> (float[@kind k])
val iround_up0_exn : (float[@kind k]) -> int
val iround_down0_exn : (float[@kind k]) -> int]
```

Now for the implementation:

```ocaml
[%%template
[@@@kind.default k = (value, float64)]

module type S0 = sig
  type t : k

  include S1 with type t := t
end

module M : S0 [@kind k] with type t = (float[@kind k]) =
struct
  type t = (float[@kind k])

  let round_up = (Float.round_up [@kind k])
  let round_down = (Float.round_down [@kind k])
  let iround_up_exn = (Float.iround_up_exn [@kind k])
  let iround_down_exn = (Float.iround_down_exn [@kind k])
end
```

We can even then define the layout-polymorphic functions in `S0` in terms of those from
`S1` to support both styles without much code duplication:

```ocaml
type module0 =
  ((module S0 with type t = (float[@kind k]))[@kind k])

let module0 : (module0[@kind k]) = (module M [@kind k])
let round_up0 x = round_up1 (module0 [@kind k]) x
let round_down0 x = round_down1 (module0 [@kind k]) x
let iround_up0_exn x = iround_up1_exn (module0 [@kind k]) x
let iround_down0_exn x =
  iround_down1_exn (module0 [@kind k]) x
;;]
```
