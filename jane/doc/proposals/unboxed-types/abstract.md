# Abstract layouts

It may sometimes be convenient to declare *abstract layouts*. For example,
we might want to have a module signature capable of describing an interface
that works over many different layouts. Here might be an interface that
works to define arithmetic types:

```ocaml
module type Arith = sig
  layout y
  type t : y
  val (+) : t -> t -> t
  val (*) : t -> t -> t
end
```

We can then define modules with this signature:

```ocaml
module Uint64 : Arith with layout y <: bits64 = struct
  layout y = bits64
  type t = #int64
  let ( + ) x y = ...
  let ( * ) x y = ...
end

module Ufloat : Arith with layout y <: float64 = struct
  layout y = float64
  type t = #float
  let ( + ) x y = ...
  let ( * ) x y = ...
end
```

It is important to put the `with` constraint on the module signature.
Without this `with` constraint, it would not be possible to bind variables
of type `Uint64.t` or `Ufloat.t` because their layouts would remain
abstract and unrepresentable.

## Details

We add the following BNF productions:

```ocaml
(* signature item *)
specification +::= `layout` layout-name [ `<:` layout ]

(* structure item *)
definition +::= `layout` layout-name `=` layout

(* with constraint *)
mod-constraint +::= `layout` layout-name `<:` layout
               |    `layout` layout-name `:=` layout
```

On the page on [kinds](kinds.md), we also see that we can write
kind abbreviations in both signatures and structures; these can
augment an abstract layout with mode-crossing information.

## Open questions

Is it worth allowing e.g. `layout lay = value` in signatures? I can't
think of a reason this would be helpful.