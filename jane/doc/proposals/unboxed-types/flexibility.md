# Layout flexibility

The design for unboxed types allows easy access to unboxed records and variants,
meaning that programmers will be using types at a large number of different layouts.
Each layout requires its own calling convention and memory allocation behavior;
accordingly, the fact that OCaml compilation erases types (and layouts) means that
an ordinary polymorphic function cannot work over types of different layouts.
Put another way, we can be polymorphic over *types* but not *layouts*.

However, changing the layout of a type does not change anything about the source
code for a function. For example, we can imagine the following functions for
extracting the first element of an unboxed pair:

```ocaml
module Fsts : sig
  val fst_value_value : ('a : value) ('b : value). #( 'a * 'b ) -> 'a
  val fst_int64_float64 : ('a : int64) ('b : float64). #( 'a * 'b ) -> 'a
  val fst_float64_int64 : ('a : float64) ('b : int64). #( 'a * 'b ) -> 'a
end = struct
  let fst_value_value #(x, _) = x
  let fst_int64_float64 #(x, _) = x
  let fst_float64_int64 #(x, _) = x
end
```

This gets boring quickly -- and note that the function definitions for all of the
different varieties are identical.

We thus sketch out the design for *layout flexibility*, as described in this document.
With layout flexibility, we can write just this:

```ocaml
module Fst : sig
  val fst : ('a : any) ('b : any). #( 'a * 'b ) -> 'a
end = struct
  let fst (x, _) = x
end
```

This `fst` function cannot be compiled to machine code: it would have no way of
accepting arguments or returning a result. However, it usefully serves as a template
for concrete instantiations that can indeed be compiled.

We call a function like `fst` *layout flexible*. We reserve "layout polymorphic" to
describe a feature (neither designed nor planned) that would allow locally quantified
layout variables. Instead, layout flexibility is all about having types with layout
`any`.

There are two thorny aspects of this design: tweaks to the type system and the implementation
strategy. The type system changes are necessary in order to restrict how layout-flexible
functions are used, so that we can look up the template implementation for instantiation.
The implementation strategy describes how the template is used to mint concrete versions
of the function.

## Type system

Details forthcoming, but they are foreshadowed in the [extended abstract for
Layout
Polymorphism](https://icfp23.sigplan.org/details/mlworkshop-2023/3/Layout-Polymorphism-Using-static-computation-to-allow-efficient-polymorphism-over-va)
(see the "file attachement").

## Implementation strategy

Details forthcoming, but they are foreshadowed in the [extended abstract for
Layout
Polymorphism](https://icfp23.sigplan.org/details/mlworkshop-2023/3/Layout-Polymorphism-Using-static-computation-to-allow-efficient-polymorphism-over-va)
(see the "file attachement").