---
layout: documentation-page
collectionName: Templates
title: Reference
---

# Templates Reference

## Overview

`ppx_template` emulates polymorphism by generating multiple copies of a given binding for
different parameters. For example, to write an identity function which works for all kinds
of unboxed numbers, one might write:

```ocaml
let%template id (type a : k) (x : a) = x
[@@kind k = (float32, float64, bits32, bits64)]
```

This generates four functions: `id__float32`, `id__float64`, `id__bits32`, and
`id__bits64`, which can be called by using the `[@kind]` attribute on an identifier:

```ocaml
let test x =
  [%test_result: float]
    (Float_u.to_float
       ((id [@kind float64]) (Float_u.of_float x)))
    ~expect:x
;;
```

## Syntax

`ppx_template` is implemented using various extensions-points and attributes.

### Extension points

The `[%template ...]` extension point, which does not actually itself affect the code,
enables `ppx_template` on the contained code. The extension point typically shows up in
shorthand form, e.g. as
`module%template T = struct ... end` or `let%template f x = ...`; this enables the ppx
for the attached AST node:

```ocaml
module T1 : sig
  (* long-hand signature extension (notice colon) *)
  [%%template:
  val id : ('a : k). 'a -> 'a
  [@@kind k = (float32, float64, bits32, bits64)]]

  (* short-hand extension on [val] *)
  val%template id : ('a : k). 'a -> 'a
  [@@kind k = (float32, float64, bits32, bits64)]
end = struct
  (* long-hand structure extension (notice no colon) *)
  [%%template
  let id x = x
  [@@kind k = (float32, float64, bits32, bits64)]]

  (* short-hand extension on [let] *)
  let%template id x = x
  [@@kind k = (float32, float64, bits32, bits64)]
end

(* short-hand extension on entire module *)
module%template T2 : sig
  val id : ('a : k). 'a -> 'a
  [@@kind k = (float32, float64, bits32, bits64)]
end = struct
  let id x = x
  [@@kind k = (float32, float64, bits32, bits64)]
end
```

There is also a `module%template.portable` extension point; see [below](#template-portable)
for details.

### Attributes

Attached poly-attributes (`[@@kind ...]`, `[@@mode ...]`, `[@@modality ...]`, and `[@@alloc ...]`)
can appear on structure/signature items; these attributes produce multiple copies of the attached
template item, updating each instance's AST based on the specified bindings. Additionally, these
attributes mangle the instance's name (e.g. function name or module name) based on the requested
bindings so that the desired template instance can be picked out by name (see [below](#mangling)):

```ocaml
module%template T_templated : sig
  val id : ('a : k). 'a @ m -> 'a @ m
  [@@kind k = (bits32, bits64)] [@@mode m = (local, global)]
end = struct
  let id x = x
  [@@kind k = (bits32, bits64)] [@@mode m = (local, global)]
  ;;
end

(* expands to *)

module T_expanded : sig
  val id__bits32 : ('a : bits32). 'a @ local -> 'a @ local

  val id__bits32__global
    : ('a : bits32)
    . 'a @ global -> 'a @ global

  val id__bits64 : ('a : bits64). 'a @ local -> 'a @ local

  val id__bits64__global
    : ('a : bits64)
    . 'a @ global -> 'a @ global
end = struct
  let id__bits32 x = x
  and id__bits32__global x = x
  and id__bits64 x = x
  and id__bits64__global x = x
end
```

Floating poly-attributes (`[@@@kind ...]`, `[@@@mode ...]`, etc. and `[@@@kind.default ...]`,
`[@@@mode.default ...]` etc.) can appear as a structure/signature item; these attributes
act as their corresponding attached poly-attributes, templating all subsequent items in the
same (shallow) lexical scope. The `.default` modifier controls whether name mangling happens
(no mangling when omitted, mangling when present). You generally *always* want to use
`.default`, and we intend to make that the default behavior in the future.

```ocaml
module%template T_floating_default = struct
  [@@@kind.default k = (bits32, bits64)]

  type ('a : k) t = { x : 'a }

  [@@@mode.default p = (nonportable, portable)]

  let wrap (x @ p) = { x }
  let unwrap ({ x } @ p) = x
end

(* is equivalent to *)

module%template T_floating = struct
  (* note no [.default] *)
  [@@@kind k = (bits32, bits64)]

  type ('a : k) t = { x : 'a }
  [@@kind k = k]

  [@@@mode p = (nonportable, portable)]

  let wrap (x @ p) = { x }
  [@@kind k = k]
  [@@mode p = p]

  let unwrap ({ x } @ p) = x
  [@@kind k = k]
  [@@mode p = p]
end

(* expands to *)

module%template T_attached = struct
  (* floating attributes introduce a scope so that, e.g.,
     [open]s don't bleed into later templates *)
  include struct
    type ('a : k) t = { x : 'a }
    [@@kind k = bits32]

    include struct
      let wrap (x @ p) = { x }
      [@@kind k = bits32]
      [@@mode p = nonportable]

      let unwrap ({ x } @ p) = x
      [@@kind k = bits32]
      [@@mode p = nonportable]
    end

    include struct
      let wrap (x @ p) = { x }
      [@@kind k = bits32]
      [@@mode p = portable]

      let unwrap ({ x } @ p) = x
      [@@kind k = bits32]
      [@@mode p = portable]
    end
  end

  include struct
    type ('a : k) t = { x : 'a }
    [@@kind k = bits64]

    include struct
      let wrap (x @ p) = { x }
      [@@kind k = bits64]
      [@@mode p = nonportable]

      let unwrap ({ x } @ p) = x
      [@@kind k = bits64]
      [@@mode p = nonportable]
    end

    include struct
      let wrap (x @ p) = { x }
      [@@kind k = bits64]
      [@@mode p = portable]

      let unwrap ({ x } @ p) = x
      [@@kind k = bits64]
      [@@mode p = portable]
    end
  end
end

(* is equivalent to *)

module T_expanded = struct
  type ('a : bits32) t__bits32 = { x : 'a }

  let wrap__bits32 (x @ nonportable) = { x }
  let unwrap__bits32 ({ x } @ nonportable) = x
  let wrap__bits32__portable (x @ portable) = { x }
  let unwrap__bits32__portable ({ x } @ portable) = x

  type ('a : bits64) t__bits64 = { x : 'a }

  let wrap__bits64 (x @ nonportable) = { x }
  let unwrap__bits64 ({ x } @ nonportable) = x
  let wrap__bits64__portable (x @ portable) = { x }
  let unwrap__bits64__portable ({ x } @ portable) = x
end
```

Mono-attributes (`[@kind ...]`, `[@mode ...]`, `[@modality ...]`, and `[@alloc ...]`)
can appear attached to identifiers in expression, type, module expression, and module type
positions. Mono-attributes have the effect of mangling the attached identifier to the name
corresponding to the requested template instance. This helps with readability, as well as
avoiding dependence on the particular mangling mechanism `ppx_template` uses as this can change
in the future.

```ocaml
let%template flip
  (type (a : k) (b : k) (c : k))
  (f : (a -> b -> c) @@ p)
  =
  fun x y -> f y x
[@@mode p = (nonportable, portable)]
[@@kind k = (bits32, bits64)]

type t : bits32

let my_flip (f : t -> t -> t) =
  (flip [@mode portable] [@kind bits32]) f
;;
```

Notice that the mono-attributes are not required to be within a `%template` node.

The `[@@alloc ...]` poly-attributes have some additional supported syntax, and there are a few
other attributes that interact with particular axes (`[@exclave_if_local ...]`,
`[@exclave_if_stack ...]`, `[@zero_alloc_if_local ...]`, and `[@zero_alloc_if_stack ...]`).
More details on these can be found in sections below.

### Attribute payloads

Poly-attributes contain a comma-separated list of bindings of the form `lhs = rhs`, where `lhs`
is a template variable and `rhs` is a list of values to instantiate the template with (written as a tuple):

```ocaml
let%template f x = x
[@@kind k1 = (value, bits32, bits64),
        k2 = (value, bits32, bits64)]
[@@mode m1 = (local, global), m2 = (portable, nonportable)]
;;
```

Mono-attributes contain a space-separated list of values that invoke the requested template.
Within each mono-attribute, the order matters (e.g. if there are multiple mode template variables),
but the order of neither the mono-attributes nor the poly-attributes matters:

```ocaml
let%template apply
  (type a b (c : k))
  (f : a @ ma -> b @ mb -> c)
  =
  fun x y -> f x y
[@@mode ma = (contended, uncontended),
        mb = (contended, uncontended)]
[@@kind k = (bits32, bits64)]

type a
type b
type c : bits32

let my_apply (f : a @ contended -> b @ uncontended -> c)
  : a @ contended -> b @ uncontended -> c
  =
  (apply [@kind bits32] [@mode contended uncontended]) f
;;
```

For poly-attributes, a form of punning is available in cases where all you want is name mangling;
just write the poly-attribute with a payload like the corresponding mono-attribute payload you
would use to invoke a template:

```ocaml
module%template T_punned = struct
  [@@@kind ka = (bits32, bits64), kb = (bits32, bits64)]

  type ('a : ka, 'b : kb) t = { a : 'a; b : 'b }
  [@@kind ka kb]

  let wrap a b = { a; b }
  [@@kind ka kb]

  let unwrap_a { a; b = _ } = a
  [@@kind ka kb]

  let unwrap_b { a = _; b } = b
  [@@kind ka kb]
end

(* is equivalent to *)

module%template T_expanded = struct
  [@@@kind ka = (bits32, bits64), kb = (bits32, bits64)]

  type ('a : ka, 'b : kb) t = { a : 'a; b : 'b }
  [@@kind ka = ka, kb = kb]

  let wrap a b = { a; b }
  [@@kind ka = ka, kb = kb]

  let unwrap_a { a; b = _ } = a
  [@@kind ka = ka, kb = kb]

  let unwrap_b { a = _; b } = b
  [@@kind ka = ka, kb = kb]
end
```

#### Grammar

Grammar of poly-attribute payloads:
```
# e.g.
# let f x = x
# [@@kind k1 = bits32, k2 = (value, bits32)]
# [@@mode local local]
poly<t> ::= simple-bindings<t> | punned-bindings<t>

simple-bindings<t> ::=
| nil
| simple-binding<t> ("," simple-binding<t>)*

simple-binding<t> ::= pattern<t> "=" expressions<t>

punned-bindings<t> ::= expression<t>*
```

Grammar of mono-attribute payloads:
```
# e.g. let x = (f [@kind value bits32])
mono<t> ::= expression<t>*
```

Patterns:
```
pattern<mode | modality | kind | alloc> ::= identifier

pattern<alloc_at_mode> ::= pattern<alloc> "@" pattern<mode>
```

Expressions:
```
expressions<t> ::=
| expression<t>
| "(" expression<t> ("," expression<t>)* ")"

expression<mode> ::= identifier

expression<modality> ::= identifier

expression<kind> ::=
| identifier
| expression<kind> ("&" expression<kind>)+
| expression<kind> "mod" expression<mode>+

expression<alloc> ::= identifier

expression<alloc_at_mode> ::=
| identifier
| identifier "@" identifier
```

## Axes

Templating acts on a few different axes; each supported axis is designed around some future
compiler feature that will enable first-class support for the corresonding polymorphism.
Template variables are namespaced by axis.

### Kind polymorphism

The `[@@kind ...]` and related attributes can be used to template over kinds. In the future,
kind polymorphism will be supported via static computation: functions which can be
beta-reduced once at compile time by kind variable substitution will be compiled once for
each kind to which they are applied.

The currently supported shapes of kinds are:

* kind-abbreviations: `[@@kind k = (bits32, bits64, value)]`
* kind-products: `[@@kind k = (value, value & value, (value & value) & value)]`
  * note: due to a limitation in the `ppx_template` syntax, right-nested products
    (e.g. `value & (value & value)`) are not supported
* mode-modifiers: `[@@kind k = (value, value mod portable)]`

Kind-abbreviations can be other kind-template variables, and mode-modifiers can be other
modality-template variables.

### Mode polymorphism

The `[@@mode ...]` and related attributes can be use to template over modes. In the future,
mode-polymorphism will be supported as a first-class type-system feature.

We also support an additional attribute, `[@exclave_if_local m]`, where `m` is a
mode-template variable (or concrete mode) expected to evaluate to either `local` or `global`.
This attribute may only be attached to expressions. When generating bindings, the ppx will
check if `m = local`, and if so, wrap the expression in an `exclave_`.

To ensure that we will be able to natively support the full feature set of this ppx in the
compiler, we only permit `[@exclave_if_local]` to appear on two classes of expression:

- "Pure" syntactic allocations, such as tuples, records, or arrays
- Tailcalls involving only bound identifiers

The former of these cases will eventually be supported by using unboxed values,
and the latter will eventually be supported by "exclaves-on-arrows".

Additionally, the `[@@zero_alloc_if_local m args...]` can be attached to any places the
`[@zero_alloc]` attribute can be attached. If `m = local`, then the attribute is replaced
with `[@zero_alloc args...]`, and otherwise it is removed. This feature is morally deprecated
in favor of `[@zero_alloc_if_stack]` (see [Alloc polymorphism](#alloc-polymorphism)).

### Modality polymorphism

The `[@@modality ...]` and related attributes can be used to template over modalities.
In the future, first-class support for modality-polymorphism will be available in a limited
way once we have first-class modalities (e.g. `val f : ('a @@ global * 'b) @ local -> 'a`).

### Alloc polymorphism

While mode polymorphism enables you to annotate functions and values with modes, it doesn't
allow you to vary the compilation behavior of a function depending on the mode;
this is sometimes referred to *ad-hoc* polymorphism. In particular, consider the functions

```ocaml
let rec map : f:('a -> 'b) -> 'a list -> 'b list =
  fun ~f list ->
  match list with
  | [] -> []
  | hd :: tl -> f hd :: map ~f tl
;;

let rec map_stack
  : f:('a -> 'b @ local) -> 'a list -> 'b list @ local
  =
  fun ~f list -> exclave_
  match list with
  | [] -> []
  | hd :: tl -> stack_ (f hd :: map_stack ~f tl)
;;
```

Both of these map functions take a `global` argument, but `map` returns a list on the heap,
while `map_stack` returns a list on the stack. This requires two different compilations, which
will not natively be supported by mode polymorphism in the future. If you try to write this using
ppx-template's mode polymorphism, you'll get an (intentional) error:

```ocaml
let%template rec map
  : f:('a -> 'b @ m) -> 'a list -> 'b list @ m
  =
  fun ~f list ->
  match[@exclave_if_local m] list with
  | [] -> []
  | hd :: tl -> f hd :: (map [@mode m]) ~f tl
[@@mode m = (local, global)]
;;
```
```
Lines 3-5, characters 5-50:
Error: ([%template]
        "exclave_if_local is only allowed on tailcalls or
       syntactic allocations (e.g. tuples) consisting
       entirely of identifiers, record fields, and/or
       constants")
```

Instead, you can use alloc polymorphism:

```ocaml
let%template rec map
  : f:('a -> 'b @ m) -> 'a list -> 'b list @ m
  =
  fun ~f list ->
  match[@exclave_if_stack a] list with
  | [] -> []
  | hd :: tl -> f hd :: (map [@alloc a]) ~f tl
[@@alloc a @ m = (heap_global, stack_local)]
;;
```

This will eventually be natively supported via first-class allocators, where users will
be able to provide a value to the function that describes *how* to allocate a value.

In the example, `[@@alloc a @ m = (heap_global, stack_local)]` means vary `a` and `m` *together* over
`(heap * global), (stack * local)`. You can also omit the `@ m` on the lhs (e.g. `[@@alloc a = (heap, stack)]`),
or use `a @ m` on the rhs (e.g. `[@@alloc a @ m = (heap @ global, stack @ local)]`).
Punning is additionally supported as `[@@alloc a]`:

```ocaml
[%%template
open struct
  (* omit [@ m] *)
  [@@@alloc a = (heap, stack)]

  let f x = x [@exclave_if_stack a]
  [@@alloc a] (* punning *)
end

open struct
  [@@@alloc a @ m = (heap_global, stack_local)]
  (* Use [a @ m] on the rhs *)
  [@@@alloc a' @ m' = (heap_global, a @ m)]

  let f x = x [@exclave_if_stack a]
end

module type T = sig
  val f : 'a @ m -> 'a @ m
  [@@alloc __ @ m = (heap_global, stack_local)]
  (* You can use [__] to ignore a template variable; this
     will still mangle the duplicate [f] and mangle the
     name, but makes it clear that the alloc variable isn't
     used.

     Side note: single [_] doesn't work due to how the
     syntax is implemented. *)
end]
```

As with mode-polymorphism, the `[@@zero_alloc_if_stack a args...]` attribute conditionally
attaches a `[@@zero_alloc args...]` attribute to a syntax node when `a = stack`. This is the
preferred attribute over `[@@zero_alloc_if_local]` as conditionally being zero-alloc is
a property of allocation behavior, not of modes.

## The [%template.portable] extension {#template-portable}

A very common pattern for functors in common-libraries is to want a functor templated over the
portability of the input and output modules:

```ocaml
(* before templating *)
module F_untemplated (T1 : S1) (T2 : S2) : S3 = struct
  (* ... *)
end

(* templating without shorthand *)
module%template
  [@modality p = (nonportable, portable)] F_long_hand
    (T1 : sig
       include S1 @@ p
     end)
    (T2 : sig
       include S2 @@ p
     end) : sig
  include S3 @@ p
end = struct
  (* ... *)
end
```

This is sufficiently common that we added a short-hand for it:

```ocaml
module%template.portable F_short_hand (T1 : S1) (T2 : S2) :
  S3 = struct
  (* ... *)
end
```

In structures, if you still want a name for `p` (e.g. to invoke other functors),
you can write:

```ocaml
module%template.portable
  [@modality p] F
    (T1 : S1)
    (T2 : S2) : S3 = struct
  include Other_f1 [@modality p] (T1)
  include Other_f2 [@modality p] (T2)
end
```

## Mangling

**WE DO NOT GUARANTEE STABILITY OF THE MANGLING ALGORITHM.**

Identifier mangling involves taking the base identifier and appending parts for each
template value used in the template instance. Each axis has a set of defaults:

* kind: `value`
* mode: `global`, `nonportable`, `uncontended`, `aliased`
* modality: `local`, `nonportable`, `uncontended`, `unique`
* alloc: `heap`

If all template values along a given axis are defaults, then the axis is omitted from
mangling. Within an axis with any non-default values, template values are mangled in
order they appear syntactically. The axes are canonically ordered: kind, mode, modality,
alloc. For the `[@@alloc a @ m = ...]` attribute, only the alloc-template variable is
used for mangling.

For identifier template values (i.e. all modes, modalities, alloc, and kind-abbreviations),
the mangle part is just `__identifier`, e.g. `__portable` or `__bits32`. For kind-products
and mode-modifiers, we use single quotes for grouping, e.g. `__'value_value'` for `value & value`
and `__'value_mod_portable'` for `value mod portable`. This has a chance of having
collisions, but these are rare in practice and just produce compile-time errors from ppx-template.

## Tricks and short hands

One may find themselves needing to "lift" OCaml constructs not generated via this ppx to
have a mangled name compatible with the ppx. We strongly encourage users to do this via
the ppx, rather than mangling names themselves, as this may be brittle or even incorrect.

Instead, a trick for this is to write a binding which is polymorphic over exactly one
kind - for example, to bind `Float` and `Float__float64` modules, one might write:

```ocaml
(* alternatively, ... = Float [@@kind __ = value] *)
module%template [@kind __ = value] Float = Float
module%template [@kind __ = float64] Float = Float_u
```

Here, `__` is just an identifier, nothing special, but seems like a good way to indicate
that a given kind variable is unused in the body of the binding. In these cases, you might
prefer to use punning to avoid the extra syntax clutter:

```ocaml
(* alternatively, ... = Float [@@kind value] *)
module%template [@kind value] Float = Float
module%template [@kind float64] Float = Float_u
```

Extending this idea, one could even use a kind-polymorphic module binding to generate
kind-polymorphic bindings for many functions at once, as a kind of functor over kinds:

```ocaml
module%template [@kind k = (value, float64)] Make = struct
  open Float [@kind k]

  let[@kind k] of_string = of_string
  let[@kind k] to_string = to_string

  (* .. *)
end

include%template Make [@kind value]
include%template Make [@kind float64]
```

This too is common enough that we provide a shorthand:

```ocaml
include%template [@kind k = (value, float64)] struct
  open Float [@kind k]

  let[@kind k] of_string = of_string
  let[@kind k] to_string = to_string

  (* .. *)
end
```

### Type and module substitutions

Type substitutions (using `:=`) are generally supported, but can sometimes get dropped due
to scoping issues. In particular, `ppx_template` may wrap certain constructs inside a
`include sig ... end`, and type substitutions are only visible to the immediately
enclosing signature. For example, these are fine:

```ocaml
module type%template A = sig
  [@@@kind.default k = (bits64, float64)]

  type ('a : k) t := 'a
  type nonrec 'a t = 'a t [@kind k]
end

module type B = sig
  type%template ('a : k) t := 'a
  [@@kind k = (bits64, float64)]

  type%template nonrec 'a t = 'a t [@kind k]
  [@@kind k = (bits64, float64)]
end
```

But this is not:

```ocaml
module type C = sig
  [%%template:
  [@@@kind.default k = (bits64, float64)]

  type ('a : k) t := 'a]

  type%template nonrec 'a t = 'a t [@kind k]
  [@@kind k = (bits64, float64)]
end
```
```
Line 7, characters 36-37:
Error: Unbound type constructor t__bits64
```

Module type substitutions are supported, but are more constrained by the limitation
described above. This is fine:

```ocaml
module type%template A = sig
  [@@@kind.default k = (bits64, float64)]

  module type S := sig end
  module type S = S [@kind k]
end
```

But these are not:

```ocaml
module type B = sig
  module type%template S := sig end
  [@@kind k = (bits64, float64)]

  module type%template S = S [@kind k]
  [@@kind k = (bits64, float64)]
end
```
```
Line 3, characters 30-31:
Error: Unbound module type S__bits64
```

```ocaml
module type C = sig
  [%%template:
  [@@@kind.default k = (bits64, float64)]

  module type S := sig end]

  module type%template S = S [@kind k]
  [@@kind k = (bits64, float64)]
end
```
```
Line 7, characters 30-31:
Error: Unbound module type S__bits64
```

Module substitutions are not supported due to missing support for attributes in the
necessary locations.

### Type variables vs. locally abstract types

The ppx may behave unexpectedly when using type variables in `let` bindings. Take, for
example, the following identity function template:

```ocaml
let%template f (x : ('a : k)) : 'a = x
[@@kind k = (value, float64)]
```
```
Line 1, characters 49-57:
Error: This type ('a : float64) should be an instance of type ('a0 : value)
       The layout of 'a is value
         because of the annotation on the type variable 'a.
       But the layout of 'a must overlap with float64
         because of the annotation on the type variable 'a.
```

Due to the way that type variables unify across function declarations in the same block,
this currently leads to an error.

To fix this, it's recommended you use locally abstract types instead of type variables
when ranging over multiple kinds:

```ocaml
let%template f (type (a : k)) (x : a) : a = x
[@@kind k = (value, float64)]
```

