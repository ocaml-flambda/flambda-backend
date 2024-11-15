# Extensions for unboxed types

This builds on the [main proposal for unboxed types](index.md), which should be
read first.

# Extensions and other addenda

## Alternative idea: `#` as an operator

One alternative idea we considered was to make `#` a (partial)
operator on the type algebra, computing an unboxed version of
a type, given its declaration. This ran aground fairly quickly,
though.

One problem is that OCaml doesn't have the ability to extract
a type argument from a composite type (like to get the `int`
from `int list`). So viewing `#` as the inverse of `box` doesn't
quite work. (Richard thought for a little bit that
`type 'a unlist = 'b constraint 'a = 'b list` did this unwrapping,
but it doesn't: it just requires that the argument to `unlist` be
statically a `list` type; this is not powerful enough for our needs,
because it would reject `'a. ... # 'a ...`, which would be part
of this more general operator's purview.)

Another problem is that `#` would have to have an elaborate type:
it would need to be something like `# : Pi (t : value) -> t layout_of_unboxed_version_of`,
necessitating the definition of `layout_of_unboxed_version_of`.
This is a dependent type. Dependent types are cool and all, but
unboxed types seem like a poor motivation for them.

## Extension: More `immediate`s

Today, if the user writes

```ocaml
type t = A | B | C | D
```

the type `t` will be inferred to be `immediate`, because it is an enumeration
and can comfortably fit within the untagged portion of a word. It will not
be an immediate if any of the constructors have fields, because such a variant
is sometimes a pointer.

However, with the addition of unboxed types, we can imagine more variants than
just enumerations to be immediate. Here are some examples:

```ocaml
type t1 = A | B | C                 (* this is immediate today *)
type t2 = D of #t1 | E of #t1 | F   (* inlining t1 makes this an enumeration *)
type t3 = G of #int8 | H of #int16  (* this easily fits in a word *)
type t4 = I of #int16 | J of #int16 (* this also fits, reusing space *)
type t5 = { x : #int8; y : #int16 } (* still fits *)
```

Note that we're discussing whether the *boxed* versions of these types are
immediate, not the *unboxed* versions (which will *not* be). In thinking
about this, we want to make sure that any immediates fit within 31 bits. That's
because we want the type system to be the same on 32-bit and 64-bit processors.
However, we might imagine further that some types are `immediate64`s, giving
them more room.

One challenge in moving in this direction is that recognizing some of these
as `immediate` bakes in some assumptions about how these are stored. For
example, take `t2` above. We could say that it has 7 constructors and thus
could be represented in bits 1-3 of a word. But this means that checking to
see whether a value is made with the `E` constructor requires a range check.
Instead, we might imagine using bits 1 and 2 to store the constructor choice,
and then bits 3 and 4 to store the payload of each constructor. This takes
more room, but might be faster for checking which constructor has been used.
In this case, all possibilities can fit into an `immediate`, but we can see
that it is possible to spill into the 32nd bit here. Marking types as
`immediate` means giving the programmer some perspective on our encoding
strategy, which is bad.

So it's not clear what we should do here. We could say that enumerations
are `immediate` (as now), and that the choice of further immediates is
implementation-dependent, unspecified, and subject to change. This allows
the implementation to optimize the representation and handling of some of
these types. But there is a risk that a user relies on this optimization and
then is surprised if the compiler changes packing strategy and changes the
inferred layouts of some of their types. One way around this is a new
layout `weak_immediate <= immediate` that gets inferred for
implementation-dependent `immediate`s, and then we could issue a warning
whenever we have to check whether `weak_immediate <= immediate`. (On the other
hand, checking `weak_immediate <= value` is always fine.) But is this worth
yet another layout? It's not clear.

## Extension: Restrictions around unexpected copy operations

Today, OCaml users expect a line like `let x = y` to be quick, copying a word of
data and requiring ~1 instruction. However, if `y` is a wide unboxed record,
this assignment might take many individual copy operations and be unexpectedly
slow. The same problem exists when an unboxed record is passed as a function
argument. Perhaps worse, this kind of copy can happen invisibly, as part of
closure capture. Example:

```ocaml
type t = { a : int; b : int; c : int; d : int; e : int }

let f (t : #t) =
  let g n = frob (n+1) t in
  ...
```

Building the closure for `g` requires copying the five fields of `t`.

We could imagine a restriction in the language stopping such copies from
happening, or requiring extra syntax to signal them. However, we hope that there
will not be too many wide unboxed records, and copying a few fields really isn't
so slow. So, on balance, we do not think this is necessary.

## Extension: limited layout polymorphism.

We can imagine an extension where
the layout of `(p, q) r` could in general depend on
the layouts of `p` and `q`. For instance:

```ocaml
type ('a : value) id = 'a
```

Since `'a : value`, we know that `'a id` is always of layout `value`,
but if `'a = int` it may also have the more specific layout
`immediate`.

For each type constructor `(_, _) r` in the environment, we store
along with it a layout `K` which is an upper bound for the layout of
any instance of `r`. (For `'a id` above, this is `value`). When
checking `(p, q) r : L`, we compare `L` and `K`. If `K` is a subtype
of `L`, then the check passes. However, if the check fails and `r` is
an alias, we must expand `r` and try again. We only report a
unification failure once the check fails on a type which is not an
alias.

For example, suppose we have the unification variables `'a : value`
and `'b : immediate`, and we're trying to unify `'a id = 'b`. We need
to check `'a id : immediate`, but the environment tells us that the
layout of `_ id` is `value`. So, we expand `'a id` to `'a` and retry
unification, which succeeds after unifying `'a` and `'b` (the
resulting type variable has layout `immediate`). The effect here is
that after unifying `'a id` with `'b`, we leave `'a` as an
uninstantiated unification variable, but its layout is restricted to
`immediate`, to match `'b`.

## Extension: more layout inference

The current design describes that we require users to tell us the
layout of rigid variables but that we will infer the layout of
flexible variables. However, it is possible to do better, inferring
the layouts even of rigid variables. For example:

```ocaml
let f1 : ('a : float). 'a -> 'a = fun x -> x
let f2 : 'a. 'a -> 'a = fun y -> f1 y   (* 'a inferred to be a float *)

module type T = sig
  type ('a : float) t1
  type t2
  type t3 = t2 t1   (* this forces t2 : float *)
end
```

This can be done simply by using unification variables for the layouts
of even rigid type variables; if we don't learn anything interesting
about the layout within the top-level definition, we default to
`value`, as elsewhere.

## Extension: more equalities for `box`

It would be nice to add this rule to the `box` magic:

* If `ty : value`, then `ty box = ty`. There is no reason to box something
already boxed. Note that this equality depends only on the *layout* of
`ty`, not the `ty` itself. For a given layout of the argument, `box` is
parametric: it treats two types of the same layout identically.

This is hard, because at the time we see `ty box`, it might be the case
that we haven't yet figured out the layout for `ty`, meaning we'd have
to continue inference, perhaps figure out the layout, and then return
to `ty box` and perhaps simplify it to `ty`. This is possible (GHC
does it), but it would be breaking new ground for OCaml, and so should
be strongly motivated. So we hold off for now, noting that adding this
feature later will be backward compatible.

## Extension: disabling magic for `box`

The main proposal describes that a declaration like

```ocaml
type t = { x : int; y : float }
```

would really be shorthand for

```ocaml
type #t = #{ x : int; y : float }
type t = #t box
```

and that the expression e.g. `{ x = 5; y = 3.14 }` is shorthand
for `box #{ x = 5; y = 3.14 }`, along with similar shorthand for
record projection and variants.

However, we could have a flag, say `-strict-boxes`, that removes
the auto-boxing and auto-unboxing magic. This would make record-construction
syntax like `{ x = 5; y = 3.14 }` an error, requiring a manual call
to `box` to package up an unboxed record. The advantage to this
is that programmers who want to be aware of allocations get more
compiler support. A downside (other than the verbosity) is that `-strict-boxes`
would still not prevent all allocation: any feature that doesn't go
via `box` might still allocate, including closures.

## Extension: `immediate <= word`

Semantically, `immediate <= word` makes good sense. To see why,
we start by observing we can collapse the `value_rep` and `word_rep`
representations: after all, both `value`s and `word`s take up the
same space in memory. (Their relationship to the garbage collector
is different, but we track that separately, not in the representation.)
We can
see this in the following table:

```
              | representation | gc_friendly | gc_ignorable
    ----------+----------------+-------------+-------------
    immediate | word_rep       | true        | true
    value     | word_rep       | true        | false
    word      | word_rep       | false       | true
```

Just as `immediate <= value` works, `immediate <= word` works:
`immediate` is really the combination of `value` (with its
gc-friendliness) and `word` (with its gc-ignorableness).

However, we hold off on adding `immediate <= word` to our type
system for now, because it adds complication without anyone
asking for it.

Here are some complications introduced:

* It is very convenient to reduce computing
`la ⊓ lb` to a constraint `rep(la) = rep(lb)`. In order to support `value ⊓ word
= immediate`, we would thus drop the `value_rep` representation, setting
`rep(value) = word_rep`.

* The `upper_bound` function we use in type inference then becomes impossible to
define: neither `value` nor `word` is an upper bound for the other, and so
`upper_bound(word_rep)` has no definition.  We thus have to design a different
type inference algorithm than the one described above, likely tracking
representation, gc-friendliness, and gc-ignorability separately. This would not
be all that hard, but it's definitely more complicated than the algorithm
proposed here.

* We sometimes might infer `immediate` even when no function
nearby has anything of layout `immediate`:

    ```ocaml
    val f1 : ('a : value). 'a -> ...
    val f2 : ('a : word). 'a -> ...

    let f3 x = (f1 x, f2 x)
    ```

    Here, we need to infer that the `x` in `f3` has layout
    `immediate`, even though neither `f1` nor `f2` mentions
    `immediate`. This might be unexpected by users.

## Further discussion: mutability within unboxed records

This proposal forbids mutability within an unboxed record (unless that
record is itself contained within a boxed record). This decision is made to
reduce the possibility of confusion and bugs, not because mutability within
unboxed records is unsound. Here, we consider some reasons that push against the
decision above and include some examples illustrating the challenges.

Reasons we might want mutability within unboxed types:

* The designer of a type may choose to have some fields be mutable while
  other fields would not be. For example, our `#mut_point` type allows the
  location of the point to change, while the identity of the point must
  remain the same.

* OCaml programmers expect to be able to update their mutable fields, and it
  may cause confusion if a programmer cannot, say, update the `x` field of a
  `#mut_point`.

The reason we decided not to allow mutability within unboxed types is because of
examples like this:

```ocaml
type mut_point = { mutable x : #int32; mutable y : #int32; identity : int }
let () =
  let p : #mut_point = #{ x = #4l; y = #5l; identity = 0 } in
  let foo () = print_uint32 p.x in
  foo ();
  p.x <- #10l;
  foo ()
```

Because the closure for `foo` captures that unboxed record `p` (by copying `p`),
this code prints 4 twice, rather confusingly. Yet the current design does not
fully rule such a confusion out, as we see here:

```ocaml
type point = { x : int; y : int }
type t = { mutable pt : #point }

let () =
  let t : t = { pt = #{ x = 5; y = 6 } } in
  let pt = t.pt in
  let foo () = print_int pt.x in
  foo ();
  t.(.pt.x) <- 7;
  foo ()
```

This will print 5 twice, because the unboxed `t.pt` is copied into the local
variable `pt`. Yet the boxed version of this (with a `mutable x` and no hash
marks) prints 5 and then 7. This is a key motivation for putting the parentheses
in the `t.(.pt.x)` syntax: it forces the programmer to think about the fact that
they are mutating `pt.x`, not just `x`. Accordingly, they should expect `pt` not
to be changed (even if `pt` were an alias -- which it's not). Without the
parentheses there, a user might think they're mutating `x` and be surprised.

Here's yet another example:

```ocaml
type t2 = { x : #int32; y : #int32 }
type t1 = { mutable t2 : #t2; other : int }

fun (t1 : t1) ->
  let other_t1 : t1 = {t1 with other = 5} in
  t1.(.t2.x) <- #10l;
  other_t1.t2.x = t1.t2.x
```

Assuming `t1.t2.x` doesn't start as `#10l`, this will return `false`. But this
should not come as a surprise: `t1.t2` is copied as part of the functional
record update, and thus mutating `t1.t2` (as part of `t1.(.t2.x) <- #10l`) changes
it. If, on the other hand, we did not have the parentheses, users might expect
`other_t1`'s `x` to be changed when `t1`'s `x` gets changed.

## Alternative: polymorphism, not subtyping

The current design uses a subtyping mechanism (or "sublayouting") to
navigate among related layouts. This allows us to use e.g. an `immediate`
as a `value` and is necessary in order to support functions polymorphic
over values (such as `map : ('a -> 'b) -> 'a list -> 'b list`) to work
with `int`s. An alternative approach would be to use polymorphism
without subtyping.

Instead of `any`, an unknown layout would just be a variable `'y`. (`'l`
is hard to read, and `'a` looks like a type.) To handle the relationship
between `value` and `immediate`, we could have `immediate value` and `pointer
value`. Then we would have `val map : ('a : 'i1 value) ('b : 'i2 value). ('a ->
'b) -> 'a list -> 'b list`. This works because an `'i value` can be compiled
even if we don't know `'i`. Non-value layouts would be unparameterized,
because there is only one way to be e.g. `bits64`.

Using polymorphism instead of subtyping would help these scenarios:

1. We want `or_null` to work with both values and immediates. Furthermore,
we want to remember that e.g. `int or_null` is still an immediate (and
is gc-ignorable). The current design is to say that the layout-check "looks
through" `or_null`: if the argument type `t` is a `non_null_immediate`, then
`t or_null` is `immediate`. This works OK, but it cannot be abstracted over,
as the "looking through" must be implemented specially for `or_null`. With
polymorphism instead, we could say `type ('a : 'i non_null_value) or_null : 'i
value`, which says exactly what we want.

2. Generalizing the point above, any time we have an unboxed record of
several components, we might want a richer layout than we currently can
offer. For example:

    ```ocaml
    type ('a : 'y1, 'b : 'y2) t : 'y1 * 'y2 = #{ x : 'a; y : 'b }
    ```
    
    Without polymorphism, the best we can do is
    
    ```ocaml
    type ('a : any, 'b : any) t = #{ x : 'a; y : 'b }
    ```
    
    This works fine, but just like `or_null`, it can't be abstracted over.
    
    In practice, though, it would be hard to abstract over this `t` even with
    polymorphism, because all functions that operate on `t`s have to work with
    concrete layouts anyway. In essence, the fact that one `t` is shared among
    many different layouts is not very useful: it saves repeating the type
    declaration, but that's it.
    
    Though not covered (yet) in this design, we do imagine adding abstract
    layouts to the module system, where something like this will be possible:
    
    ```ocaml
    module type Two_layouts = sig
      layout lay1
      layout lay2
    end
    
    module Abs_t (L : Two_layouts) : sig
      type ('a : L.lay1, 'b : L.lay2) abs_t : L.lay1 * L.lay2
    end = struct
      type ('a : L.lay1, 'b : L.lay2) abs_t = ('a, 'b) t
    end
    ```
    
    Then we could have the abstract e.g. `Abs_t(struct layout lay1 = value
    layout lay2 = bits32 end).abs_t`. Note that the `Abs_t` module above
    could also include actual functions, too.

3. Polymorphism is useful when a variable is used both co- and contra-variantly.
Put another way, polymorphism is useful when we want to link the type of an
input to the type of an output. The same is true of layouts. The challenge is
that OCaml doesn't allow us to return types from functions; the only way we can
usefully do so is by passing types to a polymorphic parameter. Here is a (contrived)
example:

    ```ocaml
    let wrap : 'i ('a : 'i value) ('b : 'i value) 'r.
      'a -> 'b -> (('c : 'i value). 'c -> ('c, 'c) Either.t) ->
      ('a, 'a) Either.t * ('b, 'b) Either.t =
      fun a b f -> f a, f b
    ```
    
    A caller might pass two `int`s; in this case, the continuation can
    still take advantage of the immediacy of its operand. This is because
    the layout of the argument `'c` to the continuation will be known to
    the same as the layouts of the arguments.
    
    If we didn't have restrictions on what could be compiled to executable
    code, it would be easier to come up with examples of how we might want
    polymorphic parameters to connect to the layouts of other parameters.
    However, it does seem this would come into play only with layouts that
    are currently subtypes of `value`.
   
4. Polymorphism allows us to express homogeneous equality, while subtyping
does not. Consider the equality GADT:

    ```ocaml
    type (_, _) eq = Refl : ('a, 'a) eq
    ```
    
    What layouts should the arguments have? If we want to have `eq` work
    across multiple layouts, then we'd need to have both arguments have
    layout `any`. (Even if we don't do this, we still have a problem because
    of `value` and its sublayouts.) So we would have
    
    ```ocaml
    type (_ : any, _ : any) eq = Refl : ('a, 'a) eq
    ```
    
    But this makes `(int, #float) eq` well formed. Such a type is empty,
    but it's still well formed. This means that we might imagine
    
    ```ocaml
    let f : (int, #float) eq -> int -> #float = function
      | Refl -> fun x -> x
    ```
    
    but I have no idea how that would actually be compiled; I would expect
    some kind of small (compile-time) disaster.
    
    On the other hand, polymorphism allows us to say what we mean:
    
    ```ocaml
    type (_ : l, _ : l) eq = Refl : ('a, 'a) eq
    ```
    
5. Similarly to the problem immediately above, subtyping prevents us from
giving a sensible type to `Obj.magic`. The type for `Obj.magic`, with
subtyping, is this:

    ```ocaml
    val magic : ('a : any) ('b : any). 'a -> 'b
    ```
    
    But this allows us to cast an `int` to a `#float`, which is worse
    than usual for `Obj.magic`. On the other hand, if we could assert
    that `'a` and `'b` had the same layout, via polymorphism, than we
    would have a safer type for `Obj.magic`.
    
    (Prior art: Haskell has two different functions, `unsafeCoerce`
    and `unsafeCoerce#`. The former is representation-monomorphic;
    the latter is not, for those extra-special occasions.)
   
6. If we define `type loopy = { x : loopy } [@@unboxed]`, what is `loopy`'s
layout? Any layout is acceptable. The current plan, as described in the
"Defaults" section of the [inference page](inference.md), is to choose
`non_null_value`. With polymorphism, however, we could choose `'y`, for a
universally quantified `'y`. Then, we could effectively choose `loopy` to
have any layout at all, which is appropriate. This is more expressive.

7. With the `unique` mode, we have an opportunity to support strong update.
For example:

    ```ocaml
    type 'a list =
      | Nil
      | Cons of { shared hd : 'a; tl : 'a list }
    
    let rec map : 'a 'b. ('a -> 'b) -> unique 'a list -> unique 'b list =
      fun f -> function
      | Nil -> Nil
      | Cons ({ hd; tl } as stuff) ->
        { unique stuff with { hd = f hd; tl = map f tl } }
    ```
    
    This reuses the existing memory for the list.
    
    The key question here: what are the layouts of `'a` and `'b`? In the
    example above, without annotations, they would default to `value`. But
    actually any layout is good for `'a` and `'b` -- *as long as they are the
    same*. That is, we could consider this type for `map`:
    
    ```ocaml
    let rec map : ('a : 'y) ('b : 'y). ...
    ```
    
    Here, `'y` is a layout variable. Compiling this function would require 
    support for layout polymorphism (likely by compile-time specialization),
    but it does seem that the ability to use a layout variable twice is helpful
    in describing the function.

## Bad idea: width subtyping

We wondered at one point whether we could support `int8 <= int64` in
the subtyping relation. After all, a function expecting an `int64`
argument can indeed deal with an `int8` one. The problem would be
collections. That is, if we have an `#int8 array` (for a magical
`array` type that can deal with multiple layouts, like `box`), how
would we index into it? Is it a `#int8`-as-64-bits or a plain `#int8`
in there? The way to tell is to have a second argument to `array`
that denotes the layout. Richard thinks this could work out in the
end, but probably would still be a bad idea.

## Bad idea: splitting `box`

The mixed-block restriction tells us that there are really two forms of `box`:
one for gc-friendly boxes and one for gc-ignorable boxes. We briefly thought
about having two different primitives `box_gc` and `box_non_gc` (and no `box`
function), where users would choose the one they wanted. The type system would
ensure that the layout of the argument is appropriate for the type of box
created.

This plan would be good because it would simplify inference: currently, every
use of `box` imposes a requirement that the layout of the argument is *either*
gc-friendly or gc-ignorable. But "or" requirements are annoying: we can't
simplify or usefully propagate the requirement until we know which branch of the
"or" we wish to take. If the user specified which kind of box they want, then
we don't have the "or" any more.

However, this plan doesn't work very well, because of auto-boxing. That is, if
we have something like

```ocaml
let x = ... in
let y = ... in
x, y
```

what kind of box should we use when constructing the tuple? It depends on the
type of `x` and `y` -- or rather on the layouts of those types. At the point
where our boxing magic inserts the call to `box`, we haven't yet completed type
inference, and so we can't know which kind of `box` to insert. In the end, we've
just re-created the challenge with "or" described above. So there seems to be
little incentive to have the two different boxes explicitly anywhere.