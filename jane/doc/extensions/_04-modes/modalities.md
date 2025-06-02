---
layout: documentation-page
collectionName: Modes
title: Modalities
---

As described in the [introduction to modes](../intro), modes are deep properties
of values: if you have a `portable` record and you project a field, that field
will also be `portable`. However, it is sometimes desirable to have an escape
hatch: some nonportable data might have fields which are nevertheless portable.
OxCaml's *modalities* feature allows you to change modes as you extract a
component of some structure.

Here is an example:

```ocaml
type tribble = {
  spawn : unit -> tribble;
  scurry : direction -> unit @@ portable;
}
```

If you have a variable `trib : tribble @ nonportable` (that is, a variable named
`trib`, whose type is `tribble` and mode is `nonportable`), then `trib.spawn`
will be `nonportable` (this is the usual deep behavior), while `trib.scurry`
will be `portable` (this is the new behavior, enabled by the `@@ portable`
modality).

Modalities can appear on elements of structures (records, constructors, modules,
etc). The full description is in our [syntax page](../syntax), but modalities
are always introduced with a `@@` herald. (This is in contrast to *modes* which
are introduced with a `@` herald.) A modality describes the relationship between
the mode of the structure itself and the mode of its element. We can think of a
modality as a function from a mode to a mode: the input is the mode of the
structure and the output is the mode of the element. Right now, all modalities
are modes (whose interpretation is described in the next paragraph), but that
might not always be the case.

Modalities work differently on future axes vs. past axes. On a future axis (like
portability), the modality imposes an upper bound on the mode (thus always
lowering that mode). Thus applying the `portable` modality to a `nonportable`
record yields a `portable` field, because `portable < nonportable`. On a past
axis (like contention), the modality imposes a lower bound (thus always raising
that mode). Accordingly, a `contended` modality applied to an `uncontended`
record yields a `contended` field, because `uncontended < contended`.

Because modalities impose upper and lower bounds, some modalities have no
effect. If we have a `type t = { x : (int -> int) @@ nonportable }`, the
`nonportable` modality has no effect: it is an upper bound on the mode of `x`,
but `nonportable` is the top element of the portability axis, and so using it as
an upper bound is uninformative.

The difference between future axes and past axes is because this is the only
way modalities are safe. It would be bad to have a `nonportable` element within
a `portable` structure: you might send that structure to another thread, and
the `nonportable` element would come along for the ride. It would similarly be
bad to have an `uncontended` element within a `contended` structure: maybe that
structure came from another thread and might be exposed to parallel reads and
writes.

## Modality types

It is sometimes convenient to apply a modality at a time other than when you are
defining a record. The `base` library thus provides *modality types*, such as
[`Modes.Portable.t`](https://github.com/janestreet/base/blob/26c2f4df29a76e792cabfceb53963b3538ba6dc1/src/modes_intf.ml#L519):

```ocaml
type 'a t = { portable : 'a @@ portable } [@@unboxed]
```

This type (which does not appear at runtime -- that's what the `[@@unboxed]` does)
allows you to store a `portable` component of an arbitrary structure.

## Modalities and mode-crossing

Some types are allowed to *mode cross* along certain mode axes. For example,
portability affects only functions; a type that contains no functions (even after
looking deeply through it) does not care about its portability. We say it
*mode-crosses portability*. A type like `string option list` has this property,
but a type like `(int -> int) list` does not.

However, a modality allows more mode-crossing to happen. For example:

```ocaml
type t = { 
  f : (int -> int) @@ portable;
  value : int 
}
```

Without the modality, it would be unsound for `t` to mode-cross portability: it
might contain a nonportable function. However, the modality means that `f` will
always be portable. Because the other field of `t` contains no functions, we can
be sure that all values of type `t` do not have functions in them, and thus it
is safe for `t` to mode-cross portability.

As a consequence of this, the modality types all mode-cross along the relevant
axis. That is, `Mode.Portability.t` mode-crosses portability. If you have a
scenario where you need a type to mode-cross but it contains a field whose type
does not cross that mode, a modality type might be the answer to your problem.

## Modalities and mutable fields

Mutable fields require special consideration. Consider this example:

```ocaml
type t = { mutable f : unit -> unit }

let set (t : t @ nonportable) =
  t.f <- (fun () -> incr global_counter)
  
let bad_news () =
  let t @ portable = { f = (fun () -> ()) } in
  let non_portable_t @ nonportable) = t in
  set non_portable_t;
  spawn t.f
```

This creates a `portable` `t`, uses submoding to turn it into a `nonportable`
`t` (this is safe; it's just locally forgetting the capability to send `t` to
another thread), and then calls `set` to put a nonportable function into `t`.
Then it takes the `portable` reference to `t` and sends it to another thread.
Horrors! The problem here is that, in `set`, just because `t` is `nonportable`,
we cannot assume that we can write a `nonportable` function into `t.f`.

To prevent examples like this from being allowed, mutable fields have
*implied modalities*. Every mutable field has the modalities derived from
the legacy modes (the modes that describe OCaml values that existed before
we had modes), as given here:

```
global aliased many nonportable uncontended unyielding stateful read_write
```

In the example above, that means that `f` has the `nonportable` modality
(among others); accordingly, the call `spawn t.f` would fail, because `spawn`
would expect a `portable` function.
