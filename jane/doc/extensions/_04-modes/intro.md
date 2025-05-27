---
layout: documentation-page
collectionName: Modes
title: Intro
---

<style>
.table {
    width: fit-content;
    margin-left: auto;
    margin-right: auto;
    margin-bottom: 20px;
    border-style: solid;
    border-color: blue;
    border-radius: 25px;
    padding: 15px;
    text-align: center;
}
</style>

# Introduction to the Mode System

Modes are deep properties of values that are tracked by the OxCaml
compiler. Like types, they are inferred from definitions and checked for
consistency. (We use the term "type checking" to include both traditional type
checking/inference and mode checking/inference.) Modes have similarities and
relationships with types, but remain distinct: types are not modes, modes are
not types, types do not have modes, and modes do not have types. Types describe
what the data *is*, while modes describe how it is *used*.

Each mode is associated with a particular operation that may be performed on a
value. The mode may be a *past* mode, which tracks whether the operation has
happened to this value in the past; or a *future* mode, which tracks whether the
operation is allowed to happen to this value in the future. Modes are deep; when
attached to structured data, they apply to components, recursively. (OxCaml's
*modality* feature cuts off the deepness of a mode; modalities can be placed on
record and constructor fields.)

Just like a value has a type, a value in OxCaml also has a mode. Types do *not*
have a mode. That is, we do not have a type `string @ local` (say), but rather
if we have `(x : string @ local)`, then `x` has type `string` and is at mode
`local`. Modes also appear in the argument and return slots of a function type,
so we can have `string @ local -> string option @ global` to describe a function
whose argument will have the `local` mode and whose return will have the
`global` mode. These modes appear in the function's type but are associated with
the function's behavior, not the argument or result types (that is, there is
still no `string @ local` or `string option @ global`). Modes are considered
part of the type system; the type checker in OxCaml additionally checks for
correct usage of modes.

This page shows the modes that are currently supported. Each mode belongs to a
modal *axis* determined by the operation it tracks and whether it is a past or
future mode. The axes on this page are arranged with the least mode at the
bottom and the greatest mode at the top.

The type system supports *submoding*: values may move freely to greater modes
(which typically restrict what can be done with those values) but not to lesser
modes. Additionally, the type system knows that some types don't have
interesting interactions with some modes. Such types are said to *mode cross* on
those axes, which means values of these types may freely move in either
direction on the axis. The sections for each axis below describe which types can
mode cross on that axis.

Each axis has a *legacy* mode, shown in bold. This is the "default" mode, and is
chosen to make the modal type system backwards compatible with legacy OCaml
programs.

* [Modes for scope (locality)](#locality)
* [Modes for moving between threads (portability and
  contention)](#portability-contention)
* [Modes for aliasing (uniqueness and linearity)](#uniqueness-linearity)

# Modes for scope {#locality}

## Future modes: Locality

|------------|
| local      |
| `|`        |
| **global** |
{: .table }

Locality is a future axis that tracks whether a value is allowed to escape its
*region*. Regions are scopes created by standard OCaml language constructs: each
function's body is a region, as are loop bodies.

The type checker does not allow values that are *local* to escape their scope
(for example, by being returned from a function or stored in a global
ref). Values that are *global* may freely escape their scope. The compiler can
stack allocate values that are local.

Locality is irrelevant for types that never cause allocation on the OCaml heap,
like int. Values of such types *mode cross* on the locality axis; they may be
used as global even when they are local.

See also the [documentation on locality and stack
allocation](../stack-allocation/intro).

# Modes for moving between threads {#portability-contention}

## Past modes: Contention

|-----------------|
| contended       |
| `|`             |
| shared          |
| `|`             |
| **uncontended** |
{: .table }

Contention is a past axis that tracks whether a value has been shared between
threads. A value is *contended* if another thread can write to it, *shared* if
multiple threads have read-only access to it, and *uncontended* otherwise.

To enforce data race freedom, the typechecker does not permit reading or writing
unprotected mutable portions of contended values. (Types like `Atomic.t` protect
mutable values from data races and allow contended values to still retain
mutable components.) The unprotected mutable portions of shared values
may be read, but not written to. Uncontended values may be accessed and mutated
freely.

Contention is irrelevant for types that are deeply immutable. Values of such
types *mode cross* on the contention axis; they may be used as uncontended even
when they are contended.

## Future modes: Portability

|-----------------|
| **nonportable** |
| `|`             |
| portable        |
{: .table }

Portability is a future axis that tracks whether a value is permitted to be
shared with another thread. OxCaml's parallelism API does not allow
*nonportable* values to move across thread boundaries, while *portable* values
may move freely.

Portability is about functions: functions that capture uncontended mutable state
are not portable.

Notably, it is generally safe to send mutable data *itself* to other threads,
because it will then be *contended*, so the mutable portions will be
inaccessible. What is scary is to send a function that *captures* uncontended
mutable data to another thread, because the captured data would remain
uncontended even when the function is shared. When the second thread runs the
funtion, both threads would be accessing the same uncontended mutable state (a
data race!).

Portability is irrelevant for types that do not contain functions. Values of
such types *mode cross* on the portability axis; they may be used as portable
even when they are nonportable.

# Modes for aliasing {#uniqueness-linearity}

## Past modes: Uniqueness

|-------------|
| **aliased** |
| `|`         |
| unique      |
{: .table }

Uniqueness is a past axis that tracks whether there are multiple references to a
value. A value is *unique* if there is only one reference to it, and *aliased*
otherwise.

A function that accepts a unique argument effectively consumes this argument,
since the caller will only be able to supply arguments they have no other
references to. This can be used to implement APIs like safe memory allocators
that ensure no use-after-free bugs.

In the future, we will implement *overwriting* on unique values. Overwriting is
an optimization that reuses the memory of a value in place, rather than
allocating a new copy. For example, we will be able to write a `List.map` that
reuses the memory in place, rather than allocating a new list, when we know no
other references to this list exist.

Uniqueness is irrelevant for types that don't contain any memory locations
subject to overwriting (even though we have not yet implemented overwriting).
Values of such types *mode cross* on the uniqueness axis; they may
be used as unique even when they are aliased.

For example, types which do not involve data allocated on the OCaml heap mode
cross on this axis, so all types that mode cross locality also mode cross
uniqueness. Some other types that we don't plan to support overwriting for can
also mode cross uniqueness, like functions.

See also the [documentation on uniqueness and
linearity](../../uniqueness/intro/).

## Future modes: Linearity

|----------|
| once     |
| `|`      |
| **many** |
{: .table }

Linearity is a future axis that tracks whether a function is permitted to be
aliased.  Values that are *many* may used multiple times, while values that are
*once* may only be used once.

Like portability, linearity is about functions: its purpose is to track unique
values in closures. A closure that captures a unique value is once, ensuring one
can not create multiple references to the unique value by using the function
multiple times.

Linearity is irrelevant for types that do not contain functions. Values of such
types *mode cross* on the linearity axis; they may be used as many even when
they are once.

See also the [documentation on uniqueness and
linearity](../../uniqueness/intro/).

# Modes for effects {#yielding}

## Future modes: Yielding

|----------------|
| yielding       |
| `|`            |
| **unyielding** |
{: .table }

Yielding is a future axis that tracks whether a function is permitted to perform
effects that will be handled in its parent stack. See [the OCaml Manual entry
for effect handlers](https://ocaml.org/manual/5.3/effects.html).

Yielding has different defaults depending on the locality axis: *global* values are
defaulted to *unyielding*, while *local* values are defaulted to *yielding*.
More documentation on mode implications is available [here](../_05-kinds/syntax.md).

Yielding is irrelevant for types that do not contain functions, and values of such types
*mode cross* on the yielding axis; they may be used as unyielding even
when they are yielding.

# Modes for purity {#visibility-statefulness}

## Past modes: Visibility

|----------------|
| immutable      |
| `|`            |
| read           |
| `|`            |
| **read_write** |
{: .table}

Visibility is a past axis that controls access to mutable portions of values.
It's similar to contention: the typechecker forbids accessing mutable fields of values
with *immutable* visiblity, and forbids writing to mutable fields of values
with *read* visibility. Unlike for contention, even thread-safe access is disallowed.

Visibility is irrelevant for types that are deeply immutable. Values of such
types *mode cross* on the visibility axis; they may be used as read_write even
when they are immutable.

## Future modes: Statefulness

|--------------|
| **stateful** |
| `|`          |
| observing    |
| `|`          |
| stateless    |
{: .table}

Statefulness is a future axis that tracks whether a function reads or writes to some
mutable state that it closes over (in other words, state that is not explicitly passed to it in an argument).

*Stateless* functions may not either read or write such state, and *observing*
functions can only read it. *Stateful* functions have no restrictions.
Stateless closures capture all values at visibility *immutable*,
while observing closures capture all values at visibility *read*.

Statefulness is irrelevant for types that do not contain functions, and values of such
types *mode cross* on the statefulness axis; they may be used as stateless
even when they are stateful.
