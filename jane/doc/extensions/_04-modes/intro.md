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

Modes are deep properties of values that are tracked by the OxCaml type
system. Each mode is associated with a particular operation that may be
performed on a value. The mode may be a *past* mode, which tracks whether the
operation has happened to this value in the past, or a *future* mode, which
tracks whether the operation is allowed to happen to this value in the future.

This page shows the modes that are currently supported. Each mode belongs to a
modal *axis* determined by the operation it tracks and whether it is a past or
future mode. The axes on this page are arranged with the least mode at the
bottom and the greatest mode at the top. The type system supports *submoding*:
values may move freely to greater modes (which typically restrict what can be
done with those values) but not to lesser modes.

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

More documentation for locality and stack allocation is available
[here](../stack-allocation/intro).

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
threads. A value is *contended* if multiple threads have full access to it,
*shared* if multiple threads have read-only access to it, and *uncontended* if
only one thread has access to it.

To enforce data race freedom, the typechecker does not permit reading or writing
the mutable portions of contended values. The mutable portions of shared values
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
shared with another thread. The type checker does not allow *nonportable* values
to move across thread boundaries, while *portable* values may move freely.

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

More documentation on uniqueness and linearity is available
[here](../../uniqueness/intro/).

## Future modes: Linearity

|----------|
| once     |
| `|`      |
| **many** |
{: .table }

Linearity is a future axis that tracks whether a function is permitted to be
aliased.  Values that are *many* may used multiple times, while values that are
*once* may only be used once.

Linearity is about functions: its purpose is to track unique values in
closures. A closure that captures a unique value is once, ensuring one can not
create multiple references to the unique value by using the function multiple
times.

Linearity is irrelevant for types that do not contain functions. Values of such
types *mode cross* on the linearity axis; they may be used as many even when
they are once.

More documentation on uniqueness and linearity is available
[here](../../uniqueness/intro/).

<!-- CR ccasinghino: Sections needed for statefulness, visibility, and yielding -->

