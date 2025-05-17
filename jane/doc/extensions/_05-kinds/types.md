---
layout: documentation-page
collectionName: Kinds
title: Kinds of types
---

# Kinds of types

[overview]: ../intro

This page describes how we compute the kind of both built-in types and
user-defined types. You may want to read the [overview][] of the kind
system first.

## Kinds of user-defined types

* A boxed record type with at least one mutable field has the kind
  `mutable_data with <<all record fields, with their modalities>>`.

* A boxed record type with no mutable fields has the kind `immutable_data with
  <<all record fields, with their modalities>>`.

* An unboxed record type (including ones implicitly declared when making a boxed
record declaration) has the kind `lay_1 & ... & lay_n mod everything with <<all
record fields, with their modalities>>`. (It is `mod everything` because it does
not allocate.)

* A record declared with `[@@unboxed]` has the same kind as its field.

* A variant type with all constant constructors has the kind `immediate`.

* A variant type with at least one mutable field has the kind `mutable_data with
<<all fields from all constructors, with their modalities>>`. For every
existentially bound variable `'a` of some kind `k`, we substitute `'a` to become
`(type : k)` when building the with-bounds. (Otherwise, the existential would
be out of scope in the with-bounds.)

* A variant type with no mutable fields (but at least one field) has the kind
`immutable_data with <<all fields from all constructors, with their
modalities>>`. For every existentially bound variable `'a` of some kind `k`, we
substitute `'a` to become `(type : k)` when building the
with-bounds. (Otherwise, the existential would be out of scope in the
with-bounds.)

* An extensible variant (declared with `= ..`) has kind `value mod non_float`.

* A `private` abbreviation has the kind of its right-hand side.

* A `private` annotation on a record or variant type does not affect its kind.

* Records and variants can have the attribute
  `[@@unsafe_allow_any_mode_crossing]`; these declarations must have a kind
  annotation. The bounds given in the kind annotation override the bounds
  determined by the rules above.

## Kinds of types in the type algebra

Types formed from the type algebra are also assigned kinds, according to
the following algorithm.

* All type variables have a kind. As described more on the [syntax](../syntax)
page, flexible type variables infer their kinds, while rigid type variables must
be assigned a kind when they are brought into scope. A rigid type variable
brought into scope without a kind annotation defaults to kind `value`.

* A function type `ty_1 -> ty_2` has kind `value mod aliased contended immutable
non_float` without constraining the kinds of either `ty_1` or `ty_2`.

* A tuple `ty_1 * ... * ty_n` (where n >= 2) has kind `immutable_data with <<all
the component types>>` without constraining the kinds of any of the `ty_i`.

* An unboxed tuple `#( ty_1 * ... * ty_n )` (where n >= 2) has kind `lay_1 &
  ... & lay_n mod everything with <<all the component types>>`, where `ty_i` has
  layout `lay_i`.

* An applied type constructor `(ty_1, ..., ty_n) t` (where n >= 0) has the kind
assigned to `t` at its declaration, with type arguments substituted into any
type variables mentioned in the `t` with-bounds. Each of the `ty_i` is constrained
to be a subkind of the kind on the corresponding type parameter to `t`.

* An object type `< f1 : ty_1; ...; fn : ty_n >` has kind `value mod
non_float`. Each `ty_i` is constrained to have a kind that is a subkind of
`value`.

* A polymorphic variant type written via expansion of another type, such as ``[
t | `C ]``, is expanded before computing its kind, which will use the rules
below.

* A closed polymorphic variant type (``[ `K_1 | ... | `K_n ]`` or
``[< `K_1 | ... `K_n ]`` or ``[< `K_1 | ... | `K_n > `K_j | ... | `K_k ]``),
where all constructors are constant constructors, has kind `immediate`.

* A closed polymorphic variant type with at least one non-constant constructor
has kind `immutable_data with <<all fields from all constructors>>`. The
`all fields from all constructors` includes any types in a conjunction built
with `&`.

* An open polymorphic variant type ``[> `K_1 of tys_1 | ... | `K_n of tys_n ]``
has kind `value mod non_float`.

* To compute the kind of a polymorphic type `'a. ty`, start with the kind `k0`
of the inner `ty`. Then find the lowest kind `k` that has no with-bounds but
with `k0 <= k`. The kind of `'a. ty` is `k`. Note that if `k0` has no
with-bounds, then `k = k0`. (We will improve this in the future, with a
treatment like that for existentials in variant type declarations.)

* A first-class module type `(module S)` has kind `value mod non_float`. (We may
be able to improve this, analyzing `S` for mode-crossing opportunities.)

* The type `'a array` has kind `mutable_data with 'a`. The kind of `'a` must
be a subkind of `any mod non_null separable`.

* The type `'a iarray` has kind `immutable_data with 'a`. The kind of `'a` must
be a subkind of `any mod non_null separable`.

* The type `'a lazy_t` has kind `value mod non_float`. The kind of `'a` must be
a subkind of `value`.
