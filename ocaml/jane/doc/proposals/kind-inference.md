# Kind inference

This document is a formal description of the kind system Richard is
proposing for OCaml. It is heavy on symbols and light on text.

We use `{{ ... }}` to denote an optional piece of syntax, and
`[[ ... ]]` to denote zero or more occurrences of a piece of syntax.
In `[[ ... ]]ᵢ`, the `ᵢ` is the number of repetitions of the syntax.
We sometimes write `[[ ... | <punct> ]]` to denote that <punct> is
a separating punctuation in the list. A `+` at the end of a list denotes
that the list contains at least one element. A `++` at the end of a list
denotes that the list contains at least two elements.

The syntax here is not exactly OCaml syntax, deviating where convenient.

The definitions are implicitly self-recursive, but mutual recursion is
written explicitly with `rec`. Later definitions may refer to earlier ones,
but not vice versa.

Grammar:

```
a ::= (* type variable name *)
t ::= (* type constructor name *)
k ::= (* kind abbreviation name *)
ℓ ::= (* label name *)
K ::= (* constructor name *)
M ::= (* module name *)

(* modes are written in increasing order within an axis *)

(* comonadic axes = prescriptive axes *)
lm ::= global | local
om ::= many | separate | once
pm ::= portable | observing | nonportable
em ::= external | external64 | internal

(* monadic axes = descriptive axes *)
um ::= unique | exclusive | aliased
cm ::= uncontended | shared | contended

m ::= lm | om | um | cm | pm | em
modes, ms ::= [[ m ]]  (* [ms] is used when all modes are from the same axis *)

modality ::= global | many | portable | external | aliased | contended
modalities ::= [[ modality ]]

const_layout ::= any | value | float64 | void | [[ const_layout | * ]]
             |   ( const_layout ) | ...

rec {
  τ ::= τs t | [[ τ | * ]]++ | #( [[ τ | * ]]++ ) | #( ) | σ₁ -> τ₂
    | 'a | τ as (_ : jkind) | (τ) | polymorphic_variant | class_type
  τs ::= [[ τ | , ]]

  σ ::= τ | type_param . σ

  field_type ::= σ @@ modalities
  (* we may leave out [@@] when there are no modalities *)

  field_types(sep) ::= [[ field_type | sep ]]

  jkind ::= k | const_layout | kind_of_ τ | jkind mod modes | jkind with field_types(and)
  (* we may leave out [mod], [with], and [@@] when nothing appears after them *)

  type_param ::= 'a {{ : jkind }}
}

type_params ::= [[ type_param | , ]]+

record_kind ::= { [[ {{ mutable }} ℓ : field_type | ; ]]+ }

constructor_args ::=
  | field_types(*)
  | record_kind

type_kind ::=
  | {{ private }} record_kind {{ [@@unboxed] }}
  | {{ private }} [[ K : constructor_args -> τs t | \| ]] {{ [@@unboxed] }}
  (* choose GADT notation as it subsumes non-GADT notation *)
  | ..

type_decl ::=
  type {{ (type_params) }} t {{ : jkind }} {{ = {{ private }} τ }} {{ = type_kind }}

type_subst ::=
  type {{ (type_params) }} t := τ

sig_item ::= type_decl
         |   type_subst
         |   kind_abbrev_ k = jkind

module_type ::= sig [[ sig_item ]] end

rec {
  struct_item ::= type_decl
              |   kind_abbrev_ k = jkind
              |   module M : module_type = module_expr

  module_expr ::= struct [[ struct_item ]] end
}

(* constructs used only in typing rules: *)

Ξ ::= locality | onceness | uniqueness | contention | portability | externality
layout ::= layout_of σ | const_layout
κ ::= layout; ⟪m_Ξ with field_types⟫  (* syntax with ⟪ and ⟫ defined below *)
(* we may leave out a [with] when there are no types afterwards *)

δ ::= (* left abstract; internal representation of a [type_kind] *)

tconstr_jkind = π [[ 'a : κ ]]. κ₀
  (* That should be a capital π, but that looks too much like ⨅. *)

rec {
  type_binding ::=
    | 'a : κ
    | t : tconstr_jkind            (* abstract type *)
    | t = λ [[ 'a : κ ]]. τ        (* type abbreviation *)
    | t := λ [[ 'a : κ ]]. τ       (* type substitution in a signature *)
    | t := λ [[ 'a : κ ]]. δ : κ₀  (* quantified nominative type description *)
    | k = κ                        (* kind abbreviation *)
    | M = Γ
  Γ ::= [[ type_binding | , ]]
}

μ ::= (* semantic modality *)
```

Meta-syntax:

* `⊤_Ξ` denotes the top mode of axis `Ξ`.
* `⊥_Ξ` denotes the bottom mode of axis `Ξ`.
* Given `m₁` and `m₂` in the same axis, `m₁ ⊔ m₂` denotes join (least upper bound).
* Similarly, `m₁ ⊓ m₂` denotes meet (greatest lower bound).
* Given many modes `ms` from the same axis, `⨆ ms` denotes join of the list. If `ms` is empty, `⨆ ms` is `⊥`.
* Similarly, `⨅ ms` denote meet. If `ms` is empty, `⨅ ms` is `⊤`.

We write `ms = extract(Ξ, modes)` to extract all modes from axis `Ξ`.

We write `⟪ ... ⟫` to denote repeating the inner expression for each
of the six modal axes. In that expression, `Ξ` refers to the axis in play.

Each syntactic modality is associated with an axis `Ξ` and function `μ : Ξ → Ξ`.
We write `μ = extract(Ξ, modalities)` to denote the modality function along `Ξ`
included in `modalities`. If no modality along axis `Ξ` is in `modalities`,
then `extract(Ξ, modalities)` is the identity function. If more than one
modality along `Ξ` is in `modalities`, `extract(Ξ, modalities)` fails (it
is a partial function; any rule using it will fail if the function fails).

Given an axis `Ξ` and modality function `μ : Ξ → Ξ`, let `sup(μ)` be a mode in `Ξ`,
calculated in this way: Let `ms₀ = { m ∈ Ξ | ∀ m' ∈ Ξ, m ≤ m' ⇒ μ(m') = μ(m) }`.  Then let
`ms₁ = { m ∈ ms₀ | ∀ m' ∈ ms₀, ¬ (m' ≤ m ∧ m' ≠ m) }`.  Finally, `sup(μ) = ⨆
ms₁`. `sup(μ)` is the least mode in `Ξ` such that `μ` treats all modes greater than
`sup(μ)` identically. For a constant modality `μ`, `sup(μ) = ⊥`. For an identity modality
`μ`, `sup(μ) = ⊤`.

All modalities in our system are either constant or identity functions. Therefore,
`sup(μ)` is either `⊤` or `⊥`. Furthermore, `sup(μ)` is used only in a `⊓` operation.
For constant modalities `μ` (where `sup(μ) = ⊥`), `m ⊓ sup(μ) = ⊥`. For identity
modalities `μ` (where `sup(μ) = ⊤`), `m ⊓ sup(μ) = m`. We thus do not need to track
modalities in internal kinds `κ`. We can thus simplify the definition of `κ` to

```
modal_bound_Ξ ::= m_Ξ with [[ σ ]]
κ ::= layout; ⟪modal_bound_Ξ⟫
```

To convert from `field_types` to the types in a `modal_bound`, we
use

    types_for(Ξ, [[ σᵢ @@ modalitiesᵢ ]]ₙ) =
      { σᵢ | i ∈ [1, j] ∧ sup(extract(Ξ, modalitiesᵢ)) = ⊤ }

This extracts all types `σᵢ` such that its modality along `Ξ`
is the identity. Put another way, this gets all the types from
`field_types`, omitting those with a modality along `Ξ`.

We write `Ξ(κ)` to denote the mode in `κ` corresponding to axis `Ξ`
and `lay(κ)` to denote the layout in `κ`.

Typing rules:

```
rec {

Γ ⊢ jkind ↠ κ  (* translate a user-written jkind to an internal κ *)
=============

k = κ ∈ Γ
--------- K_ABBREV
Γ ⊢ k ↠ κ

---------------------------------------- K_LAYOUT
Γ ⊢ const_layout ↠ const_layout; ⟪⊤_Ξ⟫

Γ ⊢ τ : κ
------------------------------------------ K_OF
Γ ⊢ kind_of_ τ ↠ layout_of τ; ⟪⊥_Ξ with τ⟫

Γ ⊢ jkind ↠ κ
------------------------------------------------- K_MOD
Γ ⊢ jkind mod modes ↠ lay(κ); ⟪Ξ(κ) ⊓ ⨅ extract(Ξ, modes)⟫

Γ ⊢ jkind ↠ κ
∀ σᵢ ∈ field_types, Γ ⊢ σᵢ : κᵢ
---------------------------------------------------------------------- K_WITH
Γ ⊢ jkind with field_types ↠ lay(κ); ⟪Ξ(κ) with types_for(Ξ, field_types)⟫
```

In `K_OF`, we produce a kind in terms of `τ`, not just `κ`. This allows us
to make use of refinements to `τ` that we learn later, perhaps through
functor application or GADT refinement.

```
Γ ⊢ σ : κ   (* check a type σ to have kind κ *)
=========

t : π [[ 'aᵢ : κᵢ ]]ₙ. κ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
------------------ T_ABSTRACT
Γ ⊢ [[ τᵢ ]]ₙ t : κ[ [[ τᵢ/'aᵢ ]] ]

t = λ [[ 'aᵢ : κᵢ ]]ₙ. τ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
Γ ⊢ τ[ [[ τᵢ/'aᵢ ]] ] : κ
------------------- T_ABBREV
Γ ⊢ [[ τᵢ ]]ₙ t : κ

t := λ [[ 'aᵢ : κᵢ ]]ₙ. τ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
Γ ⊢ τ[ [[ τᵢ/'aᵢ ]] ] : κ
------------------- T_SUBST
Γ ⊢ [[ τᵢ ]]ₙ t : κ

t := λ [[ 'aᵢ : κᵢ ]]. δ : κ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
------------------- T_NOMINATIVE
Γ ⊢ [[ τᵢ ]]ₙ t : κ[ [[ τᵢ/'aᵢ ]] ]

∀ i, Γ ⊢ τᵢ : κᵢ
∃ j ∈ [0, n],
  (∀ i ∈ [1, j], lay(κᵢ) = value) ∧ (∀ i ∈ (j, n], externality(κᵢ) ≤ external64)
  (* this premise implements the ordering restriction on mixed blocks *)
-------------------------- T_TUPLE
Γ ⊢ [[ τᵢ | * ]]ₙ++ :
  value; local; many with [[ τᵢ ]]; aliased; uncontended with [[ τᵢ ]];
  portable with [[ τᵢ ]]; internal

∀ i, Γ ⊢ τᵢ : κᵢ
-------------------------------- T_UNBOXED_TUPLE
Γ ⊢ #( [[ τᵢ | * ]]ₙ++ ) : [[ layout_of τᵢ | * ]]; ⟪⊥_Ξ with [[ τᵢ ]]⟫

------------------------ T_UNBOXED_UNIT
Γ ⊢ #( ) : void; ⟪⊥_Ξ⟫

Γ ⊢ σ₁ : κ₁
Γ ⊢ τ₂ : κ₂
----------------------------------------------------------------------------- T_ARROW
Γ ⊢ σ₁ -> τ₂ : value; local; once; unique; uncontended; nonportable; internal

'a : κ ∈ Γ
---------- T_VAR
Γ ⊢ 'a : κ

Γ ⊢ τ : κ₁
Γ ⊢ jkind ↠ κ₂
Γ ⊢ κ₁ ≤ κ₂
------------------------- T_CONSTRAINT
Γ ⊢ τ as (_ : jkind) : κ₂

Γ, 'a : value; ⟪⊤_Ξ⟫ ⊢ σ : κ  (* or the kind of ['a] is κ₀ if we decide to infer it *)
'a # κ
-------------- T_POLY_DEFAULT
Γ ⊢ 'a. σ : κ

Γ ⊢ jkind ↠ κ₀
Γ, 'a : κ₀ ⊢ σ : κ
'a # κ
----------------------- T_POLY
Γ ⊢ ('a : jkind). σ : κ

TODO
---------------------------------------- T_POLY_VARIANT
Γ ⊢ polymorphic_variant : value; ⟪⊤_Ξ⟫

TODO
------------------------------- T_CLASS
Γ ⊢ class_type : value; ⟪⊤_Ξ⟫


Γ ⊢ground layout ↠ const_layout   (* ground out the layout, resolving [layout_of] *)
================================

------------------------------- LG_CONST
Γ ⊢ground const_layout ↠ const_layout

Γ ⊢ σ : κ
------------------------ LG_OF
Γ ⊢ground layout_of σ ↠ lay(κ)


Γ ⊢ layout₁ ≤ layout₂
=============================

Γ ⊢ground layout₁ ↠ const_layout₁
Γ ⊢ground layout₂ ↠ const_layout₂
const_layout₁ ≤ const_layout₂
------------------------------- L_SUB
Γ ⊢ layout₁ ≤ layout₂


Γ ⊢ground modal_bound_Ξ ↠ m_Ξ   (* ground out the bound, resolving [with] *)
=============================

∀ i, Γ ⊢ σᵢ : κᵢ
---------------------------------------------- MG
Γ ⊢ground m_Ξ with [[ σᵢ ]] ↠ m_Ξ ⊔ (⨆ [[ Ξ(κᵢ) ]])


Γ ⊢ κ₁ ≤ κ₂
===========

Γ ⊢ layout₁ ≤ layout₂
∀ Ξ:
  Γ ⊢ground modal_bound₁_Ξ ↠ m₁_Ξ
  Γ ⊢ground modal_bound₂_Ξ ↠ m₂_Ξ
  m₁_Ξ ≤ m₂_Ξ
-------------------------------------------------------- SUB
Γ ⊢ layout₁; ⟪modal_bound₁_Ξ⟫ ≤ layout₂; ⟪modal_bound₂_Ξ⟫

}

Γ ⊢tk type_kind ↠ δ : κ  (* translate a [type_kind] to an internal rep δ of kind κ *)
=======================

field_types = extract_types(record_kind)
∀ σᵢ ∈ field_types:
  Γ ⊢ σᵢ : κᵢ
m_contention = if mutable ∈ record_kind then contended else uncontended
m_locality, m_externality = if (∀ i, κᵢ = void) then global, external else local, internal
m_uniqueness = shared
m_linearity = many
m_portability = portable
δ represents {{ private }} record_kind
---------------------------------- TK_RECORD
Γ ⊢tk {{ private }} record_kind ↠
  δ : value; ⟪m_Ξ with types_for(Ξ, field_types)⟫

Γ ⊢ σ : κ
δ represents the type_kind
---------------------------------------------- TK_UNBOXED
Γ ⊢tk {{ private }} (( { ℓ : σ @@ modalities }
                     | K : σ @@ modalities -> τs t
                     | K : { ℓ : σ @@ modalities } -> τs t )) [@@unboxed] ↠
  δ : lay(κ); ⟪⊥_Ξ with types_for(Ξ, σ @@ modalities)⟫

field_types = extract_types([[ constructor_argsᵢ ]]ₙ)
∀ σᵢ ∈ field_types:
  Γ ⊢ σᵢ : κᵢ
m_contention = if mutable ∈ [[ constructor_argsᵢ ]] then contended else uncontended
m_locality, m_externality = if (∀ i, κᵢ = void) then global, external else local, internal
m_uniqueness = if record_kind ∈ [[ constructor_argsᵢ ]] then shared else unique
m_linearity = many
m_portability = portable
δ represents {{ private }} [[ Kᵢ : constructor_argsᵢ -> τsᵢ t ]]ₙ
---------------------------------------------------------------- TK_VARIANT
Γ ⊢tk {{ private }} [[ Kᵢ : constructor_argsᵢ -> τsᵢ t ]]ₙ ↠
  δ : value; ⟪m_Ξ with types_for(Ξ, field_types)⟫

δ represents ..
--------------------------- TK_EXTENSIBLE
Γ ⊢tk .. ↠ δ : value; ⟪⊤_Ξ⟫


Γ ⊢ δ₁ reexports δ₂
===================

(* left abstract *)


Γ ⊢?any type_params ↠ [[ 'aᵢ : κᵢ ]]  (* the [?any] is essentially an optional param *)
====================================

--------- P_EMPTY
Γ ⊢?any ∅ ↠ ∅

Γ, 'a : κ₀ ⊢?any type_params ↠ [[ 'bᵢ : κᵢ ]]
κ₀ ≠ any
-------------------------------------------- P_INFER
Γ ⊢ 'a, type_params ↠ 'a : κ₀, [[ 'bᵢ : κᵢ ]]

Γ, 'a : κ₀ ⊢?any type_params ↠ [[ 'bᵢ : κᵢ ]]
-------------------------------------------- P_INFER_ANY
Γ ⊢any 'a, type_params ↠ 'a : κ₀, [[ 'bᵢ : κᵢ ]]

Γ ⊢ jkind ↠ κ₀
Γ, 'a : κ₀ ⊢?any type_params ↠ [[ 'bᵢ : κᵢ ]]
---------------------------------------- P_KINDED
Γ ⊢?any 'a : jkind, type_params ↠ 'a : κ₀, [[ 'bᵢ : κᵢ ]]
```

The `⊢any` variant allows us to infer `any` as the layout of a type
variable. It is used for e.g. `type 'a t := ...` in signatures.

```
Γ ⊢ type_decl ↠ Γ'  (* translate a [type_decl] into a fresh env Γ' *)
==================

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
---------------------------------------------------------- D_ABSTRACT
Γ ⊢ type type_params t ↠ t : π [[ 'aᵢ : κᵢ ]]. value; ⟪⊤_Ξ⟫

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]], t : tconstr_jkind ⊢ jkind ↠ κ₀  (* [t] may appear in its kind *)
tconstr_jkind = π [[ 'aᵢ : κᵢ ]]. κ₀
------------------------------------------------- D_KINDED
Γ ⊢ type type_params t : jkind ↠ t : tconstr_jkind

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : κᵢ ]]. τ
--------------------------------------------- D_ABBREV
Γ ⊢ type type_params t = τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t : π [[ 'aᵢ : κᵢ ]]. κ₀   (* private: make [t] abstract in the result *)
---------------------------------------- D_PRIVATE
Γ ⊢ type type_params t = private τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]], t : π [[ 'aᵢ : κᵢ ]]. κ₀ ⊢tk type_kind ↠ δ : κ₀
--------------------------------------------- D_NOMINATIVE
Γ ⊢ type type_params t = type_kind ↠ t := λ [[ 'aᵢ : κᵢ ]]. δ : κ₀

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : κᵢ ]]. τ
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ jkind ↠ κ₀'
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ κ₀ ≤ κ₀'
------------------------------------- D_KINDED_ABBREV
Γ ⊢ type type_params t : jkind = τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t : π [[ 'aᵢ : κᵢ ]]. κ₀'        (* κ₀', not κ₀, for private abbrevs *)
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ jkind ↠ κ₀'
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ κ₀ ≤ κ₀'
--------------------------------------------- D_KINDED_PRIVATE
Γ ⊢ type type_params t : jkind = private τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
τ = [[ 'aᵢ ]] t'
Γ, [[ 'aᵢ : κᵢ ]], t : π [[ 'aᵢ : κᵢ ]]. κ₀ ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : κᵢ ]]. τ
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢tk type_kind ↠ δ : κ₀'
t' := λ [[ 'aᵢ : κᵢ ]]. δ' : κ₀'' ∈ Γ
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ δ reexports δ'
----------------------------------------- D_REEXPORT
Γ ⊢ type type_params t = τ = type_kind ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]], t : π [[ 'aᵢ : κᵢ ]]. κ₀ ⊢tk type_kind ↠ δ : κ₀'
Γ' = t := λ [[ 'aᵢ : κᵢ ]]. δ : κ₀  (* user-written jkind is preserved *)
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ jkind ↠ κ₀
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ κ₀' ≤ κ₀
----------------------------------------------- D_KINDED_NOMINATIVE
Γ ⊢ type type_params t : jkind = type_kind ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : κᵢ ]]
τ = [[ 'aᵢ ]] t'
Γ, [[ 'aᵢ : κᵢ ]], t : π [[ 'aᵢ : κᵢ ]]. κ₀ ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : κᵢ ]]. τ
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢tk type_kind ↠ δ : κ₀'
t' := λ [[ 'aᵢ : κᵢ ]]. δ' ∈ Γ
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ δ reexports δ'
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ jkind ↠ κ₀''
Γ, [[ 'aᵢ : κᵢ ]], Γ' ⊢ κ₀ ≤ κ₀''
-------------------------------------------------- D_KINDED_REEXPORT
Γ ⊢ type type_params t : jkind = τ = type_kind ↠ Γ'


(Γ₁ ⊢ τ₁) =_Γ₀ (Γ₂ ⊢ τ₂)
========================

(* left abstract; This checks whether τ₁ (in context Γ₁) equals τ₂ (in context Γ₂),
but with access to equalities in Γ₀. This "access to equalities" bit seems necessary
to accept

  {[
    module M : sig
      type a
      type b
      type c = b
    end = struct
      type a
      type c = a
      type b = c
    end
  ]}
  
The struct is acceptable only with the last equation, which occurs *after* the definition
of [c]. *)


Γ ⊢ground κ ↠ const_layout; ⟪m_Ξ⟫
=================================

Γ ⊢ground layout ↠ const_layout
∀ Ξ, Γ ⊢ground modal_bound_Ξ ↠ m_Ξ
------------------------------------------------------- KG
Γ ⊢ground layout; ⟪modal_bound_Ξ⟫ ↠ const_layout; ⟪m_Ξ⟫


Γ ⊢find t ∈ Γ₂ ↠ (Γ₂' ⊢ tconstr_jkind)  (* find [t] in Γ₂, with prefix env Γ₂' *)
======================================

Γ₂ = Γ₂', t : tconstr_jkind ∈ Γ₂, Γ₂''
--------------------------- FIND_ABSTRACT
Γ ⊢find t ∈ Γ₂ ↠ (Γ₂' ⊢ tconstr_jkind)

Γ₂ = Γ₂', t = λ [[ 'aᵢ : κᵢ ]]. τ, Γ₂''
Γ, Γ₂', [[ 'aᵢ : κᵢ ]] ⊢ τ : κ₀
------------------------------------- FIND_ABBREV
Γ ⊢find t ∈ Γ₂ ↠ (Γ₂' ⊢ π [[ 'aᵢ : κᵢ ]]. κ₀)

Γ₂ = Γ₂', t := λ [[ 'aᵢ : κᵢ ]]. δ : κ₀, Γ₂''
--------------------------------------- FIND_NOMINATIVE
Γ ⊢find t ∈ Γ₂ ↠ (Γ₂' ⊢ π [[ 'aᵢ : κᵢ ]]. κ₀)


Γ ⊢include (Γ₀ ⊢ type_binding) ∈~ Γ₂
  (* check whether type_binding (in env Γ₀) is more general than a definition in Γ₂,
     with ambient env Γ *)
====================================

tconstr_jkind₁ = π [[ 'aᵢ : κ₁ᵢ ]]ₙ. κ₁₀
Γ ⊢find t ∈ Γ₂ ↠ (Γ₂' ⊢ π [[ 'bᵢ : κ₂ᵢ ]]ₙ. κ₂₀)
∀ 1 ≤ j ≤ n:
  Γ, Γ₀, [[ 'aᵢ : κ₁ᵢ ]]ⱼ₋₁ ⊢ground κ₁ᵢ ↠ κ₁ᵢ'
  Γ, Γ₂', [[ 'bᵢ : κ₁ᵢ' ]]ⱼ₋₁ ⊢ κ₁ᵢ' ≤ κ₂ᵢ 
    (* check with more restrictive κ₁, not κ₂; this is also contravariant *)
Γ, Γ₀, [[ 'aᵢ : κ₁ᵢ ]]ₙ ⊢ground κ₁₀ ↠ κ₁₀'
Γ, Γ₂', [[ 'bᵢ : κ₁ᵢ' ]]ₙ ⊢ κ₂₀ ≤ κ₁₀'
------------------------------------------ INCL_ABSTRACT
Γ ⊢include (Γ₀ ⊢ t : tconstr_jkind₁) ∈~ Γ₂

Γ₂ = Γ₂', t = λ [[ 'aᵢ ]]ₙ. τ₂, Γ₂''
t : π [[ 'aᵢ : κᵢ ]]ₙ. κ₀ ∈ Γ₀
∀ 1 ≤ j ≤ n:
  Γ, Γ₀, [[ 'aᵢ : κᵢ ]]ⱼ₋₁ ⊢ground κᵢ ↠ κᵢ'
(Γ, Γ₀, [[ 'aᵢ : κᵢ' ]]ₙ ⊢ τ₁) =_Γ₂ (Γ, Γ₂', [[ 'aᵢ : κᵢ' ]]ₙ ⊢ τ₂)
----------------------------------------- INCL_ABBREV
Γ ⊢include (Γ₀ ⊢ t = λ [[ 'aᵢ ]]. τ₁) ∈~ Γ₂

(* nothing to check for here *)
------------------------------------------ INCL_SUBST
Γ ⊢include (Γ₀ ⊢ t := λ [[ 'aᵢ ]]. τ) ∈~ Γ₂

TODO: More type_binding rules


Γ ⊢includemod Γ₂ ≤ (Γ₀ ⊢ Γ₁)  (* check whether Γ₁, with prefix Γ₀, covers Γ₂ *)
============================

--------------------------- IM_EMPTY
Γ ⊢includemod Γ₂ ≤ (Γ₀ ⊢ ∅)

Γ ⊢include (Γ₀ ⊢ type_binding) ∈~ Γ₂
Γ ⊢includemod Γ₂ ≤ (Γ₀, type_binding ⊢ Γ₁)
----------------------------------- IM_BINDING
Γ ⊢includemod Γ₂ ≤ (Γ₀ ⊢ type_binding, Γ₁)


Γ ⊢sig sig_item ↠ Γ'
==================

Γ ⊢ type_decl ↠ Γ'
---------------------- SIG_TYPE_DECL
Γ ⊢sig type_decl ↠ Γ'

Γ ⊢ jkind ↠ κ
---------------------------------------- SIG_KIND_ABBREV
Γ ⊢sig kind_abbrev_ k = jkind ↠ k = κ

Γ ⊢any type_params ↠ [[ 'aᵢ : κᵢ ]]
Γ, [[ 'aᵢ : κᵢ ]] ⊢ τ : κ₀
-------------------------------------------------- SIG_TYPE_SUBST
Γ ⊢sig type type_params t := τ ↠ t := λ [[ 'aᵢ ]]. τ


Γ ⊢mty module_type ↠ Γ'
=======================

∀ 1 ≤ i ≤ n, Γ₀,…,Γᵢ₋₁ ⊢sig sig_itemᵢ ⊢ Γᵢ
------------------------------------------ MTY_SIG
Γ₀ ⊢mty sig [[ sig_itemᵢ ]]ₙ end ↠ Γ₁,…,Γₙ


rec {

Γ ⊢struct struct_item ↠ Γ'
==========================

Γ ⊢ type_decl ↠ Γ'
------------------------- STRUCT_TYPE_DECL
Γ ⊢struct type_decl ↠ Γ'

Γ ⊢ jkind ↠ κ
------------------------------------------- STRUCT_KIND_ABBREV
Γ ⊢struct kind_abbrev_ k = jkind ↠ k = κ

Γ ⊢mty module_type ↠ Γ₁
Γ ⊢mod module_expr ↠ Γ₂
Γ ⊢includemod Γ₂ ≤ (∅ ⊢ Γ₁)
---------------------------------------------- STRUCT_MOD
Γ ⊢struct module M : module_type = module_expr ↠ M = Γ₁


Γ ⊢mod module_expr ↠ Γ'
=======================

∀ 1 ≤ i ≤ n, Γ₀,…,Γᵢ₋₁ ⊢struct struct_itemᵢ ⊢ Γᵢ
------------------------------------------------ MOD_STRUCT
Γ₀ ⊢mod struct [[ struct_itemᵢ ]]ₙ end ↠ Γ₁,…,Γₙ

}

```

# Examples

```
kind_abbrev_ immutable_data = value mod many portable uncontended
type 'a t = { x : 'a; y : ('a -> 'a) @@ portable; z : ('a -> 'a) @@ many }

↠
type 'a t : immutable_data
              with 'a
	      and ('a -> 'a) @@ portable many
	      and ('a -> 'a) @@ many

↠ layout: value
  locality: ⊤ with 'a
              and ('a -> 'a)
	      and ('a -> 'a)
  linearity: many with 'a
  uniqueness: ⊤ with 'a
                and ('a -> 'a)
		and ('a -> 'a)
  portability: portable with 'a
                        and ('a -> 'a)
  contention: uncontended with 'a
                          and ('a -> 'a)
			  and ('a -> 'a)
  externality: ⊤ with 'a
                 and ('a -> 'a)
		 and ('a -> 'a)
≅ layout: value
  locality: ⊤
  linearity: many with 'a
  uniqueness: ⊤
  portability: nonportable (= ⊤)
  contention: uncontended with 'a
  externality: ⊤

==================

(* example showing why we need kind_of *)

module type S1 = sig
  type t1 : any
end

module type S2 = functor (X : S1) -> sig
  type t2 : kind_of_ X.t1
end

module M1 : S2 = functor (X : S1) -> struct
  type t2 = X.t1
end

module M1' (X : S1) : sig
  type t2 : kind_of_ X.t1
end = struct
  type t2 = X.t1
end

module M2 = struct
  type t1 = string
end

module M3 = M1(M2)

let f (x : M3.t2) = x

type ('a, 'b : kind_of_ 'a) t = ...

===========================

type 'a list =
  | Nil
  | Cons of 'a * 'a list
(* string list crosses portability axis *)

type 'a other =
  | Nil
  | Cons of 'a * ('a -> 'a) * 'a other
(* string other does not cross portability axis *)

type 'a id =
  | Id of 'a [@@unboxed]
(* string id crosses uniqueness, but only because of the [@@unboxed]
  *)
```
