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
(* These are *backwards* from the way the submode operation works in terms. *)
um ::= aliased | exclusive | unique
cm ::= contended | shared | uncontended

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

  jkind ::= k | const_layout | jkind mod modes

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

jkind_scheme ::= kind_of_ τ | jkind with field_types(and)
  (* we may leave out [with] and [@@] when nothing appears after them *)
  (* These are kept separate from plain [jkind]s to aid in inference: we
     can't have a join on the right of a subjkind check during inference.
     The treatment below allowing [with] constraints on the right makes
     sense only during module inclusion, when we are not doing inference. *)

type_decl ::=
  type {{ (type_params) }} t {{ : jkind_scheme }} {{ = {{ private }} τ }} {{ = type_kind }}

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

  (* syntax with ⟪ and ⟫ defined below *)
χ ::= layout; ⟪m_Ξ⟫                   (* translation of a [jkind] *)
κ ::= layout; ⟪m_Ξ with field_types⟫  (* translation of a [jkind_scheme] *)
(* we may leave out a [with] when there are no types afterwards *)
(* The syntax for [χ] is a subsyntax for [κ]:
     In premises, we may write [κ] where the grammar suggests we write [χ].
     In conclusions, we may write [χ] where the grammar suggests we write [κ]. *)

δ ::= (* left abstract; internal representation of a [type_kind] *)

tconstr_jkind ::= π [[ 'a : χ ]]. κ₀
  (* That should be a capital π, but that looks too much like ⨅. *)

rec {
  type_binding ::=
    | 'a : χ
    | t : tconstr_jkind            (* abstract type *)
    | t = λ [[ 'a : χ ]]. τ        (* type abbreviation *)
    | t := λ [[ 'a : χ ]]. τ       (* type substitution in a signature *)
    | t := λ [[ 'a : χ ]]. δ : κ₀  (* quantified nominative type description *)
    | k = χ                        (* kind abbreviation *)
    | M = Γ
  Γ ::= [[ type_binding | , ]]
}

μ ::= (* semantic modality *)
q ::= best | not_best   (* quality of an inferred kind; best < not_best *)
```

Mode crossing is not covered in this design. 

However, it may be helpful to know that having e.g. `mod observing` in a kind
means that `observing` is the upper bound for the locality mode of values whose
types have that kind. For example, if `t : ... mod observing` and some
expression `e` of type `t` has mode `nonportable` (the top mode), it actually
has mode `observing`.  If `e'` of type `t` has mode `portable` (the bottom
mode), that `e` is completely unaffected by the mode-crossing.  Other comonadic
axes work similarly.

Monadic axes are different, though: a kind with `mod exclusive` means that
`exclusive` is a lower bound for mode requirements on terms whose types have
that kind.  Suppose `t : ... mod exclusive`. If a context requires `e` of type
`t` to have mode `unique` (the bottom mode), then actually having mode
`exclusive` is sufficient. A requirement of `aliased` (the top mode) is
completely unaffected.

The key question: if `t : ... mod exclusive` and we have 
`type ('a : ... mod aliased) t2`, is `t t2` valid? No! Even though
`exclusive < aliased`. That's because `mod aliased` puts a *harder* requirement
on its type (it must be agnostic between all of `unique`, `exclusive`, and
`aliased`) than `mod exclusive` does (which says the type is agnostic between
`unique` and `exclusive` only). This means that the subkind relation works
backwards on monadic axes: `... mod aliased ≤ ... mod exclusive`. For this
reason, in the presentation above, the monadic axis elements are listed in 
reverse order: this document does not care about submoding or mode crossing
directly, and writing the axes in reverse order gives us the right behavior
on subkinding.

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
modal_bound_item_Ξ ::= mode m_Ξ | with σ
modal_bound_Ξ ::= [[ modal_bound_item_Ξ | ⊔ ]]
κ ::= layout; ⟪modal_bound_Ξ⟫
```

To convert from `field_types` to the types in a `modal_bound`, we
use

    types_for(Ξ, [[ σᵢ @@ modalitiesᵢ ]]ₙ) =
      { σᵢ | i ∈ [1, j] ∧ sup(extract(Ξ, modalitiesᵢ)) = ⊤ }

This extracts all types `σᵢ` such that its modality along `Ξ`
is the identity. Put another way, this gets all the types from
`field_types`, omitting those with a modality along `Ξ`.

We write `Ξ(κ)` to denote the modal_bound in `κ` corresponding to axis `Ξ`,
and `lay(κ)` to denote the layout in `κ`. Similarly for `lay(χ)` and `Ξ(χ)`.

Typing rules:

```
Γ ⊢ jkind ↠ χ  (* translate a user-written jkind to an internal κ *)
=============

k = χ ∈ Γ
--------- K_ABBREV
Γ ⊢ k ↠ χ

---------------------------------------- K_LAYOUT
Γ ⊢ const_layout ↠ const_layout; ⟪⊤_Ξ⟫

Γ ⊢ jkind ↠ χ
------------------------------------------------- K_MOD
Γ ⊢ jkind mod modes ↠ lay(χ); ⟪Ξ(χ) ⊓ ⨅ extract(Ξ, modes)⟫


rec {

Γ ⊢ jkind_scheme ↠ κ  (* translate a user-written jkind scheme to an internal κ *)
====================

Γ ⊢ τ : κ
------------------------------------------ KS_OF
Γ ⊢ kind_of_ τ ↠ layout_of τ; ⟪⊥_Ξ with τ⟫

Γ ⊢ jkind ↠ χ
∀ σᵢ ∈ field_types, Γ ⊢ σᵢ : κᵢ
---------------------------------------------------------------------- KS_WITH
Γ ⊢ jkind with field_types ↠ lay(χ); ⟪Ξ(χ) with types_for(Ξ, field_types)⟫
```

In `K_OF`, we produce a kind in terms of `τ`, not just `κ`. This allows us
to make use of refinements to `τ` that we learn later, perhaps through
functor application or GADT refinement.

```
Γ ⊢ σ : κ {q}   (* check a type σ to have kind κ, with quality q *)
=============   (* the {q} can be omitted in a premise if we don't care about it *)

t : π [[ 'aᵢ : κᵢ ]]ₙ. κ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
------------------ T_ABSTRACT
Γ ⊢ [[ τᵢ ]]ₙ t : κ[ [[ τᵢ/'aᵢ ]] ] {not_best}

t = λ [[ 'aᵢ : κᵢ ]]ₙ. τ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
Γ ⊢ τ[ [[ τᵢ/'aᵢ ]] ] : κ {q}
------------------- T_ABBREV
Γ ⊢ [[ τᵢ ]]ₙ t : κ {q}

t := λ [[ 'aᵢ : κᵢ ]]ₙ. τ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
Γ ⊢ τ[ [[ τᵢ/'aᵢ ]] ] : κ {q}
------------------- T_SUBST
Γ ⊢ [[ τᵢ ]]ₙ t : κ {q}

t := λ [[ 'aᵢ : κᵢ ]]. δ : κ ∈ Γ
∀ i, Γ ⊢ τᵢ : κᵢ
------------------- T_NOMINATIVE
Γ ⊢ [[ τᵢ ]]ₙ t : κ[ [[ τᵢ/'aᵢ ]] ] {best}
  (* this can be [best] because we use the (best) kind inferred by the
  compiler, not any kind written by the user, for nominative type declarations.
  (Alternatively, we could require the kind of a nominative
  type in a sig to *equal* the kind of a nominative type in a struct, which
  would also allow [best] here.) *)

∀ i, Γ ⊢ τᵢ : κᵢ
∃ j ∈ [0, n],
  (∀ i ∈ [1, j], lay(κᵢ) = value) ∧ (∀ i ∈ (j, n], externality(κᵢ) ≤ external64)
  (* this premise implements the ordering restriction on mixed blocks *)
-------------------------- T_TUPLE
Γ ⊢ [[ τᵢ | * ]]ₙ++ :
  value; local; many with [[ τᵢ ]]; aliased; uncontended with [[ τᵢ ]];
  portable with [[ τᵢ ]]; internal {best}

∀ i, Γ ⊢ τᵢ : κᵢ
-------------------------------- T_UNBOXED_TUPLE
Γ ⊢ #( [[ τᵢ | * ]]ₙ++ ) : [[ layout_of τᵢ | * ]]; ⟪⊥_Ξ with [[ τᵢ ]]⟫ {best}

------------------------ T_UNBOXED_UNIT
Γ ⊢ #( ) : void; ⟪⊥_Ξ⟫ {best}

Γ ⊢ σ₁ : κ₁
Γ ⊢ τ₂ : κ₂
----------------------------------------------------------------------------- T_ARROW
Γ ⊢ σ₁ -> τ₂ : value; local; once; unique; uncontended; nonportable; internal {best}

'a : χ ∈ Γ
---------- T_VAR
Γ ⊢ 'a : χ {not_best}

Γ ⊢ τ : κ₁ {q}
Γ ⊢ jkind ↠ χ₂
Γ ⊢ κ₁ ≤ χ₂
------------------------- T_CONSTRAINT
Γ ⊢ τ as (_ : jkind) : κ₁ {q}

Γ, 'a : value; ⟪⊤_Ξ⟫ ⊢ σ : κ {q} (* or the kind of ['a] is κ₀ if we decide to infer it *)
'a # κ
-------------- T_POLY_DEFAULT
Γ ⊢ 'a. σ : κ {q}

Γ ⊢ jkind ↠ χ₀
Γ, 'a : χ₀ ⊢ σ : κ {q}
'a # κ
----------------------- T_POLY
Γ ⊢ ('a : jkind). σ : κ {q}

TODO
---------------------------------------- T_POLY_VARIANT
Γ ⊢ polymorphic_variant : value; ⟪⊤_Ξ⟫ {not_best}

TODO
------------------------------- T_CLASS
Γ ⊢ class_type : value; ⟪⊤_Ξ⟫ {not_best}

Γ ⊢ σ : κ₁
Γ ⊢ κ₁ ≤ κ₂
----------- T_SUB
Γ ⊢ σ : κ₂ {not_best}


Γ ⊢ σ₁ = σ₂
===========

(* left abstract; checks equality between `σ₁` and `σ₂` *)


Γ ⊢ layout₁ ≤ layout₂
=====================

const_layout₁ ≤ const_layout₂
--------------------------------- LSUB_CONST_CONST
Γ ⊢ const_layout₁ ≤ const_layout₂

Γ ⊢ σ₁ : κ₁
lay(κ₁) ≤ const_layout₂      (* this is a conservative check *)
-------------------------------- LSUB_SIGMA_CONST
Γ ⊢ layout_of σ₁ ≤ const_layout₂

Γ ⊢ σ₂ : κ₂ {best}           (* this is where we care about [best] *)
const_layout₁ ≤ lay(κ₂)
-------------------------------- LSUB_CONST_SIGMA
Γ ⊢ const_layout₁ ≤ layout_of σ₂

Γ ⊢ σ₁ = σ₂
------------------------------- LSUB_SIGMA_SIGMA
Γ ⊢ layout_of σ₁ ≤ layout_of σ₂

Γ ⊢ σ₂ : κ₂
lay(κ₂) is a sort  (* this relies on the discrete nature of the sort lattice *)
                   (* if the sort lattice becomes non-discrete, use [best] *)
Γ ⊢ layout_of σ₁ ≤ lay(κ₂)
------------------------------- LSUB_SIGMA_SORT
Γ ⊢ layout_of σ₁ ≤ layout_of σ₂


Γ ⊢ modal_bound_item₁_Ξ ≤ modal_bound₂_Ξ
========================================

Γ ⊢ σ : κ {best}   (* another critical use of `best` *)
Γ ⊢ modal_bound_item₁_Ξ ≤ modal_bound₂_Ξ ⊔ Ξ(κ)
----------------------------------------------- MB_EXPAND_R
Γ ⊢ modal_bound_item₁_Ξ ≤ modal_bound₂_Ξ ⊔ with σ
  (* the [with σ] is nondeterministically chosen; order does not matter *)

m₁_Ξ ≤ ⨆ [[ m₂ᵢ_Ξ ]]
------------------------------------------------ MB_MODE
Γ ⊢ mode m₁_Ξ ≤ modal_bound₂_Ξ ⊔ [[ mode m₂ᵢ_Ξ ]]
  (* The list of [m₂ᵢ_Ξ] is nondeterministically chosen, but grabbing
     them all is a good choice. *)

Γ ⊢ σ₁ = σ₂
-------------------------------------- MB_WITH
Γ ⊢ with σ₁ ≤ modal_bound₂_Ξ ⊔ with σ₂
  (* [with σ₂] nondeterministically chosen *)

Γ ⊢ σ₁ : κ
∀ modal_bound_item_Ξ ∈ Ξ(κ):
  Γ ⊢ modal_bound_item_Ξ ≤ modal_bound₂_Ξ
---------------------------- MB_EXPAND_L
Γ ⊢ with σ₁ ≤ modal_bound₂_Ξ


Γ ⊢ κ₁ ≤ κ₂
===========

Γ ⊢ layout₁ ≤ layout₂
∀ Ξ:
  ∀ modal_bound_item₁_Ξ ∈ modal_bound₁_Ξ:
    Γ ⊢ modal_bound_item₁_Ξ ≤ modal_bound₂_Ξ
--------------------------------------------------------- SUB
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


Γ; tconstr_jkind ⊢tk type_kind ↠ δ  (* process [type_kind] with known jkind [κ] *)
==================================

∀ j:
  field_typesⱼ = extract_types(constructor_argsⱼ)
  ∀ σⱼₖ ∈ field_typesⱼ:
    Γ ⊢ σⱼₖ : κⱼₖ
  mⱼ_contention = if mutable ∈ constructor_argsⱼ then contended else uncontended
  mⱼ_locality, mⱼ_externality = if (∀ k, κⱼₖ = void) ∨ [@@unboxed] then global, external else local, internal
  mⱼ_uniqueness = if constructor_argsⱼ = record_kind ∧ ¬ [@@unboxed] then shared else unique
  mⱼ_linearity = many
  mⱼ_portability = portable
  ∀ i (where τᵢ ∈ τsⱼ):
    Γ ⊢ τᵢ : κᵢ
    Γ ⊢ κᵢ ≤ χᵢ
  κ₀' = κ₀{τsⱼ/[['aᵢ]]}
  ∀ Ξ:
    mode m_Ξ ≤ Ξ(κ₀')
    ∀ σ ∈ types_for(Ξ, field_typesⱼ), with σ ≤ Ξ(κ₀')
  value ≤ lay(κ₀')
if [@@unboxed]:
  n = 1
  constructor_args₁ has exactly one field
δ represents the type_kind
---------------------------------------------------------------------- TK_GADT
Γ; π [[ 'aᵢ : χᵢ ]]. κ₀ ⊢tk {{ private }} [[ Kⱼ : constructor_argsⱼ -> τsⱼ t ]]ₙ {{ [@@unboxed] }} ↠ δ

(* No other rules necessary: non-variants can go by the normal ⊢tk relation *)

Γ ⊢ δ₁ reexports δ₂
===================

(* left abstract *)


Γ ⊢?any type_params ↠ [[ 'aᵢ : χᵢ ]]  (* the [?any] is essentially an optional param *)
====================================

--------- P_EMPTY
Γ ⊢?any ∅ ↠ ∅

Γ, 'a : χ₀ ⊢?any type_params ↠ [[ 'bᵢ : χᵢ ]]
lay(χ₀) ≠ any
-------------------------------------------- P_INFER
Γ ⊢ 'a, type_params ↠ 'a : χ₀, [[ 'bᵢ : χᵢ ]]

Γ, 'a : χ₀ ⊢?any type_params ↠ [[ 'bᵢ : χᵢ ]]
-------------------------------------------- P_INFER_ANY
Γ ⊢any 'a, type_params ↠ 'a : χ₀, [[ 'bᵢ : χᵢ ]]

Γ ⊢ jkind ↠ χ₀
Γ, 'a : χ₀ ⊢?any type_params ↠ [[ 'bᵢ : χᵢ ]]
---------------------------------------- P_KINDED
Γ ⊢?any 'a : jkind, type_params ↠ 'a : χ₀, [[ 'bᵢ : χᵢ ]]
```

The `⊢any` variant allows us to infer `any` as the layout of a type
variable. It is used for e.g. `type 'a t := ...` in signatures.

```
Γ ⊢ type_decl ↠ Γ'  (* translate a [type_decl] into a fresh env Γ' *)
==================

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
---------------------------------------------------------- D_ABSTRACT
Γ ⊢ type type_params t ↠ t : π [[ 'aᵢ : χᵢ ]]. value; ⟪⊤_Ξ⟫

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]], t : tconstr_jkind ⊢ jkind_scheme ↠ κ₀  (* [t] may appear in its kind *)
tconstr_jkind = π [[ 'aᵢ : χᵢ ]]. κ₀
------------------------------------------------- D_KINDED
Γ ⊢ type type_params t : jkind_scheme ↠ t : tconstr_jkind

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : χᵢ ]]. τ
--------------------------------------------- D_ABBREV
Γ ⊢ type type_params t = τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t : π [[ 'aᵢ : χᵢ ]]. κ₀   (* private: make [t] abstract in the result *)
---------------------------------------- D_PRIVATE
Γ ⊢ type type_params t = private τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]], t : π [[ 'aᵢ : χᵢ ]]. κ₀ ⊢tk type_kind ↠ δ : κ₀
------------------------------------------------------------------ D_NOMINATIVE
Γ ⊢ type type_params t = type_kind ↠ t := λ [[ 'aᵢ : χᵢ ]]. δ : κ₀

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : χᵢ ]]. τ
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ jkind_scheme ↠ κ₀'
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ κ₀ ≤ κ₀'
---------------------------------------------- D_KINDED_ABBREV
Γ ⊢ type type_params t : jkind_scheme = τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ τ : κ₀
Γ' = t : π [[ 'aᵢ : χᵢ ]]. κ₀'        (* κ₀', not κ₀, for private abbrevs *)
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ jkind_scheme ↠ κ₀'
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ κ₀ ≤ κ₀'
----------------------------------------------------- D_KINDED_PRIVATE
Γ ⊢ type type_params t : jkind_scheme = private τ ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
τ = [[ 'aᵢ ]] t'
Γ, [[ 'aᵢ : χᵢ ]], t : π [[ 'aᵢ : χᵢ ]]. κ₀ ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : χᵢ ]]. τ
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢tk type_kind ↠ δ : κ₀'
t' := λ [[ 'aᵢ : χᵢ ]]. δ' : κ₀'' ∈ Γ
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ δ reexports δ'
----------------------------------------- D_REEXPORT
Γ ⊢ type type_params t = τ = type_kind ↠ Γ'

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]], t : π [[ 'aᵢ : χᵢ ]]. κ₀' ⊢tk type_kind ↠ δ : κ₀'
Γ' = t := λ [[ 'aᵢ : χᵢ ]]. δ : κ₀'  (* inferred jkind, not user-written one *)
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ jkind_scheme ↠ κ₀
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ κ₀' ≤ κ₀
----------------------------------------------------- D_KINDED_NOMINATIVE
Γ ⊢ type type_params t : jkind_scheme = type_kind ↠ Γ'

(* This rule is to support using type refinements in GADT constructors to infer
   a lower overall jkind; we can implement it later than the rest of the design *)
Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]] ⊢ jkind_scheme ↠ κ₀    (* [t] is *not* in scope here *)
Γ, [[ 'aᵢ : χᵢ ]], t : π [[ 'aᵢ : χᵢ ]]. κ₀; π [[ 'aᵢ : χᵢ ]]. κ₀ ⊢tk type_kind ↠ δ : κ₀
----------------------------------------------- D_GADT
Γ ⊢ type type_params t : jkind_scheme = type_kind ↠ Γ, t := λ [[ 'aᵢ : χᵢ ]]. δ : κ₀

Γ ⊢ type_params ↠ [[ 'aᵢ : χᵢ ]]
τ = [[ 'aᵢ ]] t'
Γ, [[ 'aᵢ : χᵢ ]], t : π [[ 'aᵢ : χᵢ ]]. κ₀ ⊢ τ : κ₀
Γ' = t = λ [[ 'aᵢ : χᵢ ]]. τ
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢tk type_kind ↠ δ : κ₀'
t' := λ [[ 'aᵢ : χᵢ ]]. δ' ∈ Γ
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ δ reexports δ'
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ jkind_scheme ↠ κ₀''
Γ, [[ 'aᵢ : χᵢ ]], Γ' ⊢ κ₀ ≤ κ₀''
--------------------------------------------------------- D_KINDED_REEXPORT
Γ ⊢ type type_params t : jkind_scheme = τ = type_kind ↠ Γ'


Γ ⊢find t ∈ Γ₂ ↠ tconstr_jkind  (* find [t] in Γ₂ *)
======================================
  (* pre-condition: Γ₂ ⊆ Γ *)

t : tconstr_jkind ∈ Γ₂
--------------------------- FIND_ABSTRACT
Γ ⊢find t ∈ Γ₂ ↠ tconstr_jkind

t = λ [[ 'aᵢ : χᵢ ]]. τ ∈ Γ₂
------------------------------------- FIND_ABBREV
Γ ⊢find t ∈ Γ₂ ↠ π [[ 'aᵢ : χᵢ ]]. layout_of τ; ⟪⊥_Ξ with τ⟫

t := λ [[ 'aᵢ : χᵢ ]]. δ : κ₀ ∈ Γ₂
--------------------------------------- FIND_NOMINATIVE
Γ ⊢find t ∈ Γ₂ ↠ π [[ 'aᵢ : χᵢ ]]. κ₀


Γ ⊢include type_binding ∈~ Γ₂
  (* check whether type_binding is more general than a definition in Γ₂,
     with ambient env Γ *)
====================================

tconstr_jkind₁ = π [[ 'aᵢ : χ₁ᵢ ]]ₙ. κ₁₀
Γ ⊢find t ∈ Γ₂ ↠ π [[ 'aᵢ : χ₂ᵢ ]]ₙ. κ₂₀
∀ 1 ≤ j ≤ n:
  Γ, [[ 'aᵢ : χ₁ᵢ ]]ⱼ₋₁ ⊢ χ₁ⱼ ≤ χ₂ⱼ
    (* check with more restrictive χ₁ in env, not χ₂; this is also contravariant *)
Γ, [[ 'aᵢ : χ₁ᵢ ]]ₙ ⊢ κ₂₀ ≤ κ₁₀
------------------------------------------ INCL_ABSTRACT
Γ ⊢include (t : tconstr_jkind₁) ∈~ Γ₂

(t = λ [[ 'aᵢ : χ₂ᵢ ]]ₙ. τ₂) ∈ Γ₂
∀ 1 ≤ j ≤ n:
  Γ, [[ 'aᵢ : χ₁ᵢ ]]ⱼ₋₁ ⊢ χ₁ⱼ ≤ χ₂ⱼ
Γ, [[ 'aᵢ : χ₁ᵢ ]]ₙ ⊢ τ₁ = τ₂
----------------------------------------- INCL_ABBREV
Γ ⊢include (t = λ [[ 'aᵢ : χ₁ᵢ ]]ₙ. τ₁) ∈~ Γ₂

(* nothing to check for here *)
------------------------------------------ INCL_SUBST
Γ ⊢include (t := λ [[ 'aᵢ ]]. τ) ∈~ Γ₂

TODO: More type_binding rules


Γ ⊢includemod Γ₂ ≤ Γ₁  (* check whether Γ₁, covers Γ₂ *)
============================

-------------------- IM_EMPTY
Γ ⊢includemod Γ₂ ≤ ∅

Γ ⊢include type_binding ∈~ Γ₂
Γ ⊢includemod Γ₂ ≤ Γ₁
----------------------------------- IM_BINDING
Γ ⊢includemod Γ₂ ≤ type_binding, Γ₁


Γ ⊢sig sig_item ↠ Γ'
==================

Γ ⊢ type_decl ↠ Γ'
---------------------- SIG_TYPE_DECL
Γ ⊢sig type_decl ↠ Γ'

Γ ⊢ jkind ↠ χ
---------------------------------------- SIG_KIND_ABBREV
Γ ⊢sig kind_abbrev_ k = jkind ↠ k = χ

Γ ⊢any type_params ↠ [[ 'aᵢ : χᵢ ]]
Γ, [[ 'aᵢ : χᵢ ]] ⊢ τ : κ₀
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

Γ ⊢ jkind ↠ χ
------------------------------------------- STRUCT_KIND_ABBREV
Γ ⊢struct kind_abbrev_ k = jkind ↠ k = χ

Γ ⊢mty module_type ↠ Γ₁
Γ ⊢mod module_expr ↠ Γ₂
Γ, Γ₂ ⊢includemod Γ₂ ≤ Γ₁
---------------------------------------------- STRUCT_MOD
Γ ⊢struct module M : module_type = module_expr ↠ M = Γ₁


Γ ⊢mod module_expr ↠ Γ'
=======================

∀ 1 ≤ i ≤ n, Γ₀,…,Γᵢ₋₁ ⊢struct struct_itemᵢ ⊢ Γᵢ
------------------------------------------------ MOD_STRUCT
Γ₀ ⊢mod struct [[ struct_itemᵢ ]]ₙ end ↠ Γ₁,…,Γₙ

}

```

# Properties

Lemma (Borrowing is sound): If `Γ ⊢ σ : κ {best}` and `Γ ⊢ κ ≤ any mod global`,
then `Γ ⊢ κ ≤ any mod unique`.

Proof: TODO.

# Examples

```
(* To remind ourselves:
lm ::= global | local
om ::= many | separate | once
pm ::= portable | observing | nonportable
em ::= external | external64 | internal

(* monadic axes = descriptive axes *)
um ::= unique | exclusive | aliased
cm ::= uncontended | shared | contended
*)

type 'a t = { x : 'a }
↠
type 'a t : value mod many portable uncontended with 'a

goal:
  We want int list to mode-cross portability
  We want (int -> int) list *not* to mode-cross portability
  And similarly Set.Make(X).t (whose kind must have `with X.t` in it)

============

kind_abbrev_ data = value mod many portable  (* no functions *)
kind_abbrev_ immutable_data = data mod uncontended
type 'a t = { x : 'a; y : ('a -> 'a) @@ portable many; z : ('a -> 'a) @@ many }

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

==============================

(* example showing how grounding out a right-hand argument
   to ≤ is unsound *)

module type S = sig
  type t1 : any
  type t2 : any
end

module F (X : S) = struct
  type t3 : layout_of_ X.t1 = X.t2
end

module M = F (struct
  type t1 = int
  type t2 = float#
end)

(* ^^ strange, but not actually problematic *)

module F2 (X : S) = struct
  type t4 : layout_of_ X.t1 = private X.t2
end

module M2 = F2 (struct
  type t1 = int
  type t2 = float#
end)

(* ^^ actually problematic. *)

(* I think we just want to reject [F]. *)

```
