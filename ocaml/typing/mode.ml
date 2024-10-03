(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Solver
open Solver_intf
open Mode_intf

type nonrec allowed = allowed

type nonrec disallowed = disallowed

type nonrec equate_step = equate_step

module type Heyting = sig
  (** Extend the [Lattice] interface with the implication operation
      of a Heyting algebra *)

  include Lattice

  (** [imply c] is the right adjoint of [meet c]; That is, for any [a] and [b],
      [meet c a <= b] iff [a <= imply c b] *)
  val imply : t -> t -> t

end

module type Co_Heyting = sig
  (** Extend the [Lattice] interface with the subtraction operation
      of a Co-Heyting algebra *)

  include Lattice

  (** [subtract _ c] is the left adjoint of [join c]. That is, for any [a] and [b],
      [subtract a c <= b] iff [a <= join c b] *)
  val subtract : t -> t -> t
end

module type Bi_Heyting = sig

  include Heyting

  include Co_Heyting with type t := t

end

(* Even though our lattices are all heyting algebras, that knowledge is
   internal to this module. Externally they are seen as normal lattices. *)
module Lattices = struct
  module Opposite (L : Co_Heyting) : Heyting with type t = L.t = struct
    type t = L.t

    let min = L.max

    let max = L.min

    let legacy = L.legacy

    let le a b = L.le b a

    let equal a b = L.equal a b

    let join = L.meet

    let meet = L.join

    let print = L.print

    let imply a b = L.subtract b a

  end
  [@@inline]

  (* A lattice is total order, if for any [a] [b], [a <= b] or [b <= a].
     A total lattice has a bi-heyting structure given as follows. *)
  module Total (L : Lattice) : Bi_Heyting with type t := L.t = struct
    include L

    (* Prove the [subtract] below is the left adjoint of [join].
       - If [subtract a c <= b], by the definition of [subtract] below,
         that could mean one of two things:
         - Took the branch [a <= c], and [min <= b]. In this case, we have [a <= c <= join c b].
         - Took the other branch, and [a <= b]. In this case, we have [a <= b <= join c b].

       - In the other direction: Given [a <= join c b], compare [c] and [b]:
         - if [c <= b], then [a <= join c b = b], and:
           - either [a <= c], then [subtract a c = min <= b]
           - or the other branch, then [subtract a c = a <= b]
         - if [b <= c], then [a <= join c b = c], then [subtract a c = min <= b]
    *)
    let subtract a c = if le a c then min else a

    (* The proof for [imply] is dual and omitted. *)
    let imply c b = if le c b then max else b
  end
  [@@inline]

  type locality =
    | Global
    | Local

  type regionality =
    | Global
    | Regional
    | Local

  type 'a areality =
    | Locality : locality areality
    | Regionality : regionality areality

  let eq_areality
      : type a b. a areality -> b areality -> (a, b) Misc.eq option =
    fun a b ->
      match a, b with
      | Locality, Locality -> Some Refl
      | Regionality, Regionality -> Some Refl
      | (Locality | Regionality), _ -> None

  module type Areality = sig
    include Heyting

    val areality : t areality
  end

  module Locality = struct
    type t = locality =
      | Global
      | Local

    include Total (struct
      type nonrec t = t

      let min = Global

      let max = Local

      let legacy = Global

      let le a b =
        match a, b with Global, _ | _, Local -> true | Local, Global -> false

      let equal a b =
        match a, b with
        | Global, Global -> true
        | Local, Local -> true
        | (Global | Local), _ -> false

      let join a b =
        match a, b with
        | Local, _ | _, Local -> Local
        | Global, Global -> Global

      let meet a b =
        match a, b with
        | Global, _ | _, Global -> Global
        | Local, Local -> Local

      let print ppf = function
        | Global -> Format.fprintf ppf "global"
        | Local -> Format.fprintf ppf "local"
    end)

    let areality = Locality
  end

  module Regionality = struct
    type t = regionality =
      | Global
      | Regional
      | Local

    include Total (struct
      type nonrec t = t

      let min = Global

      let max = Local

      let legacy = Global

      let join a b =
        match a, b with
        | Local, _ | _, Local -> Local
        | Regional, _ | _, Regional -> Regional
        | Global, Global -> Global

      let meet a b =
        match a, b with
        | Global, _ | _, Global -> Global
        | Regional, _ | _, Regional -> Regional
        | Local, Local -> Local

      let le a b =
        match a, b with
        | Global, _ | _, Local -> true
        | _, Global | Local, _ -> false
        | Regional, Regional -> true

      let equal a b =
        match a, b with
        | Global, Global -> true
        | Regional, Regional -> true
        | Local, Local -> true
        | (Global | Regional | Local), _ -> false

      let print ppf = function
        | Global -> Format.fprintf ppf "global"
        | Regional -> Format.fprintf ppf "regional"
        | Local -> Format.fprintf ppf "local"
    end)

    let areality = Regionality
  end

  module Uniqueness = struct
    type t =
      | Unique
      | Aliased

    include Total (struct
      type nonrec t = t

      let min = Unique

      let max = Aliased

      let legacy = Aliased

      let le a b =
        match a, b with
        | Unique, _ | _, Aliased -> true
        | Aliased, Unique -> false

      let equal a b =
        match a, b with
        | Unique, Unique -> true
        | Aliased, Aliased -> true
        | (Unique | Aliased), _ -> false

      let join a b =
        match a, b with
        | Aliased, _ | _, Aliased -> Aliased
        | Unique, Unique -> Unique

      let meet a b =
        match a, b with
        | Unique, _ | _, Unique -> Unique
        | Aliased, Aliased -> Aliased

      let print ppf = function
        | Aliased -> Format.fprintf ppf "aliased"
        | Unique -> Format.fprintf ppf "unique"
    end)
  end

  module Uniqueness_op = Opposite (Uniqueness)

  module Linearity = struct
    type t =
      | Many
      | Once

    include Total (struct
      type nonrec t = t

      let min = Many

      let max = Once

      let legacy = Many

      let le a b =
        match a, b with Many, _ | _, Once -> true | Once, Many -> false

      let equal a b =
        match a, b with
        | Many, Many -> true
        | Once, Once -> true
        | (Many| Once), _ -> false

      let join a b =
        match a, b with Once, _ | _, Once -> Once | Many, Many -> Many

      let meet a b =
        match a, b with Many, _ | _, Many -> Many | Once, Once -> Once

      let print ppf = function
        | Once -> Format.fprintf ppf "once"
        | Many -> Format.fprintf ppf "many"
    end)
  end

  module Portability = struct
    type t =
      | Portable
      | Nonportable

    include Total (struct
      type nonrec t = t

      let min = Portable

      let max = Nonportable

      let legacy = Nonportable

      let le a b =
        match a, b with
        | Portable, _ | _, Nonportable -> true
        | Nonportable, Portable -> false

      let equal a b =
        match a, b with
        | Portable, Portable -> true
        | Nonportable, Nonportable -> true
        | (Nonportable | Portable), _ -> false

      let join a b =
        match a, b with
        | Nonportable, _ | _, Nonportable -> Nonportable
        | Portable, Portable -> Portable

      let meet a b =
        match a, b with
        | Portable, _ | _, Portable -> Portable
        | Nonportable, Nonportable -> Nonportable

      let print ppf = function
        | Portable -> Format.fprintf ppf "portable"
        | Nonportable -> Format.fprintf ppf "nonportable"
    end)
  end

  module Contention = struct
    type t =
      | Contended
      | Shared
      | Uncontended

    include Total (struct
      type nonrec t = t

      let min = Uncontended

      let max = Contended

      let legacy = Uncontended

      let le a b =
        match a, b with
        | Uncontended, _ | _, Contended -> true
        | _, Uncontended | Contended, _ -> false
        | Shared, Shared -> true

      let equal a b =
        match a, b with
        | Contended, Contended -> true
        | Shared, Shared -> true
        | Uncontended, Uncontended -> true
        | (Contended | Shared | Uncontended), _ -> false

      let join a b =
        match a, b with
        | Contended, _ | _, Contended -> Contended
        | Shared, _ | _, Shared -> Shared
        | Uncontended, Uncontended -> Uncontended

      let meet a b =
        match a, b with
        | Uncontended, _ | _, Uncontended -> Uncontended
        | Shared, _ | _, Shared -> Shared
        | Contended, Contended -> Contended

      let print ppf = function
        | Contended -> Format.fprintf ppf "contended"
        | Shared -> Format.fprintf ppf "shared"
        | Uncontended -> Format.fprintf ppf "uncontended"
    end)
  end

  module Contention_op = Opposite (Contention)

  type monadic =
    { uniqueness : Uniqueness.t;
      contention : Contention.t }

  module Monadic = struct
    type t = monadic

    let min =
      let uniqueness = Uniqueness.min in
      let contention = Contention.min in
      { uniqueness; contention }

    let max =
      let uniqueness = Uniqueness.max in
      let contention = Contention.max in
      { uniqueness; contention }

    let legacy =
      let uniqueness = Uniqueness.legacy in
      let contention = Contention.legacy in
      { uniqueness; contention }

    let le m1 m2 =
      let { uniqueness = uniqueness1;
            contention = contention1 } = m1 in
      let { uniqueness = uniqueness2;
            contention = contention2 } = m2 in
      Uniqueness.le uniqueness1 uniqueness2
      && Contention.le contention1 contention2

    let equal m1 m2 =
      let { uniqueness = uniqueness1;
            contention = contention1 } = m1 in
      let { uniqueness = uniqueness2;
            contention = contention2 } = m2 in
      Uniqueness.equal uniqueness1 uniqueness2
      && Contention.equal contention1 contention2

    let join m1 m2 =
      let uniqueness = Uniqueness.join m1.uniqueness m2.uniqueness in
      let contention = Contention.join m1.contention m2.contention in
      { uniqueness; contention }

    let meet m1 m2 =
      let uniqueness = Uniqueness.meet m1.uniqueness m2.uniqueness in
      let contention = Contention.meet m1.contention m2.contention in
      { uniqueness; contention }

    let subtract m1 m2 =
      let uniqueness = Uniqueness.subtract m1.uniqueness m2.uniqueness in
      let contention = Contention.subtract m1.contention m2.contention in
      { uniqueness; contention }

    let print ppf m =
      Format.fprintf ppf "%a,%a"
        Uniqueness.print m.uniqueness
        Contention.print m.contention
  end

  type 'areality comonadic_with =
  { areality : 'areality;
    linearity : Linearity.t;
    portability :  Portability.t; }

  module Comonadic_with (Areality : Areality) = struct
    type t = Areality.t comonadic_with

    let min =
      let areality = Areality.min in
      let linearity = Linearity.min in
      let portability = Portability.min in
      { areality; linearity; portability }

    let max =
      let areality = Areality.max in
      let linearity = Linearity.max in
      let portability = Portability.max in
      { areality; linearity; portability }

    let legacy =
      let areality = Areality.legacy in
      let linearity = Linearity.legacy in
      let portability = Portability.legacy in
      { areality; linearity; portability }

    let le m1 m2 =
      let { areality = areality1;
            linearity = linearity1;
            portability = portability1 } = m1 in
      let { areality = areality2;
            linearity = linearity2;
            portability = portability2 } = m2 in
      Areality.le areality1 areality2
      && Linearity.le linearity1 linearity2
      && Portability.le portability1 portability2

    let equal m1 m2 =
      let { areality = areality1;
            linearity = linearity1;
            portability = portability1 } = m1 in
      let { areality = areality2;
            linearity = linearity2;
            portability = portability2 } = m2 in
      Areality.equal areality1 areality2
      && Linearity.equal linearity1 linearity2
      && Portability.equal portability1 portability2

    let join m1 m2 =
      let areality = Areality.join m1.areality m2.areality in
      let linearity = Linearity.join m1.linearity m2.linearity in
      let portability = Portability.join m1.portability m2.portability in
      { areality; linearity; portability }

    let meet m1 m2 =
      let areality = Areality.meet m1.areality m2.areality in
      let linearity = Linearity.meet m1.linearity m2.linearity in
      let portability = Portability.meet m1.portability m2.portability in
      { areality; linearity; portability }

    let imply m1 m2 =
      let areality = Areality.imply m1.areality m2.areality in
      let linearity = Linearity.imply m1.linearity m2.linearity in
      let portability = Portability.imply m1.portability m2.portability in
      { areality; linearity; portability }

    let print ppf m =
      Format.fprintf ppf "%a,%a,%a"
        Areality.print m.areality
        Linearity.print m.linearity
        Portability.print m.portability

  end
  [@@inline]

  module Monadic_op = Opposite (Monadic)
  module Comonadic_with_locality = Comonadic_with (Locality)
  module Comonadic_with_regionality = Comonadic_with (Regionality)

  (* Axes are categorized into monadic and comonadic fragments, and in general:
     - Morphisms between the same fragment are always monotone.
     - Morphisms between different fragments are always antitone.
     To play well with the solver, here we flip the whole monadic fragment, so all
     morphisms are monotone. [Solver_polarized] will flip it back. *)
  type 'a obj =
    | Locality : Locality.t obj
    | Regionality : Regionality.t obj
    | Uniqueness_op : Uniqueness_op.t obj
    | Linearity : Linearity.t obj
    | Portability : Portability.t obj
    | Contention_op : Contention_op.t obj
    | Monadic_op : Monadic_op.t obj
    | Comonadic_with_regionality : Comonadic_with_regionality.t obj
    | Comonadic_with_locality : Comonadic_with_locality.t obj

  let print_obj : type a. _ -> a obj -> unit =
   fun ppf -> function
    | Locality -> Format.fprintf ppf "Locality"
    | Regionality -> Format.fprintf ppf "Regionality"
    | Uniqueness_op -> Format.fprintf ppf "Uniqueness_op"
    | Linearity -> Format.fprintf ppf "Linearity"
    | Portability -> Format.fprintf ppf "Portability"
    | Contention_op -> Format.fprintf ppf "Contention_op"
    | Monadic_op -> Format.fprintf ppf "Monadic_op"
    | Comonadic_with_locality -> Format.fprintf ppf "Comonadic_with_locality"
    | Comonadic_with_regionality ->
      Format.fprintf ppf "Comonadic_with_regionality"

  let min : type a. a obj -> a = function
    | Locality -> Locality.min
    | Regionality -> Regionality.min
    | Uniqueness_op -> Uniqueness_op.min
    | Contention_op -> Contention_op.min
    | Linearity -> Linearity.min
    | Portability -> Portability.min
    | Monadic_op -> Monadic_op.min
    | Comonadic_with_locality -> Comonadic_with_locality.min
    | Comonadic_with_regionality -> Comonadic_with_regionality.min

  let max : type a. a obj -> a = function
    | Locality -> Locality.max
    | Regionality -> Regionality.max
    | Uniqueness_op -> Uniqueness_op.max
    | Contention_op -> Contention_op.max
    | Linearity -> Linearity.max
    | Portability -> Portability.max
    | Monadic_op -> Monadic_op.max
    | Comonadic_with_locality -> Comonadic_with_locality.max
    | Comonadic_with_regionality -> Comonadic_with_regionality.max

  let le : type a. a obj -> a -> a -> bool =
   fun obj a b ->
    match obj with
    | Locality -> Locality.le a b
    | Regionality -> Regionality.le a b
    | Uniqueness_op -> Uniqueness_op.le a b
    | Contention_op -> Contention_op.le a b
    | Linearity -> Linearity.le a b
    | Portability -> Portability.le a b
    | Monadic_op -> Monadic_op.le a b
    | Comonadic_with_locality -> Comonadic_with_locality.le a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.le a b

  let equal : type a. a obj -> a -> a -> bool =
   fun obj a b ->
    match obj with
    | Locality -> Locality.equal a b
    | Regionality -> Regionality.equal a b
    | Uniqueness_op -> Uniqueness_op.equal a b
    | Contention_op -> Contention_op.equal a b
    | Linearity -> Linearity.equal a b
    | Portability -> Portability.equal a b
    | Monadic_op -> Monadic_op.equal a b
    | Comonadic_with_locality -> Comonadic_with_locality.equal a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.equal a b

  let join : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.join a b
    | Regionality -> Regionality.join a b
    | Uniqueness_op -> Uniqueness_op.join a b
    | Contention_op -> Contention_op.join a b
    | Linearity -> Linearity.join a b
    | Portability -> Portability.join a b
    | Monadic_op -> Monadic_op.join a b
    | Comonadic_with_locality -> Comonadic_with_locality.join a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.join a b

  let meet : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.meet a b
    | Regionality -> Regionality.meet a b
    | Uniqueness_op -> Uniqueness_op.meet a b
    | Contention_op -> Contention_op.meet a b
    | Linearity -> Linearity.meet a b
    | Portability -> Portability.meet a b
    | Monadic_op -> Monadic_op.meet a b
    | Comonadic_with_locality -> Comonadic_with_locality.meet a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.meet a b

  let imply : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.imply a b
    | Regionality -> Regionality.imply a b
    | Uniqueness_op -> Uniqueness_op.imply a b
    | Contention_op -> Contention_op.imply a b
    | Linearity -> Linearity.imply a b
    | Portability -> Portability.imply a b
    | Comonadic_with_locality -> Comonadic_with_locality.imply a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.imply a b
    | Monadic_op -> Monadic_op.imply a b

  (* not hotpath, Ok to curry *)
  let print : type a. a obj -> _ -> a -> unit = function
    | Locality -> Locality.print
    | Regionality -> Regionality.print
    | Uniqueness_op -> Uniqueness_op.print
    | Contention_op -> Contention_op.print
    | Linearity -> Linearity.print
    | Portability -> Portability.print
    | Monadic_op -> Monadic_op.print
    | Comonadic_with_locality -> Comonadic_with_locality.print
    | Comonadic_with_regionality -> Comonadic_with_regionality.print

  let eq_obj : type a b. a obj -> b obj -> (a, b) Misc.eq option =
    fun a b ->
    match a, b with
    | Locality, Locality -> Some Misc.Refl
    | Regionality, Regionality -> Some Misc.Refl
    | Uniqueness_op, Uniqueness_op -> Some Misc.Refl
    | Contention_op, Contention_op -> Some Misc.Refl
    | Linearity, Linearity -> Some Misc.Refl
    | Portability, Portability -> Some Misc.Refl
    | Monadic_op, Monadic_op -> Some Misc.Refl
    | Comonadic_with_locality, Comonadic_with_locality -> Some Misc.Refl
    | Comonadic_with_regionality, Comonadic_with_regionality -> Some Misc.Refl
    | ( ( Locality | Regionality | Uniqueness_op | Contention_op
          | Linearity | Portability | Monadic_op
          | Comonadic_with_locality | Comonadic_with_regionality ),
        _ ) ->
        None
end

module Lattices_mono = struct
  include Lattices

  module Axis = struct
    type ('t, 'r) t =
      | Areality : ('a comonadic_with, 'a) t
      | Linearity : ('areality comonadic_with, Linearity.t) t
      | Portability : ('areality comonadic_with, Portability.t) t
      | Uniqueness : (Monadic_op.t, Uniqueness_op.t) t
      | Contention : (Monadic_op.t, Contention_op.t) t

    let print : type p r. _ -> (p, r) t -> unit =
     fun ppf -> function
      | Areality -> Format.fprintf ppf "areality"
      | Linearity -> Format.fprintf ppf "linearity"
      | Portability -> Format.fprintf ppf "portability"
      | Uniqueness -> Format.fprintf ppf "uniqueness"
      | Contention -> Format.fprintf ppf "contention"

    let eq : type p r0 r1. (p, r0) t -> (p, r1) t -> (r0, r1) Misc.eq option =
     fun ax0 ax1 ->
      match ax0, ax1 with
      | Areality, Areality -> Some Refl
      | Linearity, Linearity -> Some Refl
      | Portability, Portability -> Some Refl
      | Uniqueness, Uniqueness -> Some Refl
      | Contention, Contention -> Some Refl
      | (Areality | Linearity | Uniqueness | Portability | Contention), _ ->
        None

    let proj : type p r. (p, r) t -> p -> r =
     fun ax t ->
      match ax with
      | Areality -> t.areality
      | Linearity -> t.linearity
      | Portability -> t.portability
      | Uniqueness -> t.uniqueness
      | Contention -> t.contention

    let update : type p r. (p, r) t -> r -> p -> p =
     fun ax r t ->
      match ax with
      | Areality -> { t with areality = r }
      | Linearity -> { t with linearity = r }
      | Portability -> { t with portability = r }
      | Uniqueness -> { t with uniqueness = r }
      | Contention -> { t with contention = r }
  end

  type ('a, 'b, 'd) core_morph =
    | Monadic_to_comonadic_min
        : (Monadic_op.t, 'a comonadic_with, 'l * disallowed) core_morph
        (** Dualize the monadic fragment to the comonadic fragment.
            The areality is set to min. *)
    | Comonadic_to_monadic :
        'a areality
        -> ('a comonadic_with, Monadic_op.t, 'l * 'r) core_morph
        (** Dualize the comonadic fragment to the monadic fragment.
            The areality axis is ignored.  *)
    | Monadic_to_comonadic_max
        : (Monadic_op.t, 'a comonadic_with, disallowed * 'r) core_morph
        (** Dualize the monadic fragment to the comonadic fragment.
            The areality is set to max. *)
    (* Following is a chain of adjunctions (this can be extended one
       further, but we never need the missing operation) *)
    | Local_to_regional :
        (Comonadic_with_locality.t,
         Comonadic_with_regionality.t, 'l * disallowed) core_morph
        (** Maps local to regional, global to global *)
    | Regional_to_local :
        (Comonadic_with_regionality.t,
         Comonadic_with_locality.t, 'l * 'r) core_morph
        (** Maps regional to local, identity otherwise *)
    | Locality_as_regionality :
        (Comonadic_with_locality.t,
         Comonadic_with_regionality.t, 'l * 'r) core_morph
        (** Inject locality into regionality  *)
    | Regional_to_global :
        (Comonadic_with_regionality.t,
         Comonadic_with_locality.t, disallowed * 'r) core_morph
        (** Maps regional to global, identity otherwise *)
    (* Versions of the above morphisms operating on regionality. *)
    | Local_to_regional_regionality :
        (Comonadic_with_regionality.t,
         Comonadic_with_regionality.t, 'l * disallowed) core_morph
        (** Maps regional to local, identity otherwise. *)
    | Regional_to_local_regionality :
        (Comonadic_with_regionality.t,
         Comonadic_with_regionality.t, 'l * 'r) core_morph
        (** Maps regional to local, identity otherwise. *)
    | Regional_to_global_regionality :
        (Comonadic_with_regionality.t,
         Comonadic_with_regionality.t, disallowed * 'r) core_morph
        (** Maps regional to global, identity otherwise. *)

  type ('a, 'b, 'd) morph =
    | Id : ('a, 'a, 'd) morph  (** identity morphism *)
    | Core : ('a, 'b, 'd) core_morph -> ('a, 'b, 'd) morph
    | Meet_const : 'a -> ('a, 'a, 'l * disallowed) morph
        (** Meet the input with the parameter *)
    | Imply : 'a -> ('a, 'a, disallowed * 'd) morph
        (** The right adjoint of [Meet_const] *)
    | Core_and_meet_const :
        'b * ('a, 'b, 'l * disallowed) core_morph -> ('a, 'b, 'l * disallowed) morph
        (** Composition of [Core] and [Meet_const]. We only need to include one
            order of composition because currently all our core left morphisms
            preserve binary meets. *)
    | Imply_and_core :
        ('a, 'b, disallowed * 'r) core_morph * 'a -> ('a, 'b, disallowed * 'r) morph
        (** Composition of [Core] and [Imply]. We only need to include one
            order of composition because currently all our core right morphisms
            commute with implication. *)
    | And_proj :
        's obj * ('s, 'p) Axis.t * ('t, 's, 'l * 'r) morph
        -> ('t, 'p, 'l * 'r) morph
        (** Composition of a morphism and projecting out an axis. *)
    | Max_with_and :
        ('t, 's, disallowed * 'r) morph * ('t, 'p) Axis.t
        -> ('p, 's, disallowed * 'r) morph
        (** Composition of combining an axis with the maxima along other
            axes and a morphism. *)
    | Min_with_and :
        ('t, 's, 'l * disallowed) morph * ('t, 'p) Axis.t
        -> ('p, 's, 'l * disallowed) morph
        (** Composition of a morphism on an axis and combining that axis
            with the minima along other axes *)
    | Compose : ('b, 'c, neither) morph * ('a, 'b, neither) morph
                -> ('a, 'c, neither) morph
        (** Compoistion of two morphisms. We don't allow compositions to
            appear on either side to ensure that there are a finite
            number of morphisms we can encounter in practice. *)

  include Magic_allow_disallow (struct
    type ('a, 'b, 'd) sided = ('a, 'b, 'd) morph constraint 'd = 'l * 'r

    let allow_left_core :
          type a b l r. (a, b, allowed * r) core_morph -> (a, b, l * r) core_morph =
      function
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Local_to_regional -> Local_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Local_to_regional_regionality -> Local_to_regional_regionality
      | Regional_to_local_regionality -> Regional_to_local_regionality

    let rec allow_left :
        type a b l r. (a, b, allowed * r) morph -> (a, b, l * r) morph =
      function
      | Id -> Id
      | Core m -> Core (allow_left_core m)
      | Meet_const c -> Meet_const c
      | Core_and_meet_const(c, m) -> Core_and_meet_const(c, allow_left_core m)
      | And_proj(obj, ax, m) -> And_proj(obj, ax, allow_left m)
      | Min_with_and(m, ax) -> Min_with_and(allow_left m, ax)

    let allow_right_core :
        type a b l r. (a, b, l * allowed) core_morph -> (a, b, l * r) core_morph =
      function
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Regional_to_local -> Regional_to_local
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_global -> Regional_to_global
      | Regional_to_local_regionality -> Regional_to_local_regionality
      | Regional_to_global_regionality -> Regional_to_global_regionality

    let rec allow_right :
        type a b l r. (a, b, l * allowed) morph -> (a, b, l * r) morph =
      function
      | Id -> Id
      | Core m -> Core (allow_right_core m)
      | Imply c -> Imply c
      | Imply_and_core(m, c) -> Imply_and_core(allow_right_core m, c)
      | And_proj(obj, ax, m) -> And_proj(obj, ax, allow_right m)
      | Max_with_and(m, ax) -> Max_with_and(allow_right m, ax)

    let disallow_left_core :
        type a b l r.
             (a, b, l * r) core_morph -> (a, b, disallowed * r) core_morph =
      function
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Local_to_regional -> Local_to_regional
      | Regional_to_local -> Regional_to_local
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_global -> Regional_to_global
      | Local_to_regional_regionality -> Local_to_regional_regionality
      | Regional_to_local_regionality -> Regional_to_local_regionality
      | Regional_to_global_regionality -> Regional_to_global_regionality

    let rec disallow_left :
        type a b l r. (a, b, l * r) morph -> (a, b, disallowed * r) morph =
      function
      | Id -> Id
      | Core m -> Core (disallow_left_core m)
      | Meet_const c -> Meet_const c
      | Imply c -> Imply c
      | Core_and_meet_const(c, m) -> Core_and_meet_const(c, disallow_left_core m)
      | Imply_and_core(m, c) -> Imply_and_core(disallow_left_core m, c)
      | And_proj(obj, ax, m) -> And_proj(obj, ax, disallow_left m)
      | Max_with_and(m, ax) -> Max_with_and(disallow_left m, ax)
      | Min_with_and(m, ax) -> Min_with_and(disallow_left m, ax)
      | Compose (f, g) ->
        let f = disallow_left f in
        let g = disallow_left g in
        Compose (f, g)

    let disallow_right_core :
        type a b l r.
             (a, b, l * r) core_morph -> (a, b, l * disallowed) core_morph =
      function
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Local_to_regional -> Local_to_regional
      | Regional_to_local -> Regional_to_local
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_global -> Regional_to_global
      | Local_to_regional_regionality -> Local_to_regional_regionality
      | Regional_to_local_regionality -> Regional_to_local_regionality
      | Regional_to_global_regionality -> Regional_to_global_regionality

    let rec disallow_right :
        type a b l r. (a, b, l * r) morph -> (a, b, l * disallowed) morph =
      function
      | Id -> Id
      | Core m -> Core (disallow_right_core m)
      | Meet_const c -> Meet_const c
      | Imply c -> Imply c
      | Core_and_meet_const(c, m) -> Core_and_meet_const(c, disallow_right_core m)
      | Imply_and_core(m, c) -> Imply_and_core(disallow_right_core m, c)
      | And_proj(obj, ax, m) -> And_proj(obj, ax, disallow_right m)
      | Max_with_and(m, ax) -> Max_with_and(disallow_right m, ax)
      | Min_with_and(m, ax) -> Min_with_and(disallow_right m, ax)
      | Compose (f, g) ->
        let f = disallow_right f in
        let g = disallow_right g in
        Compose (f, g)

  end)

  let proj_obj : type t r. (t, r) Axis.t -> t obj -> r obj =
   fun ax obj ->
    match ax, obj with
    | Areality, Comonadic_with_locality -> Locality
    | Areality, Comonadic_with_regionality -> Regionality
    | Linearity, Comonadic_with_locality -> Linearity
    | Linearity, Comonadic_with_regionality -> Linearity
    | Portability, Comonadic_with_locality -> Portability
    | Portability, Comonadic_with_regionality -> Portability
    | Uniqueness, Monadic_op -> Uniqueness_op
    | Contention, Monadic_op -> Contention_op

  let areality_comonadic_obj : type a. a areality -> a comonadic_with obj =
    function
    | Locality -> Comonadic_with_locality
    | Regionality -> Comonadic_with_regionality

  let comonadic_obj_areality : type a. a comonadic_with obj -> a areality =
    function
    | Comonadic_with_locality -> Locality
    | Comonadic_with_regionality -> Regionality

  let src_core : type a b d. (a, b, d) core_morph -> a obj = function
    | Monadic_to_comonadic_min -> Monadic_op
    | Comonadic_to_monadic ar -> areality_comonadic_obj ar
    | Monadic_to_comonadic_max -> Monadic_op
    | Local_to_regional -> Comonadic_with_locality
    | Regional_to_local -> Comonadic_with_regionality
    | Locality_as_regionality -> Comonadic_with_locality
    | Regional_to_global -> Comonadic_with_regionality
    | Local_to_regional_regionality -> Comonadic_with_regionality
    | Regional_to_local_regionality -> Comonadic_with_regionality
    | Regional_to_global_regionality -> Comonadic_with_regionality

  let rec src : type a b d. b obj -> (a, b, d) morph -> a obj =
   fun dst f ->
    match f with
    | Id -> dst
    | Core m -> src_core m
    | Meet_const _ -> dst
    | Imply _ -> dst
    | Core_and_meet_const(_, m) -> src_core m
    | Imply_and_core(m, _) -> src_core m
    | And_proj(mid, _, m) -> src mid m
    | Max_with_and(m, ax) -> proj_obj ax (src dst m)
    | Min_with_and(m, ax) -> proj_obj ax (src dst m)
    | Compose (f, g) ->
      let mid = src dst f in
      src mid g

  let eq_core_morph :
        type a0 l0 r0 a1 b l1 r1.
        b obj ->
        (a0, b, l0 * r0) core_morph ->
        (a1, b, l1 * r1) core_morph ->
        (a0, a1) Misc.eq option =
     fun _dst f0 f1 ->
      match f0, f1 with
      | Monadic_to_comonadic_min, Monadic_to_comonadic_min -> Some Refl
      | Comonadic_to_monadic a0, Comonadic_to_monadic a1 -> begin
          match eq_areality a0 a1 with
          | None -> None
          | Some Refl -> Some Refl
        end
      | Monadic_to_comonadic_max, Monadic_to_comonadic_max -> Some Refl
      | Local_to_regional, Local_to_regional -> Some Refl
      | Regional_to_local, Regional_to_local -> Some Refl
      | Locality_as_regionality, Locality_as_regionality -> Some Refl
      | Regional_to_global, Regional_to_global -> Some Refl
      | Local_to_regional_regionality, Local_to_regional_regionality -> Some Refl
      | Regional_to_local_regionality, Regional_to_local_regionality -> Some Refl
      | Regional_to_global_regionality, Regional_to_global_regionality -> Some Refl
      | ( ( Monadic_to_comonadic_min | Comonadic_to_monadic _
          | Monadic_to_comonadic_max
          | Local_to_regional | Regional_to_local | Locality_as_regionality
          | Regional_to_global | Local_to_regional_regionality
          | Regional_to_local_regionality | Regional_to_global_regionality ),
          _ ) ->
        None


  let rec eq_morph :
        type a0 l0 r0 a1 b l1 r1.
        b obj ->
        (a0, b, l0 * r0) morph ->
        (a1, b, l1 * r1) morph ->
        (a0, a1) Misc.eq option =
     fun dst f0 f1 ->
      match f0, f1 with
      | Id, Id -> Some Refl
      | Core m0, Core m1 -> eq_core_morph dst m0 m1
      | Meet_const c0, Meet_const c1 ->
        if equal dst c0 c1 then Some Refl else None
      | Imply c0, Imply c1 ->
          if equal dst c0 c1 then Some Refl else None
      | Core_and_meet_const(c0, m0), Core_and_meet_const(c1, m1) ->
          if equal dst c0 c1 then begin
            match eq_core_morph dst m0 m1 with
            | Some Refl -> Some Refl
            | None -> None
          end else None
      | Imply_and_core(m0, c0), Imply_and_core(m1, c1) -> begin
          match eq_core_morph dst m0 m1 with
          | None -> None
          | Some Refl ->
              if equal (src_core m0) c0 c1 then Some Refl
              else None
        end
      | And_proj(src1, ax1, m1), And_proj(src2, ax2, m2) -> begin
          match eq_obj src1 src2 with
          | None -> None
          | Some Refl ->
              match Axis.eq ax1 ax2 with
              | None -> None
              | Some Refl ->
                  match eq_morph src1 m1 m2 with
                  | None -> None
                  | Some Refl -> Some Refl
        end
      | Max_with_and(m1, ax1), Max_with_and(m2, ax2) -> begin
          match eq_morph dst m1 m2 with
          | None -> None
          | Some Refl ->
              match Axis.eq ax1 ax2 with
              | None -> None
              | Some Refl -> Some Refl
        end
      | Min_with_and(m1, ax1), Min_with_and(m2, ax2) -> begin
          match eq_morph dst m1 m2 with
          | None -> None
          | Some Refl ->
              match Axis.eq ax1 ax2 with
              | None -> None
              | Some Refl -> Some Refl
        end
      | Compose(m1, n1), Compose(m2, n2) -> begin
          match eq_morph dst m1 m2 with
          | None -> None
          | Some Refl ->
              match eq_morph (src dst m1) n1 n2 with
              | None -> None
              | Some Refl -> Some Refl
        end
      | ( ( Id | Core _ | Meet_const _ | Imply _
            | Core_and_meet_const _ | Imply_and_core _
            | And_proj _ | Max_with_and _ | Min_with_and _
            | Compose _ ),
          _ ) ->
        None

  let print_core_morph :
      type a b d. b obj -> Format.formatter -> (a, b, d) core_morph -> unit =
    fun _dst ppf -> function
    | Monadic_to_comonadic_min -> Format.fprintf ppf "monadic_to_comonadic_min"
    | Comonadic_to_monadic _ -> Format.fprintf ppf "comonadic_to_monadic"
    | Monadic_to_comonadic_max -> Format.fprintf ppf "monadic_to_comonadic_max"
    | Local_to_regional -> Format.fprintf ppf "local_to_regional"
    | Regional_to_local -> Format.fprintf ppf "regional_to_local"
    | Locality_as_regionality -> Format.fprintf ppf "locality_as_regionality"
    | Regional_to_global -> Format.fprintf ppf "regional_to_global"
    | Local_to_regional_regionality ->
        Format.fprintf ppf "local_to_regional_regionality"
    | Regional_to_local_regionality ->
        Format.fprintf ppf "regional_to_local_regionality"
    | Regional_to_global_regionality ->
        Format.fprintf ppf "regional_to_global_regionality"

  let rec print_morph :
      type a b d. b obj -> Format.formatter -> (a, b, d) morph -> unit =
   fun dst ppf -> function
    | Id -> Format.fprintf ppf "id"
    | Core m -> print_core_morph dst ppf m
    | Meet_const c -> Format.fprintf ppf "meetc(%a)" (print dst) c
    | Imply c -> Format.fprintf ppf "implyc(%a)" (print dst) c
    | Core_and_meet_const(c, m) ->
        Format.fprintf ppf "meetc(%a) . %a"
          (print dst) c (print_core_morph dst) m
    | Imply_and_core(m, c) ->
        Format.fprintf ppf "%a . implyc(%a)"
          (print_core_morph dst) m (print (src_core m)) c
    | And_proj(mid, ax, Id) ->
        Format.fprintf ppf "proj_%a" print_obj (proj_obj ax mid)
    | And_proj(mid, ax, m) ->
        Format.fprintf ppf "proj_%a . %a"
          print_obj (proj_obj ax mid) (print_morph mid) m
    | Max_with_and(Id, ax) ->
        Format.fprintf ppf "max_with_%a" print_obj (proj_obj ax dst)
    | Max_with_and(m, ax) ->
        Format.fprintf ppf "%a . max_with_%a"
          (print_morph dst) m print_obj (proj_obj ax (src dst m))
    | Min_with_and(Id, ax) ->
        Format.fprintf ppf "min_with_%a" print_obj (proj_obj ax dst)
    | Min_with_and(m, ax) ->
        Format.fprintf ppf "%a . min_with_%a"
          (print_morph dst) m print_obj (proj_obj ax (src dst m))
    | Compose (f0, f1) ->
      let mid = src dst f0 in
      Format.fprintf ppf "%a . %a" (print_morph dst) f0 (print_morph mid) f1

  let id = Id

  let linear_to_unique = function
    | Linearity.Many -> Uniqueness.Aliased
    | Linearity.Once -> Uniqueness.Unique

  let unique_to_linear = function
    | Uniqueness.Unique -> Linearity.Once
    | Uniqueness.Aliased -> Linearity.Many

  let portable_to_contended = function
    | Portability.Portable -> Contention.Contended
    | Portability.Nonportable -> Contention.Uncontended

  let contended_to_portable = function
    | Contention.Contended -> Portability.Portable
    | Contention.Shared -> Portability.Nonportable
    | Contention.Uncontended -> Portability.Nonportable

  let local_to_regional_projected = function
    | Locality.Global -> Regionality.Global
    | Locality.Local -> Regionality.Regional

  let local_to_regional r =
    let areality = local_to_regional_projected r.areality in
    { r with areality }

  let regional_to_local_projected = function
    | Regionality.Global -> Locality.Global
    | Regionality.Regional -> Locality.Local
    | Regionality.Local -> Locality.Local

  let regional_to_local r =
    let areality = regional_to_local_projected r.areality in
    { r with areality }

  let locality_as_regionality_projected = function
    | Locality.Global -> Regionality.Global
    | Locality.Local -> Regionality.Local

  let locality_as_regionality r =
    let areality = locality_as_regionality_projected r.areality in
    { r with areality }

  let regional_to_global_projected = function
    | Regionality.Global -> Locality.Global
    | Regionality.Regional -> Locality.Global
    | Regionality.Local -> Locality.Local

  let regional_to_global r =
    let areality = regional_to_global_projected r.areality in
    { r with areality }

  let local_to_regional_regionality_projected = function
    | Regionality.Global -> Regionality.Global
    | Regionality.Regional -> Regionality.Regional
    | Regionality.Local -> Regionality.Regional

  let local_to_regional_regionality r =
    let areality = local_to_regional_regionality_projected r.areality in
    { r with areality }

  let regional_to_local_regionality_projected = function
    | Regionality.Global -> Regionality.Global
    | Regionality.Regional -> Regionality.Local
    | Regionality.Local -> Regionality.Local

  let regional_to_local_regionality r =
    let areality = regional_to_local_regionality_projected r.areality in
    { r with areality }

  let regional_to_global_regionality_projected = function
    | Regionality.Global -> Regionality.Global
    | Regionality.Regional -> Regionality.Global
    | Regionality.Local -> Regionality.Local

  let regional_to_global_regionality r =
    let areality = regional_to_global_regionality_projected r.areality in
    { r with areality }

  let min_with dst ax a = Axis.update ax a (min dst)

  let max_with dst ax a = Axis.update ax a (max dst)

  let monadic_to_comonadic_min :
      type a. a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj m ->
     let areality : a =
       match obj with
       | Comonadic_with_locality -> Locality.min
       | Comonadic_with_regionality -> Regionality.min
     in
     let linearity = unique_to_linear m.uniqueness in
     let portability = contended_to_portable m.contention in
     { areality; linearity; portability }

  let comonadic_to_monadic :
      type a. a areality -> a comonadic_with -> Monadic_op.t =
   fun _ m ->
     let uniqueness = linear_to_unique m.linearity in
     let contention = portable_to_contended m.portability in
     { uniqueness; contention }

  let monadic_to_comonadic_max :
      type a. a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj m ->
     let areality : a =
       match obj with
       | Comonadic_with_locality -> Locality.max
       | Comonadic_with_regionality -> Regionality.max
     in
     let linearity = unique_to_linear m.uniqueness in
     let portability = contended_to_portable m.contention in
     { areality; linearity; portability }

  let apply_core : type a b d. b obj -> (a, b, d) core_morph -> a -> b =
   fun dst f a ->
     match f with
     | Monadic_to_comonadic_min -> monadic_to_comonadic_min dst a
     | Comonadic_to_monadic src -> comonadic_to_monadic src a
     | Monadic_to_comonadic_max -> monadic_to_comonadic_max dst a
     | Local_to_regional -> local_to_regional a
     | Regional_to_local -> regional_to_local a
     | Locality_as_regionality -> locality_as_regionality a
     | Regional_to_global -> regional_to_global a
     | Local_to_regional_regionality -> local_to_regional_regionality a
     | Regional_to_local_regionality -> regional_to_local_regionality a
     | Regional_to_global_regionality -> regional_to_global_regionality a

  let rec apply : type a b d. b obj -> (a, b, d) morph -> a -> b =
   fun dst f a ->
    match f with
    | Id -> a
    | Core m -> apply_core dst m a
    | Meet_const c -> meet dst c a
    | Imply c -> imply dst c a
    | Core_and_meet_const(c, m) ->
        meet dst c (apply_core dst m a)
    | Imply_and_core(m, c) ->
        apply_core dst m (imply (src_core m) c a)
    | And_proj(mid, ax, m) -> Axis.proj ax (apply mid m a)
    | Max_with_and(m, ax) -> apply dst m (max_with (src dst m) ax a)
    | Min_with_and(m, ax) -> apply dst m (min_with (src dst m) ax a)
    | Compose (f, g) ->
      let mid = src dst f in
      let g' = apply mid g in
      let f' = apply dst f in
      f' (g' a)

  let right_adjoint_core :
     type a b r.
       b obj
       -> (a, b, allowed * r) core_morph
       -> (b, a, disallowed * allowed) core_morph =
   fun dst -> function
     | Monadic_to_comonadic_min ->
         Comonadic_to_monadic (comonadic_obj_areality dst)
     | Comonadic_to_monadic _ -> Monadic_to_comonadic_max
     | Local_to_regional -> Regional_to_local
     | Regional_to_local -> Locality_as_regionality
     | Locality_as_regionality -> Regional_to_global
     | Local_to_regional_regionality -> Regional_to_local_regionality
     | Regional_to_local_regionality -> Regional_to_global_regionality

  let left_adjoint_core :
      type a b l.
        b obj
        -> (a, b, l * allowed) core_morph
        -> (b, a, allowed * disallowed) core_morph =
   fun dst -> function
     | Comonadic_to_monadic _ ->
         Monadic_to_comonadic_min
     | Monadic_to_comonadic_max ->
         Comonadic_to_monadic (comonadic_obj_areality dst)
     | Regional_to_local -> Local_to_regional
     | Locality_as_regionality -> Regional_to_local
     | Regional_to_global -> Locality_as_regionality
     | Regional_to_local_regionality -> Local_to_regional_regionality
     | Regional_to_global_regionality -> Regional_to_local_regionality

  let rec right_adjoint :
      type a b r.
      b obj -> (a, b, allowed * r) morph -> (b, a, disallowed * allowed) morph =
   fun dst f ->
    match f with
    | Id -> Id
    | Core m -> Core (right_adjoint_core dst m)
    | Meet_const c -> Imply c
    | Core_and_meet_const(c, m) -> Imply_and_core(right_adjoint_core dst m, c)
    | And_proj(mid, ax, m) -> Max_with_and(right_adjoint mid m, ax)
    | Min_with_and(m, ax) -> And_proj(src dst m, ax, right_adjoint dst m)

  let rec left_adjoint :
      type a b l.
      b obj -> (a, b, l * allowed) morph -> (b, a, allowed * disallowed) morph =
   fun dst f ->
    match f with
    | Id -> Id
    | Core m -> Core (left_adjoint_core dst m)
    | Imply c -> Meet_const c
    | Imply_and_core(m, c) -> Core_and_meet_const(c, left_adjoint_core dst m)
    | And_proj(mid, ax, m) -> Min_with_and(left_adjoint mid m, ax)
    | Max_with_and(m, ax) -> And_proj(src dst m, ax, left_adjoint dst m)

  type ('a, 'b, 'd) maybe_allowed_right_core =
    | Allowed_right :
        ('a, 'b, 'l * allowed) core_morph
        -> ('a, 'b, 'l * 'r) maybe_allowed_right_core
    | Not_allowed_right : ('a, 'b, 'l * disallowed) maybe_allowed_right_core

  let maybe_allowed_right_core :
      type a b d.
        (a, b, d) core_morph -> (a, b, d) maybe_allowed_right_core =
    function
    | Comonadic_to_monadic _ as m -> Allowed_right m
    | Monadic_to_comonadic_max as m -> Allowed_right m
    | Regional_to_local as m -> Allowed_right m
    | Locality_as_regionality as m -> Allowed_right m
    | Regional_to_global as m -> Allowed_right m
    | Regional_to_local_regionality as m -> Allowed_right m
    | Regional_to_global_regionality as m -> Allowed_right m
    | Monadic_to_comonadic_min -> Not_allowed_right
    | Local_to_regional -> Not_allowed_right
    | Local_to_regional_regionality -> Not_allowed_right

  let core_morphs_are_wide_right :
       type a b d t k.
      (a, b, d) core_morph -> t obj -> (t, a) Axis.t -> k =
    fun m obj ax ->
      match m, obj, ax with
      | _, _, _ -> .

  let core_morphs_are_wide_left :
       type a b d t k.
      t obj -> (t, b) Axis.t -> (a, b, d) core_morph -> k =
    fun obj ax m ->
      match m, obj, ax with
      | _, _, _ -> .

  let rec compose_meet_const_left :
      type a b l.
      b obj -> b
      -> (a, b, l * disallowed) morph
      -> (a, b, l * disallowed) morph =
    fun dst c m ->
    match m with
    | Id -> Meet_const c
    | Core m -> Core_and_meet_const(c, m)
    | Meet_const c' -> Meet_const (meet dst c c')
    | Core_and_meet_const(c', m) ->
        Core_and_meet_const(meet dst c c', m)
    | And_proj(mid, ax, m) ->
        let c = max_with mid ax c in
        And_proj(mid, ax, compose_meet_const_left mid c m)
    | Min_with_and(m, ax) ->
        Min_with_and(compose_meet_const_left dst c m, ax)
    | Imply _ as m -> Compose(Meet_const c, m)
    | Imply_and_core _ as m -> Compose(Meet_const c, m)
    | Max_with_and _ as m -> Compose(Meet_const c, m)
    | Compose _ as m -> Compose(Meet_const c, m)

  let commute_meet_const_from_right_core :
        type a b l.
             b obj -> (a, b, l * disallowed) core_morph -> a -> b =
    fun dst m c ->
      apply_core dst m c

  let rec compose_meet_const_right :
      type a b l.
      b obj -> (a, b, l * disallowed) morph -> a
      -> (a, b, l * disallowed) morph =
    fun dst m c ->
    match m with
    | Id -> Meet_const c
    | Core m ->
        let c = commute_meet_const_from_right_core dst m c in
        Core_and_meet_const(c, m)
    | Meet_const c' -> Meet_const (meet dst c' c)
    | Core_and_meet_const(c', m) ->
        let c = commute_meet_const_from_right_core dst m c in
        Core_and_meet_const(meet dst c' c, m)
    | And_proj(mid, ax, m) ->
        And_proj(mid, ax, compose_meet_const_right mid m c)
    | Min_with_and(m, ax) ->
        let c = max_with (src dst m) ax c in
        Min_with_and(compose_meet_const_right dst m c, ax)
    | Imply _ as m -> Compose(m, Meet_const c)
    | Imply_and_core _ as m -> Compose(m, Meet_const c)
    | Max_with_and _ as m -> Compose(m, Meet_const c)
    | Compose _ as m -> Compose(m, Meet_const c)


  let rec compose_imply_right :
      type a b r. b obj
        -> (a, b, disallowed * r) morph
        -> a -> (a, b, disallowed * r) morph =
    fun dst m c ->
    match m with
    | Id -> Imply c
    | Core m -> Imply_and_core(m, c)
    | Imply c' -> Imply (meet dst c c')
    | Imply_and_core(m, c') ->
        Imply_and_core(m, meet (src_core m) c' c)
    | And_proj(mid, ax, m) ->
        And_proj(mid, ax, compose_imply_right mid m c)
    | Max_with_and(m, ax) ->
        let c = max_with (src dst m) ax c in
        Max_with_and(compose_imply_right dst m c, ax)
    | Meet_const _ as m -> Compose(m, Imply c)
    | Core_and_meet_const _ as m -> Compose(m, Imply c)
    | Min_with_and _ as m -> Compose(m, Imply c)
    | Compose _ as m -> Compose(m, Imply c)


  let commute_imply_from_left_core :
        type a b l.
             b obj -> b -> (a, b, l * allowed) core_morph -> a =
    fun dst c m ->
      apply_core (src_core m) (left_adjoint_core dst m) c

  let rec compose_imply_left :
      type a b r. b obj
        -> b -> (a, b, disallowed * r) morph
        -> (a, b, disallowed * r) morph =
    fun dst c m ->
    match m with
    | Id -> Imply c
    | Core m -> begin
        match maybe_allowed_right_core m with
        | Not_allowed_right -> Compose(Imply c, Core m)
        | Allowed_right m' ->
            let c = commute_imply_from_left_core dst c m' in
            Imply_and_core(m, c)
      end
    | Imply c' -> Imply (meet dst c c')
    | Imply_and_core(m, c') -> begin
        match maybe_allowed_right_core m with
        | Not_allowed_right -> Compose(Imply c, Imply_and_core(m, c'))
        | Allowed_right m' ->
            let c = commute_imply_from_left_core dst c m' in
            Imply_and_core(m, meet (src_core m) c c')
      end
    | And_proj(mid, ax, m) ->
        let c = max_with mid ax c in
        And_proj(mid, ax, compose_imply_left mid c m)
    | Max_with_and(m, ax) ->
        Max_with_and(compose_imply_left dst c m, ax)
    | Meet_const _ as m -> Compose(Imply c, m)
    | Core_and_meet_const _ as m -> Compose(Imply c, m)
    | Min_with_and _ as m -> Compose(Imply c, m)
    | Compose _ as m -> Compose(Imply c, m)

  let compose_core :
      type a b c d.
        c obj
        -> (b, c, d) core_morph
        -> (a, b, d) core_morph
        -> (a, c, d) morph =
    fun dst m0 m1 ->
      match m0, m1 with
      (* Compositions of the comonadic/monadic conversions *)
      | Comonadic_to_monadic _, Monadic_to_comonadic_min -> Id
      | Comonadic_to_monadic _, Monadic_to_comonadic_max -> Id
      | Monadic_to_comonadic_min, Comonadic_to_monadic areality -> begin
          match areality, dst with
          | Locality, Comonadic_with_locality ->
              let c =
                max_with Comonadic_with_locality Areality (min Locality)
              in
              Meet_const c
          | Regionality, Comonadic_with_locality ->
              let c =
                max_with Comonadic_with_locality Areality (min Locality) in
              Core_and_meet_const(c, Regional_to_local)
          | Locality, Comonadic_with_regionality ->
              let c =
                max_with Comonadic_with_regionality Areality (min Regionality)
              in
              Core_and_meet_const(c, Locality_as_regionality)
          | Regionality, Comonadic_with_regionality ->
              let c =
                max_with Comonadic_with_regionality Areality (min Regionality)
              in
              Meet_const c
        end
      | Monadic_to_comonadic_max, Comonadic_to_monadic areality -> begin
          match areality, dst with
          | Locality, Comonadic_with_locality ->
              let c =
                max_with Comonadic_with_locality Areality (min Locality)
              in
              Imply c
          | Regionality, Comonadic_with_locality ->
              let c =
                max_with Comonadic_with_regionality Areality (min Regionality)
              in
              Imply_and_core(Regional_to_local, c)
          | Locality, Comonadic_with_regionality ->
              let c =
                max_with Comonadic_with_locality Areality (min Locality)
              in
              Imply_and_core(Locality_as_regionality, c)
          | Regionality, Comonadic_with_regionality ->
              let c =
                max_with Comonadic_with_regionality Areality (min Regionality)
              in
              Imply c
        end
      (* Compositions of comonadic/monadic conversions
         with locality/regionality conversions *)
      | Comonadic_to_monadic Regionality, Local_to_regional ->
          Core (Comonadic_to_monadic Locality)
      | Comonadic_to_monadic Locality, Regional_to_local ->
          Core (Comonadic_to_monadic Regionality)
      | Comonadic_to_monadic Regionality, Locality_as_regionality ->
          Core (Comonadic_to_monadic Locality)
      | Comonadic_to_monadic Locality, Regional_to_global ->
          Core (Comonadic_to_monadic Regionality)
      | Comonadic_to_monadic Regionality, Local_to_regional_regionality ->
          Core (Comonadic_to_monadic Regionality)
      | Comonadic_to_monadic Regionality, Regional_to_local_regionality ->
          Core (Comonadic_to_monadic Regionality)
      | Comonadic_to_monadic Regionality, Regional_to_global_regionality ->
          Core (Comonadic_to_monadic Regionality)
      | Local_to_regional, Monadic_to_comonadic_min ->
          Core Monadic_to_comonadic_min
      | Regional_to_local, Monadic_to_comonadic_min ->
          Core Monadic_to_comonadic_min
      | Locality_as_regionality, Monadic_to_comonadic_min ->
          Core Monadic_to_comonadic_min
      | Regional_to_global, Monadic_to_comonadic_min ->
          Core Monadic_to_comonadic_min
      | Local_to_regional_regionality, Monadic_to_comonadic_min ->
          Core Monadic_to_comonadic_min
      | Regional_to_local_regionality, Monadic_to_comonadic_min ->
          Core Monadic_to_comonadic_min
      | Regional_to_global_regionality, Monadic_to_comonadic_min ->
          Core Monadic_to_comonadic_min
      | Regional_to_local, Monadic_to_comonadic_max ->
          Core Monadic_to_comonadic_max
      | Locality_as_regionality, Monadic_to_comonadic_max ->
          Core Monadic_to_comonadic_max
      | Regional_to_global, Monadic_to_comonadic_max ->
          Core Monadic_to_comonadic_max
      | Regional_to_local_regionality, Monadic_to_comonadic_max ->
          Core Monadic_to_comonadic_max
      | Regional_to_global_regionality, Monadic_to_comonadic_max ->
          Core Monadic_to_comonadic_max
      (* Compositions of the locality/regionality conversions *)
      | Local_to_regional, Regional_to_local ->
          Core Local_to_regional_regionality
      | Regional_to_local, Local_to_regional -> Id
      | Regional_to_local, Locality_as_regionality -> Id
      | Regional_to_local, Local_to_regional_regionality ->
          Core Regional_to_local
      | Regional_to_local, Regional_to_local_regionality ->
          Core Regional_to_local
      | Regional_to_local, Regional_to_global_regionality ->
          Core Regional_to_global
      | Locality_as_regionality, Regional_to_local ->
          Core Regional_to_local_regionality
      | Locality_as_regionality, Regional_to_global ->
          Core Regional_to_global_regionality
      | Regional_to_global, Locality_as_regionality -> Id
      | Regional_to_global, Regional_to_local_regionality ->
          Core Regional_to_local
      | Regional_to_global, Regional_to_global_regionality ->
          Core Regional_to_global
      | Local_to_regional_regionality, Local_to_regional ->
          Core Local_to_regional
      | Local_to_regional_regionality, Locality_as_regionality ->
          Core Local_to_regional
      | Local_to_regional_regionality, Local_to_regional_regionality ->
          Core Local_to_regional_regionality
      | Local_to_regional_regionality, Regional_to_local_regionality ->
          Core Local_to_regional_regionality
      | Regional_to_local_regionality, Local_to_regional ->
          Core Locality_as_regionality
      | Regional_to_local_regionality, Locality_as_regionality ->
          Core Locality_as_regionality
      | Regional_to_local_regionality, Local_to_regional_regionality ->
          Core Regional_to_local_regionality
      | Regional_to_local_regionality, Regional_to_local_regionality ->
          Core Regional_to_local_regionality
      | Regional_to_local_regionality, Regional_to_global_regionality ->
          Core Regional_to_global_regionality
      | Regional_to_global_regionality, Locality_as_regionality ->
          Core Locality_as_regionality
      | Regional_to_global_regionality, Regional_to_local_regionality ->
          Core Regional_to_local_regionality
      | Regional_to_global_regionality, Regional_to_global_regionality ->
          Core Regional_to_global_regionality
      (* Compositions of operations that cannot appear on the same side *)
      | Local_to_regional, Monadic_to_comonadic_max ->
          Compose(Core m0, Core m1)
      | Local_to_regional_regionality, Monadic_to_comonadic_max ->
          Compose(Core m0, Core m1)
      | Local_to_regional, Regional_to_global ->
          Compose(Core m0, Core m1)
      | Regional_to_global, Local_to_regional ->
          Compose(Core m0, Core m1)
      | Regional_to_global, Local_to_regional_regionality ->
          Compose(Core m0, Core m1)
      | Local_to_regional_regionality, Regional_to_global_regionality ->
          Compose(Core m0, Core m1)
      | Regional_to_global_regionality, Local_to_regional ->
          Compose(Core m0, Core m1)
      | Regional_to_global_regionality, Local_to_regional_regionality ->
          Compose(Core m0, Core m1)

  let rec compose_core_left :
      type a b c d.
      c obj -> (b, c, d) core_morph -> (a, b, d) morph -> (a, c, d) morph =
    fun dst m0 m1 ->
      match m1 with
      | Id -> Core m0
      | Core m1 -> compose_core dst m0 m1
      | Meet_const c1 ->
          let c1 = commute_meet_const_from_right_core dst m0 c1 in
          Core_and_meet_const(c1, m0)
      | Imply c1 -> Imply_and_core(m0, c1)
      | Core_and_meet_const(c1, m1) ->
          let c1 = commute_meet_const_from_right_core dst m0 c1 in
          compose_meet_const_left dst c1 (compose_core dst m0 m1)
      | Imply_and_core(m1, c1) ->
          compose_imply_right dst (compose_core dst m0 m1) c1
      | And_proj(obj1, ax1, _) -> core_morphs_are_wide_right m0 obj1 ax1
      | Max_with_and(m1, ax1) -> Max_with_and(compose_core_left dst m0 m1, ax1)
      | Min_with_and(m1, ax1) -> Min_with_and(compose_core_left dst m0 m1, ax1)
      | Compose _ as m1 -> Compose(Core m0, m1)

  let rec compose_core_right :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) core_morph -> (a, c, d) morph =
    fun dst m0 m1 ->
      match m0 with
      | Id -> Core m1
      | Core m0 -> compose_core dst m0 m1
      | Meet_const c0 -> Core_and_meet_const(c0, m1)
      | Imply c0 -> begin
          match maybe_allowed_right_core m1 with
          | Not_allowed_right -> Compose(m0, Core m1)
          | Allowed_right m1' ->
              let c0 = commute_imply_from_left_core dst c0 m1' in
              Imply_and_core(m1, c0)
        end
      | Core_and_meet_const(c0, m0) ->
          compose_meet_const_left dst c0 (compose_core dst m0 m1)
      | Imply_and_core(m0', c0) -> begin
          match maybe_allowed_right_core m1 with
          | Not_allowed_right -> Compose(m0, Core m1)
          | Allowed_right m1' ->
              let c0 = commute_imply_from_left_core (src_core m0') c0 m1' in
              compose_imply_right dst (compose_core dst m0' m1) c0
        end
      | And_proj(obj0, ax0, m0) ->
          And_proj(obj0, ax0, compose_core_right obj0 m0 m1)
      | Max_with_and(m0, ax0) -> core_morphs_are_wide_left (src dst m0) ax0 m1
      | Min_with_and(m0, ax0) -> core_morphs_are_wide_left (src dst m0) ax0 m1
      | Compose _ as m0 -> Compose(m0, Core m1)

  let connect_projections :
        type a b c l r p.
             a obj -> (b, a, l * r) morph
             -> b obj -> (b, p) Axis.t
             -> c obj -> (c, p) Axis.t
             -> (c, a, l * r) morph =
    fun obj0 m0 obj1 ax1 obj2 ax2 ->
      match obj1, ax1, obj2, ax2 with
      | Comonadic_with_locality, Areality,
        Comonadic_with_locality, Areality -> m0
      | Comonadic_with_regionality, Areality,
        Comonadic_with_regionality, Areality -> m0
      | Comonadic_with_locality, Linearity,
        Comonadic_with_locality, Linearity -> m0
      | Comonadic_with_regionality, Linearity,
        Comonadic_with_regionality, Linearity -> m0
      | Comonadic_with_locality, Linearity,
        Comonadic_with_regionality, Linearity ->
          compose_core_right obj0 m0 Regional_to_local
      | Comonadic_with_regionality, Linearity,
        Comonadic_with_locality, Linearity ->
          compose_core_right obj0 m0 Locality_as_regionality
      | Comonadic_with_locality, Portability,
        Comonadic_with_locality, Portability -> m0
      | Comonadic_with_regionality, Portability,
        Comonadic_with_regionality, Portability -> m0
      | Comonadic_with_locality, Portability,
        Comonadic_with_regionality, Portability ->
          compose_core_right obj0 m0 Regional_to_local
      | Comonadic_with_regionality, Portability,
        Comonadic_with_locality, Portability ->
          compose_core_right obj0 m0 Locality_as_regionality
      | Monadic_op, Uniqueness,
        Monadic_op, Uniqueness -> m0
      | Monadic_op, Contention,
        Monadic_op, Contention -> m0
      | _, _, _, _ -> .

  let rec compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph =
   fun dst m0 m1 ->
    match m0, m1 with
    | Id, m -> m
    | m, Id -> m
    | Meet_const c0, _ -> compose_meet_const_left dst c0 m1
    | _, Meet_const c1 -> compose_meet_const_right dst m0 c1
    | Imply c0, _ -> compose_imply_left dst c0 m1
    | _, Imply c1 -> compose_imply_right dst m0 c1
    | Core m0, _ -> compose_core_left dst m0 m1
    | _, Core m1 -> compose_core_right dst m0 m1
    | Core_and_meet_const(c0, m0), _ ->
        compose_meet_const_left dst c0 (compose_core_left dst m0 m1)
    | _, Core_and_meet_const(c1, m1) ->
        compose_core_right dst (compose_meet_const_right dst m0 c1) m1
    | Imply_and_core(m0, c0), _ ->
        compose_core_left dst m0 (compose_imply_left (src_core m0) c0 m1)
    | _, Imply_and_core(m1, c1) ->
        compose_imply_right dst (compose_core_right dst m0 m1) c1
    | _, Max_with_and(m1, ax1) ->
        Max_with_and(compose dst m0 m1, ax1)
    | _, Min_with_and(m1, ax1) ->
        Min_with_and(compose dst m0 m1, ax1)
    | And_proj(obj0, ax0, m0), _ ->
        And_proj(obj0, ax0, compose obj0 m0 m1)
    | Max_with_and(m0, ax0), And_proj(obj1, ax1, m1)-> begin
        let mid = src dst m0 in
        let c = min_with mid ax0 (max (proj_obj ax0 mid)) in
        let m0 = compose_imply_right dst m0 c in
        let m0 = connect_projections dst m0 (src dst m0) ax0 obj1 ax1 in
        compose dst m0 m1
      end
    | Min_with_and(m0, ax0), And_proj(obj1, ax1, m1)-> begin
        let mid = src dst m0 in
        let c = min_with mid ax0 (max (proj_obj ax0 mid)) in
        let m0 = compose_meet_const_right dst m0 c in
        let m0 = connect_projections dst m0 (src dst m0) ax0 obj1 ax1 in
        compose dst m0 m1
      end
    | Compose _, _ -> Compose(m0, m1)
    | _, Compose _ -> Compose(m0, m1)

end

module C = Lattices_mono
module S = Solvers_polarized (C)

type monadic = C.monadic =
  { uniqueness : C.Uniqueness.t;
    contention : C.Contention.t; }

type 'a comonadic_with = 'a C.comonadic_with =
  { areality : 'a;
    linearity : C.Linearity.t;
    portability : C.Portability.t; }

module Axis = C.Axis

type changes = S.changes

let undo_changes = S.undo_changes

(* To be filled in by [types.ml] *)
let append_changes : (changes ref -> unit) ref = ref (fun _ -> assert false)

let set_append_changes f = append_changes := f

type ('a, 'd) mode_monadic = ('a, 'd) S.Negative.mode

type ('a, 'd) mode_comonadic = ('a, 'd) S.Positive.mode

(** Representing a single object *)
module type Obj = sig
  type const

  module Solver : S.Solver_polarized

  val obj : const C.obj
end

let try_with_log op =
  let log' = ref S.empty_changes in
  let log = Some log' in
  match op ~log with
  | Ok _ as x ->
    !append_changes log';
    x
  | Error _ as x ->
    S.undo_changes !log';
    x
  [@@inline]

let with_log op =
  let log' = ref S.empty_changes in
  let log = Some log' in
  let r = op ~log in
  !append_changes log';
  r
  [@@inline]

let equate_from_submode submode_log m0 m1 ~log =
  match submode_log m0 m1 ~log with
  | Error e -> Error (Left_le_right, e)
  | Ok () -> (
    match submode_log m1 m0 ~log with
    | Error e -> Error (Right_le_left, e)
    | Ok () -> Ok ())
  [@@inline]

let equate_from_submode' submode m0 m1 =
  match submode m0 m1 with
  | Error e -> Error (Left_le_right, e)
  | Ok () -> (
    match submode m1 m0 with
    | Error e -> Error (Right_le_left, e)
    | Ok () -> Ok ())
  [@@inline]

module Common (Obj : Obj) = struct
  open Obj

  type 'd t = (const, 'd) Solver.mode

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type nonrec error = const error

  type equate_error = equate_step * error

  type (_, _, 'd) sided = 'd t

  let disallow_right m = Solver.disallow_right m

  let disallow_left m = Solver.disallow_left m

  let allow_left m = Solver.allow_left m

  let allow_right m = Solver.allow_right m

  let newvar () = Solver.newvar obj

  let min = Solver.min obj

  let max = Solver.max obj

  let newvar_above m = Solver.newvar_above obj m

  let newvar_below m = Solver.newvar_below obj m

  let submode_log a b ~log = Solver.submode obj a b ~log

  let submode a b = try_with_log (submode_log a b)

  let join l = Solver.join obj l

  let meet l = Solver.meet obj l

  let submode_exn m0 m1 = assert (submode m0 m1 |> Result.is_ok)

  let equate a b = try_with_log (equate_from_submode submode_log a b)

  let equate_exn m0 m1 = assert (equate m0 m1 |> Result.is_ok)

  let print ?verbose () ppf m = Solver.print ?verbose obj ppf m

  let zap_to_ceil m = with_log (Solver.zap_to_ceil obj m)

  let zap_to_floor m = with_log (Solver.zap_to_floor obj m)

  let of_const : type l r. const -> (l * r) t = fun a -> Solver.of_const obj a

  module Guts = struct
    let get_floor m = Solver.get_floor obj m

    let get_ceil m = Solver.get_ceil obj m

    let get_conservative_floor m = Solver.get_conservative_floor obj m

    let get_conservative_ceil m = Solver.get_conservative_ceil obj m
  end
end
[@@inline]

module Locality = struct
  module Const = C.Locality

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj = C.Locality
  end

  include Common (Obj)

  let global = of_const Global

  let local = of_const Local

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor

  module Guts = struct
    let check_const m =
      let floor = Guts.get_floor m in
      let ceil = Guts.get_ceil m in
      if Const.le ceil floor then Some ceil else None

    let check_const_conservative m =
      let floor = Guts.get_conservative_floor m in
      let ceil = Guts.get_conservative_ceil m in
      if Const.le ceil floor then Some ceil else None
  end
end

module Regionality = struct
  module Const = C.Regionality

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj = C.Regionality
  end

  include Common (Obj)

  let local = of_const Const.Local

  let regional = of_const Const.Regional

  let global = of_const Const.Global

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Linearity = struct
  module Const = C.Linearity

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj : _ C.obj = C.Linearity
  end

  include Common (Obj)

  let many = of_const Many

  let once = of_const Once

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Portability = struct
  module Const = C.Portability

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj : _ C.obj = C.Portability
  end

  include Common (Obj)

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

module Uniqueness = struct
  module Const = C.Uniqueness

  module Const_op = C.Uniqueness_op

  module Obj = struct
    type const = Const.t

    (* the negation of Uniqueness_op gives us the proper uniqueness *)
    module Solver = S.Negative

    let obj = C.Uniqueness_op
  end

  include Common (Obj)

  let aliased = of_const Aliased

  let unique = of_const Unique

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

module Contention = struct
  module Const = C.Contention

  module Const_op = C.Contention_op

  module Obj = struct
    type const = Const.t

    (* the negation of Contention_op gives us the proper contention *)
    module Solver = S.Negative

    let obj = C.Contention_op
  end

  include Common (Obj)

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

let locality_as_regionality m =
  S.Positive.via_monotone Regionality.Obj.obj
    (C.Min_with_and(
         C.And_proj(Comonadic_with_regionality, Areality,
                    Core C.Locality_as_regionality),
         Areality))
    m

module type Areality = sig
  module Const : C.Areality

  module Obj : Obj with type const = Const.t and module Solver = S.Positive

  val zap_to_legacy : (Const.t, allowed * 'r) Obj.Solver.mode -> Const.t
end

module Comonadic_with (Areality : Areality) = struct
  module Obj = struct
    type const = Areality.Obj.const C.comonadic_with

    module Solver = S.Positive

    let obj = C.areality_comonadic_obj Areality.Const.areality
  end

  include Common (Obj)

  type error = Error : (Obj.const, 'a) C.Axis.t * 'a Solver.error -> error

  type equate_error = equate_step * error

  let proj_obj ax = C.proj_obj ax Obj.obj

  module Const = struct
    include C.Comonadic_with (Areality.Const)

    let eq a b = le a b && le b a

    let le_axis ax a b =
      let obj = proj_obj ax in
      C.le obj a b

    let min_axis ax =
      let obj = proj_obj ax in
      C.min obj

    let max_axis ax =
      let obj = proj_obj ax in
      C.max obj

    let max_with ax c = Axis.update ax c (C.max Obj.obj)

    let print_axis ax ppf a =
      let obj = proj_obj ax in
      C.print obj ppf a
  end

  let meet_const c m =
    Obj.Solver.via_monotone Obj.obj (Meet_const c) (disallow_right m)

  let imply c m =
    Obj.Solver.via_monotone Obj.obj (Imply c) (disallow_left m)

  let proj ax m =
    Obj.Solver.via_monotone (proj_obj ax) (And_proj(Obj.obj, ax, Id)) m

  let min_with ax m =
    Obj.Solver.via_monotone Obj.obj (Min_with_and(Id, ax)) (disallow_right m)

  let max_with ax m =
    Obj.Solver.via_monotone Obj.obj (Max_with_and(Id, ax)) (disallow_left m)

  let meet_const_with ax c m =
    meet_const (C.max_with Obj.obj ax c) m

  let imply_with ax c m = imply (C.max_with Obj.obj ax c) m

  let zap_to_legacy m : Const.t =
    let areality = proj Areality m |> Areality.zap_to_legacy in
    let linearity = proj Linearity m |> Linearity.zap_to_legacy in
    let portability = proj Portability m |> Portability.zap_to_legacy in
    { areality; linearity; portability }

  let legacy = of_const Const.legacy

  let axis_of_error (err : Obj.const Solver.error) : error =
    if Areality.Const.le err.left.areality err.right.areality
    then
      if Linearity.Const.le err.left.linearity err.right.linearity
      then
        if Portability.Const.le err.left.portability err.right.portability
        then assert false
        else Error (Portability,
                    { left = err.left.portability; right = err.right.portability })
      else Error (Linearity, { left = err.left.linearity; right = err.right.linearity })
    else Error (Areality, { left = err.left.areality; right = err.right.areality })

  (* overriding to report the offending axis *)
  let submode_log m0 m1 ~log : _ result =
    match submode_log m0 m1 ~log with
    | Ok () -> Ok ()
    | Error e -> Error (axis_of_error e)

  let submode a b = try_with_log (submode_log a b)

  (* override to report the offending axis *)
  let equate a b = try_with_log (equate_from_submode submode_log a b)
end
[@@inline]

module Monadic = struct
  module Obj = struct
    type const = C.Monadic_op.t

    (* Negative solver on the opposite of monadic should give the monadic
       fragment with original ordering *)
    module Solver = S.Negative

    let obj = C.Monadic_op
  end

  include Common (Obj)

  type error = Error : (Obj.const, 'a) C.Axis.t * 'a Solver.error -> error

  type equate_error = equate_step * error

  let proj_obj ax = C.proj_obj ax Obj.obj

  module Const = struct
    include C.Monadic

    (* CR zqian: The flipping logic leaking to here is bad. Refactoring needed. *)

    (* Monadic fragment is flipped, so are the following definitions. *)
    let min_with ax c = Axis.update ax c (C.max Obj.obj)

    let min_axis ax =
      let obj = proj_obj ax in
      C.max obj

    let max_axis ax =
      let obj = proj_obj ax in
      C.min obj

    let le_axis ax a b =
      let obj = proj_obj ax in
      C.le obj b a
  end

  module Const_op = C.Monadic_op

  let join_const c m =
    Obj.Solver.via_monotone Obj.obj (Meet_const c) (disallow_left m)

  let subtract c m =
    Obj.Solver.via_monotone Obj.obj (Imply c) (disallow_right m)

  let proj ax m = Obj.Solver.via_monotone (proj_obj ax) (And_proj(Obj.obj, ax, Id)) m

  (* The monadic fragment is inverted. Most of the inversion logic is taken care
     by [Solver_polarized], but some remain, such as the [Min_with] below which
     is inverted from [Max_with]. *)

  let max_with ax m =
    Obj.Solver.via_monotone Obj.obj (Min_with_and(Id, ax)) (disallow_left m)

  let min_with ax m =
    Obj.Solver.via_monotone Obj.obj (Max_with_and(Id, ax)) (disallow_right m)

  let join_const_with ax c m = join_const (C.min_with Obj.obj ax c) m

  let subtract_with ax c m = subtract (C.min_with Obj.obj ax c) m

  let zap_to_legacy m : Const.t =
    let uniqueness = proj Uniqueness m |> Uniqueness.zap_to_legacy in
    let contention = proj Contention m |> Contention.zap_to_legacy in
    { uniqueness; contention }

  let legacy = of_const Const.legacy

  let axis_of_error (err : Obj.const Solver.error) : error =
    if Uniqueness.Const.le err.left.uniqueness err.right.uniqueness
    then
      if Contention.Const.le err.left.contention err.right.contention
      then assert false
      else Error (Contention, { left = err.left.contention; right = err.right.contention })
    else Error (Uniqueness, { left = err.left.uniqueness; right = err.right.uniqueness })

  (* overriding to report the offending axis *)
  let submode_log m0 m1 ~log : _ result =
    match submode_log m0 m1 ~log with
    | Ok () -> Ok ()
    | Error e -> Error (axis_of_error e)

  let submode a b = try_with_log (submode_log a b)

  (* override to report the offending axis *)
  let equate a b = try_with_log (equate_from_submode submode_log a b)
end

type ('mo, 'como) monadic_comonadic =
  { monadic : 'mo;
    comonadic : 'como
  }

module Value_with (Areality : Areality) = struct
  module Comonadic = Comonadic_with (Areality)
  module Monadic = Monadic

  type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type ('m, 'a, 'd) axis =
    | Monadic :
        (Monadic.Const.t, 'a) Axis.t
        -> (('a, 'd) mode_monadic, 'a, 'd) axis
    | Comonadic :
        (Comonadic.Const.t, 'a) Axis.t
        -> (('a, 'd) mode_comonadic, 'a, 'd) axis

  let proj_obj : type m a d. (m, a, d) axis -> a C.obj = function
    | Monadic ax -> Monadic.proj_obj ax
    | Comonadic ax -> Comonadic.proj_obj ax

  type ('a, 'b, 'c, 'd, 'e) modes =
    { areality : 'a;
      linearity : 'b;
      uniqueness : 'c;
      portability : 'd;
      contention : 'e
    }

  let split { areality; linearity; portability; uniqueness; contention } =
    let monadic : Monadic.Const.t = { uniqueness; contention } in
    let comonadic : Comonadic.Const.t = { areality; linearity; portability } in
    { comonadic; monadic }

  let merge { comonadic; monadic } =
    let { areality; linearity; portability } : Comonadic.Const.t = comonadic in
    let { uniqueness; contention } : Monadic.Const.t = monadic in
    { areality; linearity; portability; uniqueness; contention }

  let print ?verbose () ppf { monadic; comonadic } =
    Format.fprintf ppf "%a;%a"
      (Comonadic.print ?verbose ())
      comonadic
      (Monadic.print ?verbose ())
      monadic

  let of_const c =
    let { monadic; comonadic } = split c in
    let comonadic = Comonadic.of_const comonadic in
    let monadic = Monadic.of_const monadic in
    { comonadic; monadic }

  module Const = struct
    type t =
      ( Areality.Const.t,
        Linearity.Const.t,
        Uniqueness.Const.t,
        Portability.Const.t,
        Contention.Const.t )
      modes

    module Monadic = Monadic.Const
    module Comonadic = Comonadic.Const

    let min = merge { comonadic = Comonadic.min; monadic = Monadic.min }

    let max = merge { comonadic = Comonadic.max; monadic = Monadic.max }

    let le m0 m1 =
      let m0 = split m0 in
      let m1 = split m1 in
      Comonadic.le m0.comonadic m1.comonadic
      && Monadic.le m0.monadic m1.monadic

    let equal m0 m1 =
      let m0 = split m0 in
      let m1 = split m1 in
      Comonadic.equal m0.comonadic m1.comonadic
      && Monadic.equal m0.monadic m1.monadic

    let print ppf m =
      let { monadic; comonadic } = split m in
      Format.fprintf ppf "%a,%a" Comonadic.print comonadic Monadic.print monadic

    let legacy =
      merge { comonadic = Comonadic.legacy; monadic = Monadic.legacy }

    let meet m0 m1 =
      let m0 = split m0 in
      let m1 = split m1 in
      let monadic = Monadic.meet m0.monadic m1.monadic in
      let comonadic = Comonadic.meet m0.comonadic m1.comonadic in
      merge { monadic; comonadic }

    let join m0 m1 =
      let m0 = split m0 in
      let m1 = split m1 in
      let monadic = Monadic.join m0.monadic m1.monadic in
      let comonadic = Comonadic.join m0.comonadic m1.comonadic in
      merge { monadic; comonadic }

    module Option = struct
      type some = t

      type t =
        ( Areality.Const.t option,
          Linearity.Const.t option,
          Uniqueness.Const.t option,
          Portability.Const.t option,
          Contention.Const.t option )
        modes

      let none =
        { areality = None;
          uniqueness = None;
          linearity = None;
          portability = None;
          contention = None
        }

      let value opt ~default =
        let areality = Option.value opt.areality ~default:default.areality in
        let uniqueness =
          Option.value opt.uniqueness ~default:default.uniqueness
        in
        let linearity = Option.value opt.linearity ~default:default.linearity in
        let portability =
          Option.value opt.portability ~default:default.portability
        in
        let contention =
          Option.value opt.contention ~default:default.contention
        in
        { areality; uniqueness; linearity; portability; contention }

      let print ppf { areality; uniqueness; linearity; portability; contention }
          =
        let option_print print ppf = function
          | None -> Format.fprintf ppf "None"
          | Some a -> Format.fprintf ppf "Some %a" print a
        in
        Format.fprintf ppf "%a,%a,%a,%a,%a"
          (option_print Areality.Const.print)
          areality
          (option_print Linearity.Const.print)
          linearity
          (option_print Uniqueness.Const.print)
          uniqueness
          (option_print Portability.Const.print)
          portability
          (option_print Contention.Const.print)
          contention
    end

    let diff m0 m1 =
      let diff le a0 a1 = if le a0 a1 && le a1 a0 then None else Some a0 in
      let areality = diff Areality.Const.le m0.areality m1.areality in
      let linearity = diff Linearity.Const.le m0.linearity m1.linearity in
      let uniqueness = diff Uniqueness.Const.le m0.uniqueness m1.uniqueness in
      let portability =
        diff Portability.Const.le m0.portability m1.portability
      in
      let contention = diff Contention.Const.le m0.contention m1.contention in
      { areality; linearity; uniqueness; portability; contention }

    (** See [Alloc.close_over] for explanation. *)
    let close_over m =
      let { monadic; comonadic } = split m in
      let comonadic =
        Comonadic.join comonadic
          (C.monadic_to_comonadic_min
             (C.areality_comonadic_obj Areality.Const.areality)
             monadic)
      in
      let monadic = Monadic.min in
      merge { comonadic; monadic }

    (** See [Alloc.partial_apply] for explanation. *)
    let partial_apply m =
      let { comonadic; _ } = split m in
      let monadic = Monadic.min in
      merge { comonadic; monadic }

    let print_axis : type m a d. (m, a, d) axis -> _ -> a -> unit =
     fun ax ppf a ->
      let obj = proj_obj ax in
      C.print obj ppf a

    let le_axis : type m a d. (m, a, d) axis -> a -> a -> bool =
     fun ax m0 m1 ->
      match ax with
      | Comonadic ax -> Comonadic.le_axis ax m0 m1
      | Monadic ax -> Monadic.le_axis ax m0 m1

    let min_axis : type m a d. (m, a, d) axis -> a = function
      | Comonadic ax -> Comonadic.min_axis ax
      | Monadic ax -> Monadic.min_axis ax

    let max_axis : type m a d. (m, a, d) axis -> a = function
      | Comonadic ax -> Comonadic.max_axis ax
      | Monadic ax -> Monadic.max_axis ax

    let split = split

    let merge = merge
  end

  let min = { comonadic = Comonadic.min; monadic = Monadic.min }

  let max = { comonadic = Comonadic.max; monadic = Monadic.max }

  include Magic_allow_disallow (struct
    type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

    let allow_left { monadic; comonadic } =
      let monadic = Monadic.allow_left monadic in
      let comonadic = Comonadic.allow_left comonadic in
      { monadic; comonadic }

    let allow_right { monadic; comonadic } =
      let monadic = Monadic.allow_right monadic in
      let comonadic = Comonadic.allow_right comonadic in
      { monadic; comonadic }

    let disallow_left { monadic; comonadic } =
      let monadic = Monadic.disallow_left monadic in
      let comonadic = Comonadic.disallow_left comonadic in
      { monadic; comonadic }

    let disallow_right { monadic; comonadic } =
      let monadic = Monadic.disallow_right monadic in
      let comonadic = Comonadic.disallow_right comonadic in
      { monadic; comonadic }
  end)

  let newvar () =
    let comonadic = Comonadic.newvar () in
    let monadic = Monadic.newvar () in
    { comonadic; monadic }

  let newvar_above { comonadic; monadic } =
    let comonadic, b0 = Comonadic.newvar_above comonadic in
    let monadic, b1 = Monadic.newvar_above monadic in
    { monadic; comonadic }, b0 || b1

  let newvar_below { comonadic; monadic } =
    let comonadic, b0 = Comonadic.newvar_below comonadic in
    let monadic, b1 = Monadic.newvar_below monadic in
    { monadic; comonadic }, b0 || b1

  type error = Error : ('m, 'a, 'd) axis * 'a Solver.error -> error

  type equate_error = equate_step * error

  let submode_log { monadic = monadic0; comonadic = comonadic0 }
      { monadic = monadic1; comonadic = comonadic1 } ~log : (_, error) result =
    (* comonadic before monadic, so that locality errors dominate
       (error message backward compatibility) *)
    match Comonadic.submode_log comonadic0 comonadic1 ~log with
    | Error (Error (ax, e)) -> Error (Error (Comonadic ax, e))
    | Ok () -> (
      match Monadic.submode_log monadic0 monadic1 ~log with
      | Error (Error (ax, e)) -> Error (Error (Monadic ax, e))
      | Ok () -> Ok ())

  let submode a b = try_with_log (submode_log a b)

  let equate a b = try_with_log (equate_from_submode submode_log a b)

  let submode_exn m0 m1 =
    match submode m0 m1 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let equate_exn m0 m1 =
    match equate m0 m1 with Ok () -> () | Error _ -> invalid_arg "equate_exn"

  let legacy =
    let comonadic = Comonadic.legacy in
    let monadic = Monadic.legacy in
    { comonadic; monadic }

  let join l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.join como in
    let monadic = Monadic.join mo in
    { comonadic; monadic }

  let meet l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.meet como in
    let monadic = Monadic.meet mo in
    { comonadic; monadic }

  let meet_const c { monadic; comonadic } =
    let comonadic = Comonadic.meet_const c comonadic in
    let monadic = Monadic.disallow_right monadic in
    { comonadic; monadic }

  let join_const c { monadic; comonadic } =
    let comonadic = Comonadic.disallow_left comonadic in
    let monadic = Monadic.join_const c monadic in
    { monadic; comonadic }

  let imply c { monadic; comonadic } =
    let comonadic = Comonadic.imply c comonadic in
    let monadic = Monadic.disallow_left monadic in
    { comonadic; monadic }

  let subtract c { monadic; comonadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    let monadic = Monadic.subtract c monadic in
    { monadic; comonadic }

  let proj_monadic ax { monadic; _ } = Monadic.proj ax monadic

  let proj_comonadic ax { comonadic; _ } = Comonadic.proj ax comonadic

  let proj : type m a l r. (m, a, l * r) axis -> (l * r) t -> m =
   fun ax m ->
    match ax with
    | Monadic ax -> proj_monadic ax m
    | Comonadic ax -> proj_comonadic ax m

  let max_with_monadic ax m =
    let comonadic =
      Comonadic.max |> Comonadic.disallow_left |> Comonadic.allow_right
    in
    let monadic = Monadic.max_with ax m in
    { comonadic; monadic }

  let max_with_comonadic ax m =
    let comonadic = Comonadic.max_with ax m in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let max_with : type m a l r. (m, a, l * r) axis -> m -> (disallowed * r) t =
   fun ax m ->
    match ax with
    | Monadic ax -> max_with_monadic ax m
    | Comonadic ax -> max_with_comonadic ax m

  let min_with_monadic ax m =
    let comonadic =
      Comonadic.min |> Comonadic.disallow_right |> Comonadic.allow_left
    in
    let monadic = Monadic.min_with ax m in
    { comonadic; monadic }

  let min_with_comonadic ax m =
    let comonadic = Comonadic.min_with ax m in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let min_with : type m a l r. (m, a, l * r) axis -> m -> (l * disallowed) t =
   fun ax m ->
    match ax with
    | Monadic ax -> min_with_monadic ax m
    | Comonadic ax -> min_with_comonadic ax m

  let meet_with ax m1 m2 =
    meet [max_with ax m1; disallow_left m2]

  let join_with ax m1 m2 =
    join [min_with ax m1; disallow_right m2]

  let meet_const_with : type a l r. (Comonadic.Const.t, a) Axis.t ->
           a -> (l * r) t -> (l * disallowed) t
= fun ax c { monadic; comonadic } ->
    let comonadic = Comonadic.meet_const_with ax c comonadic in
    let monadic = Monadic.disallow_right monadic in
    { comonadic; monadic }

  let join_const_with ax c { monadic; comonadic } =
    let monadic = Monadic.join_const_with ax c monadic in
    let comonadic = Comonadic.disallow_left comonadic in
    { monadic; comonadic }

  let imply_with ax c { monadic; comonadic } =
    let comonadic = Comonadic.imply_with ax c comonadic in
    let monadic = Monadic.disallow_left monadic in
    { comonadic; monadic }

  let subtract_with ax c { monadic; comonadic } =
    let monadic = Monadic.subtract_with ax c monadic in
    let comonadic = Comonadic.disallow_right comonadic in
    { monadic; comonadic }

  let comonadic_to_monadic m =
    S.Negative.via_antitone Monadic.Obj.obj
      (Core (Comonadic_to_monadic Areality.Const.areality)) m

  let monadic_to_comonadic_min m =
    S.Positive.via_antitone Comonadic.Obj.obj (Core Monadic_to_comonadic_min)
      (Monadic.disallow_left m)

  let zap_to_ceil { comonadic; monadic } =
    let monadic = Monadic.zap_to_ceil monadic in
    let comonadic = Comonadic.zap_to_ceil comonadic in
    merge { monadic; comonadic }

  let zap_to_legacy { comonadic; monadic } =
    let monadic = Monadic.zap_to_legacy monadic in
    let comonadic = Comonadic.zap_to_legacy comonadic in
    merge { monadic; comonadic }

  (** This is about partially applying [A -> B -> C] to [A] and getting [B ->
    C]. [comonadic] and [monadic] constutute the mode of [A], and we need to
    give the lower bound mode of [B -> C]. *)
  let close_over { comonadic; monadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    (* The comonadic of the returned function is constrained by the monadic of the closed argument via the dualizing morphism. *)
    let comonadic1 = monadic_to_comonadic_min monadic in
    (* It's also constrained by the comonadic of the closed argument. *)
    let comonadic = Comonadic.join [comonadic; comonadic1] in
    (* The returned function crosses all monadic axes that we know of
       (uniqueness/contention). *)
    let monadic = Monadic.disallow_right Monadic.min in
    { comonadic; monadic }

  (** Similar to above, but we are given the mode of [A -> B -> C], and need to
      give the lower bound mode of [B -> C]. *)
  let partial_apply { comonadic; _ } =
    (* The returned function crosses all monadic axes that we know of. *)
    let monadic = Monadic.disallow_right Monadic.min in
    let comonadic = Comonadic.disallow_right comonadic in
    { comonadic; monadic }

  module List = struct
    type nonrec 'd t = 'd t list

    include Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

      let allow_left l = List.map allow_left l

      let allow_right l = List.map allow_right l

      let disallow_left l = List.map disallow_left l

      let disallow_right l = List.map disallow_right l
    end)
  end
end
[@@inline]

module Value = Value_with (Regionality)
module Alloc = Value_with (Locality)

module Const = struct
  let alloc_as_value_comonadic m =
    C.locality_as_regionality m

  let alloc_as_value m =
    let { comonadic; monadic } = Alloc.split m in
    let comonadic = alloc_as_value_comonadic comonadic in
    Value.merge { comonadic; monadic }

  let locality_as_regionality =
    C.locality_as_regionality_projected
end

let alloc_as_value m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Value.Comonadic.Obj.obj
      (Core Locality_as_regionality) comonadic
  in
  { comonadic; monadic }

let alloc_to_value_l2r m =
  let { comonadic; monadic } = Alloc.disallow_right m in
  let comonadic =
    S.Positive.via_monotone Value.Comonadic.Obj.obj
      (Core Local_to_regional) comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2g :
      type l r. (l * r) Value.t -> (disallowed * r) Alloc.t =
  fun m ->
    let { comonadic; monadic } = m in
    let comonadic =
      S.Positive.via_monotone Alloc.Comonadic.Obj.obj
        (Core Regional_to_global)
        (Alloc.Comonadic.disallow_left comonadic)
    in
    let monadic = Monadic.disallow_left monadic in
    { comonadic; monadic }

let value_to_alloc_r2l m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Alloc.Comonadic.Obj.obj
      (Core Regional_to_local) comonadic
  in
  { comonadic; monadic }

module Modality = struct
  type ('m, 'a) raw =
    | Meet_const : 'a -> (('a, 'l * 'r) mode_comonadic, 'a) raw
    | Join_const : 'a -> (('a, 'l * 'r) mode_monadic, 'a) raw

  type t = Atom : ('m, 'a, _) Value.axis * ('m, 'a) raw -> t

  let is_id (Atom (ax, a)) =
    match a with
    | Join_const c -> Value.Const.le_axis ax c (Value.Const.min_axis ax)
    | Meet_const c -> Value.Const.le_axis ax (Value.Const.max_axis ax) c

  let print ppf = function
    | Atom (ax, Join_const c) ->
      Format.fprintf ppf "join_const(%a)" (C.print (Value.proj_obj ax)) c
    | Atom (ax, Meet_const c) ->
      Format.fprintf ppf "meet_const(%a)" (C.print (Value.proj_obj ax)) c

  module Monadic = struct
    module Mode = Value.Monadic

    type 'a axis = (Mode.Const.t, 'a) Axis.t

    type error =
      | Error : 'a axis * (('a, _) mode_monadic, 'a) raw Solver.error -> error

    module Const = struct
      type t = Join_const of Mode.Const.t

      let id = Join_const Mode.Const.min

      let max = Join_const Mode.Const.max

      let sub left right : (_, error) Result.t =
        match left, right with
        | Join_const c0, Join_const c1 ->
          if Mode.Const.le c0 c1
          then Ok ()
          else
            let (Error (ax, { left; right })) =
              Mode.axis_of_error { left = c0; right = c1 }
            in
            Error
              (Error (ax, { left = Join_const left; right = Join_const right }))

      let compose :
          type a l r. a axis -> ((a, l * r) mode_monadic, a) raw -> t -> t =
       fun ax a t ->
        match a, t with
        | Join_const c0, Join_const c ->
          Join_const (Mode.Const.join (Mode.Const.min_with ax c0) c)
        | Meet_const _, Join_const _ -> assert false

      let concat ~then_ t =
        match then_, t with
        | Join_const c0, Join_const c1 -> Join_const (Mode.Const.join c0 c1)

      let apply_right : t -> ('l * allowed) Mode.t -> Mode.r =
        fun t x ->
          match t with
          | Join_const c -> Mode.join_const c x

      let apply_left : t -> (allowed * 'r) Mode.t -> Mode.l =
        fun t x ->
          match t with
          | Join_const c -> Mode.join [Mode.of_const c; x]

      let to_list = function
        | Join_const c ->
          [ (let ax : _ Axis.t = Uniqueness in
             Atom (Monadic ax, Join_const (Axis.proj ax c)));
            (let ax : _ Axis.t = Contention in
             Atom (Monadic ax, Join_const (Axis.proj ax c))) ]

      let print ppf = function
        | Join_const c -> Format.fprintf ppf "join_const(%a)" Mode.Const.print c

      (** Given a modality and a guarantee that the modality will only be appled
      on [x >= mm], we can find some lower modality that is equivalent on the
      restricted range. This is similar to mode-crossing, where we can push a
      mode lower given a restricted range of types. *)
      let modality_cross_left ~mm = function
        | Join_const c ->
          (* We want to find the minimal [c'] such that [join c x <= join c' x]
             for all [x >= mm]. By definition of join, this is equivalent to [c
             <= join x c'] for all [x >= mm]. This is equivalent to [c <= join
             mm c']. Equivalently [subtract c mm <= c']. Note that [mm] is a
             mode variable, but we need a constant. Therefore, we conservatively
             take its incomplete lower bound [mm.lower]. Also recall that we
             want the smallest such [c']. So we take [c' = subtract c mm.lower].
          *)
          let mm = Mode.Guts.get_floor mm in
          Join_const (Mode.Const.subtract c mm)
    end

    type t =
      | Const of Const.t
      | Diff of Mode.lr * Mode.l
      | Undefined

    let sub_log left right ~log : (unit, error) Result.t =
      match left, right with
      | Const c0, Const c1 -> Const.sub c0 c1
      | Diff (mm, m), Const (Join_const c) -> (
        (* Check that for any x >= mm, join(x, m) <= join(x, c), which (by
           definition of join) is equivalent to m <= join(x, c). This has to
           hold for all x >= mm, so we check m <= join(mm, c). *)
        match Mode.submode_log m
                (Mode.join_const c (Mode.disallow_left mm)) ~log with
        | Ok () -> Ok ()
        | Error (Error (ax, { left; _ })) ->
          Error
            (Error
               ( ax,
                 { left = Join_const left; right = Join_const (Axis.proj ax c) }
               )))
      | Diff (_, _m0), Diff (_, _m1) ->
        (* [m1] is a left mode so it cannot appear on the right. So we can't do
           a proper check. However, this branch is only hit by
           [wrap_constraint_with_shape], in which case LHS and RHS should be
           physically equal. *)
        assert (left == right);
        Ok ()
      | Const _, Diff _ ->
        Misc.fatal_error
          "inferred modality Diff should not be on the RHS of sub."
      | Undefined, _ | _, Undefined ->
        Misc.fatal_error "modality Undefined should not be in sub."

    let id = Const Const.id

    let apply : type r. t -> (allowed * r) Mode.t -> Mode.l =
     fun t x ->
      match t with
      | Const c -> Const.apply_left c (Mode.disallow_right x)
      | Undefined ->
        Misc.fatal_error "modality Undefined should not be applied."
      | Diff (_, m) -> Mode.join [m; Mode.disallow_right x]

    let print ppf = function
      | Const c -> Const.print ppf c
      | Undefined -> Format.fprintf ppf "undefined"
      | Diff _ -> Format.fprintf ppf "diff"

    let zap_to_floor = function
      | Const c -> c
      | Undefined -> Misc.fatal_error "modality Undefined should not be zapped."
      | Diff (mm, m) ->
        let c = Mode.zap_to_floor m in
        let m = Const.Join_const c in
        (* To give the best modality, we try to cross modality. *)
        Const.modality_cross_left ~mm m

    let zap_to_id = zap_to_floor

    let to_const_exn = function
      | Const c -> c
      | Undefined | Diff _ ->
        Misc.fatal_error "Got infered modality but constant modality expected."

    let of_const c = Const c

    let infer ~md_mode ~mode = Diff (md_mode, mode)

    let max = Const Const.max
  end

  module Comonadic = struct
    module Mode = Value.Comonadic

    type 'a axis = (Mode.Const.t, 'a) Axis.t

    type error =
      | Error : 'a axis * (('a, _) mode_comonadic, 'a) raw Solver.error -> error

    module Const = struct
      type t = Meet_const of Mode.Const.t

      let id = Meet_const Mode.Const.max

      let max = Meet_const Mode.Const.max

      let sub left right : (_, error) Result.t =
        match left, right with
        | Meet_const c0, Meet_const c1 ->
          if Mode.Const.le c0 c1
          then Ok ()
          else
            let (Error (ax, { left; right })) =
              Mode.axis_of_error { left = c0; right = c1 }
            in
            Error
              (Error (ax, { left = Meet_const left; right = Meet_const right }))

      let compose :
          type a l r. a axis -> ((a, l * r) mode_comonadic, a) raw -> t -> t =
       fun ax a t ->
        match a, t with
        | Meet_const c0, Meet_const c ->
          Meet_const (Mode.Const.meet (Mode.Const.max_with ax c0) c)
        | Join_const _, Meet_const _ -> assert false

      let concat ~then_ t =
        match then_, t with
        | Meet_const c0, Meet_const c1 -> Meet_const (Mode.Const.meet c0 c1)

      let apply_left : t -> (allowed * 'r) Mode.t -> Mode.l =
        fun t x ->
          match t with
          | Meet_const c -> Mode.meet_const c x

      let apply_right : t -> ('l * allowed) Mode.t -> Mode.r =
       fun t x ->
         match t with
         | Meet_const c -> Mode.meet [Mode.of_const c; x]

      let to_list = function
        | Meet_const c ->
          [ (let ax : _ Axis.t = Areality in
             Atom (Comonadic ax, Meet_const (Axis.proj ax c)));
            (let ax : _ Axis.t = Linearity in
             Atom (Comonadic ax, Meet_const (Axis.proj ax c)));
            (let ax : _ Axis.t = Portability in
             Atom (Comonadic ax, Meet_const (Axis.proj ax c))) ]

      let print ppf = function
        | Meet_const c -> Format.fprintf ppf "meet_const(%a)" Mode.Const.print c
    end

    type t =
      | Const of Const.t
      | Undefined
      | Exactly of Mode.lr * Mode.l

    let sub_log left right ~log : (unit, error) Result.t =
      match left, right with
      | Const c0, Const c1 -> Const.sub c0 c1
      | Exactly (_mm, m), Const (Meet_const c) -> (
        (* Check for all x >= mm, m <= meet x c. Equivalent to check [m <= meet
           mm c]. By definition of meet, equivalent to check [m <= mm] and [m <=
           c]. The former is the precondition of [Exactly]. So we only check the
           latter. *)
        match Mode.submode_log m (Mode.of_const c) ~log with
        | Ok () -> Ok ()
        | Error (Error (ax, { left; _ })) ->
          Error
            (Error
               ( ax,
                 { left = Meet_const left; right = Meet_const (Axis.proj ax c) }
               )))
      | Exactly (_, _m0), Exactly (_, _m1) ->
        (* [m1] is a left mode, so there is no good way to check.
           However, this branch only hit by [wrap_constraint_with_shape],
           in which case LHS and RHS should be physically equal. *)
        assert (left == right);
        Ok ()
      | Const _, Exactly _ ->
        Misc.fatal_error
          "inferred modaltiy Exactly should not be on the RHS of sub."
      | Undefined, _ | _, Undefined ->
        Misc.fatal_error "modality Undefined should not be in sub."

    let id = Const Const.id

    let apply : type r. t -> (allowed * r) Mode.t -> Mode.l =
     fun t x ->
      match t with
      | Const c -> Const.apply_left c (Mode.disallow_right x)
      | Undefined ->
        Misc.fatal_error "modality Undefined should not be applied."
      | Exactly (_mm, m) -> m

    let print ppf = function
      | Const c -> Const.print ppf c
      | Undefined -> Format.fprintf ppf "undefined"
      | Exactly _ -> Format.fprintf ppf "exactly"

    let infer ~md_mode ~mode = Exactly (md_mode, mode)

    let max = Const Const.max

    let zap_to_ceil = function
      | Const c -> c
      | Undefined -> Misc.fatal_error "modality Undefined should not be zapped."
      | Exactly _ -> Const.id

    let zap_to_id = zap_to_ceil

    let zap_to_floor = function
      | Const c -> c
      | Undefined -> Misc.fatal_error "modality Undefined should not be zapped."
      | Exactly (_, m) ->
        let c = Mode.zap_to_floor m in
        Const.Meet_const c

    let to_const_exn = function
      | Const c -> c
      | Undefined | Exactly _ ->
        Misc.fatal_error "Got inferred modality but expected constant modality."

    let of_const c = Const c
  end

  module Value = struct
    type error =
      | Error : ('m, 'a, _) Value.axis * ('m, 'a) raw Solver.error -> error

    type equate_error = equate_step * error

    module Const = struct
      module Monadic = Monadic.Const
      module Comonadic = Comonadic.Const

      type t = (Monadic.t, Comonadic.t) monadic_comonadic

      let id = { monadic = Monadic.id; comonadic = Comonadic.id }

      let sub t0 t1 : (unit, error) Result.t =
        match Monadic.sub t0.monadic t1.monadic with
        | Error (Error (ax, e)) -> Error (Error (Monadic ax, e))
        | Ok () -> (
          match Comonadic.sub t0.comonadic t1.comonadic with
          | Ok () -> Ok ()
          | Error (Error (ax, e)) -> Error (Error (Comonadic ax, e)))

      let equate = equate_from_submode' sub

      let apply_left t { monadic; comonadic } =
        let monadic = Monadic.apply_left t.monadic monadic in
        let comonadic = Comonadic.apply_left t.comonadic comonadic in
        { monadic; comonadic }

      let apply_right t { monadic; comonadic } =
        let monadic = Monadic.apply_right t.monadic monadic in
        let comonadic = Comonadic.apply_right t.comonadic comonadic in
        { monadic; comonadic }

      let compose ~then_:(Atom (ax, a)) t =
        match ax with
        | Monadic ax ->
          let monadic = Monadic.compose ax a t.monadic in
          { t with monadic }
        | Comonadic ax ->
          let comonadic = Comonadic.compose ax a t.comonadic in
          { t with comonadic }

      let concat ~then_ t =
        let monadic = Monadic.concat ~then_:then_.monadic t.monadic in
        let comonadic = Comonadic.concat ~then_:then_.comonadic t.comonadic in
        { monadic; comonadic }

      let singleton a = compose ~then_:a id

      let to_list { monadic; comonadic } =
        Comonadic.to_list comonadic @ Monadic.to_list monadic
    end

    type t = (Monadic.t, Comonadic.t) monadic_comonadic

    let id : t = { monadic = Monadic.id; comonadic = Comonadic.id }

    let undefined : t = { monadic = Undefined; comonadic = Comonadic.Undefined }

    let apply t { monadic; comonadic } =
      let monadic = Monadic.apply t.monadic monadic in
      let comonadic = Comonadic.apply t.comonadic comonadic in
      { monadic; comonadic }

    let sub_log t0 t1 ~log : (unit, error) Result.t =
      match Monadic.sub_log t0.monadic t1.monadic ~log with
      | Error (Error (ax, e)) -> Error (Error (Monadic ax, e))
      | Ok () -> (
        match Comonadic.sub_log t0.comonadic t1.comonadic ~log with
        | Ok () -> Ok ()
        | Error (Error (ax, e)) -> Error (Error (Comonadic ax, e)))

    let sub l r = try_with_log (sub_log l r)

    let equate m0 m1 = try_with_log (equate_from_submode sub_log m0 m1)

    let print ppf ({ monadic; comonadic } : t) =
      Format.fprintf ppf "%a;%a" Monadic.print monadic Comonadic.print comonadic

    let infer ~md_mode ~mode : t =
      let comonadic =
        Comonadic.infer ~md_mode:md_mode.comonadic ~mode:mode.comonadic
      in
      let monadic = Monadic.infer ~md_mode:md_mode.monadic ~mode:mode.monadic in
      { monadic; comonadic }

    let zap_to_id t =
      let { monadic; comonadic } = t in
      let comonadic = Comonadic.zap_to_id comonadic in
      let monadic = Monadic.zap_to_id monadic in
      { monadic; comonadic }

    let zap_to_floor t =
      let { monadic; comonadic } = t in
      let comonadic = Comonadic.zap_to_floor comonadic in
      let monadic = Monadic.zap_to_floor monadic in
      { monadic; comonadic }

    let to_const_exn t =
      let { monadic; comonadic } = t in
      let comonadic = Comonadic.to_const_exn comonadic in
      let monadic = Monadic.to_const_exn monadic in
      { monadic; comonadic }

    let of_const { monadic; comonadic } =
      let comonadic = Comonadic.of_const comonadic in
      let monadic = Monadic.of_const monadic in
      { monadic; comonadic }

    let max =
      let monadic = Monadic.max in
      let comonadic = Comonadic.max in
      { monadic; comonadic }
  end
end
