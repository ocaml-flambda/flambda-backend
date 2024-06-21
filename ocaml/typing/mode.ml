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

(* warn on fragile matches *)
[@@@warning "+4"]

open Solver
open Solver_intf
open Mode_intf

type nonrec allowed = allowed

type nonrec disallowed = disallowed

type nonrec equate_step = equate_step

module type BiHeyting = sig
  (** Extend the [Lattice] interface with operations of bi-Heyting algebras *)

  include Lattice

  (** [imply c] is the right adjoint of [meet c]; That is, for any [a] and [b],
      [meet c a <= b] iff [a <= imply c b] *)
  val imply : t -> t -> t

  (** [subtract _ c] is the left adjoint of [join c]. That is, for any [a] and [b],
      [subtract a c <= b] iff [a <= join c b] *)
  val subtract : t -> t -> t
end

(* Even though our lattices are all bi-heyting algebras, that knowledge is
   internal to this module. Externally they are seen as normal lattices. *)
module Lattices = struct
  module Opposite (L : BiHeyting) : BiHeyting with type t = L.t = struct
    type t = L.t

    let min = L.max

    let max = L.min

    let legacy = L.legacy

    let le a b = L.le b a

    let join = L.meet

    let meet = L.join

    let print = L.print

    let imply a b = L.subtract b a

    let subtract a b = L.imply b a
  end
  [@@inline]

  (* A lattice is total order, if for any [a] [b], [a <= b] or [b <= a].
     A total lattice has a bi-heyting structure given as follows. *)
  module Total (L : Lattice) : BiHeyting with type t := L.t = struct
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

  (* Make the type of [Locality] and [Regionality] below distinguishable,
     so that we can be sure [Comonadic_with] is applied correctly. *)
  module type Areality = sig
    include BiHeyting

    val _is_areality : unit
  end

  module Locality = struct
    type t =
      | Global
      | Local

    include Total (struct
      type nonrec t = t

      let min = Global

      let max = Local

      let legacy = Global

      let le a b =
        match a, b with Global, _ | _, Local -> true | Local, Global -> false

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

    let _is_areality = ()
  end

  module Regionality = struct
    type t =
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

      let print ppf = function
        | Global -> Format.fprintf ppf "global"
        | Regional -> Format.fprintf ppf "regional"
        | Local -> Format.fprintf ppf "local"
    end)

    let _is_areality = ()
  end

  module Uniqueness = struct
    type t =
      | Unique
      | Shared

    include Total (struct
      type nonrec t = t

      let min = Unique

      let max = Shared

      let legacy = Shared

      let le a b =
        match a, b with
        | Unique, _ | _, Shared -> true
        | Shared, Unique -> false

      let join a b =
        match a, b with
        | Shared, _ | _, Shared -> Shared
        | Unique, Unique -> Unique

      let meet a b =
        match a, b with
        | Unique, _ | _, Unique -> Unique
        | Shared, Shared -> Shared

      let print ppf = function
        | Shared -> Format.fprintf ppf "shared"
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
      | Uncontended

    include Total (struct
      type nonrec t = t

      let min = Uncontended

      let max = Contended

      let legacy = Uncontended

      let le a b =
        match a, b with
        | Uncontended, _ | _, Contended -> true
        | Contended, Uncontended -> false

      let join a b =
        match a, b with
        | Contended, _ | _, Contended -> Contended
        | Uncontended, Uncontended -> Uncontended

      let meet a b =
        match a, b with
        | Uncontended, _ | _, Uncontended -> Uncontended
        | Contended, Contended -> Contended

      let print ppf = function
        | Contended -> Format.fprintf ppf "contended"
        | Uncontended -> Format.fprintf ppf "uncontended"
    end)
  end

  module Contention_op = Opposite (Contention)

  type monadic = Uniqueness.t * Contention.t

  module Monadic = struct
    type t = monadic

    let min = Uniqueness.min, Contention.min

    let max = Uniqueness.max, Contention.max

    let legacy = Uniqueness.legacy, Contention.legacy

    let le (a0, a1) (b0, b1) = Uniqueness.le a0 b0 && Contention.le a1 b1

    let join (a0, a1) (b0, b1) = Uniqueness.join a0 b0, Contention.join a1 b1

    let meet (a0, a1) (b0, b1) = Uniqueness.meet a0 b0, Contention.meet a1 b1

    let imply (a0, a1) (b0, b1) = Uniqueness.imply a0 b0, Contention.imply a1 b1

    let subtract (a0, a1) (b0, b1) =
      Uniqueness.subtract a0 b0, Contention.subtract a1 b1

    let print ppf (a0, a1) =
      Format.fprintf ppf "%a,%a" Uniqueness.print a0 Contention.print a1
  end

  type 'areality comonadic_with = 'areality * Linearity.t * Portability.t

  module Comonadic_with (Areality : Areality) = struct
    type t = Areality.t comonadic_with

    let min = Areality.min, Linearity.min, Portability.min

    let max = Areality.max, Linearity.max, Portability.max

    let legacy = Areality.legacy, Linearity.legacy, Portability.legacy

    let le (a0, a1, a2) (b0, b1, b2) =
      Areality.le a0 b0 && Linearity.le a1 b1 && Portability.le a2 b2

    let join (a0, a1, a2) (b0, b1, b2) =
      Areality.join a0 b0, Linearity.join a1 b1, Portability.join a2 b2

    let meet (a0, a1, a2) (b0, b1, b2) =
      Areality.meet a0 b0, Linearity.meet a1 b1, Portability.meet a2 b2

    let imply (a0, a1, a2) (b0, b1, b2) =
      Areality.imply a0 b0, Linearity.imply a1 b1, Portability.imply a2 b2

    let subtract (a0, a1, a2) (b0, b1, b2) =
      ( Areality.subtract a0 b0,
        Linearity.subtract a1 b1,
        Portability.subtract a2 b2 )

    let print ppf (a0, a1, a2) =
      Format.fprintf ppf "%a,%a,%a" Areality.print a0 Linearity.print a1
        Portability.print a2
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

  let subtract : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.subtract a b
    | Regionality -> Regionality.subtract a b
    | Uniqueness_op -> Uniqueness_op.subtract a b
    | Contention_op -> Contention_op.subtract a b
    | Linearity -> Linearity.subtract a b
    | Portability -> Portability.subtract a b
    | Comonadic_with_locality -> Comonadic_with_locality.subtract a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.subtract a b
    | Monadic_op -> Monadic_op.subtract a b

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

  module Equal_obj = Magic_equal (struct
    type ('a, _, 'd) t = 'a obj constraint 'd = 'l * 'r

    let equal : type a b. a obj -> b obj -> (a, b) Misc.eq option =
     fun a b ->
      match a, b with
      | Locality, Locality -> Some Refl
      | Regionality, Regionality -> Some Refl
      | Uniqueness_op, Uniqueness_op -> Some Refl
      | Contention_op, Contention_op -> Some Refl
      | Linearity, Linearity -> Some Refl
      | Portability, Portability -> Some Refl
      | Monadic_op, Monadic_op -> Some Refl
      | Comonadic_with_locality, Comonadic_with_locality -> Some Refl
      | Comonadic_with_regionality, Comonadic_with_regionality -> Some Refl
      | ( ( Locality | Regionality | Uniqueness_op | Contention_op | Linearity
          | Portability | Monadic_op | Comonadic_with_locality
          | Comonadic_with_regionality ),
          _ ) ->
        None
  end)

  let eq_obj = Equal_obj.equal
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
      match ax, t with
      | Areality, (a, _, _) -> a
      | Linearity, (_, lin, _) -> lin
      | Portability, (_, _, s) -> s
      | Uniqueness, (uni, _) -> uni
      | Contention, (_, con) -> con

    let update : type p r. (p, r) t -> r -> p -> p =
     fun ax r t ->
      match ax, t with
      | Areality, (_, lin, portable) -> r, lin, portable
      | Linearity, (area, _, portable) -> area, r, portable
      | Portability, (area, lin, _) -> area, lin, r
      | Uniqueness, (_, con) -> r, con
      | Contention, (uni, _) -> uni, r
  end

  type ('a, 'b, 'd) morph =
    | Id : ('a, 'a, 'd) morph  (** identity morphism *)
    | Meet_with : 'a -> ('a, 'a, 'l * 'r) morph
        (** Meet the input with the parameter *)
    | Imply : 'a -> ('a, 'a, disallowed * 'd) morph
        (** The right adjoint of [Meet_with] *)
    | Join_with : 'a -> ('a, 'a, 'l * 'r) morph
        (** Join the input with the parameter *)
    | Subtract : 'a -> ('a, 'a, 'd * disallowed) morph
        (** The left adjoint of [Join_with] *)
    | Proj : 't obj * ('t, 'r_) Axis.t -> ('t, 'r_, 'l * 'r) morph
        (** Project from a product to an axis *)
    | Max_with : ('t, 'r_) Axis.t -> ('r_, 't, disallowed * 'r) morph
        (** Combine an axis with maxima along other axes *)
    | Min_with : ('t, 'r_) Axis.t -> ('r_, 't, 'l * disallowed) morph
        (** Combine an axis with minima along other axes *)
    | Map_comonadic :
        ('a0, 'a1, 'd) morph
        -> ('a0 comonadic_with, 'a1 comonadic_with, 'd) morph
        (** Lift an morphism on areality to a morphism on the comonadic fragment   *)
    | Monadic_to_comonadic_min
        : (Monadic_op.t, 'a comonadic_with, 'l * disallowed) morph
        (** Dualize the monadic fragment to the comonadic fragment. The areality is set to min. *)
    | Comonadic_to_monadic :
        'a comonadic_with obj
        -> ('a comonadic_with, Monadic_op.t, 'l * 'r) morph
        (** Dualize the comonadic fragment to the monadic fragment. The areality axis is ignored.  *)
    | Monadic_to_comonadic_max
        : (Monadic_op.t, 'a comonadic_with, disallowed * 'r) morph
        (** Dualize the monadic fragment to the comonadic fragment. The areality is set to max. *)
    (* Following is a chain of adjunction (complete and cannot extend in
       either direction) *)
    | Local_to_regional : (Locality.t, Regionality.t, 'l * disallowed) morph
        (** Maps local to regional, global to global *)
    | Regional_to_local : (Regionality.t, Locality.t, 'l * 'r) morph
        (** Maps regional to local, identity otherwise *)
    | Locality_as_regionality : (Locality.t, Regionality.t, 'l * 'r) morph
        (** Inject locality into regionality  *)
    | Regional_to_global : (Regionality.t, Locality.t, 'l * 'r) morph
        (** Maps regional to global, identity otherwise *)
    | Global_to_regional : (Locality.t, Regionality.t, disallowed * 'r) morph
        (** Maps global to regional, local to local *)
    | Compose : ('b, 'c, 'd) morph * ('a, 'b, 'd) morph -> ('a, 'c, 'd) morph
        (** Compoistion of two morphisms *)

  include Magic_allow_disallow (struct
    type ('a, 'b, 'd) sided = ('a, 'b, 'd) morph constraint 'd = 'l * 'r

    let rec allow_left :
        type a b l r. (a, b, allowed * r) morph -> (a, b, l * r) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Min_with ax -> Min_with ax
      | Meet_with c -> Meet_with c
      | Join_with c -> Join_with c
      | Subtract c -> Subtract c
      | Compose (f, g) ->
        let f = allow_left f in
        let g = allow_left g in
        Compose (f, g)
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Local_to_regional -> Local_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map_comonadic f ->
        let f = allow_left f in
        Map_comonadic f

    let rec allow_right :
        type a b l r. (a, b, l * allowed) morph -> (a, b, l * r) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Max_with ax -> Max_with ax
      | Join_with c -> Join_with c
      | Meet_with c -> Meet_with c
      | Imply c -> Imply c
      | Compose (f, g) ->
        let f = allow_right f in
        let g = allow_right g in
        Compose (f, g)
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Global_to_regional -> Global_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map_comonadic f ->
        let f = allow_right f in
        Map_comonadic f

    let rec disallow_left :
        type a b l r. (a, b, l * r) morph -> (a, b, disallowed * r) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Min_with ax -> Min_with ax
      | Max_with ax -> Max_with ax
      | Join_with c -> Join_with c
      | Subtract c -> Subtract c
      | Meet_with c -> Meet_with c
      | Imply c -> Imply c
      | Compose (f, g) ->
        let f = disallow_left f in
        let g = disallow_left g in
        Compose (f, g)
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Local_to_regional -> Local_to_regional
      | Global_to_regional -> Global_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map_comonadic f ->
        let f = disallow_left f in
        Map_comonadic f

    let rec disallow_right :
        type a b l r. (a, b, l * r) morph -> (a, b, l * disallowed) morph =
      function
      | Id -> Id
      | Proj (src, ax) -> Proj (src, ax)
      | Min_with ax -> Min_with ax
      | Max_with ax -> Max_with ax
      | Join_with c -> Join_with c
      | Subtract c -> Subtract c
      | Meet_with c -> Meet_with c
      | Imply c -> Imply c
      | Compose (f, g) ->
        let f = disallow_right f in
        let g = disallow_right g in
        Compose (f, g)
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic a -> Comonadic_to_monadic a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Local_to_regional -> Local_to_regional
      | Global_to_regional -> Global_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Regional_to_global -> Regional_to_global
      | Map_comonadic f ->
        let f = disallow_right f in
        Map_comonadic f
  end)

  let set_areality : type a0 a1. a1 -> a0 comonadic_with -> a1 comonadic_with =
   fun r (_, lin, portable) -> r, lin, portable

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

  let comonadic_with_obj : type a. a obj -> a comonadic_with obj =
   fun a0 ->
    match a0 with
    | Locality -> Comonadic_with_locality
    | Regionality -> Comonadic_with_regionality
    | Uniqueness_op | Linearity | Monadic_op | Comonadic_with_regionality
    | Comonadic_with_locality | Contention_op | Portability ->
      assert false

  let rec src : type a b d. b obj -> (a, b, d) morph -> a obj =
   fun dst f ->
    match f with
    | Id -> dst
    | Proj (src, _) -> src
    | Max_with ax -> proj_obj ax dst
    | Min_with ax -> proj_obj ax dst
    | Join_with _ -> dst
    | Meet_with _ -> dst
    | Imply _ -> dst
    | Subtract _ -> dst
    | Compose (f, g) ->
      let mid = src dst f in
      src mid g
    | Monadic_to_comonadic_min -> Monadic_op
    | Comonadic_to_monadic src -> src
    | Monadic_to_comonadic_max -> Monadic_op
    | Local_to_regional -> Locality
    | Locality_as_regionality -> Locality
    | Global_to_regional -> Locality
    | Regional_to_local -> Regionality
    | Regional_to_global -> Regionality
    | Map_comonadic f ->
      let dst0 = proj_obj Areality dst in
      let src0 = src dst0 f in
      comonadic_with_obj src0

  module Equal_morph = Magic_equal (struct
    type ('a, 'b, 'd) t = ('a, 'b, 'd) morph constraint 'd = 'l * 'r

    let rec equal :
        type a0 l0 r0 a1 b l1 r1.
        (a0, b, l0 * r0) morph ->
        (a1, b, l1 * r1) morph ->
        (a0, a1) Misc.eq option =
     fun f0 f1 ->
      match f0, f1 with
      | Id, Id -> Some Refl
      | Proj (src0, ax0), Proj (src1, ax1) -> (
        match eq_obj src0 src1 with
        | Some Refl -> (
          match Axis.eq ax0 ax1 with None -> None | Some Refl -> Some Refl)
        | None -> None)
      | Max_with ax0, Max_with ax1 -> (
        match Axis.eq ax0 ax1 with Some Refl -> Some Refl | None -> None)
      | Min_with ax0, Min_with ax1 -> (
        match Axis.eq ax0 ax1 with Some Refl -> Some Refl | None -> None)
      | Meet_with c0, Meet_with c1 ->
        (* This polymorphic equality is correct only if runtime representation
           uniquely identifies a constant, which could be false. For example,
           the lattice of rational number would be represented as the tuple of
           numerator and denominator, and (9,4) and (18, 8) means the same
           thing. However, even in that case, it's not unsound, as [eq_morph] is
           not requird to be complete: i.e., it's allowed to return [None] when
           it should return [Some]. It would cause duplication but not error. *)
        if c0 = c1 then Some Refl else None
      | Join_with c0, Join_with c1 -> if c0 = c1 then Some Refl else None
      | Imply c0, Imply c1 -> if c0 = c1 then Some Refl else None
      | Subtract c0, Subtract c1 -> if c0 = c1 then Some Refl else None
      | Monadic_to_comonadic_min, Monadic_to_comonadic_min -> Some Refl
      | Comonadic_to_monadic a0, Comonadic_to_monadic a1 -> (
        match eq_obj a0 a1 with None -> None | Some Refl -> Some Refl)
      | Monadic_to_comonadic_max, Monadic_to_comonadic_max -> Some Refl
      | Local_to_regional, Local_to_regional -> Some Refl
      | Locality_as_regionality, Locality_as_regionality -> Some Refl
      | Global_to_regional, Global_to_regional -> Some Refl
      | Regional_to_local, Regional_to_local -> Some Refl
      | Regional_to_global, Regional_to_global -> Some Refl
      | Compose (f0, g0), Compose (f1, g1) -> (
        match equal f0 f1 with
        | None -> None
        | Some Refl -> (
          match equal g0 g1 with None -> None | Some Refl -> Some Refl))
      | Map_comonadic f, Map_comonadic g -> (
        match equal f g with Some Refl -> Some Refl | None -> None)
      | ( ( Id | Proj _ | Max_with _ | Min_with _ | Meet_with _ | Join_with _
          | Monadic_to_comonadic_min | Comonadic_to_monadic _
          | Monadic_to_comonadic_max | Local_to_regional
          | Locality_as_regionality | Global_to_regional | Regional_to_local
          | Regional_to_global | Compose _ | Map_comonadic _ | Imply _
          | Subtract _ ),
          _ ) ->
        None
  end)

  let eq_morph = Equal_morph.equal

  let rec print_morph :
      type a b d. b obj -> Format.formatter -> (a, b, d) morph -> unit =
   fun dst ppf -> function
    | Id -> Format.fprintf ppf "id"
    | Join_with c -> Format.fprintf ppf "join_%a" (print dst) c
    | Meet_with c -> Format.fprintf ppf "meet_%a" (print dst) c
    | Imply c -> Format.fprintf ppf "imply_%a" (print dst) c
    | Subtract c -> Format.fprintf ppf "subtract_%a" (print dst) c
    | Proj (_, ax) -> Format.fprintf ppf "proj_%a" Axis.print ax
    | Max_with ax -> Format.fprintf ppf "max_with_%a" Axis.print ax
    | Min_with ax -> Format.fprintf ppf "min_with_%a" Axis.print ax
    | Map_comonadic f ->
      let dst0 = proj_obj Areality dst in
      Format.fprintf ppf "map_comonadic(%a)" (print_morph dst0) f
    | Monadic_to_comonadic_min -> Format.fprintf ppf "monadic_to_comonadic_min"
    | Comonadic_to_monadic _ -> Format.fprintf ppf "comonadic_to_monadic"
    | Monadic_to_comonadic_max -> Format.fprintf ppf "monadic_to_comonadic_max"
    | Local_to_regional -> Format.fprintf ppf "local_to_regional"
    | Regional_to_local -> Format.fprintf ppf "regional_to_local"
    | Locality_as_regionality -> Format.fprintf ppf "locality_as_regionality"
    | Regional_to_global -> Format.fprintf ppf "regional_to_global"
    | Global_to_regional -> Format.fprintf ppf "global_to_regional"
    | Compose (f0, f1) ->
      let mid = src dst f0 in
      Format.fprintf ppf "%a ∘ %a" (print_morph dst) f0 (print_morph mid) f1

  let id = Id

  let linear_to_unique = function
    | Linearity.Many -> Uniqueness.Shared
    | Linearity.Once -> Uniqueness.Unique

  let unique_to_linear = function
    | Uniqueness.Unique -> Linearity.Once
    | Uniqueness.Shared -> Linearity.Many

  let portable_to_contended = function
    | Portability.Portable -> Contention.Contended
    | Portability.Nonportable -> Contention.Uncontended

  let contended_to_portable = function
    | Contention.Contended -> Portability.Portable
    | Contention.Uncontended -> Portability.Nonportable

  let local_to_regional = function
    | Locality.Global -> Regionality.Global
    | Locality.Local -> Regionality.Regional

  let regional_to_local = function
    | Regionality.Local -> Locality.Local
    | Regionality.Regional -> Locality.Local
    | Regionality.Global -> Locality.Global

  let locality_as_regionality = function
    | Locality.Local -> Regionality.Local
    | Locality.Global -> Regionality.Global

  let regional_to_global = function
    | Regionality.Local -> Locality.Local
    | Regionality.Regional -> Locality.Global
    | Regionality.Global -> Locality.Global

  let global_to_regional = function
    | Locality.Local -> Regionality.Local
    | Locality.Global -> Regionality.Regional

  let min_with dst ax a = Axis.update ax a (min dst)

  let max_with dst ax a = Axis.update ax a (max dst)

  let monadic_to_comonadic_min :
      type a. a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj (uniqueness, contention) ->
    match obj with
    | Comonadic_with_locality ->
      ( Locality.min,
        unique_to_linear uniqueness,
        contended_to_portable contention )
    | Comonadic_with_regionality ->
      ( Regionality.min,
        unique_to_linear uniqueness,
        contended_to_portable contention )

  let comonadic_to_monadic :
      type a. a comonadic_with obj -> a comonadic_with -> Monadic_op.t =
   fun obj (_, linearity, portability) ->
    match obj with
    | Comonadic_with_locality ->
      linear_to_unique linearity, portable_to_contended portability
    | Comonadic_with_regionality ->
      linear_to_unique linearity, portable_to_contended portability

  let monadic_to_comonadic_max :
      type a. a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj (uniqueness, contention) ->
    match obj with
    | Comonadic_with_locality ->
      ( Locality.max,
        unique_to_linear uniqueness,
        contended_to_portable contention )
    | Comonadic_with_regionality ->
      ( Regionality.max,
        unique_to_linear uniqueness,
        contended_to_portable contention )

  let rec apply : type a b d. b obj -> (a, b, d) morph -> a -> b =
   fun dst f a ->
    match f with
    | Compose (f, g) ->
      let mid = src dst f in
      let g' = apply mid g in
      let f' = apply dst f in
      f' (g' a)
    | Id -> a
    | Proj (_, ax) -> Axis.proj ax a
    | Max_with ax -> max_with dst ax a
    | Min_with ax -> min_with dst ax a
    | Meet_with c -> meet dst c a
    | Join_with c -> join dst c a
    | Imply c -> imply dst c a
    | Subtract c -> subtract dst a c
    | Monadic_to_comonadic_min -> monadic_to_comonadic_min dst a
    | Comonadic_to_monadic src -> comonadic_to_monadic src a
    | Monadic_to_comonadic_max -> monadic_to_comonadic_max dst a
    | Local_to_regional -> local_to_regional a
    | Regional_to_local -> regional_to_local a
    | Locality_as_regionality -> locality_as_regionality a
    | Regional_to_global -> regional_to_global a
    | Global_to_regional -> global_to_regional a
    | Map_comonadic f ->
      let dst0 = proj_obj Areality dst in
      let a0 = Axis.proj Areality a in
      set_areality (apply dst0 f a0) a

  (** Compose m0 after m1. Returns [Some f] if the composition can be
    represented by [f] instead of [Compose m0 m1]. [None] otherwise. *)
  let rec maybe_compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph option =
   fun dst m0 m1 ->
    let is_max c = le dst (max dst) c in
    let is_min c = le dst c (min dst) in
    let is_mid_max c =
      let mid = src dst m0 in
      le mid (max mid) c
    in
    let is_mid_min c =
      let mid = src dst m0 in
      le mid c (min mid)
    in
    match m0, m1 with
    | Id, m -> Some m
    | m, Id -> Some m
    | Meet_with c0, Meet_with c1 -> Some (Meet_with (meet dst c0 c1))
    | Join_with c0, Join_with c1 -> Some (Join_with (join dst c0 c1))
    | Imply c0, Imply c1 -> Some (Imply (meet dst c0 c1))
    | Subtract c0, Subtract c1 -> Some (Subtract (join dst c0 c1))
    | Imply c0, Join_with c1 when le dst c0 c1 -> Some (Join_with (max dst))
    | Imply c0, Meet_with c1 when le dst c0 c1 -> Some (Imply c0)
    | Subtract c0, Meet_with c1 when le dst c1 c0 -> Some (Meet_with (min dst))
    | Subtract c0, Join_with c1 when le dst c1 c0 -> Some (Subtract c0)
    | Meet_with c0, m1 when is_max c0 -> Some m1
    | Join_with c0, m1 when is_min c0 -> Some m1
    | Imply c0, m1 when is_max c0 -> Some m1
    | Subtract c0, m1 when is_min c0 -> Some m1
    | m1, Meet_with c0 when is_mid_max c0 -> Some m1
    | m1, Join_with c0 when is_mid_min c0 -> Some m1
    | m1, Imply c0 when is_mid_max c0 -> Some m1
    | m1, Subtract c0 when is_mid_min c0 -> Some m1
    | Compose (f0, f1), g -> (
      let mid = src dst f0 in
      match maybe_compose mid f1 g with
      | Some m -> Some (compose dst f0 m)
      (* the check needed to prevent infinite loop *)
      | None -> None)
    | f, Compose (g0, g1) -> (
      match maybe_compose dst f g0 with
      | Some m -> Some (compose dst m g1)
      | None -> None)
    | Proj (mid, ax), Meet_with c ->
      Some (compose dst (Meet_with (Axis.proj ax c)) (Proj (mid, ax)))
    | Proj (mid, ax), Join_with c ->
      Some (compose dst (Join_with (Axis.proj ax c)) (Proj (mid, ax)))
    | Proj (_, ax0), Max_with ax1 -> (
      match Axis.eq ax0 ax1 with None -> None | Some Refl -> Some Id)
    | Proj (_, ax0), Min_with ax1 -> (
      match Axis.eq ax0 ax1 with None -> None | Some Refl -> Some Id)
    | Proj (mid, ax), Map_comonadic f -> (
      let src' = src mid m1 in
      match ax with
      | Areality -> Some (compose dst f (Proj (src', Areality)))
      | Linearity -> Some (Proj (src', Linearity))
      | Portability -> Some (Proj (src', Portability)))
    | Proj _, Monadic_to_comonadic_min -> None
    | Proj _, Monadic_to_comonadic_max -> None
    | Proj _, Comonadic_to_monadic _ -> None
    | Map_comonadic f, Map_comonadic g ->
      let dst0 = proj_obj Areality dst in
      Some (Map_comonadic (compose dst0 f g))
    | Regional_to_local, Local_to_regional -> Some Id
    | Regional_to_local, Global_to_regional -> Some (Join_with Locality.Local)
    | Regional_to_local, Locality_as_regionality -> Some Id
    | Regional_to_local, Meet_with c ->
      Some (compose dst (Meet_with (regional_to_local c)) Regional_to_local)
    | Regional_to_local, Join_with c ->
      Some (compose dst (Join_with (regional_to_local c)) Regional_to_local)
    | Regional_to_global, Join_with c ->
      Some (compose dst (Join_with (regional_to_global c)) Regional_to_global)
    | Regional_to_global, Meet_with c ->
      Some (compose dst (Meet_with (regional_to_global c)) Regional_to_global)
    | Local_to_regional, Meet_with c ->
      Some (compose dst (Meet_with (local_to_regional c)) Local_to_regional)
    | Local_to_regional, Join_with c ->
      Some (compose dst (Join_with (local_to_regional c)) Local_to_regional)
    | Global_to_regional, Meet_with c ->
      Some (compose dst (Meet_with (global_to_regional c)) Global_to_regional)
    | Global_to_regional, Join_with c ->
      Some (compose dst (Join_with (global_to_regional c)) Global_to_regional)
    | Locality_as_regionality, Meet_with c ->
      Some
        (compose dst
           (Meet_with (locality_as_regionality c))
           Locality_as_regionality)
    | Locality_as_regionality, Join_with c ->
      Some
        (compose dst
           (Join_with (locality_as_regionality c))
           Locality_as_regionality)
    | Map_comonadic f, Join_with c ->
      let dst0 = proj_obj Areality dst in
      let areality = Axis.proj Areality c in
      Some
        (compose dst
           (Join_with (set_areality (min dst0) c))
           (Map_comonadic (compose dst0 f (Join_with areality))))
    | Map_comonadic f, Meet_with c ->
      let dst0 = proj_obj Areality dst in
      let areality = Axis.proj Areality c in
      Some
        (compose dst
           (Meet_with (set_areality (max dst0) c))
           (Map_comonadic (compose dst0 f (Meet_with areality))))
    | Map_comonadic f, Imply c ->
      let dst0 = proj_obj Areality dst in
      let areality = Axis.proj Areality c in
      Some
        (compose dst
           (Imply (set_areality (max dst0) c))
           (Map_comonadic (compose dst0 f (Imply areality))))
    | Map_comonadic f, Subtract c ->
      let dst0 = proj_obj Areality dst in
      let areality = Axis.proj Areality c in
      Some
        (compose dst
           (Subtract (set_areality (min dst0) c))
           (Map_comonadic (compose dst0 f (Subtract areality))))
    | Regional_to_global, Locality_as_regionality -> Some Id
    | Regional_to_global, Local_to_regional -> Some (Meet_with Locality.Global)
    | Local_to_regional, Regional_to_local -> None
    | Local_to_regional, Regional_to_global -> None
    | Locality_as_regionality, Regional_to_local -> None
    | Locality_as_regionality, Regional_to_global -> None
    | Global_to_regional, Regional_to_local -> None
    | Regional_to_global, Global_to_regional -> Some Id
    | Global_to_regional, Regional_to_global -> None
    | Min_with _, _ -> None
    | Max_with _, _ -> None
    | _, Meet_with _ -> None
    | Meet_with _, _ -> None
    | _, Join_with _ -> None
    | Join_with _, _ -> None
    | _, Imply _ -> None
    | Imply _, _ -> None
    | _, Subtract _ -> None
    | Subtract _, _ -> None
    | _, Proj _ -> None
    | Map_comonadic _, _ -> None
    | Monadic_to_comonadic_min, _ -> None
    | Monadic_to_comonadic_max, _ -> None
    | Comonadic_to_monadic _, _ -> None
    | ( Proj _,
        ( Local_to_regional | Regional_to_local | Locality_as_regionality
        | Regional_to_global | Global_to_regional ) ) ->
      .
    | ( ( Local_to_regional | Regional_to_local | Locality_as_regionality
        | Regional_to_global | Global_to_regional ),
        Min_with _ ) ->
      .
    | ( ( Local_to_regional | Regional_to_local | Locality_as_regionality
        | Regional_to_global | Global_to_regional ),
        Max_with _ ) ->
      .

  and compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph =
   fun dst f g ->
    match maybe_compose dst f g with Some m -> m | None -> Compose (f, g)

  let rec left_adjoint :
      type a b l.
      b obj -> (a, b, l * allowed) morph -> (b, a, allowed * disallowed) morph =
   fun dst f ->
    match f with
    | Id -> Id
    | Proj (_, ax) -> Min_with ax
    | Max_with ax -> Proj (dst, ax)
    | Compose (f, g) ->
      let mid = src dst f in
      let f' = left_adjoint dst f in
      let g' = left_adjoint mid g in
      Compose (g', f')
    | Join_with c -> Subtract c
    | Meet_with _c ->
      (* The downward closure of [Meet_with c]'s image is all [x <= c].
         For those, [x <= meet c y] is equivalent to [x <= y]. *)
      Id
    | Imply c -> Meet_with c
    | Comonadic_to_monadic _ -> Monadic_to_comonadic_min
    | Monadic_to_comonadic_max -> Comonadic_to_monadic dst
    | Global_to_regional -> Regional_to_global
    | Regional_to_global -> Locality_as_regionality
    | Locality_as_regionality -> Regional_to_local
    | Regional_to_local -> Local_to_regional
    | Map_comonadic f ->
      let dst0 = proj_obj Areality dst in
      let f' = left_adjoint dst0 f in
      Map_comonadic f'

  and right_adjoint :
      type a b r.
      b obj -> (a, b, allowed * r) morph -> (b, a, disallowed * allowed) morph =
   fun dst f ->
    match f with
    | Id -> Id
    | Proj (_, ax) -> Max_with ax
    | Min_with ax -> Proj (dst, ax)
    | Compose (f, g) ->
      let mid = src dst f in
      let f' = right_adjoint dst f in
      let g' = right_adjoint mid g in
      Compose (g', f')
    | Meet_with c -> Imply c
    | Subtract c -> Join_with c
    | Join_with _c ->
      (* The upward closure of [Join_with c]'s image is all [x >= c].
         For those, [join c y <= x] is equivalent to [y <= x]. *)
      Id
    | Comonadic_to_monadic _ -> Monadic_to_comonadic_max
    | Monadic_to_comonadic_min -> Comonadic_to_monadic dst
    | Local_to_regional -> Regional_to_local
    | Regional_to_local -> Locality_as_regionality
    | Locality_as_regionality -> Regional_to_global
    | Regional_to_global -> Global_to_regional
    | Map_comonadic f ->
      let dst0 = proj_obj Areality dst in
      let f' = right_adjoint dst0 f in
      Map_comonadic f'
end

module C = Lattices_mono
module S = Solvers_polarized (C)

type monadic = C.monadic

type 'a comonadic_with = 'a C.comonadic_with

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

  module Obj = struct
    type const = Const.t

    (* the negation of Uniqueness_op gives us the proper uniqueness *)
    module Solver = S.Negative

    let obj = C.Uniqueness_op
  end

  include Common (Obj)

  let shared = of_const Shared

  let unique = of_const Unique

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

module Contention = struct
  module Const = C.Contention

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

let regional_to_local m =
  S.Positive.via_monotone Locality.Obj.obj C.Regional_to_local m

let locality_as_regionality m =
  S.Positive.via_monotone Regionality.Obj.obj C.Locality_as_regionality m

let regional_to_global m =
  S.Positive.via_monotone Locality.Obj.obj C.Regional_to_global m

module type Areality = sig
  module Const : C.Areality

  module Obj : Obj with type const = Const.t and module Solver = S.Positive

  val zap_to_legacy : (Const.t, allowed * 'r) Obj.Solver.mode -> Const.t
end

module Comonadic_with (Areality : Areality) = struct
  module Obj = struct
    type const = Areality.Obj.const C.comonadic_with

    module Solver = S.Positive

    let obj = C.comonadic_with_obj Areality.Obj.obj
  end

  include Common (Obj)

  type error = Error : (Obj.const, 'a) C.Axis.t * 'a Solver.error -> error

  type equate_error = equate_step * error

  open Obj

  let proj_obj ax = C.proj_obj ax obj

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

    let max_with ax c = Axis.update ax c (C.max obj)

    let print_axis ax ppf a =
      let obj = proj_obj ax in
      C.print obj ppf a
  end

  let proj ax m = Solver.via_monotone (proj_obj ax) (Proj (Obj.obj, ax)) m

  let meet_const c m = Solver.via_monotone obj (Meet_with c) m

  let join_const c m = Solver.via_monotone obj (Join_with c) m

  let min_with ax m =
    Solver.via_monotone Obj.obj (Min_with ax) (Solver.disallow_right m)

  let max_with ax m =
    Solver.via_monotone Obj.obj (Max_with ax) (Solver.disallow_left m)

  let join_with ax c m = join_const (C.min_with Obj.obj ax c) m

  let meet_with ax c m = meet_const (C.max_with Obj.obj ax c) m

  let zap_to_legacy m =
    let areality = proj Areality m |> Areality.zap_to_legacy in
    let linearity = proj Linearity m |> Linearity.zap_to_legacy in
    let portability = proj Portability m |> Portability.zap_to_legacy in
    areality, linearity, portability

  let imply c m = Solver.via_monotone obj (Imply c) (Solver.disallow_left m)

  let legacy = of_const Const.legacy

  let axis_of_error { left = area0, lin0, port0; right = area1, lin1, port1 } :
      error =
    if Areality.Const.le area0 area1
    then
      if Linearity.Const.le lin0 lin1
      then
        if Portability.Const.le port0 port1
        then assert false
        else Error (Portability, { left = port0; right = port1 })
      else Error (Linearity, { left = lin0; right = lin1 })
    else Error (Areality, { left = area0; right = area1 })

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

  open Obj

  let proj_obj ax = C.proj_obj ax obj

  module Const = struct
    include C.Monadic

    (* CR zqian: The flipping logic leaking to here is bad. Refactoring needed. *)

    (* Monadic fragment is flipped, so are the following definitions. *)
    let min_with ax c = Axis.update ax c (C.max obj)

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

  let proj ax m = Solver.via_monotone (proj_obj ax) (Proj (Obj.obj, ax)) m

  (* The monadic fragment is inverted. Most of the inversion logic is taken care
     by [Solver_polarized], but some remain, such as the [Min_with] below which
     is inverted from [Max_with]. *)

  let meet_const c m = Solver.via_monotone obj (Join_with c) m

  let join_const c m = Solver.via_monotone obj (Meet_with c) m

  let max_with ax m =
    Solver.via_monotone Obj.obj (Min_with ax) (Solver.disallow_left m)

  let min_with ax m =
    Solver.via_monotone Obj.obj (Max_with ax) (Solver.disallow_right m)

  let join_with ax c m = join_const (C.max_with Obj.obj ax c) m

  let meet_with ax c m = meet_const (C.min_with Obj.obj ax c) m

  let imply c m = Solver.via_monotone obj (Subtract c) (Solver.disallow_left m)

  let subtract c m = Solver.via_monotone obj (Imply c) (Solver.disallow_right m)

  let zap_to_legacy m =
    let uniqueness = proj Uniqueness m |> Uniqueness.zap_to_legacy in
    let contention = proj Contention m |> Contention.zap_to_legacy in
    uniqueness, contention

  let legacy = of_const Const.legacy

  let axis_of_error { left = uni0, con0; right = uni1, con1 } : error =
    if Uniqueness.Const.le uni0 uni1
    then
      if Contention.Const.le con0 con1
      then assert false
      else Error (Contention, { left = con0; right = con1 })
    else Error (Uniqueness, { left = uni0; right = uni1 })

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
    let monadic = uniqueness, contention in
    let comonadic = areality, linearity, portability in
    { comonadic; monadic }

  let merge { comonadic; monadic } =
    let areality, linearity, portability = comonadic in
    let uniqueness, contention = monadic in
    { areality; linearity; portability; uniqueness; contention }

  let print ?verbose () ppf { monadic; comonadic } =
    Format.fprintf ppf "%a,%a"
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
      Comonadic.le m0.comonadic m1.comonadic && Monadic.le m0.monadic m1.monadic

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
             (C.comonadic_with_obj Areality.Obj.obj)
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

  let join_with_monadic ax c { monadic; comonadic } =
    let monadic = Monadic.join_with ax c monadic in
    { monadic; comonadic }

  let join_with_comonadic ax c { monadic; comonadic } =
    let comonadic = Comonadic.join_with ax c comonadic in
    { comonadic; monadic }

  let join_with : type m a d l r. (m, a, d) axis -> a -> (l * r) t -> (l * r) t
      =
   fun ax c m ->
    match ax with
    | Monadic ax -> join_with_monadic ax c m
    | Comonadic ax -> join_with_comonadic ax c m

  let meet_with_monadic ax c { monadic; comonadic } =
    let monadic = Monadic.meet_with ax c monadic in
    { monadic; comonadic }

  let meet_with_comonadic ax c { monadic; comonadic } =
    let comonadic = Comonadic.meet_with ax c comonadic in
    { comonadic; monadic }

  let meet_with : type m a d l r. (m, a, d) axis -> a -> (l * r) t -> (l * r) t
      =
   fun ax c m ->
    match ax with
    | Monadic ax -> meet_with_monadic ax c m
    | Comonadic ax -> meet_with_comonadic ax c m

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

  let comonadic_to_monadic m =
    S.Negative.via_antitone Monadic.Obj.obj
      (Comonadic_to_monadic Comonadic.Obj.obj) m

  let monadic_to_comonadic_min m =
    S.Positive.via_antitone Comonadic.Obj.obj Monadic_to_comonadic_min
      (Monadic.disallow_left m)

  let meet_const c { comonadic; monadic } =
    let c = split c in
    let comonadic = Comonadic.meet_const c.comonadic comonadic in
    let monadic = Monadic.meet_const c.monadic monadic in
    { monadic; comonadic }

  let imply c { comonadic; monadic } =
    let c = split c in
    let comonadic = Comonadic.imply c.comonadic comonadic in
    let monadic = Monadic.imply c.monadic monadic in
    { monadic; comonadic }

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
  let alloc_as_value
      ({ areality; linearity; portability; uniqueness; contention } :
        Alloc.Const.t) : Value.Const.t =
    let areality = C.locality_as_regionality areality in
    { areality; linearity; portability; uniqueness; contention }
end

let alloc_as_value m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Value.Comonadic.Obj.obj
      (Map_comonadic Locality_as_regionality) comonadic
  in
  { comonadic; monadic }

let alloc_to_value_l2r m =
  let { comonadic; monadic } = Alloc.disallow_right m in
  let comonadic =
    S.Positive.via_monotone Value.Comonadic.Obj.obj
      (Map_comonadic Local_to_regional) comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2g : type l r. (l * r) Value.t -> (l * r) Alloc.t =
 fun m ->
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Alloc.Comonadic.Obj.obj
      (Map_comonadic Regional_to_global) comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2l m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Positive.via_monotone Alloc.Comonadic.Obj.obj
      (Map_comonadic Regional_to_local) comonadic
  in
  { comonadic; monadic }

module Modality = struct
  type ('m, 'a) raw =
    | Meet_with : 'a -> (('a, 'l * 'r) mode_comonadic, 'a) raw
    | Join_with : 'a -> (('a, 'l * 'r) mode_monadic, 'a) raw

  type t = Atom : ('m, 'a, _) Value.axis * ('m, 'a) raw -> t

  let is_id (Atom (ax, a)) =
    match a with
    | Join_with c -> Value.Const.le_axis ax c (Value.Const.min_axis ax)
    | Meet_with c -> Value.Const.le_axis ax (Value.Const.max_axis ax) c

  let print ppf = function
    | Atom (ax, Join_with c) ->
      Format.fprintf ppf "join_with(%a)" (C.print (Value.proj_obj ax)) c
    | Atom (ax, Meet_with c) ->
      Format.fprintf ppf "meet_with(%a)" (C.print (Value.proj_obj ax)) c

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
              (Error (ax, { left = Join_with left; right = Join_with right }))

      let compose :
          type a l r. a axis -> ((a, l * r) mode_monadic, a) raw -> t -> t =
       fun ax a t ->
        match a, t with
        | Join_with c0, Join_const c ->
          Join_const (Mode.Const.join (Mode.Const.min_with ax c0) c)
        | Meet_with _, Join_const _ -> assert false

      let apply : type l r. t -> (l * r) Mode.t -> (l * r) Mode.t =
       fun t x -> match t with Join_const c -> Mode.join_const c x

      let to_list = function
        | Join_const c ->
          [ (let ax : _ Axis.t = Uniqueness in
             Atom (Monadic ax, Join_with (Axis.proj ax c)));
            (let ax : _ Axis.t = Contention in
             Atom (Monadic ax, Join_with (Axis.proj ax c))) ]

      let print ppf = function
        | Join_const c -> Format.fprintf ppf "join_const(%a)" Mode.Const.print c
    end

    type t =
      | Const of Const.t
      | Diff of Mode.lr * Mode.l
      | Undefined

    let sub_log left right ~log : (unit, error) Result.t =
      match left, right with
      | Const c0, Const c1 -> Const.sub c0 c1
      | Diff (mm, m), Const (Join_const c) -> (
        (* Check that for any x >= mm, join(x, m) <= join(x, c).
           By definition of join, equivalent to check m <= join(x, c)
           Since x >= mm, equivalent to check m <= join(mm, c). *)
        match Mode.submode_log m (Mode.join_const c mm) ~log with
        | Ok () -> Ok ()
        | Error (Error (ax, { left; _ })) ->
          Error
            (Error
               ( ax,
                 { left = Join_with left; right = Join_with (Axis.proj ax c) }
               )))
      | Diff _, Diff _ ->
        (* [m1] is a left mode so there is no good way to check.
           However, this branch is only hit by [wrap_constraint_with_shape],
           in which case LHS and RHS should be physically equal. *)
        assert (left == right);
        Ok ()
      | Const _, Diff _ -> assert false
      | Undefined, _ | _, Undefined ->
        Misc.fatal_error "undefined modality should not be inspected."

    let id = Const Const.id

    let apply : type r. t -> (allowed * r) Mode.t -> Mode.l =
     fun t x ->
      match t with
      | Const c -> Const.apply c x |> Mode.disallow_right
      | Undefined -> assert false
      | Diff (_, m) -> Mode.join [m; Mode.disallow_right x]

    let print ppf = function
      | Const c -> Const.print ppf c
      | Undefined -> Format.fprintf ppf "undefined"
      | Diff _ -> Format.fprintf ppf "diff"

    let cross mm m =
      (* We will be producing [join x m] for any [x >= mm].
         We want to find the minimal [m'] such that [join x m <= join x m'] for any [x >= mm].
         Or equivalently [m <= join x m'] for any [x >= mm].
         Equivalently [m <= join mm m'].
         Equivalently [m <= join mm.lower m'].
         or [subtract mm.lower m <= m'].
      *)
      let mc = Mode.Guts.get_floor mm in
      Mode.subtract mc m

    let zap_to_floor = function
      | Const c -> c
      | Undefined -> assert false
      | Diff (mm, m) ->
        let m' = cross mm m in
        let c = Mode.zap_to_floor m' in
        Const.Join_const c

    let zap_to_id = zap_to_floor

    let to_const_exn = function
      | Const c -> c
      | Undefined | Diff _ -> assert false

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
              (Error (ax, { left = Meet_with left; right = Meet_with right }))

      let compose :
          type a l r. a axis -> ((a, l * r) mode_comonadic, a) raw -> t -> t =
       fun ax a t ->
        match a, t with
        | Meet_with c0, Meet_const c ->
          Meet_const (Mode.Const.meet (Mode.Const.max_with ax c0) c)
        | Join_with _, Meet_const _ -> assert false

      let apply : type l r. t -> (l * r) Mode.t -> (l * r) Mode.t =
       fun t x -> match t with Meet_const c -> Mode.meet_const c x

      let to_list = function
        | Meet_const c ->
          [ (let ax : _ Axis.t = Areality in
             Atom (Comonadic ax, Meet_with (Axis.proj ax c)));
            (let ax : _ Axis.t = Linearity in
             Atom (Comonadic ax, Meet_with (Axis.proj ax c)));
            (let ax : _ Axis.t = Portability in
             Atom (Comonadic ax, Meet_with (Axis.proj ax c))) ]

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
        (* check for any x >= mm, meet(m, x) <= meet(c, x).
           Equivalent to check meet(m, x) <= c.
           Equivalent to check m <= c. *)
        match Mode.submode_log m (Mode.of_const c) ~log with
        | Ok () -> Ok ()
        | Error (Error (ax, { left; _ })) ->
          Error
            (Error
               ( ax,
                 { left = Meet_with left; right = Meet_with (Axis.proj ax c) }
               )))
      | Exactly _, Exactly _ ->
        (* [m1] is a left mode, so there is no good way to check.
           However, this branch only hit by [wrap_constraint_with_shape],
           in which case LHS and RHS should be physically equal. *)
        assert (left == right);
        Ok ()
      | Const _, Exactly _ -> assert false
      | Undefined, _ | _, Undefined -> assert false

    let id = Const Const.id

    let apply : type r. t -> (allowed * r) Mode.t -> Mode.l =
     fun t x ->
      match t with
      | Const c -> Const.apply c x |> Mode.disallow_right
      | Undefined -> assert false
      | Exactly (_mm, m) -> m

    let print ppf = function
      | Const c -> Const.print ppf c
      | Undefined -> Format.fprintf ppf "undefined"
      | Exactly _ -> Format.fprintf ppf "exactly"

    let infer ~md_mode ~mode = Exactly (md_mode, mode)

    let max = Const Const.max

    let zap_to_ceil = function
      | Const c -> c
      | Undefined -> assert false
      | Exactly _ -> Const.id

    let zap_to_id = zap_to_ceil

    let zap_to_floor = function
      | Const c -> c
      | Undefined -> assert false
      | Exactly (_, m) ->
        let c = Mode.zap_to_floor m in
        Const.Meet_const c

    let to_const_exn = function
      | Const c -> c
      | Undefined | Exactly _ -> assert false

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

      let apply t { monadic; comonadic } =
        let monadic = Monadic.apply t.monadic monadic in
        let comonadic = Comonadic.apply t.comonadic comonadic in
        { monadic; comonadic }

      let compose (Atom (ax, a)) t =
        match ax with
        | Monadic ax ->
          let monadic = Monadic.compose ax a t.monadic in
          { t with monadic }
        | Comonadic ax ->
          let comonadic = Comonadic.compose ax a t.comonadic in
          { t with comonadic }

      let singleton a = compose a id

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
      Format.fprintf ppf "%a,%a" Monadic.print monadic Comonadic.print comonadic

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
