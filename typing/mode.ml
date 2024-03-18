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

module Axis = struct
  type t =
    [ `Locality
    | `Regionality
    | `Uniqueness
    | `Linearity ]

  let to_string = function
    | `Locality -> "locality"
    | `Regionality -> "regionality"
    | `Uniqueness -> "uniqueness"
    | `Linearity -> "linearity"
end

module Global_flag = struct
  type t =
    | Global
    | Unrestricted

  let compare flag0 flag1 =
    match flag0, flag1 with
    | Global, Unrestricted -> -1
    | Unrestricted, Global -> 1
    | Global, Global | Unrestricted, Unrestricted -> 0
end

module type BiHeyting = sig
  (** Extend the [Lattice] interface with operations of bi-Heyting algebras *)

  include Lattice

  (** [imply c] is the right adjoint of [meet c]; That is, for any [a] and [b],
      [meet c a <= b] iff [a <= imply c b] *)
  val imply : t -> t -> t

  (** [subtract c] is the left adjoint of [join c]. That is, for any [a] and [b],
      [subtract c a <= b] iff [a <= join c b] *)
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

    let imply = L.subtract

    let subtract = L.imply
  end
  [@@inline]

  (* A lattice is total order, if for any [a] [b], [a <= b] or [b <= a].
     A total lattice has a bi-heyting structure given as follows. *)
  module Total (L : Lattice) : BiHeyting with type t := L.t = struct
    include L

    (* Prove the [subtract] below is the left adjoint of [join].
       - If [subtract c a <= b], by the definition of [subtract] below,
         that could mean one of two things:
         - Took the branch [a <= c], and [min <= b]. In this case, we have [a <= c <= join c b].
         - Took the other branch, and [a <= b]. In this case, we have [a <= b <= join c b].

       - In the other direction: Given [a <= join c b], compare [c] and [b]:
         - if [c <= b], then [a <= join c b = b], and:
           - either [a <= c], then [subtract c a = min <= b]
           - or the other branch, then [subtract c a = a <= b]
         - if [b <= c], then [a <= join c b = c], then [subtract c a = min <= b]
    *)
    let subtract c a = if le a c then min else a

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
        | Global -> Format.fprintf ppf "Global"
        | Local -> Format.fprintf ppf "Local"
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
        | Global -> Format.fprintf ppf "Global"
        | Regional -> Format.fprintf ppf "Regional"
        | Local -> Format.fprintf ppf "Local"
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
        | Shared -> Format.fprintf ppf "Shared"
        | Unique -> Format.fprintf ppf "Unique"
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
        | Once -> Format.fprintf ppf "Once"
        | Many -> Format.fprintf ppf "Many"
    end)
  end

  module Unit = struct
    type t = unit

    include Total (struct
      type nonrec t = t

      let min = ()

      let max = ()

      let legacy = ()

      let le () () = true

      let join () () = ()

      let meet () () = ()

      let print _ppf () = ()
    end)
  end

  module Monadic = struct
    (* CR zqian: the extra unit is to distinguish [Moandic.t] from
       [Uniqueness.t], in order to eliminate some GADT match cases. There might
       be smarter ways to do this, but we will very soon replace [unit] with
       proper [Contention.t] anyway, so this seem like a easy quick fix. *)
    type t = Uniqueness.t * Unit.t

    let min = Uniqueness.min, Unit.min

    let max = Uniqueness.max, Unit.max

    let legacy = Uniqueness.legacy, Unit.legacy

    let le (a0, a1) (b0, b1) = Uniqueness.le a0 b0 && Unit.le a1 b1

    let join (a0, a1) (b0, b1) = Uniqueness.join a0 b0, Unit.join a1 b1

    let meet (a0, a1) (b0, b1) = Uniqueness.meet a0 b0, Unit.meet a1 b1

    let imply (a0, a1) (b0, b1) = Uniqueness.imply a0 b0, Unit.imply a1 b1

    let subtract (a0, a1) (b0, b1) =
      Uniqueness.subtract a0 b0, Unit.subtract a1 b1

    let print ppf (a0, ()) = Format.fprintf ppf "%a" Uniqueness.print a0
  end

  type 'areality comonadic_with = 'areality * Linearity.t

  module Comonadic_with (Areality : Areality) = struct
    type t = Areality.t comonadic_with

    let min = Areality.min, Linearity.min

    let max = Areality.max, Linearity.max

    let legacy = Areality.legacy, Linearity.legacy

    let le (a0, a1) (b0, b1) = Areality.le a0 b0 && Linearity.le a1 b1

    let join (a0, a1) (b0, b1) = Areality.join a0 b0, Linearity.join a1 b1

    let meet (a0, a1) (b0, b1) = Areality.meet a0 b0, Linearity.meet a1 b1

    let imply (a0, a1) (b0, b1) = Areality.imply a0 b0, Linearity.imply a1 b1

    let subtract (a0, a1) (b0, b1) =
      Areality.subtract a0 b0, Linearity.subtract a1 b1

    let print ppf (a0, a1) =
      Format.fprintf ppf "%a,%a" Areality.print a0 Linearity.print a1
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
    | Monadic_op : Monadic_op.t obj
    | Comonadic_with_regionality : Comonadic_with_regionality.t obj
    | Comonadic_with_locality : Comonadic_with_locality.t obj

  let print_obj : type a. _ -> a obj -> unit =
   fun ppf -> function
    | Locality -> Format.fprintf ppf "Locality"
    | Regionality -> Format.fprintf ppf "Regionality"
    | Uniqueness_op -> Format.fprintf ppf "Uniqueness_op"
    | Linearity -> Format.fprintf ppf "Linearity"
    | Monadic_op -> Format.fprintf ppf "Monadic_op"
    | Comonadic_with_locality -> Format.fprintf ppf "Comonadic_with_locality"
    | Comonadic_with_regionality ->
      Format.fprintf ppf "Comonadic_with_regionality"

  (** ('t, 'r) represents a projection from a product of type ['t] to a element
  of type ['r]. *)
  type ('t, 'r) axis =
    | Areality : ('a comonadic_with, 'a) axis
    | Linearity : ('areality comonadic_with, Linearity.t) axis
    | Uniqueness : (Monadic_op.t, Uniqueness_op.t) axis

  let print_axis : type t r. _ -> (t, r) axis -> unit =
   fun ppf -> function
    | Areality -> Format.fprintf ppf "areality"
    | Linearity -> Format.fprintf ppf "linearity"
    | Uniqueness -> Format.fprintf ppf "uniqueness"

  let eq_axis :
      type t r0 r1. (t, r0) axis -> (t, r1) axis -> (r0, r1) Misc.eq option =
   fun ax0 ax1 ->
    match ax0, ax1 with
    | Areality, Areality -> Some Refl
    | Linearity, Linearity -> Some Refl
    | Uniqueness, Uniqueness -> Some Refl
    | (Areality | Linearity | Uniqueness), _ -> None

  let proj : type t r. (t, r) axis -> t -> r =
   fun ax t ->
    match ax, t with
    | Areality, (a, _) -> a
    | Linearity, (_, lin) -> lin
    | Uniqueness, (uni, ()) -> uni

  let update : type t r. (t, r) axis -> r -> t -> t =
   fun ax r t ->
    match ax, t with
    | Areality, (_, lin) -> r, lin
    | Linearity, (area, _) -> area, r
    | Uniqueness, (_, ()) -> r, ()

  let proj_obj : type t r. (t, r) axis -> t obj -> r obj =
   fun ax obj ->
    match ax, obj with
    | Areality, Comonadic_with_locality -> Locality
    | Areality, Comonadic_with_regionality -> Regionality
    | Linearity, Comonadic_with_locality -> Linearity
    | Linearity, Comonadic_with_regionality -> Linearity
    | Uniqueness, Monadic_op -> Uniqueness_op

  let comonadic_with_obj : type a. a obj -> a comonadic_with obj =
   fun a0 ->
    match a0 with
    | Locality -> Comonadic_with_locality
    | Regionality -> Comonadic_with_regionality
    | Uniqueness_op | Linearity | Monadic_op | Comonadic_with_regionality
    | Comonadic_with_locality ->
      assert false

  let min : type a. a obj -> a = function
    | Locality -> Locality.min
    | Regionality -> Regionality.min
    | Uniqueness_op -> Uniqueness_op.min
    | Linearity -> Linearity.min
    | Monadic_op -> Monadic_op.min
    | Comonadic_with_locality -> Comonadic_with_locality.min
    | Comonadic_with_regionality -> Comonadic_with_regionality.min

  let max : type a. a obj -> a = function
    | Locality -> Locality.max
    | Regionality -> Regionality.max
    | Uniqueness_op -> Uniqueness_op.max
    | Linearity -> Linearity.max
    | Monadic_op -> Monadic_op.max
    | Comonadic_with_locality -> Comonadic_with_locality.max
    | Comonadic_with_regionality -> Comonadic_with_regionality.max

  let le : type a. a obj -> a -> a -> bool =
   fun obj a b ->
    match obj with
    | Locality -> Locality.le a b
    | Regionality -> Regionality.le a b
    | Uniqueness_op -> Uniqueness_op.le a b
    | Linearity -> Linearity.le a b
    | Monadic_op -> Monadic_op.le a b
    | Comonadic_with_locality -> Comonadic_with_locality.le a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.le a b

  let join : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.join a b
    | Regionality -> Regionality.join a b
    | Uniqueness_op -> Uniqueness_op.join a b
    | Linearity -> Linearity.join a b
    | Monadic_op -> Monadic_op.join a b
    | Comonadic_with_locality -> Comonadic_with_locality.join a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.join a b

  let meet : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.meet a b
    | Regionality -> Regionality.meet a b
    | Uniqueness_op -> Uniqueness_op.meet a b
    | Linearity -> Linearity.meet a b
    | Monadic_op -> Monadic_op.meet a b
    | Comonadic_with_locality -> Comonadic_with_locality.meet a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.meet a b

  let imply : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.imply a b
    | Regionality -> Regionality.imply a b
    | Uniqueness_op -> Uniqueness_op.imply a b
    | Linearity -> Linearity.imply a b
    | Comonadic_with_locality -> Comonadic_with_locality.imply a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.imply a b
    | Monadic_op -> Monadic_op.imply a b

  let subtract : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.subtract a b
    | Regionality -> Regionality.subtract a b
    | Uniqueness_op -> Uniqueness_op.subtract a b
    | Linearity -> Linearity.subtract a b
    | Comonadic_with_locality -> Comonadic_with_locality.subtract a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.subtract a b
    | Monadic_op -> Monadic_op.subtract a b

  (* not hotpath, Ok to curry *)
  let print : type a. a obj -> _ -> a -> unit = function
    | Locality -> Locality.print
    | Regionality -> Regionality.print
    | Uniqueness_op -> Uniqueness_op.print
    | Linearity -> Linearity.print
    | Monadic_op -> Monadic_op.print
    | Comonadic_with_locality -> Comonadic_with_locality.print
    | Comonadic_with_regionality -> Comonadic_with_regionality.print

  module Equal_obj = Magic_equal (struct
    type ('a, _, 'd) t = 'a obj constraint 'd = 'l * 'r

    let equal : type a b. a obj -> b obj -> (a, b) Misc.eq option =
     fun a b ->
      match a, b with
      | Locality, Locality -> Some Misc.Refl
      | Regionality, Regionality -> Some Misc.Refl
      | Uniqueness_op, Uniqueness_op -> Some Misc.Refl
      | Linearity, Linearity -> Some Misc.Refl
      | Monadic_op, Monadic_op -> Some Misc.Refl
      | Comonadic_with_locality, Comonadic_with_locality -> Some Misc.Refl
      | Comonadic_with_regionality, Comonadic_with_regionality -> Some Misc.Refl
      | ( ( Locality | Regionality | Uniqueness_op | Linearity | Monadic_op
          | Comonadic_with_locality | Comonadic_with_regionality ),
          _ ) ->
        None
  end)

  let eq_obj = Equal_obj.equal
end

module Lattices_mono = struct
  include Lattices

  type ('a, 'b, 'd) morph =
    | Id : ('a, 'a, 'd) morph  (** identity morphism *)
    | Meet_with : 'a -> ('a, 'a, 'd * disallowed) morph
        (** Meet the input with the parameter *)
    | Imply : 'a -> ('a, 'a, disallowed * 'd) morph
        (** The right adjoint of [Meet_with] *)
    | Join_with : 'a -> ('a, 'a, disallowed * 'd) morph
        (** Join the input with the parameter *)
    | Subtract : 'a -> ('a, 'a, 'd * disallowed) morph
        (** The left adjoint of [Join_with] *)
    | Proj : 't obj * ('t, 'r_) axis -> ('t, 'r_, 'l * 'r) morph
        (** Project from a product to an axis *)
    | Max_with : ('t, 'r_) axis -> ('r_, 't, disallowed * 'r) morph
        (** Combine an axis with maxima along other axes *)
    | Min_with : ('t, 'r_) axis -> ('r_, 't, 'l * disallowed) morph
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
          match eq_axis ax0 ax1 with None -> None | Some Refl -> Some Refl)
        | None -> None)
      | Max_with ax0, Max_with ax1 -> (
        match eq_axis ax0 ax1 with Some Refl -> Some Refl | None -> None)
      | Min_with ax0, Min_with ax1 -> (
        match eq_axis ax0 ax1 with Some Refl -> Some Refl | None -> None)
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
    | Proj (_, ax) -> Format.fprintf ppf "proj_%a" print_axis ax
    | Max_with ax -> Format.fprintf ppf "max_with_%a" print_axis ax
    | Min_with ax -> Format.fprintf ppf "min_with_%a" print_axis ax
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

  let min_with dst ax a = update ax a (min dst)

  let max_with dst ax a = update ax a (max dst)

  let monadic_to_comonadic_min :
      type a. a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj (uniqueness, ()) ->
    match obj with
    | Comonadic_with_locality -> Locality.min, unique_to_linear uniqueness
    | Comonadic_with_regionality -> Regionality.min, unique_to_linear uniqueness

  let comonadic_to_monadic :
      type a. a comonadic_with obj -> a comonadic_with -> Monadic_op.t =
   fun obj (_, linearity) ->
    match obj with
    | Comonadic_with_locality -> linear_to_unique linearity, ()
    | Comonadic_with_regionality -> linear_to_unique linearity, ()

  let monadic_to_comonadic_max :
      type a. a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj (uniqueness, ()) ->
    match obj with
    | Comonadic_with_locality -> Locality.max, unique_to_linear uniqueness
    | Comonadic_with_regionality -> Regionality.max, unique_to_linear uniqueness

  let rec apply : type a b d. b obj -> (a, b, d) morph -> a -> b =
   fun dst f a ->
    match f with
    | Compose (f, g) ->
      let mid = src dst f in
      let g' = apply mid g in
      let f' = apply dst f in
      f' (g' a)
    | Id -> a
    | Proj (_, ax) -> proj ax a
    | Max_with ax -> max_with dst ax a
    | Min_with ax -> min_with dst ax a
    | Meet_with c -> meet dst c a
    | Join_with c -> join dst c a
    | Imply c -> imply dst c a
    | Subtract c -> subtract dst c a
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
      let a0, a1 = a in
      apply dst0 f a0, a1

  (** Compose m0 after m1. Returns [Some f] if the composition can be
    represented by [f] instead of [Compose m0 m1]. [None] otherwise. *)
  let rec maybe_compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph option =
   fun dst m0 m1 ->
    match m0, m1 with
    | Id, m -> Some m
    | m, Id -> Some m
    | Meet_with c0, Meet_with c1 -> Some (Meet_with (meet dst c0 c1))
    | Join_with c0, Join_with c1 -> Some (Join_with (join dst c0 c1))
    | Meet_with c0, m1 when le dst (max dst) c0 -> Some m1
    | Join_with c0, m1 when le dst c0 (min dst) -> Some m1
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
      Some (Compose (Meet_with (proj ax c), Proj (mid, ax)))
    | Proj (mid, ax), Join_with c ->
      Some (Compose (Join_with (proj ax c), Proj (mid, ax)))
    | Proj (_, ax0), Max_with ax1 -> (
      match eq_axis ax0 ax1 with None -> None | Some Refl -> Some Id)
    | Proj (_, ax0), Min_with ax1 -> (
      match eq_axis ax0 ax1 with None -> None | Some Refl -> Some Id)
    | Proj (mid, ax), Map_comonadic f -> (
      let src' = src mid m1 in
      match ax with
      | Areality -> Some (compose dst f (Proj (src', Areality)))
      | Linearity -> Some (Proj (src', Linearity)))
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
      let areality, linearity = c in
      Some
        (compose dst
           (Join_with (min_with dst Linearity linearity))
           (Map_comonadic (compose dst0 f (Join_with areality))))
    | Map_comonadic f, Meet_with c ->
      let dst0 = proj_obj Areality dst in
      let areality, linearity = c in
      Some
        (compose dst
           (Meet_with (max_with dst Linearity linearity))
           (Map_comonadic (compose dst0 f (Meet_with areality))))
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

type changes = S.changes

let undo_changes = S.undo_changes

let set_append_changes = S.set_append_changes

(** Representing a single object *)
module type Obj = sig
  type const

  module Solver : S.Solver_polarized

  val obj : const C.obj
end

let equate_from_submode submode m0 m1 =
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

  let submode m0 m1 : (unit, error) result = Solver.submode obj m0 m1

  let join l = Solver.join obj l

  let meet l = Solver.meet obj l

  let submode_exn m0 m1 = assert (submode m0 m1 |> Result.is_ok)

  let equate = equate_from_submode submode

  let equate_exn m0 m1 = assert (equate m0 m1 |> Result.is_ok)

  let print ?(raw = false) ?verbose () ppf m =
    if raw
    then Solver.print_raw ?verbose obj ppf m
    else Solver.print ?verbose obj ppf m

  let zap_to_ceil m = Solver.zap_to_ceil obj m

  let zap_to_floor m = Solver.zap_to_floor obj m

  let of_const : type l r. const -> (l * r) t = fun a -> Solver.of_const obj a

  let check_const m = Solver.check_const obj m
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

let regional_to_local m =
  S.Positive.via_monotone Locality.Obj.obj C.Regional_to_local m

let locality_as_regionality m =
  S.Positive.via_monotone Regionality.Obj.obj C.Locality_as_regionality m

let regional_to_global m =
  S.Positive.via_monotone Locality.Obj.obj C.Regional_to_global m

module Comonadic_with_regionality = struct
  module Const = C.Comonadic_with_regionality

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj = C.Comonadic_with_regionality
  end

  include Common (Obj)
  open Obj

  type error =
    [ `Regionality of Regionality.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  let regionality m =
    S.Positive.via_monotone Regionality.Obj.obj (Proj (Obj.obj, Areality)) m

  let min_with_regionality m =
    S.Positive.via_monotone Obj.obj (Min_with Areality)
      (S.Positive.disallow_right m)

  let max_with_regionality m =
    S.Positive.via_monotone Obj.obj (Max_with Areality)
      (S.Positive.disallow_left m)

  let join_with_regionality c m =
    S.Positive.via_monotone Obj.obj
      (Join_with (C.min_with Obj.obj Areality c))
      (S.Positive.disallow_left m)

  let meet_with_regionality c m =
    S.Positive.via_monotone Obj.obj
      (Meet_with (C.max_with Obj.obj Areality c))
      (S.Positive.disallow_right m)

  let linearity m =
    S.Positive.via_monotone Linearity.Obj.obj (Proj (Obj.obj, Linearity)) m

  let min_with_linearity m =
    S.Positive.via_monotone Obj.obj (Min_with Linearity)
      (S.Positive.disallow_right m)

  let max_with_linearity m =
    S.Positive.via_monotone Obj.obj (Max_with Linearity)
      (S.Positive.disallow_left m)

  let join_with_linearity c m =
    S.Positive.via_monotone Obj.obj
      (Join_with (C.min_with Obj.obj Linearity c))
      (S.Positive.disallow_left m)

  let meet_with_linearity c m =
    S.Positive.via_monotone Obj.obj
      (Meet_with (C.max_with Obj.obj Linearity c))
      (S.Positive.disallow_right m)

  let meet_with c m =
    Solver.via_monotone obj (Meet_with c) (Solver.disallow_right m)

  let imply c m = Solver.via_monotone obj (Imply c) (Solver.disallow_left m)

  let zap_to_legacy m =
    let regionality = regionality m |> Regionality.zap_to_legacy in
    let linearity = linearity m |> Linearity.zap_to_legacy in
    regionality, linearity

  let legacy = of_const Const.legacy

  (* overriding to report the offending axis *)
  let submode m0 m1 =
    match submode m0 m1 with
    | Ok () -> Ok ()
    | Error { left = reg0, lin0; right = reg1, lin1 } ->
      if Regionality.Const.le reg0 reg1
      then
        if Linearity.Const.le lin0 lin1
        then assert false
        else Error (`Linearity { left = lin0; right = lin1 })
      else Error (`Regionality { left = reg0; right = reg1 })

  (* override to report the offending axis *)
  let equate = equate_from_submode submode

  (** overriding to check per-axis *)
  let check_const m =
    let regionality = Regionality.check_const (regionality m) in
    let linearity = Linearity.check_const (linearity m) in
    regionality, linearity
end

module Comonadic_with_locality = struct
  module Const = struct
    include C.Comonadic_with_locality
  end

  module Obj = struct
    type const = Const.t

    module Solver = S.Positive

    let obj = C.Comonadic_with_locality
  end

  include Common (Obj)
  open Obj

  type error =
    [ `Locality of Locality.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  let locality m =
    S.Positive.via_monotone Locality.Obj.obj (Proj (Obj.obj, Areality)) m

  let min_with_locality m =
    S.Positive.via_monotone Obj.obj (Min_with Areality)
      (S.Positive.disallow_right m)

  let max_with_locality m =
    S.Positive.via_monotone Obj.obj (Max_with Areality)
      (S.Positive.disallow_left m)

  let join_with_locality c m =
    S.Positive.via_monotone Obj.obj
      (Join_with (C.min_with Obj.obj Areality c))
      (S.Positive.disallow_left m)

  let meet_with_locality c m =
    S.Positive.via_monotone Obj.obj
      (Meet_with (C.max_with Obj.obj Areality c))
      (S.Positive.disallow_right m)

  let linearity m =
    S.Positive.via_monotone Linearity.Obj.obj (Proj (Obj.obj, Linearity)) m

  let min_with_linearity m =
    S.Positive.via_monotone Obj.obj (Min_with Linearity)
      (S.Positive.disallow_right m)

  let max_with_linearity m =
    S.Positive.via_monotone Obj.obj (Max_with Linearity)
      (S.Positive.disallow_left m)

  let join_with_linearity c m =
    S.Positive.via_monotone Obj.obj
      (Join_with (C.min_with Obj.obj Linearity c))
      (S.Positive.disallow_left m)

  let meet_with_linearity c m =
    S.Positive.via_monotone Obj.obj
      (Meet_with (C.max_with Obj.obj Linearity c))
      (S.Positive.disallow_right m)

  let zap_to_legacy m =
    let locality = locality m |> Locality.zap_to_legacy in
    let linearity = linearity m |> Linearity.zap_to_legacy in
    locality, linearity

  let meet_with c m =
    Solver.via_monotone obj (Meet_with c) (Solver.disallow_right m)

  let imply c m = Solver.via_monotone obj (Imply c) (Solver.disallow_left m)

  let legacy = of_const Const.legacy

  (* overriding to report the offending axis *)
  let submode m0 m1 =
    match submode m0 m1 with
    | Ok () -> Ok ()
    | Error { left = loc0, lin0; right = loc1, lin1 } ->
      if Locality.Const.le loc0 loc1
      then
        if Linearity.Const.le lin0 lin1
        then assert false
        else Error (`Linearity { left = lin0; right = lin1 })
      else Error (`Locality { left = loc0; right = loc1 })

  (* override to report the offending axis *)
  let equate = equate_from_submode submode

  (** overriding to check per-axis *)
  let check_const m =
    let locality = Locality.check_const (locality m) in
    let linearity = Linearity.check_const (linearity m) in
    locality, linearity
end

module Monadic = struct
  module Const = C.Monadic

  module Obj = struct
    type const = Const.t

    (* Negative solver on the opposite of monadic should give the monadic
       fragment with original ordering *)
    module Solver = S.Negative

    let obj = C.Monadic_op
  end

  include Common (Obj)
  open Obj

  type error = [`Uniqueness of Uniqueness.error]

  type equate_error = equate_step * error

  let uniqueness m =
    S.Negative.via_monotone Uniqueness.Obj.obj (Proj (Obj.obj, Uniqueness)) m

  (* The monadic fragment is inverted. Most of the inversion logic is taken care
     by [Solver_polarized], but some remain, such as the [Min_with] below which
     is inverted from [Max_with]. *)

  let max_with_uniqueness m =
    S.Negative.via_monotone Obj.obj (Min_with Uniqueness)
      (S.Negative.disallow_left m)

  let min_with_uniqueness m =
    S.Negative.via_monotone Obj.obj (Max_with Uniqueness)
      (S.Negative.disallow_right m)

  let join_with_uniqueness c m =
    S.Negative.via_monotone Obj.obj
      (Meet_with (C.max_with Obj.obj Uniqueness c))
      (S.Negative.disallow_left m)

  let meet_with_uniqueness c m =
    S.Negative.via_monotone Obj.obj
      (Join_with (C.min_with Obj.obj Uniqueness c))
      (S.Negative.disallow_right m)

  let meet_with c m =
    Solver.via_monotone obj (Join_with c) (Solver.disallow_right m)

  let imply c m = Solver.via_monotone obj (Subtract c) (Solver.disallow_left m)

  let zap_to_legacy m =
    let uniqueness = uniqueness m |> Uniqueness.zap_to_legacy in
    uniqueness, ()

  let legacy = of_const Const.legacy

  (* overriding to report the offending axis *)
  let submode m0 m1 =
    match submode m0 m1 with
    | Ok () -> Ok ()
    | Error { left = uni0, (); right = uni1, () } ->
      if Uniqueness.Const.le uni0 uni1
      then assert false
      else Error (`Uniqueness { left = uni0; right = uni1 })

  (* override to report the offending axis *)
  let equate = equate_from_submode submode

  (** overriding to check per-axis *)
  let check_const m =
    let uniqueness = Uniqueness.check_const (uniqueness m) in
    uniqueness
end

type ('mo, 'como) monadic_comonadic =
  { monadic : 'mo;
    comonadic : 'como
  }

module Value = struct
  module Comonadic = Comonadic_with_regionality
  module Monadic = Monadic

  type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type ('a, 'b, 'c) modes =
    { regionality : 'a;
      linearity : 'b;
      uniqueness : 'c
    }

  let min = { comonadic = Comonadic.min; monadic = Monadic.min }

  let max =
    { comonadic = Comonadic.max;
      monadic = Monadic.max |> Monadic.allow_left |> Monadic.allow_right
    }

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

  let uniqueness { monadic; _ } = Monadic.uniqueness monadic

  let linearity { comonadic; _ } = Comonadic.linearity comonadic

  let regionality { comonadic; _ } = Comonadic.regionality comonadic

  type error =
    [ `Regionality of Regionality.error
    | `Uniqueness of Uniqueness.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  (* NB: state mutated when error *)
  let submode { monadic = monadic0; comonadic = comonadic0 }
      { monadic = monadic1; comonadic = comonadic1 } =
    (* comonadic before monadic, so that locality errors dominate
       (error message backward compatibility) *)
    match Comonadic.submode comonadic0 comonadic1 with
    | Error e -> Error e
    | Ok () -> (
      match Monadic.submode monadic0 monadic1 with
      | Error e -> Error e
      | Ok () -> Ok ())

  let equate = equate_from_submode submode

  let submode_exn m0 m1 =
    match submode m0 m1 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let equate_exn m0 m1 =
    match equate m0 m1 with Ok () -> () | Error _ -> invalid_arg "equate_exn"

  let print ?raw ?verbose () ppf { monadic; comonadic } =
    Format.fprintf ppf "%a,%a"
      (Comonadic.print ?raw ?verbose ())
      comonadic
      (Monadic.print ?raw ?verbose ())
      monadic

  let zap_to_floor { comonadic; monadic } =
    match Monadic.zap_to_floor monadic, Comonadic.zap_to_floor comonadic with
    | (uniqueness, ()), (regionality, linearity) ->
      { regionality; linearity; uniqueness }

  let zap_to_ceil { comonadic; monadic } =
    match Monadic.zap_to_ceil monadic, Comonadic.zap_to_ceil comonadic with
    | (uniqueness, ()), (regionality, linearity) ->
      { regionality; linearity; uniqueness }

  let zap_to_legacy { comonadic; monadic } =
    match Monadic.zap_to_legacy monadic, Comonadic.zap_to_legacy comonadic with
    | (uniqueness, ()), (regionality, linearity) ->
      { regionality; linearity; uniqueness }

  let check_const { comonadic; monadic } =
    let regionality, linearity = Comonadic.check_const comonadic in
    let uniqueness = Monadic.check_const monadic in
    { regionality; linearity; uniqueness }

  let of_const { regionality; linearity; uniqueness } =
    let comonadic = Comonadic.of_const (regionality, linearity) in
    let monadic = Monadic.of_const (uniqueness, ()) in
    { comonadic; monadic }

  let legacy =
    let comonadic = Comonadic.legacy in
    let monadic = Monadic.legacy in
    { comonadic; monadic }

  let max_with_uniqueness uniqueness =
    let comonadic =
      Comonadic.max |> Comonadic.disallow_left |> Comonadic.allow_right
    in
    let monadic = Monadic.max_with_uniqueness uniqueness in
    { comonadic; monadic }

  let min_with_uniqueness uniqueness =
    let comonadic =
      Comonadic.min |> Comonadic.disallow_right |> Comonadic.allow_left
    in
    let monadic = Monadic.min_with_uniqueness uniqueness in
    { comonadic; monadic }

  let join_with_uniqueness c { monadic; comonadic } =
    let comonadic = Comonadic.disallow_left comonadic in
    let monadic = Monadic.join_with_uniqueness c monadic in
    { monadic; comonadic }

  let meet_with_uniqueness c { monadic; comonadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    let monadic = Monadic.meet_with_uniqueness c monadic in
    { monadic; comonadic }

  let min_with_regionality regionality =
    let comonadic = Comonadic.min_with_regionality regionality in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_regionality regionality =
    let comonadic = Comonadic.max_with_regionality regionality in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let meet_with_regionality c { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.meet_with_regionality c comonadic in
    { comonadic; monadic }

  let join_with_regionality c { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.join_with_regionality c comonadic in
    { comonadic; monadic }

  let min_with_linearity linearity =
    let comonadic = Comonadic.min_with_linearity linearity in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_linearity linearity =
    let comonadic = Comonadic.max_with_linearity linearity in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let join_with_linearity c { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.join_with_linearity c comonadic in
    { comonadic; monadic }

  let meet_with_linearity c { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.meet_with_linearity c comonadic in
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

  let comonadic_to_monadic m =
    S.Negative.via_antitone Monadic.Obj.obj
      (Comonadic_to_monadic Comonadic.Obj.obj) m

  module Const = struct
    type t = (Regionality.Const.t, Linearity.Const.t, Uniqueness.Const.t) modes

    let split { regionality; linearity; uniqueness } =
      let monadic = uniqueness, () in
      let comonadic = regionality, linearity in
      { comonadic; monadic }

    let _merge { comonadic; monadic } =
      let regionality, linearity = comonadic in
      let uniqueness, () = monadic in
      { regionality; linearity; uniqueness }

    let min =
      { regionality = Regionality.Const.min;
        linearity = Linearity.Const.min;
        uniqueness = Uniqueness.Const.min
      }

    let max =
      { regionality = Regionality.Const.max;
        linearity = Linearity.Const.max;
        uniqueness = Uniqueness.Const.max
      }

    let le m0 m1 =
      Regionality.Const.le m0.regionality m1.regionality
      && Uniqueness.Const.le m0.uniqueness m1.uniqueness
      && Linearity.Const.le m0.linearity m1.linearity

    let print ppf m = print () ppf (of_const m)

    let legacy =
      { regionality = Regionality.Const.legacy;
        linearity = Linearity.Const.legacy;
        uniqueness = Uniqueness.Const.legacy
      }

    let meet m0 m1 =
      let regionality = Regionality.Const.meet m0.regionality m1.regionality in
      let linearity = Linearity.Const.meet m0.linearity m1.linearity in
      let uniqueness = Uniqueness.Const.meet m0.uniqueness m1.uniqueness in
      { regionality; linearity; uniqueness }

    let join m0 m1 =
      let regionality = Regionality.Const.join m0.regionality m1.regionality in
      let linearity = Linearity.Const.join m0.linearity m1.linearity in
      let uniqueness = Uniqueness.Const.join m0.uniqueness m1.uniqueness in
      { regionality; linearity; uniqueness }
  end

  let meet_with c { comonadic; monadic } =
    let c = Const.split c in
    let comonadic = Comonadic.meet_with c.comonadic comonadic in
    let monadic = Monadic.meet_with c.monadic monadic in
    { monadic; comonadic }

  let imply c { comonadic; monadic } =
    let c = Const.split c in
    let comonadic = Comonadic.imply c.comonadic comonadic in
    let monadic = Monadic.imply c.monadic monadic in
    { monadic; comonadic }

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

module Alloc = struct
  module Comonadic = Comonadic_with_locality
  module Monadic = Monadic

  type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type ('a, 'b, 'c) modes =
    { locality : 'a;
      linearity : 'b;
      uniqueness : 'c
    }

  let min = { comonadic = Comonadic.min; monadic = Monadic.min }

  let max = { comonadic = Comonadic.min; monadic = Monadic.max }

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

  let uniqueness { monadic; _ } = Monadic.uniqueness monadic

  let linearity { comonadic; _ } = Comonadic.linearity comonadic

  let locality { comonadic; _ } = Comonadic.locality comonadic

  type error =
    [ `Locality of Locality.error
    | `Uniqueness of Uniqueness.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  (* NB: state mutated when error - should be fine as this always indicates type
     error in typecore.ml which triggers backtracking. *)
  let submode { monadic = monadic0; comonadic = comonadic0 }
      { monadic = monadic1; comonadic = comonadic1 } =
    match Monadic.submode monadic0 monadic1 with
    | Error e -> Error e
    | Ok () -> (
      match Comonadic.submode comonadic0 comonadic1 with
      | Error e -> Error e
      | Ok () -> Ok ())

  let equate = equate_from_submode submode

  let submode_exn m0 m1 =
    match submode m0 m1 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let equate_exn m0 m1 =
    match equate m0 m1 with Ok () -> () | Error _ -> invalid_arg "equate_exn"

  let print ?raw ?verbose () ppf { monadic; comonadic } =
    Format.fprintf ppf "%a,%a"
      (Comonadic.print ?raw ?verbose ())
      comonadic
      (Monadic.print ?raw ?verbose ())
      monadic

  let legacy =
    let comonadic = Comonadic.legacy in
    let monadic = Monadic.legacy in
    { comonadic; monadic }

  (* Below we package up the complex projection from alloc to three axes as if
     they live under alloc directly and uniformly. We define functions that operate
     on modes numerically, instead of defining symbolic functions *)
  (* type const = (LR.Const.t, Linearity.Const.t, Uniqueness.Const.t) modes *)

  let max_with_uniqueness uniqueness =
    let comonadic =
      Comonadic.max |> Comonadic.disallow_left |> Comonadic.allow_right
    in
    let monadic = Monadic.max_with_uniqueness uniqueness in
    { comonadic; monadic }

  let min_with_uniqueness uniqueness =
    let comonadic =
      Comonadic.min |> Comonadic.disallow_right |> Comonadic.allow_left
    in
    let monadic = Monadic.min_with_uniqueness uniqueness in
    { comonadic; monadic }

  let join_with_uniqueness c { monadic; comonadic } =
    let comonadic = Comonadic.disallow_left comonadic in
    let monadic = Monadic.join_with_uniqueness c monadic in
    { monadic; comonadic }

  let meet_with_uniqueness c { monadic; comonadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    let monadic = Monadic.meet_with_uniqueness c monadic in
    { monadic; comonadic }

  let min_with_locality locality =
    let comonadic = Comonadic.min_with_locality locality in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_locality locality =
    let comonadic = Comonadic.max_with_locality locality in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let meet_with_locality c { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.meet_with_locality c comonadic in
    { comonadic; monadic }

  let join_with_locality c { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.join_with_locality c comonadic in
    { comonadic; monadic }

  let min_with_linearity linearity =
    let comonadic = Comonadic.min_with_linearity linearity in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_linearity linearity =
    let comonadic = Comonadic.max_with_linearity linearity in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let join_with_linearity c { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.join_with_linearity c comonadic in
    { comonadic; monadic }

  let meet_with_linearity c { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.meet_with_linearity c comonadic in
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

  let monadic_to_comonadic_min m =
    S.Positive.via_antitone Comonadic.Obj.obj Monadic_to_comonadic_min
      (Monadic.disallow_left m)

  let of_const { locality; linearity; uniqueness } =
    let comonadic = Comonadic.of_const (locality, linearity) in
    let monadic = Monadic.of_const (uniqueness, ()) in
    { comonadic; monadic }

  module Const = struct
    type t = (Locality.Const.t, Linearity.Const.t, Uniqueness.Const.t) modes

    let min =
      let locality = Locality.Const.min in
      let linearity = Linearity.Const.min in
      let uniqueness = Uniqueness.Const.min in
      { locality; linearity; uniqueness }

    let max =
      let locality = Locality.Const.max in
      let linearity = Linearity.Const.max in
      let uniqueness = Uniqueness.Const.max in
      { locality; linearity; uniqueness }

    let le m0 m1 =
      Locality.Const.le m0.locality m1.locality
      && Uniqueness.Const.le m0.uniqueness m1.uniqueness
      && Linearity.Const.le m0.linearity m1.linearity

    let print ppf m = print () ppf (of_const m)

    let legacy =
      let locality = Locality.Const.legacy in
      let linearity = Linearity.Const.legacy in
      let uniqueness = Uniqueness.Const.legacy in
      { locality; linearity; uniqueness }

    let meet m0 m1 =
      let locality = Locality.Const.meet m0.locality m1.locality in
      let linearity = Linearity.Const.meet m0.linearity m1.linearity in
      let uniqueness = Uniqueness.Const.meet m0.uniqueness m1.uniqueness in
      { locality; linearity; uniqueness }

    let join m0 m1 =
      let locality = Locality.Const.join m0.locality m1.locality in
      let linearity = Linearity.Const.join m0.linearity m1.linearity in
      let uniqueness = Uniqueness.Const.join m0.uniqueness m1.uniqueness in
      { locality; linearity; uniqueness }

    module Option = struct
      type some = t

      type t =
        ( Locality.Const.t option,
          Linearity.Const.t option,
          Uniqueness.Const.t option )
        modes

      let none = { locality = None; uniqueness = None; linearity = None }

      let value opt ~default =
        let locality = Option.value opt.locality ~default:default.locality in
        let uniqueness =
          Option.value opt.uniqueness ~default:default.uniqueness
        in
        let linearity = Option.value opt.linearity ~default:default.linearity in
        { locality; uniqueness; linearity }
    end

    let split { locality; linearity; uniqueness } =
      let monadic = uniqueness, () in
      let comonadic = locality, linearity in
      { comonadic; monadic }

    let merge { comonadic; monadic } =
      let locality, linearity = comonadic in
      let uniqueness, () = monadic in
      { locality; linearity; uniqueness }

    (** See [Alloc.close_over] for explanation. *)
    let close_over m =
      let { monadic; comonadic } = split m in
      let comonadic =
        Comonadic.Const.join comonadic
          (C.monadic_to_comonadic_min Comonadic.Obj.obj monadic)
      in
      let monadic = Monadic.Const.min in
      merge { comonadic; monadic }

    (** See [Alloc.partial_apply] for explanation. *)
    let partial_apply m =
      let { comonadic; _ } = split m in
      let monadic = Monadic.Const.min in
      merge { comonadic; monadic }
  end

  let meet_with c { comonadic; monadic } =
    let c = Const.split c in
    let comonadic = Comonadic.meet_with c.comonadic comonadic in
    let monadic = Monadic.meet_with c.monadic monadic in
    { monadic; comonadic }

  let imply c { comonadic; monadic } =
    let c = Const.split c in
    let comonadic = Comonadic.imply c.comonadic comonadic in
    let monadic = Monadic.imply c.monadic monadic in
    { monadic; comonadic }

  let zap_to_floor { comonadic; monadic } : Const.t =
    match Monadic.zap_to_floor monadic, Comonadic.zap_to_floor comonadic with
    | (uniqueness, ()), (locality, linearity) ->
      { locality; linearity; uniqueness }

  let zap_to_ceil { comonadic; monadic } : Const.t =
    match Monadic.zap_to_ceil monadic, Comonadic.zap_to_ceil comonadic with
    | (uniqueness, ()), (locality, linearity) ->
      { locality; linearity; uniqueness }

  let zap_to_legacy { comonadic; monadic } : Const.t =
    match Monadic.zap_to_legacy monadic, Comonadic.zap_to_legacy comonadic with
    | (uniqueness, ()), (locality, linearity) ->
      { locality; linearity; uniqueness }

  let check_const { comonadic; monadic } : Const.Option.t =
    let locality, linearity = Comonadic.check_const comonadic in
    let uniqueness = Monadic.check_const monadic in
    { locality; linearity; uniqueness }

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
end

module Const = struct
  let alloc_as_value ({ locality; linearity; uniqueness } : Alloc.Const.t) :
      Value.Const.t =
    let regionality = C.locality_as_regionality locality in
    { regionality; linearity; uniqueness }
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
