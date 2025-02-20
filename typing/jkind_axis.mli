(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Re-export *)
module type Axis_ops = sig
  include Mode_intf.Lattice

  val less_or_equal : t -> t -> Misc.Le_result.t

  val equal : t -> t -> bool
end

(** The jkind axis of Externality *)
module Externality : sig
  type t =
    | External
    | External64
    | Internal

  include Axis_ops with type t := t
end

(** The jkind axis of nullability *)
module Nullability : sig
  type t =
    | Non_null
    | Maybe_null

  include Axis_ops with type t := t
end

module Axis : sig
  module Nonmodal : sig
    type 'a t =
      | Externality : Externality.t t
      | Nullability : Nullability.t t
  end

  (** Represents an axis of a jkind *)
  type 'a t =
    | Modal : ('m, 'a, 'd) Mode.Alloc.axis -> 'a t
    | Nonmodal : 'a Nonmodal.t -> 'a t

  type packed = Pack : 'a t -> packed [@@unboxed]

  (* CR zqian: push ['a t] into the module to avoid first-class module. *)

  (** Given a jkind axis, get its interface *)
  val get : 'a t -> (module Axis_ops with type t = 'a)

  val all : packed list

  val name : _ t -> string

  (** Is this a modal axis? Includes externality, because that will one
      day be modal (it is a deep property). *)
  val is_modal : _ t -> bool
end

(** A collection with one item for each jkind axis *)
module Axis_collection : sig
  module type S_gen := sig
    type ('a, 'b) u

    (* This is t_poly instead of t because in some instantiations of this signature, u
       ignores its second parameter. In order to avoid needed to apply a useless type
       parameter for those instantiations, we define [type t = unit t_poly] in them. In
       instantiations where the polymorphism is actually used, we define
       [type 'a t = 'a t_poly] *)
    type 'a t_poly =
      { locality : (Mode.Locality.Const.t, 'a) u;
        linearity : (Mode.Linearity.Const.t, 'a) u;
        uniqueness : (Mode.Uniqueness.Const.t, 'a) u;
        portability : (Mode.Portability.Const.t, 'a) u;
        contention : (Mode.Contention.Const.t, 'a) u;
        yielding : (Mode.Yielding.Const.t, 'a) u;
        externality : (Externality.t, 'a) u;
        nullability : (Nullability.t, 'a) u
      }

    val get : axis:'a Axis.t -> 'b t_poly -> ('a, 'b) u

    val set : axis:'a Axis.t -> 'b t_poly -> ('a, 'b) u -> 'b t_poly

    (** Create an axis collection by applying the function on each axis *)
    module Create : sig
      module Monadic (M : Misc.Stdlib.Monad.S) : sig
        type 'a f = { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) u M.t }
        [@@unboxed]

        val f : 'a f -> 'a t_poly M.t
      end

      (** This record type is used to pass a polymorphic function to [create] *)
      type 'a f = 'a Monadic(Misc.Stdlib.Monad.Identity).f

      val f : 'a f -> 'a t_poly
    end

    (** Map an operation over all the bounds *)
    module Map : sig
      module Monadic (M : Misc.Stdlib.Monad.S) : sig
        type ('a, 'b) f =
          { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) u -> ('axis, 'b) u M.t }
        [@@unboxed]

        val f : ('a, 'b) f -> 'a t_poly -> 'b t_poly M.t
      end

      type ('a, 'b) f = ('a, 'b) Monadic(Misc.Stdlib.Monad.Identity).f

      val f : ('a, 'b) f -> 'a t_poly -> 'b t_poly
    end

    module Iter : sig
      type 'a f = { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) u -> unit }
      [@@unboxed]

      val f : 'a f -> 'a t_poly -> unit
    end

    (** Map an operation over two sets of bounds *)
    module Map2 : sig
      module Monadic (M : Misc.Stdlib.Monad.S) : sig
        type ('a, 'b, 'c) f =
          { f :
              'axis.
              axis:'axis Axis.t ->
              ('axis, 'a) u ->
              ('axis, 'b) u ->
              ('axis, 'c) u M.t
          }
        [@@unboxed]

        val f : ('a, 'b, 'c) f -> 'a t_poly -> 'b t_poly -> 'c t_poly M.t
      end

      type ('a, 'b, 'c) f = ('a, 'b, 'c) Monadic(Misc.Stdlib.Monad.Identity).f

      val f : ('a, 'b, 'c) f -> 'a t_poly -> 'b t_poly -> 'c t_poly
    end

    (** Fold an operation over the bounds to a summary value *)
    module Fold : sig
      type ('a, 'r) f = { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) u -> 'r }
      [@@unboxed]

      (** [combine] should be commutative and associative. *)
      val f : ('a, 'r) f -> 'a t_poly -> combine:('r -> 'r -> 'r) -> 'r
    end

    (** Fold an operation over two sets of bounds to a summary value *)
    module Fold2 : sig
      type ('a, 'b, 'r) f =
        { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) u -> ('axis, 'b) u -> 'r }
      [@@unboxed]

      (** [combine] should be commutative and associative. *)
      val f :
        ('a, 'b, 'r) f ->
        'a t_poly ->
        'b t_poly ->
        combine:('r -> 'r -> 'r) ->
        'r
    end
  end

  module type S_poly := sig
    include S_gen

    type 'a t = 'a t_poly
  end

  module type S_mono := sig
    include S_gen

    type t = unit t_poly
  end

  module Indexed_gen (T : Misc.T2) : S_poly with type ('a, 'b) u := ('a, 'b) T.t

  module Indexed (T : Misc.T1) : S_mono with type ('a, 'b) u := 'a T.t

  module Identity : S_mono with type ('a, 'b) u := 'a

  include S_poly with type ('a, 'b) u := 'b

  val create : f:(axis:Axis.packed -> 'a) -> 'a t

  val get : axis:'ax Axis.t -> 'a t -> 'a

  val set : axis:'ax Axis.t -> 'a t -> 'a -> 'a t

  val mapi : f:(axis:Axis.packed -> 'a -> 'a) -> 'a t -> 'a t

  val map : f:('a -> 'a) -> 'a t -> 'a t

  val fold :
    f:(axis:Axis.packed -> 'a -> 'r) -> combine:('r -> 'r -> 'r) -> 'a t -> 'r
end

module Axis_set : sig
  (** A set of [Axis.t], represented as a bitfield for efficiency. *)
  type t [@@immediate]

  val empty : t

  val singleton : _ Axis.t -> t

  val is_empty : t -> bool

  val add : t -> _ Axis.t -> t

  val remove : t -> _ Axis.t -> t

  val mem : t -> _ Axis.t -> bool

  val union : t -> t -> t

  val intersection : t -> t -> t

  val diff : t -> t -> t

  val is_subset : t -> t -> bool

  val complement : t -> t

  val to_seq : t -> Axis.packed Seq.t

  val to_list : t -> Axis.packed list

  (** Create a [t], specify for each axis whether it should be included *)
  val create : f:(axis:Axis.packed -> bool) -> t

  (** A set of all monadic modal axes *)
  val all_monadic_axes : t

  (** A set of all comonadic modal axes *)
  val all_comonadic_axes : t

  val print : Format.formatter -> t -> unit
end
