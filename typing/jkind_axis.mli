(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
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

  val is_deep : _ t -> bool

  (* CR layouts v2.8: Not sure this belongs here, but there's not another obvious spot. Once this
     file is more aligned with axis treatment in mode.ml, possibly re-home this. *)
  val modality_is_const_for_axis : _ t -> Mode.Modality.Value.Const.t -> bool
end

(** A collection with one item for each jkind axis *)
module Axis_collection : sig
  type 'a t =
    { locality : 'a;
      linearity : 'a;
      uniqueness : 'a;
      portability : 'a;
      contention : 'a;
      externality : 'a;
      nullability : 'a
    }

  val create : f:(axis:Axis.packed -> 'a) -> 'a t

  val get : axis:'ax Axis.t -> 'a t -> 'a

  val set : axis:'ax Axis.t -> 'a t -> 'a -> 'a t

  val mapi : f:(axis:Axis.packed -> 'a -> 'a) -> 'a t -> 'a t

  val map : f:('a -> 'a) -> 'a t -> 'a t

  val fold :
    f:(axis:Axis.packed -> 'a -> 'r) -> combine:('r -> 'r -> 'r) -> 'a t -> 'r

  (** A collection with one item for each jkind axis, where the value type is indexed by the
      particular axis. *)
  module Indexed (T : Misc.T1) : sig
    type t =
      { locality : Mode.Locality.Const.t T.t;
        linearity : Mode.Linearity.Const.t T.t;
        uniqueness : Mode.Uniqueness.Const.t T.t;
        portability : Mode.Portability.Const.t T.t;
        contention : Mode.Contention.Const.t T.t;
        externality : Externality.t T.t;
        nullability : Nullability.t T.t
      }

    val get : axis:'a Axis.t -> t -> 'a T.t

    val set : axis:'a Axis.t -> t -> 'a T.t -> t

    (** Create an axis collection by applying the function on each axis *)
    module Create : sig
      module Monadic (M : Misc.Stdlib.Monad.S) : sig
        type f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t M.t } [@@unboxed]

        val f : f -> t M.t
      end

      (** This record type is used to pass a polymorphic function to [create] *)
      type f = Monadic(Misc.Stdlib.Monad.Identity).f

      val f : f -> t
    end

    (** Map an operation over all the bounds *)
    module Map : sig
      module Monadic (M : Misc.Stdlib.Monad.S) : sig
        type f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> 'axis T.t M.t }
        [@@unboxed]

        val f : f -> t -> t M.t
      end

      type f = Monadic(Misc.Stdlib.Monad.Identity).f

      val f : f -> t -> t
    end

    module Iter : sig
      type f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> unit }

      val f : f -> t -> unit
    end

    (** Map an operation over two sets of bounds *)
    module Map2 : sig
      module Monadic (M : Misc.Stdlib.Monad.S) : sig
        type f =
          { f :
              'axis.
              axis:'axis Axis.t -> 'axis T.t -> 'axis T.t -> 'axis T.t M.t
          }
        [@@unboxed]

        val f : f -> t -> t -> t M.t
      end

      type f = Monadic(Misc.Stdlib.Monad.Identity).f

      val f : f -> t -> t -> t
    end

    (** Fold an operation over the bounds to a summary value *)
    module Fold : sig
      type 'r f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> 'r }
      [@@unboxed]

      (** [combine] should be commutative and associative. *)
      val f : 'r f -> t -> combine:('r -> 'r -> 'r) -> 'r
    end

    (** Fold an operation over two sets of bounds to a summary value *)
    module Fold2 : sig
      type 'r f =
        { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> 'axis T.t -> 'r }
      [@@unboxed]

      (** [combine] should be commutative and associative. *)
      val f : 'r f -> t -> t -> combine:('r -> 'r -> 'r) -> 'r
    end
  end
end

module Axis_set : sig
  type t

  val empty : t

  val add : t -> Axis.packed -> t

  val union : t -> t -> t

  val intersection : t -> t -> t

  val is_subset : t -> t -> bool

  val to_list : t -> Axis.packed list
end
