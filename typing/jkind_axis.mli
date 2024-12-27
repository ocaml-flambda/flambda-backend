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
  (* CR zqian: remove this and use [Mode.Alloc.axis] instead *)
  module Modal : sig
    type 'a t =
      | Locality : Mode.Locality.Const.t t
      | Linearity : Mode.Linearity.Const.t t
      | Uniqueness : Mode.Uniqueness.Const.t t
      | Portability : Mode.Portability.Const.t t
      | Contention : Mode.Contention.Const.t t
  end

  module Nonmodal : sig
    type 'a t =
      | Externality : Externality.t t
      | Nullability : Nullability.t t
  end

  (** Represents an axis of a jkind *)
  type 'a t =
    | Modal of 'a Modal.t
    | Nonmodal of 'a Nonmodal.t

  type packed = Pack : 'a t -> packed

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

(** [Axed] describes a type that is parameterized by axis. *)
module type Axed = sig
  type (+'type_expr, 'd, 'axis) t constraint 'd = 'l * 'r
end

(** A collection with one item for each jkind axis.
    [T] parametrizes what element is being held for each axis. *)
module Axis_collection (T : Axed) : sig
  (** [t] is parameterized over 'type_expr to enable usages in
        [jkind_types.mli].  It is tempting to make those usages instead push the
        [`type_expr] into the functor arg [T], but this leads to issues at
        usages of [Jkind.t] in [types.mli] due to recursive definitions. *)
  type (+'type_expr, 'd) t =
    { locality : ('type_expr, 'd, Mode.Locality.Const.t) T.t;
      linearity : ('type_expr, 'd, Mode.Linearity.Const.t) T.t;
      uniqueness : ('type_expr, 'd, Mode.Uniqueness.Const.t) T.t;
      portability : ('type_expr, 'd, Mode.Portability.Const.t) T.t;
      contention : ('type_expr, 'd, Mode.Contention.Const.t) T.t;
      externality : ('type_expr, 'd, Externality.t) T.t;
      nullability : ('type_expr, 'd, Nullability.t) T.t
    }

  val get : axis:'a Axis.t -> ('type_expr, 'd) t -> ('type_expr, 'd, 'a) T.t

  val set :
    axis:'a Axis.t ->
    ('type_expr, 'd) t ->
    ('type_expr, 'd, 'a) T.t ->
    ('type_expr, 'd) t

  (** Create an axis collection by applying the function on each axis *)
  module Create : sig
    module Monadic (M : Misc.Stdlib.Monad.S) : sig
      type ('type_expr, 'd) f =
        { f : 'axis. axis:'axis Axis.t -> ('type_expr, 'd, 'axis) T.t M.t }
      [@@unboxed]

      val f : ('type_expr, 'd) f -> ('type_expr, 'd) t M.t
    end

    (** This record type is used to pass a polymorphic function to [create] *)
    type ('type_expr, 'd) f =
      ('type_expr, 'd) Monadic(Misc.Stdlib.Monad.Identity).f

    val f : ('type_expr, 'd) f -> ('type_expr, 'd) t
  end

  (** Map an operation over all the bounds *)
  module Map : sig
    module Monadic (M : Misc.Stdlib.Monad.S) : sig
      type ('type_expr, 'd1, 'd2) f =
        { f :
            'axis.
            axis:'axis Axis.t ->
            ('type_expr, 'd1, 'axis) T.t ->
            ('type_expr, 'd2, 'axis) T.t M.t
        }
      [@@unboxed]

      val f :
        ('type_expr, 'd1, 'd2) f ->
        ('type_expr, 'd1) t ->
        ('type_expr, 'd2) t M.t
    end

    type ('type_expr, 'd1, 'd2) f =
      ('type_expr, 'd1, 'd2) Monadic(Misc.Stdlib.Monad.Identity).f

    val f :
      ('type_expr, 'd1, 'd2) f -> ('type_expr, 'd1) t -> ('type_expr, 'd2) t
  end

  (** Map an operation over two sets of bounds *)
  module Map2 : sig
    module Monadic (M : Misc.Stdlib.Monad.S) : sig
      type ('type_expr, 'd1, 'd2, 'd3) f =
        { f :
            'axis.
            axis:'axis Axis.t ->
            ('type_expr, 'd1, 'axis) T.t ->
            ('type_expr, 'd2, 'axis) T.t ->
            ('type_expr, 'd3, 'axis) T.t M.t
        }
      [@@unboxed]

      val f :
        ('type_expr, 'd1, 'd2, 'd3) f ->
        ('type_expr, 'd1) t ->
        ('type_expr, 'd2) t ->
        ('type_expr, 'd3) t M.t
    end

    type ('type_expr, 'd1, 'd2, 'd3) f =
      ('type_expr, 'd1, 'd2, 'd3) Monadic(Misc.Stdlib.Monad.Identity).f

    val f :
      ('type_expr, 'd1, 'd2, 'd3) f ->
      ('type_expr, 'd1) t ->
      ('type_expr, 'd2) t ->
      ('type_expr, 'd3) t
  end

  (** Fold an operation over the bounds to a summary value *)
  module Fold : sig
    type ('type_expr, 'd, 'r) f =
      { f : 'axis. axis:'axis Axis.t -> ('type_expr, 'd, 'axis) T.t -> 'r }
    [@@unboxed]

    (** [combine] should be associative. *)
    val f :
      ('type_expr, 'd, 'r) f ->
      ('type_expr, 'd) t ->
      combine:('r -> 'r -> 'r) ->
      'r
  end

  (** Fold an operation over two sets of bounds to a summary value *)
  module Fold2 : sig
    type ('type_expr, 'd1, 'd2, 'r) f =
      { f :
          'axis.
          axis:'axis Axis.t ->
          ('type_expr, 'd1, 'axis) T.t ->
          ('type_expr, 'd2, 'axis) T.t ->
          'r
      }
    [@@unboxed]

    (** [combine] should be associative. *)
    val f :
      ('type_expr, 'd1, 'd2, 'r) f ->
      ('type_expr, 'd1) t ->
      ('type_expr, 'd2) t ->
      combine:('r -> 'r -> 'r) ->
      'r
  end
end
