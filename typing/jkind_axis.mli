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

(** The common interface for jkind axes *)
module type Axis_s = sig
  type t

  val max : t

  val min : t

  val equal : t -> t -> bool

  val less_or_equal : t -> t -> Misc.Le_result.t

  val le : t -> t -> bool

  val meet : t -> t -> t

  val join : t -> t -> t

  val print : Format.formatter -> t -> unit
end

(** The jkind axis of Externality *)
module Externality : sig
  type t =
    | External
    | External64
    | Internal

  include Axis_s with type t := t
end

(** The jkind axis of nullability *)
module Nullability : sig
  type t =
    | Non_null
    | Maybe_null

  include Axis_s with type t := t
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
  val get : 'a t -> (module Axis_s with type t = 'a)

  val all : packed list

  val name : _ t -> string
end

(** A collection with one item for each jkind axis.
    [T] parametizes what element is being held for each axis. *)
module Axis_collection (T : Misc.T1) : sig
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

  module Create_f : sig
    (** This record type is used to pass a polymorphic function to [create] *)
    type t = { f : 'a. axis:'a Axis.t -> 'a T.t }
  end

  (** Create an axis collection by applying the function on each axis *)
  val create : Create_f.t -> t
end
