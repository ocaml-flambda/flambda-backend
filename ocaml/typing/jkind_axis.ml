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

module Externality = struct
  type t =
    | External
    | External64
    | Internal

  let max = Internal

  let min = External

  let equal e1 e2 =
    match e1, e2 with
    | External, External -> true
    | External64, External64 -> true
    | Internal, Internal -> true
    | (External | External64 | Internal), _ -> false

  let less_or_equal t1 t2 : Misc.Le_result.t =
    match t1, t2 with
    | External, External -> Equal
    | External, (External64 | Internal) -> Less
    | External64, External -> Not_le
    | External64, External64 -> Equal
    | External64, Internal -> Less
    | Internal, (External | External64) -> Not_le
    | Internal, Internal -> Equal

  let le t1 t2 = Misc.Le_result.is_le (less_or_equal t1 t2)

  let meet t1 t2 =
    match t1, t2 with
    | External, (External | External64 | Internal)
    | (External64 | Internal), External ->
      External
    | External64, (External64 | Internal) | Internal, External64 -> External64
    | Internal, Internal -> Internal

  let join t1 t2 =
    match t1, t2 with
    | Internal, (Internal | External64 | External)
    | (External64 | External), Internal ->
      Internal
    | External64, (External64 | External) | External, External64 -> External64
    | External, External -> External

  let print ppf = function
    | External -> Format.fprintf ppf "external_"
    | External64 -> Format.fprintf ppf "external64"
    | Internal -> Format.fprintf ppf "internal"
end

module Nullability = struct
  type t =
    | Non_null
    | Maybe_null

  let max = Maybe_null

  let min = Non_null

  let equal n1 n2 =
    match n1, n2 with
    | Non_null, Non_null -> true
    | Maybe_null, Maybe_null -> true
    | (Non_null | Maybe_null), _ -> false

  let less_or_equal n1 n2 : Misc.Le_result.t =
    match n1, n2 with
    | Non_null, Non_null -> Equal
    | Non_null, Maybe_null -> Less
    | Maybe_null, Non_null -> Not_le
    | Maybe_null, Maybe_null -> Equal

  let le n1 n2 = Misc.Le_result.is_le (less_or_equal n1 n2)

  let meet n1 n2 =
    match n1, n2 with
    | Non_null, (Non_null | Maybe_null) | Maybe_null, Non_null -> Non_null
    | Maybe_null, Maybe_null -> Maybe_null

  let join n1 n2 =
    match n1, n2 with
    | Maybe_null, (Maybe_null | Non_null) | Non_null, Maybe_null -> Maybe_null
    | Non_null, Non_null -> Non_null

  let print ppf = function
    | Non_null -> Format.fprintf ppf "non_null"
    | Maybe_null -> Format.fprintf ppf "maybe_null"
end

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

module Axis = struct
  module Modal = struct
    type 'a t =
      | Locality : Mode.Locality.Const.t t
      | Linearity : Mode.Linearity.Const.t t
      | Uniqueness : Mode.Uniqueness.Const.t t
      | Portability : Mode.Portability.Const.t t
      | Contention : Mode.Contention.Const.t t
  end

  module Nonmodal = struct
    type 'a t =
      | Externality : Externality.t t
      | Nullability : Nullability.t t
  end

  type 'a t =
    | Modal of 'a Modal.t
    | Nonmodal of 'a Nonmodal.t

  type packed = Pack : 'a t -> packed

  module Accent_lattice (M : Mode_intf.Lattice) = struct
    (* A functor to add some convenient functions to modal axes *)
    include M

    let less_or_equal a b : Misc.Le_result.t =
      match le a b, le b a with
      | true, true -> Equal
      | true, false -> Less
      | false, _ -> Not_le

    let equal a b = Misc.Le_result.is_equal (less_or_equal a b)
  end

  let get (type a) : a t -> (module Axis_s with type t = a) = function
    | Modal Locality ->
      (module Accent_lattice (Mode.Locality.Const) : Axis_s with type t = a)
    | Modal Linearity ->
      (module Accent_lattice (Mode.Linearity.Const) : Axis_s with type t = a)
    | Modal Uniqueness ->
      (module Accent_lattice (Mode.Uniqueness.Const) : Axis_s with type t = a)
    | Modal Portability ->
      (module Accent_lattice (Mode.Portability.Const) : Axis_s with type t = a)
    | Modal Contention ->
      (module Accent_lattice (Mode.Contention.Const) : Axis_s with type t = a)
    | Nonmodal Externality -> (module Externality : Axis_s with type t = a)
    | Nonmodal Nullability -> (module Nullability : Axis_s with type t = a)

  let all =
    [ Pack (Modal Locality);
      Pack (Modal Linearity);
      Pack (Modal Uniqueness);
      Pack (Modal Portability);
      Pack (Modal Contention);
      Pack (Nonmodal Externality);
      Pack (Nonmodal Nullability) ]

  let name (type a) : a t -> string = function
    | Modal Locality -> "locality"
    | Modal Linearity -> "linearity"
    | Modal Uniqueness -> "uniqueness"
    | Modal Portability -> "portability"
    | Modal Contention -> "contention"
    | Nonmodal Externality -> "externality"
    | Nonmodal Nullability -> "nullability"

  let is_deep (type a) : a t -> bool = function
    | Modal Locality -> true
    | Modal Linearity -> true
    | Modal Uniqueness -> true
    | Modal Portability -> true
    | Modal Contention -> true
    | Nonmodal Externality -> true
    | Nonmodal Nullability -> false
end

(* Sadly this needs to be functorized since we don't have higher-kinded types *)
module Axis_collection (T : Misc.T2) = struct
  type 'type_expr t =
    { locality : ('type_expr, Mode.Locality.Const.t) T.t;
      linearity : ('type_expr, Mode.Linearity.Const.t) T.t;
      uniqueness : ('type_expr, Mode.Uniqueness.Const.t) T.t;
      portability : ('type_expr, Mode.Portability.Const.t) T.t;
      contention : ('type_expr, Mode.Contention.Const.t) T.t;
      externality : ('type_expr, Externality.t) T.t;
      nullability : ('type_expr, Nullability.t) T.t
    }

  let get (type a) ~(axis : a Axis.t) values : (_, a) T.t =
    match axis with
    | Modal Locality -> values.locality
    | Modal Linearity -> values.linearity
    | Modal Uniqueness -> values.uniqueness
    | Modal Portability -> values.portability
    | Modal Contention -> values.contention
    | Nonmodal Externality -> values.externality
    | Nonmodal Nullability -> values.nullability

  let set (type a) ~(axis : a Axis.t) values (value : (_, a) T.t) =
    match axis with
    | Modal Locality -> { values with locality = value }
    | Modal Linearity -> { values with linearity = value }
    | Modal Uniqueness -> { values with uniqueness = value }
    | Modal Portability -> { values with portability = value }
    | Modal Contention -> { values with contention = value }
    | Nonmodal Externality -> { values with externality = value }
    | Nonmodal Nullability -> { values with nullability = value }

  (* Since we don't have polymorphic parameters, use a record to pass the polymorphic
     function *)
  module Create_f = struct
    type 'type_expr t = { f : 'a. axis:'a Axis.t -> ('type_expr, 'a) T.t }
  end

  let create ({ f } : _ Create_f.t) =
    { locality = f ~axis:Axis.(Modal Locality);
      linearity = f ~axis:Axis.(Modal Linearity);
      uniqueness = f ~axis:Axis.(Modal Uniqueness);
      portability = f ~axis:Axis.(Modal Portability);
      contention = f ~axis:Axis.(Modal Contention);
      externality = f ~axis:Axis.(Nonmodal Externality);
      nullability = f ~axis:Axis.(Nonmodal Nullability)
    }
end
