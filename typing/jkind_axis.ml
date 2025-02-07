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

module type Axis_ops = sig
  include Mode_intf.Lattice

  val less_or_equal : t -> t -> Misc.Le_result.t

  val equal : t -> t -> bool
end

module Externality = struct
  type t =
    | External
    | External64
    | Internal

  let max = Internal

  let min = External

  let legacy = Internal

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

  let legacy = Non_null

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

module Axis = struct
  module Modal = struct
    type 'a t =
      | Locality : Mode.Locality.Const.t t
      | Linearity : Mode.Linearity.Const.t t
      | Uniqueness : Mode.Uniqueness.Const.t t
      | Portability : Mode.Portability.Const.t t
      | Contention : Mode.Contention.Const.t t
      | Yielding : Mode.Yielding.Const.t t
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

  module Accent_lattice (M : Mode_intf.Lattice) : Axis_ops with type t = M.t =
  struct
    (* A functor to add some convenient functions to modal axes *)
    include M

    let less_or_equal a b = Misc.Le_result.less_or_equal ~le a b

    let equal a b = Misc.Le_result.equal ~le a b
  end

  let get (type a) : a t -> (module Axis_ops with type t = a) = function
    | Modal Locality -> (module Accent_lattice (Mode.Locality.Const))
    | Modal Linearity -> (module Accent_lattice (Mode.Linearity.Const))
    | Modal Uniqueness -> (module Accent_lattice (Mode.Uniqueness.Const))
    | Modal Portability -> (module Accent_lattice (Mode.Portability.Const))
    | Modal Contention -> (module Accent_lattice (Mode.Contention.Const))
    | Modal Yielding -> (module Accent_lattice (Mode.Yielding.Const))
    | Nonmodal Externality -> (module Externality)
    | Nonmodal Nullability -> (module Nullability)

  let all =
    [ Pack (Modal Locality);
      Pack (Modal Uniqueness);
      Pack (Modal Linearity);
      Pack (Modal Contention);
      Pack (Modal Portability);
      Pack (Modal Yielding);
      Pack (Nonmodal Externality);
      Pack (Nonmodal Nullability) ]

  let name (type a) : a t -> string = function
    | Modal Locality -> "locality"
    | Modal Linearity -> "linearity"
    | Modal Uniqueness -> "uniqueness"
    | Modal Portability -> "portability"
    | Modal Contention -> "contention"
    | Modal Yielding -> "yielding"
    | Nonmodal Externality -> "externality"
    | Nonmodal Nullability -> "nullability"

  let is_modal (type a) : a t -> bool = function
    | Modal Locality -> true
    | Modal Linearity -> true
    | Modal Uniqueness -> true
    | Modal Portability -> true
    | Modal Contention -> true
    | Modal Yielding -> true
    | Nonmodal Externality -> true
    | Nonmodal Nullability -> false
end

module type Axed = sig
  type (+'type_expr, 'd, 'axis) t constraint 'd = 'l * 'r
end

(* Sadly this needs to be functorized since we don't have higher-kinded types *)
module Axis_collection (T : Axed) = struct
  type (+'type_expr, 'd) t =
    { locality : ('type_expr, 'd, Mode.Locality.Const.t) T.t;
      linearity : ('type_expr, 'd, Mode.Linearity.Const.t) T.t;
      uniqueness : ('type_expr, 'd, Mode.Uniqueness.Const.t) T.t;
      portability : ('type_expr, 'd, Mode.Portability.Const.t) T.t;
      contention : ('type_expr, 'd, Mode.Contention.Const.t) T.t;
      yielding : ('type_expr, 'd, Mode.Yielding.Const.t) T.t;
      externality : ('type_expr, 'd, Externality.t) T.t;
      nullability : ('type_expr, 'd, Nullability.t) T.t
    }

  let get (type a) ~(axis : a Axis.t) values : (_, _, a) T.t =
    match axis with
    | Modal Locality -> values.locality
    | Modal Linearity -> values.linearity
    | Modal Uniqueness -> values.uniqueness
    | Modal Portability -> values.portability
    | Modal Contention -> values.contention
    | Modal Yielding -> values.yielding
    | Nonmodal Externality -> values.externality
    | Nonmodal Nullability -> values.nullability

  let set (type a) ~(axis : a Axis.t) values (value : (_, _, a) T.t) =
    match axis with
    | Modal Locality -> { values with locality = value }
    | Modal Linearity -> { values with linearity = value }
    | Modal Uniqueness -> { values with uniqueness = value }
    | Modal Portability -> { values with portability = value }
    | Modal Contention -> { values with contention = value }
    | Modal Yielding -> { values with yielding = value }
    | Nonmodal Externality -> { values with externality = value }
    | Nonmodal Nullability -> { values with nullability = value }

  (* Since we don't have polymorphic parameters, use a record to pass the
     polymorphic function *)
  module Create = struct
    module Monadic (M : Misc.Stdlib.Monad.S) = struct
      type ('type_expr, 'd) f =
        { f : 'axis. axis:'axis Axis.t -> ('type_expr, 'd, 'axis) T.t M.t }
      [@@unboxed]

      let[@inline] f { f } =
        let open M.Syntax in
        let* locality = f ~axis:Axis.(Modal Locality) in
        let* uniqueness = f ~axis:Axis.(Modal Uniqueness) in
        let* linearity = f ~axis:Axis.(Modal Linearity) in
        let* contention = f ~axis:Axis.(Modal Contention) in
        let* portability = f ~axis:Axis.(Modal Portability) in
        let* yielding = f ~axis:Axis.(Modal Yielding) in
        let* externality = f ~axis:Axis.(Nonmodal Externality) in
        let* nullability = f ~axis:Axis.(Nonmodal Nullability) in
        M.return
          { locality;
            uniqueness;
            linearity;
            contention;
            portability;
            yielding;
            externality;
            nullability
          }
    end
    [@@inline]

    module Monadic_identity = Monadic (Misc.Stdlib.Monad.Identity)

    type ('type_expr, 'd) f = ('type_expr, 'd) Monadic_identity.f

    let[@inline] f f = Monadic_identity.f f
  end

  module Map = struct
    module Monadic (M : Misc.Stdlib.Monad.S) = struct
      type ('type_expr, 'd1, 'd2) f =
        { f :
            'axis.
            axis:'axis Axis.t ->
            ('type_expr, 'd1, 'axis) T.t ->
            ('type_expr, 'd2, 'axis) T.t M.t
        }
      [@@unboxed]

      module Create = Create.Monadic (M)

      let[@inline] f { f } bounds =
        Create.f { f = (fun ~axis -> f ~axis (get ~axis bounds)) }
    end
    [@@inline]

    module Monadic_identity = Monadic (Misc.Stdlib.Monad.Identity)

    type ('type_expr, 'd1, 'd2) f = ('type_expr, 'd1, 'd2) Monadic_identity.f

    let[@inline] f f bounds = Monadic_identity.f f bounds
  end

  module Map2 = struct
    module Monadic (M : Misc.Stdlib.Monad.S) = struct
      type ('type_expr, 'd1, 'd2, 'd3) f =
        { f :
            'axis.
            axis:'axis Axis.t ->
            ('type_expr, 'd1, 'axis) T.t ->
            ('type_expr, 'd2, 'axis) T.t ->
            ('type_expr, 'd3, 'axis) T.t M.t
        }
      [@@unboxed]

      module Create = Create.Monadic (M)

      let[@inline] f { f } bounds1 bounds2 =
        Create.f
          { f = (fun ~axis -> f ~axis (get ~axis bounds1) (get ~axis bounds2)) }
    end
    [@@inline]

    module Monadic_identity = Monadic (Misc.Stdlib.Monad.Identity)

    type ('type_expr, 'd1, 'd2, 'd3) f =
      ('type_expr, 'd1, 'd2, 'd3) Monadic_identity.f

    let[@inline] f f bounds1 bounds2 = Monadic_identity.f f bounds1 bounds2
  end

  module Fold = struct
    type ('type_expr, 'd, 'r) f =
      { f : 'axis. axis:'axis Axis.t -> ('type_expr, 'd, 'axis) T.t -> 'r }
    [@@unboxed]

    let[@inline] f { f }
        { locality;
          linearity;
          uniqueness;
          portability;
          contention;
          yielding;
          externality;
          nullability
        } ~combine =
      combine (f ~axis:Axis.(Modal Locality) locality)
      @@ combine (f ~axis:Axis.(Modal Uniqueness) uniqueness)
      @@ combine (f ~axis:Axis.(Modal Linearity) linearity)
      @@ combine (f ~axis:Axis.(Modal Contention) contention)
      @@ combine (f ~axis:Axis.(Modal Portability) portability)
      @@ combine (f ~axis:Axis.(Modal Yielding) yielding)
      @@ combine (f ~axis:Axis.(Nonmodal Externality) externality)
      @@ f ~axis:Axis.(Nonmodal Nullability) nullability
  end

  module Fold2 = struct
    type ('type_expr, 'd1, 'd2, 'r) f =
      { f :
          'axis.
          axis:'axis Axis.t ->
          ('type_expr, 'd1, 'axis) T.t ->
          ('type_expr, 'd2, 'axis) T.t ->
          'r
      }
    [@@unboxed]

    let[@inline] f { f }
        { locality = loc1;
          linearity = lin1;
          uniqueness = uni1;
          portability = por1;
          contention = con1;
          yielding = yie1;
          externality = ext1;
          nullability = nul1
        }
        { locality = loc2;
          linearity = lin2;
          uniqueness = uni2;
          portability = por2;
          contention = con2;
          yielding = yie2;
          externality = ext2;
          nullability = nul2
        } ~combine =
      combine (f ~axis:Axis.(Modal Locality) loc1 loc2)
      @@ combine (f ~axis:Axis.(Modal Uniqueness) uni1 uni2)
      @@ combine (f ~axis:Axis.(Modal Linearity) lin1 lin2)
      @@ combine (f ~axis:Axis.(Modal Contention) con1 con2)
      @@ combine (f ~axis:Axis.(Modal Portability) por1 por2)
      @@ combine (f ~axis:Axis.(Modal Yielding) yie1 yie2)
      @@ combine (f ~axis:Axis.(Nonmodal Externality) ext1 ext2)
      @@ f ~axis:Axis.(Nonmodal Nullability) nul1 nul2
  end
end
