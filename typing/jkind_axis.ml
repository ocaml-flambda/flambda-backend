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

module type Lattice = Mode_intf.Lattice

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
  module Nonmodal = struct
    type 'a t =
      | Externality : Externality.t t
      | Nullability : Nullability.t t
  end

  type 'a t =
    | Modal : ('m, 'a, 'd) Mode.Alloc.axis -> 'a t
    | Nonmodal : 'a Nonmodal.t -> 'a t

  type packed = Pack : 'a t -> packed

  let get (type a) : a t -> (module Lattice with type t = a) = function
    | Modal axis -> Mode.Alloc.lattice_of_axis axis
    | Nonmodal Externality -> (module Externality : Lattice with type t = a)
    | Nonmodal Nullability -> (module Nullability : Lattice with type t = a)

  let all =
    [ Pack (Modal (Comonadic Areality));
      Pack (Modal (Monadic Uniqueness));
      Pack (Modal (Comonadic Linearity));
      Pack (Modal (Monadic Contention));
      Pack (Modal (Comonadic Portability));
      Pack (Nonmodal Externality);
      Pack (Nonmodal Nullability) ]

  let name (type a) : a t -> string = function
    | Modal axis -> Format.asprintf "%a" Mode.Alloc.print_axis axis
    | Nonmodal Externality -> "externality"
    | Nonmodal Nullability -> "nullability"

  let is_deep (type a) : a t -> bool = function
    | Modal (Comonadic Areality) -> true
    | Modal (Comonadic Linearity) -> true
    | Modal (Monadic Uniqueness) -> true
    | Modal (Comonadic Portability) -> true
    | Modal (Monadic Contention) -> true
    | Nonmodal Externality -> true
    | Nonmodal Nullability -> false

  let modality_is_const_for_axis (type a) (t : a t)
      (modality : Mode.Modality.Value.Const.t) =
    match t with
    | Nonmodal Nullability | Nonmodal Externality -> false
    | Modal axis ->
      let (P axis) = Mode.Const.Axis.alloc_as_value axis in
      Mode.Modality.Value.Const.is_constant_for axis modality
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
      externality : ('type_expr, 'd, Externality.t) T.t;
      nullability : ('type_expr, 'd, Nullability.t) T.t
    }

  let get (type a) ~(axis : a Axis.t) values : (_, _, a) T.t =
    match axis with
    | Modal (Comonadic Areality) -> values.locality
    | Modal (Comonadic Linearity) -> values.linearity
    | Modal (Monadic Uniqueness) -> values.uniqueness
    | Modal (Comonadic Portability) -> values.portability
    | Modal (Monadic Contention) -> values.contention
    | Nonmodal Externality -> values.externality
    | Nonmodal Nullability -> values.nullability

  let set (type a) ~(axis : a Axis.t) values (value : (_, _, a) T.t) =
    match axis with
    | Modal (Comonadic Areality) -> { values with locality = value }
    | Modal (Comonadic Linearity) -> { values with linearity = value }
    | Modal (Monadic Uniqueness) -> { values with uniqueness = value }
    | Modal (Comonadic Portability) -> { values with portability = value }
    | Modal (Monadic Contention) -> { values with contention = value }
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
        let* locality = f ~axis:Axis.(Modal (Comonadic Areality)) in
        let* uniqueness = f ~axis:Axis.(Modal (Monadic Uniqueness)) in
        let* linearity = f ~axis:Axis.(Modal (Comonadic Linearity)) in
        let* contention = f ~axis:Axis.(Modal (Monadic Contention)) in
        let* portability = f ~axis:Axis.(Modal (Comonadic Portability)) in
        let* externality = f ~axis:Axis.(Nonmodal Externality) in
        let* nullability = f ~axis:Axis.(Nonmodal Nullability) in
        M.return
          { locality;
            uniqueness;
            linearity;
            contention;
            portability;
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
          externality;
          nullability
        } ~combine =
      combine (f ~axis:Axis.(Modal (Comonadic Areality)) locality)
      @@ combine (f ~axis:Axis.(Modal (Monadic Uniqueness)) uniqueness)
      @@ combine (f ~axis:Axis.(Modal (Comonadic Linearity)) linearity)
      @@ combine (f ~axis:Axis.(Modal (Monadic Contention)) contention)
      @@ combine (f ~axis:Axis.(Modal (Comonadic Portability)) portability)
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
          externality = ext1;
          nullability = nul1
        }
        { locality = loc2;
          linearity = lin2;
          uniqueness = uni2;
          portability = por2;
          contention = con2;
          externality = ext2;
          nullability = nul2
        } ~combine =
      combine (f ~axis:Axis.(Modal (Comonadic Areality)) loc1 loc2)
      @@ combine (f ~axis:Axis.(Modal (Monadic Uniqueness)) uni1 uni2)
      @@ combine (f ~axis:Axis.(Modal (Comonadic Linearity)) lin1 lin2)
      @@ combine (f ~axis:Axis.(Modal (Monadic Contention)) con1 con2)
      @@ combine (f ~axis:Axis.(Modal (Comonadic Portability)) por1 por2)
      @@ combine (f ~axis:Axis.(Nonmodal Externality) ext1 ext2)
      @@ f ~axis:Axis.(Nonmodal Nullability) nul1 nul2
  end
end
