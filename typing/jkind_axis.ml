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
  module Nonmodal = struct
    type 'a t =
      | Externality : Externality.t t
      | Nullability : Nullability.t t
  end

  type 'a t =
    | Modal : ('m, 'a, 'd) Mode.Alloc.axis -> 'a t
    | Nonmodal : 'a Nonmodal.t -> 'a t

  type packed = Pack : 'a t -> packed [@@unboxed]

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

  let get (type a) : a t -> (module Axis_ops with type t = a) = function
    | Modal axis ->
      (module Accent_lattice ((val Mode.Alloc.lattice_of_axis axis)))
    | Nonmodal Externality -> (module Externality)
    | Nonmodal Nullability -> (module Nullability)

  let all =
    [ Pack (Modal (Comonadic Areality));
      Pack (Modal (Monadic Uniqueness));
      Pack (Modal (Comonadic Linearity));
      Pack (Modal (Monadic Contention));
      Pack (Modal (Comonadic Portability));
      Pack (Modal (Comonadic Yielding));
      Pack (Nonmodal Externality);
      Pack (Nonmodal Nullability) ]

  let name (type a) : a t -> string = function
    | Modal axis -> Format.asprintf "%a" Mode.Alloc.print_axis axis
    | Nonmodal Externality -> "externality"
    | Nonmodal Nullability -> "nullability"

  let is_modal (type a) : a t -> bool = function
    | Modal (Comonadic Areality) -> true
    | Modal (Comonadic Linearity) -> true
    | Modal (Monadic Uniqueness) -> true
    | Modal (Comonadic Portability) -> true
    | Modal (Monadic Contention) -> true
    | Modal (Comonadic Yielding) -> true
    | Nonmodal Externality -> true
    | Nonmodal Nullability -> false
end

module type Axed = sig
  type 'axis t
end

module Axis_collection = struct
  type 'a t =
    { locality : 'a;
      linearity : 'a;
      uniqueness : 'a;
      portability : 'a;
      contention : 'a;
      yielding : 'a;
      externality : 'a;
      nullability : 'a
    }

  let create ~(f : axis:Axis.packed -> _) =
    { locality = f ~axis:(Pack (Modal (Comonadic Areality)));
      linearity = f ~axis:(Pack (Modal (Comonadic Linearity)));
      uniqueness = f ~axis:(Pack (Modal (Monadic Uniqueness)));
      portability = f ~axis:(Pack (Modal (Comonadic Portability)));
      contention = f ~axis:(Pack (Modal (Monadic Contention)));
      yielding = f ~axis:(Pack (Modal (Comonadic Yielding)));
      externality = f ~axis:(Pack (Nonmodal Externality));
      nullability = f ~axis:(Pack (Nonmodal Nullability))
    }

  let get (type a) ~(axis : a Axis.t) t =
    match axis with
    | Modal (Comonadic Areality) -> t.locality
    | Modal (Comonadic Linearity) -> t.linearity
    | Modal (Monadic Uniqueness) -> t.uniqueness
    | Modal (Comonadic Portability) -> t.portability
    | Modal (Monadic Contention) -> t.contention
    | Modal (Comonadic Yielding) -> t.yielding
    | Nonmodal Externality -> t.externality
    | Nonmodal Nullability -> t.nullability

  let set (type a) ~(axis : a Axis.t) t value =
    match axis with
    | Modal (Comonadic Areality) -> { t with locality = value }
    | Modal (Comonadic Linearity) -> { t with linearity = value }
    | Modal (Monadic Uniqueness) -> { t with uniqueness = value }
    | Modal (Comonadic Portability) -> { t with portability = value }
    | Modal (Monadic Contention) -> { t with contention = value }
    | Modal (Comonadic Yielding) -> { t with yielding = value }
    | Nonmodal Externality -> { t with externality = value }
    | Nonmodal Nullability -> { t with nullability = value }

  let mapi ~(f : axis:Axis.packed -> _ -> _)
      { locality;
        linearity;
        uniqueness;
        portability;
        contention;
        yielding;
        externality;
        nullability
      } =
    { locality = f ~axis:(Pack (Modal (Comonadic Areality))) locality;
      linearity = f ~axis:(Pack (Modal (Comonadic Linearity))) linearity;
      uniqueness = f ~axis:(Pack (Modal (Monadic Uniqueness))) uniqueness;
      portability = f ~axis:(Pack (Modal (Comonadic Portability))) portability;
      contention = f ~axis:(Pack (Modal (Monadic Contention))) contention;
      yielding = f ~axis:(Pack (Modal (Comonadic Yielding))) yielding;
      externality = f ~axis:(Pack (Nonmodal Externality)) externality;
      nullability = f ~axis:(Pack (Nonmodal Nullability)) nullability
    }

  let map ~f = mapi ~f:(fun ~axis:_ x -> f x)

  let fold ~(f : axis:Axis.packed -> _ -> _) ~combine
      { locality;
        linearity;
        uniqueness;
        portability;
        contention;
        yielding;
        externality;
        nullability
      } =
    combine (f ~axis:(Pack (Modal (Comonadic Areality))) locality)
    @@ combine (f ~axis:(Pack (Modal (Comonadic Linearity))) linearity)
    @@ combine (f ~axis:(Pack (Modal (Monadic Uniqueness))) uniqueness)
    @@ combine (f ~axis:(Pack (Modal (Comonadic Portability))) portability)
    @@ combine (f ~axis:(Pack (Modal (Monadic Contention))) contention)
    @@ combine (f ~axis:(Pack (Modal (Comonadic Yielding))) yielding)
    @@ combine (f ~axis:(Pack (Nonmodal Externality)) externality)
    @@ f ~axis:(Pack (Nonmodal Nullability)) nullability

  (* Sadly this needs to be functorized since we don't have higher-kinded types *)
  module Indexed (T : Axed) = struct
    type t =
      { locality : Mode.Locality.Const.t T.t;
        linearity : Mode.Linearity.Const.t T.t;
        uniqueness : Mode.Uniqueness.Const.t T.t;
        portability : Mode.Portability.Const.t T.t;
        contention : Mode.Contention.Const.t T.t;
        yielding : Mode.Yielding.Const.t T.t;
        externality : Externality.t T.t;
        nullability : Nullability.t T.t
      }

    let get (type a) ~(axis : a Axis.t) values : a T.t =
      match axis with
      | Modal (Comonadic Areality) -> values.locality
      | Modal (Comonadic Linearity) -> values.linearity
      | Modal (Monadic Uniqueness) -> values.uniqueness
      | Modal (Comonadic Portability) -> values.portability
      | Modal (Monadic Contention) -> values.contention
      | Modal (Comonadic Yielding) -> values.yielding
      | Nonmodal Externality -> values.externality
      | Nonmodal Nullability -> values.nullability

    let set (type a) ~(axis : a Axis.t) values (value : a T.t) =
      match axis with
      | Modal (Comonadic Areality) -> { values with locality = value }
      | Modal (Comonadic Linearity) -> { values with linearity = value }
      | Modal (Monadic Uniqueness) -> { values with uniqueness = value }
      | Modal (Comonadic Portability) -> { values with portability = value }
      | Modal (Monadic Contention) -> { values with contention = value }
      | Modal (Comonadic Yielding) -> { values with yielding = value }
      | Nonmodal Externality -> { values with externality = value }
      | Nonmodal Nullability -> { values with nullability = value }

    (* Since we don't have polymorphic parameters, use a record to pass the
       polymorphic function *)
    module Create = struct
      module Monadic (M : Misc.Stdlib.Monad.S) = struct
        type f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t M.t } [@@unboxed]

        let[@inline] f { f } =
          let open M.Syntax in
          let* locality = f ~axis:Axis.(Modal (Comonadic Areality)) in
          let* uniqueness = f ~axis:Axis.(Modal (Monadic Uniqueness)) in
          let* linearity = f ~axis:Axis.(Modal (Comonadic Linearity)) in
          let* contention = f ~axis:Axis.(Modal (Monadic Contention)) in
          let* portability = f ~axis:Axis.(Modal (Comonadic Portability)) in
          let* yielding = f ~axis:Axis.(Modal (Comonadic Yielding)) in
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

      type f = Monadic_identity.f

      let[@inline] f f = Monadic_identity.f f
    end

    module Map = struct
      module Monadic (M : Misc.Stdlib.Monad.S) = struct
        type f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> 'axis T.t M.t }
        [@@unboxed]

        module Create = Create.Monadic (M)

        let[@inline] f { f } bounds =
          Create.f { f = (fun ~axis -> f ~axis (get ~axis bounds)) }
      end
      [@@inline]

      module Monadic_identity = Monadic (Misc.Stdlib.Monad.Identity)

      type f = Monadic_identity.f

      let[@inline] f f bounds = Monadic_identity.f f bounds
    end

    module Iter = struct
      type f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> unit }

      let[@inline] f { f }
          { locality;
            linearity;
            uniqueness;
            portability;
            contention;
            yielding;
            externality;
            nullability
          } =
        f ~axis:Axis.(Modal (Comonadic Areality)) locality;
        f ~axis:Axis.(Modal (Monadic Uniqueness)) uniqueness;
        f ~axis:Axis.(Modal (Comonadic Linearity)) linearity;
        f ~axis:Axis.(Modal (Monadic Contention)) contention;
        f ~axis:Axis.(Modal (Comonadic Portability)) portability;
        f ~axis:Axis.(Modal (Comonadic Yielding)) yielding;
        f ~axis:Axis.(Nonmodal Externality) externality;
        f ~axis:Axis.(Nonmodal Nullability) nullability
    end

    module Map2 = struct
      module Monadic (M : Misc.Stdlib.Monad.S) = struct
        type f =
          { f :
              'axis.
              axis:'axis Axis.t -> 'axis T.t -> 'axis T.t -> 'axis T.t M.t
          }
        [@@unboxed]

        module Create = Create.Monadic (M)

        let[@inline] f { f } bounds1 bounds2 =
          Create.f
            { f = (fun ~axis -> f ~axis (get ~axis bounds1) (get ~axis bounds2))
            }
      end
      [@@inline]

      module Monadic_identity = Monadic (Misc.Stdlib.Monad.Identity)

      type f = Monadic_identity.f

      let[@inline] f f bounds1 bounds2 = Monadic_identity.f f bounds1 bounds2
    end

    module Fold = struct
      type 'r f = { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> 'r }
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
        combine (f ~axis:Axis.(Modal (Comonadic Areality)) locality)
        @@ combine (f ~axis:Axis.(Modal (Monadic Uniqueness)) uniqueness)
        @@ combine (f ~axis:Axis.(Modal (Comonadic Linearity)) linearity)
        @@ combine (f ~axis:Axis.(Modal (Monadic Contention)) contention)
        @@ combine (f ~axis:Axis.(Modal (Comonadic Portability)) portability)
        @@ combine (f ~axis:Axis.(Modal (Comonadic Yielding)) yielding)
        @@ combine (f ~axis:Axis.(Nonmodal Externality) externality)
        @@ f ~axis:Axis.(Nonmodal Nullability) nullability
    end

    module Fold2 = struct
      type 'r f =
        { f : 'axis. axis:'axis Axis.t -> 'axis T.t -> 'axis T.t -> 'r }
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
        combine (f ~axis:Axis.(Modal (Comonadic Areality)) loc1 loc2)
        @@ combine (f ~axis:Axis.(Modal (Monadic Uniqueness)) uni1 uni2)
        @@ combine (f ~axis:Axis.(Modal (Comonadic Linearity)) lin1 lin2)
        @@ combine (f ~axis:Axis.(Modal (Monadic Contention)) con1 con2)
        @@ combine (f ~axis:Axis.(Modal (Comonadic Portability)) por1 por2)
        @@ combine (f ~axis:Axis.(Modal (Comonadic Yielding)) yie1 yie2)
        @@ combine (f ~axis:Axis.(Nonmodal Externality) ext1 ext2)
        @@ f ~axis:Axis.(Nonmodal Nullability) nul1 nul2
    end
  end
end

module Axis_set = struct
  (* each axis is true or false to indicate membership  *)
  type t = bool Axis_collection.t

  (* TODO: this could be represented with a uint8 since there's only 7 possible members *)

  let empty = Axis_collection.create ~f:(fun ~axis:_ -> false)

  let create ~f = Axis_collection.create ~f

  let add t axis = Axis_collection.set ~axis t true

  let remove t axis = Axis_collection.set ~axis t false

  let mem t axis = Axis_collection.get ~axis t

  let union t1 t2 =
    Axis_collection.create ~f:(fun ~axis:(Pack axis) ->
        Axis_collection.get ~axis t1 || Axis_collection.get ~axis t2)

  let intersection t1 t2 =
    Axis_collection.create ~f:(fun ~axis:(Pack axis) ->
        Axis_collection.get ~axis t1 && Axis_collection.get ~axis t2)

  let is_subset t1 t2 =
    Axis_collection.fold
      ~f:(fun ~axis:(Pack axis) t1_on_axis ->
        let t2_on_axis = Axis_collection.get ~axis t2 in
        (not t1_on_axis) || t2_on_axis)
      ~combine:( && ) t1

  let complement t = Axis_collection.map ~f:not t

  let to_list t =
    Axis_collection.fold
      ~f:(fun ~axis t_on_axis ->
        match t_on_axis with true -> [axis] | false -> [])
      ~combine:( @ ) t

  let print ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
      (fun ppf (Axis.Pack axis) -> Format.fprintf ppf "%s" (Axis.name axis))
      ppf (to_list t)
end
