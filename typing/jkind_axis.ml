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

    let[@inline] less_or_equal a b : Misc.Le_result.t =
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

module Axis_collection = struct
  module Indexed_gen (T : Misc.T2) = struct
    type 'a t_poly =
      { locality : (Mode.Locality.Const.t, 'a) T.t;
        linearity : (Mode.Linearity.Const.t, 'a) T.t;
        uniqueness : (Mode.Uniqueness.Const.t, 'a) T.t;
        portability : (Mode.Portability.Const.t, 'a) T.t;
        contention : (Mode.Contention.Const.t, 'a) T.t;
        yielding : (Mode.Yielding.Const.t, 'a) T.t;
        externality : (Externality.t, 'a) T.t;
        nullability : (Nullability.t, 'a) T.t
      }

    type 'a t = 'a t_poly

    let get (type a) ~(axis : a Axis.t) (t : 'b t) : (a, 'b) T.t =
      match axis with
      | Modal (Comonadic Areality) -> t.locality
      | Modal (Comonadic Linearity) -> t.linearity
      | Modal (Monadic Uniqueness) -> t.uniqueness
      | Modal (Comonadic Portability) -> t.portability
      | Modal (Monadic Contention) -> t.contention
      | Modal (Comonadic Yielding) -> t.yielding
      | Nonmodal Externality -> t.externality
      | Nonmodal Nullability -> t.nullability

    let set (type a) ~(axis : a Axis.t) (t : 'b t) (value : (a, 'b) T.t) =
      match axis with
      | Modal (Comonadic Areality) -> { t with locality = value }
      | Modal (Comonadic Linearity) -> { t with linearity = value }
      | Modal (Monadic Uniqueness) -> { t with uniqueness = value }
      | Modal (Comonadic Portability) -> { t with portability = value }
      | Modal (Monadic Contention) -> { t with contention = value }
      | Modal (Comonadic Yielding) -> { t with yielding = value }
      | Nonmodal Externality -> { t with externality = value }
      | Nonmodal Nullability -> { t with nullability = value }

    (* Since we don't have polymorphic parameters, use a record to pass the
       polymorphic function *)
    module Create = struct
      module Monadic (M : Misc.Stdlib.Monad.S) = struct
        type 'a f = { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) T.t M.t }
        [@@unboxed]

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

      type 'a f = 'a Monadic_identity.f

      let[@inline] f f = Monadic_identity.f f
    end

    module Map = struct
      module Monadic (M : Misc.Stdlib.Monad.S) = struct
        type ('a, 'b) f =
          { f :
              'axis. axis:'axis Axis.t -> ('axis, 'a) T.t -> ('axis, 'b) T.t M.t
          }
        [@@unboxed]

        module Create = Create.Monadic (M)

        let[@inline] f { f } bounds =
          Create.f { f = (fun ~axis -> f ~axis (get ~axis bounds)) }
      end
      [@@inline]

      module Monadic_identity = Monadic (Misc.Stdlib.Monad.Identity)

      type ('a, 'b) f = ('a, 'b) Monadic_identity.f

      let[@inline] f f bounds = Monadic_identity.f f bounds
    end

    module Iter = struct
      type 'a f = { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) T.t -> unit }
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
        type ('a, 'b, 'c) f =
          { f :
              'axis.
              axis:'axis Axis.t ->
              ('axis, 'a) T.t ->
              ('axis, 'b) T.t ->
              ('axis, 'c) T.t M.t
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

      type ('a, 'b, 'c) f = ('a, 'b, 'c) Monadic_identity.f

      let[@inline] f f bounds1 bounds2 = Monadic_identity.f f bounds1 bounds2
    end

    module Fold = struct
      type ('a, 'r) f =
        { f : 'axis. axis:'axis Axis.t -> ('axis, 'a) T.t -> 'r }
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
      type ('a, 'b, 'r) f =
        { f :
            'axis. axis:'axis Axis.t -> ('axis, 'a) T.t -> ('axis, 'b) T.t -> 'r
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

  module Indexed (T : Misc.T1) = struct
    include Indexed_gen (struct
      type ('a, 'b) t = 'a T.t
    end)

    type nonrec t = unit t
  end

  module Identity = Indexed (Misc.Stdlib.Monad.Identity)

  include Indexed_gen (struct
    type ('a, 'b) t = 'b
  end)

  let create ~f = Create.f { f = (fun ~axis -> f ~axis:(Axis.Pack axis)) }

  let map ~f t = Map.f { f = (fun ~axis:_ x -> f x) } t

  let mapi ~f t = Map.f { f = (fun ~axis x -> f ~axis:(Axis.Pack axis) x) } t

  let fold ~f ~combine t =
    Fold.f { f = (fun ~axis acc -> f ~axis:(Axis.Pack axis) acc) } t ~combine
end

module Axis_set = struct
  (* This could be [bool Axis_collection.t], but instead we represent it as a bitfield for
     performance (this matters, since these are hammered on quite a bit during with-bound
     normalization) *)

  type t = int
  (* CR layouts: if we had unboxed types in the compiler, this could be represented with a
     uint8 since there are only 8 bits that we care about *)

  let[@inline] axis_index (type a) : a Axis.t -> _ = function
    | Modal (Comonadic Areality) -> 0
    | Modal (Comonadic Linearity) -> 1
    | Modal (Monadic Uniqueness) -> 2
    | Modal (Comonadic Portability) -> 3
    | Modal (Monadic Contention) -> 4
    | Modal (Comonadic Yielding) -> 5
    | Nonmodal Externality -> 6
    | Nonmodal Nullability -> 7

  let[@inline] axis_mask ax = 1 lsl axis_index ax

  let[@inline] set ~axis ~to_ t =
    match to_ with
    | true -> t lor axis_mask axis
    | false -> t land lnot (axis_mask axis)

  let empty = 0

  let[@inline] add t axis = set ~axis ~to_:true t

  let[@inline] create ~f =
    (* PERF: this is manually unrolled because flambda2 doesn't unroll for us, and this
       function is quite hot *)
    let[@inline] set_axis axis t =
      if f ~axis:(Axis.Pack axis) then t lor axis_mask axis else t
    in
    0
    |> set_axis (Modal (Comonadic Areality))
    |> set_axis (Modal (Comonadic Linearity))
    |> set_axis (Modal (Monadic Uniqueness))
    |> set_axis (Modal (Comonadic Portability))
    |> set_axis (Modal (Monadic Contention))
    |> set_axis (Modal (Comonadic Yielding))
    |> set_axis (Nonmodal Externality)
    |> set_axis (Nonmodal Nullability)

  let all = create ~f:(fun ~axis:_ -> true)

  let all_monadic_axes =
    create ~f:(fun ~axis ->
        match axis with
        | Pack (Modal (Monadic _)) -> true
        | Pack (Modal (Comonadic _)) | Pack (Nonmodal _) -> false)

  let all_comonadic_axes =
    create ~f:(fun ~axis ->
        match axis with
        | Pack (Modal (Comonadic _)) -> true
        | Pack (Modal (Monadic _)) | Pack (Nonmodal _) -> false)

  let[@inline] singleton axis = add empty axis

  let[@inline] remove t axis = set ~axis ~to_:false t

  let[@inline] mem t axis = not (Int.equal (t land axis_mask axis) 0)

  let[@inline] union t1 t2 = t1 lor t2

  let[@inline] intersection t1 t2 = t1 land t2

  let[@inline] diff t1 t2 = t1 land lnot t2

  let[@inline] is_subset t1 t2 = Int.equal (t1 land t2) t1

  let[@inline] is_empty t = Int.equal t 0

  let[@inline] complement t = diff all t

  let[@inline] to_seq t =
    Axis.all |> List.to_seq |> Seq.filter (fun (Axis.Pack axis) -> mem t axis)

  let[@inline] to_list t = List.of_seq (to_seq t)

  let print ppf t =
    Format.pp_print_seq
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
      (fun ppf (Axis.Pack axis) -> Format.fprintf ppf "%s" (Axis.name axis))
      ppf (to_seq t)
end
