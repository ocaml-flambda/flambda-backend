open Solver
open Solver_intf
open Mode_intf

type nonrec allowed = allowed

type nonrec disallowed = disallowed

module Product = struct
  type ('a0, 'a1) t = 'a0 * 'a1

  (* type aware indexing into a tuple *)
  type ('a0, 'a1, 'a) axis =
    | Axis0 : ('a0, 'a1, 'a0) axis
    | Axis1 : ('a0, 'a1, 'a1) axis

  let print_axis : type a0 a1 a. Format.formatter -> (a0, a1, a) axis -> unit =
   fun ppf -> function
    | Axis0 -> Format.fprintf ppf "0"
    | Axis1 -> Format.fprintf ppf "1"

  let proj (type a0 a1 a) : (a0, a1, a) axis -> a0 * a1 -> a = function
    | Axis0 -> fun (x, _) -> x
    | Axis1 -> fun (_, x) -> x

  let eq_axis (type a0 a1 a b) :
      (a0, a1, a) axis -> (a0, a1, b) axis -> (a, b) Misc.eq option =
   fun a b ->
    match a, b with
    | Axis0, Axis0 -> Some Refl
    | Axis1, Axis1 -> Some Refl
    | _ -> None

  type ('a0, 'a1, 'a, 'b0, 'b1, 'b) saxis =
    | SAxis0 : ('a0, 'a1, 'a0, 'b0, 'a1, 'b0) saxis
    | SAxis1 : ('a0, 'a1, 'a1, 'a0, 'b1, 'b1) saxis

  let flip (type a0 a1 a b0 b1 b) :
      (a0, a1, a, b0, b1, b) saxis -> (b0, b1, b, a0, a1, a) saxis = function
    | SAxis0 -> SAxis0
    | SAxis1 -> SAxis1

  let set (type a0 a1 a b0 b1 b) :
      (a0, a1, a, b0, b1, b) saxis -> (a -> b) -> (a0, a1) t -> (b0, b1) t =
    function
    | SAxis0 -> fun f (a0, a1) -> f a0, a1
    | SAxis1 -> fun f (a0, a1) -> a0, f a1

  let diag (type a0 a1 a) : (a0, a1, a) axis -> (a0, a1, a, a0, a1, a) saxis =
    function
    | Axis0 -> SAxis0
    | Axis1 -> SAxis1

  let src (type a0 a1 a b0 b1 b) :
      (a0, a1, a, b0, b1, b) saxis -> (a0, a1, a) axis = function
    | SAxis0 -> Axis0
    | SAxis1 -> Axis1

  let dst (type a0 a1 a b0 b1 b) :
      (a0, a1, a, b0, b1, b) saxis -> (b0, b1, b) axis = function
    | SAxis0 -> Axis0
    | SAxis1 -> Axis1

  let update (type a0 a1 a) : (a0, a1, a) axis -> a -> a0 * a1 -> a0 * a1 =
   fun ax a t -> set (diag ax) (fun _ -> a) t

  (** Proj after set.
      - If they operate on the same axis, then provide the proof;
      - If they operate on different axis, then provide the axis that allows
      projection without set. *)
  let proj_set (type a0 a1 a b0 b1 b b') :
      (b0, b1, b') axis ->
      (a0, a1, a, b0, b1, b) saxis ->
      ((b, b') Misc.eq, (a0, a1, b') axis) Either.t =
   fun ax sax ->
    match sax, ax with
    | SAxis0, Axis0 -> Either.left Misc.Refl
    | SAxis1, Axis1 -> Either.left Misc.Refl
    | SAxis0, Axis1 -> Either.right Axis1
    | SAxis1, Axis0 -> Either.right Axis0

  (** Set after Set. If they operate on the same axis, then provide the saxis
      that allows a single set operation, and the proof that they operate on the
       same axis *)
  let set_set (type a0 a1 a b0 b1 b b' c0 c1 c) :
      (b0, b1, b', c0, c1, c) saxis ->
      (a0, a1, a, b0, b1, b) saxis ->
      ((a0, a1, a, c0, c1, c) saxis * (b, b') Misc.eq) option =
   fun sax0 sax1 ->
    match sax0, sax1 with
    | SAxis0, SAxis0 -> Some (SAxis0, Misc.Refl)
    | SAxis1, SAxis1 -> Some (SAxis1, Misc.Refl)
    | _ -> None

  module Lattice (L0 : Lattice) (L1 : Lattice) :
    Lattice with type t = L0.t * L1.t = struct
    type nonrec t = L0.t * L1.t

    let min = L0.min, L1.min

    let max = L0.max, L1.max

    let legacy = L0.legacy, L1.legacy

    let le (a0, a1) (b0, b1) = L0.le a0 b0 && L1.le a1 b1

    let join (a0, a1) (b0, b1) = L0.join a0 b0, L1.join a1 b1

    let meet (a0, a1) (b0, b1) = L0.meet a0 b0, L1.meet a1 b1

    let print ppf (a0, a1) = Format.fprintf ppf "%a,%a" L0.print a0 L1.print a1
  end

  module Lattice_indexed (L0 : Lattice_indexed_bound) (L1 : Lattice) = struct
    module Index = L0.Index

    type t = L0.t * L1.t

    let min = L0.min, L1.min

    let max i = L0.max i, L1.max

    let le (a0, a1) (b0, b1) = L0.le a0 b0 && L1.le a1 b1

    let join (a0, a1) (b0, b1) = L0.join a0 b0, L1.join a1 b1

    let meet (a0, a1) (b0, b1) = L0.meet a0 b0, L1.meet a1 b1

    let print ppf (a0, a1) = Format.fprintf ppf "%a,%a" L0.print a0 L1.print a1
  end
end

module Lattices = struct
  module Opposite (L : Lattice) : Lattice with type t = L.t = struct
    type t = L.t

    let min = L.max

    let max = L.min

    let legacy = L.legacy

    let le a b = L.le b a

    let join = L.meet

    let meet = L.join

    let print = L.print
  end

  module Locality = struct
    type t =
      | Global
      | Local

    let min = Global

    let max = Local

    let legacy = Global

    let le a b =
      match a, b with Global, _ | _, Local -> true | Local, Global -> false

    let join a b =
      match a, b with Local, _ | _, Local -> Local | Global, Global -> Global

    let meet a b =
      match a, b with Global, _ | _, Global -> Global | Local, Local -> Local

    let print ppf = function
      | Global -> Format.fprintf ppf "Global"
      | Local -> Format.fprintf ppf "Local"
  end

  module Regionality : sig
    type t = private T of Int.t

    (** Abstracting to guarantee that index is >= 1 *)
    module Index : sig
      type t = private T of Int.t

      val min : t

      val max : t

      val succ : t -> t

      val le : t -> t -> bool

      val lt : t -> t -> bool

      val print : Format.formatter -> t -> unit
    end

    (** The mode local to the region *)
    val local : Index.t -> t

    (** The mode to escape the region *)
    val escape : Index.t -> t

    val global : t

    include Lattice_indexed_bound with type t := t and module Index := Index
  end = struct
    type t = T of Int.t

    module Index = struct
      type t = T of Int.t

      let min = T 1

      (* let valid (T i) = i > 0 *)
      let max = T Int.max_int

      let le (T i) (T j) = i <= j

      let lt (T i) (T j) = i < j

      let succ (T t) = T (t + 1)

      let print ppf (T a) = Format.fprintf ppf "%i" a
    end

    let local (Index.T i) = T i

    let escape (Index.T i) =
      (* For the special case when i = 1, we return 0; Concretely, this means
         that to escape the top-level region, you need to be global. *)
      T (i - 1)

    let min = T 0

    let max (Index.T i) = T i

    let global = min

    let join (T a) (T b) = T (Int.max a b)

    let meet (T a) (T b) = T (Int.min a b)

    let le (T a) (T b) = a <= b

    let print ppf (T a) = Format.fprintf ppf "%i" a
  end

  module Uniqueness = struct
    type t =
      | Unique
      | Shared

    let min = Unique

    let max = Shared

    let legacy = Shared

    let le a b =
      match a, b with Unique, _ | _, Shared -> true | Shared, Unique -> false

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
  end

  module Uniqueness_op = Opposite (Uniqueness)

  module Linearity = struct
    type t =
      | Many
      | Once

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
  end

  module Comonadic_with_locality = Product.Lattice (Locality) (Linearity)
  module Comonadic_with_regionality =
    Product.Lattice_indexed (Regionality) (Linearity)

  type 'a obj =
    | Locality : Locality.t obj
    | Regionality : Regionality.Index.t -> Regionality.t obj
    (* use the flipped version of uniqueness, so that [unique_to_linear] is monotone *)
    | Uniqueness_op : Uniqueness_op.t obj
    | Linearity : Linearity.t obj
    | Comonadic_with_regionality :
        Comonadic_with_regionality.Index.t
        -> Comonadic_with_regionality.t obj
    | Comonadic_with_locality : Comonadic_with_locality.t obj

  let print_obj : type a. _ -> a obj -> unit =
   fun ppf -> function
    | Locality -> Format.fprintf ppf "Locality"
    | Regionality i ->
      Format.fprintf ppf "Regionality_%a" Regionality.Index.print i
    | Uniqueness_op -> Format.fprintf ppf "Uniqueness_op"
    | Linearity -> Format.fprintf ppf "Linearity"
    | Comonadic_with_locality -> Format.fprintf ppf "Comonadic_with_locality"
    | Comonadic_with_regionality i ->
      Format.fprintf ppf "Comonadic_with_regionality_%a" Regionality.Index.print
        i

  let proj_obj :
      type a0 a1 a. (a0, a1, a) Product.axis -> (a0, a1) Product.t obj -> a obj
      = function
    | Axis0 -> (
      function
      | Comonadic_with_locality -> Locality
      | Comonadic_with_regionality i -> Regionality i)
    | Axis1 -> (
      function
      | Comonadic_with_locality -> Linearity
      | Comonadic_with_regionality _ -> Linearity)

  let set_obj :
      type a0 a1 a b0 b1 b.
      (a0, a1, a, b0, b1, b) Product.saxis ->
      b obj ->
      (a0, a1) Product.t obj ->
      (b0, b1) Product.t obj =
   fun sax dst -> function
    | Comonadic_with_locality -> (
      match sax, dst with
      | SAxis0, Locality -> Comonadic_with_locality
      | SAxis0, Regionality i -> Comonadic_with_regionality i
      | SAxis1, Linearity -> Comonadic_with_locality
      | _, _ -> assert false)
    | Comonadic_with_regionality i -> (
      match sax, dst with
      | SAxis0, Locality -> Comonadic_with_locality
      | SAxis0, Regionality j -> Comonadic_with_regionality j
      | SAxis1, Linearity -> Comonadic_with_regionality i
      | _, _ -> assert false)

  let min : type a. a obj -> a = function
    | Locality -> Locality.min
    | Regionality _ -> Regionality.min
    | Uniqueness_op -> Uniqueness_op.min
    | Linearity -> Linearity.min
    | Comonadic_with_locality -> Comonadic_with_locality.min
    | Comonadic_with_regionality _ -> Comonadic_with_regionality.min

  let max : type a. a obj -> a = function
    | Locality -> Locality.max
    | Regionality obj -> Regionality.max obj
    | Uniqueness_op -> Uniqueness_op.max
    | Linearity -> Linearity.max
    | Comonadic_with_locality -> Comonadic_with_locality.max
    | Comonadic_with_regionality obj -> Comonadic_with_regionality.max obj

  let le : type a. a obj -> a -> a -> bool = function
    | Locality -> Locality.le
    | Regionality _ -> Regionality.le
    | Uniqueness_op -> Uniqueness_op.le
    | Linearity -> Linearity.le
    | Comonadic_with_locality -> Comonadic_with_locality.le
    | Comonadic_with_regionality _ -> Comonadic_with_regionality.le

  let join : type a. a obj -> a -> a -> a = function
    | Locality -> Locality.join
    | Regionality _ -> Regionality.join
    | Uniqueness_op -> Uniqueness_op.join
    | Linearity -> Linearity.join
    | Comonadic_with_locality -> Comonadic_with_locality.join
    | Comonadic_with_regionality _ -> Comonadic_with_regionality.join

  let meet : type a. a obj -> a -> a -> a = function
    | Locality -> Locality.meet
    | Regionality _ -> Regionality.meet
    | Uniqueness_op -> Uniqueness_op.meet
    | Linearity -> Linearity.meet
    | Comonadic_with_locality -> Comonadic_with_locality.meet
    | Comonadic_with_regionality _ -> Comonadic_with_regionality.meet

  let print : type a. a obj -> _ -> a -> unit = function
    | Locality -> Locality.print
    | Regionality _ -> Regionality.print
    | Uniqueness_op -> Uniqueness_op.print
    | Linearity -> Linearity.print
    | Comonadic_with_locality -> Comonadic_with_locality.print
    | Comonadic_with_regionality _ -> Comonadic_with_regionality.print

  let eq_obj : type a b. a obj -> b obj -> (a, b) Misc.eq option =
   fun a b ->
    match a, b with
    | Locality, Locality -> Some Misc.Refl
    | Regionality n, Regionality m -> if n = m then Some Misc.Refl else None
    | Uniqueness_op, Uniqueness_op -> Some Misc.Refl
    | Linearity, Linearity -> Some Misc.Refl
    | Comonadic_with_locality, Comonadic_with_locality -> Some Misc.Refl
    | Comonadic_with_regionality n, Comonadic_with_regionality m ->
      if n = m then Some Misc.Refl else None
    | _ -> None
end

module Lattices_mono = struct
  include Lattices

  (* In the following, morphisms contain enough information to:
     - determine its runtime behaviour on constants
     - determine its adjoints (with the parameters).

       ACCIDENTALLY that is also enough to infer the source and target of morphisms.
       This should not be taken advantages of - in particular, note that this
       feature is not exposed to the solver. The user of modes should know

       Due to ['a Solver.mode] being object-blind (mode doesn't know the object
       it's in), user might use modes of wrong object (carrier type matching) - this
       is wrong and _in_general_ not preventable, compile-time or runtime.
       However, the rich information in the following morphisms can help to prevent
       many of those mistakes. See those [assert false] below.
  *)
  type ('a, 'b, 'd) morph =
    | Id : ('a, 'a, 'd) morph  (** identity morphism *)
    | Const_min : 'a obj -> ('a, 'b, 'd * disallowed) morph
        (** The constant morphism that always maps to the minimum *)
    | Const_max : 'a obj -> ('a, 'b, disallowed * 'd) morph
        (** The constant morphism that always maps to the maximum *)
    | Proj :
        ('a0, 'a1) Product.t obj * ('a0, 'a1, 'a) Product.axis
        -> (('a0, 'a1) Product.t, 'a, 'l * 'r) morph
        (** projection from product to an axis *)
    | Max_with :
        ('a0, 'a1, 'a) Product.axis
        -> ('a, ('a0, 'a1) Product.t, disallowed * 'r) morph
        (** Maps to maximum product except the given axis *)
    | Min_with :
        ('a0, 'a1, 'a) Product.axis
        -> ('a, ('a0, 'a1) Product.t, 'l * disallowed) morph
        (** Maps to minimum product except the given axis *)
    | Set :
        ('a0, 'a1, 'a, 'b0, 'b1, 'b) Product.saxis * ('a, 'b, 'd) morph
        -> (('a0, 'a1) Product.t, ('b0, 'b1) Product.t, 'd) morph
        (** Maps the given axis by the given morphism; identity on the other
            axes. *)
    | Unique_to_linear : (Uniqueness.t, Linearity.t, 'l * 'r) morph
        (** Returns the linearity dual to the given uniqueness *)
    | Linear_to_unique : (Linearity.t, Uniqueness.t, 'l * 'r) morph
        (** Returns the uniqueness dual to the given linearity *)
    (* Chain of adjunctions that jumps between locality and regionality.
       Complete and cannot extend in either direction. index i >= 1 guaranteed
       by abstraction *)
    | Local_to_regional : (Locality.t, Regionality.t, 'l * disallowed) morph
        (** Maps local to 1, global to 0 *)
    | Regional_to_local :
        Regionality.Index.t
        -> (Regionality.t, Locality.t, 'l * 'r) morph
        (** maps 0 to G, otherwise to L. *)
    | Locality_as_regionality : (Locality.t, Regionality.t, 'l * 'r) morph
        (** maps L to i, G to 0 *)
    | Regional_to_global :
        Regionality.Index.t
        -> (Regionality.t, Locality.t, 'l * 'r) morph
        (** maps =i to L, <i to G *)
    | Global_to_regional : (Locality.t, Regionality.t, disallowed * 'r) morph
        (** maps L to i, G to i-1 *)
    | Compose : ('b, 'c, 'd) morph * ('a, 'b, 'd) morph -> ('a, 'c, 'd) morph
        (** Composition of morphisms *)
    (* Chain of adjunctions that jumps between different regionalities, in the
       following we will write [i] for the smaller index, and [j] for the larger.
       The destination index is always omitted as it will be given at runtime by
       the solver. *)
    | Inj_l :
        Regionality.Index.t
        -> (Regionality.t, Regionality.t, 'l * disallowed) morph
        (** Contains [j].
           maps n <= i to n; raise for j >= n > i; this is a partial morphism,
           but the undefined domain is prevented by the solver and should never
           hit *)
    | Inj : Regionality.Index.t -> (Regionality.t, Regionality.t, 'l * 'r) morph
        (** Contains [i].
           maps n <= i to n *)
    | Cap : Regionality.Index.t -> (Regionality.t, Regionality.t, 'l * 'r) morph
        (** Contains [j].
            maps n > i to i, n <= i to n *)
    | Cap_r :
        Regionality.Index.t
        -> (Regionality.t, Regionality.t, disallowed * 'r) morph
        (** Contains [i]
            maps n < i to i, maps n = i to j *)

  let rec src : type a b d. b obj -> (a, b, d) morph -> a obj =
   fun dst -> function
    | Id -> dst
    | Proj (src, _) -> src
    | Max_with ax -> proj_obj ax dst
    | Min_with ax -> proj_obj ax dst
    | Const_min src | Const_max src -> src
    | Compose (f, g) ->
      let mid = src dst f in
      src mid g
    | Unique_to_linear -> Uniqueness_op
    | Linear_to_unique -> Linearity
    | Local_to_regional -> Locality
    | Locality_as_regionality -> Locality
    | Global_to_regional -> Locality
    | Regional_to_local i -> Regionality i
    | Regional_to_global i -> Regionality i
    | Inj_l j -> Regionality j
    | Inj i -> Regionality i
    | Cap j -> Regionality j
    | Cap_r i -> Regionality i
    | Set (sax, f) ->
      let dst0 = proj_obj (Product.dst sax) dst in
      let src0 = src dst0 f in
      set_obj (Product.flip sax) src0 dst

  let rec print_morph :
      type a b d. b obj -> Format.formatter -> (a, b, d) morph -> unit =
   fun dst ppf -> function
    | Id -> Format.fprintf ppf "id"
    | Const_min _ -> Format.fprintf ppf "const_min"
    | Const_max _ -> Format.fprintf ppf "const_max"
    | Proj (src, ax) ->
      Format.fprintf ppf "proj_%a_%a" print_obj src Product.print_axis ax
    | Max_with ax -> Format.fprintf ppf "max_with_%a" Product.print_axis ax
    | Min_with ax -> Format.fprintf ppf "min_with_%a" Product.print_axis ax
    | Set (sax, morph) ->
      let ax = Product.src sax in
      Format.fprintf ppf "set_%a(%a)" Product.print_axis ax
        (print_morph (proj_obj (Product.dst sax) dst))
        morph
    | Unique_to_linear -> Format.fprintf ppf "unique_to_linear"
    | Linear_to_unique -> Format.fprintf ppf "linear_to_unique"
    | Local_to_regional ->
      let (Regionality i) = dst in
      Format.fprintf ppf "local_to_regional_%a" Regionality.Index.print i
    | Regional_to_local i ->
      Format.fprintf ppf "regional_to_local_%a" Regionality.Index.print i
    | Locality_as_regionality ->
      let (Regionality i) = dst in
      Format.fprintf ppf "locality_as_regionality_%a" Regionality.Index.print i
    | Regional_to_global i ->
      Format.fprintf ppf "regional_to_global_%a" Regionality.Index.print i
    | Global_to_regional ->
      let (Regionality i) = dst in
      Format.fprintf ppf "global_to_regional_%a" Regionality.Index.print i
    | Compose (f0, f1) ->
      let mid = src dst f0 in
      Format.fprintf ppf "%a ∘ %a" (print_morph dst) f0 (print_morph mid) f1
    | Inj_l j ->
      let (Regionality i) = dst in
      Format.fprintf ppf "injl_%a->%a" Regionality.Index.print j
        Regionality.Index.print i
    | Inj i ->
      let (Regionality j) = dst in
      Format.fprintf ppf "inj_%a->%a" Regionality.Index.print i
        Regionality.Index.print j
    | Cap j ->
      let (Regionality i) = dst in
      Format.fprintf ppf "cap_%a->%a" Regionality.Index.print j
        Regionality.Index.print i
    | Cap_r i ->
      let (Regionality j) = dst in
      Format.fprintf ppf "capr_%a->%a" Regionality.Index.print i
        Regionality.Index.print j

  let id = Id

  let linear_to_unique = function
    | Linearity.Many -> Uniqueness.Shared
    | Linearity.Once -> Uniqueness.Unique

  let unique_to_linear = function
    | Uniqueness.Unique -> Linearity.Once
    | Uniqueness.Shared -> Linearity.Many

  let local_to_regional _ = function
    | Locality.Global -> Regionality.escape Regionality.Index.min
    | Locality.Local -> Regionality.max Regionality.Index.min

  let regional_to_local i n =
    assert (Regionality.le n (Regionality.local i));
    if Regionality.le n Regionality.min then Locality.Global else Locality.Local

  let locality_as_regionality i = function
    | Locality.Local -> Regionality.max i
    | Locality.Global -> Regionality.min

  let regional_to_global i n =
    assert (Regionality.le n (Regionality.local i));
    if Regionality.le (Regionality.max i) n
    then Locality.Local
    else Locality.Global

  let global_to_regional i = function
    | Locality.Local -> Regionality.max i
    | Locality.Global -> Regionality.escape i

  let inj_l i j n =
    if not (Regionality.Index.le i j)
    then (
      Format.eprintf "%a %a\n" Regionality.Index.print i Regionality.Index.print
        j;
      assert false);
    if Regionality.le n (Regionality.local i) then n else assert false

  let inj i j n =
    if not (Regionality.Index.le i j)
    then (
      Format.eprintf "%a %a\n" Regionality.Index.print i Regionality.Index.print
        j;
      assert false);
    if Regionality.le n (Regionality.local i) then n else assert false

  let cap i j n =
    if not (Regionality.Index.le i j)
    then (
      Format.eprintf "%a %a\n" Regionality.Index.print i Regionality.Index.print
        j;
      assert false);
    if Regionality.le n (Regionality.local i)
    then n
    else if Regionality.le n (Regionality.local j)
    then Regionality.local i
    else assert false

  let cap_r i j n =
    if not (Regionality.Index.le i j)
    then (
      Format.eprintf "%a %a\n" Regionality.Index.print i Regionality.Index.print
        j;
      assert false);
    if Regionality.le n (Regionality.local i)
    then
      if Regionality.le (Regionality.local i) n then Regionality.local j else n
    else assert false

  let rec apply' : type a b d. b obj -> (a, b, d) morph -> (a -> b) * int =
   fun dst -> function
    | Compose (f, g) ->
      let mid = src dst f in
      let g', c0 = apply' mid g in
      let f', c1 = apply' dst f in
      (fun a -> a |> g' |> f'), c0 + c1
    | Id -> Fun.id, 1
    | Proj (src, ax) -> (
      match eq_obj dst (proj_obj ax src) with
      | Some Refl -> Product.proj ax, 1
      | None -> assert false)
    | Max_with ax -> (fun a -> Product.update ax a (max dst)), 1
    | Min_with ax -> (fun a -> Product.update ax a (min dst)), 1
    | Const_min _ -> (fun _ -> min dst), 1
    | Const_max _ -> (fun _ -> max dst), 1
    | Unique_to_linear -> unique_to_linear, 1
    | Linear_to_unique -> linear_to_unique, 1
    | Local_to_regional ->
      let (Regionality i) = dst in
      local_to_regional i, 1
    | Regional_to_local i -> regional_to_local i, 1
    | Locality_as_regionality ->
      let (Regionality i) = dst in
      locality_as_regionality i, 1
    | Regional_to_global i -> regional_to_global i, 1
    | Global_to_regional ->
      let (Regionality i) = dst in
      global_to_regional i, 1
    | Inj_l j ->
      let (Regionality i) = dst in
      inj_l i j, 1
    | Inj i ->
      let (Regionality j) = dst in
      inj i j, 1
    | Cap j ->
      let (Regionality i) = dst in
      cap i j, 1
    | Cap_r i ->
      let (Regionality j) = dst in
      cap_r i j, 1
    | Set (sax, f) ->
      let dst = proj_obj (Product.dst sax) dst in
      let f', c = apply' dst f in
      Product.set sax f', c

  and apply : type a b d. b obj -> (a, b, d) morph -> a -> b =
   fun dst f ->
    let f', c = apply' dst f in
    if c > 6
    then (
      Format.eprintf
        "Morphism chain too long; contact Jane Street compiler devs with this:\n\
         %a\n"
        (print_morph dst) f;
      assert false)
    else f'

  (** Compose m0 after m1. Returns [Some f] if the composition can be
    represented by [f] instead of [Compose m0 m1]. [None] otherwise. *)
  let rec maybe_compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph option =
   (* TODO: add cases for regionality related stuff;
      In particular, as we will be looking at indices closely, we can check that
      the regionality indices match at the point of composition; this is more
      efficient than plainly checking [eq_obj (src f) (dst g)]. *)
   fun dst m0 m1 ->
    match m0, m1 with
    | Id, m -> Some m
    | m, Id -> Some m
    | Inj_l mid, Inj_l src ->
      let (Regionality dst) = dst in
      assert (Regionality.Index.le dst mid);
      assert (Regionality.Index.le mid src);
      Some (Inj_l src)
    | Inj mid, Inj src ->
      let (Regionality dst) = dst in
      assert (Regionality.Index.le src mid);
      assert (Regionality.Index.le mid dst);
      Some (Inj src)
    | Cap mid, Cap src ->
      let (Regionality dst) = dst in
      assert (Regionality.Index.le dst mid);
      assert (Regionality.Index.le mid src);
      Some (Cap src)
    | Cap_r mid, Cap_r src ->
      let (Regionality dst) = dst in
      assert (Regionality.Index.le src mid);
      assert (Regionality.Index.le mid dst);
      Some (Cap_r src)
    | Inj_l mid, Cap src ->
      let (Regionality dst) = dst in
      assert (Regionality.Index.le src mid);
      assert (Regionality.Index.le mid dst);
      Some (Inj_l src)
    | Cap_r mid, Inj src ->
      let (Regionality dst) = dst in
      assert (Regionality.Index.le src mid);
      assert (Regionality.Index.le mid dst);
      if src = mid then Some (Cap_r src) else Some (Inj src)
    | Regional_to_global mid, Cap_r src ->
      assert (Regionality.Index.le src mid);
      Some (Regional_to_global src)
    | Regional_to_local mid, Inj src ->
      assert (Regionality.Index.le src mid);
      Some (Regional_to_local src)
    | Const_min mid, f -> Some (Const_min (src mid f))
    | Const_max mid, f -> Some (Const_max (src mid f))
    | Proj (mid, ax0), Max_with ax1 -> (
      match Product.eq_axis ax0 ax1 with
      | None -> Some (Const_max (proj_obj ax1 mid))
      | Some Refl -> Some Id)
    | Proj (mid, ax0), Min_with ax1 -> (
      match Product.eq_axis ax0 ax1 with
      | None -> Some (Const_min (proj_obj ax1 mid))
      | Some Refl -> Some Id)
    | Proj (_, _), Const_min src -> Some (Const_min src)
    | Proj (_, _), Const_max src -> Some (Const_max src)
    | Max_with _, Const_max src -> Some (Const_max src)
    | Min_with _, Const_min src -> Some (Const_min src)
    | Unique_to_linear, Const_min src -> Some (Const_min src)
    | Linear_to_unique, Const_min src -> Some (Const_min src)
    | Unique_to_linear, Const_max src -> Some (Const_max src)
    | Linear_to_unique, Const_max src -> Some (Const_max src)
    | Unique_to_linear, Linear_to_unique -> Some Id
    | Linear_to_unique, Unique_to_linear -> Some Id
    | Regional_to_global _, Locality_as_regionality -> Some Id
    | Regional_to_local _, Locality_as_regionality -> Some Id
    | Set (sax0, f0), Set (sax1, f1) -> (
      match Product.set_set sax0 sax1 with
      | Some (sax, Refl) ->
        Some (Set (sax, compose (proj_obj (Product.dst sax0) dst) f0 f1))
      | None -> None (* the following are important: look inside compose *))
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
    | Proj (mid, ax), Set (sax, f) -> (
      let src' = src mid m1 in
      match Product.proj_set ax sax with
      | Either.Left Refl -> Some (compose dst f (Proj (src', Product.src sax)))
      | Either.Right ax' -> Some (Proj (src', ax')))
    | Min_with _, _ -> None
    | Max_with _, _ -> None
    | _, Proj _ -> None
    | Set _, _ -> None
    | _, _ -> None

  and compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph =
   fun dst f g ->
    match maybe_compose dst f g with
    (* if collapsed, [maybe_compose] knows what it's doing and handles
       implicit injection well *)
    | Some m -> m
    (* If not collapsed, this is when we need to record that  *)
    | None -> Compose (f, g)

  let rec left_adjoint :
      type a b l.
      b obj -> (a, b, l * allowed) morph -> (b, a, allowed * disallowed) morph =
   fun dst -> function
    | Id -> Id
    | Proj (src, ax) -> (
      match eq_obj (proj_obj ax src) dst with
      | Some Refl -> Min_with ax
      | None -> assert false)
    | Max_with ax -> Proj (dst, ax)
    | Compose (f, g) ->
      let mid = src dst f in
      let f' = left_adjoint dst f in
      let g' = left_adjoint mid g in
      Compose (g', f')
    | Const_max _ -> Const_min dst
    | Unique_to_linear -> Linear_to_unique
    | Linear_to_unique -> Unique_to_linear
    | Global_to_regional ->
      let (Regionality i) = dst in
      Regional_to_global i
    | Regional_to_global _ -> Locality_as_regionality
    | Locality_as_regionality ->
      let (Regionality i) = dst in
      Regional_to_local i
    | Regional_to_local _ -> Local_to_regional
    | Inj i ->
      let (Regionality j) = dst in
      assert (Regionality.Index.le i j);
      Inj_l j
    | Cap j ->
      let (Regionality i) = dst in
      assert (Regionality.Index.le i j);
      Inj i
    | Cap_r i ->
      let (Regionality j) = dst in
      assert (Regionality.Index.le i j);
      Cap j
    | Set (sax, f) ->
      let f' = left_adjoint (proj_obj (Product.dst sax) dst) f in
      Set (Product.flip sax, f')

  let rec right_adjoint :
      type a b r.
      b obj -> (a, b, allowed * r) morph -> (b, a, disallowed * allowed) morph =
   fun dst -> function
    | Id -> Id
    | Proj (src, ax) -> (
      match eq_obj (proj_obj ax src) dst with
      | Some Refl -> Max_with ax
      | None -> assert false)
    | Min_with ax -> Proj (dst, ax)
    | Compose (f, g) ->
      let mid = src dst f in
      let f' = right_adjoint dst f in
      let g' = right_adjoint mid g in
      Compose (g', f')
    | Const_min _ -> Const_max dst
    | Unique_to_linear -> Linear_to_unique
    | Linear_to_unique -> Unique_to_linear
    | Local_to_regional ->
      let (Regionality i) = dst in
      Regional_to_local i
    | Regional_to_local _ -> Locality_as_regionality
    | Locality_as_regionality ->
      let (Regionality i) = dst in
      Regional_to_global i
    | Regional_to_global _ -> Global_to_regional
    | Inj_l j ->
      let (Regionality i) = dst in
      assert (Regionality.Index.le i j);
      Inj i
    | Inj i ->
      let (Regionality j) = dst in
      assert (Regionality.Index.le i j);
      Cap j
    | Cap j ->
      let (Regionality i) = dst in
      assert (Regionality.Index.le i j);
      Cap_r i
    | Set (sax, f) ->
      let f' = right_adjoint (proj_obj (Product.dst sax) dst) f in
      Set (Product.flip sax, f')

  let disallow_right :
      type a b l r. (a, b, l * r) morph -> (a, b, l * disallowed) morph =
    Obj.magic

  let disallow_left :
      type a b l r. (a, b, l * r) morph -> (a, b, disallowed * r) morph =
    Obj.magic

  let allow_left :
      type a b l r. (a, b, allowed * r) morph -> (a, b, l * r) morph =
    Obj.magic

  let allow_right :
      type a b l r. (a, b, l * allowed) morph -> (a, b, l * r) morph =
    Obj.magic
end

module C = Lattices_mono
module S = Solver_polarized (C)

type changes = S.changes

let undo_changes = S.undo_changes

let append_changes = S.append_changes

(** Representing a single object *)
module type Obj = sig
  type const

  type polarity

  val obj_s : (const * polarity) S.obj
end

let equate_from_submode submode m0 m1 =
  match submode m0 m1 with
  | Error e -> Error (Left_le_right, e)
  | Ok () -> (
    match submode m1 m0 with
    | Error e -> Error (Right_le_left, e)
    | Ok () -> Ok ())

module Common (Obj : Obj) = struct
  open Obj

  type 'd t = (const * polarity, 'd) S.mode

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type nonrec error = const error

  type equate_error = equate_step * error

  let disallow_right m = S.disallow_right m

  let disallow_left m = S.disallow_left m

  let allow_left m = S.allow_left m

  let allow_right m = S.allow_right m

  let newvar () = S.newvar obj_s

  let min = S.min obj_s

  let max = S.max obj_s

  let newvar_above m = S.newvar_above obj_s m

  let newvar_below m = S.newvar_below obj_s m

  let submode m0 m1 : (unit, error) result = S.submode obj_s m0 m1

  let join l = S.join obj_s l

  let meet l = S.meet obj_s l

  let submode_exn m0 m1 = assert (submode m0 m1 |> Result.is_ok)

  let equate = equate_from_submode submode

  let equate_exn m0 m1 = assert (equate m0 m1 |> Result.is_ok)

  let print ?(raw = false) ?verbose () ppf m =
    if raw
    then S.print_raw ?verbose obj_s ppf m
    else S.print ?verbose obj_s ppf m

  let zap_to_ceil m = S.zap_to_ceil obj_s m

  let zap_to_floor m = S.zap_to_floor obj_s m

  let of_const : type l r. const -> (l * r) t = fun a -> S.of_const obj_s a

  let check_const m = S.check_const obj_s m
end

(* Representing a family of indexed objects that share the same base type,
   and only differ in upper bounds (with implicit injection) *)
module type Obj_indexed_bound = sig
  module Index : sig
    type t

    val max : t
  end

  type const

  type polarity

  val obj_s : Index.t -> (const * polarity) S.obj
end

module Common_indexed_bound (Obj : Obj_indexed_bound) = struct
  open Obj

  type 'd t = (const * polarity, 'd) S.mode

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type nonrec error = const error

  type equate_error = equate_step * error

  let disallow_right m = S.disallow_right m

  let disallow_left m = S.disallow_left m

  let allow_left m = S.allow_left m

  let allow_right m = S.allow_right m

  let newvar i = S.newvar (obj_s i)

  let min i = S.min (obj_s i)

  let max i = S.max (obj_s i)

  let newvar_above i m = S.newvar_above (obj_s i) m

  let newvar_below i m = S.newvar_below (obj_s i) m

  let submode i m0 m1 : (unit, error) result = S.submode (obj_s i) m0 m1

  (* i gives the upper bound of [l] for short-circuiting *)
  let join i l = S.join (obj_s i) l

  let meet i l = S.meet (obj_s i) l

  let submode_exn i m0 m1 = assert (submode i m0 m1 |> Result.is_ok)

  let equate i = equate_from_submode (submode i)

  let equate_exn i m0 m1 = assert (equate i m0 m1 |> Result.is_ok)

  let print ?(raw = false) ?verbose i ppf m =
    if raw
    then S.print_raw (obj_s i) ?verbose ppf m
    else S.print (obj_s i) ?verbose ppf m

  let zap_to_ceil i m = S.zap_to_ceil (obj_s i) m

  let zap_to_floor i m = S.zap_to_floor (obj_s i) m

  let of_const : type l r. const -> (l * r) t =
   fun a -> S.of_const (obj_s Index.max) a

  let check_const i m = S.check_const (obj_s i) m
end

module Locality = struct
  module Const = C.Locality

  module Obj = struct
    type const = Const.t

    type polarity = S.positive

    let obj_c = C.Locality

    let obj_s : (const * polarity) S.obj = S.Positive obj_c
  end

  include Common (Obj)

  let global = of_const Global

  let local = of_const Local

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Regionality = struct
  module Const = C.Regionality
  module Index = Const.Index

  module Obj = struct
    module Index = Index

    type const = Const.t

    type polarity = S.positive

    let obj_s i : (const * polarity) S.obj = S.Positive (C.Regionality i)
  end

  include Common_indexed_bound (Obj)

  let local i = of_const (Const.local i)

  let escape i = of_const (Const.escape i)

  let global = of_const Const.global

  let zap_to_legacy = zap_to_floor

  let inj ~src ~dst m =
    assert (Index.le src dst);
    S.apply (Obj.obj_s dst) (Pos_Pos (Inj src)) m

  let inj_l ~src ~dst m =
    assert (Index.le dst src);
    S.apply (Obj.obj_s dst) (Pos_Pos (Inj_l src)) (disallow_right m)
end

module Linearity = struct
  module Const = struct
    include C.Linearity

    let legacy = Many
  end

  module Obj = struct
    type const = Const.t

    type polarity = S.positive

    let obj_c = C.Linearity

    let obj_s : (const * polarity) S.obj = S.Positive obj_c
  end

  include Common (Obj)

  let many = of_const Many

  let once = of_const Once

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Uniqueness = struct
  module Const = struct
    include C.Uniqueness

    let legacy = Shared
  end

  module Obj = struct
    type const = Const.t

    (* the negation of Uniqueness_op gives us the proper uniqueness *)
    type polarity = S.negative

    let obj_c = C.Uniqueness_op

    let obj_s : (const * polarity) S.obj = S.Negative obj_c
  end

  include Common (Obj)

  let shared = of_const Shared

  let unique = of_const Unique

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

let unique_to_linear m =
  S.apply (Positive Linearity) (Neg_Pos Unique_to_linear) m

let linear_to_unique m =
  S.apply (Negative Uniqueness_op) (Pos_Neg Linear_to_unique) m

let regional_to_local i m =
  S.apply (Positive Locality) (S.Pos_Pos (C.Regional_to_local i)) m

let locality_as_regionality i m =
  S.apply (Positive (Regionality i)) (S.Pos_Pos C.Locality_as_regionality) m

let regional_to_global i m =
  S.apply (Positive Locality) (S.Pos_Pos (C.Regional_to_global i)) m

module Const = struct
  let unique_to_linear a = C.unique_to_linear a
end

module Comonadic_with_regionality = struct
  module Const = struct
    include C.Comonadic_with_regionality
  end

  module Obj = struct
    module Index = Regionality.Index

    type const = Const.t

    type polarity = S.positive

    let obj i : const C.obj = C.Comonadic_with_regionality i

    let obj_s i : (const * polarity) S.obj = S.Positive (obj i)
  end

  include Common_indexed_bound (Obj)

  let inj ~src ~dst m =
    assert (Regionality.Index.le src dst);
    S.apply (Obj.obj_s dst) (Pos_Pos (Set (SAxis0, Inj src))) m

  let inj_l ~src ~dst m =
    assert (Regionality.Index.le dst src);
    S.apply (Obj.obj_s dst)
      (Pos_Pos (Set (SAxis0, Inj_l src)))
      (disallow_right m)

  let cap_r ~src ~dst m =
    S.apply (Obj.obj_s dst)
      (Pos_Pos (Set (SAxis0, Cap_r src)))
      (disallow_left m)

  let cap ~src ~dst m =
    S.apply (Obj.obj_s dst) (Pos_Pos (Set (SAxis0, Cap src))) m

  type error =
    [ `Regionality of Regionality.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  let regionality i m =
    S.apply (Positive (Regionality i)) (S.Pos_Pos (C.Proj (Obj.obj i, Axis0))) m

  let min_with_regionality i m =
    S.apply (Obj.obj_s i) (S.Pos_Pos (C.Min_with Axis0)) (S.disallow_right m)

  let max_with_regionality i m =
    S.apply (Obj.obj_s i) (S.Pos_Pos (C.Max_with Axis0)) (S.disallow_left m)

  let set_regionality_max i m =
    S.apply (Obj.obj_s i)
      (S.Pos_Pos (C.Set (Product.SAxis0, C.Const_max (Regionality i))))
      (S.disallow_left m)

  let set_regionality_min i m =
    S.apply (Obj.obj_s i)
      (S.Pos_Pos (C.Set (Product.SAxis0, C.Const_min (Regionality i))))
      (S.disallow_right m)

  let linearity i m =
    S.apply (Positive Linearity) (S.Pos_Pos (C.Proj (Obj.obj i, Axis1))) m

  let min_with_linearity i m =
    S.apply (Obj.obj_s i) (S.Pos_Pos (C.Min_with Axis1)) (S.disallow_right m)

  let max_with_linearity i m =
    S.apply (Obj.obj_s i) (S.Pos_Pos (C.Max_with Axis1)) (S.disallow_left m)

  let set_linearity_max i m =
    S.apply (Obj.obj_s i)
      (S.Pos_Pos (C.Set (Product.SAxis1, C.Const_max Linearity)))
      (S.disallow_left m)

  let set_linearity_min i m =
    S.apply (Obj.obj_s i)
      (S.Pos_Pos (C.Set (Product.SAxis1, C.Const_min Linearity)))
      (S.disallow_right m)

  let zap_to_legacy = zap_to_floor

  let legacy = min Obj.Index.max

  (* overriding to report the offending axis *)
  let submode i m0 m1 =
    match submode i m0 m1 with
    | Ok () -> Ok ()
    | Error { left = reg0, lin0; right = reg1, lin1 } ->
      if Regionality.Const.le reg0 reg1
      then
        if Linearity.Const.le lin0 lin1
        then assert false
        else Error (`Linearity { left = lin0; right = lin1 })
      else Error (`Regionality { left = reg0; right = reg1 })

  (* override to report the offending axis *)

  let equate i = equate_from_submode (submode i)

  (** overriding to check per-axis *)
  let check_const i m =
    let regionality = Regionality.check_const i (regionality i m) in
    let linearity = Linearity.check_const (linearity i m) in
    regionality, linearity
end

module Comonadic_with_locality = struct
  module Const = struct
    include C.Comonadic_with_locality
  end

  module Obj = struct
    type const = Const.t

    type polarity = S.positive

    let obj : const C.obj = C.Comonadic_with_locality

    let obj_s : (const * polarity) S.obj = S.Positive obj
  end

  include Common (Obj)

  type error =
    [ `Locality of Locality.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  let locality m =
    S.apply (Positive Locality) (Pos_Pos (Proj (Obj.obj, Axis0))) m

  let min_with_locality m =
    S.apply Obj.obj_s (Pos_Pos (Min_with Axis0)) (S.disallow_right m)

  let max_with_locality m =
    S.apply Obj.obj_s (Pos_Pos (Max_with Axis0)) (S.disallow_left m)

  let set_locality_max m =
    S.apply Obj.obj_s
      (Pos_Pos (Set (SAxis0, Const_max Locality)))
      (S.disallow_left m)

  let set_locality_min m =
    S.apply Obj.obj_s
      (Pos_Pos (Set (SAxis0, Const_min Locality)))
      (S.disallow_right m)

  let linearity m =
    S.apply (Positive Linearity) (Pos_Pos (Proj (Obj.obj, Axis1))) m

  let min_with_linearity m =
    S.apply Obj.obj_s (Pos_Pos (Min_with Axis1)) (S.disallow_right m)

  let max_with_linearity m =
    S.apply Obj.obj_s (Pos_Pos (Max_with Axis1)) (S.disallow_left m)

  let set_linearity_max m =
    S.apply Obj.obj_s
      (Pos_Pos (Set (SAxis1, Const_max Linearity)))
      (S.disallow_left m)

  let set_linearity_min m =
    S.apply Obj.obj_s
      (Pos_Pos (Set (SAxis1, Const_min Linearity)))
      (S.disallow_right m)

  let zap_to_legacy = zap_to_floor

  let legacy = min

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

  let equate = equate_from_submode submode

  (** overriding to check per-axis *)
  let check_const m =
    let locality = Locality.check_const (locality m) in
    let linearity = Linearity.check_const (linearity m) in
    locality, linearity
end

module Monadic = struct
  let uniqueness m = m

  (* secretly just uniqueness *)
  include Uniqueness

  type error = [`Uniqueness of Uniqueness.error]

  type equate_error = equate_step * error

  let max_with_uniqueness m = S.disallow_left m

  let min_with_uniqueness m = S.disallow_right m

  let set_uniqueness_max _ = Uniqueness.max |> S.disallow_left |> S.allow_right

  let set_uniqueness_min _ = Uniqueness.min |> S.disallow_right |> S.allow_left

  let submode m0 m1 =
    match submode m0 m1 with Ok () -> Ok () | Error e -> Error (`Uniqueness e)

  let equate = equate_from_submode submode
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

  let min i =
    { comonadic = Comonadic.min i;
      monadic = Monadic.min |> Monadic.allow_left |> Monadic.allow_right
    }

  let max i =
    { comonadic = Comonadic.max i;
      monadic = Monadic.max |> Monadic.allow_left |> Monadic.allow_right
    }

  let disallow_right = Obj.magic

  let disallow_left = Obj.magic

  let allow_right = Obj.magic

  let allow_left = Obj.magic

  let newvar i =
    let comonadic = Comonadic.newvar i in
    let monadic = Monadic.newvar () in
    { comonadic; monadic }

  let newvar_above i { comonadic; monadic } =
    let comonadic, b0 = Comonadic.newvar_above i comonadic in
    let monadic, b1 = Monadic.newvar_above monadic in
    { monadic; comonadic }, b0 || b1

  let newvar_below i { comonadic; monadic } =
    let comonadic, b0 = Comonadic.newvar_below i comonadic in
    let monadic, b1 = Monadic.newvar_below monadic in
    { monadic; comonadic }, b0 || b1

  let uniqueness { monadic; _ } = Monadic.uniqueness monadic

  let linearity i { comonadic; _ } = Comonadic.linearity i comonadic

  let regionality i { comonadic; _ } = Comonadic.regionality i comonadic

  let inj ~src ~dst { comonadic; monadic } =
    let comonadic = Comonadic.inj ~src ~dst comonadic in
    { comonadic; monadic }

  let inj_l ~src ~dst { comonadic; monadic } =
    let comonadic = Comonadic.inj_l ~src ~dst comonadic in
    let monadic = Monadic.disallow_right monadic in
    { comonadic; monadic }

  let cap_r ~src ~dst { comonadic; monadic } =
    let comonadic = Comonadic.cap_r ~src ~dst comonadic in
    let monadic = Monadic.disallow_left monadic in
    { comonadic; monadic }

  let cap ~src ~dst { comonadic; monadic } =
    let comonadic = Comonadic.cap ~src ~dst comonadic in
    { comonadic; monadic }

  type error =
    [ `Regionality of Regionality.error
    | `Uniqueness of Uniqueness.error
    | `Linearity of Linearity.error ]

  type equate_error = equate_step * error

  (* NB: state mutated when error *)
  let submode i { monadic = monadic0; comonadic = comonadic0 }
      { monadic = monadic1; comonadic = comonadic1 } =
    (* comonadic before monadic, so that locality errors dominate
       (error message backward compatibility) *)
    match Comonadic.submode i comonadic0 comonadic1 with
    | Error e -> Error e
    | Ok () -> (
      match Monadic.submode monadic0 monadic1 with
      | Error e -> Error e
      | Ok () -> Ok ())

  let equate i = equate_from_submode (submode i)

  let submode_exn i m0 m1 =
    match submode i m0 m1 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let equate_exn i m0 m1 =
    match equate i m0 m1 with
    | Ok () -> ()
    | Error _ -> invalid_arg "equate_exn"

  let print ?raw ?verbose i ppf { monadic; comonadic } =
    Format.fprintf ppf "%a,%a"
      (Comonadic.print ?raw ?verbose i)
      comonadic
      (Monadic.print ?raw ?verbose ())
      monadic

  let zap_to_floor i { comonadic; monadic } =
    match Monadic.zap_to_floor monadic, Comonadic.zap_to_floor i comonadic with
    | uniqueness, (locality, linearity) -> locality, linearity, uniqueness

  let zap_to_ceil i { comonadic; monadic } =
    match Monadic.zap_to_ceil monadic, Comonadic.zap_to_ceil i comonadic with
    | uniqueness, (locality, linearity) -> locality, linearity, uniqueness

  let zap_to_legacy i { comonadic; monadic } =
    match
      Monadic.zap_to_legacy monadic, Comonadic.zap_to_legacy i comonadic
    with
    | uniqueness, (locality, linearity) -> locality, linearity, uniqueness

  let check_const i { comonadic; monadic } =
    let locality, linearity = Comonadic.check_const i comonadic in
    let uniqueness = Monadic.check_const monadic in
    locality, linearity, uniqueness

  let of_const (locality, linearity, uniqueness) =
    let comonadic = Comonadic.of_const (locality, linearity) in
    let monadic = Monadic.of_const uniqueness in
    { comonadic; monadic }

  let legacy =
    let comonadic = Comonadic.legacy in
    let monadic = Monadic.legacy in
    { comonadic; monadic }

  let max_with_uniqueness i uniqueness =
    let comonadic =
      Comonadic.max i |> Comonadic.disallow_left |> Comonadic.allow_right
    in
    let monadic = Monadic.max_with_uniqueness uniqueness in
    { comonadic; monadic }

  let min_with_uniqueness i uniqueness =
    let comonadic =
      Comonadic.min i |> Comonadic.disallow_right |> Comonadic.allow_left
    in
    let monadic = Monadic.min_with_uniqueness uniqueness in
    { comonadic; monadic }

  let set_uniqueness_max { monadic; comonadic } =
    let comonadic = Comonadic.disallow_left comonadic in
    let monadic = Monadic.set_uniqueness_max monadic in
    { monadic; comonadic }

  let set_uniqueness_min { monadic; comonadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    let monadic = Monadic.set_uniqueness_min monadic in
    { monadic; comonadic }

  let min_with_regionality i regionality =
    let comonadic = Comonadic.min_with_regionality i regionality in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_regionality i regionality =
    let comonadic = Comonadic.max_with_regionality i regionality in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let set_regionality_min i { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.set_regionality_min i comonadic in
    { comonadic; monadic }

  let set_regionality_max i { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.set_regionality_max i comonadic in
    { comonadic; monadic }

  let min_with_linearity i linearity =
    let comonadic = Comonadic.min_with_linearity i linearity in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_linearity i linearity =
    let comonadic = Comonadic.max_with_linearity i linearity in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let set_linearity_max i { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.set_linearity_max i comonadic in
    { comonadic; monadic }

  let set_linearity_min i { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.set_linearity_min i comonadic in
    { comonadic; monadic }

  let join i l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.join i como in
    let monadic = Monadic.join mo in
    { comonadic; monadic }

  let meet i l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.meet i como in
    let monadic = Monadic.meet mo in
    { comonadic; monadic }

  module Const = struct
    type t = Regionality.Const.t * Linearity.Const.t * Uniqueness.Const.t

    let min = Regionality.Const.min, Linearity.Const.min, Uniqueness.Const.min

    let max i =
      Regionality.Const.max i, Linearity.Const.max, Uniqueness.Const.max

    let le (locality0, linearity0, uniqueness0)
        (locality1, linearity1, uniqueness1) =
      Regionality.Const.le locality0 locality1
      && Uniqueness.Const.le uniqueness0 uniqueness1
      && Linearity.Const.le linearity0 linearity1

    let print ppf m = print Regionality.Index.max ppf (of_const m)

    let legacy =
      Regionality.Const.global, Linearity.Const.legacy, Uniqueness.Const.legacy

    let meet (l0, l1, l2) (r0, r1, r2) =
      ( Regionality.Const.meet l0 r0,
        Linearity.Const.meet l1 r1,
        Uniqueness.Const.meet l2 r2 )

    let join (l0, l1, l2) (r0, r1, r2) =
      ( Regionality.Const.join l0 r0,
        Linearity.Const.join l1 r1,
        Uniqueness.Const.join l2 r2 )
  end
end

module Alloc = struct
  module Comonadic = Comonadic_with_locality
  module Monadic = Monadic

  type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  let min = { comonadic = Comonadic.min; monadic = Monadic.min }

  let max = { comonadic = Comonadic.min; monadic = Monadic.max }

  let disallow_right = Obj.magic

  let disallow_left = Obj.magic

  let allow_right = Obj.magic

  let allow_left = Obj.magic

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

  let zap_to_floor { comonadic; monadic } =
    match Monadic.zap_to_floor monadic, Comonadic.zap_to_floor comonadic with
    | uniqueness, (locality, linearity) -> locality, linearity, uniqueness

  let zap_to_ceil { comonadic; monadic } =
    match Monadic.zap_to_ceil monadic, Comonadic.zap_to_ceil comonadic with
    | uniqueness, (locality, linearity) -> locality, linearity, uniqueness

  let zap_to_legacy { comonadic; monadic } =
    match Monadic.zap_to_legacy monadic, Comonadic.zap_to_legacy comonadic with
    | uniqueness, (locality, linearity) -> locality, linearity, uniqueness

  let check_const { comonadic; monadic } =
    let locality, linearity = Comonadic.check_const comonadic in
    let uniqueness = Monadic.check_const monadic in
    locality, linearity, uniqueness

  let of_const (locality, linearity, uniqueness) =
    let comonadic = Comonadic.of_const (locality, linearity) in
    let monadic = Monadic.of_const uniqueness in
    { comonadic; monadic }

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

  let set_uniqueness_max { monadic; comonadic } =
    let comonadic = Comonadic.disallow_left comonadic in
    let monadic = Monadic.set_uniqueness_max monadic in
    { monadic; comonadic }

  let set_uniqueness_min { monadic; comonadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    let monadic = Monadic.set_uniqueness_min monadic in
    { monadic; comonadic }

  let min_with_locality locality =
    let comonadic = Comonadic.min_with_locality locality in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_locality locality =
    let comonadic = Comonadic.max_with_locality locality in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let set_locality_min { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.set_locality_min comonadic in
    { comonadic; monadic }

  let set_locality_max { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.set_locality_max comonadic in
    { comonadic; monadic }

  let min_with_linearity linearity =
    let comonadic = Comonadic.min_with_linearity linearity in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let max_with_linearity linearity =
    let comonadic = Comonadic.max_with_linearity linearity in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let set_linearity_max { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.set_linearity_max comonadic in
    { comonadic; monadic }

  let set_linearity_min { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.set_linearity_min comonadic in
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

  module Const = struct
    type t = Locality.Const.t * Linearity.Const.t * Uniqueness.Const.t

    let min = Locality.Const.min, Linearity.Const.min, Uniqueness.Const.min

    let max = Locality.Const.max, Linearity.Const.max, Uniqueness.Const.max

    let le (locality0, linearity0, uniqueness0)
        (locality1, linearity1, uniqueness1) =
      Locality.Const.le locality0 locality1
      && Uniqueness.Const.le uniqueness0 uniqueness1
      && Linearity.Const.le linearity0 linearity1

    let print ppf m = print () ppf (of_const m)

    let legacy =
      Locality.Const.legacy, Linearity.Const.legacy, Uniqueness.Const.legacy

    let meet (l0, l1, l2) (r0, r1, r2) =
      ( Locality.Const.meet l0 r0,
        Linearity.Const.meet l1 r1,
        Uniqueness.Const.meet l2 r2 )

    let join (l0, l1, l2) (r0, r1, r2) =
      ( Locality.Const.join l0 r0,
        Linearity.Const.join l1 r1,
        Uniqueness.Const.join l2 r2 )

    (** constrain uncurried function ret_mode from arg_mode *)
    let close_over (locality, linearity, uniqueness) =
      let locality' = locality in
      (* uniqueness of the returned function is not constrained *)
      let uniqueness' = Uniqueness.Const.min in
      let linearity' =
        Linearity.Const.join linearity
          (* In addition, unique argument make the returning function once.
             In other words, if argument <= unique, returning function >= once.
             That is, returning function >= (dual of argument) *)
          (Const.unique_to_linear uniqueness)
      in
      locality', linearity', uniqueness'

    (** constrain uncurried function ret_mode from the mode of the whole function *)
    let partial_apply (locality, linearity, _) =
      let locality' = locality in
      let uniqueness' = Uniqueness.Const.min in
      let linearity' = linearity in
      locality', linearity', uniqueness'
  end

  let close_over comonadic monadic =
    let locality = min_with_locality (Comonadic.locality comonadic) in
    (* uniqueness of the returned function is not constrained *)
    let linearity0 = min_with_linearity (Comonadic.linearity comonadic) in
    let linearity1 =
      min_with_linearity (unique_to_linear (Monadic.uniqueness monadic))
    in
    join [locality; linearity0; linearity1]

  let partial_apply alloc_mode = set_uniqueness_min alloc_mode
end

(* module Consts_mono_padded (L : Const) = struct
     type index = unit
     type nonrec t = L.t

     let min () = L.min
     let max () = L.max
     let join () = L.join
     let meet () = L.meet
     let le () = L.le
     let print () = L.print
     let legacy () = L.legacy
   end *)

let alloc_as_value i m =
  let { comonadic; monadic } = Alloc.disallow_right m in
  let comonadic =
    S.apply
      (Value.Comonadic.Obj.obj_s i)
      (Pos_Pos (Set (SAxis0, Locality_as_regionality)))
      comonadic
  in
  { comonadic; monadic }

let alloc_to_value_l2r i m =
  let { comonadic; monadic } = Alloc.disallow_right m in
  let comonadic =
    S.apply
      (Value.Comonadic.Obj.obj_s i)
      (Pos_Pos (Set (SAxis0, Local_to_regional)))
      comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2g i m =
  let { comonadic; monadic } = Alloc.disallow_right m in
  let comonadic =
    S.apply Alloc.Comonadic.Obj.obj_s
      (Pos_Pos (Set (SAxis0, Regional_to_global i)))
      comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2l i m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.apply Alloc.Comonadic.Obj.obj_s
      (Pos_Pos (Set (SAxis0, Regional_to_local i)))
      comonadic
  in
  { comonadic; monadic }
