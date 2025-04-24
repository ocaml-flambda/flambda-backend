(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int_base = Container_types.Make (struct
  type t = int

  let compare = Int.compare

  let hash i = i

  let equal (i : int) j = i = j

  let print = Format.pp_print_int
end)

module Int = struct
  type t = int

  include Int_base

  let rec zero_to_n n =
    if n < 0 then Set.empty else Set.add n (zero_to_n (n - 1))

  let to_string n = Int.to_string n
end

module Short_int (M : sig
  val num_bits : int
end) =
struct
  let num_bits = M.num_bits

  let () = assert (0 < num_bits && num_bits < 31)

  module T0 = struct
    type t = int

    let zero = 0

    let one = 1

    let min_int64 = Int64.shift_left Int64.minus_one (num_bits - 1)

    let max_int64 = Int64.lognot min_int64

    let of_int64_exn i =
      if Int64.compare i min_int64 < 0 || Int64.compare i max_int64 > 0
      then Misc.fatal_errorf "Int%d: %Ld is out of range" num_bits i
      else Int64.to_int i

    let of_int_exn i = of_int64_exn (Int64.of_int i)

    let of_int i =
      let extra_bits = Sys.int_size - num_bits in
      (i lsl extra_bits) asr extra_bits

    let to_int i = i

    let to_float = Float.of_int

    let of_float f = of_int (Float.to_int f)

    let compare = Int.compare

    let equal = Int.equal

    let hash = (Hashtbl.hash : t -> int)

    let print = Format.pp_print_int
  end

  include T0
  include Container_types.Make (T0)
end

module Int8 = Short_int (struct
  let num_bits = 8
end)

module Int16 = Short_int (struct
  let num_bits = 16
end)

module Int32 = struct
  include Int32

  external swap_byte_endianness : t -> t = "%bswap_int32"

  module T0 = struct
    type t = Int32.t

    let compare x y = Int32.compare x y

    let equal t1 t2 = compare t1 t2 = 0

    let hash f = Hashtbl.hash f

    let print ppf t = Format.fprintf ppf "%ld" t
  end

  module Self = Container_types.Make (T0)
  include Self

  module Pair = struct
    include
      Container_types.Make_pair
        (struct
          type nonrec t = t

          include Self
        end)
        (struct
          type nonrec t = t

          include Self
        end)

    type nonrec t = t * t
  end

  let cross_product = Pair.create_from_cross_product
end

module Int64 = struct
  include Int64

  external swap_byte_endianness : t -> t = "%bswap_int64"

  module T0 = struct
    type t = Int64.t

    let compare x y = Int64.compare x y

    let equal t1 t2 = compare t1 t2 = 0

    let hash f = Hashtbl.hash f

    let print ppf t = Format.fprintf ppf "%Ld" t
  end

  module Self = Container_types.Make (T0)
  include Self

  module Pair = struct
    include
      Container_types.Make_pair
        (struct
          type nonrec t = t

          include Self
        end)
        (struct
          type nonrec t = t

          include Self
        end)

    type nonrec t = t * t
  end

  let cross_product = Pair.create_from_cross_product
end

module type IEEE_semantics = sig
  type t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val mod_ : t -> t -> t

  val neg : t -> t

  val abs : t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

module type Float_by_bit_pattern = sig
  type bits

  include Container_types.S

  val create : float -> t

  val of_bits : bits -> t

  val to_bits : t -> bits

  val of_string : string -> t

  val to_float : t -> float

  val one : t

  val zero : t

  val minus_one : t

  val is_either_zero : t -> bool

  val is_any_nan : t -> bool

  module IEEE_semantics : IEEE_semantics with type t = t

  module Pair : Container_types.S with type t = t * t

  val cross_product : Set.t -> Set.t -> Pair.Set.t
end

module Float_by_bit_pattern_gen (Bits : sig
  type t

  val bits_of_float : float -> t

  val float_of_bits : t -> float

  val of_string : string -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  module IEEE_semantics : IEEE_semantics with type t = t
end) =
struct
  type bits = Bits.t

  let create f = Bits.bits_of_float f

  let of_bits bits = bits

  let to_bits bits = bits

  let of_string str = Bits.of_string str

  let to_float t = Bits.float_of_bits t

  let zero = create 0.

  let one = create 1.

  let minus_one = create (-1.)

  module T0 = struct
    type t = Bits.t

    let compare = Bits.compare

    let equal = Bits.equal

    let hash f = Hashtbl.hash f

    let print ppf t = Format.pp_print_float ppf (Bits.float_of_bits t)
  end

  include T0
  module Self = Container_types.Make (T0)
  include Self

  module Pair = struct
    include
      Container_types.Make_pair
        (struct
          type nonrec t = t

          include Self
        end)
        (struct
          type nonrec t = t

          include Self
        end)

    type nonrec t = t * t
  end

  let cross_product = Pair.create_from_cross_product

  module IEEE_semantics = Bits.IEEE_semantics

  let is_any_nan t =
    match classify_float (to_float t) with
    | FP_nan -> true
    | FP_normal | FP_subnormal | FP_infinite | FP_zero -> false

  let is_either_zero t =
    match classify_float (to_float t) with
    | FP_zero -> true
    | FP_normal | FP_subnormal | FP_infinite | FP_nan -> false
end

module Float_by_bit_pattern = Float_by_bit_pattern_gen (struct
  include Int64

  let of_string str = bits_of_float (float_of_string str)

  module IEEE_semantics = struct
    type nonrec t = t

    let of_ = bits_of_float

    let to_ = float_of_bits

    let add t1 t2 = of_ (Stdlib.( +. ) (to_ t1) (to_ t2))

    let sub t1 t2 = of_ (Stdlib.( -. ) (to_ t1) (to_ t2))

    let mul t1 t2 = of_ (Stdlib.( *. ) (to_ t1) (to_ t2))

    let div t1 t2 = of_ (Stdlib.( /. ) (to_ t1) (to_ t2))

    let mod_ t1 t2 = of_ (Stdlib.mod_float (to_ t1) (to_ t2))

    let neg t = of_ (Stdlib.( ~-. ) (to_ t))

    let abs t = of_ (Stdlib.abs_float (to_ t))

    let compare t1 t2 = Stdlib.compare (to_ t1) (to_ t2)

    let equal t1 t2 =
      (* N.B. This can't just be defined in terms of [compare_ieee]! *)
      Stdlib.( = ) (to_ t1) (to_ t2)
  end
end)

module Float32_by_bit_pattern = Float_by_bit_pattern_gen (struct
  include Int32
  module F32 = Flambda2_floats.Float32

  let of_string str = F32.to_bits (F32.of_string str)

  module IEEE_semantics = struct
    type nonrec t = t

    let of_ = F32.to_bits

    let to_ = F32.of_bits

    let add t1 t2 = of_ (F32.add (to_ t1) (to_ t2))

    let sub t1 t2 = of_ (F32.sub (to_ t1) (to_ t2))

    let mul t1 t2 = of_ (F32.mul (to_ t1) (to_ t2))

    let div t1 t2 = of_ (F32.div (to_ t1) (to_ t2))

    let mod_ t1 t2 = of_ (F32.mod_ (to_ t1) (to_ t2))

    let neg t = of_ (F32.neg (to_ t))

    let abs t = of_ (F32.abs (to_ t))

    let compare t1 t2 = F32.compare (to_ t1) (to_ t2)

    let equal t1 t2 =
      (* N.B. This can't just be defined in terms of [compare_ieee]! *)
      F32.equal (to_ t1) (to_ t2)
  end
end)
