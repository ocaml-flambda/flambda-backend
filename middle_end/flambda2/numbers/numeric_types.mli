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

(** Modules about numbers, some of which satisfy {!Container_types.S}.

    {b Warning:} this module is unstable and part of
    {{!Compiler_libs}compiler-libs}. *)

module Int : sig
  include Container_types.S_plus_stdlib with type t = int

  (** [zero_to_n n] is the set of numbers \{0, ..., n\} (inclusive). *)
  val zero_to_n : int -> Set.t

  val to_string : int -> string
end

module Int8 : sig
  type t

  val zero : t

  val one : t

  val of_int_exn : int -> t

  val to_int : t -> int
end

module Int16 : sig
  type t

  val of_int_exn : int -> t

  val of_int64_exn : Int64.t -> t

  val to_int : t -> int
end

module Int32 : sig
  include module type of struct
    include Int32
  end

  include Container_types.S with type t := Int32.t

  val swap_byte_endianness : t -> t

  module Pair : Container_types.S with type t = t * t

  val cross_product : Set.t -> Set.t -> Pair.Set.t
end

module Int64 : sig
  include module type of struct
    include Int64
  end

  include Container_types.S with type t := Int64.t

  val swap_byte_endianness : t -> t

  module Pair : Container_types.S with type t = t * t

  val cross_product : Set.t -> Set.t -> Pair.Set.t
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
  (** Floating point numbers whose comparison and equality relations are the
      usual [Int64/Int32] relations on the bit patterns of the floats. This in
      particular means that different representations of NaN will be
      distinguished, as will the two signed zeros.

      Never use [Stdlib.compare] on values of type [t]. Use either [compare]
      (comparison on bit patterns) or [IEEE_semantics.compare] depending on
      which semantics you want. Likewise for equality. *)

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

module Float_by_bit_pattern : Float_by_bit_pattern with type bits = int64

module Float32_by_bit_pattern : Float_by_bit_pattern with type bits = int32
