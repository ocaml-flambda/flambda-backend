module type S = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val print : Format.formatter -> t -> unit

  val min_value : t

  val max_value : t

  val minus_one : t

  val zero : t

  val one : t

  val ten : t

  val hex_ff : t

  val ( <= ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( < ) : t -> t -> bool

  val bottom_byte_to_int : t -> int

  val of_char : char -> t

  val of_int : int -> t

  val of_int_option : int -> t option

  val of_int32 : int32 -> t

  val of_int64 : int64 -> t

  val of_targetint : Targetint_32_64.t -> t

  val of_float : float -> t

  val to_float : t -> float

  val to_int : t -> int

  val to_int_exn : t -> int

  val to_int_option : t -> int option

  val to_int32 : t -> int32

  val to_int64 : t -> int64

  val to_targetint : t -> Targetint_32_64.t

  val neg : t -> t

  val get_least_significant_16_bits_then_byte_swap : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val mod_ : t -> t -> t

  val div : t -> t -> t

  val and_ : t -> t -> t

  val or_ : t -> t -> t

  val xor : t -> t -> t

  val shift_left : t -> int -> t

  val shift_right : t -> int -> t

  val shift_right_logical : t -> int -> t

  val max : t -> t -> t

  val min : t -> t -> t
end

module Make : functor (I : S) -> S with type t = I.t
