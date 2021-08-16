(** Modules about numbers, some of which satisfy {!Identifiable.S}.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.
*)

module Int = Numbers.Int

module Float = Numbers.Float

module Int8 : sig 
  include module type of Numbers.Int8 
  val print : Format.formatter -> t -> unit
end

module Int16 : sig 
  include module type of Numbers.Int16 
  val zero : t
  val one : t
  val print : Format.formatter -> t -> unit
end

(** Do not use polymorphic comparison on the unsigned integer types. *)

module Uint8 : sig
  type t

  val print : Format.formatter -> t -> unit

  val zero : t
  val one : t

  val of_nonnegative_int_exn : int -> t
  val to_int : t -> int
end

module Uint16 : sig
  type t

  val print : Format.formatter -> t -> unit

  val of_nonnegative_int_exn : int -> t
  val of_nonnegative_int64_exn : Int64.t -> t

  val to_int : t -> int
end

module Uint32 : sig
  type t

  val print : Format.formatter -> t -> unit

  val zero : t

  val of_nonnegative_int_exn : int -> t
  val of_nonnegative_int32_exn : Int32.t -> t
  val of_nonnegative_int64_exn : Int64.t -> t

  val to_int64 : t -> Int64.t
end

module Uint64 : sig
  type t

  val zero : t

  val succ : t -> t

  val of_uint8 : Uint8.t -> t
  val of_uint16 : Uint16.t -> t
  val of_uint32 : Uint32.t -> t

  val of_nonnegative_int_exn : int -> t
  val of_nonnegative_int32_exn : Int32.t -> t
  val of_nonnegative_int64_exn : Int64.t -> t

  val to_int64 : t -> Int64.t

  include Identifiable.S with type t := t
end