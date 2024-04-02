type t

val none : t

val create : strict:bool -> never_returns_normally:bool -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val join : t -> t -> t

val meet : t -> t -> t

val to_string : t -> string

val print : Format.formatter -> t -> unit

val is_none : t -> bool

module Witnesses : sig
  type t = unit

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

include module type of Zero_alloc_utils.Make (Witnesses)

val get_value : t -> Value.t option
