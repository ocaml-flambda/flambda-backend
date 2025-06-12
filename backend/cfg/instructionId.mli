[@@@ocaml.warning "+a-40-41-42"]

type t

include Identifiable.S with type t := t

val none : t

val max : t -> t -> t

val equal : t -> t -> bool

val to_string : t -> string

val to_string_padded : t -> string

val format : Format.formatter -> t -> unit

type sequence

val make_sequence : ?last_used:t -> unit -> sequence

val reset : sequence -> unit

val get : sequence -> t

val get_and_incr : sequence -> t

val to_int_unsafe : t -> int
