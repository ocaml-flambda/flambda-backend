include Identifiable.S

val create : make_symbol:(string -> string) -> string -> t

val encode : ?without_prefix:unit -> t -> string