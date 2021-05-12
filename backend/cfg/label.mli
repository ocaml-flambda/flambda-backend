[@@@ocaml.warning "+a-30-40-41-42"]

type t = int

include Identifiable.S with type t := t

val equal : t -> t -> bool

val to_string : t -> string
