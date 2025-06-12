[@@@ocaml.warning "+a-40-41-42"]

type t =
  { src : Label.t;
    dst : Label.t
  }

val compare : t -> t -> int

module Map : Map.S with type key = t

module Set : Set.S with type elt = t
