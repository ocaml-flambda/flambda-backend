type t =
  | C0
  | C1
  | B0 of int
  | B1 of int

val emit : t list -> unit
