type t =
  | Mk : (unit -> 'a) * ('a -> unit) -> t
