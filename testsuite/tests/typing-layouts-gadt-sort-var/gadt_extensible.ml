type t_ext = ..

type t_ext +=
| Mk : (unit -> 'a) * ('a -> unit) -> t_ext
