
type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

val map_foo : ('a -> 'a) -> (unit -> 'a node) -> unit -> 'a node
