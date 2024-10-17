type (_, _) t =
  | Pair
       : ('a1, 'a2) Category.t * ('b1, 'b2) Category_b.t
      -> ('a1 * 'b1, 'a2 * 'b2) t
  | Id : ('a, 'a) t

include Category_intf.S with type ('a, 'b) t := ('a, 'b) t

val to_pair
   : ('a1 * 'b1, 'a2 * 'b2) t
  -> ('a1, 'a2) Category.t * ('b1, 'b2) Category_b.t
