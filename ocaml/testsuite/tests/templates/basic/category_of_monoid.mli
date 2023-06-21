type ('a, 'b) t = Monoid.t

val id : ('a, 'a) t
val compose : first:('a, 'b) t -> second:('b, 'c) t -> ('a, 'c) t
