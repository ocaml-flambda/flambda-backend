type ('a, 'b) t = Monoid.t

val id : ('a, 'a) t
val compose : first:('a, 'b) t -> second:('b, 'c) t -> ('a, 'c) t

(* Demonstrate that we can have extra functions beyond what's required by the
   .mli for the parameter type *)
val as_unit : (_, _) t -> (unit, unit) t
