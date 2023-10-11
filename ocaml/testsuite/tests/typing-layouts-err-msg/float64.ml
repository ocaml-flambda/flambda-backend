(* TEST
   flags = "-extension layouts_alpha"
 * expect
*)

(*************************)
(* Float64 layout errors *)

(* Primitive *)
let f (x: float#): ('a : value) = x
[%%expect{|
Line 1, characters 34-35:
1 | let f (x: float#): ('a : value) = x
                                      ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         of the annotation on the type variable 'a.
|}];;
