(* TEST
 expect;
*)

(* An example where [type_argument] can refine modes using a gadt equation. *)
type _ t =
  | A : bool t

let id x = x

let f (type output) ~(output : output t) =
    match output with
    | A -> (id : bool -> output)
[%%expect{|
type _ t = A : bool t
val id : 'a -> 'a = <fun>
Line 8, characters 11-32:
8 |     | A -> (id : bool -> output)
               ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "bool -> output"
       but an expression was expected of type "'a"
       This instance of "bool" is ambiguous:
       it would escape the scope of its equation
|}]
