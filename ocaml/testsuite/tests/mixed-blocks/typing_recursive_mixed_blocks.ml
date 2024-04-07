(* TEST
 flags = "-extension layouts_alpha"

 * expect
*)

type rec_t = { rec_t : rec_t; x1 : float# }
type t = { t : rec_t; x2 : float# }

[%%expect {|
type rec_t = { rec_t : rec_t; x1 : float#; }
type t = { t : rec_t; x2 : float#; }
|}];;

(* OK: the recursive use is for a field in the value prefix. *)
let rec rec_t = { rec_t; x1 = #4.0 }

[%%expect {|
val rec_t : rec_t = {rec_t = <cycle>; x1 = <abstr>}
|}];;

(* Error: the recursive use is for a field in the flat suffix *)
let rec x2 = let _ = { t = rec_t; x2 } in #4.0;;

[%%expect {|
Line 1, characters 13-46:
1 | let rec x2 = let _ = { t = rec_t; x2 } in #4.0;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec rec_t = let _ = { rec_t; x1 = #4.0 } in { rec_t; x1 = #4.0 };;

[%%expect {|
val rec_t : rec_t = {rec_t = <cycle>; x1 = <abstr>}
|}];;
