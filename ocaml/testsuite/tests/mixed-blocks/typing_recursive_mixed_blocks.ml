(* TEST
 flags = "-extension layouts_beta";
 expect;
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

type cstr = A of cstr * float#
[%%expect {|
type cstr = A of cstr * float#
|}];;

(* OK: the recursive use is for a field in the value prefix. *)
let rec rec_cstr = A (rec_cstr, #4.0)
[%%expect {|
val rec_cstr : cstr = A (<cycle>, <abstr>)
|}];;

(* Error: the recursive use is for a field in the flat suffix *)
let rec bad_flat = let _ = A (rec_cstr, bad_flat) in #4.0;;
[%%expect {|
Line 1, characters 19-57:
1 | let rec bad_flat = let _ = A (rec_cstr, bad_flat) in #4.0;;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec good_block = let _ = A (good_block, #4.0) in A (good_block, #4.0);;

[%%expect {|
val good_block : cstr = A (<cycle>, <abstr>)
|}];;
