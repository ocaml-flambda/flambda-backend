(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Record *)

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
Line 1, characters 34-36:
1 | let rec x2 = let _ = { t = rec_t; x2 } in #4.0;;
                                      ^^
Error: This expression has type ('a : value)
       but an expression was expected of type float#
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's the type of the recursive variable x2.
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec rec_t = let _ = { rec_t; x1 = #4.0 } in { rec_t; x1 = #4.0 };;

[%%expect {|
val rec_t : rec_t = {rec_t = <cycle>; x1 = <abstr>}
|}];;

(* Constructor: tupled args *)

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
Line 1, characters 40-48:
1 | let rec bad_flat = let _ = A (rec_cstr, bad_flat) in #4.0;;
                                            ^^^^^^^^
Error: This expression has type ('a : value)
       but an expression was expected of type float#
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's the type of the recursive variable bad_flat.
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec good_block = let _ = A (good_block, #4.0) in A (good_block, #4.0);;

[%%expect {|
val good_block : cstr = A (<cycle>, <abstr>)
|}];;

(* Constructor: inline record args *)

type cstr = A of { cstr : cstr; flt : float# }
[%%expect {|
type cstr = A of { cstr : cstr; flt : float#; }
|}];;

(* OK: the recursive use is for a field in the value prefix. *)
let rec rec_cstr = A { cstr = rec_cstr; flt = #4.0 }
[%%expect {|
val rec_cstr : cstr = A {cstr = <cycle>; flt = <abstr>}
|}];;

(* Error: the recursive use is for a field in the flat suffix *)
let rec bad_flat = let _ = A { cstr = rec_cstr; flt = bad_flat } in #4.0;;
[%%expect {|
Line 1, characters 54-62:
1 | let rec bad_flat = let _ = A { cstr = rec_cstr; flt = bad_flat } in #4.0;;
                                                          ^^^^^^^^
Error: This expression has type ('a : value)
       but an expression was expected of type float#
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's the type of the recursive variable bad_flat.
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec good_block = let _ = A { cstr = good_block; flt = #4.0 } in
                     A { cstr = good_block; flt = #4.0 };;

[%%expect {|
val good_block : cstr = A {cstr = <cycle>; flt = <abstr>}
|}];;
