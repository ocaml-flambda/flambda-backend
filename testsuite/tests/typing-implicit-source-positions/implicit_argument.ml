(* TEST_BELOW
Fille
*)

let f = fun ~(call_pos:[%call_pos]) () -> call_pos
[%%expect{|
val f : call_pos:[%call_pos] -> unit -> lexing_position @@ global many =
  <fun>
|}]

let _ = f ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 173; pos_cnum = 181}
|}]

let j = (f : unit -> lexing_position);;
[%%expect{|
val j : unit -> lexing_position @@ global many = <fun>
|}]

let g = fun ~(a:[%call_pos]) ?(c = 0) ~(b:[%call_pos]) () -> a, b
[%%expect{|
val g :
  a:[%call_pos] ->
  ?c:int -> b:[%call_pos] -> unit -> lexing_position * lexing_position @@
  global many = <fun>
|}]

let _ = g () ;;
[%%expect{|
- : lexing_position * lexing_position =
({pos_fname = ""; pos_lnum = 1; pos_bol = 607; pos_cnum = 615},
 {pos_fname = ""; pos_lnum = 1; pos_bol = 607; pos_cnum = 615})
|}]

let h ~(a:[%call_pos]) ~(b:[%call_pos]) () : lexing_position * lexing_position
  = a, b
[%%expect{|
val h :
  a:[%call_pos] -> b:[%call_pos] -> unit -> lexing_position * lexing_position
  @@ global many = <fun>
|}]

(* Partial application *)
let x = h ~b:{Lexing.dummy_pos with pos_fname = "b"};;
[%%expect{|
val x : a:[%call_pos] -> unit -> lexing_position * lexing_position @@ global
  many = <fun>
|}]

let y = x ();;
[%%expect{|
val y : lexing_position * lexing_position @@ global many =
  ({pos_fname = ""; pos_lnum = 1; pos_bol = 1214; pos_cnum = 1222},
   {pos_fname = "b"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1})
|}]

let k = (f : unit -> lexing_position);;
[%%expect{|
val k : unit -> lexing_position @@ global many = <fun>
|}]

let _ = j ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 289; pos_cnum = 298}
|}]

let _ = k ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 1437; pos_cnum = 1446}
|}]

let m ~(call_pos:[%call_pos]) = ()
[%%expect {|
Line 1, characters 8-16:
1 | let m ~(call_pos:[%call_pos]) = ()
            ^^^^^^^^
Warning 188 [unerasable-position-argument]: this position argument cannot be erased.

val m : call_pos:[%call_pos] -> unit @@ global many = <fun>
|}]


(* TEST
 expect;
*)
