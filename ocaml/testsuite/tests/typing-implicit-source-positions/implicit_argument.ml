(* TEST
   * expect
*)

let f = fun ~(src_pos:[%src_pos]) () -> src_pos
[%%expect{|
val f : src_pos:[%src_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 151; pos_cnum = 159}
|}]

let j = (f : unit -> lexing_position);;
[%%expect{|
val j : unit -> lexing_position = <fun>
|}]

let g = fun ~(a:[%src_pos]) ?(c = 0) ~(b:[%src_pos]) () -> a, b
[%%expect{|
val g :
  a:[%src_pos] ->
  ?c:int -> b:[%src_pos] -> unit -> lexing_position * lexing_position = <fun>
|}]

let _ = g () ;;
[%%expect{|
- : lexing_position * lexing_position =
({pos_fname = ""; pos_lnum = 1; pos_bol = 549; pos_cnum = 557},
 {pos_fname = ""; pos_lnum = 1; pos_bol = 549; pos_cnum = 557})
|}]

let h ~(a:[%src_pos]) ~(b:[%src_pos]) () : lexing_position * lexing_position
  = a, b
[%%expect{|
val h :
  a:[%src_pos] -> b:[%src_pos] -> unit -> lexing_position * lexing_position =
  <fun>
|}]

(* Partial application *)
let x = h ~b:{Lexing.dummy_pos with pos_fname = "b"};;
[%%expect{|
val x : a:[%src_pos] -> unit -> lexing_position * lexing_position = <fun>
|}]

let y = x ();;
[%%expect{|
val y : lexing_position * lexing_position =
  ({pos_fname = ""; pos_lnum = 1; pos_bol = 1119; pos_cnum = 1127},
   {pos_fname = "b"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1})
|}]

let k = (f : unit -> lexing_position);;
[%%expect{|
val k : unit -> lexing_position = <fun>
|}]

let _ = j ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 267; pos_cnum = 276}
|}]

let _ = k ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 1327; pos_cnum = 1336}
|}]

let m ~(src_pos:[%src_pos]) = ()
[%%expect {|
Line 1, characters 8-15:
1 | let m ~(src_pos:[%src_pos]) = ()
            ^^^^^^^
Warning 188 [unerasable-position-argument]: this position argument cannot be erased.

val m : src_pos:[%src_pos] -> unit = <fun>
|}]

