(* TEST_BELOW
Fille
*)

let x = [%src_pos]
[%%expect{|
val x : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 24; pos_cnum = 32}
|}]

let f = fun ~(call_pos:[%call_pos]) () -> call_pos
[%%expect{|
val f : call_pos:[%call_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ~call_pos:x () ;;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 24; pos_cnum = 32}
|}]

let _ = "Increment line count"
let _ = f ~call_pos:[%src_pos] () ;;
[%%expect{|
- : string = "Increment line count"
- : lexing_position =
{pos_fname = ""; pos_lnum = 2; pos_bol = 438; pos_cnum = 458}
|}]

(* TEST
 expect;
*)
