(* TEST
   * expect
*)

let x = [%src_pos]
[%%expect{|
val x : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 24; pos_cnum = 32}
|}]

let f = fun ~(src_pos:[%src_pos]) () -> src_pos
[%%expect{|
val f : src_pos:[%src_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ~src_pos:x () ;;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 24; pos_cnum = 32}
|}]

let _ = "Increment line count"
let _ = f ~src_pos:[%src_pos] () ;;
[%%expect{|
- : string = "Increment line count"
- : lexing_position =
{pos_fname = ""; pos_lnum = 2; pos_bol = 432; pos_cnum = 451}
|}]
