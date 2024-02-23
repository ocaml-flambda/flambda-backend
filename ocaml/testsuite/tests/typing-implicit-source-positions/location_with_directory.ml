(* TEST
   * expect

   flags = "-dir app/foo"
*)

let f = fun ~(src_pos:[%src_pos]) () -> src_pos
[%%expect{|
val f : src_pos:[%src_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ();;
[%%expect{|
- : lexing_position =
{pos_fname = "app/foo/"; pos_lnum = 1; pos_bol = 178; pos_cnum = 186}
|}]

