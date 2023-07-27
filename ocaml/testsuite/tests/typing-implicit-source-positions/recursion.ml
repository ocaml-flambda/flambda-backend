(* TEST
   * expect
*)

let rec f ~(src_pos:[%src_pos]) i = 
  if i < 0 then 0 else f ~src_pos (i - 1)
[%%expect {|
val f : src_pos:[%src_pos] -> int -> int = <fun>
|}]
