(* TEST
   * expect
*)

type f = (src_pos:[%src_pos] -> unit) -> unit

[%%expect {|
type f = (src_pos:lexing_position -> unit) -> unit
|}]
