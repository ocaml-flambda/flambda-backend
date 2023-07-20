(* TEST
   * expect
*)

type t = [%src_pos]
[%%expect {|
Line 1, characters 11-18:
1 | type t = [%src_pos]
               ^^^^^^^
Error: Uninterpreted extension 'src_pos'.
|}]

type t = unit -> unit -> [%src_pos]
[%%expect {|
Line 1, characters 27-34:
1 | type t = unit -> unit -> [%src_pos]
                               ^^^^^^^
Error: Uninterpreted extension 'src_pos'.
|}]

let f ~(src_pos:[%src_pos]) () : [%src_pos] = src_pos

[%%expect{|
Line 1, characters 35-42:
1 | let f ~(src_pos:[%src_pos]) () : [%src_pos] = src_pos
                                       ^^^^^^^
Error: Uninterpreted extension 'src_pos'.
|}]
