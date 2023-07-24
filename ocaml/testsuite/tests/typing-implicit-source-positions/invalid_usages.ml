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

let apply f = f ~src_pos:Lexing.dummy_pos () ;;
[%%expect {|
val apply : (src_pos:Lexing.position -> unit -> 'a) -> 'a = <fun>
|}]

let g = fun ~(src_pos:[%src_pos]) () -> ()

[%%expect{|
val g : src_pos:[%src_pos] -> unit -> unit = <fun>
|}]

let _ = apply g ;;
[%%expect{|
Line 1, characters 14-15:
1 | let _ = apply g ;;
                  ^
Error: This expression has type src_pos:[%src_pos] -> unit -> unit
       but an expression was expected of type
         src_pos:Lexing.position -> unit -> 'a
|}]
