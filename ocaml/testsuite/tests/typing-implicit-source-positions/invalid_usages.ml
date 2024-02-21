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
(* CR src_pos: Improve this error message to notify that [%src_pos] may only
   be used in arguments *)

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
         src_pos:Lexing.position -> (unit -> 'a)
|}]

let h ?(src_pos:[%src_pos]) () = ()
[%%expect{|
Line 1, characters 16-26:
1 | let h ?(src_pos:[%src_pos]) () = ()
                    ^^^^^^^^^^
Error: A position argument must not be optional.
|}]

let j (src_pos:[%src_pos]) () = ()
[%%expect{|
Line 1, characters 15-25:
1 | let j (src_pos:[%src_pos]) () = ()
                   ^^^^^^^^^^
Error: A position argument must not be unlabelled.
|}]

let k : src_pos:[%src_pos] -> unit -> unit =
   fun ~src_pos () -> ()
(* CR src_pos: Improve this error message *)
[%%expect{|
Line 2, characters 3-24:
2 |    fun ~src_pos () -> ()
       ^^^^^^^^^^^^^^^^^^^^^
Error: This function should have type src_pos:[%src_pos] -> unit -> unit
       but its first argument is labeled ~src_pos
       instead of ~(src_pos:[%src_pos])
|}]

let n = fun ~(src_pos:[%src_pos]) () -> src_pos
[%%expect{|
val n : src_pos:[%src_pos] -> unit -> lexing_position = <fun>
|}]

let _ = n Lexing.dummy_pos ();;
[%%expect {|
Line 1, characters 27-29:
1 | let _ = n Lexing.dummy_pos ();;
                               ^^
Error: The function applied to this argument has type
         src_pos:[%src_pos] -> lexing_position
This argument cannot be applied without label
|}]

class this_class_has_an_unerasable_argument ~(pos : [%src_pos]) = object end

[%%expect{|
Line 1, characters 46-49:
1 | class this_class_has_an_unerasable_argument ~(pos : [%src_pos]) = object end
                                                  ^^^
Warning 188 [unerasable-position-argument]: this position argument cannot be erased.

class this_class_has_an_unerasable_argument : pos:[%src_pos] -> object  end
|}]
