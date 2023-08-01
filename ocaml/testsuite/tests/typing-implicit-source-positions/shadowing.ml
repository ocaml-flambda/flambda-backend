(* TEST
   * expect
*)

(* Shadowing *)

type lexing_position = int
[%%expect{|
type lexing_position = int
|}]

(* src_pos works *)
let f ~(src_pos:[%src_pos]) () = ();;
[%%expect{|
val f : src_pos:[%src_pos] -> unit -> unit = <fun>
|}]

let _ = f ~src_pos:Lexing.dummy_pos () ;;
[%%expect{|
- : unit = ()
|}]

(* new type works *)
let h (x : lexing_position) = x ;;
[%%expect{|
val h : lexing_position -> lexing_position = <fun>
|}]

let _ = h 5;;
[%%expect {|
- : lexing_position = 5
|}]
