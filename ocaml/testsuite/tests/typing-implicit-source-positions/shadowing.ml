(* TEST
   * expect
*)

(* Shadowing *)

type lexing_position = int 
[%%expect{|
type lexing_position = int
|}]

let f ~(src_pos:[%src_pos]) = ();;
[%%expect{|
val f : src_pos:lexing_position -> unit = <fun>
|}]

(* src_pos works *)
let _ = f ~src_pos:{pos_fname="hello" ; pos_lnum=1; pos_bol=2; pos_cnum=3} ;;
[%%expect{|
- : unit = ()
|}]

(* new type works *)
let h (x:lexing_position) = x ;;
[%%expect{|
val h : lexing_position -> lexing_position = <fun>
|}]

let _ = h 5;;
[%%expect {|
- : lexing_position = 5
|}]
