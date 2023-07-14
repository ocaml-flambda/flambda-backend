(* TEST
   * expect
*)

let apply f = f ~src_pos:{pos_fname="hello" ; pos_lnum=1; pos_bol=2; pos_cnum=3} ;;
[%%expect {|
val apply : (src_pos:lexing_position -> 'a) -> 'a = <fun>
|}]

let f ~(src_pos:[%src_pos]) = ();;
[%%expect{|
val f : src_pos:lexing_position -> unit = <fun>
|}]

let _ = apply f ;;
[%%expect{|
- : unit = ()
|}]

let g ~(src_pos:lexing_position) = () ;; 
[%%expect {|
val g : src_pos:lexing_position -> unit = <fun>
|}]

let _ = apply g ;;
[%%expect{|
- : unit = ()
|}]

(* Shadowing *)

type lexing_position = int 
[%%expect{|
type lexing_position = int
|}]

(* src_pos works *)
let _ = apply f ;;
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
