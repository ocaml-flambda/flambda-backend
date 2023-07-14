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

(* Using lexing_position manually *)
let g ~(src_pos:lexing_position) = () ;; 
[%%expect {|
val g : src_pos:lexing_position -> unit = <fun>
|}]

let _ = apply g ;;
[%%expect{|
- : unit = ()
|}]

(* src_pos (lexing_position) and Lexing.position are synonyms *)
let s ~(src_pos:[%src_pos]) : Lexing.position = src_pos ;; 
[%%expect{|
val s : src_pos:lexing_position -> Lexing.position = <fun>
|}]

let t (src_pos:Lexing.position) : [%src_pos] = src_pos ;; 
[%%expect{|
val t : Lexing.position -> lexing_position = <fun>
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
