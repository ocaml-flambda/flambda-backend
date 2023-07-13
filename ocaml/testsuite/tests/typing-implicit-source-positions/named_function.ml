(* TEST
   * expect
*)

let f ~(src_pos:[%src_pos]) () = ();;
[%%expect{|
val f : src_pos:position -> unit -> unit = <fun>
|}]

let _ = f ~src_pos:{pos_fname="hello" ; pos_lnum=1; pos_bol=2; pos_cnum=3} ();;
[%%expect{|
- : unit = ()
|}]
