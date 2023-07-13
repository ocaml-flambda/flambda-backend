(* TEST
   * expect
*)

let f ~(src_pos:[%src_pos]) () = ();;

let _ = f ();;
[%%expect{||}]
