(* TEST
   * expect
*)

type t = src_pos:[%src_pos] -> unit -> unit

[%%expect {|
type t = src_pos:[%src_pos] -> unit -> unit
|}]

let f : t = fun ~(src_pos:[%src_pos]) () -> ()

[%%expect{|
val f : t = <fun>
|}]

let g ~(src_pos:[%src_pos]) () = ()

[%%expect{|
val g : src_pos:[%src_pos] -> unit -> unit = <fun>
|}]

let apply (f : t) = f ~src_pos:Lexing.dummy_pos () ;;
[%%expect {|
val apply : t -> unit = <fun>
|}]

let _ = apply f ;;
[%%expect{|
- : unit = ()
|}]

let _ = apply g ;;
[%%expect{|
- : unit = ()
|}]

let _ = g ~src_pos:Lexing.dummy_pos () ;;
[%%expect{|
- : unit = ()
|}]
