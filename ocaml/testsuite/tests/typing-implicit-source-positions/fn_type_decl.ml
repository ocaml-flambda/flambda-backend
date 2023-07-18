(* TEST
   * expect
*)

type t = src_pos:[%src_pos] -> unit -> unit

[%%expect {|
type t = src_pos:lexing_position -> unit -> unit
|}]

(*  TODO: Desired behavior below  *)

(* This fails because the type expression thinks t has a Labeled argument,
   while the definition type includes that it's a Position label *)

(* let f : t = fun ~(src_pos:[%src_pos]) () -> ()

[%%expect{|
val f : src_pos:lexing_position -> unit -> unit = <fun>
|}] *)

(*  The best we can do for now:  *)

let f = fun ~(src_pos:[%src_pos]) () -> ()

[%%expect{|
val f : src_pos:lexing_position -> unit -> unit = <fun>
|}]

(*  end  *)

let g ~(src_pos:[%src_pos]) () = ()

[%%expect{|
val g : src_pos:lexing_position -> unit -> unit = <fun>
|}]
