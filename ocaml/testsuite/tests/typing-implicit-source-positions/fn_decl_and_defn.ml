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


let apply f = f ~src_pos:{pos_fname="hello" ; pos_lnum=1; pos_bol=2; pos_cnum=3} () ;;
[%%expect {|
val apply : (src_pos:lexing_position -> unit -> 'a) -> 'a = <fun>
|}]

(* TODO: These fail for the same reason as above *)

(* let _ = apply f ;;
[%%expect{|
- : unit = ()
|}]

let _ = apply g ;;
[%%expect{|
- : unit = ()
|}] *)

(*  end  *)

(* Using lexing_position manually *)
let g ~(src_pos:lexing_position) () = () ;; 
[%%expect {|
val g : src_pos:lexing_position -> unit -> unit = <fun>
|}]

let _ = apply g ;;
[%%expect{|
- : unit = ()
|}]
