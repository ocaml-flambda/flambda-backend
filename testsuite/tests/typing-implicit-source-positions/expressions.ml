(* TEST_BELOW
Fille
*)

let x = [%src_pos]
[%%expect{|
val x : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 24; pos_cnum = 32}
|}]

let f = fun ~(call_pos:[%call_pos]) () -> call_pos
[%%expect{|
val f : call_pos:[%call_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ~call_pos:x () ;;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 24; pos_cnum = 32}
|}]

let _ = "Increment line count"
let _ = f ~call_pos:[%src_pos] () ;;
[%%expect{|
- : string = "Increment line count"
- : lexing_position =
{pos_fname = ""; pos_lnum = 2; pos_bol = 438; pos_cnum = 458}
|}]

(* passing an argment explicitly as call_pos *)
let wrap x f g h ~here =
  match x with
  | `F -> f ~here
  | `G -> g ~(here : [%call_pos]) ()
  | `H -> h ~h_arg:(here : [%call_pos]) ()
;;

[%%expect{|
val wrap :
  [< `F | `G | `H ] ->
  (here:lexing_position -> 'a) ->
  (here:[%call_pos] -> unit -> 'a) ->
  (h_arg:[%call_pos] -> unit -> 'a) -> here:lexing_position -> 'a = <fun>
|}]

let _ = wrap `G (fun ~here:_ -> assert false)
                (fun ~(here:[%call_pos]) () -> here)
                (fun ~h_arg:(_ : [%call_pos]) () -> assert false)
                ~here:[%src_pos]
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 4; pos_bol = 1164; pos_cnum = 1186}
|}]

(* TEST
 expect;
*)
