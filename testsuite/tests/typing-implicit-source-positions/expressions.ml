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
  ('a : value_or_null).
    [< `F | `G | `H ] ->
    (here:lexing_position -> 'a) ->
    (here:[%call_pos] -> unit -> 'a) ->
    (h_arg:[%call_pos] -> unit -> 'a) -> here:lexing_position -> 'a =
  <fun>
|}]

let _ = wrap `G (fun ~here:_ -> assert false)
                (fun ~(here:[%call_pos]) () -> here)
                (fun ~h_arg:(_ : [%call_pos]) () -> assert false)
                ~here:[%src_pos]
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 4; pos_bol = 1198; pos_cnum = 1220}
|}]

(* call_pos type annotations not permitted anywhere other than labelled arg
   position *)
let error1 x = (x : [%call_pos])

[%%expect{|
Line 1, characters 22-30:
1 | let error1 x = (x : [%call_pos])
                          ^^^^^^^^
Error: [%call_pos] can only exist as the type of a labelled argument
|}]

let error2 f x = f (x : [%call_pos])

[%%expect{|
Line 1, characters 24-35:
1 | let error2 f x = f (x : [%call_pos])
                            ^^^^^^^^^^^
Error: A position argument must not be unlabelled.
|}]

let error3 f x = f ?x:(x : [%call_pos])

[%%expect{|
Line 1, characters 27-38:
1 | let error3 f x = f ?x:(x : [%call_pos])
                               ^^^^^^^^^^^
Error: A position argument must not be optional.
|}]

(* TEST
 expect;
*)
