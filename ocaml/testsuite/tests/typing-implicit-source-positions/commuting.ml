(* TEST
   * expect
*)

let pos_a : lexing_position = {Lexing.dummy_pos with pos_fname = "a"};;
let pos_b : lexing_position = {Lexing.dummy_pos with pos_fname = "b"};;
[%%expect{|
val pos_a : lexing_position =
  {pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
val pos_b : lexing_position =
  {pos_fname = "b"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

let f = fun ~(a:[%src_pos]) ~(b:[%src_pos]) () -> a, b
[%%expect{|
val f :
  a:[%src_pos] -> b:[%src_pos] -> unit -> lexing_position * lexing_position =
  <fun>
|}]

let _ = f ~b:pos_b ~a:pos_a () ;;
[%%expect{|
- : lexing_position * lexing_position =
({pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1},
 {pos_fname = "b"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1})
|}]

(* Partial application *)
let x = f ~b:pos_b ;;
let y = x ~a:pos_a ;;
let z = y () ;;
[%%expect {|
val x : a:[%src_pos] -> unit -> lexing_position * lexing_position = <fun>
val y : unit -> lexing_position * lexing_position = <fun>
val z : lexing_position * lexing_position =
  ({pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1},
   {pos_fname = "b"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1})
|}]

let g = fun ~(a:[%src_pos]) ?(c = 0) ~(b:[%src_pos]) () -> a, b, c
[%%expect{|
val g :
  a:[%src_pos] ->
  ?c:int -> b:[%src_pos] -> unit -> lexing_position * lexing_position * int =
  <fun>
|}]

let _ = g ~b:pos_b ~a:pos_a () ;;
[%%expect{|
- : lexing_position * lexing_position * int =
({pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1},
 {pos_fname = "b"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}, 0)
|}]

let h = fun ~(a:[%src_pos]) ~(b:int) () -> a, b
[%%expect{|
val h : a:[%src_pos] -> b:int -> unit -> lexing_position * int = <fun>
|}]

let _ = h ~b:0 ~a:pos_a ();;
[%%expect{|
- : lexing_position * int =
({pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}, 0)
|}]

let k = fun ~(a:int) ~(a:[%src_pos])() -> a
[%%expect{|
val k : a:int -> a:[%src_pos] -> unit -> lexing_position = <fun>
|}]

let _ = k ~a:Lexing.dummy_pos ~a:0 ();;
[%%expect{|
Line 1, characters 13-29:
1 | let _ = k ~a:Lexing.dummy_pos ~a:0 ();;
                 ^^^^^^^^^^^^^^^^
Error: This expression has type Lexing.position = lexing_position
       but an expression was expected of type int
|}]

let _ = k ~a:0 ~a:Lexing.dummy_pos ();;
[%%expect{|
- : Lexing.position =
{Lexing.pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

(* Labels on source positions can't commute in definitions *)
let m : a:[%src_pos] -> b:[%src_pos] -> unit -> unit = fun ~(b:[%src_pos]) ~(a:[%src_pos]) () -> ()
[%%expect{|
Line 1, characters 55-99:
1 | let m : a:[%src_pos] -> b:[%src_pos] -> unit -> unit = fun ~(b:[%src_pos]) ~(a:[%src_pos]) () -> ()
                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function should have type
         a:[%src_pos] -> b:[%src_pos] -> unit -> unit
       but its first argument is ~(b:[%src_pos]) instead of ~(a:[%src_pos])
|}]
