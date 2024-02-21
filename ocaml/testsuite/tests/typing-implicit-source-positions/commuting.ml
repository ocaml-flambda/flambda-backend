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

(* Object system *)

class c ~(a : [%src_pos]) ~(b : [%src_pos]) () =
  object 
    method x = a, b
  end
[%%expect{|
class c :
  a:[%src_pos] ->
  b:[%src_pos] ->
  unit -> object method x : lexing_position * lexing_position end
|}]

(* Object system partial application *)
let x = new c ~b:pos_b ;;
let y = x ~a:pos_a ;;
let a, b = (y ())#x ;;
[%%expect{|
val x : a:[%src_pos] -> unit -> c = <fun>
val y : unit -> c = <fun>
val a : lexing_position =
  {pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
val b : lexing_position =
  {pos_fname = "b"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

(* Labels on source positions can't commute in class definitions *)
class m : a:[%src_pos] -> b:[%src_pos] -> unit -> object end =
  fun ~(b:[%src_pos]) ~(a:[%src_pos]) () -> object end
[%%expect{|
Line 2, characters 6-54:
2 |   fun ~(b:[%src_pos]) ~(a:[%src_pos]) () -> object end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type b:[%src_pos] -> a:[%src_pos] -> unit -> object  end
       is not matched by the class type
         a:[%src_pos] -> b:[%src_pos] -> unit -> object  end
|}]

(* [%src_pos] is distinct from lexing_position *)
class c :
  a:lexing_position -> b:[%src_pos] -> unit -> object
    method x : lexing_position * lexing_position
  end = fun ~(a : [%src_pos]) ~b () -> object
    method x = a, b
  end
[%%expect{|
Lines 4-6, characters 12-5:
4 | ............~(a : [%src_pos]) ~b () -> object
5 |     method x = a, b
6 |   end
Error: The class type
         a:[%src_pos] -> b:'b -> unit -> object method x : 'a * 'b end
       is not matched by the class type
         a:lexing_position ->
         b:[%src_pos] ->
         unit -> object method x : lexing_position * lexing_position end
|}]

