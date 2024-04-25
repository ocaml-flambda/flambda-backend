(* TEST_BELOW
Fille
*)

let f = fun ~(call_pos:[%call_pos]) () -> call_pos
[%%expect {|
val f : call_pos:[%call_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ?call_pos:None ();
[%%expect {|
Line 1, characters 20-24:
1 | let _ = f ?call_pos:None ();
                        ^^^^
Warning 43 [nonoptional-label]: the label call_pos is not optional.

Line 1, characters 20-24:
1 | let _ = f ?call_pos:None ();
                        ^^^^
Error: This expression should not be a constructor, the expected type is
       lexing_position
|}]

let _ =
  let pos = f () in
  f ?call_pos:(Some pos) ();
[%%expect {|
Line 3, characters 14-24:
3 |   f ?call_pos:(Some pos) ();
                  ^^^^^^^^^^
Warning 43 [nonoptional-label]: the label call_pos is not optional.

Line 3, characters 14-24:
3 |   f ?call_pos:(Some pos) ();
                  ^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is
       lexing_position
|}]

let ( >>| ) ~(call_pos : [%call_pos]) a b = a + b, call_pos ;;
[%%expect {|
val ( >>| ) : call_pos:[%call_pos] -> int -> int -> int * lexing_position =
  <fun>
|}]

let _ =  ( >>| ) ?call_pos:None 1 2 ;;
[%%expect {|
Line 1, characters 27-31:
1 | let _ =  ( >>| ) ?call_pos:None 1 2 ;;
                               ^^^^
Warning 43 [nonoptional-label]: the label call_pos is not optional.

Line 1, characters 27-31:
1 | let _ =  ( >>| ) ?call_pos:None 1 2 ;;
                               ^^^^
Error: This expression should not be a constructor, the expected type is
       lexing_position
|}]

let _ =
  let pos = f () in
  ( >>| ) ?call_pos:(Some pos) 1 2
;;
[%%expect {|
Line 3, characters 20-30:
3 |   ( >>| ) ?call_pos:(Some pos) 1 2
                        ^^^^^^^^^^
Warning 43 [nonoptional-label]: the label call_pos is not optional.

Line 3, characters 20-30:
3 |   ( >>| ) ?call_pos:(Some pos) 1 2
                        ^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is
       lexing_position
|}]

class c ~(call_pos : [%call_pos]) () = object 
  method call_pos = call_pos
end
[%%expect {|
class c :
  call_pos:[%call_pos] ->
  unit -> object method call_pos : lexing_position end
|}]

let _ = new c ?call_pos:None ();;
[%%expect {|
Line 1, characters 24-28:
1 | let _ = new c ?call_pos:None ();;
                            ^^^^
Warning 43 [nonoptional-label]: the label call_pos is not optional.

Line 1, characters 24-28:
1 | let _ = new c ?call_pos:None ();;
                            ^^^^
Error: This expression should not be a constructor, the expected type is
       lexing_position
|}]

let _ = 
  let pos = f () in
  new c ?call_pos:(Some pos) ();;
[%%expect {|
Line 3, characters 18-28:
3 |   new c ?call_pos:(Some pos) ();;
                      ^^^^^^^^^^
Warning 43 [nonoptional-label]: the label call_pos is not optional.

Line 3, characters 18-28:
3 |   new c ?call_pos:(Some pos) ();;
                      ^^^^^^^^^^
Error: This expression should not be a constructor, the expected type is
       lexing_position
|}]

(* TEST
 expect;
*)
