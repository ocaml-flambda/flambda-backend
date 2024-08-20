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
Error: the argument labeled call_pos is a [%call_pos] argument, filled in
       automatically if omitted. It cannot be passed with '?'.
|}]

let _ =
  let pos = f () in
  f ?call_pos:(Some pos) ();
[%%expect {|
Line 3, characters 14-24:
3 |   f ?call_pos:(Some pos) ();
                  ^^^^^^^^^^
Error: the argument labeled call_pos is a [%call_pos] argument, filled in
       automatically if omitted. It cannot be passed with '?'.
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
Error: the argument labeled call_pos is a [%call_pos] argument, filled in
       automatically if omitted. It cannot be passed with '?'.
|}]

let _ =
  let pos = f () in
  ( >>| ) ?call_pos:(Some pos) 1 2
;;
[%%expect {|
Line 3, characters 20-30:
3 |   ( >>| ) ?call_pos:(Some pos) 1 2
                        ^^^^^^^^^^
Error: the argument labeled call_pos is a [%call_pos] argument, filled in
       automatically if omitted. It cannot be passed with '?'.
|}]

class c ~(call_pos : [%call_pos]) () = object 
  method call_pos = call_pos
end
[%%expect {|
class c :
  call_pos:[%call_pos] ->
  unit -> object method call_pos : lexing_position end
|}]

let _ = (new c ?call_pos:None ())#call_pos;;
[%%expect {|
Line 1, characters 25-29:
1 | let _ = (new c ?call_pos:None ())#call_pos;;
                             ^^^^
Error: the argument labeled call_pos is a [%call_pos] argument, filled in
       automatically if omitted. It cannot be passed with '?'.
|}]

let _ = 
  let pos = f () in
  (new c ?call_pos:(Some pos) ())#call_pos;;
[%%expect {|
Line 3, characters 19-29:
3 |   (new c ?call_pos:(Some pos) ())#call_pos;;
                       ^^^^^^^^^^
Error: the argument labeled call_pos is a [%call_pos] argument, filled in
       automatically if omitted. It cannot be passed with '?'.
|}]

class parent ~(call_pos : [%call_pos]) () = object
  method pos = call_pos
end
[%%expect {|
class parent :
  call_pos:[%call_pos] -> unit -> object method pos : lexing_position end
|}]

let _ = (object 
  inherit parent ?call_pos:None ()
end)#pos;;
[%%expect {|
Line 2, characters 27-31:
2 |   inherit parent ?call_pos:None ()
                               ^^^^
Error: the argument labeled 'call_pos' is a [%call_pos] argument, filled in
       automatically if ommitted. It cannot be passed with '?'.
|}]

let o = (object 
  inherit parent ?call_pos:(Some (f ())) ()
end)#pos
[%%expect {|
Line 2, characters 27-40:
2 |   inherit parent ?call_pos:(Some (f ())) ()
                               ^^^^^^^^^^^^^
Error: the argument labeled 'call_pos' is a [%call_pos] argument, filled in
       automatically if ommitted. It cannot be passed with '?'.
|}]


(* TEST
 expect;
*)
