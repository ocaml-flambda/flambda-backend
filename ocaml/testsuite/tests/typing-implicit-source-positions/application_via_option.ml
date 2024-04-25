(* TEST_BELOW
Fille
*)

let f = fun ~(call_pos:[%call_pos]) () -> call_pos
[%%expect {|
val f : call_pos:[%call_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ?call_pos:None ();
[%%expect {|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 157; pos_cnum = 165}
|}]

let _ =
  let pos = f () in
  f ?call_pos:(Some pos) ();
[%%expect {|
- : lexing_position =
{pos_fname = ""; pos_lnum = 2; pos_bol = 296; pos_cnum = 308}
|}]

let ( >>| ) ~(call_pos : [%call_pos]) a b = a + b, call_pos ;;
[%%expect {|
val ( >>| ) : call_pos:[%call_pos] -> int -> int -> int * lexing_position =
  <fun>
|}]

let _ =  ( >>| ) ?call_pos:None 1 2 ;;
[%%expect {|
- : int * lexing_position =
(3, {pos_fname = ""; pos_lnum = 1; pos_bol = 612; pos_cnum = 621})
|}]

let _ =
  let pos = f () in
  ( >>| ) ?call_pos:(Some pos) 1 2
;;
[%%expect {|
- : int * lexing_position =
(3, {pos_fname = ""; pos_lnum = 2; pos_bol = 772; pos_cnum = 784})
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
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 1132; pos_cnum = 1140}
|}]

let _ = 
  let pos = f () in
  (new c ?call_pos:(Some pos) ())#call_pos;;
[%%expect {|
- : lexing_position =
{pos_fname = ""; pos_lnum = 2; pos_bol = 1290; pos_cnum = 1302}
|}]

(* TEST
 expect;
*)
