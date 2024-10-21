(* TEST
 expect;
*)

(* lexing_position and Lexing.position are synonyms *)
let x = Lexing.dummy_pos;;
[%%expect {|
val x : Lexing.position =
  {Lexing.pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

let y : lexing_position = x;;
[%%expect {|
val y : lexing_position =
  {pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

let predef_to_module ~(call_pos:[%call_pos]) () : Lexing.position = call_pos ;;
[%%expect{|
val predef_to_module : call_pos:[%call_pos] -> unit -> Lexing.position =
  <fun>
|}]

let module_to_predef (call_pos:Lexing.position) : lexing_position = call_pos ;;
[%%expect{|
val module_to_predef : Lexing.position -> lexing_position = <fun>
|}]

let x = predef_to_module ~call_pos:Lexing.dummy_pos ();;
[%%expect{|
val x : Lexing.position =
  {Lexing.pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

let y = module_to_predef Lexing.dummy_pos;;
[%%expect{|
val y : lexing_position =
  {pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

(* Fields accessible from within Lexing module *)
let _ = x.Lexing.pos_cnum = y.Lexing.pos_cnum && x.pos_cnum = y.pos_cnum && x.Lexing.pos_cnum = x.pos_cnum
[%%expect{|
- : bool = true
|}]
