(* TEST
   * expect
*)

(* src_pos (lexing_position) and Lexing.position are synonyms *)
let predef_to_module ~(src_pos:[%src_pos]) : Lexing.position = src_pos ;; 
[%%expect{|
val predef_to_module : src_pos:lexing_position -> Lexing.position = <fun>
|}]

let module_to_predef (src_pos:Lexing.position) : [%src_pos] = src_pos ;; 
[%%expect{|
val module_to_predef : Lexing.position -> lexing_position = <fun>
|}]

let x = predef_to_module ~src_pos:{pos_fname="hello" ; pos_lnum=1; pos_bol=2; pos_cnum=3};;
[%%expect{|
val x : Lexing.position =
  {Lexing.pos_fname = "hello"; pos_lnum = 1; pos_bol = 2; pos_cnum = 3}
|}]

let y = module_to_predef {pos_fname="hello" ; pos_lnum=1; pos_bol=2; pos_cnum=3};;
[%%expect{|
val y : lexing_position =
  {pos_fname = "hello"; pos_lnum = 1; pos_bol = 2; pos_cnum = 3}
|}]

(* Fields accessible from within Lexing module *)
let _ = x.Lexing.pos_cnum = y.Lexing.pos_cnum && x.pos_cnum = y.pos_cnum && x.Lexing.pos_cnum = x.pos_cnum
[%%expect{|
- : bool = true
|}]
