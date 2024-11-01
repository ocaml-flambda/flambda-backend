(* TEST
 expect;
*)

type t = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[%%expect{|
type t = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
|}]

(* type-based disambiguation *)
let rec f ~(call_pos:[%call_pos]) i =
  if i < 0 then 0
  else f ~call_pos:{ pos_fname = ""
                  ; pos_lnum = 0
                  ; pos_bol = 0
                  ; pos_cnum = -1 }
          (i - 1)
[%%expect {|
val f : call_pos:[%call_pos] -> int -> int = <fun>
|}]

let y = { pos_fname = ""
        ; pos_lnum = 0
        ; pos_bol = 0
        ; pos_cnum = -1 }
[%%expect {|
val y : t = {pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]

let rec g ~(call_pos:[%call_pos]) i =
  if i < 0 then 0
  else g ~call_pos:y (i - 1)
[%%expect {|
Line 3, characters 19-20:
3 |   else g ~call_pos:y (i - 1)
                       ^
Error: This expression has type "t" but an expression was expected of type
         "lexing_position"
|}]
