(* TEST
 expect;
*)

let f ~(here : [%call_pos]) x = here, x

[%%expect
  {|
val f : ('a : value_or_null). here:[%call_pos] -> 'a -> lexing_position * 'a =
  <fun>
|}]

let result = () |> f |> f

(* Importantly, these locations are different. *)
[%%expect
  {|
val result : lexing_position * (lexing_position * unit) =
  ({pos_fname = ""; pos_lnum = 1; pos_bol = 169; pos_cnum = 193},
   ({pos_fname = ""; pos_lnum = 1; pos_bol = 169; pos_cnum = 188}, ()))
|}]

class ['a] c : here:[%call_pos] -> 'a -> object
    method here : lexing_position * 'a
  end = fun ~(here : [%call_pos]) a -> object
    method here = here, a
  end

[%%expect{|
class ['a] c :
  here:[%call_pos] -> 'a -> object method here : lexing_position * 'a end
|}]

let obj = (() |> new c |> new c)

let second_here = fst obj#here
let first_here = fst (snd obj#here)#here


[%%expect{|
val obj : unit c c = <obj>
val second_here : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 734; pos_cnum = 760}
val first_here : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 734; pos_cnum = 751}
|}]
