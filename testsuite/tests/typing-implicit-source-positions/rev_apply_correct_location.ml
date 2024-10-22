(* TEST
 expect;
*)

let f ~(here : [%call_pos]) x = here, x

[%%expect
  {|
val f : here:[%call_pos] -> 'a -> lexing_position * 'a = <fun>
|}]

let result = () |> f |> f

(* Importantly, these locations are different. *)
[%%expect
  {|
val result : lexing_position * (lexing_position * unit) =
  ({pos_fname = ""; pos_lnum = 1; pos_bol = 145; pos_cnum = 169},
   ({pos_fname = ""; pos_lnum = 1; pos_bol = 145; pos_cnum = 164}, ()))
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
  {pos_fname = ""; pos_lnum = 1; pos_bol = 710; pos_cnum = 736}
val first_here : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 710; pos_cnum = 727}
|}]
