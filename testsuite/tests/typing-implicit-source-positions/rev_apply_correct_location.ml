(* TEST
 expect;
*)

let f ~(here : [%call_pos]) x = here, x

[%%expect
  {|
val f : here:[%call_pos] -> 'a -> lexing_position * 'a @@ global many = <fun>
|}]

let result = () |> f |> f

(* Importantly, these locations are different. *)
[%%expect
  {|
val result : lexing_position * (lexing_position * unit) @@ global many =
  ({pos_fname = ""; pos_lnum = 1; pos_bol = 160; pos_cnum = 184},
   ({pos_fname = ""; pos_lnum = 1; pos_bol = 160; pos_cnum = 179}, ()))
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
val obj : unit c c @@ global many = <obj>
val second_here : lexing_position @@ global many =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 740; pos_cnum = 766}
val first_here : lexing_position @@ global many =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 740; pos_cnum = 757}
|}]
