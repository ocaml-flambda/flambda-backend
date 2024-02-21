(* TEST
   * expect
*)

let object_with_a_method_with_a_positional_parameter = object 
  method m ~(src_pos : [%src_pos]) () = src_pos
end

[%%expect{|
val object_with_a_method_with_a_positional_parameter :
  < m : src_pos:[%src_pos] -> unit -> lexing_position > = <obj>
|}]

let position = object_with_a_method_with_a_positional_parameter#m ();;

[%%expect{|
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 276; pos_cnum = 291}
|}]

class class_with_a_method_with_a_positional_parameter = object 
  method m ~(src_pos : [%src_pos]) () = src_pos
end

[%%expect{|
class class_with_a_method_with_a_positional_parameter :
  object method m : src_pos:[%src_pos] -> unit -> lexing_position end
|}]

let o = new class_with_a_method_with_a_positional_parameter;;

[%%expect{|
val o : class_with_a_method_with_a_positional_parameter = <obj>
|}]

let position = o#m ();;

[%%expect{|
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 866; pos_cnum = 881}
|}]

let position = (new class_with_a_method_with_a_positional_parameter)#m ();;

[%%expect{|
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 1005; pos_cnum = 1020}
|}]


class class_with_positional_parameter ~(src_pos : [%src_pos]) () = object 
  method src_pos = src_pos
end

[%%expect{|
class class_with_positional_parameter :
  src_pos:[%src_pos] -> unit -> object method src_pos : lexing_position end
|}]

let o = new class_with_positional_parameter ()
let position = o#src_pos

[%%expect{|
val o : class_with_positional_parameter = <obj>
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 1439; pos_cnum = 1447}
|}]


(* Different kinds of shadowed parameters (both a class parameter is shadowed and a
   method parameter is shadowed) *)

class c ~(src_pos : [%src_pos]) () = object 
  method m ~(src_pos : [%src_pos]) () = src_pos
end
[%%expect{|
class c :
  src_pos:[%src_pos] ->
  unit -> object method m : src_pos:[%src_pos] -> unit -> lexing_position end
|}]

let _ = (new c ())#m()

[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 2024; pos_cnum = 2032}
|}]
