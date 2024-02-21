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

class c ~(src_pos : [%src_pos]) () = object(self)
  method from_class_param = src_pos

  method m ~(src_pos : [%src_pos]) () = src_pos, self#from_class_param
end
[%%expect{|
class c :
  src_pos:[%src_pos] ->
  unit ->
  object
    method from_class_param : lexing_position
    method m :
      src_pos:[%src_pos] -> unit -> lexing_position * lexing_position
  end
|}]

let c = (new c ())
let from_method_param, from_class_param = c#m()

[%%expect{|
val c : c = <obj>
val from_method_param : lexing_position =
  {pos_fname = ""; pos_lnum = 2; pos_bol = 2186; pos_cnum = 2228}
val from_class_param : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 2167; pos_cnum = 2175}
|}]

(* XXX jrodri: This should probably work... due to the segment below working... *)
class parent ~(src_pos : [%src_pos]) () = object
  method pos = src_pos
end

let o = object 
  inherit parent ()
end
let position = o#pos

[%%expect{|
class parent :
  src_pos:[%src_pos] -> unit -> object method pos : lexing_position end
val o : parent = <obj>
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 6; pos_bol = 2661; pos_cnum = 2671}
|}]

let o ~(src_pos : [%src_pos]) () = object 
  inherit parent ~src_pos ()
end
let position = (o ())#pos
[%%expect{|
val o : src_pos:[%src_pos] -> unit -> parent = <fun>
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 4; pos_bol = 3063; pos_cnum = 3078}
|}, Principal{|
val o : src_pos:[%src_pos] -> unit -> parent = <fun>
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 4; pos_bol = 3288; pos_cnum = 3303}
|}]

(* Applying an src_pos argument without a label. *)
let o ~(src_pos : [%src_pos]) () = object 
  inherit parent src_pos ()
end
let position = (o ())#pos
[%%expect{|
Line 2, characters 10-16:
2 |   inherit parent src_pos ()
              ^^^^^^
Warning 6 [labels-omitted]: label src_pos was omitted in the application of this function.

val o : src_pos:[%src_pos] -> unit -> parent = <fun>
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 4; pos_bol = 3385; pos_cnum = 3400}
|}, Principal{|
Line 2, characters 10-16:
2 |   inherit parent src_pos ()
              ^^^^^^
Warning 6 [labels-omitted]: label src_pos was omitted in the application of this function.

val o : src_pos:[%src_pos] -> unit -> parent = <fun>
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 4; pos_bol = 3610; pos_cnum = 3625}
|}]


class parent ?(i = 1) () = object
  method i = i
end

let o = object 
  inherit parent ()
end
let position = o#i

[%%expect{|
class parent : ?i:int -> unit -> object method i : int end
val o : parent = <obj>
val position : int = 1
|}]

