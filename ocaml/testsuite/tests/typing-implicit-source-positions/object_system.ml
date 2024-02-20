<<<<<<< HEAD
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
  {pos_fname = ""; pos_lnum = 6; pos_bol = 2578; pos_cnum = 2588}
|}]

let o ~(src_pos : [%src_pos]) () = object 
  inherit parent ~src_pos ()
end
let position = (o ())#pos

[%%expect{|
val o : src_pos:[%src_pos] -> unit -> parent = <fun>
val position : lexing_position =
  {pos_fname = ""; pos_lnum = 4; pos_bol = 2926; pos_cnum = 2941}
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
  {pos_fname = ""; pos_lnum = 4; pos_bol = 3249; pos_cnum = 3264}
|}]


(* Same behavior as optional parameters. *)
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

(* Partially applying a class *)
class c ~(a : [%src_pos]) ~(b : [%src_pos]) () =
  object 
    method a = a
    method b = b
  end

[%%expect{|
class c :
  a:[%src_pos] ->
  b:[%src_pos] ->
  unit -> object method a : lexing_position method b : lexing_position end
|}]

let pos_a : lexing_position = {Lexing.dummy_pos with pos_fname = "a"};;
let partially_applied_class = new c ~a:pos_a

[%%expect{|
val pos_a : lexing_position =
  {pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
val partially_applied_class : b:[%src_pos] -> unit -> c = <fun>
|}]

let fully_applied_class = partially_applied_class ()

[%%expect{|
val fully_applied_class : c = <obj>
|}]

let a, b = fully_applied_class#a, fully_applied_class#b

[%%expect{|
val a : lexing_position =
  {pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
val b : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 4459; pos_cnum = 4485}
|}]

class c :
  x:[%src_pos] -> y:lexing_position -> unit -> object
    method xy : lexing_position * lexing_position
  end = fun ~(x : [%src_pos]) ~y () -> object
    method xy = x, y
  end

[%%expect{|
class c :
  x:[%src_pos] ->
  y:lexing_position ->
  unit -> object method xy : lexing_position * lexing_position end
|}]

let x, y = (new c ~y:pos_a ())#xy

[%%expect{|
val x : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 5143; pos_cnum = 5154}
val y : lexing_position =
  {pos_fname = "a"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1}
|}]
||||||| parent of c53b6323 (Moves changes from original class-type support branch into a rebased branch)
=======
(* TEST
   * expect
*)

let o = object 
  method m ~(src_pos : [%src_pos]) () = src_pos
end

[%%expect{|
val o : < m : src_pos:[%src_pos] -> unit -> lexing_position > = <obj>
|}]

let x = o#m ();;

[%%expect{|
val x : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 180; pos_cnum = 188}
|}]

class c = object 
  method m ~(src_pos : [%src_pos]) () = src_pos
end

let o2 = new c;;

[%%expect{|
class c : object method m : src_pos:[%src_pos] -> unit -> lexing_position end
val o2 : c = <obj>
|}]

let x = o2#m ();;

[%%expect{|
val x : lexing_position =
  {pos_fname = ""; pos_lnum = 1; pos_bol = 508; pos_cnum = 516}
|}]

(* CR src_pos: This should probably work... *)
class this_one ~(foo : [%src_pos]) () = object 
  method m = foo
end

[%%expect{|
Lines 1-3, characters 0-3:
1 | class this_one ~(foo : [%src_pos]) () = object
2 |   method m = foo
3 | end
Error: Some type variables are unbound in this type:
         class this_one : foo:[%src_pos] -> unit -> object method m : 'a end
       The method m has type 'a where 'a is unbound
|}]

class this_one_but_optional ?(bar = 1) () = object 
  method m = bar
end

[%%expect{|
class this_one_but_optional : ?bar:int -> unit -> object method m : int end
|}]

class c ~(foo : int) = object 
  method m = foo
end

[%%expect{|
class c : foo:int -> object method m : int end
|}]

let o = new c ~foo:1
let o_m = o#m

[%%expect{|
val o : c = <obj>
val o_m : int = 1
|}]

>>>>>>> c53b6323 (Moves changes from original class-type support branch into a rebased branch)
