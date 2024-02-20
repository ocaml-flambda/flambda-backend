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

