(* TEST
 flags = "-extension layouts_beta -extension small_numbers_beta";
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int16#.
*)

(* Basic syntax: int16# is an unboxed int16. *)
type t = int16#;;
let f (_ : int16#) = ();;
[%%expect {|
type t = int16#
val f : int16# -> unit = <fun>
|}];;

type t = C of int16#;;
[%%expect {|
type t = C of int16#
|}];;

type t = C : int16# -> t;;
[%%expect {|
type t = C : int16# -> t
|}];;

(* int16# works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int16# list;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int16# list;;
             ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value_or_null)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

let f (_ : int16# list) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int16# list) = ();;
               ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value_or_null)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C of int16# list;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int16# list;;
                  ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value_or_null)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C : int16# list -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int16# list -> t;;
                 ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value_or_null)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

(* Syntax: int16#c
   Interpreted as type application of [c] to [int16#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int16#c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int16#c;;
             ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int16#c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int16#c) = ();;
               ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int16#c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int16#c;;
                  ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int16#c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int16#c -> t;;
                 ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int16# c
   Interpreted as type application of [c] to [int16#].
*)
type t = int16# c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int16# c;;
             ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int16# c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int16# c) = ();;
               ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int16# c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int16# c;;
                  ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int16# c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int16# c -> t;;
                 ^^^^^^
Error: This type "int16#" should be an instance of type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int16 #c
   Interpreted as type application of [#c] to [int16].

   Note that [int16 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = int16 #c;;
[%%expect {|
Line 1, characters 0-17:
1 | type t = int16 #c;;
    ^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int16 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int16 #c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of int16 #c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int16 #c as 'a)" the variable "'a" is unbound
|}];;
type 'a t = (int16 #c as 'a);;
let f (_ : int16 #c) = ();;
type 'a t = C of (int16 #c as 'a);;
type t = C : int16 #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int16 #c
val f : int16 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int16 #c
type t = C : int16 #c -> t
|}];;

(* Syntax: int16 # c
   Interpreted as type application of [#c] to [int16].

   Note that [int16 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = int16 # c;;
[%%expect {|
Line 1, characters 0-18:
1 | type t = int16 # c;;
    ^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int16 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int16 # c;;
[%%expect {|
Line 1, characters 0-23:
1 | type t = C of int16 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int16 #c as 'a)" the variable "'a" is unbound
|}];;

type 'a t = (int16 # c as 'a);;
let f (_ : int16 # c) = ();;
type 'a t = C of (int16 # c as 'a);;
type t = C : int16 # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int16 #c
val f : int16 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int16 #c
type t = C : int16 #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int int16#;;
[%%expect {|
Line 1, characters 9-19:
1 | type t = int int16#;;
             ^^^^^^^^^^
Error: The type constructor "int16#" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int16#;;
[%%expect {|
Line 1, characters 9-26:
1 | type t = (int, int) int16#;;
             ^^^^^^^^^^^^^^^^^
Error: The type constructor "int16#" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
