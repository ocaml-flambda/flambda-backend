(* TEST
 flags = "-extension layouts_beta -extension small_numbers_beta";
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int8#.
*)

(* Basic syntax: int8# is an unboxed int8. *)
type t = int8#;;
let f (_ : int8#) = ();;
[%%expect {|
type t = int8#
val f : int8# -> unit = <fun>
|}];;

type t = C of int8#;;
[%%expect {|
type t = C of int8#
|}];;

type t = C : int8# -> t;;
[%%expect {|
type t = C : int8# -> t
|}];;

(* int8# works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int8# list;;
[%%expect {|
Line 1, characters 9-14:
1 | type t = int8# list;;
             ^^^^^
Error: This type "int8#" should be an instance of type "('a : value_or_null)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

let f (_ : int8# list) = ();;
[%%expect {|
Line 1, characters 11-16:
1 | let f (_ : int8# list) = ();;
               ^^^^^
Error: This type "int8#" should be an instance of type "('a : value_or_null)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C of int8# list;;
[%%expect {|
Line 1, characters 14-19:
1 | type t = C of int8# list;;
                  ^^^^^
Error: This type "int8#" should be an instance of type "('a : value_or_null)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C : int8# list -> t;;
[%%expect {|
Line 1, characters 13-18:
1 | type t = C : int8# list -> t;;
                 ^^^^^
Error: This type "int8#" should be an instance of type "('a : value_or_null)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

(* Syntax: int8#c
   Interpreted as type application of [c] to [int8#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int8#c;;
[%%expect {|
Line 1, characters 9-14:
1 | type t = int8#c;;
             ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int8#c) = ();;
[%%expect {|
Line 1, characters 11-16:
1 | let f (_ : int8#c) = ();;
               ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int8#c;;
[%%expect {|
Line 1, characters 14-19:
1 | type t = C of int8#c;;
                  ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int8#c -> t;;
[%%expect {|
Line 1, characters 13-18:
1 | type t = C : int8#c -> t;;
                 ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int8# c
   Interpreted as type application of [c] to [int8#].
*)
type t = int8# c;;
[%%expect {|
Line 1, characters 9-14:
1 | type t = int8# c;;
             ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int8# c) = ();;
[%%expect {|
Line 1, characters 11-16:
1 | let f (_ : int8# c) = ();;
               ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int8# c;;
[%%expect {|
Line 1, characters 14-19:
1 | type t = C of int8# c;;
                  ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int8# c -> t;;
[%%expect {|
Line 1, characters 13-18:
1 | type t = C : int8# c -> t;;
                 ^^^^^
Error: This type "int8#" should be an instance of type "('a : value)"
       The layout of int8# is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of int8# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int8 #c
   Interpreted as type application of [#c] to [int8].

   Note that [int8 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = int8 #c;;
[%%expect {|
Line 1, characters 0-16:
1 | type t = int8 #c;;
    ^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int8 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int8 #c;;
[%%expect {|
Line 1, characters 0-21:
1 | type t = C of int8 #c;;
    ^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int8 #c as 'a)" the variable "'a" is unbound
|}];;
type 'a t = (int8 #c as 'a);;
let f (_ : int8 #c) = ();;
type 'a t = C of (int8 #c as 'a);;
type t = C : int8 #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int8 #c
val f : int8 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int8 #c
type t = C : int8 #c -> t
|}];;

(* Syntax: int8 # c
   Interpreted as type application of [#c] to [int8].

   Note that [int8 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = int8 # c;;
[%%expect {|
Line 1, characters 0-17:
1 | type t = int8 # c;;
    ^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "int8 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int8 # c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of int8 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int8 #c as 'a)" the variable "'a" is unbound
|}];;

type 'a t = (int8 # c as 'a);;
let f (_ : int8 # c) = ();;
type 'a t = C of (int8 # c as 'a);;
type t = C : int8 # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int8 #c
val f : int8 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int8 #c
type t = C : int8 #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int int8#;;
[%%expect {|
Line 1, characters 9-18:
1 | type t = int int8#;;
             ^^^^^^^^^
Error: The type constructor "int8#" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int8#;;
[%%expect {|
Line 1, characters 9-25:
1 | type t = (int, int) int8#;;
             ^^^^^^^^^^^^^^^^
Error: The type constructor "int8#" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
