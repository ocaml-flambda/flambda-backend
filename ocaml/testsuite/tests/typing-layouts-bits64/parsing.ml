(* TEST
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int64#.
*)

(* Basic syntax: int64# is an unboxed int64. *)
type t = int64#;;
let f (_ : int64#) = ();;
[%%expect {|
type t = int64#
val f : int64# -> unit = <fun>
|}];;

type t = C of int64#;;
[%%expect {|
Line 1, characters 9-20:
1 | type t = C of int64#;;
             ^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_beta to use this feature.
|}];;

type t = C : int64# -> t;;
[%%expect {|
Line 1, characters 9-24:
1 | type t = C : int64# -> t;;
             ^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_beta to use this feature.
|}];;

(* int64# works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int64# list;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int64# list;;
             ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

let f (_ : int64# list) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int64# list) = ();;
               ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

type t = C of int64# list;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int64# list;;
                  ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

type t = C : int64# list -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int64# list -> t;;
                 ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

(* Syntax: int64#c
   Interpreted as type application of [c] to [int64#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int64#c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int64#c;;
             ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

let f (_ : int64#c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int64#c) = ();;
               ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

type t = C of int64#c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int64#c;;
                  ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

type t = C : int64#c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int64#c -> t;;
                 ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

(* Syntax: int64# c
   Interpreted as type application of [c] to [int64#].
*)
type t = int64# c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int64# c;;
             ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

let f (_ : int64# c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int64# c) = ();;
               ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

type t = C of int64# c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int64# c;;
                  ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

type t = C : int64# c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int64# c -> t;;
                 ^^^^^^
Error: This type int64# should be an instance of type ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's a type argument to a class constructor.
|}];;

(* Syntax: int64 #c
   Interpreted as type application of [#c] to [int64].

   Note that [int64 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = int64 #c;;
[%%expect {|
Line 1, characters 0-17:
1 | type t = int64 #c;;
    ^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type int64 #c as 'a the variable 'a is unbound
|}];;
type t = C of int64 #c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of int64 #c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (int64 #c as 'a) the variable 'a is unbound
|}];;
type 'a t = (int64 #c as 'a);;
let f (_ : int64 #c) = ();;
type 'a t = C of (int64 #c as 'a);;
type t = C : int64 #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int64 #c
val f : int64 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int64 #c
type t = C : int64 #c -> t
|}];;

(* Syntax: int64 # c
   Interpreted as type application of [#c] to [int64].

   Note that [int64 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = int64 # c;;
[%%expect {|
Line 1, characters 0-18:
1 | type t = int64 # c;;
    ^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type int64 #c as 'a the variable 'a is unbound
|}];;
type t = C of int64 # c;;
[%%expect {|
Line 1, characters 0-23:
1 | type t = C of int64 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (int64 #c as 'a) the variable 'a is unbound
|}];;

type 'a t = (int64 # c as 'a);;
let f (_ : int64 # c) = ();;
type 'a t = C of (int64 # c as 'a);;
type t = C : int64 # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int64 #c
val f : int64 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int64 #c
type t = C : int64 #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int int64#;;
[%%expect {|
Line 1, characters 9-19:
1 | type t = int int64#;;
             ^^^^^^^^^^
Error: The type constructor int64# expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int64#;;
[%%expect {|
Line 1, characters 9-26:
1 | type t = (int, int) int64#;;
             ^^^^^^^^^^^^^^^^^
Error: The type constructor int64# expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
