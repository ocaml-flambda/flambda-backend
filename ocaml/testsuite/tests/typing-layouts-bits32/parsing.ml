(* TEST
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int32#.
*)

(* Basic syntax: int32# is an unboxed int32. *)
type t = int32#;;
let f (_ : int32#) = ();;
[%%expect {|
type t = int32#
val f : int32# -> unit = <fun>
|}];;

type t = C of int32#;;
[%%expect {|
type t = C of int32#
|}];;

type t = C : int32# -> t;;
[%%expect {|
type t = C : int32# -> t
|}];;

(* int32# works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int32# list;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int32# list;;
             ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

let f (_ : int32# list) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int32# list) = ();;
               ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C of int32# list;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int32# list;;
                  ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C : int32# list -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int32# list -> t;;
                 ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

(* Syntax: int32#c
   Interpreted as type application of [c] to [int32#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int32#c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int32#c;;
             ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int32#c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int32#c) = ();;
               ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int32#c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int32#c;;
                  ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int32#c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int32#c -> t;;
                 ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int32# c
   Interpreted as type application of [c] to [int32#].
*)
type t = int32# c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = int32# c;;
             ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int32# c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : int32# c) = ();;
               ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int32# c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of int32# c;;
                  ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int32# c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : int32# c -> t;;
                 ^^^^^^
Error: This type int32# should be an instance of type ('a : value)
       The layout of int32# is bits32
         because it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int32 #c
   Interpreted as type application of [#c] to [int32].

   Note that [int32 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = int32 #c;;
[%%expect {|
Line 1, characters 0-17:
1 | type t = int32 #c;;
    ^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type int32 #c as 'a the variable 'a is unbound
|}];;
type t = C of int32 #c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of int32 #c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (int32 #c as 'a) the variable 'a is unbound
|}];;
type 'a t = (int32 #c as 'a);;
let f (_ : int32 #c) = ();;
type 'a t = C of (int32 #c as 'a);;
type t = C : int32 #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int32 #c
val f : int32 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int32 #c
type t = C : int32 #c -> t
|}];;

(* Syntax: int32 # c
   Interpreted as type application of [#c] to [int32].

   Note that [int32 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = int32 # c;;
[%%expect {|
Line 1, characters 0-18:
1 | type t = int32 # c;;
    ^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type int32 #c as 'a the variable 'a is unbound
|}];;
type t = C of int32 # c;;
[%%expect {|
Line 1, characters 0-23:
1 | type t = C of int32 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (int32 #c as 'a) the variable 'a is unbound
|}];;

type 'a t = (int32 # c as 'a);;
let f (_ : int32 # c) = ();;
type 'a t = C of (int32 # c as 'a);;
type t = C : int32 # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = int32 #c
val f : int32 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = int32 #c
type t = C : int32 #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int int32#;;
[%%expect {|
Line 1, characters 9-19:
1 | type t = int int32#;;
             ^^^^^^^^^^
Error: The type constructor int32# expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int32#;;
[%%expect {|
Line 1, characters 9-26:
1 | type t = (int, int) int32#;;
             ^^^^^^^^^^^^^^^^^
Error: The type constructor int32# expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
