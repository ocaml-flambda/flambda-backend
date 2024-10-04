(* TEST
 flags = "-extension layouts_beta -extension simd_beta";
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and int64x2#.
*)

(* Basic syntax: int64x2# is an unboxed int64x2. *)
type t = int64x2#;;
let f (_ : int64x2#) = ();;
[%%expect {|
type t = int64x2#
val f : int64x2# -> unit = <fun>
|}];;

type t = C of int64x2#;;
[%%expect {|
type t = C of int64x2#
|}];;

type t = C : int64x2# -> t;;
[%%expect {|
type t = C : int64x2# -> t
|}];;

(* int64x2# works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = int64x2# list;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = int64x2# list;;
             ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

let f (_ : int64x2# list) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : int64x2# list) = ();;
               ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C of int64x2# list;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of int64x2# list;;
                  ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C : int64x2# list -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : int64x2# list -> t;;
                 ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

(* Syntax: int64x2#c
   Interpreted as type application of [c] to [int64x2#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = int64x2#c;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = int64x2#c;;
             ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int64x2#c) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : int64x2#c) = ();;
               ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int64x2#c;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of int64x2#c;;
                  ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int64x2#c -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : int64x2#c -> t;;
                 ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: int64x2# c
   Interpreted as type application of [c] to [int64x2#].
*)
type t = int64x2# c;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = int64x2# c;;
             ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : int64x2# c) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : int64x2# c) = ();;
               ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of int64x2# c;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of int64x2# c;;
                  ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : int64x2# c -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : int64x2# c -> t;;
                 ^^^^^^^^
Error: This type "int64x2#" should be an instance of type "('a : value)"
       The layout of int64x2# is vec128
         because it is the primitive type int64x2#.
       But the layout of int64x2# must be a sublayout of value
         because it's a type argument to a class constructor.
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
       In type "int64 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int64 #c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of int64 #c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int64 #c as 'a)" the variable "'a" is unbound
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
       In type "int64 #c as 'a" the variable "'a" is unbound
|}];;
type t = C of int64 # c;;
[%%expect {|
Line 1, characters 0-23:
1 | type t = C of int64 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (int64 #c as 'a)" the variable "'a" is unbound
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

type t = int int64x2#;;
[%%expect {|
Line 1, characters 9-21:
1 | type t = int int64x2#;;
             ^^^^^^^^^^^^
Error: The type constructor "int64x2#" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) int64x2#;;
[%%expect {|
Line 1, characters 9-28:
1 | type t = (int, int) int64x2#;;
             ^^^^^^^^^^^^^^^^^^^
Error: The type constructor "int64x2#" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
