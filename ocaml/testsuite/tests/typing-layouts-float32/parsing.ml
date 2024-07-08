(* TEST
 flags = "-extension layouts_alpha -extension small_numbers";
 {
   flags = "-extension layouts_beta -extension small_numbers";
   expect;
 }
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and float32#.
*)

(* Basic syntax: float32# is an unboxed float. *)
type t = float32#;;
let f (_ : float32#) = ();;
[%%expect {|
type t = float32#
val f : float32# -> unit = <fun>
|}];;

type t = C of float32#;;
[%%expect {|
type t = C of float32#
|}];;

type t = C : float32# -> t;;
[%%expect {|
type t = C : float32# -> t
|}];;

(* float32# works as an argument to normal type constructors, not just classes,
   even though many of the rest of the tests in this file are concerned with
   classes.
*)
type t = float32# list;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = float32# list;;
             ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

let f (_ : float32# list) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : float32# list) = ();;
               ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C of float32# list;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of float32# list;;
                  ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C : float32# list -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : float32# list -> t;;
                 ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

(* Syntax: float32#c
   Interpreted as type application of [c] to [float32#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = float32#c;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = float32#c;;
             ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : float32#c) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : float32#c) = ();;
               ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of float32#c;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of float32#c;;
                  ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : float32#c -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : float32#c -> t;;
                 ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: float32# c
   Interpreted as type application of [c] to [float32#].
*)
type t = float32# c;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = float32# c;;
             ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : float32# c) = ();;
[%%expect {|
Line 1, characters 11-19:
1 | let f (_ : float32# c) = ();;
               ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of float32# c;;
[%%expect {|
Line 1, characters 14-22:
1 | type t = C of float32# c;;
                  ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : float32# c -> t;;
[%%expect {|
Line 1, characters 13-21:
1 | type t = C : float32# c -> t;;
                 ^^^^^^^^
Error: This type float32# should be an instance of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: float32 #c
   Interpreted as type application of [#c] to [float32].

   Note that [float32 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = float32 #c;;
[%%expect {|
Line 1, characters 0-19:
1 | type t = float32 #c;;
    ^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type float32 #c as 'a the variable 'a is unbound
|}];;
type t = C of float32 #c;;
[%%expect {|
Line 1, characters 0-24:
1 | type t = C of float32 #c;;
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (float32 #c as 'a) the variable 'a is unbound
|}];;
type 'a t = (float32 #c as 'a);;
let f (_ : float32 #c) = ();;
type 'a t = C of (float32 #c as 'a);;
type t = C : float32 #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = float32 #c
val f : float32 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = float32 #c
type t = C : float32 #c -> t
|}];;

(* Syntax: float32 # c
   Interpreted as type application of [#c] to [float32].

   Note that [float32 #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = float32 # c;;
[%%expect {|
Line 1, characters 0-20:
1 | type t = float32 # c;;
    ^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type float32 #c as 'a the variable 'a is unbound
|}];;
type t = C of float32 # c;;
[%%expect {|
Line 1, characters 0-25:
1 | type t = C of float32 # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (float32 #c as 'a) the variable 'a is unbound
|}];;

type 'a t = (float32 # c as 'a);;
let f (_ : float32 # c) = ();;
type 'a t = C of (float32 # c as 'a);;
type t = C : float32 # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = float32 #c
val f : float32 #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = float32 #c
type t = C : float32 #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int float32#;;
[%%expect {|
Line 1, characters 9-21:
1 | type t = int float32#;;
             ^^^^^^^^^^^^
Error: The type constructor float32# expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) float32#;;
[%%expect {|
Line 1, characters 9-28:
1 | type t = (int, int) float32#;;
             ^^^^^^^^^^^^^^^^^^^
Error: The type constructor float32# expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;

(*******************)
(* Hint for #float32 *)
type t = #float32;;
[%%expect {|
Line 1, characters 9-17:
1 | type t = #float32;;
             ^^^^^^^^
Error: float32 isn't a class type. Did you mean the unboxed type float32#?
|}]
