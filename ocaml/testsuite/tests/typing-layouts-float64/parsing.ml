(* TEST
 flags = "-extension layouts_alpha";
 {
   flags = "-extension layouts_beta";
   expect;
 }{
   expect;
 }
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and float#.
*)

(* Basic syntax: float# is an unboxed float. *)
type t = float#;;
let f (_ : float#) = ();;
[%%expect {|
type t = float#
val f : float# -> unit = <fun>
|}];;

(* float# works as an argument to normal type constructors, not just classes,
   even though many of the rest of the tests in this file are concerned with
   classes.
*)
type t = float# list;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = float# list;;
             ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

let f (_ : float# list) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : float# list) = ();;
               ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C of float# list;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of float# list;;
                  ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

type t = C : float# list -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : float# list -> t;;
                 ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because the type argument of list has layout value.
|}];;

(* Syntax: float#c
   Interpreted as type application of [c] to [float#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = float#c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = float#c;;
             ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : float#c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : float#c) = ();;
               ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of float#c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of float#c;;
                  ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : float#c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : float#c -> t;;
                 ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: float# c
   Interpreted as type application of [c] to [float#].
*)
type t = float# c;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = float# c;;
             ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : float# c) = ();;
[%%expect {|
Line 1, characters 11-17:
1 | let f (_ : float# c) = ();;
               ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of float# c;;
[%%expect {|
Line 1, characters 14-20:
1 | type t = C of float# c;;
                  ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : float# c -> t;;
[%%expect {|
Line 1, characters 13-19:
1 | type t = C : float# c -> t;;
                 ^^^^^^
Error: This type float# should be an instance of type ('a : value)
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: float #c
   Interpreted as type application of [#c] to [float].

   Note that [float #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = float #c;;
[%%expect {|
Line 1, characters 0-17:
1 | type t = float #c;;
    ^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type float #c as 'a the variable 'a is unbound
|}];;
type t = C of float #c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = C of float #c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (float #c as 'a) the variable 'a is unbound
|}];;
type 'a t = (float #c as 'a);;
let f (_ : float #c) = ();;
type 'a t = C of (float #c as 'a);;
type t = C : float #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = float #c
val f : float #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = float #c
type t = C : float #c -> t
|}];;

(* Syntax: float # c
   Interpreted as type application of [#c] to [float].

   Note that [float #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = float # c;;
[%%expect {|
Line 1, characters 0-18:
1 | type t = float # c;;
    ^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type float #c as 'a the variable 'a is unbound
|}];;
type t = C of float # c;;
[%%expect {|
Line 1, characters 0-23:
1 | type t = C of float # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case C of (float #c as 'a) the variable 'a is unbound
|}];;

type 'a t = (float # c as 'a);;
let f (_ : float # c) = ();;
type 'a t = C of (float # c as 'a);;
type t = C : float # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = float #c
val f : float #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = float #c
type t = C : float #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int float#;;
[%%expect {|
Line 1, characters 9-19:
1 | type t = int float#;;
             ^^^^^^^^^^
Error: The type constructor float# expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) float#;;
[%%expect {|
Line 1, characters 9-26:
1 | type t = (int, int) float#;;
             ^^^^^^^^^^^^^^^^^
Error: The type constructor float# expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;

(*******************)
(* Hint for #float *)
type t = #float;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = #float;;
<<<<<<< HEAD
             ^^^^^^
Error: float isn't a class type. Did you mean the unboxed type float#?
|}]

(* Hint should not show up in this case *)
class type floot = object end
class type c = float
[%%expect {|
class type floot = object  end
Line 2, characters 15-20:
2 | class type c = float
                   ^^^^^
Error: Unbound class type float
Hint: Did you mean floot?
||||||| b26b2bd6c5
              ^^^^^
Error: Unbound class type float
Hint: Did you mean float#?
=======
             ^^^^^^
Error: float isn't a class type. Did you mean the unboxed type float#?
>>>>>>> ocaml-jst/flambda-patches
|}]
