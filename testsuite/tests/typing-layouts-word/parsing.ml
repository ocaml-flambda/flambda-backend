(* TEST
 expect;
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and nativeint#.
*)

(* Basic syntax: nativeint# is an unboxed nativeint. *)
type t = nativeint#;;
let f (_ : nativeint#) = ();;
[%%expect {|
type t = nativeint#
val f : nativeint# -> unit = <fun>
|}];;

type t = C of nativeint#;;
[%%expect {|
type t = C of nativeint#
|}];;

type t = C : nativeint# -> t;;
[%%expect {|
type t = C : nativeint# -> t
|}];;

(* nativeint# works as an argument to normal type constructors, not just
   classes, even though many of the rest of the tests in this file are concerned
   with classes.
*)
type t = nativeint# list;;
[%%expect {|
Line 1, characters 9-19:
1 | type t = nativeint# list;;
             ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

let f (_ : nativeint# list) = ();;
[%%expect {|
Line 1, characters 11-21:
1 | let f (_ : nativeint# list) = ();;
               ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C of nativeint# list;;
[%%expect {|
Line 1, characters 14-24:
1 | type t = C of nativeint# list;;
                  ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

type t = C : nativeint# list -> t;;
[%%expect {|
Line 1, characters 13-23:
1 | type t = C : nativeint# list -> t;;
                 ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type
         "('a : value_or_null)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because the type argument of list has layout value_or_null.
|}];;

(* Syntax: nativeint#c
   Interpreted as type application of [c] to [nativeint#].
*)
class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;

type t = nativeint#c;;
[%%expect {|
Line 1, characters 9-19:
1 | type t = nativeint#c;;
             ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : nativeint#c) = ();;
[%%expect {|
Line 1, characters 11-21:
1 | let f (_ : nativeint#c) = ();;
               ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of nativeint#c;;
[%%expect {|
Line 1, characters 14-24:
1 | type t = C of nativeint#c;;
                  ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : nativeint#c -> t;;
[%%expect {|
Line 1, characters 13-23:
1 | type t = C : nativeint#c -> t;;
                 ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: nativeint# c
   Interpreted as type application of [c] to [nativeint#].
*)
type t = nativeint# c;;
[%%expect {|
Line 1, characters 9-19:
1 | type t = nativeint# c;;
             ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

let f (_ : nativeint# c) = ();;
[%%expect {|
Line 1, characters 11-21:
1 | let f (_ : nativeint# c) = ();;
               ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C of nativeint# c;;
[%%expect {|
Line 1, characters 14-24:
1 | type t = C of nativeint# c;;
                  ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

type t = C : nativeint# c -> t;;
[%%expect {|
Line 1, characters 13-23:
1 | type t = C : nativeint# c -> t;;
                 ^^^^^^^^^^
Error: This type "nativeint#" should be an instance of type "('a : value)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's a type argument to a class constructor.
|}];;

(* Syntax: nativeint #c
   Interpreted as type application of [#c] to [nativeint].

   Note that [nativeint #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)
type t = nativeint #c;;
[%%expect {|
Line 1, characters 0-21:
1 | type t = nativeint #c;;
    ^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "nativeint #c as 'a" the variable "'a" is unbound
|}];;
type t = C of nativeint #c;;
[%%expect {|
Line 1, characters 0-26:
1 | type t = C of nativeint #c;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (nativeint #c as 'a)" the variable "'a" is unbound
|}];;
type 'a t = (nativeint #c as 'a);;
let f (_ : nativeint #c) = ();;
type 'a t = C of (nativeint #c as 'a);;
type t = C : nativeint #c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = nativeint #c
val f : nativeint #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = nativeint #c
type t = C : nativeint #c -> t
|}];;

(* Syntax: nativeint # c
   Interpreted as type application of [#c] to [nativeint].

   Note that [nativeint #c] implicitly binds a type variable,
   so we need to name it with [as] to get some examples to
   typecheck.
*)

type t = nativeint # c;;
[%%expect {|
Line 1, characters 0-22:
1 | type t = nativeint # c;;
    ^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In type "nativeint #c as 'a" the variable "'a" is unbound
|}];;
type t = C of nativeint # c;;
[%%expect {|
Line 1, characters 0-27:
1 | type t = C of nativeint # c;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In case "C of (nativeint #c as 'a)" the variable "'a" is unbound
|}];;

type 'a t = (nativeint # c as 'a);;
let f (_ : nativeint # c) = ();;
type 'a t = C of (nativeint # c as 'a);;
type t = C : nativeint # c -> t;;
[%%expect {|
type 'a t = 'a constraint 'a = nativeint #c
val f : nativeint #c -> unit = <fun>
type 'a t = C of 'a constraint 'a = nativeint #c
type t = C : nativeint #c -> t
|}];;

(***************************)
(* Type application: it's a type error, not a parse error. *)

type t = int nativeint#;;
[%%expect {|
Line 1, characters 9-23:
1 | type t = int nativeint#;;
             ^^^^^^^^^^^^^^
Error: The type constructor "nativeint#" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}];;

type t = (int, int) nativeint#;;
[%%expect {|
Line 1, characters 9-30:
1 | type t = (int, int) nativeint#;;
             ^^^^^^^^^^^^^^^^^^^^^
Error: The type constructor "nativeint#" expects 0 argument(s),
       but is here applied to 2 argument(s)
|}];;
