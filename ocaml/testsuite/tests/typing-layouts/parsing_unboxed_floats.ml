(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* These tests show how potential ambiguities are resolved
   between the types #c and float#.
*)

(* CR layouts: These tests will change a lot when we add real float
   to the type system, e.g. [C of float#] will be prohibited.
*)

(* Basic syntax: float# is an unboxed float. *)
type t = float#;;
let f (_ : float#) = ();;
type t = C of float#;;
type t = C : float# -> t;;
[%%expect {|
type t = float#
val f : float# -> unit = <fun>
type t = C of float#
type t = C : float# -> t
|}];;

(* float# works as an argument to normal type constructors, not just classes,
   even though many of the rest of the tests in this file are concerned with
   classes.
*)
type t = float# list;;
let f (_ : float# list) = ();;
type t = C of float# list;;
type t = C : float# list -> t;;
[%%expect {|
type t = float# list
val f : float# list -> unit = <fun>
type t = C of float# list
type t = C : float# list -> t
|}];;

class ['a] c = object(self)
  method x :'a = assert false
end;;
[%%expect {|
class ['a] c : object method x : 'a end
|}];;


(* Syntax: float#c
   Interpreted as type application of [c] to [float#].
*)
type t = float#c;;
let f (_ : float#c) = ();;
type t = C of float#c;;
type t = C : float#c -> t;;
[%%expect {|
type t = float# c
val f : float# c -> unit = <fun>
type t = C of float# c
type t = C : float# c -> t
|}];;

(* Syntax: float# c
   Interpreted as type application of [c] to [float#].
*)
type t = float# c;;
let f (_ : float# c) = ();;
type t = C of float# c;;
type t = C : float# c -> t;;
[%%expect {|
type t = float# c
val f : float# c -> unit = <fun>
type t = C of float# c
type t = C : float# c -> t
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
