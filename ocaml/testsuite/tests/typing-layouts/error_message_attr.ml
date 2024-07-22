(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 expect;
*)

module Float_u = Stdlib_upstream_compatible.Float_u

[%%expect{|
module Float_u = Stdlib_upstream_compatible.Float_u
|}]

(* Needs a string payload *)

let f (v: float#): ((_ : value)[@error_message]) = v
[%%expect{|
Line 1, characters 31-47:
1 | let f (v: float#): ((_ : value)[@error_message]) = v
                                   ^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'error_message'.
error_message attribute expects a string argument

Line 1, characters 51-52:
1 | let f (v: float#): ((_ : value)[@error_message]) = v
                                                       ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64
         because it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value
         because of the annotation on the wildcard _ at line 1, characters 20-31.
|}]

let f (v: float#): ((_ : value)[@error_message 1]) = v
[%%expect{|
Line 1, characters 31-49:
1 | let f (v: float#): ((_ : value)[@error_message 1]) = v
                                   ^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'error_message'.
error_message attribute expects a string argument

Line 1, characters 53-54:
1 | let f (v: float#): ((_ : value)[@error_message 1]) = v
                                                         ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64
         because it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value
         because of the annotation on the wildcard _ at line 1, characters 20-31.
|}]

(* Ltyp_var { name = None; layout } case *)
let f (v: float#): ((_ : value)[@error_message "Custom message"]) = v
[%%expect{|
Line 1, characters 68-69:
1 | let f (v: float#): ((_ : value)[@error_message "Custom message"]) = v
                                                                        ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64
         because it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value
         because of the annotation on the wildcard _ at line 1, characters 20-31.
         Custom message
|}]

let f x =
  ignore ((x : (_ : value)[@error_message "Custom message"]));
  Float_u.to_float x
[%%expect{|
Line 3, characters 19-20:
3 |   Float_u.to_float x
                       ^
Error: This expression has type ('a : value)
       but an expression was expected of type Float_u.t = float#
       The layout of Float_u.t is float64
         because it is the primitive float64 type float#.
       But the layout of Float_u.t must be a sublayout of value
         because of the annotation on the wildcard _ at line 2, characters 15-26.
         Custom message
|}]

(* Ltyp_var { name = Some name; layout } case *)
module type a = sig
  type ('a : float64) t = 'a
  val f : (('a : value)[@error_message "Custom message"]) -> 'a t
end

[%%expect{|
Line 3, characters 61-63:
3 |   val f : (('a : value)[@error_message "Custom message"]) -> 'a t
                                                                 ^^
Error: This type ('a : value) should be an instance of type ('b : float64)
       The layout of 'a is value
         because of the annotation on the type variable 'a.
         Custom message
       But the layout of 'a must overlap with float64
         because of the definition of t at line 2, characters 2-28.
|}]


(* Ltyp_alias { aliased_type; name; layout } case *)

(* First call to [layout_of_annotation] in [transl_type_alias] *)
module type a = sig
  type t : float64
  val f : 'a -> t -> (t as ('a : value)[@error_message "Custom message"])
end
[%%expect{|
Line 3, characters 33-38:
3 |   val f : 'a -> t -> (t as ('a : value)[@error_message "Custom message"])
                                     ^^^^^
Error: Bad layout annotation:
         The layout of t is float64
           because of the definition of t at line 2, characters 2-18.
         But the layout of t must be a sublayout of value
           because of the annotation on the type variable 'a.
           Custom message
|}]

(* Second call to [layout_of_annotation] in the Not_found case
   of [transl_type_alias] *)
module type a = sig
  type t : float64
  val f : t -> (t as ('a : value)[@error_message "Custom message"])
end
[%%expect{|
Line 3, characters 16-33:
3 |   val f : t -> (t as ('a : value)[@error_message "Custom message"])
                    ^^^^^^^^^^^^^^^^^
Error: This alias is bound to type t but is used as an instance of type
         ('a : value)
       The layout of t is float64
         because of the definition of t at line 2, characters 2-18.
       But the layout of t must be a sublayout of value
         because of the annotation on the type variable 'a.
         Custom message
|}]

(* Third call to [layout_of_annotation] in the None case
   of [transl_type_alias] *)
module type a = sig
  type t : float64
  val f : t -> (t as (_ : value)[@error_message "Custom message"])
end
[%%expect{|
Line 3, characters 26-31:
3 |   val f : t -> (t as (_ : value)[@error_message "Custom message"])
                              ^^^^^
Error: Bad layout annotation:
         The layout of t/2 is float64
           because of the definition of t at line 2, characters 2-18.
         But the layout of t/2 must be a sublayout of value
           because of the annotation on the wildcard _ at line 3, characters 26-31.
           Custom message
|}]

(* Currently it's not possible to attach attributes to Ltyp_poly *)

(* *************************************************************** *)
(* Tests for [@error_message] applied to [Pexp_constraint].
   Seperate implementation from when it's applied to layout annotations. *)

(* Needs a string body *)
let f (x : bool) = (x : int)[@error_message]
[%%expect{|
Line 1, characters 28-44:
1 | let f (x : bool) = (x : int)[@error_message]
                                ^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'error_message'.
error_message attribute expects a string argument

Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message]
                        ^
Error: This expression has type bool but an expression was expected of type
         int
|}]

(* Can only be applied once *)
let f (x : bool) = (x : int)[@error_message "A"][@error_message "B"]
[%%expect{|
Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message "A"][@error_message "B"]
                        ^
Error: This expression has type bool but an expression was expected of type
         int
       A
|}]

(* Simple test case *)
let f (x : bool) = (x : int)[@error_message "custom message"]
[%%expect{|
Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message "custom message"]
                        ^
Error: This expression has type bool but an expression was expected of type
         int
       custom message
|}]

(* Doesn't work when the type mismatch happens later. This differs from
   the layout annotation case. *)
let f x: bool = (x : int)[@error_message "custom message"]
[%%expect{|
Line 1, characters 16-25:
1 | let f x: bool = (x : int)[@error_message "custom message"]
                    ^^^^^^^^^
Error: This expression has type int but an expression was expected of type
         bool
|}]

(* Doesn't apply when the type error is from elsewhere within the expression *)
let g (x : int) = x
let f (x : bool) = (let y = false in g y : int)[@error_message "custom message"]
[%%expect{|
val g : int -> int = <fun>
Line 2, characters 39-40:
2 | let f (x : bool) = (let y = false in g y : int)[@error_message "custom message"]
                                           ^
Error: This expression has type bool but an expression was expected of type
         int
|}]

(* Can be used to enforce layouts but not great *)
let f (x : string) = (x : (_ : immediate))[@error_message "custom message"]
[%%expect{|
Line 1, characters 22-23:
1 | let f (x : string) = (x : (_ : immediate))[@error_message "custom message"]
                          ^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       custom message
       The kind of string is immutable_data
         because it is the primitive immutable_data type string.
       But the kind of string must be a subkind of immediate
         because of the annotation on the wildcard _ at line 1, characters 26-41.
|}]

(* Doesn't apply when the mismatch is deep *)
let f () = (fun (x: int) -> x : string -> string)[@error_message "custom message"]
[%%expect{|
Line 1, characters 16-24:
1 | let f () = (fun (x: int) -> x : string -> string)[@error_message "custom message"]
                    ^^^^^^^^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type string
|}]

let f () = (fun (x: int) -> x : string)[@error_message "custom message"]
[%%expect{|
Line 1, characters 12-29:
1 | let f () = (fun (x: int) -> x : string)[@error_message "custom message"]
                ^^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       string
       custom message
|}]

(* Same when the function is not declared inline *)
let g (x: int) = x
let f () = (g : (string -> string))[@error_message "custom message"]
[%%expect{|
val g : int -> int = <fun>
Line 2, characters 12-13:
2 | let f () = (g : (string -> string))[@error_message "custom message"]
                ^
Error: This expression has type int -> int
       but an expression was expected of type string -> string
       Type int is not compatible with type string
|}]

let g (x: int) = x
let f () = (g : string)[@error_message "custom message"]
[%%expect{|
val g : int -> int = <fun>
Line 2, characters 12-13:
2 | let f () = (g : string)[@error_message "custom message"]
                ^
Error: This expression has type int -> int
       but an expression was expected of type string
       custom message
|}]
