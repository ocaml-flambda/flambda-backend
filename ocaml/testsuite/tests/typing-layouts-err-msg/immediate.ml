(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(***************************)
(* Immediate layout errors *)

(* Empty_record *)
type ('a: void) t: void = { a: 'a }
[%%expect{|
Line 1, characters 0-35:
1 | type ('a: void) t: void = { a: 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
(* Records with all void fields are not yet supported *)

(* Enumeration *)
type ('a: void) t = 'a
type v = unit
let f (x: v): 'a t = x
[%%expect{|
type ('a : void) t = 'a
type v = unit
Line 3, characters 21-22:
3 | let f (x: v): 'a t = x
                         ^
Error: This expression has type v = unit
       but an expression was expected of type 'a t = ('a : void)
       The layout of unit is immediate, because
         it's an enumeration variant type (all constructors are constant).
       But the layout of unit must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]

(* Primitive *)
type ('a: void) t = 'a
let f (x: int): 'a t = x
[%%expect{|
type ('a : void) t = 'a
Line 2, characters 23-24:
2 | let f (x: int): 'a t = x
                           ^
Error: This expression has type int but an expression was expected of type
         'a t = ('a : void)
       The layout of int is immediate, because
         it is the primitive immediate type int.
       But the layout of int must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}];;

(* Immediate_polymorphic_variant *)
type ('a: void) t = 'a
let f (x: [`A | `B]): 'a t = x
[%%expect{|
type ('a : void) t = 'a
Line 2, characters 29-30:
2 | let f (x: [`A | `B]): 'a t = x
                                 ^
Error: This expression has type [ `A | `B ]
       but an expression was expected of type 'a t = ('a : void)
       The layout of [ `A | `B ] is immediate, because
         it's an enumeration variant type (all constructors are constant).
       But the layout of [ `A | `B ] must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]
