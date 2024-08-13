(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(*********************)
(* Annotation errors *)

type ('a : value) value = unit

(* Type_declaration *)
type t_void : void
and t = t_void value

[%%expect{|
type 'a value = unit
Line 5, characters 8-14:
5 | and t = t_void value
            ^^^^^^
Error: This type "t_void" = "('a : void)" should be an instance of type
         "('b : value)"
       The layout of t_void is void
         because of the annotation on the declaration of the type t_void.
       But the layout of t_void must overlap with value
         because of the definition of value at line 1, characters 0-30.
|}]

(* Type_parameter *)
type ('a : void) t = 'a value

[%%expect{|
Line 1, characters 21-23:
1 | type ('a : void) t = 'a value
                         ^^
Error: This type "('a : value)" should be an instance of type "('a0 : void)"
       The layout of 'a is void
         because of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with value
         because of the definition of value at line 1, characters 0-30.
|}]

(* Newtype_declaration *)
let f (type a : void) (x: a value) = x

[%%expect{|
Line 1, characters 26-27:
1 | let f (type a : void) (x: a value) = x
                              ^
Error: This type "a" should be an instance of type "('a : value)"
       The layout of a is void
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a sublayout of value
         because of the definition of value at line 1, characters 0-30.
|}]

(* Constructor_type_parameter *)
type _ g = A : ('a: void) . 'a value -> unit g

[%%expect{|
Line 1, characters 28-30:
1 | type _ g = A : ('a: void) . 'a value -> unit g
                                ^^
Error: This type "('a : void)" should be an instance of type "('b : value)"
       The layout of 'a is void
         because of the annotation on a in the declaration of constructor A.
       But the layout of 'a must overlap with value
         because of the definition of value at line 1, characters 0-30.
|}]

(* Univar *)
let f : ('a : void). 'a -> 'a value = assert false

[%%expect{|
Line 1, characters 27-29:
1 | let f : ('a : void). 'a -> 'a value = assert false
                               ^^
Error: This type "('a : void)" should be an instance of type "('b : value)"
       The layout of 'a is void
         because of the annotation on the universal variable 'a.
       But the layout of 'a must overlap with value
         because of the definition of value at line 1, characters 0-30.
|}]

(* Type_variable *)
type t = 'a -> int as ('b : void)

[%%expect{|
Line 85, characters 23-25:
85 | type t = 'a -> int as ('b : void)
                            ^^
Error: This alias is bound to type "'a -> int"
       but is used as an instance of type "('b : void)"
       The layout of 'a -> int is value
         because it's a function type.
       But the layout of 'a -> int must be a sublayout of void
         because of the annotation on the type variable 'b.
|}]

(* Type_wildcard *)
type t = 'a -> int as (_ : void)

[%%expect{|
Line 1, characters 27-31:
1 | type t = 'a -> int as (_ : void)
                               ^^^^
Error: Bad layout annotation:
         The layout of "'a -> int" is value
           because it's a function type.
         But the layout of "'a -> int" must be a sublayout of void
           because of the annotation on the wildcard _ at line 1, characters 27-31.
|}]
