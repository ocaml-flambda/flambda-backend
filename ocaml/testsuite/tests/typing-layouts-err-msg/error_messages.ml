(* TEST

readonly_files = "a.ml"
flags = "-extension layouts_alpha"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "a.ml"
*** expect
*)

(* Arrow *)
type ('a: void) t = 'a
let f (x : int -> int): 'a t = x

[%%expect{|
type ('a : void) t = 'a
Line 2, characters 31-32:
2 | let f (x : int -> int): 'a t = x
                                   ^
Error: This expression has type int -> int
       but an expression was expected of type 'a t = ('a : void)
       The layout of int -> int is value, because
         it's a function type.
       But the layout of int -> int must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]


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
         it's an enumeration variant (all constructors are constant).
       But the layout of [ `A | `B ] must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]


(* Polymorphic_variant *)
type ('a: void) t = 'a
let f (x: [`A of int | `B]): 'a t = x

[%%expect{|
type ('a : void) t = 'a
Line 2, characters 36-37:
2 | let f (x: [`A of int | `B]): 'a t = x
                                        ^
Error: This expression has type [ `A of int | `B ]
       but an expression was expected of type 'a t = ('a : void)
       The layout of [ `A of int | `B ] is value, because
         it's a polymorphic variant.
       But the layout of [ `A of int | `B ] must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]


(* Enumeration *)
type ('a: void) t = 'a
type v = A
let f (x: v): 'a t = x

[%%expect{|
type ('a : void) t = 'a
type v = A
Line 3, characters 21-22:
3 | let f (x: v): 'a t = x
                         ^
Error: This expression has type v but an expression was expected of type
         'a t = ('a : void)
       The layout of v is immediate, because
         of the definition of v at line 2, characters 0-10.
       But the layout of v must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]

(* Extensible_variant *)
type ('a: void) t = 'a
type attr = ..
and w = attr t

[%%expect{|
type ('a : void) t = 'a
Line 2, characters 0-14:
2 | type attr = ..
    ^^^^^^^^^^^^^^
Error:
       The layout of attr is value, because
         it's an extensible variant.
       But the layout of attr must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]

(* First_class_module *)
type ('a: void) t = 'a
module type X_int = sig val x : int end;;
module Three : X_int = struct let x = 3 end;;
let f (): 'a t = (module Three : X_int)

[%%expect{|
type ('a : void) t = 'a
module type X_int = sig val x : int end
module Three : X_int
Line 4, characters 17-39:
4 | let f (): 'a t = (module Three : X_int)
                     ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type (module X_int)
       but an expression was expected of type 'a t = ('a : void)
       The layout of (module X_int) is value, because
         it's a first-class module type.
       But the layout of (module X_int) must be a sublayout of void, because
         of the definition of t at line 1, characters 0-22.
|}]

(* Match *)
type t_any : any
let () = match (assert false : t_any) with _ -> ()

[%%expect{|
type t_any : any
Line 2, characters 15-37:
2 | let () = match (assert false : t_any) with _ -> ()
                   ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_1)
       The layout of t_any is any, because
         of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a sublayout of '_representable_layout_1, because
         it's matched against a pattern.
|}]

(* Type_wildcard *)
type t = 'a -> int as (_ : void)

[%%expect{|
Line 1, characters 27-31:
1 | type t = 'a -> int as (_ : void)
                               ^^^^
Error: Bad layout annotation:
         The layout of 'a -> int is value, because
           it's a function type.
         But the layout of 'a -> int must be a sublayout of void, because
           of the annotation on the wildcard _ at line 1, characters 27-31.
|}]
