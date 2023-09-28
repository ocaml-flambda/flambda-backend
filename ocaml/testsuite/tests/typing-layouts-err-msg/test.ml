(* TEST

readonly_files = "a.ml"
flags = "-extension layouts_alpha"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "a.ml"
*** expect
*)

#directory "ocamlc.byte";;
#load "a.cmo";;
module B = A
type t_void : void
type t_value : value

[%%expect{|
module B = A
type t_void : void
type t_value : value
|}]

let f (x : t_void): 'a A.t = x

[%%expect{|
Line 1, characters 29-30:
1 | let f (x : t_void): 'a A.t = x
                                 ^
Error: This expression has type t_void but an expression was expected of type
         'a A.t = ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the type argument of A.t has this layout.
|}]

type t = t_void A.t

[%%expect{|
Line 1, characters 9-15:
1 | type t = t_void A.t
             ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the type argument of A.t has this layout.
|}]


let f (x : t_void): 'a B.t = x

[%%expect{|
Line 1, characters 29-30:
1 | let f (x : t_void): 'a B.t = x
                                 ^
Error: This expression has type t_void but an expression was expected of type
         'a B.t = ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the type argument of B.t has this layout.
|}]

type t = t_void B.t

[%%expect{|
Line 1, characters 9-15:
1 | type t = t_void B.t
             ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the type argument of B.t has this layout.
|}]

let f (x : t_void): ('a, 'b) A.t2 = x

[%%expect{|
Line 1, characters 36-37:
1 | let f (x : t_void): ('a, 'b) A.t2 = x
                                        ^
Error: This expression has type t_void but an expression was expected of type
         ('a, 'b) A.t2 = ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the 1st type argument of A.t2 has this layout.
|}]

type t = (t_void, t_value) A.t2

[%%expect{|
Line 1, characters 10-16:
1 | type t = (t_void, t_value) A.t2
              ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the 1st type argument of A.t2 has this layout.
|}]

type t = (t_value, t_void, t_void, t_void, t_void) A.t5

[%%expect{|
Line 1, characters 19-25:
1 | type t = (t_value, t_void, t_void, t_void, t_void) A.t5
                       ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the 2nd type argument of A.t5 has this layout.
|}]

type t = (t_value, t_value, t_void, t_void, t_void) A.t5

[%%expect{|
Line 1, characters 28-34:
1 | type t = (t_value, t_value, t_void, t_void, t_void) A.t5
                                ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the 3rd type argument of A.t5 has this layout.
|}]

type t = (t_value, t_value, t_value, t_void, t_void) A.t5

[%%expect{|
Line 1, characters 37-43:
1 | type t = (t_value, t_value, t_value, t_void, t_void) A.t5
                                         ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the 4th type argument of A.t5 has this layout.
|}]


type t = (t_value, t_value, t_value, t_value, t_void) A.t5

[%%expect{|
Line 1, characters 46-52:
1 | type t = (t_value, t_value, t_value, t_value, t_void) A.t5
                                                  ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         the 5th type argument of A.t5 has this layout.
|}]

let f (x: t_void) = A.f x

[%%expect{|
Line 1, characters 24-25:
1 | let f (x: t_void) = A.f x
                            ^
Error: This expression has type t_void but an expression was expected of type
         int
|}]

let f2 (x: t_void) = A.f2 x

[%%expect{|
Line 1, characters 26-27:
1 | let f2 (x: t_void) = A.f2 x
                              ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       The layout of t_void is void, because
         of the annotation on the declaration of the type t_void.
       But the layout of t_void must be a sublayout of value, because
         it's imported from another compilation unit.
|}]
