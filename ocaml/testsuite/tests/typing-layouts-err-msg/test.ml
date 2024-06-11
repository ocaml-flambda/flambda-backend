(* TEST
 readonly_files = "a.ml";
 flags = "-extension layouts_alpha";
 setup-ocamlc.byte-build-env;
 module = "a.ml";
 ocamlc.byte;
 expect;
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
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void A.t.
       Please report this error to the Jane Street compilers team.
|}]

type t = t_void A.t

[%%expect{|
type t = t_void A.t
|}]


let f (x : t_void): 'a B.t = x

[%%expect{|
Line 1, characters 29-30:
1 | let f (x : t_void): 'a B.t = x
                                 ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void B.t.
       Please report this error to the Jane Street compilers team.
|}]

type t = t_void B.t

[%%expect{|
type t = t_void B.t
|}]

let f (x : t_void): ('a, 'b) A.t2 = x

[%%expect{|
Line 1, characters 36-37:
1 | let f (x : t_void): ('a, 'b) A.t2 = x
                                        ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       (t_void, 'b) A.t2.
       Please report this error to the Jane Street compilers team.
|}]

type t = (t_void, t_value) A.t2

[%%expect{|
type t = (t_void, t_value) A.t2
|}]

type t = (t_value, t_void, t_void, t_void, t_void) A.t5

[%%expect{|
type t = (t_value, t_void, t_void, t_void, t_void) A.t5
|}]

type t = (t_value, t_value, t_void, t_void, t_void) A.t5

[%%expect{|
type t = (t_value, t_value, t_void, t_void, t_void) A.t5
|}]

type t = (t_value, t_value, t_value, t_void, t_void) A.t5

[%%expect{|
type t = (t_value, t_value, t_value, t_void, t_void) A.t5
|}]


type t = (t_value, t_value, t_value, t_value, t_void) A.t5

[%%expect{|
type t = (t_value, t_value, t_value, t_value, t_void) A.t5
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
         of the definition of t_void at line 2, characters 0-18.
       But the layout of t_void must be a sublayout of value, because
         of layout requirements from an imported definition.
|}]

type ('a : value) t_v = 'a
type ('a : void) t_vv = 'a
let f x: 'a t_v = x
let f2 = f
let () = ignore (f2 (assert false : 'a t_vv))

[%%expect{|
type 'a t_v = 'a
type ('a : void) t_vv = 'a
val f : 'a t_v -> 'a t_v = <fun>
val f2 : 'a t_v -> 'a t_v = <fun>
Line 5, characters 20-44:
5 | let () = ignore (f2 (assert false : 'a t_vv))
                        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a t_vv = ('a : void)
       but an expression was expected of type 'b t_v = ('b : value)
       The layout of 'a is value, because
         of the definition of f2 at line 4, characters 9-10.
       But the layout of 'a must overlap with void, because
         of the definition of t_vv at line 2, characters 0-26.
|}]

type ('a : value) t_v = 'a
type ('a : void) t_v1 = 'a
type 'a t_v2 = 'a t_v1
let f x: 'a t_v = x

let () = ignore (f (assert false : 'a t_v2))

[%%expect{|
type 'a t_v = 'a
type ('a : void) t_v1 = 'a
type ('a : void) t_v2 = 'a t_v1
val f : 'a t_v -> 'a t_v = <fun>
Line 6, characters 19-43:
6 | let () = ignore (f (assert false : 'a t_v2))
                       ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a t_v2 = ('a : void)
       but an expression was expected of type 'b t_v = ('b : value)
       The layout of 'a is value, because
         of the definition of f at line 4, characters 6-19.
       But the layout of 'a must overlap with void, because
         of the definition of t_v2 at line 3, characters 0-22.
|}]


module type S1 = sig
  type ('a : void) t = 'a
end

module type S2 = S1

module M : S2 = struct
  type 'a t = 'a
end

[%%expect{|
module type S1 = sig type ('a : void) t = 'a end
module type S2 = S1
module M : S2
|}]
