(* TEST
   * expect
   flags = "-extension layouts"
*)


type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
;;
[%%expect{|
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
|}];;

type t_any : any;;

[%%expect{|
Line 1, characters 13-16:
1 | type t_any : any;;
                 ^^^
Error: Layout any is used here, but the appropriate layouts extension is not enabled
|}]

type t_void : void

[%%expect{|
Line 1, characters 14-18:
1 | type t_void : void
                  ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}]

(***************************************)
(* Test 1: annotation on type variable *)

let x : int as ('a : value) = 5
let x : int as ('a : immediate) = 5
;;
[%%expect {|
val x : int = 5
Line 2, characters 21-30:
2 | let x : int as ('a : immediate) = 5
                         ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when [immediate] becomes available in [layouts] *)

let x : int as ('a : any) = 5;;

[%%expect{|
Line 1, characters 21-24:
1 | let x : int as ('a : any) = 5;;
                         ^^^
Error: Layout any is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when [any] becomes available in [layouts] *)

let x : (int as ('a : immediate)) list as ('b : value) = [3;4;5]
;;
[%%expect {|
Line 1, characters 22-31:
1 | let x : (int as ('a : immediate)) list as ('b : value) = [3;4;5]
                          ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let x : int list as ('a : immediate) = [3;4;5]
;;
[%%expect {|
Line 1, characters 26-35:
1 | let x : int list as ('a : immediate) = [3;4;5]
                              ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

(****************************************)
(* Test 2: Annotation on type parameter *)

(* CR layouts: move over beta tests once [immediate] is allowed in [layouts] *)

type ('a : value) t2
type (_ : value) t2'
type t3 = int t2
type t4 = bool t2
;;
[%%expect {|
type 'a t2
type _ t2'
type t3 = int t2
type t4 = bool t2
|}]

module M1 : sig
  type ('a : value) t
end = struct
  type (_ : value) t
end

module M2 : sig
  type (_ : value) t
end = struct
  type ('a : value) t
end

[%%expect {|
module M1 : sig type 'a t end
module M2 : sig type _ t end
|}]

type t = string t2
;;
[%%expect {|
type t = string t2
|}]

(********************************************)
(* Test 3: Annotation on types in functions *)

let f : ('a : any) -> 'a = fun x -> x
;;
[%%expect {|
Line 1, characters 14-17:
1 | let f : ('a : any) -> 'a = fun x -> x
                  ^^^
Error: Layout any is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f : ('a : any). 'a -> 'a = fun x -> x
;;
[%%expect {|
Line 1, characters 14-17:
1 | let f : ('a : any). 'a -> 'a = fun x -> x
                  ^^^
Error: Layout any is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when [any] becomes available in [layouts] *)

(********************************************)
(* Test 4: Annotation on record field types *)

type r = { field : ('a : immediate). 'a -> 'a }

[%%expect{|
Line 1, characters 25-34:
1 | type r = { field : ('a : immediate). 'a -> 'a }
                             ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when we allow annotations on field types in [layouts] *)

(********************)
(* Test 5: newtypes *)

let f = fun (type (a : value)) (x : a) -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f = fun (type (a : immediate)) (x : a) -> x
;;
[%%expect {|
Line 1, characters 23-32:
1 | let f = fun (type (a : immediate)) (x : a) -> x
                           ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f = fun (type (a : any)) (x : a) -> x
;;
[%%expect {|
Line 1, characters 23-26:
1 | let f = fun (type (a : any)) (x : a) -> x
                           ^^^
Error: Layout any is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when we allow annotations on newtypes in [layouts] *)

(****************************************)
(* Test 6: abstract universal variables *)

let f : type (a : value). a -> a = fun x -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f : type (a : immediate). a -> a = fun x -> x
;;
[%%expect {|
Line 1, characters 18-27:
1 | let f : type (a : immediate). a -> a = fun x -> x
                      ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f : type (a : any). a -> a = fun x -> x
;;
[%%expect {|
Line 1, characters 18-21:
1 | let f : type (a : any). a -> a = fun x -> x
                      ^^^
Error: Layout any is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when we allow annotations on newtypes in [layouts] *)

(**************************************************)
(* Test 7: Defaulting universal variable to value *)

(********************************************)
(* Test 8: Annotation on universal variable *)

module type S = sig
  val f : ('a : value). 'a t2 -> 'a t2
end
;;
[%%expect {|
module type S = sig val f : 'a t2 -> 'a t2 end
|}]

module type S = sig
  val f : 'a t2 -> 'a t2
  val g : ('a : immediate). 'a t2 -> 'a t2
end
;;
[%%expect {|
Line 3, characters 16-25:
3 |   val g : ('a : immediate). 'a t2 -> 'a t2
                    ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when we allow annotations on universals in [layouts] *)

(************************************************************)
(* Test 9: Annotation on universal in polymorphic parameter *)

let f (x : ('a : value). 'a -> 'a) = x "string", x 5

[%%expect {|
val f : ('a. 'a -> 'a) -> string * int = <fun>
|}]

let f (x : ('a : immediate). 'a -> 'a) = x "string"

[%%expect {|
Line 1, characters 17-26:
1 | let f (x : ('a : immediate). 'a -> 'a) = x "string"
                     ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
(* CR layouts: fix when we allow annotations on universals in [layouts] *)

(**************************************)
(* Test 10: Parsing & pretty-printing *)

let f (type a : immediate) (x : a) = x

[%%expect{|
Line 1, characters 16-25:
1 | let f (type a : immediate) (x : a) = x
                    ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f = fun (type a : immediate) (x : a) -> x

[%%expect{|
Line 1, characters 22-31:
1 | let f = fun (type a : immediate) (x : a) -> x
                          ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f = fun (type a : value) (x : a) -> x

[%%expect{|
val f : 'a -> 'a = <fun>
|}]

let o = object
  method m : type (a : immediate). a -> a = fun x -> x
end

[%%expect{|
Line 2, characters 23-32:
2 |   method m : type (a : immediate). a -> a = fun x -> x
                           ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f : type (a : immediate). a -> a = fun x -> x

[%%expect{|
Line 1, characters 18-27:
1 | let f : type (a : immediate). a -> a = fun x -> x
                      ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f x =
  let local_ g (type a : immediate) (x : a) = x in
  g x [@nontail]

[%%expect{|
Line 2, characters 25-34:
2 |   let local_ g (type a : immediate) (x : a) = x in
                             ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f = fun x y (type (a : immediate)) (z : a) -> z

[%%expect{|
Line 1, characters 27-36:
1 | let f = fun x y (type (a : immediate)) (z : a) -> z
                               ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f = fun x y (type a : immediate) (z : a) -> z

[%%expect{|
Line 1, characters 26-35:
1 | let f = fun x y (type a : immediate) (z : a) -> z
                              ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

external f : ('a : immediate). 'a -> 'a = "%identity"

[%%expect{|
Line 1, characters 19-28:
1 | external f : ('a : immediate). 'a -> 'a = "%identity"
                       ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

type (_ : any) t2_any

[%%expect{|
Line 1, characters 10-13:
1 | type (_ : any) t2_any
              ^^^
Error: Layout any is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn

[%%expect{|
Line 1, characters 20-29:
1 | exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn
                        ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f (x : ('a : immediate). 'a -> 'a) = x 3, x true

[%%expect {|
Line 1, characters 17-26:
1 | let f (x : ('a : immediate). 'a -> 'a) = x 3, x true
                     ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

type _ a = Mk : [> ] * ('a : immediate) -> int a

[%%expect {|
Line 1, characters 29-38:
1 | type _ a = Mk : [> ] * ('a : immediate) -> int a
                                 ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f_imm : ('a : immediate). 'a -> 'a = fun x -> x

[%%expect {|
Line 1, characters 18-27:
1 | let f_imm : ('a : immediate). 'a -> 'a = fun x -> x
                      ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

let f_val : ('a : value). 'a -> 'a = fun x -> x

[%%expect {|
val f_val : 'a -> 'a = <fun>
|}]

type (_ : value) g =
  | MkG : ('a : immediate). 'a g

[%%expect {|
Line 2, characters 16-25:
2 |   | MkG : ('a : immediate). 'a g
                    ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

type t = int as (_ : immediate)

[%%expect {|
Line 1, characters 21-30:
1 | type t = int as (_ : immediate)
                         ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]
