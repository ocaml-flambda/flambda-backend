(* TEST
   * expect
   flags = "-extension layouts"
   * expect
   flags = "-extension layouts_beta"
*)

type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_any : any;;

[%%expect{|
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_any : any
|}]

type t_void : void;;

[%%expect{|
Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}]

(***************************************)
(* Test 1: annotation on type variable *)

let x : int as ('a: value) = 5
let x : int as ('a : immediate) = 5
let x : int as ('a : any) = 5;;

[%%expect{|
val x : int = 5
val x : int = 5
val x : int = 5
|}]

let x : int as ('a : float64) = 5;;
[%%expect {|
Line 1, characters 8-29:
1 | let x : int as ('a : float64) = 5;;
            ^^^^^^^^^^^^^^^^^^^^^
Error: This alias is bound to type int but is used as an instance of type
         ('a : float64)
       int has layout immediate, which is not a sublayout of float64.
|}]

let x : (int as ('a : immediate)) list as ('b : value) = [3;4;5]
;;
[%%expect {|
val x : int list = [3; 4; 5]
|}]

let x : int list as ('a : immediate) = [3;4;5]
;;
[%%expect {|
Line 1, characters 8-36:
1 | let x : int list as ('a : immediate) = [3;4;5]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This alias is bound to type int list
       but is used as an instance of type ('a : immediate)
       int list has layout value, which is not a sublayout of immediate.
|}]
(* CR layouts: error message could be phrased better *)

(****************************************)
(* Test 2: Annotation on type parameter *)

type ('a : value) t2
type (_ : value) t2'
type t3 = int t2
type t4 = bool t2

[%%expect {|
type 'a t2
type _ t2'
type t3 = int t2
type t4 = bool t2
|}]

type t = string t2
;;
[%%expect {|
type t = string t2
|}]

type ('a : immediate) t2_imm
type (_ : immediate) t2_imm'
type t1 = int t2_imm
type t2' = bool t2_imm
type ('a : float64) t2_float64
type (_ : float64) t2_float64'
type t3 = float# t2_float64


[%%expect {|
type ('a : immediate) t2_imm
type (_ : immediate) t2_imm'
type t1 = int t2_imm
type t2' = bool t2_imm
type ('a : float64) t2_float64
type (_ : float64) t2_float64'
type t3 = float# t2_float64
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

module M1 : sig
  type ('a : immediate) t
end = struct
  type (_ : immediate) t
end

module M2 : sig
  type (_ : immediate) t
end = struct
  type ('a : immediate) t
end

[%%expect {|
module M1 : sig type ('a : immediate) t end
module M2 : sig type (_ : immediate) t end
|}]

type t = string t2_imm
;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = string t2_imm
             ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       string has layout value, which is not a sublayout of immediate.
|}]

let f : 'a t2_imm -> 'a t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : immediate) t2_imm -> ('a : value) t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : value) t2_imm -> ('a : value) t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : immediate). 'a t2_imm -> 'a t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : value). 'a t2_imm -> 'a t2_imm = fun x -> x
;;
[%%expect {|
Line 1, characters 8-44:
1 | let f : ('a : value). 'a t2_imm -> 'a t2_imm = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout value, but was inferred to have layout immediate.
|}]

type 'a t = 'a t2_imm
;;
[%%expect {|
type ('a : immediate) t = 'a t2_imm
|}]

type ('a : value) t = 'a t2_imm
;;
[%%expect {|
type ('a : immediate) t = 'a t2_imm
|}]

type ('a : immediate) t = 'a t2_imm
;;
[%%expect {|
type ('a : immediate) t = 'a t2_imm
|}]

let f : (_ : value) t2_imm -> unit = fun _ -> ()
let g : (_ : immediate) t2_imm -> unit = fun _ -> ()

[%%expect {|
val f : ('a : immediate). 'a t2_imm -> unit = <fun>
val g : ('a : immediate). 'a t2_imm -> unit = <fun>
|}]

let f : (_ : immediate) -> unit = fun _ -> ()
let g : (_ : value) -> unit = fun _ -> ()

[%%expect {|
val f : ('a : immediate). 'a -> unit = <fun>
val g : 'a -> unit = <fun>
|}]

let f : (_ : immediate) -> (_ : value) = fun _ -> assert false
let g : (_ : value) -> (_ : immediate) = fun _ -> assert false

[%%expect {|
val f : 'b ('a : immediate). 'a -> 'b = <fun>
val g : ('b : immediate) 'a. 'a -> 'b = <fun>
|}]

(********************************************)
(* Test 3: Annotation on types in functions *)

let f : ('a : any) -> 'a = fun x -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f : ('a : any). 'a -> 'a = fun x -> x
;;
[%%expect {|
Line 1, characters 8-28:
1 | let f : ('a : any). 'a -> 'a = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout any, but was inferred to have a representable layout.
|}]
(* CR layouts v2.5: This error message should change to complain
   about the [fun x], not the arrow type. *)

let f : ('a : float64). 'a -> 'a = fun x -> x
;;
[%%expect {|
val f : ('a : float64). 'a -> 'a = <fun>
|}]

(********************************************)
(* Test 4: Annotation on record field types *)

type r = { field : ('a : immediate). 'a -> 'a }
let f { field } = field 5
;;
[%%expect {|
type r = { field : ('a : immediate). 'a -> 'a; }
val f : r -> int = <fun>
|}]

type rf = { fieldf : ('a : float64). 'a -> 'a }
let f { fieldf } = fieldf (Stdlib__Float_u.of_float 3.14);;
[%%expect {|
type rf = { fieldf : ('a : float64). 'a -> 'a; }
val f : rf -> float# = <fun>
|}]

let f { field } = field "hello"
;;
[%%expect {|
Line 1, characters 24-31:
1 | let f { field } = field "hello"
                            ^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       string has layout value, which is not a sublayout of immediate.
|}]

let r = { field = fun x -> x }
let r = { field = Fun.id }
;;
[%%expect {|
val r : r = {field = <fun>}
val r : r = {field = <fun>}
|}]

let r = { field = fun (type (a : immediate)) (x : a) -> x }
;;
[%%expect {|
val r : r = {field = <fun>}
|}]

let r = { field = fun (type (a : value)) (x : a) -> x }
;;
[%%expect {|
val r : r = {field = <fun>}
|}]

type r_value = { field : 'a. 'a -> 'a }
let r = { field = fun (type a : immediate) (x : a) -> x }

[%%expect{|
type r_value = { field : 'a. 'a -> 'a; }
Line 2, characters 18-55:
2 | let r = { field = fun (type a : immediate) (x : a) -> x }
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This field value has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       'a has layout value, which is not a sublayout of immediate.
|}]
(* CR layouts v1.5: that's a pretty awful error message *)

type ('a : immediate) t_imm

type s = { f : ('a : value). 'a -> 'a u }
and 'a u = 'a t_imm

[%%expect{|
type ('a : immediate) t_imm
Line 3, characters 15-39:
3 | type s = { f : ('a : value). 'a -> 'a u }
                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type 'a has layout value, which is not a sublayout of immediate.
|}]
(* CR layouts v1.5: the location on that message is wrong. But it's hard
   to improve, because it comes from re-checking typedtree, where we don't
   have locations any more. I conjecture the same location problem exists
   when constraints aren't satisfied. *)

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
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun (type (a : float64)) (x : a) -> x
;;
[%%expect {|
val f : ('a : float64). 'a -> 'a = <fun>
|}]

let f = fun (type (a : any)) (x : a) -> x
;;
[%%expect {|
Line 1, characters 29-36:
1 | let f = fun (type (a : any)) (x : a) -> x
                                 ^^^^^^^
Error: This pattern matches values of type a
       but a pattern was expected which matches values of type
         ('a : '_representable_layout_1)
       a has layout any, which is not representable.
|}]

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
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f : type (a : float64). a -> a = fun x -> x
;;
[%%expect {|
val f : ('a : float64). 'a -> 'a = <fun>
|}]

let f : type (a : any). a -> a = fun x -> x
;;
[%%expect {|
Line 1, characters 4-43:
1 | let f : type (a : any). a -> a = fun x -> x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout any, but was inferred to have a representable layout.
|}]
(* CR layouts v2.5: This error message will change to complain
   about the fun x, not the arrow type. *)

(**************************************************)
(* Test 7: Defaulting universal variable to value *)

module type S = sig
  val f : 'a. 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
Line 2, characters 10-36:
2 |   val f : 'a. 'a t2_imm -> 'a t2_imm
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have
       layout value, but was inferred to have layout immediate.
|}]

let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x

[%%expect {|
Line 1, characters 8-34:
1 | let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have
       layout value, but was inferred to have layout immediate.
|}]

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
  val f : ('a : value). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
Line 2, characters 10-46:
2 |   val f : ('a : value). 'a t2_imm -> 'a t2_imm
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout value, but was inferred to have layout immediate.
|}]

module type S = sig
  val f : 'a t2_imm -> 'a t2_imm
  val g : ('a : immediate). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
module type S =
  sig
    val f : ('a : immediate). 'a t2_imm -> 'a t2_imm
    val g : ('a : immediate). 'a t2_imm -> 'a t2_imm
  end
|}]

module type S = sig
  val f : 'a t2_float64 -> 'a t2_float64
  val g : ('a : float64). 'a t2_float64 -> 'a t2_float64
end
;;
[%%expect {|
module type S =
  sig
    val f : ('a : float64). 'a t2_float64 -> 'a t2_float64
    val g : ('a : float64). 'a t2_float64 -> 'a t2_float64
  end
|}]

(************************************************************)
(* Test 9: Annotation on universal in polymorphic parameter *)

let f (x : ('a : value). 'a -> 'a) = x "string", x 5

[%%expect {|
val f : ('a. 'a -> 'a) -> string * int = <fun>
|}]


let f (x : ('a : immediate). 'a -> 'a) = x "string"

[%%expect {|
Line 1, characters 43-51:
1 | let f (x : ('a : immediate). 'a -> 'a) = x "string"
                                               ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       string has layout value, which is not a sublayout of immediate.
|}]

(**************************************)
(* Test 10: Parsing & pretty-printing *)

let f (type a : immediate) (x : a) = x

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun (type a : immediate) (x : a) -> x

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun (type a : value) (x : a) -> x

[%%expect{|
val f : 'a -> 'a = <fun>
|}]

let o = object
  method m : type (a : immediate). a -> a = fun x -> x
end

[%%expect{|
val o : < m : ('a : immediate). 'a -> 'a > = <obj>
|}]

let f : type (a : immediate). a -> a = fun x -> x

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f x =
  let local_ g (type a : immediate) (x : a) = x in
  g x [@nontail]

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun x y (type (a : immediate)) (z : a) -> z

[%%expect{|
val f : ('a : immediate) 'c 'b. 'b -> 'c -> 'a -> 'a = <fun>
|}]

let f = fun x y (type a : immediate) (z : a) -> z

[%%expect{|
val f : ('a : immediate) 'c 'b. 'b -> 'c -> 'a -> 'a = <fun>
|}]
(* CR layouts: canonicalizing the order of quantification here
   would reduce wibbles in error messages *)

external f : ('a : immediate). 'a -> 'a = "%identity"

[%%expect{|
external f : ('a : immediate). 'a -> 'a = "%identity"
|}]


type (_ : any) t2_any
exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn

[%%expect{|
type (_ : any) t2_any
exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn
|}]


let f (x : ('a : immediate). 'a -> 'a) = x 3, x true

[%%expect{|
val f : (('a : immediate). 'a -> 'a) -> int * bool = <fun>
|}]

type _ a = Mk : [> ] * ('a : immediate) -> int a

[%%expect {|
type _ a = Mk : ('a : immediate). [>  ] * 'a -> int a
|}]

let f_imm : ('a : immediate). 'a -> 'a = fun x -> x

[%%expect {|
val f_imm : ('a : immediate). 'a -> 'a = <fun>
|}]

let f_val : ('a : value). 'a -> 'a = fun x -> f_imm x

[%%expect {|
Line 1, characters 37-53:
1 | let f_val : ('a : value). 'a -> 'a = fun x -> f_imm x
                                         ^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       'a has layout value, which is not a sublayout of immediate.
|}]

type (_ : value) g =
  | MkG : ('a : immediate). 'a g

[%%expect {|
type _ g = MkG : ('a : immediate). 'a g
|}]

type t = int as (_ : immediate)

[%%expect {|
type t = int
|}]
