(* TEST
   * expect
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
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
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
       The layout of int is immediate, because
         it is the primitive immediate type int.
       But the layout of int must be a sublayout of float64, because
         of the annotation on the type variable 'a.
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
       The layout of int list is value, because
         it's a boxed variant type.
       But the layout of int list must be a sublayout of immediate, because
         of the annotation on the type variable 'a.
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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of t2_imm at line 1, characters 0-28.
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
Error: The universal type variable 'a was declared to have layout value.
       But it was inferred to have layout immediate, because
         of the definition of t2_imm at line 1, characters 0-28.
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
val f : ('a : immediate) 'b. 'a -> 'b = <fun>
val g : 'a ('b : immediate). 'a -> 'b = <fun>
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
Line 1, characters 31-41:
1 | let f : ('a : any). 'a -> 'a = fun x -> x
                                   ^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         ('a : any). 'a -> 'a
       The layout of 'a is any, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be representable, because
         it's the type of a function argument.
|}]
(* CR layouts v2.9: This error message is not great. Check later if layout history
   is able to improve it. *)

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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of r at line 1, characters 0-47.
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
       The layout of 'a is value, because
         of the definition of r_value at line 1, characters 0-39.
       But the layout of 'a must be a sublayout of immediate, because
         of the annotation on the abstract type declaration for a.
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
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is value, because
           of the annotation on the universal variable 'a.
         But the layout of 'a must be a sublayout of immediate, because
           of the definition of t_imm at line 1, characters 0-27.
       The fix will likely be to add a layout annotation on a parameter to
       the declaration where this error is reported.
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
       The layout of a is any, because
         of the annotation on the abstract type declaration for a.
       But the layout of a must be representable, because
         it's the type of a function argument.
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
Line 1, characters 33-43:
1 | let f : type (a : any). a -> a = fun x -> x
                                     ^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any, because
         of the annotation on the abstract type declaration for a.
       But the layout of a must be representable, because
         it's the type of a function argument.
|}]

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
Error: The universal type variable 'a was defaulted to have layout value.
       But it was inferred to have layout immediate, because
         of the definition of t2_imm at line 1, characters 0-28.
|}]

let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x

[%%expect {|
Line 1, characters 8-34:
1 | let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have layout value.
       But it was inferred to have layout immediate, because
         of the definition of t2_imm at line 1, characters 0-28.
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
Error: The universal type variable 'a was declared to have layout value.
       But it was inferred to have layout immediate, because
         of the definition of t2_imm at line 1, characters 0-28.
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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the annotation on the universal variable 'a.
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
val f : 'b 'c ('a : immediate). 'b -> 'c -> 'a -> 'a = <fun>
|}]

let f = fun x y (type a : immediate) (z : a) -> z

[%%expect{|
val f : 'b 'c ('a : immediate). 'b -> 'c -> 'a -> 'a = <fun>
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
exception E : ('b : any) ('a : immediate). 'b t2_any * 'a list -> exn
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
       The layout of 'a is value, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be a sublayout of immediate, because
         of the definition of f_imm at line 1, characters 4-9.
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
