(* TEST
   flags = "-extension-universe alpha";
   expect;
*)

(* This file includes examples of all of the syntax that we at Jane Street
   have implemented in the compiler. It should be updated every time we
   add new syntax.

   When we upstream a feature, its example should be removed from here.
*)

(******************)
(* comprehensions *)

(* list comprehension *)
let nums =
  [ n, m, x
    for n = 1 to 10
    and m = 5 downto 0
    when (n + m) mod 3 = 0
    for x in [1 + m; 2; 3]
    when (n + m + x) mod 2 = 0
  ]

[%%expect{|
val nums : (int * int * int) list @@ global many =
  [(1, 5, 6); (1, 5, 2); (1, 2, 3); (1, 2, 3); (2, 4, 2); (2, 1, 3);
   (3, 3, 4); (3, 3, 2); (3, 0, 1); (3, 0, 3); (4, 5, 3); (4, 2, 2);
   (5, 4, 5); (5, 4, 3); (5, 1, 2); (5, 1, 2); (6, 3, 3); (6, 0, 2);
   (7, 5, 6); (7, 5, 2); (7, 2, 3); (7, 2, 3); (8, 4, 2); (8, 1, 3);
   (9, 3, 4); (9, 3, 2); (9, 0, 1); (9, 0, 3); (10, 5, 3); (10, 2, 2)]
|}]

(* array comprehension: note that the "in" clause must iterate over
   an array, not a list *)
let nums =
  [| n, m, x
    for n = 1 to 10
    and m = 5 downto 0
    when (n + m) mod 3 = 0
    for x in [| 1 + m; 2; 3 |]
    when (n + m + x) mod 2 = 0
  |]

[%%expect{|
val nums : (int * int * int) array @@ global many =
  [|(1, 5, 6); (1, 5, 2); (1, 2, 3); (1, 2, 3); (2, 4, 2); (2, 1, 3);
    (3, 3, 4); (3, 3, 2); (3, 0, 1); (3, 0, 3); (4, 5, 3); (4, 2, 2);
    (5, 4, 5); (5, 4, 3); (5, 1, 2); (5, 1, 2); (6, 3, 3); (6, 0, 2);
    (7, 5, 6); (7, 5, 2); (7, 2, 3); (7, 2, 3); (8, 4, 2); (8, 1, 3);
    (9, 3, 4); (9, 3, 2); (9, 0, 1); (9, 0, 3); (10, 5, 3); (10, 2, 2)|]
|}]

let broken_local =
  [ 5 for local_ n in [ 1; 2 ] ]

[%%expect{|
Line 2, characters 10-30:
2 |   [ 5 for local_ n in [ 1; 2 ] ]
              ^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* This doesn't parse:
let more_broken =
  [ 5 for local_ n = 1 to 5 ]
*)

(*********)
(* modes *)

let f ~(x1 @ many)
      ~(x2 : string @@ local)
      ~(x3 : string -> string @@ local)
      ~(x4 : 'a. 'a -> 'a @@ local)
      ~(local_ x5)
      ~x6:(local_ true | false @ many)
      ~x7:(local_ true | false : bool @@ many)
      ~x8:(local_ ())
      (_ : string @@ global)
      (_ @ local) =
  exclave_
  local_
  let x9 @ local = "hi" in
  let x10 : string @@ global = "hi" in
  let local_ x11 : int array @@ contended = [| |] in
  let x12 : type a b c. a -> (b -> a) @ local @@ local many = fun x _ -> x in
  let (x13 @ global) = "hi" in
  let (x14 @ local) y z = y + z in
  let (x15 @ local) y z : int = y + z in
  (* NO: let (x15 @ local) y z : int @@ local = y + z in *)
  let _ = ("hi" : string @@ global) in
  x1, x2, x3, x4, x5, x9, x10, x11, x12, x13, x14, x15

[%%expect{|
val f :
  ('b : value_or_null) ('c : value_or_null) ('d : value_or_null) 'e 'f 'g.
    x1:'b ->
    x2:local_ string ->
    x3:local_ (string -> string) ->
    x4:local_ ('a. 'a -> 'a) ->
    x5:local_ 'c ->
    x6:local_ bool ->
    x7:local_ bool ->
    x8:local_ unit ->
    string ->
    local_ 'd -> local_
    'b * string * (string -> string) * ('e -> 'e) * 'c * string * string *
    int array * ('f -> local_ ('g -> 'f)) * string *
    (int -> local_ (int -> int)) * (int -> local_ (int -> int)) @ contended
  @@ global many = <fun>
|}]

let f1 (_ @ local) = ()
let f2 () = let x @ local = [1; 2; 3] in f1 x [@nontail]

[%%expect{|
val f1 : ('a : value_or_null). local_ 'a -> unit @@ global many = <fun>
val f2 : unit -> unit @@ global many = <fun>
|}]

module type S = sig
  module type S0 = sig
    val x1 : string -> string @@ local
    val x2 : string -> string @@ portable
    val x3 : string -> string @@ nonportable
  end
  include S0 @@ portable many local
end

[%%expect{|
module type S =
  sig
    module type S0 =
      sig
        val x1 : string -> string
        val x2 : string -> string @@ portable
        val x3 : string -> string
      end
    val x1 : string -> string @@ many portable
    val x2 : string -> string @@ many portable
    val x3 : string -> string @@ many portable
  end
|}]

external x4 : string -> string @@ portable many = "%identity"

[%%expect{|
external x4 : string -> string @@ many portable = "%identity"
|}]

type t =
  | K1 of string @@ global * (float -> float) @@ many * string
  | K2 : string @@ global * (float -> float) @@ many * string -> t

[%%expect{|
type t =
    K1 of global_ string * (float -> float) @@ many * string
  | K2 : global_ string * (float -> float) @@ many * string -> t
|}]

type t = { x : string @@ global
         ; mutable y : float -> float @@ many
         ; global_ z : string @@ many }

[%%expect{|
type t = {
  global_ x : string;
  mutable y : float -> float;
  global_ z : string @@ many;
}
|}]

type t1 = { mutable x : float
          ; mutable f : float -> float }

type t2 = { mutable x : float [@no_mutable_implied_modalities]
          ; mutable f : float -> float [@no_mutable_implied_modalities] }

[%%expect{|
type t1 = { mutable x : float; mutable f : float -> float; }
type t2 = { mutable x : float; mutable f : float -> float; }
|}]

let f1 (x @ local) (f @ once) : t1 = exclave_ { x; f }

[%%expect{|
Line 1, characters 48-49:
1 | let f1 (x @ local) (f @ once) : t1 = exclave_ { x; f }
                                                    ^
Error: This value escapes its region.
|}]

let f2 (x @ local) (f @ once) : t2 = exclave_ { x; f }

[%%expect{|
val f2 : local_ float -> once_ (float -> float) -> local_ once_ t2 @@ global
  many = <fun>
|}]


(**********)
(* unique *)

(* No syntax; the unique extension just enables the uniqueness checker. *)

(*******************)
(* include_functor *)

module type F = functor (X : sig end) -> sig end

[%%expect{|
module type F = functor (X : sig end) -> sig end
|}]

module rec M : sig
  include functor F
end = struct end

[%%expect{|
Line 2, characters 2-19:
2 |   include functor F
      ^^^^^^^^^^^^^^^^^
Error: Including a functor is not supported in recursive module signatures
|}]

module type S = sig
  include functor F
end

[%%expect{|
module type S = sig end
|}]

module F (X : sig end) = struct end

[%%expect{|
module F : functor (X : sig end) -> sig end
|}]

module M = struct
  include functor F
end

[%%expect{|
module M : sig end
|}]

(**************************)
(* polymorphic_parameters *)

type t = ('a. 'a -> 'a) -> int

[%%expect{|
type t = ('a. 'a -> 'a) -> int
|}]

let f (_ : 'a. 'a -> 'a) = ()

[%%expect{|
val f : ('a. 'a -> 'a) -> unit @@ global many = <fun>
|}]

(********************)
(* immutable_arrays *)

let nums =
  [: x, x for x in [: 1; 2; 3 :] :]

[%%expect{|
val nums : (int * int) iarray @@ global many = [:(1, 1); (2, 2); (3, 3):]
|}]

let nums = [: 1; 2; 3 :]

[%%expect{|
val nums : int iarray @@ global many = [:1; 2; 3:]
|}]

let f xs = match xs with
  | [: _; _ :] -> 2
  | [: :] -> 0
  | _ -> 100

[%%expect{|
val f : 'a iarray -> int @@ global many = <fun>
|}]

(************************)
(* module_strengthening *)

module type S = sig
  type t1
  type t2
  type t3
end

module M = struct
  type t1 = int
  type t2 = K of string
  type t3
end

[%%expect{|
module type S = sig type t1 type t2 type t3 end
module M : sig type t1 = int type t2 = K of string type t3 end
|}]

module type S2 = S with M

[%%expect{|
module type S2 = sig type t1 = M.t1 type t2 = M.t2 type t3 = M.t3 end
|}]

(***********)
(* layouts *)

let f () = #3.14

[%%expect{|
val f : unit -> float# @@ global many = <fun>
|}]

let f1 () = #1L
let f2 () = #2l
let f3 () = #3n

[%%expect{|
val f1 : unit -> int64# @@ global many = <fun>
val f2 : unit -> int32# @@ global many = <fun>
val f3 : unit -> nativeint# @@ global many = <fun>
|}]

let f () = #4s

[%%expect{|
Line 1, characters 11-14:
1 | let f () = #4s
               ^^^
Error: Unknown modifier "s" for literal "#4s"
|}]

let f1 (type a : float64) (x : a) : a = x
let f2 (type a : bits64) = fun x -> (x : a)

[%%expect{|
val f1 : ('a : float64). 'a -> 'a @@ global many = <fun>
val f2 : ('a : bits64). 'a -> 'a @@ global many = <fun>
|}]


let f1 x = match x with
  | #42n -> 0
  | _ -> 3

[%%expect{|
val f1 : nativeint# -> int @@ global many = <fun>
|}]

let f : ('a : float64) -> (_ : bits64) -> 'a = fun x _ -> x

[%%expect{|
val f : ('a : float64) ('b : bits64). 'a -> 'b -> 'a @@ global many = <fun>
|}]

let f : ('a : float64). 'a -> 'a = fun x -> x

[%%expect{|
val f : ('a : float64). 'a -> 'a @@ global many = <fun>
|}]

let f : (('a : float64). 'a -> 'a) -> int = fun _ -> 5

[%%expect{|
val f : (('a : float64). 'a -> 'a) -> int @@ global many = <fun>
|}]

let f : (unit as (_ : immediate)) -> unit = fun () -> ()

[%%expect{|
val f : unit -> unit @@ global many = <fun>
|}]

type (_ : any) ignore = K1
type t = ..
type t += K : ('a : float64). 'a ignore -> t

[%%expect{|
type (_ : any) ignore = K1
type t = ..
type t += K : ('a : float64). 'a ignore -> t
|}]

module M : sig
  kind_abbrev_ k = immediate
end = struct
  kind_abbrev_ k = immediate
end

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

type t = K : ('a : float64). 'a ignore -> t

[%%expect{|
type t = K : ('a : float64). 'a ignore -> t
|}]

type t : float64 = float#
type t2 : bits64

[%%expect{|
type t = float#
type t2 : bits64
|}]

type t1 : any
type t2 : any_non_null
type t3 : value_or_null
type t4 : value
type t5 : void
type t6 : immediate64
type t7 : immediate
type t8 : float64
type t9 : float32
type t10 : word
type t11 : bits32
type t12 : bits64

[%%expect{|
type t1 : any
type t2 : any_non_null
type t3 : value_or_null
type t4
type t5 : void
type t6 : immediate64
type t7 : immediate
type t8 : float64
type t9 : float32
type t10 : word
type t11 : bits32
type t12 : bits64
|}]

type t = #(int * float#)

[%%expect{|
type t = #(int * float#)
|}]

let f xs = match xs with
  | #(x1, (y1 : float#)) -> #(x1, y1)

[%%expect{|
val f : ('a : value_or_null). #('a * float#) -> #('a * float#) @@ global many =
  <fun>
|}]

type t = { x: string
         ; y: float#
         ; z: int }

type ('a : immediate) t2 = { x : string
                           ; y : float#
                           ; z : 'a }

[%%expect{|
type t = { x : string; y : float#; z : int; }
type ('a : immediate) t2 = { x : string; y : float#; z : 'a; }
|}]

type 'a t = 'a or_null

[%%expect{|
type 'a t = 'a or_null
|}]

module M = struct
  type 'a t = 'a or_null [@@or_null_reexport]
end

let x () = #( M.Null, M.This "hi" )

[%%expect{|
module M : sig type 'a t = 'a or_null = Null | This of 'a end
val x : unit -> #('a M.t * string M.t) @@ global many = <fun>
|}]

external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]

[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
|}]

(********)
(* simd *)
(* CR mslater: Move documentation to GitHub *)

type t1 = int8x16
type t2 = int16x8
type t3 = int32x4
type t4 = int64x2
type t5 = float32x4
type t6 = float64x2

[%%expect{|
type t1 = int8x16
type t2 = int16x8
type t3 = int32x4
type t4 = int64x2
type t5 = float32x4
type t6 = float64x2
|}]

(*****************)
(* small_numbers *)

type t1 = float32
type t2 = float32#

[%%expect{|
type t1 = float32
type t2 = float32#
|}]

let x = 3.14s

[%%expect{|
val x : float32 @@ global many = 3.1400001s
|}]

let x () = #3.14s

[%%expect{|
val x : unit -> float32# @@ global many = <fun>
|}]

(******************)
(* labeled_tuples *)


type t = x:int * y:string

[%%expect{|
type t = x:int * y:string
|}]

let f x z = ~x, ~y:"hi", ~(z : int), ~x:4.15, 8, "weird", ~x:10

[%%expect{|
val f :
  ('a : value_or_null).
    'a -> int -> x:'a * y:string * z:int * x:float * int * string * x:int
  @@ global many = <fun>
|}]

let f xs = match (xs : x:int * y:string * z:float) with
  | ~y:"hello", ~z, ~x -> ignore (x, z); 0
  | ~z, .. -> ignore z; 1

[%%expect{|
val f : (x:int * y:string * z:float) -> int @@ global many = <fun>
|}]

(**************)
(* zero_alloc *)

(* CR gyorsh or ccasinghino: Add examples *)
(* CR gyorsh or ccasinghino: Add documentation to GitHub *)

(*****************)
(* error_message *)

let f (x : bool) = (x : int)[@error_message "custom message"]

[%%expect{|
Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message "custom message"]
                        ^
Error: This expression has type "bool" but an expression was expected of type
         "int"
       custom message
|}]

let f (v : float#) : ((_ : value)[@error_message "need a value"]) = v

[%%expect{|
Line 1, characters 68-69:
1 | let f (v : float#) : ((_ : value)[@error_message "need a value"]) = v
                                                                        ^
Error: This expression has type "float#" but an expression was expected of type
         "('a : value)"
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because of the annotation on the wildcard _ at line 1, characters 22-33.
         need a value
|}]


