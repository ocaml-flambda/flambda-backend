(* TEST
 flags = "-extension layouts_beta";
 expect;
*)
(* This test is adapted from
   [testsuite/tests/typing-unboxed-types/test.ml] *)

(* Check the unboxing *)

(* For records *)
type t2 = #{ f : string } ;;
[%%expect{|
type t2 = #{ f : string; }
|}];;

let x = #{ f = "foo" } in
Obj.repr x == Obj.repr x.#f
;;
[%%expect{|
- : bool = true
|}];;

(* Representation mismatch between module and signature must be rejected *)
module M : sig
  type t = { a : string }
end = struct
  type t = #{ a : string }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ a : string }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ a : string; } end
       is not included in
         sig type t = { a : string; } end
       The type "t#" is required but not provided
|}];;

(* Check interference with representation of float arrays. *)
type t11 = #{ f : float };;
[%%expect{|
type t11 = #{ f : float; }
|}];;
let x = Array.make 10 #{ f = 3.14 }   (* represented as a flat array *)
and f (a : t11 array) = a.(0)    (* might wrongly assume an array of pointers *)
in assert (f x = #{ f = 3.14});;
[%%expect{|
- : unit = ()
|}];;

(* Check for a potential infinite loop in the typing algorithm. *)
type 'a t12 : value = #{ a : 'a t12 };;
[%%expect{|
type 'a t12 = #{ a : 'a t12; }
|}];;
let f (a : int t12 array) = a.(0);;
[%%expect{|
val f : int t12 array -> int t12 = <fun>
|}];;

(* should work *)
type t14;;
type t15 = #{ a : t14 };;
[%%expect{|
type t14
type t15 = #{ a : t14; }
|}];;

(* should fail because the compiler knows that t is actually float and
   optimizes the record's representation *)
module S : sig
  type t
  type u = { f1 : t; f2 : t }
end = struct
  type t = #{ a : float }
  type u = { f1 : t; f2 : t }
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = #{ a : float }
6 |   type u = { f1 : t; f2 : t }
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ a : float; } type u = { f1 : t; f2 : t; } end
       is not included in
         sig type t type u = { f1 : t; f2 : t; } end
       Type declarations do not match:
         type u = { f1 : t; f2 : t; }
       is not included in
         type u = { f1 : t; f2 : t; }
       Their internal representations differ:
       the first declaration uses unboxed float representation.
|}];;

(* implementing [@@immediate] with unboxed records: this works because the
   representation of [t] is [int]
 *)
module T : sig
  type t [@@immediate]
end = struct
  type t = #{ i : int }
end;;
[%%expect{|
module T : sig type t : immediate end
|}];;


(* MPR#7682 *)
type f = #{field: 'a. 'a list} ;;
let g = Array.make 10 #{ field=[] };;
let h = g.(5);;
[%%expect{|
type f = #{ field : 'a. 'a list; }
val g : f array =
  [|#{field = []}; #{field = []}; #{field = []}; #{field = []};
    #{field = []}; #{field = []}; #{field = []}; #{field = []};
    #{field = []}; #{field = []}|]
val h : f = #{field = []}
|}];;
