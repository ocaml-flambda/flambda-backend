(* TEST
 flags = "-extension layouts_beta";
 expect;
*)


(*********************************)
(* Basic typechecking of indices *)

type r = { i : int; j : int }
type t = (r# array, r#) imm_idx
[%%expect{|
type r = { i : int; j : int; }
type t = (r# array, r#) imm_idx
|}]

(* Disambiguation of block access field *)

type t1 = { mutable a : string; b : int }
type t2 = { mutable a : string; b : int; c : string }

let a2 () = (.a)
let b2 () = (.b)
let a1 () : (t1, _) mut_idx = (.a)
let b2 () : (t1, _) imm_idx = (.b)
[%%expect{|
type t1 = { mutable a : string; b : int; }
type t2 = { mutable a : string; b : int; c : string; }
val a2 : unit -> (t2, string) mut_idx = <fun>
val b2 : unit -> (t2, int) imm_idx = <fun>
val a1 : unit -> (t1, string) mut_idx = <fun>
val b2 : unit -> (t1, int) imm_idx = <fun>
|}]

(* Still disambiguates through a Tpoly *)
let a1 =
  let a1 : 'a. (t1, _) mut_idx = (.a) in
  fun () -> a1
[%%expect{|
val a1 : unit -> (t1, string) mut_idx = <fun>
|}]

(* Disambiguate by alias to imm_idx types *)
type ('c, 'b, 'a) mi = ('a, 'b) mut_idx
type ('c, 'b, 'a) i = ('a, 'b) imm_idx
let a () : (t1, _) mut_idx = (.a)
let b () : (float, int, t1) i = (.b)
[%%expect{|
type ('c, 'b, 'a) mi = ('a, 'b) mut_idx
type ('c, 'b, 'a) i = ('a, 'b) imm_idx
val a : unit -> (t1, string) mut_idx = <fun>
val b : unit -> (t1, int) imm_idx = <fun>
|}]

(* Block access disambiguates the unboxed access *)
type u = #{ x : int }
type u2 = #{ x : string }
type 'a r = { u : u }
type 'a r2 = { u : u2 }
let f () : (_ r, _) imm_idx = (.u.#x)
[%%expect{|
type u = #{ x : int; }
type u2 = #{ x : string; }
type 'a r = { u : u; }
type 'a r2 = { u : u2; }
val f : unit -> ('a r, int) imm_idx = <fun>
|}]

(* Unboxed access disambiguates the next unboxed access *)
type wrap_r = { r : int r# }
let f () = (.r.#u.#x)
[%%expect{|
type wrap_r = { r : int r#; }
val f : unit -> (wrap_r, int) imm_idx = <fun>
|}]

(* Disambiguation causes earlier error while typechecking block access *)
type y = { y : int }
type 'a t = { a : 'a }
let f c = if c then
    ((.a.#y) : (y# t, int) imm_idx)
  else
    (.a.#a)
[%%expect{|
type y = { y : int; }
type 'a t = { a : 'a; }
Line 6, characters 4-11:
6 |     (.a.#a)
        ^^^^^^^
Error: This expression has type "('a t# t, 'a) imm_idx"
       but an expression was expected of type "(y# t, int) imm_idx"
       Type "'a t#" is not compatible with type "y#"
|}]

(***************)
(* Type errors *)

type pt = { x : int }
let f () = (.x.#x)
[%%expect{|
type pt = { x : int; }
Line 99, characters 16-17:
99 | let f () = (.x.#x)
                     ^
Error: The index preceding this unboxed access has element type "int",
       which is not an unboxed record with field "x".
|}]

type 'a t = { t : 'a }
let f () = (.t.#t)
[%%expect{|
type 'a t = { t : 'a; }
val f : unit -> ('a t# t, 'a) imm_idx = <fun>
|}]

let f () : (int t, _) imm_idx = (.t.#t)
[%%expect{|
Line 1, characters 32-39:
1 | let f () : (int t, _) imm_idx = (.t.#t)
                                    ^^^^^^^
Error: This expression has type "('a t# t, 'a) imm_idx"
       but an expression was expected of type "(int t, 'b) imm_idx"
       Type "'a t#" is not compatible with type "int"
|}]

(**********)
(* Arrays *)

let idx_array x = (.idx_array(x))
let idx_array_L x = (.idx_array_L(x))
let idx_array_l x = (.idx_array_l(x))
let idx_array_n x = (.idx_array_n(x))
let idx_iarray x = (.idx_iarray(x))
let idx_iarray_L x = (.idx_iarray_L(x))
let idx_iarray_l x = (.idx_iarray_l(x))
let idx_iarray_n x = (.idx_iarray_n(x))
[%%expect{|
val idx_array : int -> ('a array, 'a) mut_idx = <fun>
val idx_array_L : int64# -> ('a array, 'a) mut_idx = <fun>
val idx_array_l : int32# -> ('a array, 'a) mut_idx = <fun>
val idx_array_n : nativeint# -> ('a array, 'a) mut_idx = <fun>
val idx_iarray : int -> ('a iarray, 'a) imm_idx = <fun>
val idx_iarray_L : int64# -> ('a iarray, 'a) imm_idx = <fun>
val idx_iarray_l : int32# -> ('a iarray, 'a) imm_idx = <fun>
val idx_iarray_n : nativeint# -> ('a iarray, 'a) imm_idx = <fun>
|}]

type r = { a : string }
let a () = (.idx_array(5).#contents.#a)
[%%expect{|
type r = { a : string; }
val a : unit -> (r# ref# array, string) mut_idx = <fun>
|}]

type t = { mutable a : string; b : int }
let a () = (.idx_array(5).#a)
[%%expect{|
type t = { mutable a : string; b : int; }
val a : unit -> (t# array, string) mut_idx = <fun>
|}]

type t1 = { mutable a : string; b : int }
let b () = (.idx_iarray(5).#a)
[%%expect{|
type t1 = { mutable a : string; b : int; }
val b : unit -> (t1# iarray, string) imm_idx = <fun>
|}]

let bad_index_type = (.idx_array("test"))
[%%expect{|
Line 169, characters 33-39:
169 | let bad_index_type = (.idx_array("test"))
                                       ^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]

(****************)
(* Principality *)

(* We get a principality warning when the block index type is disambiguated
   non-principally. *)
type u = #{ x : int }
type 'a r = { u : u }
type 'a r2 = { u : u }
let f c =
  if c then
    ((.u.#x) : (_ r, _) imm_idx)
  else
    (.u.#x)
[%%expect{|
type u = #{ x : int; }
type 'a r = { u : u; }
type 'a r2 = { u : u; }
val f : bool -> ('a r, int) imm_idx = <fun>
|}, Principal{|
type u = #{ x : int; }
type 'a r = { u : u; }
type 'a r2 = { u : u; }
Line 190, characters 6-7:
190 |     (.u.#x)
            ^
Warning 18 [not-principal]: this type-based field disambiguation is not principal.

val f : bool -> ('a r, int) imm_idx = <fun>
|}]
