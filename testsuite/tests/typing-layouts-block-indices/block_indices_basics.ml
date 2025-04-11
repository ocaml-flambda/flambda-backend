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
Line 2, characters 24-31:
2 | type t = (r# array, r#) imm_idx
                            ^^^^^^^
Error: Unbound type constructor "imm_idx"
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
val a2 : unit -> (t2, string) idx_mut = <fun>
val b2 : unit -> (t2, int) idx_imm = <fun>
Line 6, characters 20-27:
6 | let a1 () : (t1, _) mut_idx = (.a)
                        ^^^^^^^
Error: Unbound type constructor "mut_idx"
|}]

(* Still disambiguates through a Tpoly *)
let a1 =
  let a1 : 'a. (t1, _) mut_idx = (.a) in
  fun () -> a1
[%%expect{|
Line 2, characters 23-30:
2 |   let a1 : 'a. (t1, _) mut_idx = (.a) in
                           ^^^^^^^
Error: Unbound type constructor "mut_idx"
|}]

(* Disambiguate by alias to imm_idx types *)
type ('c, 'b, 'a) mi = ('a, 'b) mut_idx
type ('c, 'b, 'a) i = ('a, 'b) imm_idx
let a () : (t1, _) mut_idx = (.a)
let b () : (float, int, t1) i = (.b)
[%%expect{|
Line 1, characters 32-39:
1 | type ('c, 'b, 'a) mi = ('a, 'b) mut_idx
                                    ^^^^^^^
Error: Unbound type constructor "mut_idx"
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
Line 5, characters 20-27:
5 | let f () : (_ r, _) imm_idx = (.u.#x)
                        ^^^^^^^
Error: Unbound type constructor "imm_idx"
|}]

(* Unboxed access disambiguates the next unboxed access *)
type wrap_r = { r : int r# }
let f () = (.r.#u.#x)
[%%expect{|
type wrap_r = { r : int r#; }
val f : unit -> (wrap_r, int) idx_imm = <fun>
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
Line 4, characters 27-34:
4 |     ((.a.#y) : (y# t, int) imm_idx)
                               ^^^^^^^
Error: Unbound type constructor "imm_idx"
|}]

(***************)
(* Type errors *)

type pt = { x : int }
let f () = (.x.#x)
[%%expect{|
type pt = { x : int; }
Line 108, characters 16-17:
108 | let f () = (.x.#x)
                      ^
Error: The index preceding this unboxed access has element type "int",
       which is not an unboxed record with field "x".
|}]

type 'a t = { t : 'a }
let f () = (.t.#t)
[%%expect{|
type 'a t = { t : 'a; }
val f : unit -> ('a t# t, 'a) idx_imm = <fun>
|}]

let f () : (int t, _) imm_idx = (.t.#t)
[%%expect{|
Line 1, characters 22-29:
1 | let f () : (int t, _) imm_idx = (.t.#t)
                          ^^^^^^^
Error: Unbound type constructor "imm_idx"
|}]

(**********)
(* Arrays *)

let idx_array x = (.(x))
let idx_array_L x = (.L(x))
let idx_array_l x = (.l(x))
let idx_array_n x = (.n(x))
let idx_iarray x = (.:(x))
let idx_iarray_L x = (.:L(x))
let idx_iarray_l x = (.:l(x))
let idx_iarray_n x = (.:n(x))
let idx_imm x = (.idx_imm(x))
let idx_mut x = (.idx_mut(x))
[%%expect{|
val idx_array : int -> ('a array, 'a) idx_mut = <fun>
val idx_array_L : int64# -> ('a array, 'a) idx_mut = <fun>
val idx_array_l : int32# -> ('a array, 'a) idx_mut = <fun>
val idx_array_n : nativeint# -> ('a array, 'a) idx_mut = <fun>
val idx_iarray : int -> ('a iarray, 'a) idx_imm = <fun>
val idx_iarray_L : int64# -> ('a iarray, 'a) idx_imm = <fun>
val idx_iarray_l : int32# -> ('a iarray, 'a) idx_imm = <fun>
val idx_iarray_n : nativeint# -> ('a iarray, 'a) idx_imm = <fun>
val idx_imm : ('a, 'b) idx_imm -> ('a, 'b) idx_imm = <fun>
val idx_mut : ('a, 'b) idx_mut -> ('a, 'b) idx_mut = <fun>
|}]

type r = { a : string }
let a () = (.(5).#contents.#a)
[%%expect{|
type r = { a : string; }
val a : unit -> (r# ref# array, string) idx_mut = <fun>
|}]

type t = { mutable a : string; b : int }
let a () = (.(5).#a)
[%%expect{|
type t = { mutable a : string; b : int; }
val a : unit -> (t# array, string) idx_mut = <fun>
|}]

type t1 = { mutable a : string; b : int }
let b () = (.:(5).#a)
[%%expect{|
type t1 = { mutable a : string; b : int; }
val b : unit -> (t1# iarray, string) idx_imm = <fun>
|}]

let bad_index_type = (.("test"))
[%%expect{|
Line 178, characters 24-30:
178 | let bad_index_type = (.("test"))
                              ^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}, Principal{|
Line 189, characters 24-30:
189 | let bad_index_type = (.("test"))
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
Line 6, characters 24-31:
6 |     ((.u.#x) : (_ r, _) imm_idx)
                            ^^^^^^^
Error: Unbound type constructor "imm_idx"
|}]

(******************)
(* Block indices! *)

let idx_imm x = (.idx_imm(x))
let idx_mut x = (.idx_mut(x))
[%%expect{|
val idx_imm : ('a, 'b) idx_imm -> ('a, 'b) idx_imm = <fun>
val idx_mut : ('a, 'b) idx_mut -> ('a, 'b) idx_mut = <fun>
|}]
