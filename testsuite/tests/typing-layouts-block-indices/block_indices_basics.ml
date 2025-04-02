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

(* Disambiguation by block access field *)

type t1 = { mutable a : string; b : int }
type t2 = { mutable a : string; b : int; c : string }

let i () = (.a)
let j () = (.b)
[%%expect{|
type t1 = { mutable a : string; b : int; }
type t2 = { mutable a : string; b : int; c : string; }
val i : unit -> (t2, string) mut_idx = <fun>
val j : unit -> (t2, int) imm_idx = <fun>
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

(***************)
(* Type errors *)

type pt = { x : int }
let f () = (.x.#x)
[%%expect{|
type pt = { x : int; }
Line 73, characters 16-17:
73 | let f () = (.x.#x)
                     ^
Error: The index preceding this unboxed access has element type "int",
       which is not an unboxed record with field "x".
|}, Principal{|
type pt = { x : int; }
Line 76, characters 16-17:
76 | let f () = (.x.#x)
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
Line 1, characters 28-35:
1 | let f () : (int t, _) imm_idx = (.t.#t)
                                ^^^^^^^
Error: This expression has type "('a t# t, 'a) imm_idx"
       but an expression was expected of type "(int t, 'b) imm_idx"
       Type "'a t#" is not compatible with type "int"
|}]

(****************)
(* Principatily *)

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
Line 115, characters 6-7:
115 |     (.u.#x)
            ^
Warning 18 [not-principal]: this type-based field disambiguation is not principal.

val f : bool -> ('a r, int) imm_idx = <fun>
|}]
