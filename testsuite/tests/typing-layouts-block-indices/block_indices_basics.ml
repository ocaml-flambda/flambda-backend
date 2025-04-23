(* TEST
 flags = "-extension layouts_beta";
 expect;
*)


(*********************************)
(* Basic typechecking of indices *)

type r = { i : int; j : int }
type t = (r# array, r#) idx_imm
[%%expect{|
type r = { i : int; j : int; }
type t = (r# array, r#) idx_imm
|}]

(* Disambiguation of block access field *)

type t1 = { mutable a : string; b : int }
type t2 = { mutable a : string; b : int; c : string }

let a2 () = (.a)
let b2 () = (.b)
let a1 () : (t1, _) idx_mut = (.a)
let b2 () : (t1, _) idx_imm = (.b)
[%%expect{|
type t1 = { mutable a : string; b : int; }
type t2 = { mutable a : string; b : int; c : string; }
val a2 : unit -> (t2, string) idx_mut = <fun>
val b2 : unit -> (t2, int) idx_imm = <fun>
val a1 : unit -> (t1, string) idx_mut = <fun>
val b2 : unit -> (t1, int) idx_imm = <fun>
|}]

(* Still disambiguates through a Tpoly *)
let a1 =
  let a1 : 'a. (t1, _) idx_mut = (.a) in
  fun () -> a1
[%%expect{|
val a1 : unit -> (t1, string) idx_mut = <fun>
|}]

(* Disambiguate by alias to idx_imm types *)
type ('c, 'b, 'a) mi = ('a, 'b) idx_mut
type ('c, 'b, 'a) i = ('a, 'b) idx_imm
let a () : (_, _, t1) mi = (.a)
let b () : (float, int, t1) i = (.b)
[%%expect{|
type ('c, 'b, 'a) mi = ('a, 'b) idx_mut
type ('c, 'b, 'a) i = ('a, 'b) idx_imm
val a : unit -> (t1, string) idx_mut = <fun>
val b : unit -> (t1, int) idx_imm = <fun>
|}]

(* Block access disambiguates the unboxed access *)
type u = #{ x : int }
type u2 = #{ x : string }
type 'a r = { u : u }
type 'a r2 = { u : u2 }
let f () : (_ r, _) idx_imm = (.u.#x)
[%%expect{|
type u = #{ x : int; }
type u2 = #{ x : string; }
type 'a r = { u : u; }
type 'a r2 = { u : u2; }
val f : unit -> ('a r, int) idx_imm = <fun>
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
let bad c = if c then
    ((.a.#y) : (y# t, int) idx_imm)
  else
    (.a.#a)
[%%expect{|
type y = { y : int; }
type 'a t = { a : 'a; }
Line 6, characters 4-11:
6 |     (.a.#a)
        ^^^^^^^
Error: This expression has type "('a t# t, 'a) idx_imm"
       but an expression was expected of type "(y# t, int) idx_imm"
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
val f : unit -> ('a t# t, 'a) idx_imm = <fun>
|}]

let f () : (int t, _) idx_imm = (.t.#t)
[%%expect{|
Line 1, characters 32-39:
1 | let f () : (int t, _) idx_imm = (.t.#t)
                                    ^^^^^^^
Error: This expression has type "('a t# t, 'a) idx_imm"
       but an expression was expected of type "(int t, 'b) idx_imm"
       Type "'a t#" is not compatible with type "int"
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
>> Fatal error: Texp_idx: array unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type r = { a : string }
let a () = (.(5).#contents.#a)
[%%expect{|
type r = { a : string; }
>> Fatal error: Texp_idx: array unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type t = { mutable a : string; b : int }
let a () = (.(5).#a)
[%%expect{|
type t = { mutable a : string; b : int; }
>> Fatal error: Texp_idx: array unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type t1 = { mutable a : string; b : int }
let b () = (.:(5).#a)
[%%expect{|
type t1 = { mutable a : string; b : int; }
>> Fatal error: Texp_idx: array unimplemented
Uncaught exception: Misc.Fatal_error

|}]

let bad_index_type = (.("test"))
[%%expect{|
Line 172, characters 24-30:
172 | let bad_index_type = (.("test"))
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
    ((.u.#x) : (_ r, _) idx_imm)
  else
    (.u.#x)
[%%expect{|
type u = #{ x : int; }
type 'a r = { u : u; }
type 'a r2 = { u : u; }
val f : bool -> ('a r, int) idx_imm = <fun>
|}, Principal{|
type u = #{ x : int; }
type 'a r = { u : u; }
type 'a r2 = { u : u; }
Line 193, characters 6-7:
193 |     (.u.#x)
            ^
Warning 18 [not-principal]: this type-based field disambiguation is not principal.

val f : bool -> ('a r, int) idx_imm = <fun>
|}]

(******************)
(* Block indices! *)

let idx_imm x = (.idx_imm(x))
let idx_mut x = (.idx_mut(x))
[%%expect{|
val idx_imm : ('a, 'b) idx_imm -> ('a, 'b) idx_imm = <fun>
val idx_mut : ('a, 'b) idx_mut -> ('a, 'b) idx_mut = <fun>
|}]
