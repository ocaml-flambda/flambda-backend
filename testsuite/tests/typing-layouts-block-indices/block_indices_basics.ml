(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* CR rtjoa: this is only alpha because arrays don't check nonfloat *)

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

(* Array type disambiguates the unboxed access *)
let f () : (u array, _) idx_mut = (.(0).#x)
[%%expect{|
val f : unit -> (u array, int) idx_mut = <fun>
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

(*****************)
(* Float records *)

(* Indicies to flattened float always have element type [float#] *)
type t = { f : float }
let f () = (.f)
[%%expect{|
type t = { f : float; }
val f : unit -> (t, float#) idx_imm = <fun>
|}]

(* Unboxed float record *)
type t = { fu : float# }
let fu () = (.fu)
[%%expect{|
type t = { fu : float#; }
val fu : unit -> (t, float#) idx_imm = <fun>
|}]

type t_float64 : float64
type t = { t_float64 : t_float64 }
let t_float64 () = (.t_float64)
[%%expect{|
type t_float64 : float64
type t = { t_float64 : t_float64; }
val t_float64 : unit -> (t, t_float64) idx_imm = <fun>
|}]

(* Singleton unboxed records containing floats can appear in float records *)
type fr = #{ f : float }
type t = { f : float; fr : fr  }
let fr () = (.fr)
let fr_f () = (.fr.#f)
[%%expect{|
type fr = #{ f : float; }
type t = { f : float; fr : fr; }
val fr : unit -> (t, float#) idx_imm = <fun>
val fr_f : unit -> (t, float#) idx_imm = <fun>
|}]

(* Mixed float record *)
type t_float64 : float64
type t = { f : float; t_float64 : t_float64; fu : float#; fr : fr  }
let f () = (.f)
let fu () = (.fu)
let t_float64 () = (.t_float64)
let fr () = (.fr)
let fr_f () = (.fr.#f)
[%%expect{|
type t_float64 : float64
type t = { f : float; t_float64 : t_float64; fu : float#; fr : fr; }
val f : unit -> (t, float#) idx_imm = <fun>
val fu : unit -> (t, float#) idx_imm = <fun>
val t_float64 : unit -> (t, t_float64) idx_imm = <fun>
val fr : unit -> (t, float#) idx_imm = <fun>
val fr_f : unit -> (t, float#) idx_imm = <fun>
|}]

(***************)
(* Type errors *)

type pt = { x : int }
let f () = (.x.#x)
[%%expect{|
type pt = { x : int; }
Line 164, characters 16-17:
164 | let f () = (.x.#x)
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

type t = { i : int } [@@unboxed]
let f () = (.i)
[%%expect{|
type t = { i : int; } [@@unboxed]
Line 192, characters 13-14:
192 | let f () = (.i)
                   ^
Error: Block indices do not support [@@unboxed] records.
|}]

(* Disambiguation errors *)
type t = { t : int }
type s = { s : int }
type a_t = { a : t# }
type a_s = { a : s# }
[%%expect{|
type t = { t : int; }
type s = { s : int; }
type a_t = { a : t#; }
type a_s = { a : s#; }
|}]

(* Disambiguation error when typing block access *)
let f c =
  if c then
    (.t)
  else
    (.s)
[%%expect{|
Line 218, characters 6-7:
218 |     (.s)
            ^
Error: This block index is expected to have base type "t"
       There is no field "s" within type "t"
|}]

(* Disambiguation error when typing unboxed access *)
let f c =
  if c then
    (.a.#t)
  else
    (.a.#s)
[%%expect{|
Line 230, characters 9-10:
230 |     (.a.#t)
               ^
Error: This unboxed access is expected to have base type "s#"
       There is no unboxed record field "t" within type "s#"
|}]

(************)
(* Variance *)

let coerce_imm (idx : (_, [ `A ]) idx_imm) =
  (idx :> (_, [ `A | `B ]) idx_imm)
[%%expect{|
val coerce_imm : ('a, [ `A ]) idx_imm -> ('a, [ `A | `B ]) idx_imm = <fun>
|}]

let coerce_mut_bad (idx : (_, [ `A ]) idx_mut) =
  (idx :> (_, [ `A | `B ]) idx_mut)
[%%expect{|
Line 2, characters 3-6:
2 |   (idx :> (_, [ `A | `B ]) idx_mut)
       ^^^
Error: This expression cannot be coerced to type ""('a, [ `A | `B ]) idx_mut"";
       it has type "('a, [ `A ]) idx_mut" but is here used with type
         "('a, [ `A | `B ]) idx_mut"
       The first variant type does not allow tag(s) "`B"
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
Line 2, characters 11-21:
2 | let b () = (.:(5).#a)
               ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let bad_index_type = (.("test"))
[%%expect{|
Line 312, characters 24-30:
312 | let bad_index_type = (.("test"))
                              ^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]

(****************)
(* Illegal gaps *)

type a = float#
type b = #(a * a * a * a * a * a * a * a) (* 2^6 bytes *)
type c = #(b * b * b * b * b * b * b * b) (* 2^9 *)
type d = #(c * c * c * c * c * c * c * c) (* 2^12 *)
type e = #(d * d * d * d * d * d * d * d) (* 2^15 *)
type f = #(e * e)                         (* 2^16 *)

type si = { s : string; i : int64# }
type r = { f : f; si : si# }
[%%expect{|
type a = float#
type b = #(a * a * a * a * a * a * a * a)
type c = #(b * b * b * b * b * b * b * b)
type d = #(c * c * c * c * c * c * c * c)
type e = #(d * d * d * d * d * d * d * d)
type f = #(e * e)
type si = { s : string; i : int64#; }
type r = { f : f; si : si#; }
|}]

(* A gap of 2^16 bytes is not allowed *)
let bad_idx () = (.si)
[%%expect{|
Line 1, characters 17-22:
1 | let bad_idx () = (.si)
                     ^^^^^
Error: Block indices into records that contain both values and non-values,
       and occupy over 2^16 bytes, cannot be created.
|}]

(* But we *can* construct a deeper, valid index *)
let f () = (.si.#s)
[%%expect{|
val f : unit -> (r, string) idx_imm = <fun>
|}]

(* A valid index that could be deepened to a gap of 2^16 bytes is not allowed *)
type hold_r = { s: string; r : r# }
let bad_idx () = (.r)
[%%expect{|
type hold_r = { s : string; r : r#; }
Line 2, characters 17-21:
2 | let bad_idx () = (.r)
                     ^^^^
Error: Block indices into records that contain both values and non-values,
       and occupy over 2^16 bytes, cannot be created.
|}]

(*************************************************************)
(* Array element not reordering when it would be in a record *)

(* CR layouts v8: these should be allowed once we reorder array elements *)

type r = #{ a : int64#; b : int }
let bad_idx () = (.(0).#a)
[%%expect{|
type r = #{ a : int64#; b : int; }
Line 2, characters 17-26:
2 | let bad_idx () = (.(0).#a)
                     ^^^^^^^^^
Error: Block indices into arrays whose element layout contains a
       non-value before a value are not yet supported.
|}]

type r = { ii : #( int * int64#) ; i : int }
let bad_idx () = (.(0).#ii)
[%%expect{|
type r = { ii : #(int * int64#); i : int; }
Line 2, characters 17-27:
2 | let bad_idx () = (.(0).#ii)
                     ^^^^^^^^^^
Error: Block indices into arrays whose element layout contains a
       non-value before a value are not yet supported.
|}]

(* Note that this does work, though, as no reordering is needed *)
type r = #{ a : int; b : int64# }
let idx_into_r_array () = (.(0).#a)
[%%expect{|
type r = #{ a : int; b : int64#; }
val idx_into_r_array : unit -> (r array, int) idx_mut = <fun>
|}]

(*************************************************************)
(* Block indices into block index accesses (aka "deepening") *)

let idx_imm x = (.idx_imm(x))
let idx_mut x = (.idx_mut(x))
[%%expect{|
val idx_imm : ('a, 'b) idx_imm -> ('a, 'b) idx_imm = <fun>
val idx_mut : ('a, 'b) idx_mut -> ('a, 'b) idx_mut = <fun>
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
Line 429, characters 6-7:
429 |     (.u.#x)
            ^
Warning 18 [not-principal]: this type-based field disambiguation is not principal.

val f : bool -> ('a r, int) idx_imm = <fun>
|}]
