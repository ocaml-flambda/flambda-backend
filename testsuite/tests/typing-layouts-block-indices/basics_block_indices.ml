(* TEST
 flags = "-extension layouts_alpha";
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

let f () = (.i)
[%%expect{|
val f : unit -> (r, int) idx_imm = <fun>
|}]

let f () = (.(5).#i)
[%%expect{|
val f : unit -> (r# array, int) idx_mut = <fun>
|}]

(* Module-qualified fields *)

module M = struct
  type u = #{ i : int; j : int }
  type t = { x : u }
end

let f () = (.M.x)
[%%expect{|
module M : sig type u = #{ i : int; j : int; } type t = { x : u; } end
val f : unit -> (M.t, M.u) idx_imm = <fun>
|}]

let f () = (.M.x.#M.i)
[%%expect{|
val f : unit -> (M.t, int) idx_imm = <fun>
|}]

(************************)
(* Field disambiguation *)

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
Line 6, characters 9-10:
6 |     (.a.#a)
             ^
Error: This unboxed access is expected to have base type "y#"
       There is no unboxed record field "a" within type "y#"
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
Line 2, characters 16-17:
2 | let f () = (.x.#x)
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
Line 1, characters 37-38:
1 | let f () : (int t, _) idx_imm = (.t.#t)
                                         ^
Error: The index preceding this unboxed access has element type "int",
       which is not an unboxed record with field "t".
|}]

type t = { i : int } [@@unboxed]
let f () = (.i)
[%%expect{|
type t = { i : int; } [@@unboxed]
Line 2, characters 13-14:
2 | let f () = (.i)
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
Line 5, characters 6-7:
5 |     (.s)
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
Line 3, characters 9-10:
3 |     (.a.#t)
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
val idx_array :
  ('a : value_or_null mod non_float). int -> ('a array, 'a) idx_mut = <fun>
val idx_array_L :
  ('a : value_or_null mod non_float). int64# -> ('a array, 'a) idx_mut =
  <fun>
val idx_array_l :
  ('a : value_or_null mod non_float). int32# -> ('a array, 'a) idx_mut =
  <fun>
val idx_array_n :
  ('a : value_or_null mod non_float). nativeint# -> ('a array, 'a) idx_mut =
  <fun>
val idx_iarray :
  ('a : value_or_null mod non_float). int -> ('a iarray, 'a) idx_imm = <fun>
val idx_iarray_L :
  ('a : value_or_null mod non_float). int64# -> ('a iarray, 'a) idx_imm =
  <fun>
val idx_iarray_l :
  ('a : value_or_null mod non_float). int32# -> ('a iarray, 'a) idx_imm =
  <fun>
val idx_iarray_n :
  ('a : value_or_null mod non_float). nativeint# -> ('a iarray, 'a) idx_imm =
  <fun>
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

type t1 = { a : string }
let b () = (.:(5).#a)
[%%expect{|
type t1 = { a : string; }
val b : unit -> (t1# iarray, string) idx_imm = <fun>
|}]

let bad_index_type = (.("test"))
[%%expect{|
Line 1, characters 24-30:
1 | let bad_index_type = (.("test"))
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

(**************)
(* Modalities *)

type 'a id = { id : 'a }
type 'a id = { mutable mut : 'a }
type 'a global = { global : 'a @@ global }
type 'a aliased = { aliased : 'a @@ aliased }
type 'a many = { many : 'a @@ many }
type 'a unyielding = { unyielding : 'a @@ unyielding }
type 'a portable = { portable : 'a @@ portable }
type 'a contended = { contended : 'a @@ contended }
type 'a mut_not_global = { mutable mut_not_global : 'a @@ local }
type 'a mut_not_many = { mutable mut_not_many : 'a @@ once }
type 'a mut_not_global_nor_aliased =
  (* mut_not_aliased not possible bc global implies aliased *)
  { mutable mut_not_global_nor_aliased : 'a @@ local unique }
type 'a mut_not_global_nor_unyielding =
  (* mut_not_unyielding not possible bc global implies unyielding *)
  { mutable mut_not_global_nor_unyielding : 'a @@ local yielding }
[%%expect{|
type 'a id = { id : 'a; }
type 'a id = { mutable mut : 'a; }
type 'a global = { global_ global : 'a; }
type 'a aliased = { aliased : 'a @@ aliased; }
type 'a many = { many : 'a @@ many; }
type 'a unyielding = { unyielding : 'a @@ unyielding; }
type 'a portable = { portable : 'a @@ portable; }
type 'a contended = { contended : 'a @@ contended; }
type 'a mut_not_global = { mutable mut_not_global : 'a @@ local; }
type 'a mut_not_many = { mutable mut_not_many : 'a @@ once; }
type 'a mut_not_global_nor_aliased = {
  mutable mut_not_global_nor_aliased : 'a @@ local unique;
}
type 'a mut_not_global_nor_unyielding = {
  mutable mut_not_global_nor_unyielding : 'a @@ local;
}
|}]

(* Immutable indices with each disallowed modality *)
let bad () = (.global)
[%%expect{|
Line 1, characters 13-22:
1 | let bad () = (.global)
                 ^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is global.
|}]
let bad () = (.aliased)
[%%expect{|
Line 1, characters 13-23:
1 | let bad () = (.aliased)
                 ^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is aliased.
|}]
let bad () = (.many)
[%%expect{|
Line 1, characters 13-20:
1 | let bad () = (.many)
                 ^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is many.
|}]
let bad () = (.unyielding)
[%%expect{|
Line 1, characters 13-26:
1 | let bad () = (.unyielding)
                 ^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is unyielding.
|}]
let bad () = (.portable)
[%%expect{|
Line 1, characters 13-24:
1 | let bad () = (.portable)
                 ^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is portable.
|}]
let bad () = (.contended)
[%%expect{|
Line 1, characters 13-25:
1 | let bad () = (.contended)
                 ^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is contended.
|}]

(* Mutable indices with each disallowed modality *)
let bad () = (.mut_not_global)
[%%expect{|
Line 1, characters 13-30:
1 | let bad () = (.mut_not_global)
                 ^^^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       mutable elements must be global, but this is not.
|}]

let bad () = (.mut_not_many)
[%%expect{|
Line 1, characters 13-28:
1 | let bad () = (.mut_not_many)
                 ^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       mutable elements must be many, but this is not.
|}]

let bad () = (.mut_not_global_nor_aliased)
[%%expect{|
Line 1, characters 13-42:
1 | let bad () = (.mut_not_global_nor_aliased)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       mutable elements must be global, but this is not.
|}]

let bad () = (.mut_not_global_nor_unyielding)
[%%expect{|
Line 1, characters 13-45:
1 | let bad () = (.mut_not_global_nor_unyielding)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       mutable elements must be global, but this is not.
|}]

let bad () = (.mut.#contended)
[%%expect{|
Line 1, characters 13-30:
1 | let bad () = (.mut.#contended)
                 ^^^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       mutable elements must have the identity modality, but this is contended.
|}]

let bad () = (.mut.#portable)
[%%expect{|
Line 1, characters 13-29:
1 | let bad () = (.mut.#portable)
                 ^^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       mutable elements must have the identity modality, but this is portable.
|}]

(* After arrays *)
let bad () = (.(0).#portable)
[%%expect{|
Line 1, characters 13-29:
1 | let bad () = (.(0).#portable)
                 ^^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       mutable elements must have the identity modality, but this is portable.
|}]
let bad () = (.(0).#mut_not_many)
[%%expect{|
val bad :
  ('a : value mod non_float). unit -> ('a mut_not_many# array, 'a) idx_mut =
  <fun>
|}]

(* After immutable arrays *)
let bad () = (.:(0).#global)
[%%expect{|
Line 1, characters 13-28:
1 | let bad () = (.:(0).#global)
                 ^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is global.
|}]
let bad () = (.:(0).#id.#global.#id)
[%%expect{|
Line 1, characters 13-36:
1 | let bad () = (.:(0).#id.#global.#id)
                 ^^^^^^^^^^^^^^^^^^^^^^^
Error: Block indices do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is global.
|}]

(* A few positive examples to show that it's the composition of modalities we
   check*)
let ok () = (.contents.#global.#many)
[%%expect{|
val ok : unit -> ('a many# global# ref, 'a) idx_mut = <fun>
|}]
let ok () = (.(0).#global.#many.#aliased.#unyielding)
[%%expect{|
val ok :
  ('a : value mod non_float).
    unit -> ('a unyielding# aliased# many# global# array, 'a) idx_mut =
  <fun>
|}]
let ok () = (.mut.#mut_not_global.#id)
[%%expect{|
val ok : unit -> ('a id/2# mut_not_global# id/1, 'a) idx_mut = <fun>
|}]

(************)
(* Variance *)

(* Immutable, but not mutable, block indices are covariant in their second type
   parameter. *)

type ('a, +'b) t = ('a, 'b) idx_imm
[%%expect{|
type ('a, 'b) t = ('a, 'b) idx_imm
|}]

type ('a, +'b) bad = ('a, 'b) idx_mut
[%%expect{|
Line 1, characters 0-37:
1 | type ('a, +'b) bad = ('a, 'b) idx_mut
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 2nd type parameter was expected to be covariant,
       but it is injective invariant.
|}]

(******************************************************)
(* Cannot take an index to float/non-separable arrays *)

let bad () : (float array, _) idx_mut = (.(0))
[%%expect{|
Line 1, characters 40-46:
1 | let bad () : (float array, _) idx_mut = (.(0))
                                            ^^^^^^
Error: This expression has type "('a array, 'a) idx_mut"
       but an expression was expected of type "(float array, 'b) idx_mut"
       The kind of float is value mod many unyielding stateless immutable
         because it is the primitive type float.
       But the kind of float must be a subkind of value_or_null mod non_float
         because it's the element type (the second type parameter) for a
         block index (idx or mut_idx).
|}]

type packed = P : 'a -> packed
let bad () : (packed array, _) idx_mut = (.(0))
[%%expect{|
type packed = P : 'a -> packed
val bad : unit -> (packed array, packed) idx_mut = <fun>
|}]

type abstract
let bad () : (abstract array, _) idx_mut = (.(0))
[%%expect{|
type abstract
Line 2, characters 43-49:
2 | let bad () : (abstract array, _) idx_mut = (.(0))
                                               ^^^^^^
Error: This expression has type "('a array, 'a) idx_mut"
       but an expression was expected of type "(abstract array, 'b) idx_mut"
       The kind of abstract is value
         because of the definition of abstract at line 1, characters 0-13.
       But the kind of abstract must be a subkind of
         value_or_null mod non_float
         because it's the element type (the second type parameter) for a
         block index (idx or mut_idx).
|}]

let bad () : (float iarray, _) idx_imm = (.:(0))
[%%expect{|
Line 1, characters 41-48:
1 | let bad () : (float iarray, _) idx_imm = (.:(0))
                                             ^^^^^^^
Error: This expression has type "('a iarray, 'a) idx_imm"
       but an expression was expected of type "(float iarray, 'b) idx_imm"
       The kind of float is value mod many unyielding stateless immutable
         because it is the primitive type float.
       But the kind of float must be a subkind of value_or_null mod non_float
         because it's the element type (the second type parameter) for a
         block index (idx or mut_idx).
|}]

type packed = P : 'a -> packed
let bad () : (packed iarray, _) idx_imm = (.:(0))
[%%expect{|
type packed = P : 'a -> packed
val bad : unit -> (packed iarray, packed) idx_imm = <fun>
|}]

type abstract
let bad () : (abstract iarray, _) idx_imm = (.:(0))
[%%expect{|
type abstract
Line 2, characters 44-51:
2 | let bad () : (abstract iarray, _) idx_imm = (.:(0))
                                                ^^^^^^^
Error: This expression has type "('a iarray, 'a) idx_imm"
       but an expression was expected of type "(abstract iarray, 'b) idx_imm"
       The kind of abstract is value
         because of the definition of abstract at line 1, characters 0-13.
       But the kind of abstract must be a subkind of
         value_or_null mod non_float
         because it's the element type (the second type parameter) for a
         block index (idx or mut_idx).
|}]

(****************)
(* Principality *)

type u = #{ x : int }
type u2 = #{ x : string }
type 'a r = { u : u }
type 'a r2 = { u : u }
type 'a t = { a : 'a }
[%%expect{|
type u = #{ x : int; }
type u2 = #{ x : string; }
type 'a r = { u : u; }
type 'a r2 = { u : u; }
type 'a t = { a : 'a; }
|}]

(* We get a principality warning when the block index type is disambiguated
   non-principally. *)
let f c =
  if c then
    ((.u.#x) : (_ r, _) idx_imm)
  else
    (.u.#x)
[%%expect{|
val f : bool -> ('a r, int) idx_imm = <fun>
|}, Principal{|
Line 5, characters 6-7:
5 |     (.u.#x)
          ^
Warning 18 [not-principal]: this type-based field disambiguation is not principal.

val f : bool -> ('a r, int) idx_imm = <fun>
|}]

(* First unboxed index disambiguated non-principally *)
let f c =
  if c then
    ((.a.#x) : (u t, _) idx_imm)
  else
    (.a.#x)
[%%expect{|
val f : bool -> (u t, int) idx_imm = <fun>
|}, Principal{|
Line 5, characters 9-10:
5 |     (.a.#x)
             ^
Warning 18 [not-principal]: this type-based unboxed record field disambiguation is not principal.

val f : bool -> (u t, int) idx_imm = <fun>
|}]

(* Second unboxed index disambiguated non-principally *)
let f c =
  if c then
    ((.a.#a.#x) : (u t# t, _) idx_imm)
  else
    (.a.#a.#x)
[%%expect{|
val f : bool -> (u t# t, int) idx_imm = <fun>
|}, Principal{|
Line 5, characters 12-13:
5 |     (.a.#a.#x)
                ^
Warning 18 [not-principal]: this type-based unboxed record field disambiguation is not principal.

val f : bool -> (u t# t, int) idx_imm = <fun>
|}]

(* First unboxed index disambiguated non-principally through the expected array
   type *)
let f c =
  if c then
    ((.(0).#x) : (u array, _) idx_mut)
  else
    (.(1).#x)
[%%expect{|
val f : bool -> (u array, int) idx_mut = <fun>
|}, Principal{|
Line 5, characters 11-12:
5 |     (.(1).#x)
               ^
Warning 18 [not-principal]: this type-based unboxed record field disambiguation is not principal.

val f : bool -> (u array, int) idx_mut = <fun>
|}]

(* Second unboxed index disambiguated non-principally through the expected array
   type *)
let f c =
  if c then
    ((.(0).#a.#x) : (u t# array, _) idx_mut)
  else
    (.(1).#a.#x)
[%%expect{|
val f : bool -> (u t# array, int) idx_mut = <fun>
|}, Principal{|
Line 5, characters 14-15:
5 |     (.(1).#a.#x)
                  ^
Warning 18 [not-principal]: this type-based unboxed record field disambiguation is not principal.

val f : bool -> (u t# array, int) idx_mut = <fun>
|}]
