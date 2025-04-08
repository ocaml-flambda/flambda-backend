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

let (.%[;..]) = Bigarray.Genarray.get
let f a = (.%[1;2;3])
[%%expect{|
val ( .%[;..] ) : ('a, 'b, 'c) Bigarray.Genarray.t -> int array -> 'a = <fun>
>> Fatal error: expected operator used in block index to be a primitive
Uncaught exception: Misc.Fatal_error

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
Line 147, characters 6-7:
147 |     (.u.#x)
            ^
Warning 18 [not-principal]: this type-based field disambiguation is not principal.

val f : bool -> ('a r, int) imm_idx = <fun>
|}]

(**********)
(* Arrays *)

external ( .:() )
  :  ('a iarray[@local_opt])
  -> int
  -> ('a[@local_opt])
  @@ portable
  = "%array_safe_get"


external ( .%() )
  :  ('a : any_non_null). ('a iarray[@local_opt])
  -> int
  -> ('a[@local_opt])
  @@ portable
  = "%array_safe_get"
[@@layout_poly]
[%%expect{|
external ( .:() ) : ('a iarray [@local_opt]) -> int -> ('a [@local_opt]) @@
  portable = "%array_safe_get"
external ( .%() ) :
  ('a : any_non_null). ('a iarray [@local_opt]) -> int -> ('a [@local_opt])
  @@ portable = "%array_safe_get" [@@layout_poly]
|}]

type r = { a : string }
let a () = (.(5).#contents.#a)
[%%expect{|
type r = { a : string; }
val a : unit -> (r# ref# array, string) mut_idx = <fun>
|}]

(* This is sad but correct - the fix is to change
   [Array.get] in the stdlib to be layout polymorphic *)
type t = { mutable a : string; b : int }
let bad () = (.(5).#a)
[%%expect{|
type t = { mutable a : string; b : int; }
Line 201, characters 20-21:
201 | let bad () = (.(5).#a)
                          ^
Error: The index preceding this unboxed access has element type "'a",
       which is not an unboxed record with field "a".
|}]

type t1 = { mutable a : string; b : int }
let b () = (.%(5).#a)
[%%expect{|
type t1 = { mutable a : string; b : int; }
val b : unit -> (t1# iarray, string) imm_idx = <fun>
|}]

let bad_index_type = (.("test"))
[%%expect{|
Line 225, characters 24-30:
225 | let bad_index_type = (.("test"))
                              ^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]

(***********************)
(* Abstract index type *)

module M : sig
  type index
  val i : index
  external ( .:() ) : 'a array -> index -> 'a = "%array_safe_get"
end = struct
  type index = int
  let i = 0
  external ( .:() ) : 'a array -> index -> 'a = "%array_safe_get"
end

let f () = (.M.:(M.i))
[%%expect{|
module M :
  sig
    type index
    val i : index
    external ( .:() ) : 'a array -> index -> 'a = "%array_safe_get"
  end
val f : unit -> ('a array, 'a) mut_idx = <fun>
|}]
