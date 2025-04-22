(* TEST
 flags = "-extension layouts_beta";
 toplevel;
*)


(*********************************)
(* Basic typechecking of indices *)

type r = { i : int; j : int }
type t = (r# array, r#) idx_imm
;;

(* Disambiguation of block access field *)

type t1 = { mutable a : string; b : int }
type t2 = { mutable a : string; b : int; c : string }

let a2 () = (.a)
let b2 () = (.b)
let a1 () : (t1, _) idx_mut = (.a)
let b2 () : (t1, _) idx_imm = (.b)
;;

(* Still disambiguates through a Tpoly *)
let a1 =
  let a1 : 'a. (t1, _) idx_mut = (.a) in
  fun () -> a1
;;

(* Disambiguate by alias to idx_imm types *)
type ('c, 'b, 'a) mi = ('a, 'b) idx_mut
type ('c, 'b, 'a) i = ('a, 'b) idx_imm
let a () : (_, _, t1) mi = (.a)
let b () : (float, int, t1) i = (.b)
;;

(* Block access disambiguates the unboxed access *)
type u = #{ x : int }
type u2 = #{ x : string }
type 'a r = { u : u }
type 'a r2 = { u : u2 }
let f () : (_ r, _) idx_imm = (.u.#x)
;;

(* Unboxed access disambiguates the next unboxed access *)
type wrap_r = { r : int r# }
let f () = (.r.#u.#x)
;;

(* Disambiguation causes earlier error while typechecking block access *)
type y = { y : int }
type 'a t = { a : 'a }
let bad c = if c then
    ((.a.#y) : (y# t, int) idx_imm)
  else
    (.a.#a)
;;

(***************)
(* Type errors *)

type pt = { x : int }
let f () = (.x.#x)
;;

type 'a t = { t : 'a }
let f () = (.t.#t)
;;

let f () : (int t, _) idx_imm = (.t.#t)
;;

(**********)
(* Arrays *)

(* let idx_array x = (.(x))
 * let idx_array_L x = (.L(x))
 * let idx_array_l x = (.l(x))
 * let idx_array_n x = (.n(x))
 * let idx_iarray x = (.:(x))
 * let idx_iarray_L x = (.:L(x))
 * let idx_iarray_l x = (.:l(x))
 * let idx_iarray_n x = (.:n(x))
 * let idx_imm x = (.idx_imm(x))
 * let idx_mut x = (.idx_mut(x)) *)
;;

(* type r = { a : string }
 * let a () = (.(5).#contents.#a)
 * ;;
 *
 * type t = { mutable a : string; b : int }
 * let a () = (.(5).#a)
 * ;;
 *
 * type t1 = { mutable a : string; b : int }
 * let b () = (.:(5).#a)
 * ;;
 *
 * let bad_index_type = (.("test")) *)
;;

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
;;

(******************)
(* Block indices! *)

let idx_imm x = (.idx_imm(x))
let idx_mut x = (.idx_mut(x))
;;
