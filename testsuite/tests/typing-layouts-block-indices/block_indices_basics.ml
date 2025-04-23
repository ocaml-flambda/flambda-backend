(* TEST
 flags = "-extension layouts_beta";
 toplevel.opt;
*)

(* There is no native expect test mode, so we use toplevel.opt, and organize
   into modules to improve side-by-side readability with the reference file. *)

(*********************************)
(* Basic typechecking of indices *)

module Basic_index_types = struct
  type t1 = (int iarray, int) idx_imm
  type 'a t2 = ('a ref, 'a) idx_mut
end

module Block_access_disambiguated_by_expected_ty = struct
  type t1 = { mutable a : string; b : int }
  type t2 = { mutable a : string; b : int; c : string }

  let a2 () = (.a)
  let b2 () = (.b)
  let a1 () : (t1, _) idx_mut = (.a)
  let b1 () : (t1, _) idx_imm = (.b)

  (* Still disambiguates through a Tpoly *)
  let a1' =
    let a1' : 'a. (t1, _) idx_mut = (.a) in
    fun () -> a1'

  (* Disambiguate by alias to idx_imm types *)
  type ('c, 'b, 'a) mi = ('a, 'b) idx_mut
  type ('c, 'b, 'a) i = ('a, 'b) idx_imm
  let a1'' () : (_, _, t1) mi = (.a)
  let b1'' () : (float, int, t1) i = (.b)
end

module Unboxed_access_disambiguated_by_prev_access = struct
  (* Block access disambiguates the unboxed access *)
  type u = #{ x : int }
  type u2 = #{ x : string }
  type 'a r = { u : u }
  type 'a r2 = { u : u2 }
  let f () : (_ r, _) idx_imm = (.u.#x)

  (* Unboxed access disambiguates the next unboxed access *)
  type wrap_r = { r : int r# }
  let f' () = (.r.#u.#x)
end
;;

(***************)
(* Type errors *)
module Type_errors_start = struct end
;;

(* Disambiguation causes earlier error while typechecking block access *)
type y = { y : int }
type 'a t = { a : 'a }
let bad c = if c then
    ((.a.#y) : (y# t, int) idx_imm)
  else
    (.a.#a)
;;

type pt = { x : int }
let f () = (.x.#x)
;;

type 'a t = { t : 'a }
let f () = (.t.#t)
;;

let f () : (int t, _) idx_imm = (.t.#t)
;;

module Type_errors_end = struct end

(****************)
(* Principality *)

(* We get a principality warning when the block index type is disambiguated
   non-principally. *)

module Principality = struct
  type u = #{ x : int }
  type 'a r = { u : u }
  type 'a r2 = { u : u }
  let f c =
    if c then
      ((.u.#x) : (_ r, _) idx_imm)
    else
      (.u.#x)
end
;;

(******************)
(* Block indices! *)

module Deepening = struct
  let idx_imm x = (.idx_imm(x))
  let idx_mut x = (.idx_mut(x))
end
;;



(* CR rtjoa; Tests for unimplemented features:

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

let bad_index_type = (.("test"))

type r = { a : string }
let a () = (.(5).#contents.#a)

type t = { mutable a : string; b : int }
let a () = (.(5).#a)

type t1 = { mutable a : string; b : int }
let b () = (.:(5).#a)
*)
