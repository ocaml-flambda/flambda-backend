  (* TEST
 flags += "-extension unique";
 expect;
*)

(* This file is to test uniqueness_analysis.ml *)

(* First some helper functions *)
let unique_id : 'a. unique_ 'a -> unique_ 'a = fun x -> x
[%%expect{|
val unique_id : unique_ 'a -> unique_ 'a = <fun>
|}]

let shared_id : 'a -> 'a = fun x -> x
[%%expect{|
val shared_id : 'a -> 'a = <fun>
|}]

let ignore_once: once_ 'a -> unit = fun x -> ()

type box = { x : int }
[%%expect{|
val ignore_once : once_ 'a -> unit = <fun>
type box = { x : int; }
|}]

let update : unique_ box -> unique_ box = unique_id
[%%expect{|
val update : unique_ box -> unique_ box = <fun>
|}]


(* testing Texp_ifthenelse  *)

let branching (unique_ x : float) = unique_ if true then x else x
[%%expect{|
val branching : unique_ float -> float = <fun>
|}]

(* whether we constrain uniqueness or linearity is irrelavant
   for testing uniqueness analysis. Therefore, in the rest we
   will only constrain uniqueness *)
let branching (once_ x : float) = if true then x else x
[%%expect{|
val branching : once_ float -> once_ float = <fun>
|}]

let branching b =
  let unique_ r = { x = 23 } in
  if b then update r
       else update r
[%%expect{|
val branching : bool -> box = <fun>
|}]

let sequence (unique_ x : float) = unique_ let y = x in (x, y)
[%%expect{|
Line 1, characters 60-61:
1 | let sequence (unique_ x : float) = unique_ let y = x in (x, y)
                                                                ^
Error: This value is used here, but it has already been used as unique:
Line 1, characters 57-58:
1 | let sequence (unique_ x : float) = unique_ let y = x in (x, y)
                                                             ^

|}]

let sequence =
  let r = { x = 23 } in
  let s = update r in
  let t = update s in
  t
[%%expect{|
val sequence : box = {x = 23}
|}]

let sequence =
  let r = { x = 23 } in
  let _s = update r in
  let t = update r in
  t
[%%expect{|
Line 4, characters 17-18:
4 |   let t = update r in
                     ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 18-19:
3 |   let _s = update r in
                      ^

|}]

let children_unique (unique_ xs : float list) =
  match xs with
  | [] -> (0., [])
  | x :: xx -> unique_ (x, xx)
[%%expect{|
val children_unique : unique_ float list -> float * float list = <fun>
|}]

let borrow_match (unique_ fs : 'a list) =
  match fs with
  | [] -> []
  | x :: xs as gs -> unique_ gs
[%%expect{|
val borrow_match : unique_ 'a list -> 'a list = <fun>
|}]

let borrow_match (unique_ fs : 'a list) =
  match fs with
    | [] -> []
    | x :: xs -> unique_ fs
[%%expect{|
val borrow_match : unique_ 'a list -> 'a list = <fun>
|}]

let dup_child (unique_ fs : 'a list) =
  match fs with
  | [] -> ([], [])
  | x :: xs as gs -> (unique_ gs), xs
[%%expect{|
Line 4, characters 35-37:
4 |   | x :: xs as gs -> (unique_ gs), xs
                                       ^^
Error: This value is used here,
       but it is part of a value that has already been used as unique:
Line 4, characters 21-33:
4 |   | x :: xs as gs -> (unique_ gs), xs
                         ^^^^^^^^^^^^

|}]

let dup_child (unique_ fs : 'a list) =
  match fs with
  | [] -> ([], [])
  | x :: xs as gs -> gs, unique_ xs
[%%expect{|
Line 4, characters 25-35:
4 |   | x :: xs as gs -> gs, unique_ xs
                             ^^^^^^^^^^
Error: This value is used here as unique,
       but it is part of a value that has already been used:
Line 4, characters 21-23:
4 |   | x :: xs as gs -> gs, unique_ xs
                         ^^

|}]
let dup_child (unique_ fs : 'a list) =
  match fs with
  | [] -> ([], [])
  | x :: xs as gs -> (unique_ xs), gs
[%%expect{|
Line 4, characters 35-37:
4 |   | x :: xs as gs -> (unique_ xs), gs
                                       ^^
Error: This value is used here,
       but part of it has already been used as unique:
Line 4, characters 21-33:
4 |   | x :: xs as gs -> (unique_ xs), gs
                         ^^^^^^^^^^^^

|}]
let dup_child (unique_ fs : 'a list) =
  match fs with
  | [] -> ([], [])
  | x :: xs as gs -> xs, unique_ gs
[%%expect{|
Line 4, characters 25-35:
4 |   | x :: xs as gs -> xs, unique_ gs
                             ^^^^^^^^^^
Error: This value is used here as unique,
       but part of it has already been used:
Line 4, characters 21-23:
4 |   | x :: xs as gs -> xs, unique_ gs
                         ^^

|}]



let or_patterns1 : unique_ float list -> float list -> float =
  fun x y -> match x, y with
  | z :: _, _ | _, z :: _ -> unique_ z
  | _, _ -> 42.0
[%%expect{|
Line 3, characters 37-38:
3 |   | z :: _, _ | _, z :: _ -> unique_ z
                                         ^
Error: This value is shared but expected to be unique.
|}]

let or_patterns2 : float list -> unique_ float list -> float =
  fun x y -> match x, y with
  | z :: _, _ | _, z :: _ -> unique_ z
  | _, _ -> 42.0
[%%expect{|
Line 3, characters 37-38:
3 |   | z :: _, _ | _, z :: _ -> unique_ z
                                         ^
Error: This value is shared but expected to be unique.
|}]

let or_patterns3 p =
  let unique_ x = 3 in let unique_ y = 4 in
  match p, x, y with
  | true, z, _ | false, _, z -> let _ = unique_id z in unique_id y
[%%expect{|
Line 4, characters 65-66:
4 |   | true, z, _ | false, _, z -> let _ = unique_id z in unique_id y
                                                                     ^
Error: This value is used here, but it has already been used as unique:
Line 4, characters 50-51:
4 |   | true, z, _ | false, _, z -> let _ = unique_id z in unique_id y
                                                      ^

|}]

let or_patterns4 p =
  let unique_ x = 3 in let unique_ y = 4 in
  match p, x, y with
  | true, z, _ | false, _, z -> let _ = unique_id x in unique_id y
[%%expect{|
val or_patterns4 : bool -> int = <fun>
|}]

let or_patterns5 p =
  let unique_ x = 3 in let unique_ y = 4 in
  match p, x, y with
  | true, z, _ | false, _, z -> let _ = unique_id z in unique_id x
[%%expect{|
Line 4, characters 65-66:
4 |   | true, z, _ | false, _, z -> let _ = unique_id z in unique_id x
                                                                     ^
Error: This value is used here, but it has already been used as unique:
Line 4, characters 50-51:
4 |   | true, z, _ | false, _, z -> let _ = unique_id z in unique_id x
                                                      ^

|}]

(* for some reason the following error message is missing "another use". Don't
know what's wrong *)
let mark_top_shared =
  let unique_ xs = 2 :: 3 :: [] in
  match xs with
  | x :: xx ->
      let _ = unique_id xs in
      unique_ xx
  | [] -> []
[%%expect{|
Line 6, characters 6-16:
6 |       unique_ xx
          ^^^^^^^^^^
Error: This value is used here,
       but it is part of a value that has already been used as unique:
Line 5, characters 24-26:
5 |       let _ = unique_id xs in
                            ^^

|}]

let mark_top_shared =
  let unique_ xs = 2 :: 3 :: [] in
  let _ = unique_id xs in
  match xs with
  | x :: xx -> unique_ xx
  | [] -> []
[%%expect{|
Line 5, characters 4-11:
5 |   | x :: xx -> unique_ xx
        ^^^^^^^
Error: This value is read from here, but it has already been used as unique:
Line 3, characters 20-22:
3 |   let _ = unique_id xs in
                        ^^

|}]

let mark_shared_in_one_branch b x =
  if b then unique_id (x, 3.0)
       else (x, x)
[%%expect{|
val mark_shared_in_one_branch : bool -> unique_ float -> float * float =
  <fun>
|}]

let mark_shared_in_one_branch b x =
  if b then (x, x)
       else unique_id (x, 3.0)
[%%expect{|
val mark_shared_in_one_branch : bool -> unique_ float -> float * float =
  <fun>
|}]

let expr_tuple_match f x y =
  match f x, y with
  | (a, b), c -> unique_ (a, c)
[%%expect{|
val expr_tuple_match : ('a -> unique_ 'b * 'c) -> 'a -> unique_ 'd -> 'b * 'd =
  <fun>
|}]

let expr_tuple_match f x y =
  match f x, y with
  | (a, b) as t, c -> let d = unique_id t in unique_ (c, d)
[%%expect{|
val expr_tuple_match :
  ('a -> unique_ 'b * 'c) -> 'a -> unique_ 'd -> 'd * ('b * 'c) = <fun>
|}]

let expr_tuple_match f x y =
  match f x, y with
  | (a, b) as t, c -> let d = unique_id t in unique_ (a, d)
[%%expect{|
Line 3, characters 54-55:
3 |   | (a, b) as t, c -> let d = unique_id t in unique_ (a, d)
                                                          ^
Error: This value is used here,
       but it is part of a value that has already been used as unique:
Line 3, characters 40-41:
3 |   | (a, b) as t, c -> let d = unique_id t in unique_ (a, d)
                                            ^

|}]

let tuple_parent_marked a b =
  match (a, b) with
  | (_, b) as _t -> shared_id b
[%%expect{|
val tuple_parent_marked : 'a -> 'b -> 'b = <fun>
|}]

(* TODO: Improve UA so that the following example can be allowed. The intuition
  is that [as _t] in the second branch shouldn't interfere with the first
  branch. One way to fix this is to try to link [_t] to the path of `a` and `b`.
   *)
let tuple_parent_marked a b =
  match (a, b) with
  | (true, b') -> unique_id b'
  | (false, b') as _t -> shared_id b'
[%%expect{|
Line 3, characters 28-30:
3 |   | (true, b') -> unique_id b'
                                ^^
Error: This value is used here, but it has already been used as unique:
Line 2, characters 12-13:
2 |   match (a, b) with
                ^

|}]

let tuple_parent_marked a b =
  match (a, b) with
  | (false, b) as _t -> shared_id b
  | (true, b) -> unique_id b
[%%expect{|
Line 4, characters 27-28:
4 |   | (true, b) -> unique_id b
                               ^
Error: This value is used here, but it has already been used as unique:
Line 2, characters 12-13:
2 |   match (a, b) with
                ^

|}]

let unique_match_on a b =
  let unique_ t = (a, b) in t
[%%expect{|
val unique_match_on : unique_ 'a -> unique_ 'b -> 'a * 'b = <fun>
|}]

type ('a, 'b) record = { foo : 'a; bar : 'b }
[%%expect{|
type ('a, 'b) record = { foo : 'a; bar : 'b; }
|}]

let match_function : unique_ 'a * 'b -> 'a * ('a * 'b) =
  function
  | (a, b) as t -> unique_ (a, t)
[%%expect{|
Line 3, characters 31-32:
3 |   | (a, b) as t -> unique_ (a, t)
                                   ^
Error: This value is used here,
       but part of it has already been used as unique:
Line 3, characters 28-29:
3 |   | (a, b) as t -> unique_ (a, t)
                                ^

|}]

let tuple_parent_marked a b =
  match (a, b) with
  | ((_, a), b) as t -> unique_ (a, t)
[%%expect{|
Line 3, characters 36-37:
3 |   | ((_, a), b) as t -> unique_ (a, t)
                                        ^
Error: This value is used here,
       but part of it has already been used as unique:
Line 3, characters 33-34:
3 |   | ((_, a), b) as t -> unique_ (a, t)
                                     ^

|}]

(* CR-someday anlorenzen: This one shouldn't fail in a more clever analysis. *)
let or_patterns6 flag f x y =
  match flag, f x, y with
  | true, a, (_, b) | false, b, (_, a) -> (unique_id a, unique_id b)
[%%expect{|
Line 3, characters 66-67:
3 |   | true, a, (_, b) | false, b, (_, a) -> (unique_id a, unique_id b)
                                                                      ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 53-54:
3 |   | true, a, (_, b) | false, b, (_, a) -> (unique_id a, unique_id b)
                                                         ^

|}]


type point = { dim : int; x : float; y : float; z : float }
[%%expect{|
type point = { dim : int; x : float; y : float; z : float; }
|}]

let record_mode_vars (p : point) =
  let x = unique_id p.x in
  let y = (p.y, p.y) in
  (x, y, unique_ p.z)
[%%expect{|
val record_mode_vars : unique_ point -> float * (float * float) * float =
  <fun>
|}]

let record_mode_vars (p : point) =
  let x = unique_id p.x in
  let y = (p.x, p.y) in
  (x, y, unique_ p.z)
[%%expect{|
Line 3, characters 11-14:
3 |   let y = (p.x, p.y) in
               ^^^
Error: This value is used here, but it has already been used as unique:
Line 2, characters 20-23:
2 |   let x = unique_id p.x in
                        ^^^

|}]

let record_mode_vars (p : point) =
  let y = (p.x, p.y) in
  let x = unique_id p.x in
  (x, y, unique_ p.z)
[%%expect{|
Line 3, characters 20-23:
3 |   let x = unique_id p.x in
                        ^^^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 11-14:
2 |   let y = (p.x, p.y) in
               ^^^

|}]

(* testing Texp_function; closure over implicit borrowing *)
let foo () =
  let unique_ r = {dim=1; x=2.0; y=3.0; z=4.0} in
  let _bar () = match r with
    | {dim; x; y; z} -> ()
   in
  unique_id r
[%%expect{|
Line 6, characters 12-13:
6 |   unique_id r
                ^
Error: This value is used here as unique,
       but it has already been read from in a closure that might be called later:
Line 4, characters 6-20:
4 |     | {dim; x; y; z} -> ()
          ^^^^^^^^^^^^^^

|}]

(* not closing over is fine *)
let foo () =
  let r = {dim=1; x=2.0; y=3.0; z=4.0} in
  match r with
  | {dim; x; y; z} -> ()
  ;
  unique_id r
[%%expect{|
val foo : unit -> point = <fun>
|}]

let foo () =
  let unique_ r = {dim=1; x=2.0; y=3.0; z=4.0} in
  let _l = lazy (r.z) in
  unique_id r
[%%expect{|
Line 4, characters 12-13:
4 |   unique_id r
                ^
Error: This value is used here as unique,
       but it has already been read from in a closure that might be called later:
Line 3, characters 17-18:
3 |   let _l = lazy (r.z) in
                     ^

|}]


type mfoo = {
  mutable a : string;
  b : string;
}

(* testing Texp_setfield *)

(* the following is bad - unique_id could strongly update x *)
let foo () =
  let x = {a = "hello"; b = "world"} in
  ignore (unique_id x);
  x.a <- "olleh"
[%%expect{|
type mfoo = { mutable a : string; b : string; }
Line 12, characters 2-3:
12 |   x.a <- "olleh"
       ^
Error: This value is written to here, but it has already been used as unique:
Line 11, characters 20-21:
11 |   ignore (unique_id x);
                         ^

|}]

let foo () =
  let x = {a = "hello"; b = "world"} in
  x.a <- "olleh";
  ignore (unique_id x)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* the following is rather interesting - after uniquely used x.b, the x as a
whole is unusable. However, one can still use x.mem_addr and x.a *)
let foo () =
  let x = {a = "hello"; b = "world"} in
  ignore (unique_id x.b);
  x.a <- "olleh";
  ignore (shared_id x.a)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Testing modalities in records *)
type r_shared = {x : string; y : string @@ shared many}
[%%expect{|
type r_shared = { x : string; y : string @@ many shared; }
|}]

let foo () =
  let r = {x = "hello"; y = "world"} in
  ignore (shared_id r.y);
  (* the following is allowed, because using r uniquely implies using r.x
     shared *)
  ignore (unique_id r)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

 (* Similarly for linearity *)
let foo () =
  let r = once_ {x = "hello"; y = "world"} in
  ignore_once r.y;
  ignore_once r;
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let r = once_ {x = "hello"; y = "world"} in
  ignore_once r.x;
  ignore_once r;
[%%expect{|
Line 4, characters 14-15:
4 |   ignore_once r;
                  ^
Error: This value is used here,
       but part of it is defined as once and has already been used:
Line 3, characters 14-17:
3 |   ignore_once r.x;
                  ^^^

|}]

let foo () =
  let r = {x = "hello"; y = "world"} in
  ignore (shared_id r.x);
  (* doesn't work for normal fields *)
  ignore (unique_id r)
[%%expect{|
Line 5, characters 20-21:
5 |   ignore (unique_id r)
                        ^
Error: This value is used here as unique,
       but part of it has already been used:
Line 3, characters 20-23:
3 |   ignore (shared_id r.x);
                        ^^^

|}]

(* testing record update in the presense of modalities *)
let foo () =
  let r = {x = "hello"; y = "world"} in
  ignore (unique_ {r with x = "hello agin"});
  (* r.y has been used shared; in the following we will use r as unique *)
  ignore (unique_id r)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let r = {x = "hello"; y = "world"} in
  ignore (unique_ {r with y = "world again"});
  (* r.x has been used unique; in the following we will use r as unique *)
  ignore (unique_id r)
[%%expect{|
Line 5, characters 20-21:
5 |   ignore (unique_id r)
                        ^
Error: This value is used here,
       but part of it has already been used as unique:
Line 3, characters 19-20:
3 |   ignore (unique_ {r with y = "world again"});
                       ^

|}]

(* testing modalities in constructors *)
type r_shared = R_shared of string * string @@ shared many
[%%expect{|
type r_shared = R_shared of string * string @@ many shared
|}]

let foo () =
  let r = R_shared ("hello", "world") in
  let R_shared (_, y) = r in
  ignore (shared_id y);
  (* the following is allowed, because using r uniquely implies using r.x
     shared *)
  ignore (unique_id r)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

 (* Similarly for linearity *)
let foo () =
  let r = once_ (R_shared ("hello", "world")) in
  let R_shared (_, y) = r in
  ignore_once y;
  ignore_once r;
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let r = once_ (R_shared ("hello", "world")) in
  let R_shared (x, _) = r in
  ignore_once x;
  ignore_once r;
[%%expect{|
Line 5, characters 14-15:
5 |   ignore_once r;
                  ^
Error: This value is used here,
       but part of it is defined as once and has already been used:
Line 4, characters 14-15:
4 |   ignore_once x;
                  ^

|}]

let foo () =
  let r = R_shared ("hello", "world") in
  let R_shared (x, _) = r in
  ignore (shared_id x);
  (* doesn't work for normal fields *)
  ignore (unique_id r)
[%%expect{|
Line 6, characters 20-21:
6 |   ignore (unique_id r)
                        ^
Error: This value is used here as unique,
       but part of it has already been used:
Line 4, characters 20-21:
4 |   ignore (shared_id x);
                        ^

|}]

(* updating record at least reads the memory_address of the record *)
type r = {x : string; y : string}
let foo () =
  let r = {x = "hello"; y = "world" } in
  ignore (unique_id r);
  ignore ({r with x = "hello again"; y = "world again"})
[%%expect{|
type r = { x : string; y : string; }
Line 5, characters 9-56:
5 |   ignore ({r with x = "hello again"; y = "world again"})
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 23 [useless-record-with]: all the fields are explicitly listed in this record:
the 'with' clause is useless.

Line 5, characters 11-12:
5 |   ignore ({r with x = "hello again"; y = "world again"})
               ^
Error: This value is read from here, but it has already been used as unique:
Line 4, characters 20-21:
4 |   ignore (unique_id r);
                        ^

|}]

type r = {x : float; y : float}

(* CR zqian: The following should pass but doesn't, because the uniqueness
   analysis doesn't support mode crossing. The following involes sequencing the
   maybe_unique usage of [r.x] and the maybe_unique usage of [r] as a whole.
   Sequencing them will force both to be shared and many. The [unique_use] in
   [r.x] is mode-crossed (being an unboxed float) so is fine. The [unique_use]
   in [r] cannot cross mode, and forcing it causes error. *)

let foo () =
  let r = {x = 3.0; y = 5.0} in
  let x = r.x in
  ignore (unique_id r);
  (* [x] is allocated fresh, unrelated to [r]. *)
  ignore (unique_id x)
[%%expect{|
type r = { x : float; y : float; }
Line 13, characters 20-21:
13 |   ignore (unique_id r);
                         ^
Error: This value is used here,
       but part of it has already been used as unique:
Line 12, characters 10-13:
12 |   let x = r.x in
               ^^^

|}]

let foo () =
  let r = {x = 3.0; y = 5.0} in
  ignore (unique_id r);
  (* but projection still uses [r]'s mem block, of course *)
  let x = r.x in
  ignore (unique_id x)
[%%expect{|
Line 5, characters 10-11:
5 |   let x = r.x in
              ^
Error: This value is read from here, but it has already been used as unique:
Line 3, characters 20-21:
3 |   ignore (unique_id r);
                        ^

|}]
