(* TEST
   * expect
   flags = "-extension unique"
*)

(* mutable current means mutable(m0) where m0 = legacy = global, many, shared).
*)
type t = {mutable a : string }

let unique_use : 'a @ unique -> unit = fun _ -> ()

(* A function that forces everything to legacy. *)
let id_legacy : 'a -> 'a = fun a -> a
[%%expect{|
type t = { mutable a : string; }
val unique_use : unique_ 'a -> unit = <fun>
val id_legacy : 'a -> 'a = <fun>
|}]

(* Upon construction, the usual constraint applies: mode of the record will be
   the join of fields. *)
let foo (a @ local) =
    let r = {a} in
    r
[%%expect{|
Line 3, characters 4-5:
3 |     r
        ^
Error: This value escapes its region
  Hint: Cannot return a local value without an "exclave_" annotation
|}]

(* In addition, upon construction, mutable(m0) forces the comonadic fragment of
   the record to be higher than [m0]. Because [m0] currently is fixed to be
   [global, many] which is min, so this doesn't impose anything.
   *)
(* CR zqian: add tests when we have mutable(m0) syntax. *)

(* Upon projection, the usual constraint applies: mode of the projection will be
   higher than the record. *)
let foo (local_ r) =
    let a = r.a in
    a
[%%expect{|
val foo : local_ t -> local_ string = <fun>
|}]

(* In addition, upon projection, mutable(m0) forces the monadic fragment of the
   projection to be higher than m0. Note that [m0] is currently fixed to be
   [shared] which is max, so this is testable. *)
let foo (unique_ r) =
    unique_use r.a
[%%expect{|
Line 2, characters 15-18:
2 |     unique_use r.a
                   ^^^
Error: Found a shared value where a unique value was expected
|}]

(* Upon mutation, we simply require the new value to be constrained by [m0], and
 nothing else. The default [m0] means it requires [many, global], even if the
 record is [once, local]. *)
let foo () =
    let a @ local once = "hello" in
    let r @ local once = {a="world"} in
    r.a <- a
[%%expect{|
Line 4, characters 11-12:
4 |     r.a <- a
               ^
Error: This value escapes its region
|}]

(* The default [m0] also means mutation requires [shared], which is vacant. That
    means you can write a shared field to a unique record. *)
let foo () =
    let r @ unique = {a = "hello"} in
    let a = "world" in
    r.a <- id_legacy a
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* CR zqian: Similar tests for mutable array. *)
