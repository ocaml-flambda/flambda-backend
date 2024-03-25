(* TEST
   * expect *)

type r = {mutable a : string; b : string}

(* TESTING records *)

(* Reading/writing mutable field from contended record is rejected. Also note
    that the mutation error precedes type error. *)
let foo (r @ contended) = r.a <- 42
[%%expect{|
type r = { mutable a : string; b : string; }
Line 7, characters 26-27:
7 | let foo (r @ contended) = r.a <- 42
                              ^
Error: Found a contended value where a uncontended value was expected
|}]

let foo (r @ contended) = r.a
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ contended) = r.a
                              ^
Error: Found a contended value where a uncontended value was expected
|}]

(* reading immutable field from contended record is fine *)
let foo (r @ contended) = r.b
[%%expect{|
val foo : contended_ r -> contended_ string = <fun>
|}]

(* Force top level to be uncontended and unsync *)
let r @ contended = "hello"
[%%expect{|
Line 1, characters 4-27:
1 | let r @ contended = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^
Error: Found a contended value where a uncontended value was expected
|}]

let x @ sync = "world"

let y @ sync = x
[%%expect{|
val x : string = "world"
Line 3, characters 15-16:
3 | let y @ sync = x
                   ^
Error: Found a unsync value where a sync value was expected
|}]

(* Closing over writing mutable field gives unsync *)
let foo () =
    let r = {a = "foo"; b = "bar"} in
    let bar () = r.a <- "hello" in
    let _ @ sync = bar in
    ()
[%%expect{|
Line 4, characters 19-22:
4 |     let _ @ sync = bar in
                       ^^^
Error: Found a unsync value where a sync value was expected
|}]

(* Closing over reading mutable field gives unsync *)
let foo () =
    let r = {a = "foo"; b = "bar"} in
    let bar () = let _ = r.a in () in
    let _ @ sync = bar in
    ()
[%%expect{|
Line 4, characters 19-22:
4 |     let _ @ sync = bar in
                       ^^^
Error: Found a unsync value where a sync value was expected
|}]

(* Closing over reading immutable field is OK *)
let foo () =
    let r @ sync = {a = "foo"; b = "bar"} in
    let bar () = let _ = r.b in () in
    let _ @ sync = bar in
    ()
(* CR zqian: currently mutable(legacy) means all records constructed are unsync,
   and the above bar is closing over an unsync record. Once we allow mutable()
   syntax, we can test this. *)
[%%expect{|
Line 2, characters 19-41:
2 |     let r @ sync = {a = "foo"; b = "bar"} in
                       ^^^^^^^^^^^^^^^^^^^^^^
Error: Found a unsync value where a sync value was expected
|}]


(* TESTING arrays *)
(* reading/writing to array requires uncontended *)
let foo (r @ contended) = Array.set r 42 "hello"
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.set r 42 "hello"
                                        ^
Error: Found a contended value where a uncontended value was expected
|}]
let foo (r @ contended) = Array.get r 42
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.get r 42
                                        ^
Error: Found a contended value where a uncontended value was expected
|}]


(* Closing over write gives unsync *)
let foo () =
    let r = [| "hello"; "world" |] in
    let bar () = Array.set r 0 "foo" in
    let _ @ sync = bar in
    ()
[%%expect{|
Line 4, characters 19-22:
4 |     let _ @ sync = bar in
                       ^^^
Error: Found a unsync value where a sync value was expected
|}]

(* Closing over read gives unsync *)
let foo () =
    let r = [| "hello"; "world" |] in
    let bar () = Array.get r 0 in
    let _ @ sync = bar in
    ()
[%%expect{|
Line 4, characters 19-22:
4 |     let _ @ sync = bar in
                       ^^^
Error: Found a unsync value where a sync value was expected
|}]


(* Closing over measuring length doesn't force unsync *)
let foo () =
    let r @ sync = [| "hello"; "world" |] in
    let bar () = Array.length r in
    let _ @ sync = bar in
    ()
(* CR zqian: currently all records are constructed as unsync. Once we support
    mutable() syntax, we can test this.  *)
[%%expect{|
Line 2, characters 19-41:
2 |     let r @ sync = [| "hello"; "world" |] in
                       ^^^^^^^^^^^^^^^^^^^^^^
Error: Found a unsync value where a sync value was expected
|}]


(* OTHER TESTS *)
(* Closing over uncontended but doesn't exploit that; the function is still
sync. *)
let foo () =
    let r @ sync uncontended = "hello" in
    let bar () = let _ = r in () in
    let _ @ sync = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Closing over unsync forces unsync. *)
let foo () =
    let r @ unsync = "hello" in
    let bar () = let _ = r in () in
    let _ @ sync = bar in
    ()
[%%expect{|
Line 4, characters 19-22:
4 |     let _ @ sync = bar in
                       ^^^
Error: Found a unsync value where a sync value was expected
|}]

(* immediates crosses syncness and contention *)
let foo (x : int @@ unsync) (y : int @@ contended) =
    let _ @ sync = x in
    let _ @ uncontended = y in
    ()
[%%expect{|
val foo : int -> contended_ int -> unit = <fun>
|}]

(* TESTING immutable array *)
module Iarray = Stdlib__Iarray

let foo (r @ contended) = Iarray.get r 42
(* CR zqian: The following should pass. let's not change [iarray.mli] to accept
contended iarray; instead, let the modal kind system to mode cross them. *)
[%%expect{|
module Iarray = Stdlib__Iarray
Line 3, characters 37-38:
3 | let foo (r @ contended) = Iarray.get r 42
                                         ^
Error: Found a contended value where a uncontended value was expected
|}]

(* CR zqian: add sync/uncontended modality and test. *)
