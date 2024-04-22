(* TEST
   expect;
*)

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
Error: This value is contended but expected to be uncontended.
  Hint: In order to write into the mutable fields,
  this record needs to be uncontended.
|}]

let foo (r @ contended) = r.a
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ contended) = r.a
                              ^
Error: This value is contended but expected to be uncontended.
  Hint: In order to read from the mutable fields,
  this record needs to be uncontended.
|}]

(* reading immutable field from contended record is fine *)
let foo (r @ contended) = r.b
[%%expect{|
val foo : r @ contended -> string @ contended = <fun>
|}]

(* Force top level to be uncontended and nonportable *)
let r @ contended = "hello"
[%%expect{|
Line 1, characters 4-27:
1 | let r @ contended = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is contended but expected to be uncontended.
|}]

let x @ portable = "world"

let y @ portable = x
[%%expect{|
val x : string = "world"
Line 3, characters 19-20:
3 | let y @ portable = x
                       ^
Error: This value is nonportable but expected to be portable.
|}]

(* Closing over writing mutable field gives nonportable *)
let foo () =
    let r = {a = "foo"; b = "bar"} in
    let bar () = r.a <- "hello" in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is nonportable but expected to be portable.
|}]

(* Closing over reading mutable field gives nonportable *)
let foo () =
    let r = {a = "foo"; b = "bar"} in
    let bar () = let _ = r.a in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is nonportable but expected to be portable.
|}]

(* Closing over reading immutable field is OK *)
let foo () =
    let r @ portable = {a = "foo"; b = "bar"} in
    let bar () = let _ = r.b in () in
    let _ @ portable = bar in
    ()
(* CR zqian: currently mutable(legacy) means all records constructed are nonportable,
   and the above bar is closing over an nonportable record. Once we allow mutable()
   syntax, we can test this. *)
[%%expect{|
Line 2, characters 23-45:
2 |     let r @ portable = {a = "foo"; b = "bar"} in
                           ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is nonportable but expected to be portable.
|}]


(* TESTING arrays *)
(* reading/writing to array requires uncontended *)
let foo (r @ contended) = Array.set r 42 "hello"
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.set r 42 "hello"
                                        ^
Error: This value is contended but expected to be uncontended.
|}]
let foo (r @ contended) = Array.get r 42
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.get r 42
                                        ^
Error: This value is contended but expected to be uncontended.
|}]
let foo (r @ contended) =
    match r with
    | [| x; y |] -> ()
[%%expect{|
Line 3, characters 6-16:
3 |     | [| x; y |] -> ()
          ^^^^^^^^^^
Error: This value is contended but expected to be uncontended.
  Hint: In order to read from the mutable fields,
  this record needs to be uncontended.
|}]


(* Closing over write gives nonportable *)
let foo () =
    let r = [| "hello"; "world" |] in
    let bar () = Array.set r 0 "foo" in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is nonportable but expected to be portable.
|}]

(* Closing over read gives nonportable *)
let foo () =
    let r = [| "hello"; "world" |] in
    let bar () = Array.get r 0 in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is nonportable but expected to be portable.
|}]


(* Closing over measuring length doesn't force nonportable *)
let foo () =
    let r @ portable = [| "hello"; "world" |] in
    let bar () = Array.length r in
    let _ @ portable = bar in
    ()
(* CR zqian: The follows fails, because currently all records are constructed as
    nonportable. Once we support mutable() syntax, we can test this. *)
[%%expect{|
Line 2, characters 23-45:
2 |     let r @ portable = [| "hello"; "world" |] in
                           ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is nonportable but expected to be portable.
|}]


(* OTHER TESTS *)
(* Closing over uncontended but doesn't exploit that; the function is still
portable. *)
let foo () =
    let r @ portable uncontended = "hello" in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Closing over nonportable forces nonportable. *)
let foo () =
    let r @ nonportable = "hello" in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is nonportable but expected to be portable.
|}]

let foo : 'a @ nonportable contended -> ('a -> 'a) @ portable = fun a b -> "hello"
[%%expect{|
Line 1, characters 64-82:
1 | let foo : 'a @ nonportable contended -> ('a -> 'a) @ portable = fun a b -> "hello"
                                                                    ^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is nonportable,
       but expected to be portable.
|}]

let foo : 'a @ uncontended portable -> (string -> string) @ portable = fun a b -> "hello"
[%%expect{|
Line 1, characters 71-89:
1 | let foo : 'a @ uncontended portable -> (string -> string) @ portable = fun a b -> "hello"
                                                                           ^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is nonportable,
       but expected to be portable.
|}]

let foo : 'a @ contended portable -> (string -> string) @ portable @@ nonportable contended = fun a b -> "hello"
(* CR layout: arrows should cross contention. *)
[%%expect{|
Line 1, characters 4-112:
1 | let foo : 'a @ contended portable -> (string -> string) @ portable @@ nonportable contended = fun a b -> "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is contended but expected to be uncontended.
|}]

let foo : 'a @ contended portable -> (string -> string) @ portable @@ uncontended portable = fun a b -> "hello"
[%%expect{|
val foo : 'a @ portable contended -> (string -> string) @ portable = <fun>
|}]


(* immediates crosses portability and contention *)
let foo (x : int @@ nonportable) (y : int @@ contended) =
    let _ @ portable = x in
    let _ @ uncontended = y in
    ()
[%%expect{|
val foo : int -> int @ contended -> unit = <fun>
|}]

(* TESTING immutable array *)
module Iarray = Stdlib__Iarray

let foo (r @ contended) = Iarray.get r 42
(* CR zqian: The following should pass. let's not change [iarray.mli] to accept
contended iarray; instead, let the modal kind system to mode cross iarray. *)
[%%expect{|
module Iarray = Stdlib__Iarray
Line 3, characters 37-38:
3 | let foo (r @ contended) = Iarray.get r 42
                                         ^
Error: This value is contended but expected to be uncontended.
|}]



(* CR zqian: add portable/uncontended modality and test. *)
