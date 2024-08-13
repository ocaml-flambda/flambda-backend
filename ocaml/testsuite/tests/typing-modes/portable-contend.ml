(* TEST
   include stdlib_stable;
   expect;
*)

type r = {mutable a : bytes; b : bytes}

let best_bytes : unit -> bytes @ portable uncontended
    = Obj.magic (fun () -> Bytes.empty)
[%%expect{|
type r = { mutable a : bytes; b : bytes; }
val best_bytes : unit -> bytes @ portable = <fun>
|}]

(* TESTING records *)

(* Reading/writing mutable field from contended record is rejected. Also note
    that the mutation error precedes type error. *)
let foo (r @ contended) = r.a <- 42
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ contended) = r.a <- 42
                              ^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to write into the mutable fields,
  this record needs to be uncontended.
|}]

let foo (r @ contended) = r.a
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ contended) = r.a
                              ^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to read from the mutable fields,
  this record needs to be uncontended.
|}]

let foo (r @ contended) = {r with a = best_bytes ()}
[%%expect{|
val foo : r @ contended -> r @ contended = <fun>
|}]

let foo (r @ contended) = {r with b = best_bytes ()}
[%%expect{|
Line 1, characters 27-28:
1 | let foo (r @ contended) = {r with b = best_bytes ()}
                               ^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to read from the mutable fields,
  this record needs to be uncontended.
|}]

(* reading immutable field from contended record is fine *)
let foo (r @ contended) = r.b
[%%expect{|
val foo : r @ contended -> bytes @ contended = <fun>
|}]

(* Force top level to be uncontended and nonportable *)
let r @ contended = best_bytes ()
[%%expect{|
Line 1, characters 4-33:
1 | let r @ contended = best_bytes ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

let x @ portable = best_bytes ()

let y @ portable = x
[%%expect{|
val x : bytes = Bytes.of_string ""
Line 3, characters 19-20:
3 | let y @ portable = x
                       ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* Closing over writing mutable field gives nonportable *)
let foo () =
    let r = {a = best_bytes (); b = best_bytes ()} in
    let bar () = r.a <- best_bytes () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* Closing over reading mutable field gives nonportable *)
let foo () =
    let r = {a = best_bytes (); b = best_bytes ()} in
    let bar () = let _ = r.a in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* Closing over reading immutable field is OK *)
let foo () =
    let r @ portable = {a = best_bytes (); b = best_bytes ()} in
    let bar () = let _ = r.b in () in
    let _ @ portable = bar in
    ()
(* CR zqian: currently mutable(legacy) means all records constructed are nonportable,
   and the above bar is closing over an nonportable record. Once we allow mutable()
   syntax, we can test this. *)
[%%expect{|
Line 2, characters 23-61:
2 |     let r @ portable = {a = best_bytes (); b = best_bytes ()} in
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but expected to be "portable".
|}]


(* TESTING arrays *)
(* reading/writing to array requires uncontended *)
let foo (r @ contended) = Array.set r 42 (best_bytes ())
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.set r 42 (best_bytes ())
                                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]
let foo (r @ contended) = Array.get r 42
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.get r 42
                                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]
let foo (r @ contended) =
    match r with
    | [| x; y |] -> ()
[%%expect{|
Line 3, characters 6-16:
3 |     | [| x; y |] -> ()
          ^^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to read from the mutable fields,
  this record needs to be uncontended.
|}]


(* Closing over write gives nonportable *)
let foo () =
    let r = [| best_bytes (); best_bytes () |] in
    let bar () = Array.set r 0 (best_bytes ()) in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* Closing over read gives nonportable *)
let foo () =
    let r = [| best_bytes (); best_bytes () |] in
    let bar () = Array.get r 0 in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]


(* Closing over Array.length doesn't force nonportable; but that needs a
   modified stdlib. Once modified the test is trivial. So we omit that. *)


(* OTHER TESTS *)
(* Closing over uncontended but doesn't exploit that; the function is still
portable. *)
let foo () =
    let r @ portable uncontended = best_bytes () in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Closing over nonportable forces nonportable. *)
let foo () =
    let r @ nonportable = best_bytes () in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo : 'a @ nonportable contended -> ('a -> 'a) @ portable = fun a b -> best_bytes ()
[%%expect{|
Line 1, characters 64-88:
1 | let foo : 'a @ nonportable contended -> ('a -> 'a) @ portable = fun a b -> best_bytes ()
                                                                    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "nonportable",
       but expected to be "portable".
|}]

let foo : 'a @ uncontended portable -> (string -> string) @ portable = fun a b -> best_bytes ()
[%%expect{|
Line 1, characters 82-95:
1 | let foo : 'a @ uncontended portable -> (string -> string) @ portable = fun a b -> best_bytes ()
                                                                                      ^^^^^^^^^^^^^
Error: This expression has type "bytes" but an expression was expected of type
         "string"
|}]

let foo : 'a @ contended portable -> (string -> string) @ portable @@ nonportable contended = fun a b -> best_bytes ()
(* CR layouts v2.8: arrows should cross contention. *)
[%%expect{|
Line 1, characters 4-118:
1 | let foo : 'a @ contended portable -> (string -> string) @ portable @@ nonportable contended = fun a b -> best_bytes ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo : 'a @ contended portable -> (string -> string) @ portable @@ uncontended portable = fun a b -> best_bytes ()
[%%expect{|
Line 1, characters 104-114:
1 | let foo : 'a @ contended portable -> (string -> string) @ portable @@ uncontended portable = fun a b -> best_bytes ()
                                                                                                            ^^^^^^^^^^
Error: The value "best_bytes" is nonportable, so cannot be used inside a closure that is portable.
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
module Iarray = Stdlib_stable.Iarray

let foo (r @ contended) = Iarray.get r 42
(* CR zqian: The following should pass; the modal kind system should mode cross
iarray depending on the type of its element. *)
[%%expect{|
module Iarray = Stdlib_stable.Iarray
Line 3, characters 37-38:
3 | let foo (r @ contended) = Iarray.get r 42
                                         ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* CR zqian: add portable/uncontended modality and test. *)
