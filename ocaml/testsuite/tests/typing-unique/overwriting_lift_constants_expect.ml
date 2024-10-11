(* TEST
   flags += "-extension-universe alpha";
   expect;
*)

type point = { dim : int; x : float; y : float; z : float }
[%%expect{|
type point = { dim : int; x : float; y : float; z : float; }
|}]

(* This file tests how constants are lifted out.
   Unique constants can not be lifted out since they
   might be overwritten but all other constants should still
   be lifted out.
   Since this pass is not yet implemented, I have added the
   expected result from the prototype in a comment to each test.
*)

(* When overwriting p with constants, this constant can be lifted out *)
let update_with_constant (p : point) =
  let q = overwrite_ p with { dim = 3; x = 2.0; y = 3.0; z = 4.0 } in
  q
[%%expect{|
Line 2, characters 10-66:
2 |   let q = overwrite_ p with { dim = 3; x = 2.0; y = 3.0; z = 4.0 } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
(*
[%%expect{|
val update_with_constant : unique_ point -> point = <fun>
|}]
*)

(* In this test, the update function returns the same value each time *)
let test =
  let p = { dim = 3; x = 1.0; y = 2.0; z = 3.0 } in
  let q = { dim = 3; x = 1.0; y = 2.0; z = 3.0 } in
  update_with_constant p == update_with_constant q
[%%expect{|
Line 4, characters 2-22:
4 |   update_with_constant p == update_with_constant q
      ^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "update_with_constant"
|}]
(*
[%%expect{|
val test : bool = true
|}]
*)

(* Floatblock *)
type fpoint = { x : float; y : float; z : float }
[%%expect{|
type fpoint = { x : float; y : float; z : float; }
|}]

(* When overwriting p with constants, this constant can be lifted out *)
let fupdate_with_constant (p : fpoint) =
  let q = overwrite_ p with { x = 2.0; y = 3.0; z = 4.0 } in
  q
[%%expect{|
Line 2, characters 10-57:
2 |   let q = overwrite_ p with { x = 2.0; y = 3.0; z = 4.0 } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
(*
[%%expect{|
val fupdate_with_constant : unique_ fpoint -> fpoint = <fun>
|}]
*)

(* In this test, the update function returns the same value each time *)
let test =
  let p = { x = 1.0; y = 2.0; z = 3.0 } in
  let q = { x = 1.0; y = 2.0; z = 3.0 } in
  fupdate_with_constant p == fupdate_with_constant q
[%%expect{|
Line 4, characters 2-23:
4 |   fupdate_with_constant p == fupdate_with_constant q
      ^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "fupdate_with_constant"
|}]
(*
[%%expect{|
val test : bool = true
|}]
*)

(* The tail of the list should be lifted out *)
let constant_list x =
  x :: 2 :: []
[%%expect{|
val constant_list : int -> int list @@ global many = <fun>
|}]
(*
[%%expect{|
val constant_list : int -> int list = <fun>
|}]
*)

(* While the head is different, the tails are the same *)
let test =
  List.hd (constant_list 1) == List.hd (constant_list 2),
  List.tl (constant_list 1) == List.tl (constant_list 2)
[%%expect{|
val test : bool * bool @@ global many = (false, true)
|}]
(*
[%%expect{|
val test : bool * bool = (false, true)
|}]
*)

(* Since the tail was marked unique, it can not be lifted out *)
(* CR: is this right? See next test. *)
let constant_list_unique x =
  let unique_ y = 2 :: [] in x :: y
[%%expect{|
val constant_list_unique : int -> int list @@ global many = <fun>
|}]
(*
[%%expect{|
val constant_list_unique : int -> int list = <fun>
|}]
*)

(* CR: I don't understand the old outcome of this test and
   it might have been a bug. I would expect (false, false) *)
let test =
  List.hd (constant_list_unique 1) == List.hd (constant_list_unique 2),
  List.tl (constant_list_unique 1) == List.tl (constant_list_unique 2)
[%%expect{|
val test : bool * bool @@ global many = (false, true)
|}]
(*
[%%expect{|
val test : bool * bool = (false, true)
|}]
*)

(* Since the tail was marked unique, it can not be lifted out *)
let constant_list_unique2 x =
  let unique_ z = [] in
  let unique_ y = 2 :: z in x :: y
[%%expect{|
val constant_list_unique2 : int -> int list @@ global many = <fun>
|}]
(*
[%%expect{|
val constant_list_unique2 : int -> int list = <fun>
|}]
*)

(* Since the tail is allocated fresh, it is not shared between invokations: *)
let test =
  List.hd (constant_list_unique2 1) == List.hd (constant_list_unique2 2),
  List.tl (constant_list_unique2 1) == List.tl (constant_list_unique2 2)
[%%expect{|
val test : bool * bool @@ global many = (false, false)
|}]
(*
[%%expect{|
val test : bool * bool = (false, false)
|}]
*)

(* We define a constant [p] which should not be lifted out *)
let constant_lift b =
  let unique_ p = { x = 1.0; y = 2.0; z = 3.0 } in
  if b then p else overwrite_ p with { x = 2.0 }
[%%expect{|
Line 3, characters 19-48:
3 |   if b then p else overwrite_ p with { x = 2.0 }
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
(*
[%%expect{|
val constant_lift : bool -> fpoint = <fun>
|}]
*)

(* If [p] was lifted out, the second and third call would have the same result *)
let test =
  ((constant_lift true).x, (constant_lift false).x, (constant_lift true).x)
[%%expect{|
Line 2, characters 4-17:
2 |   ((constant_lift true).x, (constant_lift false).x, (constant_lift true).x)
        ^^^^^^^^^^^^^
Error: Unbound value "constant_lift"
Hint: Did you mean "constant_list"?
|}]
(*
[%%expect{|
val test : float * float * float = (1., 2., 1.)
|}]
*)
