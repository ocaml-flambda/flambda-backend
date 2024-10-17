(* TEST
   flags += "-extension-universe alpha";
   expect;
   reference = "${test_source_directory}/overwriting_lift_constants.reference";
*)

(* CR uniqueness: To run this test replace 'expect' above by 'native'
   and delete the expect block. *)

type point = { mutable dim : int; x : float; y : float; z : float }

(* First: check that overwriting happens at all *)
let unsafe_dup : 'a @ unique -> 'a * 'a @ unique =
  Obj.magic (fun x -> (x, x))

let check_overwriting_enabled () =
  let (p, q) = unsafe_dup { dim = 3; x = 1.0; y = 2.0; z = 3.0 } in
  let p = overwrite_ p with { dim = 4; x = 1.0; y = 2.0; z = 3.0 } in
  p == q

(* This file tests how constants are lifted out.
   Unique constants can not be lifted out since they
   might be overwritten but all other constants should still
   be lifted out.
   Since this pass is not yet implemented, I have added the
   expected result from the prototype in a comment to each test.
*)

(* Same test for float blocks *)
type fpoint = { x : float; y : float; z : float }

let fupdate_with_constant (p : fpoint) =
  let q = overwrite_ p with { x = 2.0; y = 3.0; z = 4.0 } in
  q

let test2 () =
  let p = { x = 1.0; y = 2.0; z = 3.0 } in
  let q = { x = 1.0; y = 2.0; z = 3.0 } in
  fupdate_with_constant p == fupdate_with_constant q

(* The tail of the list should be lifted out *)
let constant_list x =
  x :: 2 :: []

(* While the head is different, the tails are the same *)
let test3 () =
  List.hd (constant_list 1) == List.hd (constant_list 2),
  List.tl (constant_list 1) == List.tl (constant_list 2)

(* Since the tail was marked unique, it can not be lifted out *)
let constant_list_unique x =
  let unique_ y = 2 :: [] in x :: y

let test4 () =
  List.hd (constant_list_unique 1) == List.hd (constant_list_unique 2),
  List.tl (constant_list_unique 1) == List.tl (constant_list_unique 2)

(* Since the tail was marked unique, it can not be lifted out *)
let constant_list_unique2 x =
  let unique_ z = [] in
  let unique_ y = 2 :: z in x :: y

let test5 () =
  List.hd (constant_list_unique2 1) == List.hd (constant_list_unique2 2),
  List.tl (constant_list_unique2 1) == List.tl (constant_list_unique2 2)

let () =
  Printf.printf "%B\n" (check_overwriting_enabled ());
  Printf.printf "%B\n" (test2 ());
  Printf.printf "(%B, %B)\n" (test3 ());
  Printf.printf "(%B, %B)\n" (test4 ());
  Printf.printf "(%B, %B)\n" (test5 ());

[%%expect{|
type point = { mutable dim : int; x : float; y : float; z : float; }
val unsafe_dup : unique_ '_a -> unique_ '_a * '_a @@ global many = <fun>
Line 9, characters 10-66:
9 |   let p = overwrite_ p with { dim = 4; x = 1.0; y = 2.0; z = 3.0 } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
