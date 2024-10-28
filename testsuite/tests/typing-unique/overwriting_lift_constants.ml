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

let pp_bool_tuple ppf (x, y) =
  Printf.fprintf ppf "(%B, %B)" x y

let () =
  Printf.printf "%B\n" (check_overwriting_enabled ());
  Printf.printf "%B\n" (test2 ());
  Printf.printf "%a\n" pp_bool_tuple (test3 ());
  Printf.printf "%a\n" pp_bool_tuple (test4 ());
  Printf.printf "%a\n" pp_bool_tuple (test5 ());

[%%expect{|
type point = { mutable dim : int; x : float; y : float; z : float; }
val unsafe_dup : '_a @ unique -> '_a * '_a @ unique @@ global many = <fun>
val check_overwriting_enabled : unit -> bool @@ global many = <fun>
type fpoint = { x : float; y : float; z : float; }
val fupdate_with_constant : fpoint @ unique -> fpoint @@ global many = <fun>
val test2 : unit -> bool @@ global many = <fun>
val constant_list : int -> int list @@ global many = <fun>
val test3 : unit -> bool * bool @@ global many = <fun>
val constant_list_unique : int -> int list @@ global many = <fun>
val test4 : unit -> bool * bool @@ global many = <fun>
val constant_list_unique2 : int -> int list @@ global many = <fun>
val test5 : unit -> bool * bool @@ global many = <fun>
val pp_bool_tuple : out_channel -> bool * bool -> unit @@ global many = <fun>
|}]
