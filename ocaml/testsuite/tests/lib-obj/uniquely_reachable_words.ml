(* TEST
*)

let native =
  match Sys.backend_type with
  | Sys.Native -> true
  | Sys.Bytecode -> false
  | Sys.Other s -> print_endline s; assert false

let sizes xs = Obj.uniquely_reachable_words (List.map Obj.repr xs |> Array.of_list)
  |> Array.to_list

let deduce_reachable size =
  let reachable = ref []
  and cur = ref 0
  and binary = ref ((size + 50) / 100) in
  while !binary > 0 do
    if !binary land 1 = 1 then
      reachable := !cur :: !reachable;
    cur := !cur + 1;
    binary := !binary / 2
  done;
  List.rev !reachable

let expect_reachable roots expected :unit=
  let actual = sizes roots in
  List.combine (List.map deduce_reachable actual) expected
  |> List.iter (fun (a, e) ->
    if List.(a <> e) then
      let string_of_arr x = List.map string_of_int x |> String.concat "," in
      Printf.printf "actual = %s; expected = %s\n" (string_of_arr a) (string_of_arr e))

type node = { id: int; used_memory: int array; mutable children: node list }
let make id ch = { id; used_memory = Array.make (Int.shift_left 100 id) 0; children = ch }

(* Note that this all needs to be in a function to ensure our nodes actually get
   allocated on the heap and are not static values in the binary (whose size we
   would not count) *)
let f () =
  let n10 = make 10 [] in
  let n9 = make 9 [] in
  let n8 = make 8 [] in
  let n7 = make 7 [n8] in
  let n6 = make 6 [n7; n8] in
  n7.children <- n6 :: n7.children;
  let n5 = make 5 [n9] in
  let n4 = make 4 [n6] in
  let n3 = make 3 [] in
  let n2 = make 2 [n3; n4] in
  let n1 = make 1 [n5; n10] in
  let n0 = make 0 [n3; n5] in
  let n14 = make 14 [] in
  let n13 = make 13 [n14] in
  let n12 = make 12 [n14] in
  let n11 = make 11 [n12; n13] in
  (*  /-> 10
   * 1 --> 5 --> 9
   *   /
   * 0 --> 3       ->8<
   *   /          /    \
   * 2 --> 4 --> 6 <--> 7
   *
   *   /-> 12 >--\
   * 11 -> 13 >- 14
   *)
  expect_reachable [n0; n1; n2] [[0]; [1; 10]; [2; 4; 6; 7; 8]]; (* Proper roots *)
  expect_reachable [n0; n2; n1] [[0]; [2; 4; 6; 7; 8]; [1; 10]]; (* check permutation doesn't matter *)
  expect_reachable [n1; n0; n2] [[1; 10]; [0]; [2; 4; 6; 7; 8]];
  expect_reachable [n1; n2; n0] [[1; 10]; [2; 4; 6; 7; 8]; [0]];
  expect_reachable [n2; n0; n1] [[2; 4; 6; 7; 8]; [0]; [1; 10]];
  expect_reachable [n2; n1; n0] [[2; 4; 6; 7; 8]; [1; 10]; [0]];
  expect_reachable [n1; n2] [[1; 5; 9; 10]; [2; 3; 4; 6; 7; 8]];
  expect_reachable [n0; n2] [[0; 5; 9]; [2; 4; 6; 7; 8]];
  expect_reachable [n0; n1] [[0; 3]; [1; 10]];

  expect_reachable [n6; n7] [[6]; [7]]; (* Cycles between roots *)
  expect_reachable [n6; n7; n2] [[6]; [7]; [2; 3; 4]];
  expect_reachable [n6; n7; n8] [[6]; [7]; [8]];

  expect_reachable [n5; n9] [[5]; [9]]; (* Root is parent of another root *)
  expect_reachable [n5; n9; n3] [[5]; [9]; [3]];
  expect_reachable [n5; n9; n3; n0] [[5]; [9]; [3]; [0]];
  expect_reachable [n1; n10; n5] [[1]; [10]; [5; 9]];

  expect_reachable [n12; n13] [[12]; [13]]; (* Multiple owners *)
  expect_reachable [n12; n13; n14] [[12]; [13]; [14]];
  expect_reachable [n11; n12] [[11; 13]; [12]];
  expect_reachable [n12; n11] [[12]; [11; 13]];
  expect_reachable [n11; n12; n13] [[11]; [12]; [13]];
  expect_reachable [n11] [[11; 12; 13; 14]];

  expect_reachable [n8; n9; n10] [[8]; [9]; [10]]; (* Leaves *)

  ()

let () = f ()
