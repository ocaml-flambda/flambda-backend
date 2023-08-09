(* TEST
*)

let native =
  match Sys.backend_type with
  | Sys.Native -> true
  | Sys.Bytecode -> false
  | Sys.Other s -> print_endline s; assert false

let sizes xs = Obj.uniquely_reachable_words (List.map Obj.repr xs)

let expect_sizes xs exp_bytecode exp_native =
  let actual = sizes xs in
  let expected = if native then exp_native else exp_bytecode in
  List.combine actual expected
  |> List.iteri (fun i (a, e) ->
    if a <> e then
      Printf.printf "index = %i; actual = %i; expected = %i\n" i a e)

type node = { id: int; used_memory: int list; mutable children: node list }
let make id ch = { id; used_memory = List.init (Int.shift_left 1 id) (fun _ -> id); children = ch }

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
  expect_sizes [n0; n1; n2] [13; 3092; 1445] [13; 3092; 1442]; (* Proper roots *)
  expect_sizes [n0; n2; n1] [13; 1445; 3092] [13; 1442; 3092]; (* check permutation doesn't matter *)
  expect_sizes [n1; n0; n2] [3092; 13; 1445] [3092; 13; 1442];
  expect_sizes [n1; n2; n0] [3092; 1445; 13] [3092; 1442; 13];
  expect_sizes [n2; n0; n1] [1445; 13; 3092] [1442; 13; 3092];
  expect_sizes [n2; n1; n0] [1445; 3092; 13] [1442; 3092; 13];
  expect_sizes [n1; n2] [4735; 1473] [4735; 1470];
  expect_sizes [n0; n2] [1656; 1445] [1656; 1442];
  expect_sizes [n0; n1] [41; 3092] [41; 3092];

  expect_sizes [n6; n7] [202; 394] [199; 391]; (* Cycles between roots *)
  expect_sizes [n6; n7; n2] [202; 394; 105] [199; 391; 105];
  expect_sizes [n6; n7; n8] [202; 394; 772] [199; 391; 772];

  expect_sizes [n5; n9] [103; 1540] [103; 1540]; (* Root is parent of another root *)
  expect_sizes [n5; n9; n3] [103; 1540; 28] [103; 1540; 28];
  expect_sizes [n5; n9; n3; n0] [103; 1540; 28; 13] [103; 1540; 28; 13];
  expect_sizes [n1; n10; n5] [16; 3076; 1643] [16; 3076; 1643];

  expect_sizes [n12; n13] [12295; 24583] [12292; 24580]; (* Multiple owners *)
  expect_sizes [n12; n13; n14] [12295; 24583; 49156] [12292; 24580; 49156];
  expect_sizes [n11; n12] [30737; 12295] [30734; 12292];
  expect_sizes [n12; n11] [12295; 30737] [12292; 30734];
  expect_sizes [n11; n12; n13] [6154; 12295; 24583] [6154; 12292; 24580];
  expect_sizes [n11] [92188] [92185];

  expect_sizes [n8; n9; n10] [772; 1540; 3076] [772; 1540; 3076]; (* Leaves *)

  print_endline "OK"

let () = f ()
