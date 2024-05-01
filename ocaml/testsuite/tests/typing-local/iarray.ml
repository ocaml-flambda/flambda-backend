(* TEST
 {
   reference = "${test_source_directory}/iarray.byte.reference";
   bytecode;
 }{
   stack-allocation;
   reference = "${test_source_directory}/iarray.stack.reference";
   native;
 }{
   no-stack-allocation;
   reference = "${test_source_directory}/iarray.heap.reference";
   native;
 }
*)

(* Testing all the [iarray] functions that allocate [iarray]s locally (not
   including multiple variants of the same function) for:

   1. Safety: No forward pointers on the local stack.

   2. Correctness: They actually create arrays on the stack (by testing that no
      GCed allocation happens). *)

module Iarray = Stdlib__Iarray

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

let ignore_local : local_ 'a -> unit = fun x ->
  let _ = local_ opaque_local x in
  ()

let local_some : 'a -> local_ 'a option = fun x -> local_ Some x

let run name f x = local_
  let prebefore = Gc.allocated_bytes () in
  let before = Gc.allocated_bytes () in
  let r = Sys.opaque_identity f x in
  let after = Gc.allocated_bytes () in
  let delta =
    int_of_float ((after -. before) -. (before -. prebefore))
      / (Sys.word_size/8)
  in
  let msg =
    match delta with
    | 0 -> "No Allocation"
    | n -> "Allocation"
  in
  Printf.printf "%20s: %s\n" name msg;
  r

(* Testing functions *)

let test_init n = local_ Iarray.init_local n local_some

let test_append (a1, a2) = local_ Iarray.append_local a1 a2

let test_concat = Iarray.concat_local

let test_sub a = local_ Iarray.sub_local a 0 3

let test_to_list = Iarray.to_list_local

let test_of_list = Iarray.of_list_local

let test_map a = local_
  Iarray.map_local
    (function Some x -> local_ Some (-x) | None -> local_ Some 0)
    a

let test_mapi a = local_ Iarray.mapi_local (fun i x -> local_ i, x) a

let test_fold_left a = local_
  Iarray.fold_left_local (fun l x -> local_ x :: l) [] a

let test_fold_left_map a = local_
  Iarray.fold_left_map_local (fun l x -> local_ x :: l, Some x) [] a

let test_fold_right a = local_
  Iarray.fold_right_local (fun x l -> local_ x :: l) a []

let test_map2 (a1, a2) = local_ Iarray.map2_local (fun x y -> local_ x, y) a1 a2

let test_split = Iarray.split_local

let test_combine (a1, a2) = local_ Iarray.combine_local a1 a2

(* Run the test, keeping the values alive *)
let () =
  let local_ r0 = run "init_local"
    test_init
    42
  in
  let local_ r1 = run "append_local"
    test_append
    (Iarray.init_local 42 local_some, [:None; Some (-1); Some (-2):])
  in
  let local_ r2 = run "concat_local"
    test_concat
    [Iarray.init_local 42 local_some; [:None; Some (-1); Some (-2):]]
  in
  let local_ r3 = run "sub_local"
    test_sub
    (Iarray.init_local 42 local_some)
  in
  let local_ r4 = run "to_list_local"
    test_to_list (Iarray.init_local 42 local_some)
  in
  let local_ r5 = run "of_list_local"
    test_of_list [Some 0; Some 1; Some 2; Some 3; Some 4; Some 5]
  in
  let local_ r6 = run "map_local"
    test_map
    (Iarray.init_local 42 local_some)
  in
  let local_ r7 = run "mapi_local"
    test_mapi
    (Iarray.init_local 42 local_some)
  in
  let local_ r8 = run "fold_left_local"
    test_fold_left
    (Iarray.init_local 42 local_some)
  in
  let local_ r9 = run "fold_left_map_local"
    test_fold_left_map
    (Iarray.init_local 42 local_some)
  in
  let local_ rA = run "fold_right_local"
    test_fold_right
    (Iarray.init_local 42 local_some)
  in
  let local_ rB = run "map2_local"
    test_map2
    (Iarray.init_local 3 local_some, [:None; Some (-1); Some (-2):])
  in
  let local_ rC = run "split_local"
    test_split
    (Iarray.init_local 42 (fun i -> local_ Some i, Some (-i)))
  in
  let local_ rD = run "combine_local"
    test_combine
    (Iarray.init_local 3 local_some, [:None; Some (-1); Some (-2):])
  in
  (* In debug mode, Gc.minor () checks for backwards local pointers and minor
     heap->local pointers (though we're more concerned about the former) *)
  Gc.minor ();
  ignore_local (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, rA, rB, rC, rD);
  ()
