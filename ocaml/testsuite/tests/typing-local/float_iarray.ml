(* TEST
 include stdlib_stable;
 {
   reference = "${test_source_directory}/float_iarray.heap.reference";
   bytecode;
 }{
   stack-allocation;
   reference = "${test_source_directory}/float_iarray.stack.reference";
   native;
 }{
   no-stack-allocation;
   reference = "${test_source_directory}/float_iarray.heap.reference";
   native;
 }
*)

(* Testing that local [float iarray]s don't allocate on access.  This is a
   question because for flat float arrays, accesses have to box the float. *)

module Iarray = Stdlib_stable.Iarray

let ( .:() ) = Iarray.( .:() )

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

let ignore_local : local_ 'a -> unit = fun x ->
  let _ = local_ opaque_local x in
  ()

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
  Printf.printf "%23s: %s\n" name msg;
  r

(* Testing functions *)

let test_access : local_ float iarray -> local_ float =
  fun iarr -> local_ iarr.:(0)

let test_match : local_ float iarray -> local_ float =
  fun iarr ->
  match iarr with
  | [: _; two; _ :] -> two
  | _ -> assert false

(* Run the test, keeping the values alive *)
let () =
  let local_ r0 = run "access from literal"
    test_access
    [: 2.7; 3.1; 1.0 :]
  in
  let local_ r1 = run "access from Iarray.init"
    test_access
    (Iarray.init_local 10 (fun i -> Float.of_int i))
  in
  (* TODO: Matching currently allocates, but that should be fixed eventually *)
  let local_ r2 = run "match on literal"
    test_match
    [: 2.7; 3.1; 1.0 :]
  in
  let local_ r3 = run "match on Iarray.init"
    test_match
    (Iarray.init_local 3 (fun i -> Float.of_int i))
  in
  ignore_local (r0, r1, r2, r3);
  ()
