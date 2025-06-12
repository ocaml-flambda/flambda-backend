(* TEST
   flambda2;
   native;
*)

(* Classic mode doesn't perform any lifting after inlining *)
[@@@ocaml.flambda_o3]

type t = { x : int64#; y : int option; }

let[@inline] f x y = { x; y }

let[@opaque] test () =
  let a = f #2L (Some 1) in
  a

let () =
  let bytes_start0 = Gc.allocated_bytes () in
  let bytes_start1 = Gc.allocated_bytes () in
  ignore (test ());
  let bytes_end = Gc.allocated_bytes () in
  assert (bytes_start0 +. bytes_end = 2. *. bytes_start1);
  ()
