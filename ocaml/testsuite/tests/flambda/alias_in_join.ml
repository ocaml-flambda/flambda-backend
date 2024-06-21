(* TEST
 native;
*)

(* This test checks for an issue when joining two branches in flambda2.
   It is only relevant at higher optimisation levels. *)
[@@@ocaml.flambda_o3]

(* Original example extracted from real code.
   Triggers the bug when CSE equations on parameters are propagated. *)
let[@inline] m (x : float) y =
  if x > y then x else y

let[@inline] foo x c n =
  if x then c +. n else c -. n

let[@inline] g a b n x b' =
  let c = m a b in
  let d = foo x c n in  (* d is the boxed float that should be removed *)
  if b' then m 0. d else d

let[@opaque] h a b n x b' p =
  let r = g a b n x b' in
  p /. r

(* Simplified example triggering the bug without CSE *)
let[@opaque] f x y z =
  let[@local] k a b =
    a +. b
  in
  if x then
    let arg = y -. z in k arg arg
  else
    let arg = z -. y in k arg arg

(* Both examples are supposed to allocate a single float at the end *)
let check_cost =
  let x = Gc.allocated_bytes () in
  let y = Gc.allocated_bytes () in
  y -. x

let check_alloc f =
  let before = Gc.allocated_bytes () in
  ignore (f ());
  let after = Gc.allocated_bytes () in
  (* A single float allocation should consume 16 bytes *)
  assert (after -. before = check_cost +. 16.)

let () =
  check_alloc (fun () -> h 0. 1. 2. true false 3.);
  check_alloc (fun () -> f true 0. 1.);
  ()
