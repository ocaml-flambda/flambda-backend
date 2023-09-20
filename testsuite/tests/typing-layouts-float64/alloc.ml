(* TEST
   flags = "-extension layouts_beta"
   * native
*)

(* A test comparing allocations with unboxed floats to allocations with boxed
   floats. *)

module Float_u = struct
  include Stdlib__Float_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( > ) x y = (compare x y) > 0
end

let alloc = ref 0.0

let measure_alloc f : float# =
  (* NB: right-to-left evaluation order gets this right *)
  let baseline_allocation = Gc.allocated_bytes() -. Gc.allocated_bytes() in
  let before = Gc.allocated_bytes () in
  let result = (f[@inlined never]) () in
  let after = Gc.allocated_bytes () in
  alloc := (after -. before) -. baseline_allocation;
  result

let measure_alloc_value f : 'a =
  (* NB: right-to-left evaluation order gets this right *)
  let baseline_allocation = Gc.allocated_bytes() -. Gc.allocated_bytes() in
  let before = Gc.allocated_bytes () in
  let result = (f[@inlined never]) () in
  let after = Gc.allocated_bytes () in
  alloc := (after -. before) -. baseline_allocation;
  result

(* We mark both [step] functions [@inline never].  Without this, flambda2 can
   eliminate all allocations in [Pi_boxed] (and we do it in [Pi_unboxed] for a
   fair comparison).  Despite the fact that flambda2 could eliminate these
   allocations, this seems like a reasonable test - it's not hard to imagine
   more complicated scenarios with functions that couldn't be inlined for other
   reasons.
*)

let get_allocations () =
  let result = if !alloc > 0. then "Allocated" else "Did not allocate" in
  alloc := 0.0;
  result

let get_exact_allocations () =
  let result = !alloc in
  alloc := 0.0;
  result

module Pi_unboxed =
struct
  open Float_u

  let[@inline never] step n estimate =
    let new_term =
      ((of_float (-1.)) ** n)
      / ((n * (of_float 2.)) + (of_float 1.))
    in
    estimate + new_term

  let rec go n est =
    if n > (of_float 10000.) then est else go (n+ (of_float 1.)) (step n est)

  let estimate () =
    let est =
      measure_alloc (fun () -> go (of_float 0.) (of_float 0.))
    in
    Printf.printf "Unboxed:\n  estimate: %f\n  allocations: %s\n"
      ((to_float est) *. 4.) (get_allocations ())
end

module Pi_boxed =
struct
  let[@inline never] step n estimate =
    let new_term =
      (-1. ** n) /. ((n *. 2.) +. 1.)
    in
    estimate +. new_term

  let rec go n est =
    if n > 10000. then est else go (n+.1.) (step n est)

  let estimate () =
    let est =
      measure_alloc (fun () -> Float_u.of_float (go 0. 0.))
    in
    Printf.printf "Boxed:\n  estimate: %f\n  allocations: %s\n"
      ((Float_u.to_float est) *. 4.) (get_allocations ())
end

let _ = Pi_unboxed.estimate ()
let _ = Pi_boxed.estimate ()

(**********************************)
(* float# record allocation tests *)
let[@inline never] consumer x y = Float_u.(x + y)

type t8 = { a : float#;
            mutable b : float#;
            c : float#;
            mutable d : float# }

let print_record_and_allocs s r =
  let allocs = get_exact_allocations () in
  Printf.printf
    "%s:\n  allocated bytes: %.2f\n  a: %.2f\n  b: %.2f\n  c: %.2f\n  d: %.2f\n"
    s allocs (Float_u.to_float r.a) (Float_u.to_float r.b)
    (Float_u.to_float r.c) (Float_u.to_float r.d)

(* Building a record should only allocate the box *)
let[@inline never] build x =
  { a = x;
    b = Float_u.of_float 42.0;
    c = consumer x x;
    d = consumer x (Float_u.of_float 1.0) }

let[@inline never] project_a r = r.a
let[@inline never] update_d r x = r.d <- x

(* We should be able to get floats out, do math on them, pass them to functions,
   etc, without allocating. *)
let[@inline never] manipulate ({c; _} as r) =
  match r with
  | { b; _ } ->
    update_d r (consumer b (project_a r));
    r.b <- Float_u.sub c (Float_u.of_float 21.1);
    r.a

let _ =
  let r = measure_alloc_value (fun () -> build (Float_u.of_float 3.14)) in
  print_record_and_allocs "Construction (40 bytes for record)" r;
  let _ = measure_alloc (fun () -> manipulate r) in
  print_record_and_allocs "Manipulation (0 bytes)" r


(* There was some concern the below would allocate when passed `false` due to
   CSE.  The idea is:
   - Projections like `r.a` were initially implemented as
     `unbox (box ( *(r+offset) ))`.  This box is supposed to be erased by the
     middle-end, but...
   - The boxes from the `true` branch could get lifted out of the `if` to be
     combined with the box from the projection, preventing it from being erased.
   Projections are no longer implemented that way, so there's no common
   subexpression to be eliminated, but I've kept the test.  *)
let[@inline never] cse_test b r =
  let (x : float#) = r.a in
  if b then
    (Float_u.to_float x, Float_u.to_float x)
  else
    (0., 0.)

let _ =
  let r = build (Float_u.of_float 3.14) in
  let _ = measure_alloc_value (fun () -> cse_test false r) in
  let allocs = get_exact_allocations () in
  Printf.printf "CSE test (0 bytes):\n  allocated bytes: %.2f\n" allocs
