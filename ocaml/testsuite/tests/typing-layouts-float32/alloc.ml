(* TEST
 flambda2;
 {
   include beta;
   flags = "-extension-universe beta";
   native;
 }
*)

(* mshinwell: This test is now only run with flambda2, as the corresponding
   ocamltest predicate is reliable for testing whether this is an
   flambda-backend build. *)

(* A test comparing allocations with unboxed floats to allocations with boxed
   floats. *)

module Float32_u = struct
  include Beta.Float32_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( > ) x y = (compare x y) > 0
end

let alloc = ref 0.0

let measure_alloc f : float32# =
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
  open Float32_u

  let[@inline never] step n estimate =
    let new_term =
      ((-#1.s) ** n)
      / ((n * #2.s) + #1.s)
    in
    estimate + new_term

  let rec go n est =
    if n > #10000.s then est else go (n+ #1.s) (step n est)

  let estimate () =
    let est =
      measure_alloc (fun () -> go #0.s #0.s)
    in
    Printf.printf "Unboxed:\n  estimate: %f\n  allocations: %s\n"
      (Float32.to_float (to_float32 est) *. 4.) (get_allocations ())
end

module Pi_boxed =
struct
  open Float32
  open Operators

  let[@inline never] step n estimate =
    let new_term =
      (-1.s ** n) /. ((n *. 2.s) +. 1.s)
    in
    estimate +. new_term

  let rec go n est =
    if n > 10000.s then est else go (n+.1.s) (step n est)

  let estimate () =
    let est =
      measure_alloc (fun () -> Float32_u.of_float32 (go 0.s 0.s))
    in
    Printf.printf "Boxed:\n  estimate: %f\n  allocations: %s\n"
      (to_float ((Float32_u.to_float32 est) *. 4.s)) (get_allocations ())
end

let _ = Pi_unboxed.estimate ()
let _ = Pi_boxed.estimate ()

(* CR mslater: (float32) float32# record allocation tests *)
