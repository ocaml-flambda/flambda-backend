(* TEST
   flags = "-extension layouts_alpha"
   * native
*)

(* A test comparing allocations with unboxed floats to allocations with boxed
   floats. *)

(* CR layouts v2: Delete this `Float_u` module and use the one we add to the
   standard library instead. *)
module type Float_u = sig
  external to_float : float# -> (float[@local_opt]) = "%box_float"
  external of_float : (float[@local_opt]) -> float# = "%unbox_float"

  val ( + ) : float# -> float# -> float#
  val ( - ) : float# -> float# -> float#
  val ( * ) : float# -> float# -> float#
  val ( / ) : float# -> float# -> float#
  val ( ** ) : float# -> float# -> float#
  val ( > ) : float# -> float# -> bool
end

module Float_u : Float_u = struct
  external to_float : float# -> (float[@local_opt]) = "%box_float"
  external of_float : (float[@local_opt]) -> float# = "%unbox_float"

  let ( + ) x y = of_float ((to_float x) +. (to_float y))
  let ( - ) x y = of_float ((to_float x) -. (to_float y))
  let ( * ) x y = of_float ((to_float x) *. (to_float y))
  let ( / ) x y = of_float ((to_float x) /. (to_float y))
  let ( ** ) x y = of_float ((to_float x) ** (to_float y))
  let ( > ) x y = (to_float x) > (to_float y)
end

let alloc = ref 0.0

let measure_alloc f =
  (* NB: right-to-left evaluation order gets this right *)
  let baseline_allocation = Gc.allocated_bytes() -. Gc.allocated_bytes() in
  let before = Gc.allocated_bytes () in
  let result = f () in
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
