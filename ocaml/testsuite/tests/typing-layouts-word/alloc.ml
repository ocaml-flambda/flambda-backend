(* TEST
   * flambda2
   ** native
*)

(* A test comparing allocations when using unboxed [nativeint#]es to allocations
   when using boxed [nativeints]. *)

(* Hide polymorphic equality *)
let ( = ) = Int.equal

module Nativeint = struct
  include Nativeint

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( = ) = equal
end

module Nativeint_u = struct
  include Stdlib__Nativeint_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( = ) = equal
end

let baseline_allocation =
  let first  = Gc.allocated_bytes () in
  let second = Gc.allocated_bytes () in
  second -. first

let measure_alloc fmt f =
  let before = Gc.allocated_bytes () in
  let result = (f[@inlined never]) () in
  let after = Gc.allocated_bytes () in
  let alloc = (after -. before) -. baseline_allocation in
  Printf.printf (fmt ^^ "; %s\n")
    (Nativeint_u.to_nativeint result)
    (if alloc > 0.0 then "allocated" else "did not allocate")

(* We mark key functions [[@inline never]].  Without this, flambda2 might be
   able to eliminate all allocations in the boxed case, and it's important to
   have neither inlined for a fair comparison.  (This was the case in the
   [float64] version of this test. *)

module Collatz_unboxed = struct
  open Nativeint_u

  let[@inline never] rec collatz_count' count n =
    if n = of_nativeint 1n then
      count
    else
      collatz_count'
        (succ count)
        (if n %% of_nativeint 2n = of_nativeint 0n then
           n // of_nativeint 2n
         else
           of_nativeint 3n * n + of_nativeint 1n)

  let collatz_count n = collatz_count' (of_nativeint 0n) n

  let go () =
    measure_alloc "Unboxed: Collatz took %nd steps to reach 1"
      (fun () -> collatz_count (of_nativeint 27n))
end

module Collatz_boxed = struct
  open Nativeint

  let[@inline never] rec collatz_count' count n =
    if n = 1n then
      count
    else
      collatz_count'
        (succ count)
        (if n %% 2n = 0n then
           n // 2n
         else
           3n*n + 1n)

  let collatz_count n = Nativeint_u.of_nativeint (collatz_count' 0n n)

  let go () =
    measure_alloc "Boxed: Collatz took %nd steps to reach 1"
      (fun () -> collatz_count 27n)
end

let () = Collatz_unboxed.go ()
let () = Collatz_boxed.go ()

let[@inline never] literal_test x y =
  let open Nativeint_u in
  let[@inline never] f x y = (#1n + x) * (y - #4n) in
  match x with
  | #2n | #0x7fffffffffffffffn ->  (f x y) / (#3n % #10n)
  | _ -> #0n

let _ = measure_alloc "literals (should be -1): %nd" (fun () -> literal_test #2n #3n)
let _ = measure_alloc "literals (should be -3074457345618258602): %nd"
          (fun () -> literal_test #0x7fffffffffffffffn #0x7fffffffffffffffn)
