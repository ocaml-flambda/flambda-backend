(* TEST
   flags = "-extension layouts_alpha"
   * native
*)

(* A test comparing allocations when using unboxed [int64#]es to allocations
   when using boxed [int64s]. *)

(* Hide polymorphic equality *)
let ( = ) = Int.equal

module Int64 = struct
  include Int64

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( = ) = equal
end

module Int64_u = struct
  include Stdlib__Int64_u

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
    (Int64_u.to_int64 result)
    (if alloc > 0.0 then "allocated (" ^ Float.to_string alloc ^  ")" else "did not allocate")

(* We mark key functions [[@inline never]].  Without this, flambda2 might be
   able to eliminate all allocations in the boxed case, and it's important to
   have neither inlined for a fair comparison.  (This was the case in the
   [float64] version of this test. *)

module Collatz_unboxed = struct
  open Int64_u

  let[@inline never] rec collatz_count' count n =
    if n = of_int64 1n then
      count
    else
      collatz_count'
        (count + of_int64 1n)
        (if n %% of_int64 2n = of_int64 0n then
           n // of_int64 2n
         else
           of_int64 3n * n + of_int64 1n)

  let collatz_count n = collatz_count' (of_int64 0n) n

  let go () =
    measure_alloc "Unboxed: Collatz took %nd steps to reach 1"
      (fun () -> collatz_count (of_int64 27n))
end

module Collatz_boxed = struct
  open Int64

  let[@inline never] rec collatz_count' count n =
    if n = 1n then
      count
    else
      collatz_count'
        (count + 1n)
        (if n %% 2n = 0n then
           n // 2n
         else
           3n*n + 1n)

  let collatz_count n = Int64_u.of_int64 (collatz_count' 0n n)

  let go () =
    measure_alloc "Boxed: Collatz took %nd steps to reach 1"
      (fun () -> collatz_count 27n)
end

let () = Collatz_unboxed.go ()
let () = Collatz_boxed.go ()
