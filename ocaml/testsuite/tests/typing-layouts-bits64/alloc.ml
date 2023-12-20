(* TEST
   * flambda2
   flags = "-extension layouts_alpha"
   ** native
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
    (if alloc > 0.0 then "allocated" else "did not allocate")

(* We mark key functions [[@inline never]].  Without this, flambda2 might be
   able to eliminate all allocations in the boxed case, and it's important to
   have neither inlined for a fair comparison.  (This was the case in the
   [float64] version of this test. *)

module Collatz_unboxed = struct
  open Int64_u

  let[@inline never] rec collatz_count' count n =
    if n = of_int64 1L then
      count
    else
      collatz_count'
        (succ count)
        (if n %% of_int64 2L = of_int64 0L then
           n // of_int64 2L
         else
           of_int64 3L * n + of_int64 1L)

  let collatz_count n = collatz_count' (of_int64 0L) n

  let go () =
    measure_alloc "Unboxed: Collatz took %Ld steps to reach 1"
      (fun () -> collatz_count (of_int64 27L))
end

module Collatz_boxed = struct
  open Int64

  let[@inline never] rec collatz_count' count n =
    if n = 1L then
      count
    else
      collatz_count'
        (succ count)
        (if n %% 2L = 0L then
           n // 2L
         else
           3L*n + 1L)

  let collatz_count n = Int64_u.of_int64 (collatz_count' 0L n)

  let go () =
    measure_alloc "Boxed: Collatz took %Ld steps to reach 1"
      (fun () -> collatz_count 27L)
end

let () = Collatz_unboxed.go ()
let () = Collatz_boxed.go ()

let[@inline never] literal_test x y =
  let open Int64_u in
  let[@inline never] f x y = (#1L + x) * (y - #4L) in
  match x with
  | #2L | #0x7fffffffffffffffL ->  (f x y) / (#3L % #10L)
  | _ -> #0L

let _ = measure_alloc "literals (should be -1): %Ld" (fun () -> literal_test #2L #3L)
let _ = measure_alloc "literals (should be -3074457345618258602): %Ld"
          (fun () -> literal_test #0x7fffffffffffffffL #0x7fffffffffffffffL)
