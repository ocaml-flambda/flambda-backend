(* TEST_BELOW *)

(* Unboxing of function params/returns is not available in classic mode,
   but we should at least check that it does not make the compiler crash. *)
[@@@ocaml.flambda_oclassic]

(* About testing for allocations.

   The call to `Gc.counters` itself allocates a bit (about 10 words: a 3-tuple
   plus some floats), so we will never have zero allocation. It is a bit
   fragile to check the exact number of words allocated, so instead we check
   that the number of allocated minor words is "not a lot more" than what
   `Gc.counters` allocates by using a reasonable threshold.

   To clearly differentiate the allocations from `Gc.counters` from those that
   are (potentially) in a function `f`, we run `f` in a loop enough times so
   that if it had any allocation, the number of minor words allocated would be
   a lot bigger than the threshold we check for. *)
let test_allocs test_name f arg1 arg2 =
  let (minor, promoted, major) = Gc.counters () in
  for i = 1 to 1_000 do
    if not (f arg1 arg2) then failwith "incorrect result"
  done;
  let (minor', promoted', major') = Gc.counters () in
  if minor' <= minor +. 20. && promoted = promoted' && major = major' then
    Format.printf "%s: allocs ok.@." test_name
  else
    Format.printf "%s: allocation check failed.@." test_name
[@@inline never]

(* Check unboxability of floats *)
module Floats = struct
  let[@unboxable] f (x[@unboxable]) y = x +. y [@@inline never]
  let g t y = let x = t +. 1. in f x y = 0. [@@inline never]
end

(* Check unboxability of Int32s *)
module Int32s = struct
  let[@unboxable] f (x[@unboxable]) y = Int32.add x y [@@inline never]
  let g t y = let x = Int32.add t Int32.one in f x y = Int32.zero [@@inline never]
end

(* Check unboxability of Int32s *)
module Int64s = struct
  let[@unboxable] f (x[@unboxable]) y = Int64.add x y [@@inline never]
  let g t y = let x = Int64.add t Int64.one in f x y = Int64.zero [@@inline never]
end

(* Check unboxability of Int32s *)
module Nativeints = struct
  let[@unboxable] f (x[@unboxable]) y = Nativeint.add x y [@@inline never]
  let g t y = let x = Nativeint.add t Nativeint.one in f x y = Nativeint.zero [@@inline never]
end

(* Check unboability of tuples *)
module Tuples = struct
  let[@unboxable] f ((x, y)[@unboxable]) = (y, x) [@@inline never]
  let g (a : int) (b : int) = let x, y = f (a, b) in x = b && y = a [@@inline never]
end

(* This does not work **as of now** *)
module Variants = struct
  type t = A of int | B of int
  let[@unboxable] f (t[@unboxable]) =
    match t with
    | A i -> B i
    | B i -> A i
  [@@inline never]

  let g i j =
    (match f (A i) with
    | B i' -> i = i'
    | A _ -> false) &&
    (match f (B j) with
    | A j' -> j = j'
    | B _ -> false)
  [@@inline never]
end

(* Actual tests *)
let () =
  test_allocs "floats" Floats.g 0. (- 1.);
  test_allocs "int32s" Int32s.g 0l (- 1l);
  test_allocs "int64s" Int64s.g 0L (- 1L);
  test_allocs "nativeints" Nativeints.g 0n (- 1n);
  test_allocs "tuples" Tuples.g 13 42;
  test_allocs "variants" Variants.g 13 42;
  ()

(* TEST
   flambda2;
   native;
*)
