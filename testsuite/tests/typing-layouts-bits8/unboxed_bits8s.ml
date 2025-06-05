(* TEST
 include stdlib_beta;
 flambda2;
 {
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   native;
 }{
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   bytecode;
 }{
   flags = "-extension layouts_beta -extension small_numbers_beta";
   native;
 }{
   flags = "-extension layouts_beta -extension small_numbers_beta";
   bytecode;
 }
*)

(* This file contains various tests for [int8#].  It's not an expect test
   to make sure it gets tested for native code. *)

(*****************************************)
(* Prelude: Functions on unboxed int8s. *)

module Int8 = Stdlib_beta.Int8

module Int8_u = struct
  include Stdlib_beta.Int8_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( > ) x y = (compare x y) > 0
end

let to_binary_string x =
  String.init 8 (fun i ->
    if Int8.(equal (logand x (shift_left (Stdlib_beta.Int8.of_int 1) (8 - i - 1))) (Stdlib_beta.Int8.of_int 0))
    then '0'
    else '1')

let print_int prefix x =
  Printf.printf "%s: %d\n" prefix x

let print_int8u prefix x =
  Printf.printf "%s: %d\n" prefix (Int8_u.to_int x)

let print_int8u_bin prefix x =
  let bx = Int8_u.to_int8 x in
  Printf.printf "%s: %d = 0b%s\n" prefix (Int8_u.to_int x) (to_binary_string bx)

(*********************************)
(* Test 1: some basic arithmetic *)

(* Tests all the operators above *)
let test1 () =
  (* CR layouts: When bits8 defs are allowed at the module level, get rid of
     [test1] and move these definitions there. *)
  let open Int8_u in

  (* Positive numbers *)

  let three = (Stdlib_beta.Int8_u.of_int 3) in
  print_int8u "Test 1, three" three;

  let twice_three = three + (Stdlib_beta.Int8_u.of_int 3) in
  print_int8u "Test 1, twice_three" twice_three;

  let thrice_three = (Stdlib_beta.Int8_u.of_int 3) * three in
  print_int8u "Test 1, thrice_three" thrice_three;

  let twice_three_again = thrice_three - three in
  print_int8u "Test 1, twice_three_again" twice_three;

  let three_again = twice_three_again / (Stdlib_beta.Int8_u.of_int 2) in
  print_int8u "Test 1, three_again" three_again;

  let three_again_unsigned = twice_three_again // (Stdlib_beta.Int8_u.of_int 2) in
  print_int8u "Test 1, three_again_unsigned" three_again_unsigned;

  let twice_three_greater_than_three = twice_three > three in
  Printf.printf "Test 1, twice_three_greater_than_three: %b\n"
    twice_three_greater_than_three;

  let three_with_effort =
    ((Stdlib_beta.Int8_u.of_int 3) + twice_three) * (Stdlib_beta.Int8_u.of_int 2) / (Stdlib_beta.Int8_u.of_int 6) in
  print_int8u "Test 1, three_with_effort" three_with_effort;

  let seven_rem_three = (Stdlib_beta.Int8_u.of_int 7) % three in
  print_int8u "Test 1, seven_rem_three" seven_rem_three;

  let seven_rem_three_unsigned = (Stdlib_beta.Int8_u.of_int 7) %% three in
  print_int8u "Test 1, seven_rem_three_unsigned" seven_rem_three_unsigned;

  let forty_two_logand_three = logand (Stdlib_beta.Int8_u.of_int 42) three in
  print_int8u_bin "Test1, forty_two_logand_three (0b00101010 & 0b00000011)" forty_two_logand_three;

  let forty_two_logor_three = logor (Stdlib_beta.Int8_u.of_int 42) three in
  print_int8u_bin "Test1, forty_two_logor_three (0b00101010 & 0b00000011)" forty_two_logor_three;

  let forty_two_logxor_three = logxor (Stdlib_beta.Int8_u.of_int 42) three in
  print_int8u_bin "Test1, forty_two_logxor_three (0b00101010 & 0b00000011)" forty_two_logxor_three;

  let lognot_three = lognot three in
  print_int8u_bin "Test1, lognot_three (~0b00000011)" lognot_three;

  let three_shl_eight = shift_left three 8 in
  print_int8u_bin "Test1, three_shl_eight (0b00000011 << 8)" three_shl_eight;

  let three_shr_one = shift_right three 1 in
  print_int8u_bin "Test1, three_shr_one (0b00000011 >> 1)" three_shr_one;

  let three_shrl_one = shift_right_logical three 1 in
  print_int8u_bin "Test1, three_shr_one (0b00000011 >>> 1)" three_shrl_one;

  (* Negative numbers *)

  let minus_five = (Stdlib_beta.Int8_u.of_int (-5)) in
  print_int8u "Test 1, minus_five" minus_five;

  let twice_minus_five = minus_five + ((Stdlib_beta.Int8_u.of_int (-5))) in
  print_int8u "Test 1, twice_minus_five" twice_minus_five;

  let thrice_minus_five = (Stdlib_beta.Int8_u.of_int 3) * minus_five in
  print_int8u "Test 1, thrice_minus_five" thrice_minus_five;

  let twice_minus_five_again = thrice_minus_five - minus_five in
  print_int8u "Test 1, twice_minus_five_again" twice_minus_five;

  let minus_five_again = twice_minus_five_again / (Stdlib_beta.Int8_u.of_int 2) in
  print_int8u "Test 1, minus_five_again" minus_five_again;

  let minus_five_again_unsigned = twice_minus_five_again // (Stdlib_beta.Int8_u.of_int 2) in
  print_int8u "Test 1, minus_five_again_unsigned" minus_five_again_unsigned;

  let minus_five_greater_than_twice_minus_five = minus_five > twice_minus_five in
  Printf.printf "Test 1, minus_five_greater_than_twice_minus_five: %b\n"
    minus_five_greater_than_twice_minus_five;

  let minus_five_with_effort =
    (((Stdlib_beta.Int8_u.of_int ( -5 ))) + twice_minus_five) * (Stdlib_beta.Int8_u.of_int 2) / (Stdlib_beta.Int8_u.of_int 6) in
  print_int8u "Test 1, minus_five_with_effort" minus_five_with_effort;

  let seven_rem_minus_five = (Stdlib_beta.Int8_u.of_int 7) % minus_five in
  print_int8u "Test 1, seven_rem_minus_five" seven_rem_minus_five;

  let seven_rem_minus_five_unsigned = (Stdlib_beta.Int8_u.of_int 7) %% minus_five in
  print_int8u "Test 1, seven_rem_minus_five_unsigned" seven_rem_minus_five_unsigned;

  let forty_two_logand_minus_five = logand (Stdlib_beta.Int8_u.of_int 42) minus_five in
  print_int8u_bin "Test1, forty_two_logand_minus_five (0b00101010 & 0b1...1011)" forty_two_logand_minus_five;

  let forty_two_logor_minus_five = logor (Stdlib_beta.Int8_u.of_int 42) minus_five in
  print_int8u_bin "Test1, forty_two_logor_minus_five (0b00101010 & 0b1...1011)" forty_two_logor_minus_five;

  let forty_two_logxor_minus_five = logxor (Stdlib_beta.Int8_u.of_int 42) minus_five in
  print_int8u_bin "Test1, forty_two_logxor_minus_five (0b00101010 & 0b1...1011)" forty_two_logxor_minus_five;

  let lognot_minus_five = lognot minus_five in
  print_int8u_bin "Test1, lognot_minus_five (~0b1...1011)" lognot_minus_five;

  let minus_five_shl_eight = shift_left minus_five 8 in
  print_int8u_bin "Test1, minus_five_shl_eight (0b1...1011 << 8)" minus_five_shl_eight;

  let minus_five_shr_one = shift_right minus_five 1 in
  print_int8u_bin "Test1, minus_five_shr_one (0b1...1011 >> 1)" minus_five_shr_one;

  let minus_five_shrl_one = shift_right_logical minus_five 1 in
  print_int8u_bin "Test1, minus_five_shr_one (0b1...1011 >>> 1)" minus_five_shrl_one

  (* CR layouts: Restore these when the appropriate constants exist *)
  (* print_int8u "Test 1, max_int" max_int;
   * print_int8u "Test 1, min_int" min_int; *)

let _ = test1 ()

(**********************************)
(* Test 2: higher-order functions *)

(* CR layouts v1.5: This type definition can be eliminated once we have
   annotations. *)
type ('a : bits8) t_bits8 = 'a

let[@inline never] twice f (x : 'a t_bits8) = f (f x)
let[@inline never] compose f g (x : 'a t_bits8) = f (g x)

let[@inline never] twice_on_three f =
  let pi = (Stdlib_beta.Int8_u.of_int 3) in
  twice f pi

let times_four = twice Int8_u.(fun x -> x * (Stdlib_beta.Int8_u.of_int 2))

let _ =
  let open Int8_u in
  print_int8u "Test 2, add three twice"
    (twice (fun x -> x + (Stdlib_beta.Int8_u.of_int 3)) (Stdlib_beta.Int8_u.of_int 0));
  print_int8u "Test 2, add three four times"
    (twice (twice (fun x -> x + (Stdlib_beta.Int8_u.of_int 3))) (Stdlib_beta.Int8_u.of_int 0));
  print_int8u "Test 2, increment three twice"
    (twice_on_three (fun x -> (Stdlib_beta.Int8_u.of_int 1) + x));
  print_int8u "Test 2, increment three four times"
    (twice_on_three (twice (fun x -> (Stdlib_beta.Int8_u.of_int 1) + x)));
  print_int8u "Test 2, two times four"
    (times_four (Stdlib_beta.Int8_u.of_int 2));
  print_int8u "Test 2, three times sixteen"
    (twice_on_three times_four);
  print_int8u "Test 2, three times sixteen again"
    (compose times_four times_four (Stdlib_beta.Int8_u.of_int 3));
  print_int8u "Test 2, three minus four"
    (let two = twice (fun x -> x + (Stdlib_beta.Int8_u.of_int 1)) (Stdlib_beta.Int8_u.of_int 0) in
     let add_two = Int8_u.(+) two in
     let add_two_after = compose add_two in
     let minus_four = add_two_after (twice (fun x -> x - (Stdlib_beta.Int8_u.of_int 3))) in
     minus_four (Stdlib_beta.Int8_u.of_int 3))

(**********************************)
(* Test 3: int8# in closures *)

(* [go]'s closure should haave an [int] (immediate), a [int8#] (bits8) and a
   [int8 array] (value). *)
let[@inline never] f3 n m steps () =
  let[@inline never] rec go k =
    if k = n
    then (Stdlib_beta.Int8_u.of_int 0)
    else begin
      let acc = go (k + 1) in
      steps.(k) <- Int8_u.to_int8 acc;
      Int8_u.(+) m acc
    end
  in
  go 0

(* many args - even args are tuples, odd args are unboxed int8s *)
let[@inline_never] f3_manyargs x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 steps () =
  let (start_k, end_k) = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then (Stdlib_beta.Int8_u.of_int 0)
    else begin
      let (x2_1, x2_2) = x2 in
      let (x4_1, x4_2) = x4 in
      let (x6_1, x6_2) = x6 in
      let (x8_1, x8_2) = x8 in
      let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
      let acc = go (k + 1) in
      steps.(k) <- Int8_u.to_int8 acc;
      Int8_u.(acc + ((x1 + x3 + x5 + x7 + x9) * (of_int8 (Int8.of_int sum))))
    end
  in
  go start_k

let test3 () =
  (* Test f3 *)
  let steps = Array.init 10 (fun _ -> (Stdlib_beta.Int8.of_int 0)) in
  let five_times_three = f3 5 (Stdlib_beta.Int8_u.of_int 3) steps in
  print_int8u "Test 3, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 3, step %d: %d\n") (Array.map Int8.to_int steps);

  (* Test f3_manyargs

          (1 + 2 + 3 + 5 + 8) = 19
      3 * (1 + 2 + 3 + 5 + 8) = 57
      6 * (1 + 2 + 3 + 5 + 8) = 114
      9 * (1 + 2 + 3 + 5 + 8) = 171
      12 * (1 + 2 + 3 + 5 + 8) = -28 (mod 255, interpreted as signed)
  *)
  let steps = Array.init 10 (fun _ -> (Stdlib_beta.Int8.of_int 0)) in
  let x1 = (Stdlib_beta.Int8_u.of_int 1) in
  let x3 = (Stdlib_beta.Int8_u.of_int 2) in
  let x5 = (Stdlib_beta.Int8_u.of_int 3) in
  let x7 = (Stdlib_beta.Int8_u.of_int 5) in
  let x9 = (Stdlib_beta.Int8_u.of_int 8) in

  (* all these 8 numbers together sum to 3 *)
  let x2 = (7, 42) in
  let x4 = (-23, 109) in
  let x6 = (-242, 90) in
  let x8 = (-2, 22) in

  let f3_manyargs = f3_manyargs (4,8) x1 x2 x3 x4 x5 x6 x7 x8 x9 steps in
  print_int8u "Test 3, -28: " (f3_manyargs ());
  Array.iteri (Printf.printf "  Test 3, step %d: %d\n") (Array.map Int8.to_int steps)

let _ = test3 ()

(*********************************************)
(* Test 4: Partial and indirect applications *)

let[@inline never] test4 () =
  (* Simple indirect call *)
  let[@inline never] go f =
    Int8_u.to_int8 (f (Stdlib_beta.Int8_u.of_int 1) (Stdlib_beta.Int8_u.of_int 2))
  in
  let (x1, x2) = (go Int8_u.(+), go Int8_u.(-)) in
  print_int8u "Test 4, 1 + 2" (Int8_u.of_int8 x1);
  print_int8u "Test 4, 1 - 2" (Int8_u.of_int8 x2);

  (* partial application to int8# *)
  let steps = Array.init 10 (fun _ -> (Stdlib_beta.Int8.of_int 0)) in
  let f = Sys.opaque_identity (f3 5 (Stdlib_beta.Int8_u.of_int 3)) in
  let five_times_three = f steps in
  print_int8u "Test 4, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %d\n") (Array.map Int8.to_int steps);

  (* partial application with int8# remaining *)
  let steps = Array.init 10 (fun _ -> (Stdlib_beta.Int8.of_int 0)) in
  let f = Sys.opaque_identity (f3 6) in
  let six_times_three = f (Stdlib_beta.Int8_u.of_int 3) steps in
  print_int8u "Test 4, 6 * 3: " (six_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %d\n") (Array.map Int8.to_int steps);

  (* Those two tests again, but making f3 also opaque to prevent expansion of
     the partial application. *)
  let f3 = Sys.opaque_identity f3 in

  let steps = Array.init 10 (fun _ -> (Stdlib_beta.Int8.of_int 0)) in
  let f = Sys.opaque_identity (f3 5 (Stdlib_beta.Int8_u.of_int 3)) in
  let five_times_three = f steps in
  print_int8u "Test 4, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %d\n") (Array.map Int8.to_int steps);

  let steps = Array.init 10 (fun _ -> (Stdlib_beta.Int8.of_int 0)) in
  let f = Sys.opaque_identity (f3 6) in
  let six_times_three = f (Stdlib_beta.Int8_u.of_int 3) steps in
  print_int8u "Test 4, 6 * 3: " (six_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %d\n") (Array.map Int8.to_int steps)

let _ = test4 ()

(****************************)
(* Test 5: Over application *)

let[@inline never] f5 n m =
  let open Int8_u in
  (* Also testing a closure with only int8# values *)
  let[@inline never] go f =
    f (n + m)
  in
  go

let test5 () =
  let open Int8_u in
  let _ : unit =
    f5 (Stdlib_beta.Int8_u.of_int 3) (Stdlib_beta.Int8_u.of_int 2)
      (fun n s m -> print_int8u s (n + m)) "Test 5, 3 + 2 + 1"
      (Stdlib_beta.Int8_u.of_int 1)
  in
  ()

let _ = test5 ()

(*********************************)
(* Test 6: methods on int8s *)

(* CR layouts: add tests that capture int8s in objects, once that is
   allowed. *)

(* int8# args and returns *)
let f6_1 () = object
  method f6_m1 f1 f2 f3 =
    let open Int8_u in
    (f1 - f2) / f3
end

(* capture a pair, recursion *)
let f6_2 n = object(self)
  method f6_m2 n3 m1 f =
    if n3 = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) then
      m1
    else f (self#f6_m2 (n3+1) m1 f)
end

(* overapplication to int8# and non-int8# args *)
let f6_3 n k = object
  method f6_m3 n3 m1 f =
    let n = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) in
    f (n + k + n3) m1
end

let test6 () =
  let add3 n (m, k) = n + m + k in
  let open Int8_u in

  (* (30 - 20) / 3 = 3 *)
  let o = (Sys.opaque_identity f6_1) () in
  print_int8u "Test 6, 3"
    (o#f6_m1 (Stdlib_beta.Int8_u.of_int 30) (Stdlib_beta.Int8_u.of_int 20) (Stdlib_beta.Int8_u.of_int 3));

  (* 4 * 8 = 32 *)
  let o = (Sys.opaque_identity f6_2) (4,7) in
  let result = o#f6_m2 8 (Stdlib_beta.Int8_u.of_int 4) (fun x -> x * (Stdlib_beta.Int8_u.of_int 2)) in
  print_int8u "Test 6, 32" result;

  (* (1 + 2 + 3 + (-2) + (-12) + 4) * (2 + (-1) + 10) = -44 *)
  let o = (Sys.opaque_identity f6_3) (1,2) 3 in
  let result =
    o#f6_m3 (-2) (Stdlib_beta.Int8_u.of_int 2)
      (fun[@inline never] i m1 m2 n m3 ->
         (of_int8 (Int8.of_int (add3 i n))) * (m1 + m2 + m3))
      ((Stdlib_beta.Int8_u.of_int (-1))) (-12,4) (Stdlib_beta.Int8_u.of_int 10)
  in
  print_int8u "Test 6, -44" result

let _ = test6 ()

(*****************************************)
(* Test 7: int8# and assert false joins *)

module M = struct
  open Int8_u
  let[@inline never] f () = assert false
  let g () = if Sys.opaque_identity true then (Stdlib_beta.Int8_u.of_int 8) else f ()
end

let test7 () =
  print_int8u "Test 7, 8" (M.g ())

let _ = test7 ()
