(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* This file contains various tests for [int64#].  It's not an expect test
   to make sure it gets tested for native code. *)

(*****************************************)
(* Prelude: Functions on unboxed int64s. *)

module Int64_u = struct
  include Stdlib_upstream_compatible.Int64_u

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
  String.init 64 (fun i ->
    if Int64.(equal (logand x (shift_left 1L (64 - i - 1))) 0L)
    then '0'
    else '1')

let print_int prefix x =
  Printf.printf "%s: %d\n" prefix x

let print_int64u prefix x =
  Printf.printf "%s: %Ld\n" prefix (Int64_u.to_int64 x)

let print_int64u_bin prefix x =
  let bx = Int64_u.to_int64 x in
  Printf.printf "%s: %Ld = 0b%s\n" prefix bx (to_binary_string bx)

(*********************************)
(* Test 1: some basic arithmetic *)

(* Tests all the operators above *)
let test1 () =
  (* CR layouts: When bits64 defs are allowed at the module level, get rid of
     [test1] and move these definitions there. *)
  let open Int64_u in

  (* Positive numbers *)

  let three = #3L in
  print_int64u "Test 1, three" three;

  let twice_three = three + #3L in
  print_int64u "Test 1, twice_three" twice_three;

  let thrice_three = #3L * three in
  print_int64u "Test 1, thrice_three" thrice_three;

  let twice_three_again = thrice_three - three in
  print_int64u "Test 1, twice_three_again" twice_three;

  let three_again = twice_three_again / #2L in
  print_int64u "Test 1, three_again" three_again;

  let three_again_unsigned = twice_three_again // #2L in
  print_int64u "Test 1, three_again_unsigned" three_again_unsigned;

  let twice_three_greater_than_three = twice_three > three in
  Printf.printf "Test 1, twice_three_greater_than_three: %b\n"
    twice_three_greater_than_three;

  let three_with_effort =
    (#3L + twice_three) * #2L / #6L in
  print_int64u "Test 1, three_with_effort" three_with_effort;

  let seven_rem_three = #7L % three in
  print_int64u "Test 1, seven_rem_three" seven_rem_three;

  let seven_rem_three_unsigned = #7L %% three in
  print_int64u "Test 1, seven_rem_three_unsigned" seven_rem_three_unsigned;

  let forty_two_logand_three = logand #42L three in
  print_int64u_bin "Test1, forty_two_logand_three (0b00101010 & 0b00000011)" forty_two_logand_three;

  let forty_two_logor_three = logor #42L three in
  print_int64u_bin "Test1, forty_two_logor_three (0b00101010 & 0b00000011)" forty_two_logor_three;

  let forty_two_logxor_three = logxor #42L three in
  print_int64u_bin "Test1, forty_two_logxor_three (0b00101010 & 0b00000011)" forty_two_logxor_three;

  let lognot_three = lognot three in
  print_int64u_bin "Test1, lognot_three (~0b00000011)" lognot_three;

  let three_shl_eight = shift_left three 8 in
  print_int64u_bin "Test1, three_shl_eight (0b00000011 << 8)" three_shl_eight;

  let three_shr_one = shift_right three 1 in
  print_int64u_bin "Test1, three_shr_one (0b00000011 >> 1)" three_shr_one;

  let three_shrl_one = shift_right_logical three 1 in
  print_int64u_bin "Test1, three_shr_one (0b00000011 >>> 1)" three_shrl_one;

  (* Negative numbers *)

  let minus_five = -#5L in
  print_int64u "Test 1, minus_five" minus_five;

  let twice_minus_five = minus_five + (-#5L) in
  print_int64u "Test 1, twice_minus_five" twice_minus_five;

  let thrice_minus_five = #3L * minus_five in
  print_int64u "Test 1, thrice_minus_five" thrice_minus_five;

  let twice_minus_five_again = thrice_minus_five - minus_five in
  print_int64u "Test 1, twice_minus_five_again" twice_minus_five;

  let minus_five_again = twice_minus_five_again / #2L in
  print_int64u "Test 1, minus_five_again" minus_five_again;

  let minus_five_again_unsigned = twice_minus_five_again // #2L in
  print_int64u "Test 1, minus_five_again_unsigned" minus_five_again_unsigned;

  let minus_five_greater_than_twice_minus_five = minus_five > twice_minus_five in
  Printf.printf "Test 1, minus_five_greater_than_twice_minus_five: %b\n"
    minus_five_greater_than_twice_minus_five;

  let minus_five_with_effort =
    ((-#5L) + twice_minus_five) * #2L / #6L in
  print_int64u "Test 1, minus_five_with_effort" minus_five_with_effort;

  let seven_rem_minus_five = #7L % minus_five in
  print_int64u "Test 1, seven_rem_minus_five" seven_rem_minus_five;

  let seven_rem_minus_five_unsigned = #7L %% minus_five in
  print_int64u "Test 1, seven_rem_minus_five_unsigned" seven_rem_minus_five_unsigned;

  let forty_two_logand_minus_five = logand #42L minus_five in
  print_int64u_bin "Test1, forty_two_logand_minus_five (0b00101010 & 0b1...1011)" forty_two_logand_minus_five;

  let forty_two_logor_minus_five = logor #42L minus_five in
  print_int64u_bin "Test1, forty_two_logor_minus_five (0b00101010 & 0b1...1011)" forty_two_logor_minus_five;

  let forty_two_logxor_minus_five = logxor #42L minus_five in
  print_int64u_bin "Test1, forty_two_logxor_minus_five (0b00101010 & 0b1...1011)" forty_two_logxor_minus_five;

  let lognot_minus_five = lognot minus_five in
  print_int64u_bin "Test1, lognot_minus_five (~0b1...1011)" lognot_minus_five;

  let minus_five_shl_eight = shift_left minus_five 8 in
  print_int64u_bin "Test1, minus_five_shl_eight (0b1...1011 << 8)" minus_five_shl_eight;

  let minus_five_shr_one = shift_right minus_five 1 in
  print_int64u_bin "Test1, minus_five_shr_one (0b1...1011 >> 1)" minus_five_shr_one;

  let minus_five_shrl_one = shift_right_logical minus_five 1 in
  print_int64u_bin "Test1, minus_five_shr_one (0b1...1011 >>> 1)" minus_five_shrl_one

  (* CR layouts: Restore these when the appropriate constants exist *)
  (* print_int64u "Test 1, max_int" max_int;
   * print_int64u "Test 1, min_int" min_int; *)

let _ = test1 ()

(**********************************)
(* Test 2: higher-order functions *)

(* CR layouts v1.5: This type definition can be eliminated once we have
   annotations. *)
type ('a : bits64) t_bits64 = 'a

let[@inline never] twice f (x : 'a t_bits64) = f (f x)
let[@inline never] compose f g (x : 'a t_bits64) = f (g x)

let[@inline never] twice_on_three f =
  let pi = #3L in
  twice f pi

let times_four = twice Int64_u.(fun x -> x * #2L)

let _ =
  let open Int64_u in
  print_int64u "Test 2, add three twice"
    (twice (fun x -> x + #3L) #0L);
  print_int64u "Test 2, add three four times"
    (twice (twice (fun x -> x + #3L)) #0L);
  print_int64u "Test 2, increment three twice"
    (twice_on_three (fun x -> #1L + x));
  print_int64u "Test 2, increment three four times"
    (twice_on_three (twice (fun x -> #1L + x)));
  print_int64u "Test 2, two times four"
    (times_four #2L);
  print_int64u "Test 2, three times sixteen"
    (twice_on_three times_four);
  print_int64u "Test 2, three times sixteen again"
    (compose times_four times_four #3L);
  print_int64u "Test 2, three minus four"
    (let two = twice (fun x -> x + #1L) #0L in
     let add_two = Int64_u.(+) two in
     let add_two_after = compose add_two in
     let minus_four = add_two_after (twice (fun x -> x - #3L)) in
     minus_four #3L)

(**********************************)
(* Test 3: int64# in closures *)

(* [go]'s closure should haave an [int] (immediate), a [int64#] (bits64) and a
   [int64 array] (value). *)
let[@inline never] f3 n m steps () =
  let[@inline never] rec go k =
    if k = n
    then #0L
    else begin
      let acc = go (k + 1) in
      steps.(k) <- Int64_u.to_int64 acc;
      Int64_u.(+) m acc
    end
  in
  go 0

(* many args - even args are tuples, odd args are unboxed int64s *)
let[@inline_never] f3_manyargs x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 steps () =
  let (start_k, end_k) = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then #0L
    else begin
      let (x2_1, x2_2) = x2 in
      let (x4_1, x4_2) = x4 in
      let (x6_1, x6_2) = x6 in
      let (x8_1, x8_2) = x8 in
      let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
      let acc = go (k + 1) in
      steps.(k) <- Int64_u.to_int64 acc;
      Int64_u.(acc + ((x1 + x3 + x5 + x7 + x9) * (of_int64 (Int64.of_int sum))))
    end
  in
  go start_k

let test3 () =
  (* Test f3 *)
  let steps = Array.init 10 (fun _ -> 0L) in
  let five_times_three = f3 5 #3L steps in
  print_int64u "Test 3, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 3, step %d: %Ld\n") steps;

  (* Test f3_manyargs

          (1 + 2 + 3 + 5 + 8) = 19
      3 * (1 + 2 + 3 + 5 + 8) = 57
      6 * (1 + 2 + 3 + 5 + 8) = 114
      9 * (1 + 2 + 3 + 5 + 8) = 171
  *)
  let steps = Array.init 10 (fun _ -> 0L) in
  let x1 = #1L in
  let x3 = #2L in
  let x5 = #3L in
  let x7 = #5L in
  let x9 = #8L in

  (* all these 8 numbers together sum to 3 *)
  let x2 = (7, 42) in
  let x4 = (-23, 109) in
  let x6 = (-242, 90) in
  let x8 = (-2, 22) in

  let f3_manyargs = f3_manyargs (4,8) x1 x2 x3 x4 x5 x6 x7 x8 x9 steps in
  print_int64u "Test 3, 171: " (f3_manyargs ());
  Array.iteri (Printf.printf "  Test 3, step %d: %Ld\n") steps

let _ = test3 ()

(*********************************************)
(* Test 4: Partial and indirect applications *)

let[@inline never] test4 () =
  (* Simple indirect call *)
  let[@inline never] go f =
    Int64_u.to_int64 (f #1L #2L)
  in
  let (x1, x2) = (go Int64_u.(+), go Int64_u.(-)) in
  print_int64u "Test 4, 1 + 2" (Int64_u.of_int64 x1);
  print_int64u "Test 4, 1 - 2" (Int64_u.of_int64 x2);

  (* partial application to int64# *)
  let steps = Array.init 10 (fun _ -> 0L) in
  let f = Sys.opaque_identity (f3 5 #3L) in
  let five_times_three = f steps in
  print_int64u "Test 4, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %Ld\n") steps;

  (* partial application with int64# remaining *)
  let steps = Array.init 10 (fun _ -> 0L) in
  let f = Sys.opaque_identity (f3 6) in
  let six_times_three = f #3L steps in
  print_int64u "Test 4, 6 * 3: " (six_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %Ld\n") steps;

  (* Those two tests again, but making f3 also opaque to prevent expansion of
     the partial application. *)
  let f3 = Sys.opaque_identity f3 in

  let steps = Array.init 10 (fun _ -> 0L) in
  let f = Sys.opaque_identity (f3 5 #3L) in
  let five_times_three = f steps in
  print_int64u "Test 4, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %Ld\n") steps;

  let steps = Array.init 10 (fun _ -> 0L) in
  let f = Sys.opaque_identity (f3 6) in
  let six_times_three = f #3L steps in
  print_int64u "Test 4, 6 * 3: " (six_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %Ld\n") steps

let _ = test4 ()

(****************************)
(* Test 5: Over application *)

let[@inline never] f5 n m =
  let open Int64_u in
  (* Also testing a closure with only int64# values *)
  let[@inline never] go f =
    f (n + m)
  in
  go

let test5 () =
  let open Int64_u in
  let _ : unit =
    f5 #3L #2L
      (fun n s m -> print_int64u s (n + m)) "Test 5, 3 + 2 + 1"
      #1L
  in
  ()

let _ = test5 ()

(*********************************)
(* Test 6: methods on int64s *)

(* CR layouts: add tests that capture int64s in objects, once that is
   allowed. *)

(* int64# args and returns *)
let f6_1 () = object
  method f6_m1 f1 f2 f3 =
    let open Int64_u in
    (f1 - f2) / f3
end

(* capture a pair, recursion *)
let f6_2 n = object(self)
  method f6_m2 n3 m1 f =
    if n3 = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) then
      m1
    else f (self#f6_m2 (n3+1) m1 f)
end

(* overapplication to int64# and non-int64# args *)
let f6_3 n k = object
  method f6_m3 n3 m1 f =
    let n = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) in
    f (n + k + n3) m1
end

let test6 () =
  let add3 n (m, k) = n + m + k in
  let open Int64_u in

  (* (30 - 20) / 3 = 3 *)
  let o = (Sys.opaque_identity f6_1) () in
  print_int64u "Test 6, 3"
    (o#f6_m1 #30L #20L #3L);

  (* 4 * 8 = 32 *)
  let o = (Sys.opaque_identity f6_2) (4,7) in
  let result = o#f6_m2 8 #4L (fun x -> x * #2L) in
  print_int64u "Test 6, 32" result;

  (* (1 + 2 + 3 + (-2) + (-12) + 4) * (2 + (-1) + 10) = -44 *)
  let o = (Sys.opaque_identity f6_3) (1,2) 3 in
  let result =
    o#f6_m3 (-2) #2L
      (fun[@inline never] i m1 m2 n m3 ->
         (of_int64 (Int64.of_int (add3 i n))) * (m1 + m2 + m3))
      (-#1L) (-12,4) #10L
  in
  print_int64u "Test 6, -44" result

let _ = test6 ()

(*****************************************)
(* Test 7: int64# and assert false joins *)

module M = struct
  open Int64_u
  let[@inline never] f () = assert false
  let g () = if Sys.opaque_identity true then #32L else f ()
end

let test7 () =
  print_int64u "Test 7, 32" (M.g ())

let _ = test7 ()
