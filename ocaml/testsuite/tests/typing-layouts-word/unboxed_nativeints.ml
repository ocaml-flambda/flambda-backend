(* TEST
 flambda2;
 {
   native;
 }{
   bytecode;
 }{
   flags = "-extension-universe alpha";
   native;
 }{
   flags = "-extension-universe alpha";
   bytecode;
 }{
   flags = "-extension-universe beta";
   native;
 }{
   flags = "-extension-universe beta";
   bytecode;
 }
*)

(* This file contains various tests for [nativeint#].  It's not an expect test
   to make sure it gets tested for native code. *)

(*****************************************)
(* Prelude: Functions on unboxed nativeints. *)

module Nativeint_u = struct
  include Stdlib__Nativeint_u

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
  String.init Nativeint.size (fun i ->
    if Nativeint.(equal (logand x (shift_left 1n (Nativeint.size - i - 1))) 0n)
    then '0'
    else '1')

let print_int prefix x =
  Printf.printf "%s: %d\n" prefix x

let print_nativeintu prefix x =
  Printf.printf "%s: %nd\n" prefix (Nativeint_u.to_nativeint x)

let print_nativeintu_bin prefix x =
  let bx = Nativeint_u.to_nativeint x in
  Printf.printf "%s: %nd = 0b%s\n" prefix bx (to_binary_string bx)

(*********************************)
(* Test 1: some basic arithmetic *)

(* Tests all the operators above *)
let test1 () =
  (* CR layouts: When word defs are allowed at the module level, get rid of
     [test1] and move these definitions there. *)
  let open Nativeint_u in

  (* Positive numbers *)

  let three = #3n in
  print_nativeintu "Test 1, three" three;

  let twice_three = three + #3n in
  print_nativeintu "Test 1, twice_three" twice_three;

  let thrice_three = #3n * three in
  print_nativeintu "Test 1, thrice_three" thrice_three;

  let twice_three_again = thrice_three - three in
  print_nativeintu "Test 1, twice_three_again" twice_three;

  let three_again = twice_three_again / #2n in
  print_nativeintu "Test 1, three_again" three_again;

  let three_again_unsigned = twice_three_again // #2n in
  print_nativeintu "Test 1, three_again_unsigned" three_again_unsigned;

  let twice_three_greater_than_three = twice_three > three in
  Printf.printf "Test 1, twice_three_greater_than_three: %b\n"
    twice_three_greater_than_three;

  let three_with_effort =
    (#3n + twice_three) * #2n / #6n in
  print_nativeintu "Test 1, three_with_effort" three_with_effort;

  let seven_rem_three = #7n % three in
  print_nativeintu "Test 1, seven_rem_three" seven_rem_three;

  let seven_rem_three_unsigned = #7n %% three in
  print_nativeintu "Test 1, seven_rem_three_unsigned" seven_rem_three_unsigned;

  let forty_two_logand_three = logand #42n three in
  print_nativeintu_bin "Test1, forty_two_logand_three (0b00101010 & 0b00000011)" forty_two_logand_three;

  let forty_two_logor_three = logor #42n three in
  print_nativeintu_bin "Test1, forty_two_logor_three (0b00101010 & 0b00000011)" forty_two_logor_three;

  let forty_two_logxor_three = logxor #42n three in
  print_nativeintu_bin "Test1, forty_two_logxor_three (0b00101010 & 0b00000011)" forty_two_logxor_three;

  let lognot_three = lognot three in
  print_nativeintu_bin "Test1, lognot_three (~0b00000011)" lognot_three;

  let three_shl_eight = shift_left three 8 in
  print_nativeintu_bin "Test1, three_shl_eight (0b00000011 << 8)" three_shl_eight;

  let three_shr_one = shift_right three 1 in
  print_nativeintu_bin "Test1, three_shr_one (0b00000011 >> 1)" three_shr_one;

  let three_shrl_one = shift_right_logical three 1 in
  print_nativeintu_bin "Test1, three_shr_one (0b00000011 >>> 1)" three_shrl_one;

  (* Negative numbers *)

  let minus_five = -#5n in
  print_nativeintu "Test 1, minus_five" minus_five;

  let twice_minus_five = minus_five + (-#5n) in
  print_nativeintu "Test 1, twice_minus_five" twice_minus_five;

  let thrice_minus_five = #3n * minus_five in
  print_nativeintu "Test 1, thrice_minus_five" thrice_minus_five;

  let twice_minus_five_again = thrice_minus_five - minus_five in
  print_nativeintu "Test 1, twice_minus_five_again" twice_minus_five;

  let minus_five_again = twice_minus_five_again / #2n in
  print_nativeintu "Test 1, minus_five_again" minus_five_again;

  let minus_five_again_unsigned = twice_minus_five_again // #2n in
  print_nativeintu "Test 1, minus_five_again_unsigned" minus_five_again_unsigned;

  let minus_five_greater_than_twice_minus_five = minus_five > twice_minus_five in
  Printf.printf "Test 1, minus_five_greater_than_twice_minus_five: %b\n"
    minus_five_greater_than_twice_minus_five;

  let minus_five_with_effort =
    ((-#5n) + twice_minus_five) * #2n / #6n in
  print_nativeintu "Test 1, minus_five_with_effort" minus_five_with_effort;

  let seven_rem_minus_five = #7n % minus_five in
  print_nativeintu "Test 1, seven_rem_minus_five" seven_rem_minus_five;

  let seven_rem_minus_five_unsigned = #7n %% minus_five in
  print_nativeintu "Test 1, seven_rem_minus_five_unsigned" seven_rem_minus_five_unsigned;

  let forty_two_logand_minus_five = logand #42n minus_five in
  print_nativeintu_bin "Test1, forty_two_logand_minus_five (0b00101010 & 0b1...1011)" forty_two_logand_minus_five;

  let forty_two_logor_minus_five = logor #42n minus_five in
  print_nativeintu_bin "Test1, forty_two_logor_minus_five (0b00101010 & 0b1...1011)" forty_two_logor_minus_five;

  let forty_two_logxor_minus_five = logxor #42n minus_five in
  print_nativeintu_bin "Test1, forty_two_logxor_minus_five (0b00101010 & 0b1...1011)" forty_two_logxor_minus_five;

  let lognot_minus_five = lognot minus_five in
  print_nativeintu_bin "Test1, lognot_minus_five (~0b1...1011)" lognot_minus_five;

  let minus_five_shl_eight = shift_left minus_five 8 in
  print_nativeintu_bin "Test1, minus_five_shl_eight (0b1...1011 << 8)" minus_five_shl_eight;

  let minus_five_shr_one = shift_right minus_five 1 in
  print_nativeintu_bin "Test1, minus_five_shr_one (0b1...1011 >> 1)" minus_five_shr_one;

  let minus_five_shrl_one = shift_right_logical minus_five 1 in
  print_nativeintu_bin "Test1, minus_five_shr_one (0b1...1011 >>> 1)" minus_five_shrl_one;

  (* Constants *)

  print_int "Test 1, size" size

  (* CR layouts: Restore these when the appropriate constants exist *)
  (* print_nativeintu "Test 1, max_int" max_int;
   * print_nativeintu "Test 1, min_int" min_int; *)

let _ = test1 ()

(**********************************)
(* Test 2: higher-order functions *)

(* CR layouts v1.5: This type definition can be eliminated once we have
   annotations. *)
type ('a : word) t_word = 'a

let[@inline never] twice f (x : 'a t_word) = f (f x)
let[@inline never] compose f g (x : 'a t_word) = f (g x)

let[@inline never] twice_on_three f =
  let pi = #3n in
  twice f pi

let times_four = twice Nativeint_u.(fun x -> x * #2n)

let _ =
  let open Nativeint_u in
  print_nativeintu "Test 2, add three twice"
    (twice (fun x -> x + #3n) #0n);
  print_nativeintu "Test 2, add three four times"
    (twice (twice (fun x -> x + #3n)) #0n);
  print_nativeintu "Test 2, increment three twice"
    (twice_on_three (fun x -> #1n + x));
  print_nativeintu "Test 2, increment three four times"
    (twice_on_three (twice (fun x -> #1n + x)));
  print_nativeintu "Test 2, two times four"
    (times_four #2n);
  print_nativeintu "Test 2, three times sixteen"
    (twice_on_three times_four);
  print_nativeintu "Test 2, three times sixteen again"
    (compose times_four times_four #3n);
  print_nativeintu "Test 2, three minus four"
    (let two = twice (fun x -> x + #1n) #0n in
     let add_two = Nativeint_u.(+) two in
     let add_two_after = compose add_two in
     let minus_four = add_two_after (twice (fun x -> x - #3n)) in
     minus_four #3n)

(**********************************)
(* Test 3: nativeint# in closures *)

(* [go]'s closure should haave an [int] (immediate), a [nativeint#] (word) and a
   [nativeint array] (value). *)
let[@inline never] f3 n m steps () =
  let[@inline never] rec go k =
    if k = n
    then #0n
    else begin
      let acc = go (k + 1) in
      steps.(k) <- Nativeint_u.to_nativeint acc;
      Nativeint_u.(+) m acc
    end
  in
  go 0

(* many args - even args are tuples, odd args are unboxed nativeints *)
let[@inline_never] f3_manyargs x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 steps () =
  let (start_k, end_k) = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then #0n
    else begin
      let (x2_1, x2_2) = x2 in
      let (x4_1, x4_2) = x4 in
      let (x6_1, x6_2) = x6 in
      let (x8_1, x8_2) = x8 in
      let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
      let acc = go (k + 1) in
      steps.(k) <- Nativeint_u.to_nativeint acc;
      Nativeint_u.(acc + ((x1 + x3 + x5 + x7 + x9) * (of_nativeint (Nativeint.of_int sum))))
    end
  in
  go start_k

let test3 () =
  (* Test f3 *)
  let steps = Array.init 10 (fun _ -> 0n) in
  let five_times_three = f3 5 #3n steps in
  print_nativeintu "Test 3, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 3, step %d: %nd\n") steps;

  (* Test f3_manyargs

          (1 + 2 + 3 + 5 + 8) = 19
      3 * (1 + 2 + 3 + 5 + 8) = 57
      6 * (1 + 2 + 3 + 5 + 8) = 114
      9 * (1 + 2 + 3 + 5 + 8) = 171
  *)
  let steps = Array.init 10 (fun _ -> 0n) in
  let x1 = #1n in
  let x3 = #2n in
  let x5 = #3n in
  let x7 = #5n in
  let x9 = #8n in

  (* all these 8 numbers together sum to 3 *)
  let x2 = (7, 42) in
  let x4 = (-23, 109) in
  let x6 = (-242, 90) in
  let x8 = (-2, 22) in

  let f3_manyargs = f3_manyargs (4,8) x1 x2 x3 x4 x5 x6 x7 x8 x9 steps in
  print_nativeintu "Test 3, 171: " (f3_manyargs ());
  Array.iteri (Printf.printf "  Test 3, step %d: %nd\n") steps

let _ = test3 ()

(*********************************************)
(* Test 4: Partial and indirect applications *)

let[@inline never] test4 () =
  (* Simple indirect call *)
  let[@inline never] go f =
    Nativeint_u.to_nativeint (f #1n #2n)
  in
  let (x1, x2) = (go Nativeint_u.(+), go Nativeint_u.(-)) in
  print_nativeintu "Test 4, 1 + 2" (Nativeint_u.of_nativeint x1);
  print_nativeintu "Test 4, 1 - 2" (Nativeint_u.of_nativeint x2);

  (* partial application to nativeint# *)
  let steps = Array.init 10 (fun _ -> 0n) in
  let f = Sys.opaque_identity (f3 5 #3n) in
  let five_times_three = f steps in
  print_nativeintu "Test 4, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %nd\n") steps;

  (* partial application with nativeint# remaining *)
  let steps = Array.init 10 (fun _ -> 0n) in
  let f = Sys.opaque_identity (f3 6) in
  let six_times_three = f #3n steps in
  print_nativeintu "Test 4, 6 * 3: " (six_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %nd\n") steps;

  (* Those two tests again, but making f3 also opaque to prevent expansion of
     the partial application. *)
  let f3 = Sys.opaque_identity f3 in

  let steps = Array.init 10 (fun _ -> 0n) in
  let f = Sys.opaque_identity (f3 5 #3n) in
  let five_times_three = f steps in
  print_nativeintu "Test 4, 5 * 3: " (five_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %nd\n") steps;

  let steps = Array.init 10 (fun _ -> 0n) in
  let f = Sys.opaque_identity (f3 6) in
  let six_times_three = f #3n steps in
  print_nativeintu "Test 4, 6 * 3: " (six_times_three ());
  Array.iteri (Printf.printf "  Test 4, step %d: %nd\n") steps

let _ = test4 ()

(****************************)
(* Test 5: Over application *)

let[@inline never] f5 n m =
  let open Nativeint_u in
  (* Also testing a closure with only nativeint# values *)
  let[@inline never] go f =
    f (n + m)
  in
  go

let test5 () =
  let open Nativeint_u in
  let _ : unit =
    f5 #3n #2n
      (fun n s m -> print_nativeintu s (n + m)) "Test 5, 3 + 2 + 1"
      #1n
  in
  ()

let _ = test5 ()

(*********************************)
(* Test 6: methods on nativeints *)

(* CR layouts: add tests that capture nativeints in objects, once that is
   allowed. *)

(* nativeint# args and returns *)
let f6_1 () = object
  method f6_m1 f1 f2 f3 =
    let open Nativeint_u in
    (f1 - f2) / f3
end

(* capture a pair, recursion *)
let f6_2 n = object(self)
  method f6_m2 n3 m1 f =
    if n3 = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) then
      m1
    else f (self#f6_m2 (n3+1) m1 f)
end

(* overapplication to nativeint# and non-nativeint# args *)
let f6_3 n k = object
  method f6_m3 n3 m1 f =
    let n = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) in
    f (n + k + n3) m1
end

let test6 () =
  let add3 n (m, k) = n + m + k in
  let open Nativeint_u in

  (* (30 - 20) / 3 = 3 *)
  let o = (Sys.opaque_identity f6_1) () in
  print_nativeintu "Test 6, 3"
    (o#f6_m1 #30n #20n #3n);

  (* 4 * 8 = 32 *)
  let o = (Sys.opaque_identity f6_2) (4,7) in
  let result = o#f6_m2 8 #4n (fun x -> x * #2n) in
  print_nativeintu "Test 6, 32" result;

  (* (1 + 2 + 3 + (-2) + (-12) + 4) * (2 + (-1) + 10) = -44 *)
  let o = (Sys.opaque_identity f6_3) (1,2) 3 in
  let result =
    o#f6_m3 (-2) #2n
      (fun[@inline never] i m1 m2 n m3 ->
         (of_nativeint (Nativeint.of_int (add3 i n))) * (m1 + m2 + m3))
      (-#1n) (-12,4) #10n
  in
  print_nativeintu "Test 6, -44" result

let _ = test6 ()

(*********************************************)
(* Test 7: nativeints and assert false joins *)

module M = struct
  open Nativeint_u
  let[@inline never] f () = assert false
  let g () = if Sys.opaque_identity true then #32n else f ()
end

let test7 () =
  print_nativeintu "Test 7, 32" (M.g ())

let _ = test7 ()
