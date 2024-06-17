(* TEST
 reference = "${test_source_directory}/unboxed_float32s.reference";
 include beta;
 flambda2;
 {
   flags = "-extension layouts_alpha -extension small_numbers";
   native;
 }{
   flags = "-extension layouts_alpha -extension small_numbers";
   bytecode;
 }{
   flags = "-extension layouts_beta -extension small_numbers";
   native;
 }{
   flags = "-extension layouts_beta -extension small_numbers";
   bytecode;
 }
*)

(* CR layouts v2.6: Layouts should be erasable and we can remove the
   only-erasable-extensions stanza above. *)

(* mshinwell: This test is now only run with flambda2, as the corresponding
   ocamltest predicate is reliable for testing whether this is an
   flambda-backend build. *)

(* This file contains various tests for float32#.  It's not an expect test to make
   sure it gets tested for native code. *)

(* CR layouts v2.5: When unboxed literals work, change this file to use them
   instead of [of_float] on boxed literals everywhere. *)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module Float32 = struct
  include Beta.Float32

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( > ) x y = (compare x y) > 0
end

module Float32_u = struct
  include Beta.Float32_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( > ) x y = (compare x y) > 0
end

(*********************************)
(* Test 1: some basic arithmetic *)

let print_floatu prefix x = Printf.printf "%s: %.2f\n" prefix (Float32.to_float (Float32_u.to_float32 x))
let print_float prefix x = Printf.printf "%s: %.2f\n" prefix (Float32.to_float x)
let print_int prefix x = Printf.printf "%s: %d\n" prefix x

(* Tests all the operators above *)
let test1 () =
  (* CR layouts: When float32 defs are allowed at the module level, get rid of
     [test1] and move these definitions there. *)
  let open Float32_u in
  let pi = #3.14s in
  print_floatu "Test 1, pi" pi;

  let twice_pi = pi + #3.14s in
  print_floatu "Test 1, twice_pi" twice_pi;

  let thrice_pi = #3.0s * pi in
  print_floatu "Test 1, thrice_pi" thrice_pi;

  let twice_pi_again = thrice_pi - pi in
  print_floatu "Test 1, twice_pi_again" twice_pi;

  let pi_again = twice_pi_again / #2.0s in
  print_floatu "Test 1, pi_again" pi_again;

  let twice_pi_to_the_pi = twice_pi ** pi in
  print_floatu "Test 1, twice_pi_to_the_pi" twice_pi_to_the_pi;

  let twice_pi_greater_than_pi = twice_pi > pi in
  Printf.printf "Test 1, twice_pi_greater_than_pi: %b\n"
    twice_pi_greater_than_pi;

  let pi_with_effort =
    (#3.14s + twice_pi) * #2.0s / #6.0s in
  print_floatu "Test 1, pi_with_effort" pi_with_effort

let _ = test1 ()

(**********************************)
(* Test 2: higher-order functions *)

(* CR layouts v1.5: This type definition can be eliminated once we have
   annotations. *)
type ('a : float32) t_float32 = 'a

let[@inline never] twice f (x : 'a t_float32) = f (f x)
let[@inline never] compose f g (x : 'a t_float32) = f (g x)

let[@inline never] twice_on_pi f =
  let pi = #3.14s in
  twice f pi

let times_four = twice Float32_u.(fun x -> x * #2.0s)

let _ =
  let open Float32_u in
  print_floatu "Test 2, add pi twice"
    (twice (fun x -> x + #3.14s) #0.0s);
  print_floatu "Test 2, add pi four times"
    (twice (twice (fun x -> x + #3.14s)) #0.0s);
  print_floatu "Test 2, increment pi twice"
    (twice_on_pi (fun x -> #1.0s + x));
  print_floatu "Test 2, increment pi four times"
    (twice_on_pi (twice (fun x -> #1.0s + x)));
  print_floatu "Test 2, e times four"
    (times_four #2.72s);
  print_floatu "Test 2, pi times sixteen"
    (twice_on_pi times_four);
  print_floatu "Test 2, pi times sixteen again"
    (compose times_four times_four #3.14s);
  print_floatu "Test 2, pi minus four"
    (let two = twice (fun x -> x + #1.0s) #0.0s in
     let add_two = Float32_u.(+) two in
     let add_two_after = compose add_two in
     let minus_four = add_two_after (twice (fun x -> x - #3.0s)) in
     minus_four #3.14s)

(******************************)
(* Test 3: float32# in closures *)

(* [go]'s closure should haave an [int] (immediate), a [float32#] (float32) and a
   [float array] (value). *)
let[@inline never] f3 n m steps () =
  let[@inline never] rec go k =
    if k = n
    then #0.s
    else begin
      let acc = go (k + 1) in
      steps.(k) <- Float32_u.to_float32 acc;
      Float32_u.(+) m acc
    end
  in
  go 0

(* many args - even args are tuples, odd args are unboxed floats *)
let[@inline_never] f3_manyargs x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 steps () =
  let (start_k, end_k) = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then #0.s
    else begin
      let (x2_1, x2_2) = x2 in
      let (x4_1, x4_2) = x4 in
      let (x6_1, x6_2) = x6 in
      let (x8_1, x8_2) = x8 in
      let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
      let acc = go (k + 1) in
      steps.(k) <- Float32_u.to_float32 acc;
      Float32_u.(acc + ((x1 + x3 + x5 + x7 + x9) * (of_float32 (Float32.of_int sum))))
    end
  in
  go start_k

let test3 () =
  (* Test f3 *)
  let steps = Array.init 10 (fun _ -> 0.0s) in
  let five_pi = f3 5 #3.14s steps in
  print_floatu "Test 3, 5 * pi: " (five_pi ());
  Array.iteri (fun i f -> Printf.printf "  Test 3, step %d: %.2f\n" i (Float32.to_float f)) steps;

  (* Test f3_manyargs

          (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 50.86
      3 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 152.58
      6 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 306.16
      9 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 457.74

    ( but we expect some floating point error )
  *)
  let steps = Array.init 10 (fun _ -> 0.0s) in
  let x1 = #3.14s in
  let x3 = #2.72s in
  let x5 = #1.62s in
  let x7 = #1.41s in
  let x9 = #42.0s in

  (* these sum to 3 *)
  let x2 = (7, 42) in
  let x4 = (-23, 109) in
  let x6 = (-242, 90) in
  let x8 = (-2, 22) in

  let f3_manyargs = f3_manyargs (4,8) x1 x2 x3 x4 x5 x6 x7 x8 x9 steps in
  print_floatu "Test 3, 610.68: " (f3_manyargs ());
  Array.iteri (fun i f -> Printf.printf "  Test 3, step %d: %.2f\n" i (Float32.to_float f)) steps

let _ = test3 ()

(*********************************************)
(* Test 4: Partial and indirect applications *)

let[@inline never] test4 () =
  (* Simple indirect call *)
  let[@inline never] go f =
    Float32_u.to_float32 (f #1.s #2.s)
  in
  let (x1, x2) = (go Float32_u.(+), go Float32_u.(-)) in
  print_floatu "Test 4, 1 + 2" (Float32_u.of_float32 x1);
  print_floatu "Test 4, 1 - 2" (Float32_u.of_float32 x2);

  (* partial application to float32# *)
  let steps = Array.init 10 (fun _ -> 0.0s) in
  let f = Sys.opaque_identity (f3 5 #3.14s) in
  let five_pi = f steps in
  print_floatu "Test 4, 5 * pi: " (five_pi ());
  Array.iteri (fun i f -> Printf.printf "  Test 4, step %d: %.2f\n" i (Float32.to_float f)) steps;

  (* partial application with float32# remaining *)
  let steps = Array.init 10 (fun _ -> 0.0s) in
  let f = Sys.opaque_identity (f3 6) in
  let five_pi = f #3.14s steps in
  print_floatu "Test 4, 6 * pi: " (five_pi ());
  Array.iteri (fun i f -> Printf.printf "  Test 4, step %d: %.2f\n" i (Float32.to_float f)) steps;

  (* Those two tests again, but making f3 also opaque to prevent expansion of
     the partial application. *)
  let f3 = Sys.opaque_identity f3 in

  let steps = Array.init 10 (fun _ -> 0.0s) in
  let f = Sys.opaque_identity (f3 5 #3.14s) in
  let five_pi = f steps in
  print_floatu "Test 4, 5 * pi: " (five_pi ());
  Array.iteri (fun i f -> Printf.printf "  Test 4, step %d: %.2f\n" i (Float32.to_float f)) steps;

  let steps = Array.init 10 (fun _ -> 0.0s) in
  let f = Sys.opaque_identity (f3 6) in
  let five_pi = f #3.14s steps in
  print_floatu "Test 4, 6 * pi: " (five_pi ());
  Array.iteri (fun i f -> Printf.printf "  Test 4, step %d: %.2f\n" i (Float32.to_float f)) steps

let _ = test4 ()

(****************************)
(* Test 5: Over application *)

let[@inline never] f5 n m =
  let open Float32_u in
  (* Also testing a closure with only float32# values *)
  let[@inline never] go f =
    f (n + m)
  in
  go

let test5 () =
  let open Float32_u in
  let _ : unit =
    f5 #3.14s #2.72s
      (fun n s m -> print_floatu s (n + m)) "Test 5, pi+e+1"
      #1.0s
  in
  ()

let _ = test5 ()

(*****************************)
(* Test 6: methods on floats *)

(* CR layouts: add tests that capture floats in objects, once that is
   allowed. *)

(* float32# args and returns *)
let f6_1 () = object
  method f6_m1 f1 f2 f3 =
    let open Float32_u in
    (f1 - f2) / f3
end

(* capture a pair, recursion *)
let f6_2 n = object(self)
  method f6_m2 n3 m1 f =
    if n3 = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) then
      m1
    else f (self#f6_m2 (n3+1) m1 f)
end

(* overapplication to float32# and non-float32# args *)
let f6_3 n k = object
  method f6_m3 n3 m1 f =
    let n = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) in
    f (n + k + n3) m1
end

let test6 () =
  let add3 n (m, k) = n + m + k in
  let open Float32_u in

  (* (3.14 - 2.72) / 2.5 = ~0.17 *)
  let o = (Sys.opaque_identity f6_1) () in
  print_floatu "Test 6, 0.17"
    (o#f6_m1 #3.14s #2.72s #2.5s);

  (* 4.25 * 8 = 34 *)
  let o = (Sys.opaque_identity f6_2) (4,7) in
  let result = o#f6_m2 8 #4.25s (fun x -> x * #2.s) in
  print_floatu "Test 6, 34.00" result;

  (* (1 + 2 + 3 + (-2) + (-12) + 4) * (2.72 + (-1) + 10) = -46.88 *)
  let o = (Sys.opaque_identity f6_3) (1,2) 3 in
  let result =
    o#f6_m3 (-2) (#2.72s)
      (fun[@inline never] i m1 m2 n m3 ->
         (of_float32 (Float32.of_int (add3 i n))) * (m1 + m2 + m3))
      (-#1.s) (-12,4) (#10.s)
  in
  print_floatu "Test 6, -46.88" result

let _ = test6 ()

(*****************************)
(* Test 7: letop with floats *)

let ( let* ) x f = f Float32_u.(x + #1.5s)

let _ =
  let* x = #42.0s in
  print_floatu "Test 7, 36.50" Float32_u.(x - #7.0s)

let ( let* ) x (f : _ -> float32#) = f x
let ( and* ) x y = Float32_u.(x, to_float32 (y - #1.2s))
let _ =
  let result =
    let* x = 42.0s
    and* y = #3.3s
    and* z = -#10.7s in
    Float32_u.of_float32 Float32.(x + y + z)
  in
  print_floatu "Test 7, 32.20" result

(********************************)
(* Test 8: basic float32# records *)

(* Copy of test 3, but the float args are in a record *)
type manyargs = { x1 : float32#; x3 : float32#; x5 : float32#; x7: float32#; x9: float32# }

(* Get some float32# args by pattern matching and others by projection *)
let[@inline_never] f8 x0 x2 x4 x6 x8 steps ({ x1; x5; _ } as fargs) () =
  let (start_k, end_k) = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then #0.s
    else begin
      let (x2_1, x2_2) = x2 in
      let (x4_1, x4_2) = x4 in
      let (x6_1, x6_2) = x6 in
      let (x8_1, x8_2) = x8 in
      let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
      let acc = go (k + 1) in
      steps.(k) <- Float32_u.to_float32 acc;
      Float32_u.(acc + ((x1 + fargs.x3 + x5 + fargs.x7 + fargs.x9)
                      * (of_float32 (Float32.of_int sum))))
    end
  in
  go start_k

let test8 () =
  (* same math as f3_manyargs *)
  let steps = Array.init 10 (fun _ -> 0.0s) in
  let x1 = #3.14s in
  let x3 = #2.72s in
  let x5 = #1.62s in
  let x7 = #1.41s in
  let x9 = #42.0s in

  (* these sum to 3 *)
  let x2 = (7, 42) in
  let x4 = (-23, 109) in
  let x6 = (-242, 90) in
  let x8 = (-2, 22) in

  let fargs = { x1; x3; x5; x7; x9 } in

  let f8 = f8 (4,8) x2 x4 x6 x8 steps fargs in
  print_floatu "Test 8, 610.68: " (f8 ());
  Array.iteri (fun i f -> Printf.printf "  Test 8, step %d: %.2f\n" i (Float32.to_float f)) steps

let _ = test8 ()

(**************************************)
(* Test 9: float32# record manipulation *)

type t9 = { a : float32#;
            mutable b : float32#;
            c : float32#;
            mutable d : float32# }

(* Construction *)
let t9_1 = { a = #3.14s;
             b = #2.72s;
             c = #1.62s;
             d = #1.41s }

let t9_2 = { a = -#3.14s;
             b = -#2.72s;
             c = -#1.62s;
             d = -#1.41s }

let print_t9 t9 =
  print_floatu "  a" t9.a;
  print_floatu "  b" t9.b;
  print_floatu "  c" t9.c;
  print_floatu "  d" t9.d

let _ =
  Printf.printf "Test 9, construction:\n";
  print_t9 t9_1;
  print_t9 t9_2

(* Matching, projection *)
let f9_1 {c; d; _} r =
  match r with
  | { a; _ } ->
    Float32_u. { a = c;
               b = a - d;
               c = r.c + c;
               d = d - r.b }

let _ =
  Printf.printf "Test 9, matching and projection:\n";
  print_t9 (f9_1 t9_1 t9_2)

(* Record update and mutation *)
let f9_2 ({a; d; _} as r1) r2 =
  r1.d <- #42.0s;
  let r3 = { r2 with c = r1.d; d = #25.0s } in
  r3.b <- Float32_u.(a + d);
  r2.b <- #17.0s;
  r3

let _ =
  Printf.printf "Test 9, record update and mutation:\n";
  let t9_3 = f9_2 t9_1 t9_2 in
  print_t9 t9_1;
  print_t9 t9_2;
  print_t9 t9_3

(***********************************************)
(* Test 10: float32# records in recursive groups *)

let rec f r =
  r.d <- t10_1.b;
  t10_2.b <- #42.0s;
  Float32_u.(r.a + t10_2.a)


and t10_1 = { a = #1.1s;
              b = #2.2s;
              c = #3.2s;
              d = #4.4s }

and t10_2 = { a = -#5.1s;
              b = -#6.2s;
              c = -#7.3s;
              d = -#8.4s }

let _ =
  Printf.printf "Test 10, float32# records in recursive groups.\n";
  print_t9 t10_1;
  print_t9 t10_2;
  let result = f t10_1 in
  print_floatu "  result (-4.00)" result;
  print_t9 t10_1;
  print_t9 t10_2

(***********************************************)
(* Test 11: Heterogeneous polymorphic equality *)

(* Not supported for float32 blocks; they are always mixed. *)

(*************************************************)
(* Test 12: If-then-else with float32 and assert *)

let _ =
  let a = if Sys.opaque_identity true then #1.s else assert false in
  Printf.printf "Test 12, If-then-else with assert and float32.\n";
  print_floatu "  result (1.00)" a

(*************************************************)
(* Test 13: basic float32 + float32# records *)

(* Same as test 13 for floats, but boxed float32s have to be in the prefix. *)

type mixed_float32_record =
  { x2_1 : float32;
    x4_1 : float32;
    x6_1 : float32;
    x8_1 : float32;
    x1 : float32#;
    x3 : float32#;
    x5 : float32#;
    x7 : float32#;
    x9 : float32# }

type int_args =
  { x0_1 : int;
    x0_2 : int;
    x2_2 : int;
    x4_2 : int;
    x6_2 : int;
    x8_2 : int;
  }

(* Get some float32# args by pattern matching and others by projection *)
let[@inline_never] f13 steps ({ x1; x8_1; x5; x6_1 } as fargs)
  ({ x0_1=start_k; x0_2=end_k; x4_2; x8_2 } as iargs) () =
  let[@inline never] rec go k =
    if k = end_k
    then Float32_u.of_float32 0.s
    else begin
      let (x2_1, x2_2) = (fargs.x2_1, iargs.x2_2) in
      let {x4_1; _}, {x6_2; _} = fargs, iargs in
      let sum =
        Float32_u.(of_float32 x2_1 + of_int x2_2 + of_float32 x4_1 + of_int x4_2
                 + of_float32 x6_1 + of_int x6_2 + of_float32 x8_1 + of_int x8_2)
      in
      let acc = go (k + 1) in
      steps.(k) <- Float32_u.to_float32 acc;
      Float32_u.(acc + ((x1 + fargs.x3 + x5 + fargs.x7 + fargs.x9)
                      * sum))
    end
  in
  go start_k

let test13 () =
  (* same math as f3_manyargs *)
  let steps = Array.init 10 (fun _ -> 0.0s) in
  let x1 = Float32_u.of_float32 3.14s in
  let x3 = Float32_u.of_float32 2.72s in
  let x5 = Float32_u.of_float32 1.62s in
  let x7 = Float32_u.of_float32 1.41s in
  let x9 = Float32_u.of_float32 42.0s in

  (* these sum to 3.0 *)
  let x2_1 = 6.6s in
  let x2_2 = 42 in
  let x4_1 = -22.9s in
  let x4_2 = 109 in
  let x6_1 = -241.2s in
  let x6_2 = 90 in
  let x8_1 = -2.5s in
  let x8_2 = 22 in

  let fargs =
    { x1; x2_1; x3; x4_1; x5; x6_1; x7;
      x8_1; x9; }
  in
  let iargs = { x0_1 = 4; x0_2 = 8; x2_2; x4_2; x6_2; x8_2 } in

  let f13 = f13 steps fargs iargs in
  print_floatu "Test 13, 610.68: " (f13 ());
  Array.iteri (fun i f -> Printf.printf "  Test 13, step %d: %.2f\n" i (Float32.to_float f)) steps

let _ = test13 ()

(*****************************************************)
(* Test 14: (float32# + float32) record manipulation *)

(* Same as test 14 for floats, but boxed float32s have to be in the prefix. *)

type t14 = { mutable b : float32;
             e : float32;
             a : float32#;
             c : float32#;
             mutable d : float32#;
             mutable f : float32# }

(* Construction *)
let t14_1 = { a = Float32_u.of_float32 3.14s;
              b = 13.s;
              c = Float32_u.of_float32 7.31s;
              d = Float32_u.of_float32 1.41s;
              e = 6.s;
              f = Float32_u.of_float32 27.1s
            }

let t14_2 = { a = Float32_u.of_float32 (-3.14s);
              b = -13.s;
              c = Float32_u.of_float32 (-7.31s);
              d = Float32_u.of_float32 (-1.41s);
              e = -6.s;
              f = Float32_u.of_float32 (-27.1s)
            }

let print_t14 t14 =
  print_floatu "  a" t14.a;
  print_float "  b" t14.b;
  print_floatu "  c" t14.c;
  print_floatu "  d" t14.d;
  print_float "  e" t14.e;
  print_floatu "  f" t14.f

let _ =
  Printf.printf "Test 14, construction:\n";
  print_t14 t14_1;
  print_t14 t14_2

(* Matching, projection *)
let f14_1 {c; d; f; _} r =
  match r with
  | { a; _ } ->
    { a = (Float32_u.of_float32 r.e);
      b = Float32_u.(to_float32 (a - d));
      c = Float32_u.(r.c + c);
      d = Float32_u.(d - (of_float32 r.b));
      e = Float32_u.(to_float32 (f + (of_float32 r.e)));
      f = r.f}

let _ =
  Printf.printf "Test 14, matching and projection:\n";
  print_t14 (f14_1 t14_1 t14_2)

(* Record update and mutation *)
let f14_2 ({a; d; _} as r1) r2 =
  r1.d <- Float32_u.of_float32 42.0s;
  let r3 = { r2 with c = r1.d;
                     d = Float32_u.of_float32 25.0s }
  in
  r3.b <- Float32_u.(to_float32 (a + d));
  r2.b <- 17.s;
  r1.f <- r2.c;
  r3

let _ =
  Printf.printf "Test 14, record update and mutation:\n";
  let t14_3 = f14_2 t14_1 t14_2 in
  print_t14 t14_1;
  print_t14 t14_2;
  print_t14 t14_3

(*****************************************************)
(* Test 14.1: (float32# + float32) variant manipulation *)

(* Same as test 14 for floats, but boxed float32s have to be in the prefix. *)

type t14_variant =
  | Const
  | T of   { mutable b : float32;
             e : float32;
             a : float32#;
             c : float32#;
             mutable d : float32#;
             mutable f : float32# }

(* Construction *)
let t14_1 = T
            { a = Float32_u.of_float32 3.14s;
              b = 13.s;
              c = Float32_u.of_float32 7.31s;
              d = Float32_u.of_float32 1.41s;
              e = 6.s;
              f = Float32_u.of_float32 27.1s
            }

let t14_2 = T
            { a = Float32_u.of_float32 (-3.14s);
              b = -13.s;
              c = Float32_u.of_float32 (-7.31s);
              d = Float32_u.of_float32 (-1.41s);
              e = -6.s;
              f = Float32_u.of_float32 (-27.1s)
            }

let[@warning "-partial-match"] print_t14_variant (T t14) =
  print_floatu "  a" t14.a;
  print_float "  b" t14.b;
  print_floatu "  c" t14.c;
  print_floatu "  d" t14.d;
  print_float "  e" t14.e;
  print_floatu "  f" t14.f

let _ =
  Printf.printf "Test 14.1, construction:\n";
  print_t14_variant t14_1;
  print_t14_variant t14_2

(* Matching, projection *)
let[@warning "-partial-match"] f14_1 (T {c; d; f; _}) r =
  match r with
  | T ({ a; _ } as r) ->
    T
    { a = (Float32_u.of_float32 r.e);
      b = Float32_u.(to_float32 (a - d));
      c = Float32_u.(r.c + c);
      d = Float32_u.(d - (of_float32 r.b));
      e = Float32_u.(to_float32 (f + (of_float32 r.e)));
      f = r.f}

let _ =
  Printf.printf "Test 14.1, matching and projection:\n";
  print_t14_variant (f14_1 t14_1 t14_2)

(* Record update and mutation *)
let[@warning "-partial-match"] f14_2 (T ({a; d; _} as r1)) (T r2) =
  r1.d <- Float32_u.of_float32 42.0s;
  let T r3 = T { r2 with c = r1.d;
                     d = Float32_u.of_float32 25.0s }
  in
  r3.b <- Float32_u.(to_float32 (a + d));
  r2.b <- 17.s;
  r1.f <- r2.c;
  T r3

let _ =
  Printf.printf "Test 14.1, variant update and mutation:\n";
  let t14_3 = f14_2 t14_1 t14_2 in
  print_t14_variant t14_1;
  print_t14_variant t14_2;
  print_t14_variant t14_3

(*************************************************************)
(* Test 15: (float32# + float32) records in recursive groups *)

let rec f r =
  r.d <- Float32_u.of_float32 t15_1.b;
  t15_2.b <- 42.s;
  t15_1.f <- t15_1.a;
  Float32_u.(r.a + t15_2.a)


and t15_1 = { a = Float32_u.of_float32 1.1s;
              b = 2.s;
              c = Float32_u.of_float32 3.3s;
              d = Float32_u.of_float32 4.4s;
              e = 5.s;
              f = Float32_u.of_float32 6.6s}

and t15_2 = { a = Float32_u.of_float32 (- 5.1s);
              b = -6.s;
              c = Float32_u.of_float32 (-7.3s);
              d = Float32_u.of_float32 (-8.4s);
              e = -9.s;
              f = Float32_u.of_float32 (-10.6s) }

let _ =
  Printf.printf "Test 15, (float32#+float32) records in recursive groups:\n";
  print_t14 t15_1;
  print_t14 t15_2;
  let result = f t15_1 in
  print_floatu "  result (-4.00)" result;
  print_t14 t15_1;
  print_t14 t15_2

let dummy = "dummy"

(*************************************************)
(* Test 16: basic mixed records involving float32# *)

type mixedargs = { x2_1 : float32;
                   x4_1 : float32;
                   x6_1 : float32;
                   x8_1 : float32;
                   (* We include the string field to document more plainly that
                      the preceding section of [float32] fields are boxed.
                      Without the [str] field, an implementation of mixed blocks
                      could more conceivably unbox the [float32] fields.
                   *)
                   dummy : string;
                   x0_1 : int;
                   x0_2 : int;
                   x1 : float32#;
                   x2_2 : int;
                   x3 : float32#;
                   x4_2 : int;
                   x5 : float32#;
                   x6_2 : int;
                   x7 : float32#;
                   x8_2 : int;
                   x9 : float32# }

(* Get some float32# args by pattern matching and others by projection *)
let[@inline_never] f16 steps ({ x1; x0_1=start_k; x0_2=end_k; x8_1; x8_2; x5;
                               x6_1; x6_2 } as fargs) () =
  let[@inline never] rec go k =
    if k = end_k
    then Float32_u.of_float32 0.s
    else begin
      let (x2_1, x2_2) = (fargs.x2_1, fargs.x2_2) in
      let {x4_1; x4_2; _} = fargs in
      let sum =
        Float32_u.(of_float32 x2_1 + of_int x2_2 + of_float32 x4_1 + of_int x4_2
                 + of_float32 x6_1 + of_int x6_2 + of_float32 x8_1 + of_int x8_2)
      in
      let acc = go (k + 1) in
      steps.(k) <- Float32_u.to_float32 acc;
      Float32_u.(acc + ((x1 + fargs.x3 + x5 + fargs.x7 + fargs.x9)
                      * sum))
    end
  in
  go start_k

let test16 () =
  (* same math as f3_manyargs *)
  let steps = Array.init 10 (fun _ -> 0.0s) in
  let x1 = Float32_u.of_float32 3.14s in
  let x3 = Float32_u.of_float32 2.72s in
  let x5 = Float32_u.of_float32 1.62s in
  let x7 = Float32_u.of_float32 1.41s in
  let x9 = Float32_u.of_float32 42.0s in

  (* these sum to 3.0 *)
  let x2_1 = 6.6s in
  let x2_2 = 42 in
  let x4_1 = -22.9s in
  let x4_2 = 109 in
  let x6_1 = -241.2s in
  let x6_2 = 90 in
  let x8_1 = -2.5s in
  let x8_2 = 22 in

  let fargs =
    { x0_1 = 4; x0_2 = 8; x1; x2_1; x2_2; x3; x4_1; x4_2; x5; x6_1; x6_2; x7;
      x8_1; x8_2; x9;
      dummy }
  in

  let f16 = f16 steps fargs in
  print_floatu "Test 16, 610.68: " (f16 ());
  Array.iteri (fun i f -> Printf.printf "  Test 16, step %d: %.2f\n" i (Float32.to_float f)) steps

let _ = test16 ()

(*****************************************************)
(* Test 17: mixed record manipulation *)

type t17 = { a : float32;
            dummy : string;
             mutable b : int;
             c : float32#;
             mutable d : float32#;
             e : int;
             mutable f : float32# }

(* Construction *)
let t17_1 = { a = 3.17s;
              b = 13;
              c = Float32_u.of_float32 7.31s;
              d = Float32_u.of_float32 1.41s;
              e = 6;
              f = Float32_u.of_float32 27.1s;
              dummy;
            }

let t17_2 = { a = (-3.17s);
              b = -13;
              c = Float32_u.of_float32 (-7.31s);
              d = Float32_u.of_float32 (-1.41s);
              e = -6;
              f = Float32_u.of_float32 (-27.1s);
              dummy;
            }

let print_t17 t17 =
  print_float "  a" t17.a;
  print_int "  b" t17.b;
  print_floatu "  c" t17.c;
  print_floatu "  d" t17.d;
  print_int "  e" t17.e;
  print_floatu "  f" t17.f

let _ =
  Printf.printf "Test 17, construction:\n";
  print_t17 t17_1;
  print_t17 t17_2

(* Matching, projection *)
let f17_1 {c; d; f; _} r =
  match r with
  | { a; _ } ->
    { a = Float32.of_int r.e;
      b = Float32_u.(to_int (of_float32 a - d));
      c = Float32_u.(r.c + c);
      d = Float32_u.(d - (of_int r.b));
      e = Float32_u.(to_int (f + (of_int r.e)));
      f = r.f;
      dummy}

let _ =
  Printf.printf "Test 17, matching and projection:\n";
  print_t17 (f17_1 t17_1 t17_2)

(* Record update and mutation *)
let f17_2 ({a; d; _} as r1) r2 =
  r1.d <- Float32_u.of_float32 42.0s;
  let r3 = { r2 with c = r1.d;
                     d = Float32_u.of_float32 25.0s }
  in
  r3.b <- Float32_u.(to_int (of_float32 a + d));
  r2.b <- 17;
  r1.f <- r2.c;
  r3

let _ =
  Printf.printf "Test 17, record update and mutation:\n";
  let t17_3 = f17_2 t17_1 t17_2 in
  print_t17 t17_1;
  print_t17 t17_2;
  print_t17 t17_3

(************************************************************)
(* Test 18: (float32# + immediate) records in recursive groups *)

let rec f r =
  r.d <- Float32_u.of_int t18_1.b;
  t18_2.b <- 42;
  t18_1.f <- Float32_u.of_float32 t18_1.a;
  Float32_u.(of_float32 r.a + of_float32 t18_2.a)


and t18_1 = { a = 1.1s;
              b = 2;
              c = Float32_u.of_float32 3.3s;
              d = Float32_u.of_float32 4.4s;
              e = 5;
              f = Float32_u.of_float32 6.6s;
              dummy;
  }

and t18_2 = { a = (- 5.1s);
              b = -6;
              c = Float32_u.of_float32 (-7.3s);
              d = Float32_u.of_float32 (-8.4s);
              e = -9;
              f = Float32_u.of_float32 (-10.6s);
              dummy;
            }

let _ =
  Printf.printf "Test 18, (float32#+imm) records in recursive groups:\n";
  print_t17 t18_1;
  print_t17 t18_2;
  let result = f t18_1 in
  print_floatu "  result (-4.00)" result;
  print_t17 t18_1;
  print_t17 t18_2
