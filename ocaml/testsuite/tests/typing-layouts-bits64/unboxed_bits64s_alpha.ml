(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }
*)

(* This should be read as a continuation of the [unboxed_bits64s.ml] test.
   We can't put them there because:
     - [unboxed_bits64s.ml] is run at all maturities, but
     - these tests use features that only are enabled at the alpha maturity.

   Once mixed blocks move to the "stable" maturity level, we can
   move these tests there.
 *)

(*****************************************)
(* Prelude: Functions on unboxed int64s. *)

module Int64_u = struct
  include stdlib_upstream_compatible.Int64_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( > ) x y = (compare x y) > 0
end

let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x

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

(***********************************)
(* Test: mixed blocks in closures *)

(* Adapted from Test 3 in unboxed_bits64s.ml *)

type block =
  { x0_1 : int;
    x0_2 : int;
    x1 : int64#;
    x2_1 : int;
    x2_2 : int;
    x3 : int64#;
    x4_1 : int;
    x4_2 : int;
    x5 : int64#;
    x6_1 : int;
    x6_2 : int;
    x7 : int64#;
    x8_1 : int;
    x8_2 : int;
    x9 : int64#;
  }

let[@inline_never] f_mixed_blocks_and_closures
    steps ({ x1; x0_1 = start_k; x0_2 = end_k; x8_1; x8_2;
             x5; x6_1; x6_2; } as iargs) () =
  let[@inline never] rec go k =
    if k = end_k
    then #0L
    else begin
      let (x2_1, x2_2) = iargs.x2_1, iargs.x2_2 in
      let {x4_1; x4_2; _} = iargs in
      let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
      let acc = go (k + 1) in
      steps.(k) <- Int64_u.to_int64 acc;
      Int64_u.(acc + ((x1 + iargs.x3 + x5 + iargs.x7 + iargs.x9)
                      * (of_int64 (Int64.of_int sum))))
    end
  in
  go start_k

let test_mixed_blocks_and_closures () =
  (* Test f_mixed_blocks_and_closures

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
  let x2_1, x2_2 = (7, 42) in
  let x4_1, x4_2 = (-23, 109) in
  let x6_1, x6_2 = (-242, 90) in
  let x8_1, x8_2 = (-2, 22) in

  let f = f_mixed_blocks_and_closures steps
      { x0_1 = 4; x0_2 = 8; x1; x2_1; x2_2; x3; x4_1; x4_2; x5;
        x6_1; x6_2; x7; x8_1; x8_2; x9 }
  in
  print_int64u "Test mixed_blocks_with_closures, 171: " (f ());
  Array.iteri (Printf.printf "  Test mixed_blocks_with_closures, step %d: %Ld\n")
    steps

let _ = test_mixed_blocks_and_closures ()

(**************************************)
(* Test: mixed record manipulation *)

type t_mixed_record =
  { a : float;
    mutable b : int;
    c : int64#;
    mutable d : int64#;
    e : int;
    mutable f : int64# }

(* Construction *)
let t_mixed1 = { a = 317.;
              b = 1300;
              c = #731L;
              d = #141L;
              e = 600;
              f = #2710L;
            }

let t_mixed2 = { a = (-317.);
              b = -1300;
              c = -#731L;
              d = -#141L;
              e = -600;
              f = -#2710L;
            }

let print_t_mixed t =
  print_float "  a" t.a;
  print_int "  b" t.b;
  print_int64u "  c" t.c;
  print_int64u "  d" t.d;
  print_int "  e" t.e;
  print_int64u "  f" t.f

let _ =
  Printf.printf "Test mixed record construction:\n";
  print_t_mixed t_mixed1;
  print_t_mixed t_mixed2

(* Matching, projection *)
let f_mixed1 {c; d; f; _} r =
  match r with
  | { a; _ } ->
    { a = Float.of_int r.e;
      b = Int64_u.(to_int (of_float a - d));
      c = Int64_u.(r.c + c);
      d = Int64_u.(d - (of_int r.b));
      e = Int64_u.(to_int (f + (of_int r.e)));
      f = r.f;
    }

let _ =
  Printf.printf "Test mixed record matching and projection:\n";
  print_t_mixed (f_mixed1 t_mixed1 t_mixed2)

(* Record update and mutation *)
let f_mixed2 ({a; d; _} as r1) r2 =
  r1.d <- #4200L;
  let r3 = { r2 with c = r1.d;
                     d = #2500L; }
  in
  r3.b <- Int64_u.(to_int (of_float a + d));
  r2.b <- 1700;
  r1.f <- r2.c;
  r3

let _ =
  Printf.printf "Test mixed record update and mutation:\n";
  let t_mixed3 = f_mixed2 t_mixed1 t_mixed2 in
  print_t_mixed t_mixed1;
  print_t_mixed t_mixed2;
  print_t_mixed t_mixed3

(************************************************************)
(* Test mixed records in recursive groups *)

let rec f r =
  r.d <- Int64_u.of_int t_rec1.b;
  t_rec2.b <- 42;
  t_rec1.f <- Int64_u.of_float t_rec1.a;
  Int64_u.(of_float r.a + of_float t_rec2.a)


and t_rec1 = { a = 11.;
              b = 2;
              c = #33L;
              d = #44L;
              e = 5;
              f = #66L;
  }

and t_rec2 = { a = (- 51.);
              b = -6;
              c = -#73L;
              d = -#84L;
              e = -9;
              f = -#106L;
            }

let _ =
  Printf.printf "Test 18, mixed records in recursive groups:\n";
  print_t_mixed t_rec1;
  print_t_mixed t_rec2;
  let result = f t_rec1 in
  print_int64u "  result (-40)" result;
  print_t_mixed t_rec1;
  print_t_mixed t_rec2
