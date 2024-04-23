(* TEST
 reference = "${test_source_directory}/unboxed_floats_alpha.reference";
 flambda2;
 {
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }
*)

(* This should be read as a continuation of the [unboxed_floats.ml] test.
   We can't put them there because:
     - [unboxed_floats.ml] is run at all maturities, but
     - these tests use features that only are enabled at the alpha maturity.

   Once mixed blocks move to the "stable" maturity level, we can
   move these tests there.
 *)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module Float_u = struct
  include Stdlib__Float_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( > ) x y = (compare x y) > 0
end

let print_floatu prefix x = Printf.printf "%s: %.2f\n" prefix (Float_u.to_float x)
let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x
let print_int prefix x = Printf.printf "%s: %d\n" prefix x

(*************************************************)
(* Test 13: basic float + float# records *)

(* Copy of test 3, everything is in the record, with boxed float fields
   necessarily in the prefix of the record.
*)
type mixed_float_record =
  { x1 : float#;
    x2_1 : float;
    x3 : float#;
    x4_1 : float;
    x5 : float#;
    x6_1 : float;
    x7 : float#;
    x8_1 : float;
    x9 : float# }

type int_args =
  { x0_1 : int;
    x0_2 : int;
    x2_2 : int;
    x4_2 : int;
    x6_2 : int;
    x8_2 : int;
  }

(* Get some float# args by pattern matching and others by projection *)
let[@inline_never] f13 steps ({ x1; x8_1; x5; x6_1 } as fargs)
  ({ x0_1=start_k; x0_2=end_k; x4_2; x8_2 } as iargs) () =
  let[@inline never] rec go k =
    if k = end_k
    then Float_u.of_float 0.
    else begin
      let (x2_1, x2_2) = (fargs.x2_1, iargs.x2_2) in
      let {x4_1; _}, {x6_2; _} = fargs, iargs in
      let sum =
        Float_u.(of_float x2_1 + of_int x2_2 + of_float x4_1 + of_int x4_2
                 + of_float x6_1 + of_int x6_2 + of_float x8_1 + of_int x8_2)
      in
      let acc = go (k + 1) in
      steps.(k) <- Float_u.to_float acc;
      Float_u.(acc + ((x1 + fargs.x3 + x5 + fargs.x7 + fargs.x9)
                      * sum))
    end
  in
  go start_k

let test13 () =
  (* same math as f3_manyargs *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let x1 = Float_u.of_float 3.14 in
  let x3 = Float_u.of_float 2.72 in
  let x5 = Float_u.of_float 1.62 in
  let x7 = Float_u.of_float 1.41 in
  let x9 = Float_u.of_float 42.0 in

  (* these sum to 3.0 *)
  let x2_1 = 6.6 in
  let x2_2 = 42 in
  let x4_1 = -22.9 in
  let x4_2 = 109 in
  let x6_1 = -241.2 in
  let x6_2 = 90 in
  let x8_1 = -2.5 in
  let x8_2 = 22 in

  let fargs =
    { x1; x2_1; x3; x4_1; x5; x6_1; x7;
      x8_1; x9; }
  in
  let iargs = { x0_1 = 4; x0_2 = 8; x2_2; x4_2; x6_2; x8_2 } in

  let f13 = f13 steps fargs iargs in
  print_floatu "Test 13, 610.68: " (f13 ());
  Array.iteri (Printf.printf "  Test 13, step %d: %.2f\n") steps

let _ = test13 ()

(*****************************************************)
(* Test 14: (float# + float) record manipulation *)

type t14 = { a : float#;
             mutable b : float;
             c : float#;
             mutable d : float#;
             e : float;
             mutable f : float# }

(* Construction *)
let t14_1 = { a = Float_u.of_float 3.14;
              b = 13.;
              c = Float_u.of_float 7.31;
              d = Float_u.of_float 1.41;
              e = 6.;
              f = Float_u.of_float 27.1
            }

let t14_2 = { a = Float_u.of_float (-3.14);
              b = -13.;
              c = Float_u.of_float (-7.31);
              d = Float_u.of_float (-1.41);
              e = -6.;
              f = Float_u.of_float (-27.1)
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
    { a = (Float_u.of_float r.e);
      b = Float_u.(to_float (a - d));
      c = Float_u.(r.c + c);
      d = Float_u.(d - (of_float r.b));
      e = Float_u.(to_float (f + (of_float r.e)));
      f = r.f}

let _ =
  Printf.printf "Test 14, matching and projection:\n";
  print_t14 (f14_1 t14_1 t14_2)

(* Record update and mutation *)
let f14_2 ({a; d; _} as r1) r2 =
  r1.d <- Float_u.of_float 42.0;
  let r3 = { r2 with c = r1.d;
                     d = Float_u.of_float 25.0 }
  in
  r3.b <- Float_u.(to_float (a + d));
  r2.b <- 17.;
  r1.f <- r2.c;
  r3

let _ =
  Printf.printf "Test 14, record update and mutation:\n";
  let t14_3 = f14_2 t14_1 t14_2 in
  print_t14 t14_1;
  print_t14 t14_2;
  print_t14 t14_3

(*************************************************************)
(* Test 15: (float# + float) records in recursive groups *)

let rec f r =
  r.d <- Float_u.of_float t15_1.b;
  t15_2.b <- 42.;
  t15_1.f <- t15_1.a;
  Float_u.(r.a + t15_2.a)


and t15_1 = { a = Float_u.of_float 1.1;
              b = 2.;
              c = Float_u.of_float 3.3;
              d = Float_u.of_float 4.4;
              e = 5.;
              f = Float_u.of_float 6.6}

and t15_2 = { a = Float_u.of_float (- 5.1);
              b = -6.;
              c = Float_u.of_float (-7.3);
              d = Float_u.of_float (-8.4);
              e = -9.;
              f = Float_u.of_float (-10.6) }

let _ =
  Printf.printf "Test 15, (float#+float) records in recursive groups:\n";
  print_t14 t15_1;
  print_t14 t15_2;
  let result = f t15_1 in
  print_floatu "  result (-4.00)" result;
  print_t14 t15_1;
  print_t14 t15_2
