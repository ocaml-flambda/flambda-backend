(* TEST
   * runtime5
   ** flambda2
   reference = "${test_source_directory}/test_mixed_blocks.reference"
   *** native
     flags = "-extension layouts_alpha"
   *** bytecode
     flags = "-extension layouts_alpha"
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

let dummy = "dummy"

(*************************************************)
(* Test 1: basic mixed records involving float# *)

type mixedargs = { x2_1 : float;
                   x4_1 : float;
                   x6_1 : float;
                   x8_1 : float;
                   (* We include the string field to document more plainly that
                      the preceding section of [float] fields are boxed.
                      Without the [str] field, an implementation of mixed blocks
                      could more conceivably unbox the [float] fields.
                   *)
                   dummy : string;
                   x0_1 : int;
                   x0_2 : int;
                   x1 : float#;
                   x2_2 : int;
                   x3 : float#;
                   x4_2 : int;
                   x5 : float#;
                   x6_2 : int;
                   x7 : float#;
                   x8_2 : int;
                   x9 : float# }

(* Get some float# args by pattern matching and others by projection *)
let[@inline_never] f1 steps ({ x1; x0_1=start_k; x0_2=end_k; x8_1; x8_2; x5;
                               x6_1; x6_2 } as fargs) () =
  let[@inline never] rec go k =
    if k = end_k
    then Float_u.of_float 0.
    else begin
      let (x2_1, x2_2) = (fargs.x2_1, fargs.x2_2) in
      let {x4_1; x4_2; _} = fargs in
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

let test1 () =
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
    { x0_1 = 4; x0_2 = 8; x1; x2_1; x2_2; x3; x4_1; x4_2; x5; x6_1; x6_2; x7;
      x8_1; x8_2; x9;
      dummy }
  in

  let f1 = f1 steps fargs in
  print_floatu "Test 1, 610.68: " (f1 ());
  Array.iteri (Printf.printf "  Test 1, step %d: %.2f\n") steps

let _ = test1 ()

(*****************************************************)
(* Test 2: mixed record manipulation *)

type t2 = { a : float;
            dummy : string;
             mutable b : int;
             c : float#;
             mutable d : float#;
             e : int;
             mutable f : float# }

(* Construction *)
let t2_1 = { a = 3.14;
              b = 13;
              c = Float_u.of_float 7.31;
              d = Float_u.of_float 1.41;
              e = 6;
              f = Float_u.of_float 27.1;
              dummy;
            }

let t2_2 = { a = (-3.14);
              b = -13;
              c = Float_u.of_float (-7.31);
              d = Float_u.of_float (-1.41);
              e = -6;
              f = Float_u.of_float (-27.1);
              dummy;
            }

let print_t2 t2 =
  print_float "  a" t2.a;
  print_int "  b" t2.b;
  print_floatu "  c" t2.c;
  print_floatu "  d" t2.d;
  print_int "  e" t2.e;
  print_floatu "  f" t2.f

let _ =
  Printf.printf "Test 2, construction:\n";
  print_t2 t2_1;
  print_t2 t2_2

(* Matching, projection *)
let f2_1 {c; d; f; _} r =
  match r with
  | { a; _ } ->
    { a = Float.of_int r.e;
      b = Float_u.(to_int (of_float a - d));
      c = Float_u.(r.c + c);
      d = Float_u.(d - (of_int r.b));
      e = Float_u.(to_int (f + (of_int r.e)));
      f = r.f;
      dummy}

let _ =
  Printf.printf "Test 2, matching and projection:\n";
  print_t2 (f2_1 t2_1 t2_2)

(* Record update and mutation *)
let f2_2 ({a; d; _} as r1) r2 =
  r1.d <- Float_u.of_float 42.0;
  let r3 = { r2 with c = r1.d;
                     d = Float_u.of_float 25.0 }
  in
  r3.b <- Float_u.(to_int (of_float a + d));
  r2.b <- 17;
  r1.f <- r2.c;
  r3

let _ =
  Printf.printf "Test 2, record update and mutation:\n";
  let t2_3 = f2_2 t2_1 t2_2 in
  print_t2 t2_1;
  print_t2 t2_2;
  print_t2 t2_3

(*************************************************************)
(* Test 3: (float# + immediate) records in recursive groups *)

let rec f r =
  r.d <- Float_u.of_int t3_1.b;
  t3_2.b <- 42;
  t3_1.f <- Float_u.of_float t3_1.a;
  Float_u.(of_float r.a + of_float t3_2.a)


and t3_1 = { a = 1.1;
              b = 2;
              c = Float_u.of_float 3.3;
              d = Float_u.of_float 4.4;
              e = 5;
              f = Float_u.of_float 6.6;
              dummy;
  }

and t3_2 = { a = (- 5.1);
              b = -6;
              c = Float_u.of_float (-7.3);
              d = Float_u.of_float (-8.4);
              e = -9;
              f = Float_u.of_float (-10.6);
              dummy;
            }

let _ =
  Printf.printf "Test 3, (float#+imm) records in recursive groups:\n";
  print_t2 t3_1;
  print_t2 t3_2;
  let result = f t3_1 in
  print_floatu "  result (-4.00)" result;
  print_t2 t3_1;
  print_t2 t3_2
