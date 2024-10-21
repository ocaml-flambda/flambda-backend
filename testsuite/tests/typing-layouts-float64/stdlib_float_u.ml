(* TEST
 include stdlib_upstream_compatible;
 {
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

module Float_u = Stdlib_upstream_compatible.Float_u

(* Constant seed for repeatable random-testing properties *)
let () = Random.init 1234

type 'a result = {
  actual : 'a;
  expected : 'a;
  equal : 'a -> 'a -> bool;
  to_string : 'a -> string
}

let float_result ~actual ~expected = {
  actual;
  expected;
  equal = Float.equal;
  to_string = Float.to_string;
}

let bool_result ~actual ~expected =  {
  actual;
  expected;
  equal = Bool.equal;
  to_string = Bool.to_string;
}

let int_result ~actual ~expected =  {
  actual;
  expected;
  equal = Int.equal;
  to_string = Int.to_string;
}

let string_result ~actual ~expected =  {
  actual;
  expected;
  equal = String.equal;
  to_string = fun x -> x;
}

let fpclass_to_string = function
    FP_normal -> "FP_normal"
  | FP_subnormal -> "FP_subnormal"
  | FP_zero -> "FP_zero"
  | FP_infinite -> "FP_infinite"
  | FP_nan -> "FP_nan"

let fpclass_result ~actual ~expected =  {
  actual;
  expected;
  equal = (=);
  to_string = fpclass_to_string;
}

let interesting_floats =
  [ 0.; 1.; -1.; Float.max_float; Float.min_float; Float.epsilon;
    Float.nan; Float.infinity; Float.neg_infinity ]

let interesting_ints = [ 0; 1; -1; Int.max_int; Int.min_int ]

let default_min = -10000.
let default_max = 10000.

let floats_in_range ~num min max =
  (* Generating well-distributed random floats in a range is obviously hard.
     The "in a range" part is important because many float functions are only
     defined on certain ranges.  I'm not trying very hard, here - this should
     only be used with min and max that aren't at the outer limits of the float
     range. *)
  let float_in_range () =
    let f = Random.float 1000. in
    let f = f *. ((max -. min) /. 1000.) in
    f +. min
  in
  List.init num (fun _ -> float_in_range ())

let float_inputs ~range ~num =
  let min, max =
    match range with
    | None -> default_min, default_max
    | Some (min, max) -> min, max
  in
  let input = floats_in_range ~num min max in
  let input =
    if Option.is_none range then interesting_floats @ input else input
  in
  input

let string_inputs ~num =
  List.map Float.to_string (float_inputs ~range:None ~num)

let int_inputs ~num =
  let gen_int _ = (Random.full_int Int.max_int) - (Int.max_int / 2) in
  interesting_ints @ List.init num gen_int

let passed { actual; expected; equal; _ } = equal actual expected

let test inputs input_to_string name prop =
  let test x =
    let {expected; actual; to_string} as result = prop x in
    if not (passed result)
    then
      Printf.printf "Test failed: %s. Input = %s; expected = %s; actual = %s\n"
        name (input_to_string x) (to_string expected) (to_string actual)
  in
  List.iter test inputs

(* zips that truncate *)
let rec zip l1 l2 =
  match l1, l2 with
  | x1 :: l1, x2 :: l2 -> (x1, x2) :: zip l1 l2
  | _ -> []

let rec zip3 l1 l2 l3 =
  match l1, l2, l3 with
  | x1 :: l1, x2 :: l2, x3 :: l3 -> (x1, x2, x3) :: zip3 l1 l2 l3
  | _ -> []

(* These run a property on inputs and check the result *)
let test_unary ?range name prop =
  let inputs = float_inputs ~range ~num:10 in
  let input_to_string = Float.to_string in
  test inputs input_to_string name prop

let test_unary_int ?range name prop =
  let inputs = int_inputs ~num:10 in
  let input_to_string = Int.to_string in
  test inputs input_to_string name prop

let test_unary_string ?range name prop =
  let inputs = string_inputs ~num:10 in
  let input_to_string x = x in
  test inputs input_to_string name prop

let test_binary ?range name prop =
  let input1 = float_inputs ~range ~num:20 in
  let input2 = List.rev (float_inputs ~range ~num:20) in
  let inputs = zip input1 (List.rev input2) in
  let input_to_string (f1,f2) = Printf.sprintf "(%f, %f)" f1 f2 in
  test inputs input_to_string name prop

let test_binary_float_int ?range name prop =
  let input1 = float_inputs ~range ~num:20 in
  let input2 = List.rev (int_inputs ~num:20) in
  let inputs = zip input1 (List.rev input2) in
  let input_to_string (f1,f2) = Printf.sprintf "(%f, %d)" f1 f2 in
  test inputs input_to_string name prop

let test_ternary ?range name prop =
  let input1 = float_inputs ~range ~num:30 in
  let input2 = float_inputs ~range ~num:30 in
  let input3 = List.rev (float_inputs ~range ~num:30) in
  let inputs = zip3 input1 input2 input3 in
  let input_to_string (f1, f2, f3) = Printf.sprintf "(%f, %f, %f)" f1 f2 f3 in
  test inputs input_to_string name prop

(* These make the property to be tested for various arities and types *)
let mk1 expected_f actual_f arg =
  let expected = expected_f arg in
  let actual = Float_u.to_float (actual_f (Float_u.of_float arg)) in
  float_result ~actual ~expected

let mk2 expected_f actual_f (arg1, arg2) =
  let expected = expected_f arg1 arg2 in
  let actual =
    Float_u.to_float
      (actual_f (Float_u.of_float arg1) (Float_u.of_float arg2))
  in
  float_result ~actual ~expected

let mk3 expected_f actual_f (arg1, arg2, arg3) =
  let expected = expected_f arg1 arg2 arg3 in
  let actual =
    Float_u.to_float
      (actual_f (Float_u.of_float arg1) (Float_u.of_float arg2)
         (Float_u.of_float arg3))
  in
  float_result ~actual ~expected

let mk_float_X result expected_f actual_f arg =
  let expected = expected_f arg in
  let actual = actual_f (Float_u.of_float arg) in
  result ~actual ~expected

let mk_X_float expected_f actual_f arg =
  let expected = expected_f arg in
  let actual = Float_u.to_float (actual_f arg) in
  float_result ~actual ~expected

let mk_float_X_float expected_f actual_f (arg1, arg2) =
  let expected = expected_f arg1 arg2 in
  let actual = Float_u.to_float (actual_f (Float_u.of_float arg1) arg2) in
  float_result ~actual ~expected

let mk_float_float_X result expected_f actual_f (arg1, arg2) =
  let expected = expected_f arg1 arg2 in
  let actual = actual_f (Float_u.of_float arg1) (Float_u.of_float arg2) in
  result ~actual ~expected

let () =
  test_unary "neg" (mk1 Float.neg Float_u.neg);
  test_binary "add" (mk2 Float.add Float_u.add);
  test_binary "sub" (mk2 Float.sub Float_u.sub);
  test_binary "mul" (mk2 Float.mul Float_u.mul);
  test_binary "div" (mk2 Float.div Float_u.div);
  test_ternary "fma" (mk3 Float.fma Float_u.fma);
  test_binary "rem" (mk2 Float.rem Float_u.rem);
  test_unary "succ" (mk1 Float.succ Float_u.succ);
  test_unary "pred" (mk1 Float.pred Float_u.pred);
  test_unary "abs" (mk1 Float.abs Float_u.abs);
  test_unary "is_finite"
    (mk_float_X bool_result Float.is_finite Float_u.is_finite);
  test_unary "is_nan" (mk_float_X bool_result Float.is_nan Float_u.is_nan);
  test_unary "is_integer"
    (mk_float_X bool_result Float.is_integer Float_u.is_integer);
  test_unary_int "of_int" (mk_X_float Float.of_int Float_u.of_int);
  test_unary "to_int" (mk_float_X int_result Float.to_int Float_u.to_int);
  test_unary_string "of_string" (mk_X_float Float.of_string Float_u.of_string);
  test_unary "to_string"
    (mk_float_X string_result Float.to_string Float_u.to_string);
  test_unary "classify_float"
    (mk_float_X fpclass_result Float.classify_float Float_u.classify_float);
  test_binary "pow" (mk2 Float.pow Float_u.pow);
  test_unary "sqt" (mk1 Float.sqrt Float_u.sqrt);
  test_unary "cbrt" (mk1 Float.cbrt Float_u.cbrt);
  test_unary "exp" (mk1 Float.exp Float_u.exp);
  test_unary "exp2" (mk1 Float.exp2 Float_u.exp2);
  test_unary "log" (mk1 Float.log Float_u.log);
  test_unary "log10" (mk1 Float.log10 Float_u.log10);
  test_unary "log2" (mk1 Float.log2 Float_u.log2);
  test_unary "log1p" (mk1 Float.log1p Float_u.log1p);
  test_unary "cos" (mk1 Float.cos Float_u.cos);
  test_unary "sin" (mk1 Float.sin Float_u.sin);
  test_unary "tan" (mk1 Float.tan Float_u.tan);
  test_unary "acos" ~range:(-1.0, 1.0) (mk1 Float.acos Float_u.acos);
  test_unary "asin" ~range:(-1.0, 1.0) (mk1 Float.asin Float_u.asin);
  test_unary "atan" (mk1 Float.atan Float_u.atan);
  test_binary "atan2" (mk2 Float.atan2 Float_u.atan2);
  test_binary "hypot" (mk2 Float.hypot Float_u.hypot);
  test_unary "cosh" (mk1 Float.cosh Float_u.cosh);
  test_unary "sinh" (mk1 Float.sinh Float_u.sinh);
  test_unary "tanh" (mk1 Float.tanh Float_u.tanh);
  test_unary "acosh" ~range:(1.0, Float.infinity)
    (mk1 Float.acosh Float_u.acosh);
  test_unary "asinh" (mk1 Float.asinh Float_u.asinh);
  test_unary "atanh" ~range:(-1.0, 1.0) (mk1 Float.atanh Float_u.atanh);
  test_unary "erf" (mk1 Float.erf Float_u.erf);
  test_unary "erfc" (mk1 Float.erfc Float_u.erfc);
  test_unary "trunk" (mk1 Float.trunc Float_u.trunc);
  test_unary "round" (mk1 Float.round Float_u.round);
  test_unary "ceil" (mk1 Float.ceil Float_u.ceil);
  test_unary "floor" (mk1 Float.floor Float_u.floor);
  test_binary "next_after" (mk2 Float.next_after Float_u.next_after);
  test_binary "copy_sign" (mk2 Float.copy_sign Float_u.copy_sign);
  test_unary "sign_bit"
    (mk_float_X bool_result Float.sign_bit Float_u.sign_bit);
  test_binary_float_int "ldexp" (mk_float_X_float Float.ldexp Float_u.ldexp);
  test_binary "compare"
    (mk_float_float_X int_result Float.compare Float_u.compare);
  test_binary "equal"
    (mk_float_float_X bool_result Float.equal Float_u.equal);
  test_binary "min" (mk2 Float.min Float_u.min);
  test_binary "max" (mk2 Float.max Float_u.max);
  test_binary "min_num" (mk2 Float.min_num Float_u.min_num);
  test_binary "max_num" (mk2 Float.max_num Float_u.max_num);
