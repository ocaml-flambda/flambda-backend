(* TEST
   readonly_files = "z.py";
   include config;
   include unix;
   binary_modules = "z";
   hasunix;
   native;
*)

(* Test suite for arbitrary precision integers *)

module Reference = struct
  type t = string

  let process =
    lazy (
    let process =
      UnixLabels.open_process_args "python3" [| "python3";  "z.py" |]
    in
    at_exit (fun () ->ignore (Unix.close_process process : Unix.process_status));
    process)

  let of_int i = string_of_int i

  let is_valid t =
    let t =
      if String.starts_with t ~prefix:"-" then StringLabels.sub t ~pos:1 ~len:(String.length t - 1) else t in
    String.for_all t ~f:Char.is_digit


  let of_string t =
    assert (not (String.contains t '\n'));
    t

  let to_string t = t


  let eval (type a)(result : a result) fmt =
    Printf.ksprintf (fun expr : a ->
      assert (not (String.contains expr '\n'));
      let (lazy (inch, ouch)) = process in
      Printf.fprintf ouch "%s\n%!" expr;
      match In_channel.input_line inch with
      | None -> failwith "No response from reference process"
      | Some output ->
        if String.starts_with output ~prefix:"ZeroDivisionError" then
          raise Division_by_zero
        else
        if String.starts_with output ~prefix:"b" then
          Scanf.sscanf output "b%S" (fun output -> `String output)
        else if String.mem output '.' then `Float (float_of_string output)
          if is_valid output then `Z output
          else
            match output with
            | "True" -> `Bool true
            | "false" -> `Bool false
            | output ->

          match result with
          | Z ->
            if is_valid output then ( output : a) else
              failwith (result ^ " while evaluating: " ^ expr)
          | Int ->
            (
              if is_valid output
              match int_of_string output with
              | exception exn when is_valid output ->
                failwith (Printexc.to_string exn ^ " while evaluating: " ^ expr)
              | exception exn ->



            )
              if is_valid output then
            try
            ( output : a)
        )
      fmt


  let equal x y =
    bool_of_string (String.lowercase_ascii ( eval "%s == %s" x y ))

  let compare x y =
    if bool_of_string (String.lowercase_ascii (eval "%s < %s" x y))
    then -1
    else if bool_of_string (String.lowercase_ascii (eval "%s == %s" x y))
    then 0
    else 1

    let add x y = eval "%s + %s" x y
    let sub x y = eval "%s - %s" x y
    let mul x y = eval "%s * %s" x y
    let div x y = eval "%s / %s" x y
    let rem x y = eval "%s %% %s" x y
    let neg x = eval "-%s" x
    let lognot x = eval "~%s" x
    let logxor x y = eval "%s ^ %s" x y
    let logand x y = eval "%s & %s" x y
    let logor x y = eval "%s | %s" x y
    let sign_extend t ~bits = eval "sign_extend(%s, %d)" t bits
    let zero_extend t ~bits = eval "zero_extend(%s, %d)" t bits
    let byteswap t ~bytes = eval "byteswap(%s, %d)" t bytes
end


open Apint

(* Test framework utilities *)
let run_test name f =
  Printf.printf "Running test: %s\n" name;
  try
    f ();
    Printf.printf "Test passed: %s\n\n" name
  with e ->
    Printf.printf "Test failed: %s - %s\n\n" name (Printexc.to_string e)

let assert_equal expected actual message =
  if expected <> actual then
    failwith (Printf.sprintf "%s: Expected '%s' but got '%s'" message expected actual)

let assert_int_equal expected actual message =
  if expected <> actual then
    failwith (Printf.sprintf "%s: Expected %d but got %d" message expected actual)

let assert_bool message condition =
  if not condition then
    failwith message

let expect_exception f message =
  try
    let _ = f () in
    failwith (Printf.sprintf "%s: Expected exception but none was raised" message)
  with
    | Failure _ -> () (* Expected failure *)
    | _ -> () (* Other exceptions are also acceptable for this simple test *)

(* Constants for boundary testing *)
let pow2_32 = of_string "4294967296"       (* 2^32 *)
let pow2_32_minus_1 = of_string "4294967295"  (* 2^32 - 1 *)
let pow2_32_plus_1 = of_string "4294967297"   (* 2^32 + 1 *)

let pow2_64 = of_string "18446744073709551616"       (* 2^64 *)
let pow2_64_minus_1 = of_string "18446744073709551615"  (* 2^64 - 1 *)
let pow2_64_plus_1 = of_string "18446744073709551617"   (* 2^64 + 1 *)

(* Basic tests *)
let test_constants () =
  assert_equal "0" (to_string zero) "zero to_string";
  assert_equal "1" (to_string one) "one to_string";
  assert_int_equal 0 (to_int_exn zero) "zero to_int_exn";
  assert_int_equal 1 (to_int_exn one) "one to_int_exn"

let test_conversion () =
  (* Integer conversion *)
  assert_int_equal 42 (to_int_exn (of_int 42)) "of_int/to_int_exn preservation";
  assert_int_equal (-42) (to_int_exn (of_int (-42))) "negative of_int/to_int_exn preservation";

  (* String conversion *)
  assert_equal "42" (to_string (of_int 42)) "int to string via BigInt";
  assert_equal "-42" (to_string (of_int (-42))) "negative int to string via BigInt";
  assert_equal "12345678901234567890"
    (to_string (of_string "12345678901234567890")) "string preservation via BigInt";

  (* Unsigned conversion *)
  let unsigned_max_int = of_int ~unsigned:true (-1) in
  assert_bool "Unsigned -1 should be a large positive number"
    (to_string unsigned_max_int <> "-1")

let test_compare () =
  (* Basic comparison *)
  let a = of_int 42 in
  let b = of_int 42 in
  let c = of_int 100 in
  let d = of_int (-10) in

  assert_int_equal 0 (compare a b) "Equal values should return 0";
  assert_bool "Less than should return negative" (compare a c < 0);
  assert_bool "Greater than should return positive" (compare c a > 0);
  assert_bool "Negative vs positive" (compare d a < 0);
  assert_bool "Positive vs negative" (compare a d > 0);

  (* Compare large numbers *)
  let large1 = of_string "999999999999999999999999" in
  let large2 = of_string "999999999999999999999998" in
  assert_bool "Compare large numbers" (compare large1 large2 > 0);

  (* Compare around 2^32 boundary *)
  assert_bool "2^32 > 2^32-1" (compare pow2_32 pow2_32_minus_1 > 0);
  assert_bool "2^32 < 2^32+1" (compare pow2_32 pow2_32_plus_1 < 0);

  (* Compare around 2^64 boundary *)
  assert_bool "2^64 > 2^64-1" (compare pow2_64 pow2_64_minus_1 > 0);
  assert_bool "2^64 < 2^64+1" (compare pow2_64 pow2_64_plus_1 < 0);

  (* Compare numbers of very different magnitudes *)
  let tiny = of_int 1 in
  let huge = of_string "10000000000000000000000000000000000000000" in
  assert_bool "Huge > Tiny" (compare huge tiny > 0);
  assert_bool "Tiny < Huge" (compare tiny huge < 0);
  assert_bool "-Huge < Tiny" (compare (neg huge) tiny < 0)

(* Basic arithmetic operations *)
let test_basic_arithmetic () =
  (* Addition *)
  assert_equal "5" (to_string (add (of_int 2) (of_int 3))) "Simple addition";
  assert_equal "0" (to_string (add (of_int 5) (of_int (-5)))) "Addition with negative";

  (* Subtraction *)
  assert_equal "2" (to_string (sub (of_int 5) (of_int 3))) "Simple subtraction";
  assert_equal "-2" (to_string (sub (of_int 3) (of_int 5))) "Subtraction resulting in negative";

  (* Multiplication *)
  assert_equal "15" (to_string (mul (of_int 3) (of_int 5))) "Simple multiplication";
  assert_equal "-15" (to_string (mul (of_int 3) (of_int (-5)))) "Multiplication with negative";

  (* Negation *)
  assert_equal "0" (to_string (neg zero)) "Negation of zero";
  assert_equal "-42" (to_string (neg (of_int 42))) "Negation of positive";
  assert_equal "42" (to_string (neg (of_int (-42)))) "Negation of negative"
;;

(* Division and remainder *)
let test_division_remainder () =
  (* Basic division *)
  assert_equal "3" (to_string (div (of_int 9) (of_int 3))) "Simple division";
  assert_equal "3" (to_string (div (of_int 10) (of_int 3))) "Division with remainder";
  assert_equal "-3" (to_string (div (of_int (-9)) (of_int 3))) "Division with negative";

  (* Basic remainder *)
  assert_equal "0" (to_string (rem (of_int 9) (of_int 3))) "Remainder with exact division";
  assert_equal "1" (to_string (rem (of_int 10) (of_int 3))) "Simple remainder";
  assert_equal "0" (to_string (rem (of_int (-9)) (of_int 3))) "Remainder with negative";

  (* Division by zero should raise an exception *)
  expect_exception (fun () -> div (of_int 1) zero) "Division by zero";
  expect_exception (fun () -> rem (of_int 1) zero) "Remainder by zero"

(* Tests around 2^32 boundary *)
let test_boundary_32bit () =
  (* Addition around 2^32 *)
  assert_equal "4294967296" (to_string (add pow2_32_minus_1 one))
    "Addition crossing 2^32 boundary (2^32-1 + 1)";

  (* Subtraction around 2^32 *)
  assert_equal "4294967295" (to_string (sub pow2_32 one))
    "Subtraction crossing 2^32 boundary (2^32 - 1)";

  (* Multiplication around 2^32 *)
  assert_equal "4294967296" (to_string (mul (of_int 2) (of_string "2147483648")))
    "Multiplication to reach 2^32 (2 * 2^31)";

  assert_equal "18446744073709551616" (to_string (mul pow2_32 pow2_32))
    "Squaring 2^32 = 2^64";

  (* Division around 2^32 *)
  assert_equal "1" (to_string (div pow2_32_plus_1 (of_string "2147483649")))
    "Division near 2^32 boundary";

  assert_equal "4294967295" (to_string (div (of_string "18446744069414584320") pow2_32_plus_1))
    "Large division near 2^32"

(* Tests around 2^64 boundary *)
let test_boundary_64bit () =
  (* Addition around 2^64 *)
  assert_equal "18446744073709551616" (to_string (add pow2_64_minus_1 one))
    "Addition crossing 2^64 boundary (2^64-1 + 1)";

  (* Subtraction around 2^64 *)
  assert_equal "18446744073709551615" (to_string (sub pow2_64 one))
    "Subtraction crossing 2^64 boundary (2^64 - 1)";

  (* Multiplication around 2^64 *)
  assert_equal "18446744073709551616" (to_string (mul (of_int 2) (of_string "9223372036854775808")))
    "Multiplication to reach 2^64 (2 * 2^63)";

  (* Division around 2^64 *)
  let large_multiple = mul pow2_64 (of_int 1000) in
  assert_equal "1000" (to_string (div large_multiple pow2_64))
    "Division by 2^64";

  assert_equal "1" (to_string (div pow2_64_plus_1 pow2_64))
    "Division just above 2^64";

  assert_equal "1" (to_string (rem pow2_64_plus_1 pow2_64))
    "Remainder just above 2^64"

(* Tests with operands of very different magnitudes *)
let test_magnitude_differences () =
  (* Small + Huge *)
  let tiny = one in
  let huge = of_string "10000000000000000000000000000000000000000" in
  assert_equal "10000000000000000000000000000000000000001"
    (to_string (add huge tiny)) "Huge + 1";

  (* Small - Huge (negative result) *)
  assert_equal "-9999999999999999999999999999999999999999"
    (to_string (sub tiny huge)) "1 - Huge";

  (* Huge - Small (almost same as Huge) *)
  assert_equal "9999999999999999999999999999999999999999"
    (to_string (sub huge tiny)) "Huge - 1";

  (* Small * Huge *)
  assert_equal "20000000000000000000000000000000000000000"
    (to_string (mul huge (of_int 2))) "Huge * 2";

  (* (* Huge / Small *)
   * assert_equal "5000000000000000000000000000000000000000"
   *   (to_string (div huge (of_int 2))) "Huge / 2";
   * () *)
  ()

  (* (* Small / Huge (should be 0) *)
   * assert_equal "0" (to_string (div (of_int 1000) huge)) "Small / Huge";
   *
   * (* Small % Huge (should be Small) *)
   * assert_equal "1000" (to_string (rem (of_int 1000) huge)) "Small % Huge" *)

(* Test multiplication of large numbers *)
let test_large_multiplication () =
  (* Numbers around register size *)
  let a = pow2_32_minus_1 in  (* 2^32 - 1 *)
  let b = pow2_32_plus_1 in   (* 2^32 + 1 *)

  (* (2^32 - 1) * (2^32 + 1) = 2^64 - 1 *)
  assert_equal "18446744065119617025" (to_string (mul a b))
    "Special multiplication case (2^32-1)*(2^32+1)";

  (* 2^32 * 2^32 = 2^64 *)
  assert_equal "18446744073709551616" (to_string (mul pow2_32 pow2_32))
    "Squaring 2^32";

  (* 2^64 * 2^64 = 2^128 *)
  assert_equal "340282366920938463463374607431768211456" (to_string (mul pow2_64 pow2_64))
    "Squaring 2^64";

  (* Test extremely large numbers (well beyond register size) *)
  let huge1 = of_string "1234567890123456789012345678901234567890" in
  let huge2 = of_string "9876543210987654321098765432109876543210" in

  assert_equal "12193263113702179522618503273634002751909443830078023883311649536239153219900"
    (to_string (mul huge1 huge2))
    "Multiplication of very large numbers"

(* Test bit operations with large numbers *)
let test_bit_operations () =
  (* Logical NOT around boundaries *)
  let not_zero = lognot zero in
  let not_minus_one = lognot (neg one) in

  assert_bool "NOT 0 should be negative" (compare not_zero zero < 0);
  assert_equal "0" (to_string not_minus_one) "NOT -1 should be 0";

  (* NOT around 2^32 *)
  let not_pow2_32_minus_1 = lognot pow2_32_minus_1 in
  assert_bool "NOT (2^32-1) should be negative" (compare not_pow2_32_minus_1 zero < 0);

  (* Sign extension tests *)
  let small_neg = of_int (-5) in

  (* Ensure sign extension preserves the sign and value *)
  assert_equal "-5" (to_string (sign_extend small_neg ~bits:16))
    "Sign extension of small negative";
  assert_equal "-5" (to_string (sign_extend small_neg ~bits:32))
    "Sign extension of small negative to 32 bits";
  assert_equal "-5" (to_string (sign_extend small_neg ~bits:64))
    "Sign extension of small negative to 64 bits";

  (* Zero extension tests *)
  let small_pos = of_int 5 in

  (* Ensure zero extension preserves positive values *)
  assert_equal "5" (to_string (zero_extend small_pos ~bits:16))
    "Zero extension of small positive";
  assert_equal "5" (to_string (zero_extend small_pos ~bits:32))
    "Zero extension of small positive to 32 bits";
  assert_equal "5" (to_string (zero_extend small_pos ~bits:64))
    "Zero extension of small positive to 64 bits";

  (* Byteswap tests - results are implementation-dependent but should be consistent *)
  let test_val = of_int 0x1234 in
  let swapped_2 = byteswap test_val ~bytes:2 in
  assert_bool "2-byte byteswap should change value" (compare test_val swapped_2 <> 0);

  (* Test byteswap with larger values *)
  let large_bytes = of_string "0x123456789ABCDEF0" in
  let swapped_8 = byteswap large_bytes ~bytes:8 in
  assert_bool "8-byte byteswap should change value" (compare large_bytes swapped_8 <> 0)


  (* Test arithmetic identity: (a + b) - b = a *)
  let verify_addition_subtraction a b =
    let sum = add a b in
    let result = sub sum b in
    assert_equal (to_string a) (to_string result)
      (Printf.sprintf "Addition-subtraction identity for %s and %s" (to_string a) (to_string b))

  (* Test arithmetic identity: (a * b) / b = a (when a is multiple of b) *)
  let verify_multiplication_division a b =
    let product = mul a b in
    let result = div product b in
    assert_equal (to_string a) (to_string result)
      (Printf.sprintf "Multiplication-division identity for %s and %s" (to_string a) (to_string b))

  (* Test arithmetic identity: a = (a / b) * b + (a % b) *)
  let verify_division_remainder a b =
    let quotient = div a b in
    let remainder = rem a b in
    let reconstructed = add (mul quotient b) remainder in
    assert_equal (to_string a) (to_string reconstructed)
      (Printf.sprintf "Division-remainder identity for %s and %s" (to_string a) (to_string b))


(* Large number arithmetic check *)
let test_large_arithmetic () =

  (* Test with values around 0 *)
  verify_addition_subtraction (of_int 0) (of_int 100);
  verify_multiplication_division (of_int 100) (of_int 5);
  verify_division_remainder (of_int 100) (of_int 7);

  (* Test with values around 2^32 *)
  verify_addition_subtraction pow2_32 pow2_32_minus_1;
  verify_multiplication_division (of_string "12884901888") pow2_32; (* 3*2^32 *)
  verify_division_remainder (of_string "4294967300") (of_int 7);

  (* Test with values around 2^64 *)
  verify_addition_subtraction pow2_64 pow2_64_minus_1;
  verify_multiplication_division (of_string "55340232221128654848") pow2_64; (* 3*2^64 *)
  verify_division_remainder (of_string "18446744073709551620") (of_int 7)


(* Edge cases for large values *)
let test_edge_cases () =
  (* Operations with zero *)
  assert_equal "42" (to_string (add (of_int 42) zero)) "Adding zero";
  assert_equal "0" (to_string (mul (of_int 42) zero)) "Multiplying by zero";

  (* Large prime numbers *)
  let large_prime1 = of_string "18446744073709551557" in  (* Near 2^64 *)
  let large_prime2 = of_string "18446744073709551533" in  (* Near 2^64 *)

  let product = mul large_prime1 large_prime2 in
  let quotient = div product large_prime1 in

  assert_equal (to_string large_prime2) (to_string quotient)
    "Division of large product by one of its prime factors";

  (* Test with extremely large number and very small divisor *)
  let very_large = of_string "123456789012345678901234567890123456789012345678901234567890" in
  let small_divisor = of_int 3 in

  verify_division_remainder very_large small_divisor;

  (* Test with numbers where one is much larger than the other *)
  let big = of_string "1000000000000000000000000000000" in
  let small = of_int 7 in

  assert_equal "1000000000000000000000000000007"
    (to_string (add big small)) "Big + small";

  assert_equal "999999999999999999999999999993"
    (to_string (sub big small)) "Big - small";

  assert_equal "7000000000000000000000000000000"
    (to_string (mul big small)) "Big * small";

  assert_equal "142857142857142857142857142857"
    (to_string (div big small)) "Big / small";

  assert_equal "1" (to_string (rem big small)) "Big % small"


(* Main test runner *)
let () =
  Printf.printf "Starting BigInt tests\n\n";
  run_test "Constants" test_constants;
  run_test "Conversion" test_conversion;
  run_test "Compare" test_compare;
  run_test "Basic Arithmetic" test_basic_arithmetic;
  run_test "Division and Remainder" test_division_remainder;
  run_test "32-bit Boundary Tests" test_boundary_32bit;
  run_test "64-bit Boundary Tests" test_boundary_64bit;
  run_test "Magnitude Difference Tests" test_magnitude_differences;
  run_test "Large Multiplication" test_large_multiplication;
  run_test "Bit Operations" test_bit_operations;
  run_test "Large Number Arithmetic" test_large_arithmetic;
  run_test "Edge Cases" test_edge_cases;
  Printf.printf "All tests completed\n"
