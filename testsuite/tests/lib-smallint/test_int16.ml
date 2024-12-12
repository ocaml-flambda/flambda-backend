(* TEST include stdlib_beta; flags = "-extension small_numbers_beta"; *)

module Int16 = Stdlib_beta.Int16

let int_size = Int16.int_size

let () = assert (0 < int_size && int_size < Sys.int_size)

let max_int = (1 lsl (int_size - 1)) - 1

let min_int = lnot max_int

let mask = (1 lsl int_size) - 1

let to_int x : int =
  let i : int = Int16.to_int x in
  assert (Obj.repr i == Obj.repr x);
  assert (min_int <= i && i <= max_int);
  i

let of_int (i : int) =
  let x = Int16.of_int i in
  assert (to_int x land mask == i land mask);
  x

let rng = Random.State.make [| int_size |]

(** sparse test cases, concentrated around 0 and the endpoints *)
let test_cases =
  let is_even = 1 - (int_size land 1) in
  List.init (int_size - is_even) (fun size ->
      let bit = 1 lsl size in
      let rand () =
        Random.State.int_in_range rng ~min:(bit lsr 1) ~max:(bit - 1)
      in
      [rand (); lnot (rand ()); max_int - rand (); lnot (max_int - rand ())])
  |> List.concat |> List.sort Int.compare

let test1 f = ListLabels.iter test_cases ~f

let test2 f = test1 (fun x -> test1 (fun y -> f x y))

let test_round_trip () =
  let test hi lo =
    let hi = hi lsl int_size in
    assert (lo == to_int (of_int (hi lxor lo)))
  in
  test2 (fun hi lo ->
      (* generate test cases with different hi bits *)
      test hi lo;
      test (Random.bits ()) lo)

let equal_arith x i = x == of_int i

let equal_logical x i = to_int x == i

let same_float x y = Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)

let assert_equal equal x y =
  let x = try Ok (x ()) with exn -> Error exn in
  let y = try Ok (y ()) with exn -> Error exn in
  match x, y with
  | Ok x, Ok y -> assert (equal x y)
  | Error exn, Error exn' -> assert (exn = exn')
  | Ok _, Error exn | Error exn, Ok _ -> raise exn

let test_conv1 int16_f int_f ~equal =
  test1 (fun x ->
      assert_equal equal (fun () -> int16_f (of_int x)) (fun () -> int_f x))

let test_conv2 int16_f int_f ~equal =
  test2 (fun x y ->
      assert_equal equal
        (fun () -> int16_f (of_int x) (of_int y))
        (fun () -> int_f x y))

let test_arith1 = test_conv1 ~equal:equal_arith

let test_arith2 = test_conv2 ~equal:equal_arith

let test_logical1 = test_conv1 ~equal:equal_logical

let test_logical2 = test_conv2 ~equal:equal_logical

let reference_shift_right_logical x i =
  (* we need to ensure that we shift in zero bytes, which is incompatible with
     sign-extension *)
  Int.shift_right_logical (if i > 0 then x land mask else x) i

let () =
  test_round_trip ();
  assert (to_int Int16.zero == Int.zero);
  assert (to_int Int16.one == Int.one);
  assert (to_int Int16.minus_one == Int.minus_one);
  test_arith2 Int16.add Int.add;
  test_arith2 Int16.sub Int.sub;
  test_arith2 Int16.mul Int.mul;
  test_arith2 Int16.div Int.div;
  test_arith2 Int16.rem Int.rem;
  test_arith1 Int16.succ Int.succ;
  test_arith1 Int16.pred Int.pred;
  test_arith1 Int16.abs Int.abs;
  test_logical2 Int16.logand Int.logand;
  test_logical2 Int16.logor Int.logor;
  test_logical2 Int16.logxor Int.logxor;
  test_logical1 Int16.lognot Int.lognot;
  for shift = 0 to int_size do
    let apply_shift f x = f x shift in
    test_logical1 (apply_shift Int16.shift_right) (apply_shift Int.shift_right);
    test_conv1
      (apply_shift Int16.shift_right_logical)
      (apply_shift reference_shift_right_logical)
      ~equal:equal_logical;
    test_conv1
      (apply_shift Int16.shift_left)
      (apply_shift Int.shift_left)
      ~equal:(if shift = 0 then equal_logical else equal_arith)
  done;
  test_conv2 Int16.equal Int.equal ~equal:Bool.equal;
  test_conv2 Int16.compare Int.compare ~equal:Int.equal;
  test_conv1 Int16.to_float Int.to_float ~equal:same_float;
  assert (Int16.of_float (-0.) = Int16.zero);
  test1 (fun x ->
      let f = Int.to_float x in
      assert (equal_logical (Int16.of_float f) x));
  test1 (fun x ->
      (* test fractional values round toward zero *)
      let f = Int.to_float x in
      let f' =
        let almost_one = Random.State.float rng (Float.pred 1.0) in
        if x < 0
        then f -. almost_one
        else if x > 0
        then f +. almost_one
        else if Random.State.bool rng
        then almost_one
        else -.almost_one
      in
      assert (equal_logical (Int16.of_float f') x));
  test1 (fun x -> assert (Int16.to_string (of_int x) = Int.to_string x));
  test_logical2 Int16.min Int.min;
  test_logical2 Int16.max Int.max;
  ()
