(* TEST
 include stdlib_beta;
 include stdlib_upstream_compatible;
 modules = "test_repr.c";
 flambda2;
 {
   flags = "-extension-universe beta";
   native;
 } {
   flags = "-O3 -extension-universe beta";
   native;
 } {
   flags = "-Oclassic -extension-universe beta";
   native;
 } {
   flags = "-extension-universe beta";
   bytecode;
 }
*)
let min_int = -0x80

let max_int = 0x7f

module Smallint = Stdlib_beta.Int8_u
module Int = Stdlib_beta.Int_wrapper

let same_float x y = Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)

let same_int x y =
  Int.equal (Smallint.to_int x) y && Smallint.equal x (Smallint.of_int y)

(** generates a random float that rounds toward zero to the same integer value *)
let nudge rng f =
  let f_pos = Float.abs f in
  if not (Float.is_finite f)
  then f
  else if f_pos < 1.
  then Random.State.float rng (Float.pred 1.0)
  else
    let lo = Float.floor f_pos in
    let hi = Float.pred (lo +. 1.) in
    if not (lo < hi)
    then f
    else
      (* the mantissa is the low bits, and we are only generating normal
         fractional values so we never need to change the exponent *)
      let lo = Int64.bits_of_float lo in
      let hi = Int64.bits_of_float hi in
      assert (Int64.shift_right lo 52 = Int64.shift_right hi 52);
      Float.copy_sign
        (Int64.float_of_bits (Random.State.int64_in_range rng ~min:lo ~max:hi))
        f

let () =
  let int_size = Smallint.size in
  assert (0 < int_size && int_size <= Sys.int_size);
  assert (max_int = (1 lsl (int_size - 1)) - 1);
  assert (min_int = lnot max_int);
  let mask = (1 lsl int_size) - 1 in
  let to_int x : int =
    let i : int = Smallint.to_int x in
    assert (same_int x i);
    assert (min_int <= i && i <= max_int);
    i
  in
  let of_int (i : int) : Smallint.t =
    let x = Smallint.of_int i in
    if not (to_int x land mask = i land mask)
    then
      failwith
        (Printf.sprintf "%x (%x) <> %x (%x)"
           (to_int x land mask)
           (to_int x) (i land mask) i);
    x
  in
  let rng = Random.State.make [| int_size |] in
  let test_cases =
    (* sparse test cases, concentrated around 0 and the endpoints *)
    List.init (int_size - 1) (fun size ->
        let bit = 1 lsl size in
        let rand () =
          Random.State.int_in_range rng ~min:(bit lsr 1) ~max:(bit - 1)
        in
        [rand (); lnot (rand ()); max_int - rand (); lnot (max_int - rand ())])
    |> List.concat |> List.sort Int.compare
  in
  let special_floats = Float.[infinity; nan; neg_infinity; epsilon; -0.; 0.] in
  let test1 f = ListLabels.iter test_cases ~f in
  let test2 f = test1 (fun x -> test1 (fun y -> f x y)) in
  let test_round_trip () =
    let test hi lo =
      let hi = hi lsl int_size in
      assert (lo = to_int (of_int (hi lxor lo)))
    in
    test2 (fun hi lo ->
        (* generate test cases with different hi bits *)
        test hi lo;
        test (Random.bits ()) lo)
  in
  let equal_arith (x : Smallint.t) i = Smallint.equal (of_int i) x in
  let equal_logical x i = to_int x == i in
  let assert_equal (equal : Smallint.t -> int -> bool) x y =
    match x () with
    | x -> assert (equal x (y ()))
    | exception exn -> (
      match y () with _ -> raise exn | exception exn' -> assert (exn = exn'))
  in
  let test_conv1 (int16_f : Smallint.t -> Smallint.t) int_f ~equal =
    test1 (fun x ->
        assert_equal equal (fun () -> int16_f (of_int x)) (fun () -> int_f x))
  in
  let test_conv2 ?(unsigned = false)
      (int16_f : Smallint.t -> Smallint.t -> Smallint.t) int_f ~equal =
    test2 (fun x y ->
        assert_equal equal
          (fun () -> int16_f (of_int x) (of_int y))
          (fun () ->
            if unsigned then int_f (x land mask) (y land mask) else int_f x y))
  in
  let test_arith1 = test_conv1 ~equal:equal_arith in
  let test_arith2 ?__LINE__ = test_conv2 ~equal:equal_arith in
  let test_logical1 = test_conv1 ~equal:equal_logical in
  let test_logical2 = test_conv2 ~equal:equal_logical in
  let reference_shift_right_logical x i =
    (* we need to ensure that we shift in zero bytes, which is incompatible with
       sign-extension *)
    Int.shift_right_logical (if i > 0 then x land mask else x) i
  in
  test_round_trip ();
  assert (same_int (Smallint.zero ()) Int.zero);
  assert (same_int (Smallint.one ()) Int.one);
  assert (same_int (Smallint.minus_one ()) Int.minus_one);
  assert (same_int (Smallint.max_int ()) max_int);
  assert (same_int (Smallint.min_int ()) min_int);
  assert (Smallint.equal Smallint.(abs (min_int ())) (Smallint.min_int ()));
  assert (
    Smallint.equal (Smallint.succ (Smallint.max_int ())) (Smallint.min_int ()));
  assert (
    Smallint.equal (Smallint.pred (Smallint.min_int ())) (Smallint.max_int ()));
  test_arith2 Smallint.add Int.add;
  test_arith2 Smallint.sub Int.sub;
  test_arith2 Smallint.mul Int.mul;
  test_arith2 Smallint.div Int.div;
  (* test_arith2 Smallint.unsigned_div Int.unsigned_div ~unsigned:true;
   * test_arith2 Smallint.unsigned_rem Int.unsigned_rem ~unsigned:true; *)
  test_arith2 Smallint.rem Int.rem;
  test_arith1 Smallint.succ Int.succ;
  test_arith1 Smallint.pred Int.pred;
  test_arith1 Smallint.abs Int.abs;
  test_arith1 Smallint.neg Int.neg;
  test_logical2 Smallint.logand Int.logand;
  test_logical2 Smallint.logor Int.logor;
  test_logical2 Smallint.logxor Int.logxor;
  test_logical1 Smallint.lognot Int.lognot;
  for shift = 0 to int_size - 1 do
    let apply_shift f x = f x shift in
    let apply_shift' f (x : Smallint.t) = f x shift in
    test_logical1
      (apply_shift' Smallint.shift_right)
      (apply_shift Int.shift_right);
    test_logical1
      (apply_shift' Smallint.shift_right_logical)
      (apply_shift reference_shift_right_logical);
    test_conv1
      (apply_shift' Smallint.shift_left)
      (apply_shift Int.shift_left)
      ~equal:(if shift = 0 then equal_logical else equal_arith)
  done;
  test2 (fun x y ->
      assert (Smallint.equal (of_int x) (of_int y) = Int.equal x y));
  test2 (fun x y ->
      assert (Smallint.compare (of_int x) (of_int y) = Int.compare x y));
  test1 (fun x ->
      assert (same_float (Smallint.to_float (of_int x)) (Int.to_float x)));
  ListLabels.iter
    (special_floats @ List.map Int.to_float test_cases)
    ~f:(fun f -> assert (equal_arith (Smallint.of_float f) (Int.of_float f)));
  test1 (fun x ->
      (* test that fractional values round toward zero *)
      let f = nudge rng (Int.to_float x) in
      assert (equal_logical (Smallint.of_float f) x));
  test1 (fun x -> assert (Smallint.to_string (of_int x) = Int.to_string x));
  test_logical2 Smallint.min Int.min;
  test_logical2 Smallint.max Int.max;
  ()

(* test that the value is stored sign-extended in the register *)
external get_register : Smallint.t -> nativeint = "get_register_bytecode" "get_register"
let get_register x = get_register (Smallint.of_int x)


let () =
  assert (get_register ((1 lsl Smallint.size) - 1) = -1n);
  ()
