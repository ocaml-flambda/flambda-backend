open Stdlib

module Float32 = struct

  external of_int : int -> float32 = "%float32ofint"

  external to_int : float32 -> int  = "%intoffloat32"

  external of_float : float -> float32 = "%float32offloat"

  external to_float : float32 -> float = "%floatoffloat32"

end

let eq l r =
  if l <> r then Printf.printf "%d <> %d\n" l r
;;

let eqf l r =
  let open Float in
  if is_nan l && is_nan r then ()
  else if l <> r then Printf.printf "%f <> %f\n" l r
;;

let check_floats f =
  let open Float in
  Random.set_state (Random.State.make [|1234567890|]);
  f zero;
  f one;
  f minus_one;
  f one;
  f (-0.0);
  f nan;
  f infinity;
  f neg_infinity;
  f nan;
  f max_float;
  f min_float;
  for i = 0 to 100_000 do
      let v = Random.int64 Int64.max_int in
      f ((if Random.bool () then v else Int64.neg v) |> Int64.float_of_bits)
  done
;;

let check_ints f =
  let open Int in
  Random.set_state (Random.State.make [|1234567890|]);
  f zero;
  f one;
  f minus_one;
  f max_int;
  f min_int;
  for i = 0 to 100_000 do
      let i = Random.int64 Int64.max_int |> Int64.to_int in
      f (if Random.bool () then i else Int.neg i)
  done
;;

let () =
  check_floats (fun f ->
    let via_f32 = Float32.to_float (Float32.of_float f) in
    let via_int32 = Int32.float_of_bits (Int32.bits_of_float f) in
    eqf via_f32 via_int32;

    let via_f32 = Float32.to_int (Float32.of_float f) in
    let via_int32 = Float.to_int via_int32 in
    eq via_f32 via_int32
  );
  check_ints (fun i ->
    let via_f32 = Float32.to_float (Float32.of_int i) in
    let via_f64 = Int32.float_of_bits (Int32.bits_of_float (Float.of_int i)) in
    eqf via_f32 via_f64
  )
;;
