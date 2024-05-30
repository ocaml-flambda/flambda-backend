open Stdlib

let eq l r =
  if l <> r then Printf.printf "%d <> %d\n" l r
;;

let eqf l r =
  let open Float in
  if is_nan l && is_nan r then ()
  else if l <> r then Printf.printf "%f <> %f\n" l r
;;

module Poly = struct
  type extern_flags =
    No_sharing
  | Closures
  | Compat_32
  | Compression

  external compare : 'a -> 'a -> int = "%compare"

  external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash_exn" [@@noalloc]

  external to_bytes: 'a -> extern_flags list -> bytes = "caml_output_value_to_bytes"
  external from_bytes_unsafe: bytes -> int -> 'a = "caml_input_value_from_bytes"

  let hash x = seeded_hash_param 10 100 0 x
end

module Float32 = struct

  external of_int : int -> float32 = "%float32ofint"

  external to_int : float32 -> int  = "%intoffloat32"

  external of_float : float -> float32 = "%float32offloat"

  external to_float : float32 -> float = "%floatoffloat32"

  external neg : float32 -> float32 = "%negfloat32"
  external add : float32 -> float32 -> float32 = "%addfloat32"
  external sub : float32 -> float32 -> float32 = "%subfloat32"
  external mul : float32 -> float32 -> float32 = "%mulfloat32"
  external div : float32 -> float32 -> float32 = "%divfloat32"
  external abs : float32 -> float32 = "%absfloat32"

  external compare : float32 -> float32 -> int = "%compare"
end


module CFloat32 = struct
  type t = float32

  external bits_to_int : (t [@unboxed]) -> (int32 [@unboxed]) = "float32_bits_to_int_boxed" "float32_bits_to_int" [@@noalloc]

  external zero : unit -> (t [@unboxed]) = "float32_zero_boxed" "float32_zero" [@@noalloc]
  external neg_zero : unit -> (t [@unboxed]) = "float32_neg_zero_boxed" "float32_neg_zero" [@@noalloc]
  external one : unit -> (t [@unboxed]) = "float32_one_boxed" "float32_one" [@@noalloc]
  external neg_one : unit -> (t [@unboxed]) = "float32_neg_one_boxed" "float32_neg_one" [@@noalloc]
  external nan : unit -> (t [@unboxed]) = "float32_nan_boxed" "float32_nan" [@@noalloc]
  external nan2 : unit -> (t [@unboxed]) = "float32_nan2_boxed" "float32_nan2" [@@noalloc]
  external neg_infinity : unit -> (t [@unboxed]) = "float32_neg_infinity_boxed" "float32_neg_infinity" [@@noalloc]
  external infinity : unit -> (t [@unboxed]) = "float32_infinity_boxed" "float32_infinity" [@@noalloc]
  external maxv : unit -> (t [@unboxed]) = "float32_maxv_boxed" "float32_maxv" [@@noalloc]
  external minv : unit -> (t [@unboxed]) = "float32_minv_boxed" "float32_minv" [@@noalloc]

  let zero = zero ()
  let neg_zero = neg_zero ()
  let one = one ()
  let nan = nan ()
  let nan2 = nan2 ()
  let neg_infinity = neg_infinity ()
  let infinity = infinity ()
  let neg_one = neg_one ()
  let maxv = maxv ()
  let minv = minv ()

  external eq : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_eq_boxed" "float32_eq" [@@noalloc]
  external neq : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_ne_boxed" "float32_ne" [@@noalloc]
  external lt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_lt_boxed" "float32_lt" [@@noalloc]
  external le : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_le_boxed" "float32_le" [@@noalloc]
  external gt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_gt_boxed" "float32_gt" [@@noalloc]
  external ge : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_ge_boxed" "float32_ge" [@@noalloc]
  external nlt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_nlt_boxed" "float32_nlt" [@@noalloc]
  external nle : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_nle_boxed" "float32_nle" [@@noalloc]
  external ngt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_ngt_boxed" "float32_ngt" [@@noalloc]
  external nge : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_nge_boxed" "float32_nge" [@@noalloc]
  external ord : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_ord_boxed" "float32_ord" [@@noalloc]
  external uord : (t [@unboxed]) -> (t [@unboxed]) -> bool = "float32_uord_boxed" "float32_uord" [@@noalloc]

  external add : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_add_boxed" "float32_add" [@@noalloc]
  external sub : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_sub_boxed" "float32_sub" [@@noalloc]
  external mul : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_mul_boxed" "float32_mul" [@@noalloc]
  external div : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_div_boxed" "float32_div" [@@noalloc]

  external abs : (t [@unboxed]) -> (t [@unboxed]) = "float32_abs_boxed" "float32_abs" [@@noalloc]
  external neg : (t [@unboxed]) -> (t [@unboxed]) = "float32_neg_boxed" "float32_neg" [@@noalloc]

  external is_nan : (t [@unboxed]) -> bool = "float32_is_nan_boxed" "float32_is_nan" [@@noalloc]

  let check_float32s f =
      Random.set_state (Random.State.make [|1234567890|]);
      f zero zero;
      f zero one;
      f one one;
      f zero neg_one;
      f neg_one neg_one;
      f one neg_one;
      f zero neg_zero;
      f neg_zero zero;
      f nan zero;
      f nan2 nan;
      f infinity zero;
      f neg_infinity zero;
      f nan nan;
      f infinity infinity;
      f neg_infinity neg_infinity;
      f neg_infinity infinity;
      f infinity nan;
      f neg_infinity nan;
      f maxv infinity;
      f maxv neg_infinity;
      f minv infinity;
      f minv neg_infinity;
      f maxv maxv;
      f minv minv;
      f maxv minv;
      for i = 0 to 100_000 do
          let f0 = Random.int32 Int32.max_int in
          let f1 = Random.int32 Int32.max_int in
          f ((if Random.bool () then f0 else Int32.neg f0) |> Int32.float_of_bits |> Beta.Float32.of_float)
            ((if Random.bool () then f1 else Int32.neg f1) |> Int32.float_of_bits |> Beta.Float32.of_float)
      done
  ;;
end

module Float64 = struct
  let check_floats f =
    let open Float in
    Random.set_state (Random.State.make [|1234567890|]);
    f zero;
    f minus_one;
    f one;
    f (-0.0);
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
end

module Int = struct
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
end

let () = (* Casts *)
  Float64.check_floats (fun f ->
    let via_f32 = Float32.to_float (Float32.of_float f) in
    let via_int32 = Int32.float_of_bits (Int32.bits_of_float f) in
    eqf via_f32 via_int32;

    let via_f32 = Float32.to_int (Float32.of_float f) in
    let via_int32 = Float.to_int via_int32 in
    eq via_f32 via_int32
  );
  Int.check_ints (fun i ->
    let via_f32 = Float32.to_float (Float32.of_int i) in
    let via_f64 = Int32.float_of_bits (Int32.bits_of_float (Float.of_int i)) in
    eqf via_f32 via_f64
  )
;;

let () = (* Arithmetic *)
  let assert_same a b =
    assert (a = b || (CFloat32.is_nan a && CFloat32.is_nan b))
  in
  CFloat32.check_float32s (fun f1 f2 ->
    assert_same (CFloat32.add f1 f2) (Float32.add f1 f2);
    assert_same (CFloat32.sub f1 f2) (Float32.sub f1 f2);
    assert_same (CFloat32.mul f1 f2) (Float32.mul f1 f2);
    assert_same (CFloat32.div f1 f2) (Float32.div f1 f2);
    assert_same (CFloat32.abs f1) (Float32.abs f1);
    assert_same (CFloat32.neg f1) (Float32.neg f1)
  )
;;

let () = (* Compare *)
  let run compare =
    CFloat32.check_float32s (fun f1 f2 ->
      let no_nan = (not (CFloat32.is_nan f1)) && (not (CFloat32.is_nan f2)) in
      if no_nan then (
        (match Float32.compare f1 f2 with
        | -1 -> assert (CFloat32.lt f1 f2)
        | 0 -> assert (CFloat32.eq f1 f2)
        | 1 -> assert (CFloat32.gt f1 f2)
        | _ -> assert false);
        let f2, f1 = f1, f2 in
        (match Float32.compare f1 f2 with
        | -1 -> assert (CFloat32.lt f1 f2)
        | 0 -> assert (CFloat32.eq f1 f2)
        | 1 -> assert (CFloat32.gt f1 f2)
        | _ -> assert false);
      ) else (
        assert (CFloat32.neq f1 f2);
        assert (CFloat32.neq f2 f1);
        assert (CFloat32.nlt f1 f2);
        assert (CFloat32.nle f1 f2);
        assert (CFloat32.ngt f1 f2);
        assert (CFloat32.nge f1 f2);
        if CFloat32.is_nan f1 && CFloat32.is_nan f2 then (
          assert (Float32.compare f1 f2 = 0)
        );
        if CFloat32.is_nan f1 && not (CFloat32.is_nan f2) then (
          assert (Float32.compare f1 f2 = -1);
          assert (Float32.compare f2 f1 = 1)
        );
        let f2, f1 = f1, f2 in
        if CFloat32.is_nan f1 && not (CFloat32.is_nan f2) then (
          assert (Float32.compare f1 f2 = -1);
          assert (Float32.compare f2 f1 = 1)
        )
      )
    )
  in
  (run [@inlined]) Float32.compare;
  (run [@inlined never]) (fun l r -> Poly.compare (Sys.opaque_identity l) (Sys.opaque_identity r))
;;

let () = (* Hash *)
  CFloat32.check_float32s (fun f _ ->
      let h = Poly.hash f in
      let bits =
        if f = Float32.of_float 0.0 then 0l
        else if CFloat32.is_nan f then 0x7F800001l
        else CFloat32.bits_to_int f
      in
      assert (h = Poly.hash bits)
  )
;;

let () = (* Marshal *)
  CFloat32.check_float32s (fun l r ->
    let b = Poly.to_bytes (Sys.opaque_identity l, Sys.opaque_identity r) [] in
    let (readl, readr) : float32 * float32 = Poly.from_bytes_unsafe b 0 in
    assert (CFloat32.bits_to_int l = CFloat32.bits_to_int readl);
    assert (CFloat32.bits_to_int r = CFloat32.bits_to_int readr)
  )
;;

let () = (* Literals *)
  let check s f =
    (* Not true if the literal was just below a float64 value exactly halfway
       between two float32s *)
    assert (s = Float32.of_float f)
  in
  check 0.0s 0.0;
  check 1.0s 1.0;
  check 0.5s 0.5;
  check 1234.1234s 1234.1234;
  check 0.s 0.;
  check 1e10s 1e10;
  check 1e-9_8s 1e-9_8;
  check 1e+1s 1e+1;
  check 1.12345e+12s 1.12345e+12;
  check 0x2_2p+0s 0x22p+0;
  check 0x2p+0s 0x2p+0;
  check 0x3p+0s 0x3p+0;
  check 0x5p+0s 0x5p+0;
  check 0x1.4p+0s 0x1.4p+0;
  check 0xcp-4s 0xcp-4;
  check 0x1p-4s 0x1p-4;
  check 0x1p+0s 0x1p+0;
  check 0x0p+0s 0x0p+0;
  check 0xf.f___ffffp+124s 0xf.fffffp+124;
  check 0xf.ffffffffffff8p+1020s 0xf.ffffffffffff8p+1020;
  check 0x4p-128s 0x4p-128;
  check 0x1p-252s 0x1p-252;
  check 0x4p-1024s 0x4p-1024;
  check 0x8p-972s 0x8p-972;
  check 0xf.fff_f_e_000001p+252s 0xf.ff_ffe_00_0001p+252;
  check 0x2.fffp+12s 0x2.fffp+12;
  check 0x1.000002p+0s 0x1.000002p+0;
  check 0x1.ffffp-24s 0x1.ffffp-24;
  check 0x2._fff006p+12s 0x2._fff006p+12;
  check 0x1.fffp+0s 0x1.fffp+0;
  check 0x1.00001p+0s 0x1.00001p+0;
  check 0xc.d5e6fp+1_24s 0xc.d5e6fp+1_24;
  check 0x2.6af378p-128s 0x2.6af378p-128;
  check 0x5p-128s 0x5p-128;
  check 0x1____p-128s 0x1p-128;
  check 0x8p-152s 0x8p-152;
  check 0x8p-4s 0x8p-4;
  check 0x8p+124s 0x8p+124;
  ()
;;
