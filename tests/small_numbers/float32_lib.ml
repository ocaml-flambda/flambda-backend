
[@@@ocaml.warning "-unused-value-declaration"]
[@@@ocaml.warning "-unused-module"]

(* Tests for the float32 otherlib  *)

module F32 = Stdlib_stable.Float32

module CF32 = struct
  type t = float32

  external to_bits : (t [@unboxed]) -> (int32 [@unboxed]) = "float32_bits_to_int_boxed" "float32_bits_to_int" [@@noalloc]

  external of_int : (int [@untagged]) -> (t [@unboxed]) = "float32_of_int_boxed" "float32_of_int" [@@noalloc]
  external of_int64 : (int64 [@unboxed]) -> (t [@unboxed]) = "float32_of_int64_boxed" "float32_of_int64" [@@noalloc]
  external of_float : (float [@unboxed]) -> (t [@unboxed]) = "float32_of_float_boxed" "float32_of_float" [@@noalloc]

  external to_int : (t [@unboxed]) -> (int [@untagged]) = "float32_to_int_boxed" "float32_to_int" [@@noalloc]
  external to_int64 : (t [@unboxed]) -> (int64 [@unboxed]) = "float32_to_int64_boxed" "float32_to_int64" [@@noalloc]
  external to_float : (t [@unboxed]) -> (float [@unboxed]) = "float32_to_float_boxed" "float32_to_float" [@@noalloc]

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
  external epsilon : unit -> (t [@unboxed]) = "float32_epsilon_boxed" "float32_epsilon" [@@noalloc]
  external pi : unit -> (t [@unboxed]) = "float32_pi_boxed" "float32_pi" [@@noalloc]

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
  let epsilon = epsilon ()
  let pi = pi ()

  external abs : (t [@unboxed]) -> (t [@unboxed]) = "float32_abs_boxed" "float32_abs" [@@noalloc]
  external neg : (t [@unboxed]) -> (t [@unboxed]) = "float32_neg_boxed" "float32_neg" [@@noalloc]
  external sqrt : (t [@unboxed]) -> (t [@unboxed]) = "float32_sqrt_boxed" "float32_sqrt" [@@noalloc]
  external cbrt : (t [@unboxed]) -> (t [@unboxed]) = "float32_cbrt_boxed" "float32_cbrt" [@@noalloc]
  external exp : (t [@unboxed]) -> (t [@unboxed]) = "float32_exp_boxed" "float32_exp" [@@noalloc]
  external exp2 : (t [@unboxed]) -> (t [@unboxed]) = "float32_exp2_boxed" "float32_exp2" [@@noalloc]
  external log : (t [@unboxed]) -> (t [@unboxed]) = "float32_log_boxed" "float32_log" [@@noalloc]
  external log10 : (t [@unboxed]) -> (t [@unboxed]) = "float32_log10_boxed" "float32_log10" [@@noalloc]
  external log2 : (t [@unboxed]) -> (t [@unboxed]) = "float32_log2_boxed" "float32_log2" [@@noalloc]
  external expm1 : (t [@unboxed]) -> (t [@unboxed]) = "float32_expm1_boxed" "float32_expm1" [@@noalloc]
  external log1p : (t [@unboxed]) -> (t [@unboxed]) = "float32_log1p_boxed" "float32_log1p" [@@noalloc]
  external cos : (t [@unboxed]) -> (t [@unboxed]) = "float32_cos_boxed" "float32_cos" [@@noalloc]
  external sin : (t [@unboxed]) -> (t [@unboxed]) = "float32_sin_boxed" "float32_sin" [@@noalloc]
  external tan : (t [@unboxed]) -> (t [@unboxed]) = "float32_tan_boxed" "float32_tan" [@@noalloc]
  external acos : (t [@unboxed]) -> (t [@unboxed]) = "float32_acos_boxed" "float32_acos" [@@noalloc]
  external asin : (t [@unboxed]) -> (t [@unboxed]) = "float32_asin_boxed" "float32_asin" [@@noalloc]
  external atan : (t [@unboxed]) -> (t [@unboxed]) = "float32_atan_boxed" "float32_atan" [@@noalloc]
  external cosh : (t [@unboxed]) -> (t [@unboxed]) = "float32_cosh_boxed" "float32_cosh" [@@noalloc]
  external sinh : (t [@unboxed]) -> (t [@unboxed]) = "float32_sinh_boxed" "float32_sinh" [@@noalloc]
  external tanh : (t [@unboxed]) -> (t [@unboxed]) = "float32_tanh_boxed" "float32_tanh" [@@noalloc]
  external acosh : (t [@unboxed]) -> (t [@unboxed]) = "float32_acosh_boxed" "float32_acosh" [@@noalloc]
  external asinh : (t [@unboxed]) -> (t [@unboxed]) = "float32_asinh_boxed" "float32_asinh" [@@noalloc]
  external atanh : (t [@unboxed]) -> (t [@unboxed]) = "float32_atanh_boxed" "float32_atanh" [@@noalloc]
  external erf : (t [@unboxed]) -> (t [@unboxed]) = "float32_erf_boxed" "float32_erf" [@@noalloc]
  external erfc : (t [@unboxed]) -> (t [@unboxed]) = "float32_erfc_boxed" "float32_erfc" [@@noalloc]
  external trunc : (t [@unboxed]) -> (t [@unboxed]) = "float32_trunc_boxed" "float32_trunc" [@@noalloc]
  external round : (t [@unboxed]) -> (t [@unboxed]) = "float32_round_boxed" "float32_round" [@@noalloc]
  external ceil : (t [@unboxed]) -> (t [@unboxed]) = "float32_ceil_boxed" "float32_ceil" [@@noalloc]
  external floor : (t [@unboxed]) -> (t [@unboxed]) = "float32_floor_boxed" "float32_floor" [@@noalloc]

  external sign_bit : (t [@unboxed]) -> bool = "float32_sign_bit_boxed" "float32_sign_bit" [@@noalloc]
  external classify_float : (t [@unboxed]) -> Stdlib.fpclass = "float32_classify_boxed" "float32_classify" [@@noalloc]
  external succ : (t [@unboxed]) -> (t [@unboxed]) = "float32_succ_boxed" "float32_succ" [@@noalloc]
  external pred : (t [@unboxed]) -> (t [@unboxed]) = "float32_pred_boxed" "float32_pred" [@@noalloc]
  external is_finite : (t [@unboxed]) -> bool = "float32_is_finite_boxed" "float32_is_finite" [@@noalloc]
  external is_infinite : (t [@unboxed]) -> bool = "float32_is_infinite_boxed" "float32_is_infinite" [@@noalloc]
  external is_nan : (t [@unboxed]) -> bool = "float32_is_nan_boxed" "float32_is_nan" [@@noalloc]
  external is_integer : (t [@unboxed]) -> bool = "float32_is_integer_boxed" "float32_is_integer" [@@noalloc]

  external add : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_add_boxed" "float32_add" [@@noalloc]
  external sub : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_sub_boxed" "float32_sub" [@@noalloc]
  external mul : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_mul_boxed" "float32_mul" [@@noalloc]
  external div : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_div_boxed" "float32_div" [@@noalloc]
  external rem : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_rem_boxed" "float32_rem" [@@noalloc]
  external pow : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_pow_boxed" "float32_pow" [@@noalloc]
  external atan2 : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_atan2_boxed" "float32_atan2" [@@noalloc]
  external hypot : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_hypot_boxed" "float32_hypot" [@@noalloc]
  external next_after : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_next_after_boxed" "float32_next_after" [@@noalloc]
  external copy_sign : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_copy_sign_boxed" "float32_copy_sign" [@@noalloc]

  external fma : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "float32_fma_boxed" "float32_fma" [@@noalloc]

  external frexp : t -> t * int = "float32_frexp_boxed"
  external ldexp : t -> int -> t = "float32_ldexp_boxed"
  external modf : t -> t * t = "float32_modf_boxed"

  external min : t -> t -> t = "float32_min_boxed"
  external max : t -> t -> t = "float32_max_boxed"
  external min_weird : t -> t -> t = "float32_min_weird_boxed"
  external max_weird : t -> t -> t = "float32_max_weird_boxed"
  external min_num : t -> t -> t = "float32_min_num_boxed"
  external max_num : t -> t -> t = "float32_max_num_boxed"
  external min_max : t -> t -> t * t = "float32_min_max_boxed"
  external min_max_num : t -> t -> t * t = "float32_min_max_num_boxed"

  external round_current : t -> t = "float32_round_current_boxed"
  external iround_current : t -> int64 = "float32_iround_current_boxed"

  external compare : t -> t -> int = "float32_compare_boxed" [@@noalloc]
  let equal x y = compare x y = 0

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
      for _ = 0 to 100_000 do
          let f0 = Random.int32 Int32.max_int in
          let f1 = Random.int32 Int32.max_int in
          f ((if Random.bool () then f0 else Int32.neg f0) |> Int32.float_of_bits |> F32.of_float)
            ((if Random.bool () then f1 else Int32.neg f1) |> Int32.float_of_bits |> F32.of_float)
      done
  ;;
end

let bit_eq f1 f2 =
  assert (CF32.to_bits f1 = CF32.to_bits f2 || (CF32.is_nan f1 && CF32.is_nan f2))

let () =
  bit_eq F32.zero CF32.zero;
  bit_eq F32.one CF32.one;
  bit_eq F32.minus_one CF32.neg_one;
  bit_eq F32.infinity CF32.infinity;
  bit_eq F32.neg_infinity CF32.neg_infinity;
  bit_eq F32.nan CF32.nan;
  bit_eq F32.pi CF32.pi;
  bit_eq F32.min_float CF32.minv;
  bit_eq F32.max_float CF32.maxv;
  bit_eq F32.epsilon CF32.epsilon
;;

let () =
  CF32.check_float32s (fun f _ ->
    bit_eq (F32.neg f) (CF32.neg f);
    bit_eq (F32.abs f) (CF32.abs f);
    bit_eq (F32.Operators.( ~-. ) f) (CF32.neg f);
    bit_eq (F32.succ f) (CF32.succ f);
    bit_eq (F32.pred f) (CF32.pred f);
    bit_eq (F32.sqrt f) (CF32.sqrt f);
    bit_eq (F32.cbrt f) (CF32.cbrt f);
    bit_eq (F32.exp f) (CF32.exp f);
    bit_eq (F32.exp2 f) (CF32.exp2 f);
    bit_eq (F32.log f) (CF32.log f);
    bit_eq (F32.log10 f) (CF32.log10 f);
    bit_eq (F32.log2 f) (CF32.log2 f);
    bit_eq (F32.expm1 f) (CF32.expm1 f);
    bit_eq (F32.log1p f) (CF32.log1p f);
    bit_eq (F32.cos f) (CF32.cos f);
    bit_eq (F32.sin f) (CF32.sin f);
    bit_eq (F32.tan f) (CF32.tan f);
    bit_eq (F32.acos f) (CF32.acos f);
    bit_eq (F32.asin f) (CF32.asin f);
    bit_eq (F32.atan f) (CF32.atan f);
    bit_eq (F32.cosh f) (CF32.cosh f);
    bit_eq (F32.sinh f) (CF32.sinh f);
    bit_eq (F32.tanh f) (CF32.tanh f);
    bit_eq (F32.acosh f) (CF32.acosh f);
    bit_eq (F32.asinh f) (CF32.asinh f);
    bit_eq (F32.atanh f) (CF32.atanh f);
    bit_eq (F32.erf f) (CF32.erf f);
    bit_eq (F32.erfc f) (CF32.erfc f);
    bit_eq (F32.trunc f) (CF32.trunc f);
    bit_eq (F32.round f) (CF32.round f);
    bit_eq (F32.ceil f) (CF32.ceil f);
    bit_eq (F32.floor f) (CF32.floor f);
    assert ((F32.is_finite f) = (CF32.is_finite f));
    assert ((F32.is_infinite f) = (CF32.is_infinite f));
    assert ((F32.is_nan f) = (CF32.is_nan f));
    assert ((F32.is_integer f) = (CF32.is_integer f));
    assert ((F32.classify_float f) = (CF32.classify_float f));
    assert ((F32.sign_bit f) = (CF32.sign_bit f));
  )
;;

let () =
  (* In glibc 2.25+, powf(nan, zero) returns one if the nan is non-signaling. *)
  bit_eq (F32.pow F32.nan F32.zero) F32.one;
  bit_eq (F32.pow F32.quiet_nan F32.zero) F32.one;
;;

let () =
  CF32.check_float32s (fun f1 f2 ->
    bit_eq (F32.add f1 f2) (CF32.add f1 f2);
    bit_eq (F32.sub f1 f2) (CF32.sub f1 f2);
    bit_eq (F32.mul f1 f2) (CF32.mul f1 f2);
    bit_eq (F32.div f1 f2) (CF32.div f1 f2);
    bit_eq (F32.pow f1 f2) (CF32.pow f1 f2);
    bit_eq (F32.Operators.( +. ) f1 f2) (CF32.add f1 f2);
    bit_eq (F32.Operators.( -. ) f1 f2) (CF32.sub f1 f2);
    bit_eq (F32.Operators.( *. ) f1 f2) (CF32.mul f1 f2);
    bit_eq (F32.Operators.( /. ) f1 f2) (CF32.div f1 f2);
    bit_eq (F32.Operators.( ** ) f1 f2) (CF32.pow f1 f2);
    bit_eq (F32.rem f1 f2) (CF32.rem f1 f2);
    bit_eq (F32.atan2 f1 f2) (CF32.atan2 f1 f2);
    bit_eq (F32.hypot f1 f2) (CF32.hypot f1 f2);
    bit_eq (F32.next_after f1 f2) (CF32.next_after f1 f2);
    bit_eq (F32.copy_sign f1 f2) (CF32.copy_sign f1 f2);
    bit_eq (F32.min f1 f2) (CF32.min f1 f2);
    bit_eq (F32.max f1 f2) (CF32.max f1 f2);
    bit_eq (F32.With_weird_nan_behavior.min f1 f2) (CF32.min_weird f1 f2);
    bit_eq (F32.With_weird_nan_behavior.max f1 f2) (CF32.max_weird f1 f2);
    bit_eq (F32.min_num f1 f2) (CF32.min_num f1 f2);
    bit_eq (F32.max_num f1 f2) (CF32.max_num f1 f2);
    assert((F32.compare f1 f2) = (CF32.compare f1 f2));
    assert((F32.equal f1 f2) = (CF32.equal f1 f2));
  )
;;

let () =
  CF32.check_float32s (fun f _ ->
    bit_eq (F32.round_up f) (CF32.ceil f);
    bit_eq (F32.round_down f) (CF32.floor f);
    bit_eq (F32.round_half_to_even f) (CF32.round_current f);
    (* Returns int64, so can compare directly. *)
    assert ((F32.iround_half_to_even f) = (CF32.iround_current f));
  )
;;

let () =
  CF32.check_float32s (fun f _ ->
    let m, i = F32.frexp f in
    let cm, ci = CF32.frexp f in
    bit_eq m cm;
    assert (i = ci)
  );
  let m, i = F32.frexp 0.0s in
  assert (m = 0.0s && i = 0);
  let m, i = F32.frexp 1.0s in
  assert (m = 0.5s && i = 1);
  let m, i = F32.frexp (-1.0s) in
  assert (m = (-0.5s) && i = 1);
  let m, i = F32.frexp 0.5s in
  assert (m = 0.5s && i = 0);
  let m, i = F32.frexp (-0.5s) in
  assert (m = (-0.5s) && i = 0);
  let m, i = F32.frexp (1024.0s) in
  assert (m = (0.5s) && i = 11);
;;

let () =
  CF32.check_float32s (fun f _ ->
    let f1, f2 = F32.modf f in
    let cf1, cf2 = CF32.modf f in
    bit_eq f1 cf1;
    bit_eq f2 cf2
  );
  let f1, f2 = F32.modf 0.0s in
  assert (f1 = 0.0s && f2 = 0.0s);
  let f1, f2 = F32.modf 1.0s in
  assert (f1 = 0.0s && f2 = 1.0s);
  let f1, f2 = F32.modf (-1.0s) in
  assert (f1 = 0.0s && f2 = (-1.0s));
  let f1, f2 = F32.modf 0.5s in
  assert (f1 = 0.5s && f2 = 0.0s);
  let f1, f2 = F32.modf (-0.5s) in
  assert (f1 = (-0.5s) && f2 = 0.0s);
  let f1, f2 = F32.modf (1024.0s) in
  assert (f1 = 0.0s && f2 = 1024.0s)
;;

let () =
  CF32.check_float32s (fun f1 f2 ->
    let cf1, cf2 = CF32.min_max f1 f2 in
    let f1, f2 = F32.min_max f1 f2 in
    bit_eq f1 cf1;
    bit_eq f2 cf2
  )
;;

let () =
  CF32.check_float32s (fun f1 f2 ->
    let cf1, cf2 = CF32.min_max_num f1 f2 in
    let f1, f2 = F32.min_max_num f1 f2 in
    bit_eq f1 cf1;
    bit_eq f2 cf2
  )
;;

let () =
  CF32.check_float32s (fun f _ ->
    let i = Random.int 1000 in
    let cf = CF32.ldexp f i in
    let f = F32.ldexp f i in
    bit_eq f cf
  );
  let f = F32.ldexp 0.0s 0 in
  assert (f = 0.0s);
  let f = F32.ldexp 1.0s 1 in
  assert (f = 2.0s);
  let f = F32.ldexp (-1.0s) 1 in
  assert (f = (-2.0s));
  let f = F32.ldexp 0.5s 3 in
  assert (f = 4.0s);
  let f = F32.ldexp (-0.5s) 3 in
  assert (f = (-4.0s));
  let f = F32.ldexp 2.0s 9 in
  assert (f = 1024.0s);
;;

let () =
  CF32.check_float32s (fun f1 f2 ->
    bit_eq (F32.fma f1 f2 f1) (CF32.fma f1 f2 f1)
  )
;;

let () =
  CF32.check_float32s (fun f _ ->
    assert (F32.seeded_hash 42 f = Hashtbl.seeded_hash 42 f);
    assert (F32.hash f = Hashtbl.hash f);
  )
;;

let () =
  CF32.check_float32s (fun f _ ->
    assert (F32.to_bits f = CF32.to_bits f);
    assert (Int32.bits_of_float (F32.to_float f) = F32.to_bits f);
    bit_eq (F32.of_bits (CF32.to_bits f)) f;
    bit_eq (F32.of_bits (F32.to_bits f)) f;
    bit_eq (F32.of_float (Int32.float_of_bits (F32.to_bits f))) f;
  );
  CF32.check_float32s (fun f _ ->
    assert (F32.to_int f = CF32.to_int f);
    assert (F32.to_int64 f = CF32.to_int64 f);
    if CF32.is_nan f then assert (Float.is_nan (F32.to_float f))
    else assert (F32.to_float f = CF32.to_float f)
  );
  for _ = 0 to 100_000 do
    let i = if Random.bool () then Random.full_int Int.max_int else Int.neg (Random.full_int Int.max_int) in
    let f = if Random.bool () then Random.float Float.max_float else Float.neg (Random.float Float.max_float) in
    let i64 = if Random.bool () then Random.int64 Int64.max_int else Int64.neg (Random.int64 Int64.max_int) in
    bit_eq (F32.of_int i) (CF32.of_int i);
    bit_eq (F32.of_int64 i64) (CF32.of_int64 i64);
    bit_eq (F32.of_float f) (CF32.of_float f);
  done
;;

let () =
  try ignore (F32.of_string ""); assert false;
  with Failure msg -> assert (msg = "float32_of_string");
  try ignore (F32.of_string "a"); assert false;
  with Failure msg -> assert (msg = "float32_of_string");
  try ignore (F32.of_string "0.0.0"); assert false;
  with Failure msg -> assert (msg = "float32_of_string");
  try ignore (F32.of_string "0xzz"); assert false;
  with Failure msg -> assert (msg = "float32_of_string");
  try ignore (F32.of_string "1e10.0"); assert false;
  with Failure msg -> assert (msg = "float32_of_string");
  assert (Option.is_none (F32.of_string_opt ""));
  assert (Option.is_none (F32.of_string_opt "a"));
  assert (Option.is_none (F32.of_string_opt "0.0.0"));
  assert (Option.is_none (F32.of_string_opt "0xzz"));
  assert (Option.is_none (F32.of_string_opt "1e10.0"));
;;

external format : string -> float32 -> string = "caml_format_float32"

(* [to_string] calls format with "%.9g"; these are some additional format string tests. *)
let () =
  assert ((format "%.0g" 0.1234s) = "0.1");
  assert ((format "%.1g" 0.1234s) = "0.1");
  assert ((format "%.2g" 0.1234s) = "0.12");
  assert ((format "%.3g" 0.1234s) = "0.123");
  assert ((format "%f" 0.1234s) = "0.123400");
  assert ((format "%f" 1024.s) = "1024.000000");
  assert ((format "%f" 1e10s) = "10000000000.000000");
  assert ((format "%g" 1e20s) = "1e+20");
  assert ((format "%g" 1e-20s) = "1e-20");
  assert ((format "%f" F32.infinity) = "inf");
  assert ((format "%f" F32.neg_infinity) = "-inf");
  assert ((format "%f" F32.nan) = "nan");
;;

external format : string -> float32 -> string = "caml_format_float32"

let () =
  CF32.check_float32s (fun f _ ->
    bit_eq (F32.of_string (F32.to_string f)) f;
    match (F32.of_string_opt (F32.to_string f)) with
    | None -> assert false
    | Some f' -> bit_eq f f'
  );
  let check s f = bit_eq (F32.of_string s) f in
  check "0.0" 0.0s;
  check "1.0" 1.0s;
  check "0.5" 0.5s;
  check "1234.1234" 1234.1234s;
  check "0." 0.s;
  check "1e10" 1e10s;
  check "1e-9_8" 1e-9_8s;
  check "1e+1" 1e+1s;
  check "1.12345e+12" 1.12345e+12s;
  check "0x2_2p+0" 0x22p+0s;
  check "0x2p+0" 0x2p+0s;
  check "0x3p+0" 0x3p+0s;
  check "0x5p+0" 0x5p+0s;
  check "0x1.4p+0" 0x1.4p+0s;
  check "0xcp-4" 0xcp-4s;
  check "0x1p-4" 0x1p-4s;
  check "0x1p+0" 0x1p+0s;
  check "0x0p+0" 0x0p+0s;
  check "0xf.f___ffffp+124" 0xf.fffffp+124s;
  check "0xf.ffffffffffff8p+1020" 0xf.ffffffffffff8p+1020s;
  check "0x4p-128" 0x4p-128s;
  check "0x1p-252" 0x1p-252s;
  check "0x4p-1024" 0x4p-1024s;
  check "0x8p-972" 0x8p-972s;
  check "0xf.fff_f_e_000001p+252" 0xf.ff_ffe_00_0001p+252s;
  check "0x2.fffp+12" 0x2.fffp+12s;
  check "0x1.ffffp-24" 0x1.ffffp-24s;
  check "0x2._fff006p+12" 0x2._fff006p+12s;
  check "0x1.fffp+0" 0x1.fffp+0s;
  check "0x1.00001p+0" 0x1.00001p+0s;
  check "0xc.d5e6fp+1_24" 0xc.d5e6fp+1_24s;
  check "0x2.6af378p-128" 0x2.6af378p-128s;
  check "0x5p-128" 0x5p-128s;
  check "0x1____p-128" 0x1p-128s;
  check "0x8p-152" 0x8p-152s;
  check "0x8p-4" 0x8p-4s;
  check "0x8p+124" 0x8p+124s;
  check "0x1000002p+0" 0x1000002p+0s;
  check "0x1000003p+0" 0x1000003p+0s;
  check "0x100000fp+0" 0x100000fp+0s;
  check "0x10000001p+0" 0x10000001p+0s;
  check "0x10000002p+0" 0x10000002p+0s;
  check "0x10000003p+0" 0x10000003p+0s;
  check "0x1000000fp+0" 0x1000000fp+0s;
  check "0x1000003fp+0" 0x1000003fp+0s;
  check "0x1000002fp+0" 0x1000002fp+0s;
  check "0x1.000002p+0" 0x1.000002p+0s;
  check "0x1.000003p+0" 0x1.000003p+0s;
  check "0x1.00000fp+0" 0x1.00000fp+0s;
  check "0x1.0000001p+0" 0x1.0000001p+0s;
  check "0x1.0000002p+0" 0x1.0000002p+0s;
  check "0x1.0000003p+0" 0x1.0000003p+0s;
  check "0x1.000000fp+0" 0x1.000000fp+0s;
  check "0x1.000002fp+0" 0x1.000002fp+0s;
  check "0x1.00000200p+0" 0x1.00000200p+0s;
  check "0x1.000002001p+0" 0x1.000002001p+0s;
  check "0x1.000002000000000p+0" 0x1.000002000000000p+0s;
  check "0x1.000002000000001p+0" 0x1.000002000000001p+0s;
  check "0x1.00000200000000000000p+0" 0x1.00000200000000000000p+0s;
  check "0x1.00000200000000000001p+0" 0x1.00000200000000000001p+0s;
  check "0x1.000003p+0" 0x1.000003p+0s;
;;

let () = (* Cast & sqrt-with-memory intrinsics (see selection.ml) *)
  CF32.check_float32s (fun f _ ->
    bit_eq (F32.sqrt (Sys.opaque_identity f)) (CF32.sqrt f);
    assert (F32.to_bits (Sys.opaque_identity f) = CF32.to_bits f);
    bit_eq (F32.of_bits (CF32.to_bits f)) f;
  )
;;

module Bytes = struct

  let data = Bytes.of_string "\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = F32.of_bits 0x03020100l
  let high = F32.of_bits 0x07060504l

  (* Getters *)

  let () =
    let v = F32.Bytes.get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bytes.unsafe_get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bytes.get data ~pos:4 in
    bit_eq high v;
    let v = F32.Bytes.unsafe_get data ~pos:4 in
    bit_eq high v;
  ;;

  let () =
    for bad = -4 to -1 do
      try
        let _ = F32.Bytes.get data ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bytes.get data ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Setters *)

  let set f pos =
    F32.Bytes.set data f ~pos;
    let v = F32.Bytes.get data ~pos in
    bit_eq f v;
  ;;

  let set_unsafe f pos =
    F32.Bytes.unsafe_set data f ~pos;
    let v = F32.Bytes.get data ~pos in
    bit_eq f v;
  ;;

  let () =
    set (F32.of_bits 0x10101010l) 0;
    set (F32.of_bits 0x20202020l) 4;
    set_unsafe (F32.of_bits 0x10101010l) 0;
    set_unsafe (F32.of_bits 0x20202020l) 4;
    Random.init 1234;
    for _ = 1 to 1000 do
      set (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5);
      set_unsafe (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5)
    done;
  ;;

  let () =
    let set = F32.of_bits 0xFFFFFFFFl in
    for bad = -4 to -1 do
      try
        let _ = F32.Bytes.set data set ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bytes.set data set ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;
end

module String = struct

  let data = "\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = F32.of_bits 0x03020100l
  let high = F32.of_bits 0x07060504l

  (* Getters *)

  let () =
    let v = F32.String.get data ~pos:0 in
    bit_eq low v;
    let v = F32.String.unsafe_get data ~pos:0 in
    bit_eq low v;
    let v = F32.String.get data ~pos:4 in
    bit_eq high v;
    let v = F32.String.unsafe_get data ~pos:4 in
    bit_eq high v;
  ;;

  let () =
    for bad = -4 to -1 do
      try
        let _ = F32.String.get data ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.String.get data ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;
end

module Bigstring = struct
  open Bigarray

  let bigstring_of_string s =
    let open Stdlib in
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  let data = bigstring_of_string "\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = F32.of_bits 0x03020100l
  let high = F32.of_bits 0x07060504l

  (* Getters *)

  let () =
    let v = F32.Bigstring.get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bigstring.unsafe_get data ~pos:0 in
    bit_eq low v;
    let v = F32.Bigstring.get data ~pos:4 in
    bit_eq high v;
    let v = F32.Bigstring.unsafe_get data ~pos:4 in
    bit_eq high v;
  ;;

  let () =
    for bad = -4 to -1 do
      try
        let _ = F32.Bigstring.get data ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bigstring.get data ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Setters *)

  let set f pos =
    F32.Bigstring.set data f ~pos;
    let v = F32.Bigstring.get data ~pos in
    bit_eq f v;
  ;;

  let set_unsafe f pos =
    F32.Bigstring.unsafe_set data f ~pos;
    let v = F32.Bigstring.get data ~pos in
    bit_eq f v;
  ;;

  let () =
    set (F32.of_bits 0x10101010l) 0;
    set (F32.of_bits 0x20202020l) 4;
    set_unsafe (F32.of_bits 0x10101010l) 0;
    set_unsafe (F32.of_bits 0x20202020l) 4;
    Random.init 1234;
    for _ = 1 to 1000 do
      set (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5);
      set_unsafe (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 5)
    done;
  ;;

  let () =
    let set = F32.of_bits 0xFFFFFFFFl in
    for bad = -4 to -1 do
      try
        let _ = F32.Bigstring.set data set ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
    for bad = 5 to 9 do
      try
        let _ = F32.Bigstring.set data set ~pos:bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;
end

module Bigarray = struct
  open Stdlib.Bigarray

  module A1 = struct
    let c_array = Array1.init Float32 C_layout 4 Float.of_int

    let f_array = Array1.init Float32 Fortran_layout 4 Float.of_int

    let () =
      let v = F32.Bigarray.Array1.get c_array 0 in
      bit_eq 0.0s v;
      let v = F32.Bigarray.Array1.unsafe_get c_array 0 in
      bit_eq 0.0s v;
      let v = F32.Bigarray.Array1.get c_array 3 in
      bit_eq 3.0s v;
      let v = F32.Bigarray.Array1.unsafe_get c_array 3 in
      bit_eq 3.0s v;
    ;;

    let () =
      let v = F32.Bigarray.Array1.get f_array 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array1.unsafe_get f_array 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array1.get f_array 4 in
      bit_eq 4.0s v;
      let v = F32.Bigarray.Array1.unsafe_get f_array 4 in
      bit_eq 4.0s v;
    ;;

    let set array f pos =
      F32.Bigarray.Array1.set array pos f;
      let v = F32.Bigarray.Array1.get array pos in
      bit_eq f v;
    ;;

    let set_unsafe array f pos =
      F32.Bigarray.Array1.unsafe_set array pos f;
      let v = F32.Bigarray.Array1.get array pos in
      bit_eq f v;
    ;;

    let () =
      set c_array (F32.of_bits 0x10101010l) 0;
      set c_array (F32.of_bits 0x20202020l) 1;
      set_unsafe c_array (F32.of_bits 0x10101010l) 2;
      set_unsafe c_array (F32.of_bits 0x20202020l) 3;
      Random.init 1234;
      for _ = 1 to 1000 do
        set c_array (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 4);
        set_unsafe c_array (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 4)
      done;
      set f_array (F32.of_bits 0x10101010l) 1;
      set f_array (F32.of_bits 0x20202020l) 2;
      set_unsafe f_array (F32.of_bits 0x10101010l) 3;
      set_unsafe f_array (F32.of_bits 0x20202020l) 4;
      Random.init 1234;
      for _ = 1 to 1000 do
        set f_array (Random.int32 Int32.max_int |> F32.of_bits) (1 + Random.int 4);
        set_unsafe f_array (Random.int32 Int32.max_int |> F32.of_bits) (1 + Random.int 4)
      done;
    ;;

    let () =
      let check f =
        try f () |> ignore; assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      in
      check (fun () -> F32.Bigarray.Array1.get c_array (-1));
      check (fun () -> F32.Bigarray.Array1.set c_array (-1) 0.0s);
      check (fun () -> F32.Bigarray.Array1.get c_array 4);
      check (fun () -> F32.Bigarray.Array1.set c_array 4 0.0s);
      check (fun () -> F32.Bigarray.Array1.get f_array 0);
      check (fun () -> F32.Bigarray.Array1.set f_array 0 0.0s);
      check (fun () -> F32.Bigarray.Array1.get f_array 5);
      check (fun () -> F32.Bigarray.Array1.set f_array 5 0.0s);
    ;;
  end

  module A2 = struct
    let c_array = Array2.init Float32 C_layout 4 4 (fun i j -> Float.of_int (i * 4 + j))

    let f_array = Array2.init Float32 Fortran_layout 4 4 (fun i j -> Float.of_int (i * 4 + j))

    let () =
      let v = F32.Bigarray.Array2.get c_array 0 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array2.unsafe_get c_array 0 1 in
      bit_eq 1.0s v;
      let v = F32.Bigarray.Array2.get c_array 3 2 in
      bit_eq 14.0s v;
      let v = F32.Bigarray.Array2.unsafe_get c_array 3 2 in
      bit_eq 14.0s v;
    ;;

    let () =
      let v = F32.Bigarray.Array2.get f_array 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array2.unsafe_get f_array 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array2.get f_array 4 3 in
      bit_eq 19.0s v;
      let v = F32.Bigarray.Array2.unsafe_get f_array 4 3 in
      bit_eq 19.0s v;
    ;;

    let set array f i j =
      F32.Bigarray.Array2.set array i j f;
      let v = F32.Bigarray.Array2.get array i j in
      bit_eq f v;
    ;;

    let set_unsafe array f i j =
      F32.Bigarray.Array2.unsafe_set array i j f;
      let v = F32.Bigarray.Array2.get array i j in
      bit_eq f v;
    ;;

    let () =
      set c_array (F32.of_bits 0x10101010l) 0 1;
      set c_array (F32.of_bits 0x20202020l) 1 0;
      set_unsafe c_array (F32.of_bits 0x10101010l) 2 3;
      set_unsafe c_array (F32.of_bits 0x20202020l) 3 2;
      Random.init 1234;
      for _ = 1 to 1000 do
        set c_array (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 4) (Random.int 4);
        set_unsafe c_array (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 4) (Random.int 4)
      done;
      set f_array (F32.of_bits 0x10101010l) 1 2;
      set f_array (F32.of_bits 0x20202020l) 2 1;
      set_unsafe f_array (F32.of_bits 0x10101010l) 3 4;
      set_unsafe f_array (F32.of_bits 0x20202020l) 4 3;
      Random.init 1234;
      for _ = 1 to 1000 do
        set f_array (Random.int32 Int32.max_int |> F32.of_bits) (1 + Random.int 4) (1 + Random.int 4);
        set_unsafe f_array (Random.int32 Int32.max_int |> F32.of_bits) (1 + Random.int 4) (1 + Random.int 4)
      done;
    ;;

    let () =
      let check f =
        try f () |> ignore; assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      in
      check (fun () -> F32.Bigarray.Array2.get c_array (-1) 0);
      check (fun () -> F32.Bigarray.Array2.set c_array (-1) 0 0.0s);
      check (fun () -> F32.Bigarray.Array2.get c_array 4 0);
      check (fun () -> F32.Bigarray.Array2.set c_array 4 0 0.0s);
      check (fun () -> F32.Bigarray.Array2.get c_array 0 (-1));
      check (fun () -> F32.Bigarray.Array2.set c_array 0 (-1) 0.0s);
      check (fun () -> F32.Bigarray.Array2.get c_array 0 4);
      check (fun () -> F32.Bigarray.Array2.set c_array 0 4 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 0 1);
      check (fun () -> F32.Bigarray.Array2.set f_array 0 1 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 5 1);
      check (fun () -> F32.Bigarray.Array2.set f_array 5 1 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 1 0);
      check (fun () -> F32.Bigarray.Array2.set f_array 1 0 0.0s);
      check (fun () -> F32.Bigarray.Array2.get f_array 1 5);
      check (fun () -> F32.Bigarray.Array2.set f_array 1 5 0.0s);
    ;;
  end

  module A3 = struct
    let c_array = Array3.init Float32 C_layout 4 4 4 (fun i j k -> Float.of_int (i * 16 + j * 4 + k))

    let f_array = Array3.init Float32 Fortran_layout 4 4 4 (fun i j k -> Float.of_int (i * 16 + j * 4 + k))

    let () =
      let v = F32.Bigarray.Array3.get c_array 0 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array3.unsafe_get c_array 0 1 2 in
      bit_eq 6.0s v;
      let v = F32.Bigarray.Array3.get c_array 3 2 1 in
      bit_eq 57.0s v;
      let v = F32.Bigarray.Array3.unsafe_get c_array 3 2 1 in
      bit_eq 57.0s v;
    ;;

    let () =
      let v = F32.Bigarray.Array3.get f_array 1 2 3 in
      bit_eq 27.0s v;
      let v = F32.Bigarray.Array3.unsafe_get f_array 1 2 3 in
      bit_eq 27.0s v;
      let v = F32.Bigarray.Array3.get f_array 4 3 2 in
      bit_eq 78.0s v;
      let v = F32.Bigarray.Array3.unsafe_get f_array 4 3 2 in
      bit_eq 78.0s v;
    ;;

    let set array f i j k =
      F32.Bigarray.Array3.set array i j k f;
      let v = F32.Bigarray.Array3.get array i j k in
      bit_eq f v;
    ;;

    let set_unsafe array f i j k =
      F32.Bigarray.Array3.unsafe_set array i j k f;
      let v = F32.Bigarray.Array3.get array i j k in
      bit_eq f v;
    ;;

    let () =
      set c_array (F32.of_bits 0x10101010l) 0 1 2;
      set c_array (F32.of_bits 0x20202020l) 2 1 0;
      set_unsafe c_array (F32.of_bits 0x10101010l) 1 2 3;
      set_unsafe c_array (F32.of_bits 0x20202020l) 3 2 1;
      Random.init 1234;
      for _ = 1 to 1000 do
        set c_array (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 4) (Random.int 4) (Random.int 4);
        set_unsafe c_array (Random.int32 Int32.max_int |> F32.of_bits) (Random.int 4) (Random.int 4) (Random.int 4)
      done;
      set f_array (F32.of_bits 0x10101010l) 1 2 3;
      set f_array (F32.of_bits 0x20202020l) 3 2 1;
      set_unsafe f_array (F32.of_bits 0x10101010l) 2 3 4;
      set_unsafe f_array (F32.of_bits 0x20202020l) 4 3 2;
      Random.init 1234;
      for _ = 1 to 1000 do
        set f_array (Random.int32 Int32.max_int |> F32.of_bits) (1 + Random.int 4) (1 + Random.int 4) (1 + Random.int 4);
        set_unsafe f_array (Random.int32 Int32.max_int |> F32.of_bits) (1 + Random.int 4) (1 + Random.int 4) (1 + Random.int 4)
      done;
    ;;

    let () =
      let check f =
        try f () |> ignore; assert false
        with | Invalid_argument s when s = "index out of bounds" -> ()
      in
      check (fun () -> F32.Bigarray.Array3.get c_array (-1) 0 0);
      check (fun () -> F32.Bigarray.Array3.set c_array (-1) 0 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 4 0 0);
      check (fun () -> F32.Bigarray.Array3.set c_array 4 0 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 (-1) 0);
      check (fun () -> F32.Bigarray.Array3.set c_array 0 (-1) 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 4 0);
      check (fun () -> F32.Bigarray.Array3.set c_array 0 4 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 0 (-1));
      check (fun () -> F32.Bigarray.Array3.set c_array 0 0 (-1) 0.0s);
      check (fun () -> F32.Bigarray.Array3.get c_array 0 0 4);
      check (fun () -> F32.Bigarray.Array3.set c_array 0 0 4 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 0 1 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 0 1 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 5 1 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 5 1 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 0 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 0 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 5 1);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 5 1 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 1 0);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 1 0 0.0s);
      check (fun () -> F32.Bigarray.Array3.get f_array 1 1 5);
      check (fun () -> F32.Bigarray.Array3.set f_array 1 1 5 0.0s);
    ;;
  end
end

