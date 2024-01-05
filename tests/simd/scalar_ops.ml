
external int64x2_of_int64s : int64 -> int64 -> int64x2 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

let eq lv hv l h =
  if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
  if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h
;;

let eq' x y =
  if x <> y then Printf.printf "%016Lx <> %016Lx\n" x y
;;

let eqi x y =
  if x <> y then Printf.printf "%d <> %d\n" x y
;;

module Int64x2 = struct

  type t = int64x2

  external clmul : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed]) = "caml_vec128_unreachable" "caml_clmul_int64x2"
      [@@noalloc] [@@builtin]

  let () =
    let v0 = int64x2_of_int64s 3L 4L in
    let v1 = int64x2_of_int64s 5L 12L in
    let c0 = clmul 0b0000_0000 v0 v1 in
    let c1 = clmul 0b0000_0001 v0 v1 in
    let c2 = clmul 0b0001_0000 v0 v1 in
    let c3 = clmul 0b0001_0001 v0 v1 in
    eq (int64x2_low_int64 c0) (int64x2_high_int64 c0) 15L 0L;
    eq (int64x2_low_int64 c1) (int64x2_high_int64 c1) 20L 0L;
    eq (int64x2_low_int64 c2) (int64x2_high_int64 c2) 20L 0L;
    eq (int64x2_low_int64 c3) (int64x2_high_int64 c3) 48L 0L
  ;;
end

module Int = struct

  external count_leading_zeros
    :  int
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int_clz_tagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_leading_zeros2
    :  int
    -> int
    = "caml_vec128_unreachable" "caml_int_clz_untagged_to_untagged"
  [@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_set_bits
    :  int
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int_popcnt_tagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_set_bits2
    :  int
    -> int
    = "caml_vec128_unreachable" "caml_int_popcnt_untagged_to_untagged"
  [@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros
    :  int
    -> int
    = "caml_vec128_unreachable" "caml_int_ctz_untagged_to_untagged"
  [@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let check f g =
      let open Int in
      Random.set_state (Random.State.make [|1234567890|]);
      eqi (f zero) (g zero);
      eqi (f one) (g one);
      eqi (f minus_one) (g minus_one);
      eqi (f max_int) (g max_int);
      eqi (f min_int) (g min_int);
      for i = 0 to 100_000 do
          let i = Random.full_int max_int in
          let i = if Random.bool () then i else neg i in
          eqi (f i) (g i)
      done
  ;;

  let rec clz i =
    if i = 0 then 63
    else if (i land 0x4000000000000000) = 0x4000000000000000 then 0
    else 1 + (clz (i lsl 1))
  ;;

  let rec ctz i =
    if i = 0 then 63
    else if (i land 1) = 1 then 0
    else 1 + (ctz (i lsr 1))
  ;;

  let rec popcnt i =
    if i = 0 then 0
    else (i land 1) + (popcnt (i lsr 1))
  ;;

  let () =
    check count_leading_zeros clz;
    check count_leading_zeros2 clz;
    check count_set_bits popcnt;
    check count_set_bits2 popcnt;
    check count_trailing_zeros ctz
  ;;
end

module Int64 = struct

  type t = int64

  external bit_deposit : t -> t -> t = "caml_vec128_unreachable" "caml_bmi2_int64_deposit_bits"
      [@@noalloc] [@@unboxed] [@@builtin]
  external bit_extract : t -> t -> t = "caml_vec128_unreachable" "caml_bmi2_int64_extract_bits"
      [@@noalloc] [@@unboxed] [@@builtin]

  let () =
    Test_helpers.run_if_not_under_rosetta2 ~f:(fun () ->
        eq' (bit_deposit 3L 4L) 0x4L;
        eq' (bit_deposit 235L 522L) 0xAL;
        eq' (bit_extract 3L 4L) 0x0L;
        eq' (bit_extract 235L 522L) 0x3L)
  ;;

  external count_leading_zeros
    :  (int64[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_clz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_leading_zeros_nonzero_arg
    :  (int64[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_clz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros
    :  (int64[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_ctz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** Same as [count_trailing_zeros] except if the argument is zero,
      then the result is undefined. Emits more efficient code. *)
  external count_trailing_zeros_nonzero_arg
    :  (int64[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_ctz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
  external count_set_bits
    :  (int64[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_popcnt_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let check ?nonzero f g =
      let nz = Option.value ~default:false nonzero in
      let open Stdlib.Int64 in
      Random.set_state (Random.State.make [|1234567890|]);
      if not nz then eqi (f zero) (g zero);
      eqi (f one) (g one);
      eqi (f minus_one) (g minus_one);
      eqi (f max_int) (g max_int);
      eqi (f min_int) (g min_int);
      for i = 0 to 100_000 do
          let i = Random.int64 max_int in
          let i = if Random.bool () then i else neg i in
          if not nz || i <> 0L then eqi (f i) (g i);
      done
  ;;

  let rec clz i =
    if i = 0L then 64
    else if Int64.((logand i 0x8000000000000000L) = 0x8000000000000000L) then 0
    else 1 + (clz (Int64.shift_left i 1))
  ;;

  let rec ctz i =
    if i = 0L then 64
    else if Int64.((logand i 1L) = 1L) then 0
    else 1 + (ctz (Int64.shift_right_logical i 1))
  ;;

  let rec popcnt i =
    if i = 0L then 0
    else Int64.(logand i 1L |> to_int) + (popcnt Int64.(shift_right_logical i 1))
  ;;

  let () =
    check count_leading_zeros clz;
    check ~nonzero:true count_leading_zeros_nonzero_arg clz;
    check count_trailing_zeros ctz;
    check ~nonzero:true count_trailing_zeros_nonzero_arg ctz;
    check count_set_bits popcnt
  ;;
end

module Int32 = struct

  external count_leading_zeros
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_clz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_leading_zeros_nonzero_arg
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_clz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_ctz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** Same as [count_trailing_zeros] except if the argument is zero,
      then the result is undefined. Emits more efficient code. *)
  external count_trailing_zeros_nonzero_arg
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_ctz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
  external count_set_bits
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_popcnt_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let check ?nonzero f g =
      let nz = Option.value ~default:false nonzero in
      let open Stdlib.Int32 in
      Random.set_state (Random.State.make [|1234567890|]);
      if not nz then eqi (f zero) (g zero);
      eqi (f one) (g one);
      eqi (f minus_one) (g minus_one);
      eqi (f max_int) (g max_int);
      eqi (f min_int) (g min_int);
      for i = 0 to 100_000 do
          let i = Random.int32 max_int in
          let i = if Random.bool () then i else neg i in
          if not nz || i <> 0l then eqi (f i) (g i);
      done
  ;;

  let rec clz i =
    if i = 0l then 32
    else if Int32.((logand i 0x80000000l) = 0x80000000l) then 0
    else 1 + (clz (Int32.shift_left i 1))
  ;;

  let rec ctz i =
    if i = 0l then 32
    else if Int32.((logand i 1l) = 1l) then 0
    else 1 + (ctz (Int32.shift_right_logical i 1))
  ;;

  let rec popcnt i =
    if i = 0l then 0
    else Int32.(logand i 1l |> to_int) + (popcnt Int32.(shift_right_logical i 1))
  ;;

  let () =
    check count_leading_zeros clz;
    check ~nonzero:true count_leading_zeros_nonzero_arg clz;
    check count_trailing_zeros ctz;
    check ~nonzero:true count_trailing_zeros_nonzero_arg ctz;
    check count_set_bits popcnt
  ;;
end
