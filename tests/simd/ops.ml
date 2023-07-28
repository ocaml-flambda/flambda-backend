open Stdlib

let failmsg = ref (fun () -> ())

let eq lv hv l h =
    if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
    if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h;
    if l <> lv || h <> hv then !failmsg ()
;;

let eql lv hv l h =
    if l <> lv then Printf.printf "%016lx <> %016lx\n" lv l;
    if h <> hv then Printf.printf "%016lx <> %016lx\n" hv h;
    if l <> lv || h <> hv then !failmsg ()
;;

let eqi lv hv l h =
    if l <> lv then Printf.printf "%016x <> %016x\n" lv l;
    if h <> hv then Printf.printf "%016x <> %016x\n" hv h;
    if l <> lv || h <> hv then !failmsg ()
;;

let eqf lv hv l h =
    let open Float in
    if l <> lv then Printf.printf "%f <> %f\n" l lv;
    if h <> hv then Printf.printf "%f <> %f\n" h hv;
    if l <> lv || h <> hv then !failmsg ()
;;

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int32x4_of_int64s : int64 -> int64 -> int32x4 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int32x4_low_int64 : int32x4 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int32x4_high_int64 : int32x4 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int16x8_of_int64s : int64 -> int64 -> int16x8 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int16x8_low_int64 : int16x8 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int16x8_high_int64 : int16x8 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int8x16_of_int64s : int64 -> int64 -> int8x16 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int8x16_low_int64 : int8x16 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int8x16_high_int64 : int8x16 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external float32x4_of_int64s : int64 -> int64 -> float32x4 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external float32x4_low_int64 : float32x4 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float32x4_high_int64 : float32x4 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external float64x2_of_int64s : int64 -> int64 -> float64x2 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external float64x2_low_int64 : float64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float64x2_high_int64 : float64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

let () =
    let a : int8x16 = int8x16_of_int64s 1L 2L in
    let b : int16x8 = int16x8_of_int64s 3L 4L in
    let c : int32x4 = int32x4_of_int64s 5L 6L in
    let d : int64x2 = int64x2_of_int64s 7L 8L in
    let e : float32x4 = float32x4_of_int64s 9L 10L in
    let f : float64x2 = float64x2_of_int64s 11L 12L in
    let al, ah = int8x16_low_int64 a, int8x16_high_int64 a in
    eq al ah 1L 2L;
    let bl, bh = int16x8_low_int64 b, int16x8_high_int64 b in
    eq bl bh 3L 4L;
    let cl, ch = int32x4_low_int64 c, int32x4_high_int64 c in
    eq cl ch 5L 6L;
    let dl, dh = int64x2_low_int64 d, int64x2_high_int64 d in
    eq dl dh 7L 8L;
    let el, eh = float32x4_low_int64 e, float32x4_high_int64 e in
    eq el eh 9L 10L;
    let fl, fh = float64x2_low_int64 f, float64x2_high_int64 f in
    eq fl fh 11L 12L
;;

module Vector_casts = struct

    external int64x2_of_int32x4 : int32x4 -> int64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_int16x8 : int16x8 -> int64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_int8x16 : int8x16 -> int64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_float32x4 : float32x4 -> int64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_float64x2 : float64x2 -> int64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let _0 = int32x4_of_int64s   1L 2L in
        let _1 = int16x8_of_int64s   3L 4L in
        let _2 = int8x16_of_int64s   5L 6L in
        let _3 = float32x4_of_int64s 7L 8L in
        let _4 = float64x2_of_int64s 9L 10L in
        let _0 = int64x2_of_int32x4   (Sys.opaque_identity _0) in
        let _1 = int64x2_of_int16x8   (Sys.opaque_identity _1) in
        let _2 = int64x2_of_int8x16   (Sys.opaque_identity _2) in
        let _3 = int64x2_of_float32x4 (Sys.opaque_identity _3) in
        let _4 = int64x2_of_float64x2 (Sys.opaque_identity _4) in
        let a, b = int64x2_low_int64 _0, int64x2_high_int64 _0 in
        eq a b 1L 2L;
        let a, b = int64x2_low_int64 _1, int64x2_high_int64 _1 in
        eq a b 3L 4L;
        let a, b = int64x2_low_int64 _2, int64x2_high_int64 _2 in
        eq a b 5L 6L;
        let a, b = int64x2_low_int64 _3, int64x2_high_int64 _3 in
        eq a b 7L 8L;
        let a, b = int64x2_low_int64 _4, int64x2_high_int64 _4 in
        eq a b 9L 10L
    ;;

    external int32x4_of_int64x2 : int64x2 -> int32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_int16x8 : int16x8 -> int32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_int8x16 : int8x16 -> int32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_float32x4 : float32x4 -> int32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_float64x2 : float64x2 -> int32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let _0 = int64x2_of_int64s   1L 2L in
        let _1 = int16x8_of_int64s   3L 4L in
        let _2 = int8x16_of_int64s   5L 6L in
        let _3 = float32x4_of_int64s 7L 8L in
        let _4 = float64x2_of_int64s 9L 10L in
        let _0 = int32x4_of_int64x2   (Sys.opaque_identity _0) in
        let _1 = int32x4_of_int16x8   (Sys.opaque_identity _1) in
        let _2 = int32x4_of_int8x16   (Sys.opaque_identity _2) in
        let _3 = int32x4_of_float32x4 (Sys.opaque_identity _3) in
        let _4 = int32x4_of_float64x2 (Sys.opaque_identity _4) in
        let a, b = int32x4_low_int64 _0, int32x4_high_int64 _0 in
        eq a b 1L 2L;
        let a, b = int32x4_low_int64 _1, int32x4_high_int64 _1 in
        eq a b 3L 4L;
        let a, b = int32x4_low_int64 _2, int32x4_high_int64 _2 in
        eq a b 5L 6L;
        let a, b = int32x4_low_int64 _3, int32x4_high_int64 _3 in
        eq a b 7L 8L;
        let a, b = int32x4_low_int64 _4, int32x4_high_int64 _4 in
        eq a b 9L 10L
    ;;

    external int16x8_of_int64x2 : int64x2 -> int16x8 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_int32x4 : int32x4 -> int16x8 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_int8x16 : int8x16 -> int16x8 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_float32x4 : float32x4 -> int16x8 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_float64x2 : float64x2 -> int16x8 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let _0 = int64x2_of_int64s   1L 2L in
        let _1 = int32x4_of_int64s   3L 4L in
        let _2 = int8x16_of_int64s   5L 6L in
        let _3 = float32x4_of_int64s 7L 8L in
        let _4 = float64x2_of_int64s 9L 10L in
        let _0 = int16x8_of_int64x2   (Sys.opaque_identity _0) in
        let _1 = int16x8_of_int32x4   (Sys.opaque_identity _1) in
        let _2 = int16x8_of_int8x16   (Sys.opaque_identity _2) in
        let _3 = int16x8_of_float32x4 (Sys.opaque_identity _3) in
        let _4 = int16x8_of_float64x2 (Sys.opaque_identity _4) in
        let a, b = int16x8_low_int64 _0, int16x8_high_int64 _0 in
        eq a b 1L 2L;
        let a, b = int16x8_low_int64 _1, int16x8_high_int64 _1 in
        eq a b 3L 4L;
        let a, b = int16x8_low_int64 _2, int16x8_high_int64 _2 in
        eq a b 5L 6L;
        let a, b = int16x8_low_int64 _3, int16x8_high_int64 _3 in
        eq a b 7L 8L;
        let a, b = int16x8_low_int64 _4, int16x8_high_int64 _4 in
        eq a b 9L 10L
    ;;

    external int8x16_of_int64x2 : int64x2 -> int8x16 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_int32x4 : int32x4 -> int8x16 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_int16x8 : int16x8 -> int8x16 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_float32x4 : float32x4 -> int8x16 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_float64x2 : float64x2 -> int8x16 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let _0 = int64x2_of_int64s   1L 2L in
        let _1 = int32x4_of_int64s   3L 4L in
        let _2 = int16x8_of_int64s   5L 6L in
        let _3 = float32x4_of_int64s 7L 8L in
        let _4 = float64x2_of_int64s 9L 10L in
        let _0 = int8x16_of_int64x2   (Sys.opaque_identity _0) in
        let _1 = int8x16_of_int32x4   (Sys.opaque_identity _1) in
        let _2 = int8x16_of_int16x8   (Sys.opaque_identity _2) in
        let _3 = int8x16_of_float32x4 (Sys.opaque_identity _3) in
        let _4 = int8x16_of_float64x2 (Sys.opaque_identity _4) in
        let a, b = int8x16_low_int64 _0, int8x16_high_int64 _0 in
        eq a b 1L 2L;
        let a, b = int8x16_low_int64 _1, int8x16_high_int64 _1 in
        eq a b 3L 4L;
        let a, b = int8x16_low_int64 _2, int8x16_high_int64 _2 in
        eq a b 5L 6L;
        let a, b = int8x16_low_int64 _3, int8x16_high_int64 _3 in
        eq a b 7L 8L;
        let a, b = int8x16_low_int64 _4, int8x16_high_int64 _4 in
        eq a b 9L 10L
    ;;

    external float32x4_of_int64x2 : int64x2 -> float32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int32x4 : int32x4 -> float32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int16x8 : int16x8 -> float32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int8x16 : int8x16 -> float32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_float64x2 : float64x2 -> float32x4 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let _0 = int64x2_of_int64s   1L 2L in
        let _1 = int32x4_of_int64s   3L 4L in
        let _2 = int16x8_of_int64s   5L 6L in
        let _3 = int8x16_of_int64s   7L 8L in
        let _4 = float64x2_of_int64s 9L 10L in
        let _0 = float32x4_of_int64x2   (Sys.opaque_identity _0) in
        let _1 = float32x4_of_int32x4   (Sys.opaque_identity _1) in
        let _2 = float32x4_of_int16x8   (Sys.opaque_identity _2) in
        let _3 = float32x4_of_int8x16   (Sys.opaque_identity _3) in
        let _4 = float32x4_of_float64x2 (Sys.opaque_identity _4) in
        let a, b = float32x4_low_int64 _0, float32x4_high_int64 _0 in
        eq a b 1L 2L;
        let a, b = float32x4_low_int64 _1, float32x4_high_int64 _1 in
        eq a b 3L 4L;
        let a, b = float32x4_low_int64 _2, float32x4_high_int64 _2 in
        eq a b 5L 6L;
        let a, b = float32x4_low_int64 _3, float32x4_high_int64 _3 in
        eq a b 7L 8L;
        let a, b = float32x4_low_int64 _4, float32x4_high_int64 _4 in
        eq a b 9L 10L
    ;;

    external float64x2_of_int64x2 : int64x2 -> float64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int32x4 : int32x4 -> float64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int16x8 : int16x8 -> float64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int8x16 : int8x16 -> float64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_float32x4 : float32x4 -> float64x2 = "" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let _0 = int64x2_of_int64s   1L 2L in
        let _1 = int32x4_of_int64s   3L 4L in
        let _2 = int16x8_of_int64s   5L 6L in
        let _3 = int8x16_of_int64s   7L 8L in
        let _4 = float32x4_of_int64s 9L 10L in
        let _0 = float64x2_of_int64x2   (Sys.opaque_identity _0) in
        let _1 = float64x2_of_int32x4   (Sys.opaque_identity _1) in
        let _2 = float64x2_of_int16x8   (Sys.opaque_identity _2) in
        let _3 = float64x2_of_int8x16   (Sys.opaque_identity _3) in
        let _4 = float64x2_of_float32x4 (Sys.opaque_identity _4) in
        let a, b = float64x2_low_int64 _0, float64x2_high_int64 _0 in
        eq a b 1L 2L;
        let a, b = float64x2_low_int64 _1, float64x2_high_int64 _1 in
        eq a b 3L 4L;
        let a, b = float64x2_low_int64 _2, float64x2_high_int64 _2 in
        eq a b 5L 6L;
        let a, b = float64x2_low_int64 _3, float64x2_high_int64 _3 in
        eq a b 7L 8L;
        let a, b = float64x2_low_int64 _4, float64x2_high_int64 _4 in
        eq a b 9L 10L
    ;;
end

(* For testing *)
module Float32 = struct
    type t = int32

    let of_float f = Int32.bits_of_float f
    let to_float i = Int32.float_of_bits i

    external zero : unit -> (t [@unboxed]) = "" "float32_zero" [@@noalloc]
    external neg_zero : unit -> (t [@unboxed]) = "" "float32_neg_zero" [@@noalloc]
    external one : unit -> (t [@unboxed]) = "" "float32_one" [@@noalloc]
    external neg_one : unit -> (t [@unboxed]) = "" "float32_neg_one" [@@noalloc]
    external nan : unit -> (t [@unboxed]) = "" "float32_nan" [@@noalloc]
    external neg_infinity : unit -> (t [@unboxed]) = "" "float32_neg_infinity" [@@noalloc]
    external infinity : unit -> (t [@unboxed]) = "" "float32_infinity" [@@noalloc]
    external maxv : unit -> (t [@unboxed]) = "" "float32_maxv" [@@noalloc]
    external minv : unit -> (t [@unboxed]) = "" "float32_minv" [@@noalloc]

    let zero = zero ()
    let neg_zero = neg_zero ()
    let one = one ()
    let nan = nan ()
    let neg_infinity = neg_infinity ()
    let infinity = infinity ()
    let neg_one = neg_one ()
    let maxv = maxv ()
    let minv = minv ()

    let to_float32x4 t0 t1 t2 t3 =
        let i0 = Int64.of_int32 t0 |> Int64.logand 0xffffffffL in
        let i1 = Int64.of_int32 t1 |> Int64.logand 0xffffffffL in
        let i2 = Int64.of_int32 t2 |> Int64.logand 0xffffffffL in
        let i3 = Int64.of_int32 t3 |> Int64.logand 0xffffffffL in
        let i0 = Int64.logor (Int64.shift_left i1 32) i0 in
        let i1 = Int64.logor (Int64.shift_left i3 32) i2 in
        float32x4_of_int64s i0 i1
    ;;

    external cvt_i32 : (t [@unboxed]) -> (t [@unboxed]) = "" "float32_cvt_i32" [@@noalloc]
    external round : (t [@unboxed]) -> (t [@unboxed]) = "" "float32_round" [@@noalloc]

    external eq : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_eq" [@@noalloc]
    external lt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_lt" [@@noalloc]
    external le : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_le" [@@noalloc]
    external neq : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_ne" [@@noalloc]
    external nle : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_nle" [@@noalloc]
    external nlt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_nlt" [@@noalloc]
    external ord : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_ord" [@@noalloc]
    external uord : (t [@unboxed]) -> (t [@unboxed]) -> bool = "" "float32_uord" [@@noalloc]

    external add : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "float32_add" [@@noalloc]
    external sub : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "float32_sub" [@@noalloc]
    external mul : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "float32_mul" [@@noalloc]
    external div : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "float32_div" [@@noalloc]
    external min : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "float32_min" [@@noalloc]
    external max : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "float32_max" [@@noalloc]

    external rcp : (t [@unboxed]) -> (t [@unboxed]) = "" "float32_rcp" [@@noalloc]
    external sqrt : (t [@unboxed]) -> (t [@unboxed]) = "" "float32_sqrt" [@@noalloc]
    external rsqrt : (t [@unboxed]) -> (t [@unboxed]) = "" "float32_rsqrt" [@@noalloc]

    let check_floats f =
        Random.set_state (Random.State.make [|1234567890|]);
        f zero zero;
        f zero one;
        f one one;
        f zero neg_one;
        f neg_one neg_one;
        f one neg_one;
        f zero neg_zero;
        f nan zero;
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
            f (if Random.bool () then f0 else Int32.neg f0)
              (if Random.bool () then f1 else Int32.neg f1)
        done
    ;;
end

module Float64 = struct

    type t = float

    external round : (t [@unboxed]) -> (t [@unboxed]) = "" "float64_round" [@@noalloc]

    let check_floats f =
        let open Float in
        Random.set_state (Random.State.make [|1234567890|]);
        f zero zero;
        f zero one;
        f one one;
        f zero minus_one;
        f minus_one minus_one;
        f one minus_one;
        f zero (-0.0);
        f nan zero;
        f infinity zero;
        f neg_infinity zero;
        f nan nan;
        f infinity infinity;
        f neg_infinity neg_infinity;
        f neg_infinity infinity;
        f infinity nan;
        f neg_infinity nan;
        f max_float infinity;
        f max_float neg_infinity;
        f min_float infinity;
        f min_float neg_infinity;
        f max_float max_float;
        f min_float min_float;
        f max_float min_float;
        for i = 0 to 100_000 do
            let f0 = Random.int64 Int64.max_int in
            let f1 = Random.int64 Int64.max_int in
            f (if Random.bool () then Int64.float_of_bits f0 else Int64.(neg f0 |> float_of_bits))
              (if Random.bool () then Int64.float_of_bits f1 else Int64.(neg f1 |> float_of_bits))
        done
    ;;
end

module Int64s = struct
    let check_ints f =
        let open Int64 in
        Random.set_state (Random.State.make [|1234567890|]);
        f zero zero;
        f zero one;
        f one one;
        f zero minus_one;
        f minus_one minus_one;
        f one minus_one;
        f max_int zero;
        f min_int zero;
        f max_int one;
        f min_int one;
        f max_int max_int;
        f min_int min_int;
        f max_int min_int;
        for i = 0 to 100_000 do
            let i0 = Random.int64 Int64.max_int in
            let i1 = Random.int64 Int64.max_int in
            f (if Random.bool () then i0 else Int64.neg i0)
              (if Random.bool () then i1 else Int64.neg i1)
        done
    ;;
end

module Float32x4 = struct

    type t = float32x4

    (* Creation / Destruction

       These are sufficient to implement set1/set4/get1/get4 etc in the user-level library.
       At least in the initial version, we will not provide set1/set4 intrinsics that
       produce constants when given constant args. Instead we provide explicit const intrinsics. *)

    external low_of : float -> t = "" "caml_float32x4_low_of_float"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> float = "" "caml_float32x4_low_to_float"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v1 = low_of 1. in
        let v2 = low_of 2. in
        let i1 = Int64.logand (float32x4_low_int64 v1) 0xffffffffL in
        let i2 = Int64.logand (float32x4_low_int64 v2) 0xffffffffL in
        eq i1 i2 0x3f800000L 0x40000000L;
        let f1 = low_to v1 in
        let f2 = low_to v2 in
        eqf f1 f2 1. 2.
    ;;

    (* Math *)

    external cmp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (int32x4 [@unboxed]) = "" "caml_sse_float32x4_cmp"
        [@@noalloc] [@@builtin]

    external movemask_32 : (int32x4 [@unboxed]) -> (int [@untagged]) = "" "caml_sse_vec128_movemask_32"
        [@@noalloc] [@@builtin]

    let check_cmp scalar vector f0 f1 =
        let expect, expect_mask =
            let r0, m0 = if scalar f0 f1 then 0xffffffffl, 1 else 0l, 0 in
            let r1, m1 = if scalar f1 f0 then 0xffffffffl, 1 else 0l, 0 in
            Float32.to_float32x4 r0 r1 r0 r1 |> Vector_casts.int32x4_of_float32x4, m0 lor (m1 lsl 1) lor (m0 lsl 2) lor (m1 lsl 3)
        in
        let v1 = Float32.to_float32x4 f0 f1 f0 f1 in
        let v2 = Float32.to_float32x4 f1 f0 f1 f0 in
        let result = vector v1 v2 in
        let mask = movemask_32 result in
        eqi mask mask expect_mask (movemask_32 expect);
        eq (int32x4_low_int64 result) (int32x4_high_int64 result)
           (int32x4_low_int64 expect) (int32x4_high_int64 expect)
    ;;
    let () =
        Float32.check_floats (check_cmp Float32.eq (fun l r -> cmp 0 l r));
        Float32.check_floats (check_cmp Float32.lt (fun l r -> cmp 1 l r));
        Float32.check_floats (check_cmp Float32.le (fun l r -> cmp 2 l r));
        Float32.check_floats (check_cmp Float32.uord (fun l r -> cmp 3 l r));
        Float32.check_floats (check_cmp Float32.neq (fun l r -> cmp 4 l r));
        Float32.check_floats (check_cmp Float32.nlt (fun l r -> cmp 5 l r));
        Float32.check_floats (check_cmp Float32.nle (fun l r -> cmp 6 l r));
        Float32.check_floats (check_cmp Float32.ord (fun l r -> cmp 7 l r))
    ;;

    external add : t -> t -> t = "" "caml_sse_float32x4_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "" "caml_sse_float32x4_sub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external mul : t -> t -> t = "" "caml_sse_float32x4_mul"
        [@@noalloc] [@@unboxed] [@@builtin]
    external div : t -> t -> t = "" "caml_sse_float32x4_div"
        [@@noalloc] [@@unboxed] [@@builtin]
    external max : t -> t -> t = "" "caml_sse_float32x4_max"
        [@@noalloc] [@@unboxed] [@@builtin]
    external min : t -> t -> t = "" "caml_sse_float32x4_min"
        [@@noalloc] [@@unboxed] [@@builtin]

    let check_binop scalar vector f0 f1 =
        let r0 = scalar f0 f1 in
        let r1 = scalar f1 f0 in
        let expect = Float32.to_float32x4 r0 r1 r0 r1 in
        let v1 = Float32.to_float32x4 f0 f1 f0 f1 in
        let v2 = Float32.to_float32x4 f1 f0 f1 f0 in
        let result = vector v1 v2 in
        eq (float32x4_low_int64 result) (float32x4_high_int64 result)
           (float32x4_low_int64 expect) (float32x4_high_int64 expect)
    ;;
    let () =
        Float32.check_floats (check_binop Float32.add add);
        Float32.check_floats (check_binop Float32.sub sub);
        Float32.check_floats (check_binop Float32.mul mul);
        Float32.check_floats (check_binop Float32.div div);
        Float32.check_floats (check_binop Float32.max max);
        Float32.check_floats (check_binop Float32.min min)
    ;;

    external rcp : t -> t = "" "caml_sse_float32x4_rcp"
        [@@noalloc] [@@unboxed] [@@builtin]
    external rsqrt : t -> t = "" "caml_sse_float32x4_rsqrt"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sqrt : t -> t = "" "caml_sse_float32x4_sqrt"
        [@@noalloc] [@@unboxed] [@@builtin]

    let check_unop scalar vector f =
        let r = scalar f in
        let expect = Float32.to_float32x4 r r r r in
        let v = Float32.to_float32x4 f f f f in
        let result = vector v in
        eq (float32x4_low_int64 result) (float32x4_high_int64 result)
           (float32x4_low_int64 expect) (float32x4_high_int64 expect)
    ;;
    let () =
        Float32.check_floats (fun f _ -> check_unop Float32.rcp rcp f);
        Float32.check_floats (fun f _ -> check_unop Float32.sqrt sqrt f);
        Float32.check_floats (fun f _ -> check_unop Float32.rsqrt rsqrt f)
    ;;


    external cvt_int32x4 : t -> int32x4 = "" "caml_sse2_cvt_float32x4_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cvt_float64x2 : t -> float64x2 = "" "caml_sse2_cvt_float32x4_float64x2"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        Float32.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0) (Int32.float_of_bits f1));
            let i0 = Float32.cvt_i32 f0 |> Int64.of_int32 |> Int64.logand 0xffffffffL in
            let i1 = Float32.cvt_i32 f1 |> Int64.of_int32 |> Int64.logand 0xffffffffL in
            let ii = Int64.(logor (shift_left i1 32) i0) in
            let iv = int32x4_of_int64s ii ii in
            let fv = Float32.to_float32x4 f0 f1 f0 f1 in
            let res = cvt_int32x4 fv in
            eq (int32x4_low_int64 res) (int32x4_high_int64 res)
               (int32x4_low_int64 iv) (int32x4_high_int64 iv)
        );
        Float32.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0) (Int32.float_of_bits f1));
            let i0 = Int32.float_of_bits f0 |> Int64.bits_of_float in
            let i1 = Int32.float_of_bits f1 |> Int64.bits_of_float in
            let iv = float64x2_of_int64s i0 i1 in
            let fv = Float32.to_float32x4 f0 f1 0l 0l in
            let res = cvt_float64x2 fv in
            eq (float64x2_low_int64 res) (float64x2_high_int64 res)
               (float64x2_low_int64 iv) (float64x2_high_int64 iv)
        )
    ;;

    external addsub : t -> t -> t = "" "caml_sse3_float32x4_addsub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hadd : t -> t -> t = "" "caml_sse3_float32x4_hadd"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hsub : t -> t -> t = "" "caml_sse3_float32x4_hsub"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        Float32.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0) (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f0 f1 f1 in
            let fv1 = Float32.to_float32x4 f1 f1 f0 f0 in
            let result = addsub fv0 fv1 in
            let expect = Float32.to_float32x4 (Float32.sub f0 f1) (Float32.add f0 f1) (Float32.sub f1 f0) (Float32.add f1 f0) in
            eq (float32x4_low_int64 result) (float32x4_high_int64 result)
               (float32x4_low_int64 expect) (float32x4_high_int64 expect)
        );
        Float32.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0) (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f0 f1 f1 in
            let fv1 = Float32.to_float32x4 f1 f1 f0 f0 in
            let result = hadd fv0 fv1 in
            let expect = Float32.to_float32x4 (Float32.add f0 f0) (Float32.add f1 f1) (Float32.add f1 f1) (Float32.add f0 f0) in
            eq (float32x4_low_int64 result) (float32x4_high_int64 result)
               (float32x4_low_int64 expect) (float32x4_high_int64 expect)
        );
        Float32.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0) (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
            let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
            let result = hsub fv0 fv1 in
            let expect = Float32.to_float32x4 (Float32.sub f0 f1) (Float32.sub f0 f1) (Float32.sub f1 f0) (Float32.sub f1 f0) in
            eq (float32x4_low_int64 result) (float32x4_high_int64 result)
               (float32x4_low_int64 expect) (float32x4_high_int64 expect)
        );
    ;;

    external dp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "caml_sse41_float32x4_dp"
        [@@noalloc] [@@builtin]
    external round : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "caml_sse41_float32x4_round"
        [@@noalloc] [@@builtin]

    let () =
        Float32.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "dpf32 %f %f\n%!" (Int32.float_of_bits f0) (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
            let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
            let result = dp 0b1111_0001 fv0 fv1 in
            let expect = Float32.to_float32x4 (Float32.add (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0)) (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0))) 0l 0l 0l in
            (* When both are NaN, AMD returns the first argument and Intel returns the second argument.
               Hence we do not test this case. *)
            if f0 |> Int32.float_of_bits |> Float.is_nan && f1 |> Int32.float_of_bits |> Float.is_nan then () else
            eq (float32x4_low_int64 result) (float32x4_high_int64 result)
            (float32x4_low_int64 expect) (float32x4_high_int64 expect)
        );
        Float32.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "roundf32 %f %f\n%!" (Int32.float_of_bits f0) (Int32.float_of_bits f1));
            let fv = Float32.to_float32x4 f0 f1 f0 f1 in
            let result = round 0 fv in
            let expect = Float32.to_float32x4 (Float32.round f0) (Float32.round f1) (Float32.round f0) (Float32.round f1) in
            eq (float32x4_low_int64 result) (float32x4_high_int64 result)
            (float32x4_low_int64 expect) (float32x4_high_int64 expect)
        )
    ;;
end

module Float64x2 = struct

    type t = float64x2

    (* Creation / Destruction *)

    external low_of : float -> t = "" "caml_float64x2_low_of_float"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> float = "" "caml_float64x2_low_to_float"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v1 = low_of 1. in
        let v2 = low_of 2. in
        let i1 = float64x2_low_int64 v1 in
        let i2 = float64x2_low_int64 v2 in
        eq i1 i2 0x3ff0000000000000L 0x4000000000000000L;
        let f1 = low_to v1 in
        let f2 = low_to v2 in
        eqf f1 f2 1. 2.
    ;;

    let to_float64x2 f0 f1 =
        let v0, v1 = Int64.bits_of_float f0, Int64.bits_of_float f1 in
        float64x2_of_int64s v0 v1
    ;;

    (* Math *)

    external cmp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (int64x2 [@unboxed]) = "" "caml_sse2_float64x2_cmp"
        [@@noalloc] [@@builtin]

    external movemask_64 : (int64x2 [@unboxed]) -> (int [@untagged]) = "" "caml_sse2_vec128_movemask_64"
        [@@noalloc] [@@builtin]

    let check_cmp scalar vector f0 f1 =
        failmsg := (fun () -> Printf.printf "%f | %f\n%!" f0 f1);
        let expect, expect_mask =
            let r0, m0 = if scalar f0 f1 then 0xffffffffffffffffL, 1 else 0L, 0 in
            let r1, m1 = if scalar f1 f0 then 0xffffffffffffffffL, 1 else 0L, 0 in
            int64x2_of_int64s r0 r1, m0 lor (m1 lsl 1)
        in
        let v1 = to_float64x2 f0 f1 in
        let v2 = to_float64x2 f1 f0 in
        let result = vector v1 v2 in
        let mask = movemask_64 result in
        eqi mask mask expect_mask (movemask_64 expect);
        eq (int64x2_low_int64 result) (int64x2_high_int64 result)
           (int64x2_low_int64 expect) (int64x2_high_int64 expect)
    ;;
    let () =
        let remove_nan p l r = p l r && not (Float.is_nan l || Float.is_nan r) in
        let add_nan p l r = p l r || (Float.is_nan l || Float.is_nan r) in
        Float64.check_floats (check_cmp (remove_nan Float.equal) (fun l r -> cmp 0 l r));
        Float64.check_floats (check_cmp (remove_nan (fun l r -> Float.compare l r = -1)) (fun l r -> cmp 1 l r));
        Float64.check_floats (check_cmp (remove_nan (fun l r -> Float.compare l r <= 0)) (fun l r -> cmp 2 l r));
        Float64.check_floats (check_cmp (fun l r -> (Float.is_nan l) || (Float.is_nan r)) (fun l r -> cmp 3 l r));
        Float64.check_floats (check_cmp (fun l r -> not (Float.equal l r) || (Float.is_nan l && Float.is_nan r)) (fun l r -> cmp 4 l r));
        Float64.check_floats (check_cmp (add_nan (fun l r -> Float.compare l r >= 0)) (fun l r -> cmp 5 l r));
        Float64.check_floats (check_cmp (add_nan (fun l r -> Float.compare l r = 1)) (fun l r -> cmp 6 l r));
        Float64.check_floats (check_cmp (fun l r -> not (Float.is_nan l) && (not (Float.is_nan r))) (fun l r -> cmp 7 l r))
    ;;

    external add : t -> t -> t = "" "caml_sse2_float64x2_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "" "caml_sse2_float64x2_sub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external mul : t -> t -> t = "" "caml_sse2_float64x2_mul"
        [@@noalloc] [@@unboxed] [@@builtin]
    external div : t -> t -> t = "" "caml_sse2_float64x2_div"
        [@@noalloc] [@@unboxed] [@@builtin]
    external max : t -> t -> t = "" "caml_sse2_float64x2_max"
        [@@noalloc] [@@unboxed] [@@builtin]
    external min : t -> t -> t = "" "caml_sse2_float64x2_min"
        [@@noalloc] [@@unboxed] [@@builtin]

    let check_binop scalar vector f0 f1 =
        failmsg := (fun () -> Printf.printf "%f | %f\n%!" f0 f1);
        let r0 = scalar f0 f1 in
        let r1 = scalar f1 f0 in
        let expect = to_float64x2 r0 r1 in
        let v1 = to_float64x2 f0 f1 in
        let v2 = to_float64x2 f1 f0 in
        let result = vector v1 v2 in
        eq (float64x2_low_int64 result) (float64x2_high_int64 result)
           (float64x2_low_int64 expect) (float64x2_high_int64 expect)
    ;;
    let () =
        let preserve_nan p l r = if Float.is_nan r then r else if Float.is_nan l then r else p l r in
        let preserve_zero p l r = if (l = 0.0 || l = -0.0) && (r = 0.0 || r = -0.0) then r else p l r in
        Float64.check_floats (check_binop Float.add add);
        Float64.check_floats (check_binop Float.sub sub);
        Float64.check_floats (check_binop Float.mul mul);
        Float64.check_floats (check_binop Float.div div);
        Float64.check_floats (check_binop (Float.max |> preserve_nan |> preserve_zero) max);
        Float64.check_floats (check_binop (Float.min |> preserve_nan |> preserve_zero) min)
    ;;

    external cvt_int32x4 : t -> int32x4 = "" "caml_sse2_cvt_float64x2_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cvt_float32x4 : t -> float32x4 = "" "caml_sse2_cvt_float64x2_float32x4"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "cvti32 %f | %f\n%!" f0 f1);
            let i0 = Int32.of_float (Float.round f0) |> Int64.of_int32 |> Int64.logand 0xffffffffL in
            let i1 = Int32.of_float (Float.round f1) |> Int64.of_int32 |> Int64.logand 0xffffffffL in
            let ii = Int64.(logor (shift_left i1 32) i0) in
            let iv = int32x4_of_int64s ii 0L in
            let fv = to_float64x2 f0 f1 in
            let res = cvt_int32x4 fv in
            eq (int32x4_low_int64 res) (int32x4_high_int64 res)
               (int32x4_low_int64 iv) (int32x4_high_int64 iv)
        );
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "cvtf32 %f %f\n%!" f0 f1);
            let i0 = Int32.bits_of_float f0 |> Int64.of_int32 |> Int64.logand 0xffffffffL in
            let i1 = Int32.bits_of_float f1 |> Int64.of_int32 |> Int64.logand 0xffffffffL in
            let ii = Int64.(logor (shift_left i1 32) i0) in
            let iv = float32x4_of_int64s ii 0L in
            let fv = to_float64x2 f0 f1 in
            let res = cvt_float32x4 fv in
            eq (float32x4_low_int64 res) (float32x4_high_int64 res)
               (float32x4_low_int64 iv) (float32x4_high_int64 iv)
        )
    ;;

    external addsub : t -> t -> t = "" "caml_sse3_float64x2_addsub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hadd : t -> t -> t = "" "caml_sse3_float64x2_hadd"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hsub : t -> t -> t = "" "caml_sse3_float64x2_hsub"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" f0 f1);
            let fv0 = to_float64x2 f0 f0 in
            let fv1 = to_float64x2 f1 f1 in
            let result = addsub fv0 fv1 in
            let expect = to_float64x2 (f0 -. f1) (f0 +. f1) in
            eq (float64x2_low_int64 result) (float64x2_high_int64 result)
               (float64x2_low_int64 expect) (float64x2_high_int64 expect)
        );
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" f0 f1);
            let fv0 = to_float64x2 f0 f0 in
            let fv1 = to_float64x2 f1 f1 in
            let result = hadd fv0 fv1 in
            let expect = to_float64x2 (f0 +. f0) (f1 +. f1) in
            eq (float64x2_low_int64 result) (float64x2_high_int64 result)
               (float64x2_low_int64 expect) (float64x2_high_int64 expect)
        );
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f | %f\n%!" f0 f1);
            let fv0 = to_float64x2 f0 f1 in
            let fv1 = to_float64x2 f1 f0 in
            let result = hsub fv0 fv1 in
            let expect = to_float64x2 (f0 -. f1) (f1 -. f0) in
            eq (float64x2_low_int64 result) (float64x2_high_int64 result)
               (float64x2_low_int64 expect) (float64x2_high_int64 expect)
        );
    ;;

    external dp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "caml_sse41_float64x2_dp"
        [@@noalloc] [@@builtin]
    external round : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "caml_sse41_float64x2_round"
        [@@noalloc] [@@builtin]

    let () =
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "%f dp %f\n%!" f0 f1);
            let fv0 = to_float64x2 f0 f1 in
            let fv1 = to_float64x2 f1 f0 in
            let result = dp 0b0011_0001 fv0 fv1 in
            let expect = to_float64x2 (f0 *. f1 +. f1 *. f0) 0.0 in
            eq (float64x2_low_int64 result) (float64x2_high_int64 result)
            (float64x2_low_int64 expect) (float64x2_high_int64 expect)
        );
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "roundf64 %f %f\n%!" f0 f1);
            let fv = to_float64x2 f0 f1 in
            let result = round 0 fv in
            let expect = to_float64x2 (Float64.round f0) (Float64.round f1) in
            eq (float64x2_low_int64 result) (float64x2_high_int64 result)
            (float64x2_low_int64 expect) (float64x2_high_int64 expect)
        )
    ;;
end

module Int64x2 = struct

    type t = int64x2

    (* Creation / Destruction *)

    external low_of : int64 -> t = "" "caml_int64x2_low_of_int64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> int64 = "" "caml_int64x2_low_to_int64"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v1 = low_of 1L in
        let v2 = low_of 2L in
        let i1 = int64x2_low_int64 v1 in
        let i2 = int64x2_low_int64 v2 in
        eq i1 i2 1L 2L;
        let i1 = low_to v1 in
        let i2 = low_to v2 in
        eq i1 i2 1L 2L
    ;;

    (* Math *)

    external add : t -> t -> t = "" "caml_sse2_int64x2_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "" "caml_sse2_int64x2_sub"
        [@@noalloc] [@@unboxed] [@@builtin]

    let check_binop scalar vector i0 i1 =
        failmsg := (fun () -> Printf.printf "%016Lx | %016Lx\n%!" i0 i1);
        let r0 = scalar i0 i1 in
        let r1 = scalar i1 i0 in
        let expect = int64x2_of_int64s r0 r1 in
        let v1 = int64x2_of_int64s i0 i1 in
        let v2 = int64x2_of_int64s i1 i0 in
        let result = vector v1 v2 in
        eq (int64x2_low_int64 result) (int64x2_high_int64 result)
           (int64x2_low_int64 expect) (int64x2_high_int64 expect)
    ;;
    let () =
        Int64s.check_ints (check_binop Int64.add add);
        Int64s.check_ints (check_binop Int64.sub sub)
    ;;

    (* TODO
    caml_sse2_int64x2_sll
    caml_sse2_int64x2_srl
    caml_sse2_int64x2_slli
    caml_sse2_int64x2_srli
    caml_sse41_int64x2_cmpeq
    caml_sse41_int64x2_extract
    caml_sse41_int64x2_insert
    caml_sse42_int64x2_cmpgt *)
end

module Int32x4 = struct

    type t = int32x4

    (* Creation / Destruction *)

    external low_of : int32 -> t = "" "caml_int32x4_low_of_int32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> int32 = "" "caml_int32x4_low_to_int32"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v1 = low_of 1l in
        let v2 = low_of 2l in
        let i1 = int32x4_low_int64 v1 |> Int64.logand 0xffffffffL in
        let i2 = int32x4_low_int64 v2 |> Int64.logand 0xffffffffL in
        eq i1 i2 1L 2L;
        let i1 = low_to v1 in
        let i2 = low_to v2 in
        eql i1 i2 1l 2l
    ;;

    (* TODO
    caml_sse2_int32x4_add
    caml_sse2_int32x4_sub
    caml_sse2_int32x4_cmpeq
    caml_sse2_int32x4_cmpgt
    caml_sse2_cvt_int32x4_float64x2
    caml_sse2_cvt_int32x4_float32x4
    caml_sse2_int32x4_sll
    caml_sse2_int32x4_srl
    caml_sse2_int32x4_sra
    caml_sse2_int32x4_slli
    caml_sse2_int32x4_srli
    caml_sse2_int32x4_srai
    caml_ssse3_int32x4_abs
    caml_ssse3_int32x4_hadd
    caml_ssse3_int32x4_hsub
    caml_ssse3_int32x4_mulsign
    caml_sse41_cvtsx_int32x4_int64x2
    caml_sse41_cvtzx_int32x4_int64x2
    caml_sse41_int32x4_extract
    caml_sse41_int32x4_insert
    caml_sse41_int32x4_max
    caml_sse41_int32x4_max_unsigned
    caml_sse41_int32x4_min
    caml_sse41_int32x4_min_unsigned *)
end

module Int16x8 = struct

    type t = int16x8

    (* Creation / Destruction *)

    external low_of : (int [@untagged]) -> (t [@unboxed]) = "" "caml_int16x8_low_of_int"
        [@@noalloc] [@@builtin]
    external low_to : (t [@unboxed]) -> (int [@untagged]) = "" "caml_int16x8_low_to_int"
        [@@noalloc] [@@builtin]

    let () =
        let v1 = low_of 1 in
        let v2 = low_of 2 in
        let i1 = int16x8_low_int64 v1 |> Int64.logand 0xffffL in
        let i2 = int16x8_low_int64 v2 |> Int64.logand 0xffffL in
        eq i1 i2 1L 2L;
        let i1 = low_to v1 in
        let i2 = low_to v2 in
        eqi i1 i2 1 2
    ;;

    (* TODO
    caml_sse2_int16x8_add
    caml_sse2_int16x8_add_saturating
    caml_sse2_int16x8_add_saturating_unsigned
    caml_sse2_int16x8_sub
    caml_sse2_int16x8_sub_saturating
    caml_sse2_int16x8_sub_saturating_unsigned
    caml_sse2_int16x8_max
    caml_sse2_int16x8_min
    caml_sse2_int16x8_cmpeq
    caml_sse2_int16x8_cmpgt
    caml_sse2_int16x8_sll
    caml_sse2_int16x8_srl
    caml_sse2_int16x8_sra
    caml_sse2_int16x8_slli
    caml_sse2_int16x8_srli
    caml_sse2_int16x8_srai
    caml_ssse3_int16x8_abs
    caml_ssse3_int16x8_hadd
    caml_ssse3_int16x8_hadd_saturating
    caml_ssse3_int16x8_hsub
    caml_ssse3_int16x8_hsub_saturating
    caml_ssse3_int16x8_mulsign
    caml_sse41_cvtsx_int16x8_int32x4
    caml_sse41_cvtsx_int16x8_int64x2
    caml_sse41_cvtzx_int16x8_int32x4
    caml_sse41_cvtzx_int16x8_int64x2
    caml_sse41_int16x8_extract
    caml_sse41_int16x8_insert
    caml_sse41_int16x8_max_unsigned
    caml_sse41_int16x8_min_unsigned *)
end

module Int8x16 = struct

    type t = int8x16

    (* Creation / Destruction *)

    external low_of : (int [@untagged]) -> (t [@unboxed]) = "" "caml_int8x16_low_of_int"
        [@@noalloc] [@@builtin]
    external low_to : (t [@unboxed]) -> (int [@untagged]) = "" "caml_int8x16_low_to_int"
        [@@noalloc] [@@builtin]

    let () =
        let v1 = low_of 1 in
        let v2 = low_of 2 in
        let i1 = int8x16_low_int64 v1 |> Int64.logand 0xffL in
        let i2 = int8x16_low_int64 v2 |> Int64.logand 0xffL in
        eq i1 i2 1L 2L;
        let i1 = low_to v1 in
        let i2 = low_to v2 in
        eqi i1 i2 1 2
    ;;

    (* TODO
    caml_sse2_int8x16_add
    caml_sse2_int8x16_add_saturating
    caml_sse2_int8x16_add_saturating_unsigned
    caml_sse2_int8x16_sub
    caml_sse2_int8x16_sub_saturating
    caml_sse2_int8x16_sub_saturating_unsigned
    caml_sse2_int8x16_max_unsigned
    caml_sse2_int8x16_min_unsigned
    caml_sse2_int8x16_cmpeq
    caml_sse2_int8x16_cmpgt
    caml_ssse3_int8x16_abs
    caml_ssse3_int8x16_mulsign
    caml_sse41_cvtsx_int8x16_int16x8
    caml_sse41_cvtsx_int8x16_int32x4
    caml_sse41_cvtsx_int8x16_int64x2
    caml_sse41_cvtzx_int8x16_int16x2
    caml_sse41_cvtzx_int8x16_int32x4
    caml_sse41_cvtzx_int8x16_int64x2
    caml_sse41_int8x16_extract
    caml_sse41_int8x16_insert
    caml_sse41_int8x16_max
    caml_sse41_int8x16_min *)
end

module SSE_Util = struct

    type t = int32x4

    external high_64_to_low_64 : t -> t -> t = "" "caml_sse_vec128_high_64_to_low_64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_64_to_high_64 : t -> t -> t = "" "caml_sse_vec128_low_64_to_high_64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_high_32 : t -> t -> t = "" "caml_sse_vec128_interleave_high_32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_low_32 : t -> t -> t = "" "caml_sse_vec128_interleave_low_32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external shuffle_32 : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "caml_sse_vec128_shuffle_32"
        [@@noalloc] [@@builtin]

    let make a b c d =
        Float32.to_float32x4 a b c d |> Vector_casts.int32x4_of_float32x4
    ;;

    let zero = 0l
    let one = 1l
    let low = int32x4_low_int64
    let high = int32x4_high_int64

    let () =
        let eql = eq in
        let open Float32 in
        let _0011 = make zero zero one one in
        let _1100 = make one one zero zero in
        let _0101 = make zero one zero one in
        let _1010 = make one zero one zero in
        let _1111 = make one one one one in
        let _0000 = make zero zero zero zero in
        let res = high_64_to_low_64 _1100 _1100 in
        eql (low _0000) (high _0000)
            (low res) (high res);
        let res = low_64_to_high_64 _1100 _1100 in
        eql (low _1111) (high _1111)
            (low res) (high res);
        let res = interleave_high_32 _1100 _0011 in
        eql (low _0101) (high _0101)
            (low res) (high res);
        let res = interleave_low_32 _1100 _0011 in
        eql (low _1010) (high _1010)
            (low res) (high res)
    ;;
    let () =
        let eql = eq in
        let open Float32 in
        let two = add one one in
        let three = add two one in
        let _0123 = make zero one two three in
        let _0000 = make zero zero zero zero in
        let _1111 = make one one one one in
        let _2222 = make two two two two in
        let _3333 = make three three three three in
        let _r0000 = shuffle_32 0b00000000 _0123 _0123 in
        let _r1111 = shuffle_32 0b01010101 _0123 _0123 in
        let _r2222 = shuffle_32 0b10101010 _0123 _0123 in
        let _r3333 = shuffle_32 0b11111111 _0123 _0123 in
        eql (low _0000) (high _0000)
            (low _r0000) (high _r0000);
        eql (low _1111) (high _1111)
            (low _r1111) (high _r1111);
        eql (low _2222) (high _2222)
            (low _r2222) (high _r2222);
        eql (low _3333) (high _3333)
            (low _r3333) (high _r3333);
        let _1010 = make one zero one zero in
        let _0101 = make zero one zero one in
        let _r0101 = shuffle_32 0b01000100 _0123 _0123 in
        let _r1010 = shuffle_32 0b00010001 _0123 _0123 in
        let _r1111 = shuffle_32 0b11010010 _r1010 _r0101 in
        eql (low _0101) (high _0101)
            (low _r0101) (high _r0101);
        eql (low _1010) (high _1010)
            (low _r1010) (high _r1010);
        eql (low _1111) (high _1111)
            (low _r1111) (high _r1111);
    ;;

    (* TODO
       caml_sse_vec128_movemask_32 *)
end

module SSE2_Util = struct

    (* TODO
    caml_sse2_vec128_and
    caml_sse2_vec128_andnot
    caml_sse2_vec128_or
    caml_sse2_vec128_xor
    caml_sse2_vec128_movemask_8
    caml_sse2_vec128_movemask_64
    caml_sse2_vec128_shift_left_bytes
    caml_sse2_vec128_shift_right_bytes
    caml_sse2_vec128_shuffle_64
    caml_sse2_vec128_shuffle_high_16
    caml_sse2_vec128_shuffle_low_16
    caml_sse2_vec128_interleave_high_8
    caml_sse2_vec128_interleave_low_8
    caml_sse2_vec128_interleave_high_16
    caml_sse2_vec128_interleave_low_16
    caml_sse2_vec128_interleave_high_64
    caml_sse2_vec128_interleave_low_64 *)
end

module SSE3_Util = struct

    (* TODO
    caml_sse3_vec128_dup_low_64
    caml_sse3_vec128_dup_odd_32
    caml_sse3_vec128_dup_even_32 *)
end

module SSSE3_Util = struct

    (* TODO
       caml_ssse3_vec128_shuffle_8 *)
end

module SSE41_Util = struct

    (* TODO
    caml_sse41_vec128_blend_16
    caml_sse41_vec128_blend_32
    caml_sse41_vec128_blend_64
    caml_sse41_vec128_blendv_8
    caml_sse41_vec128_blendv_32
    caml_sse41_vec128_blendv_64 *)
end

module SSE42_String = struct

    (* TODO
    caml_sse42_vec128_cmpestri
    caml_sse42_vec128_cmpestrm
    caml_sse42_vec128_cmpistri
    caml_sse42_vec128_cmpistrm *)
end
