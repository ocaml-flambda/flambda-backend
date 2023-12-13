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

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int32x4_of_int64s : int64 -> int64 -> int32x4 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int32x4_low_int64 : int32x4 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int32x4_high_int64 : int32x4 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int16x8_of_int64s : int64 -> int64 -> int16x8 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int16x8_low_int64 : int16x8 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int16x8_high_int64 : int16x8 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int8x16_of_int64s : int64 -> int64 -> int8x16 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int8x16_low_int64 : int8x16 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int8x16_high_int64 : int8x16 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

external float32x4_of_int64s : int64 -> int64 -> float32x4 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external float32x4_low_int64 : float32x4 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float32x4_high_int64 : float32x4 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

external float64x2_of_int64s : int64 -> int64 -> float64x2 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external float64x2_low_int64 : float64x2 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float64x2_high_int64 : float64x2 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

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

    external int64x2_of_int32x4 : int32x4 -> int64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_int16x8 : int16x8 -> int64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_int8x16 : int8x16 -> int64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_float32x4 : float32x4 -> int64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_float64x2 : float64x2 -> int64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
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

    external int32x4_of_int64x2 : int64x2 -> int32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_int16x8 : int16x8 -> int32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_int8x16 : int8x16 -> int32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_float32x4 : float32x4 -> int32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_float64x2 : float64x2 -> int32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
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

    external int16x8_of_int64x2 : int64x2 -> int16x8 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_int32x4 : int32x4 -> int16x8 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_int8x16 : int8x16 -> int16x8 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_float32x4 : float32x4 -> int16x8 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_float64x2 : float64x2 -> int16x8 = "caml_vec128_unreachable" "caml_vec128_cast"
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

    external int8x16_of_int64x2 : int64x2 -> int8x16 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_int32x4 : int32x4 -> int8x16 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_int16x8 : int16x8 -> int8x16 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_float32x4 : float32x4 -> int8x16 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_float64x2 : float64x2 -> int8x16 = "caml_vec128_unreachable" "caml_vec128_cast"
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

    external float32x4_of_int64x2 : int64x2 -> float32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int32x4 : int32x4 -> float32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int16x8 : int16x8 -> float32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int8x16 : int8x16 -> float32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_float64x2 : float64x2 -> float32x4 = "caml_vec128_unreachable" "caml_vec128_cast"
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

    external float64x2_of_int64x2 : int64x2 -> float64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int32x4 : int32x4 -> float64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int16x8 : int16x8 -> float64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int8x16 : int8x16 -> float64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_float32x4 : float32x4 -> float64x2 = "caml_vec128_unreachable" "caml_vec128_cast"
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

    external zero : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_zero" [@@noalloc]
    external neg_zero : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_neg_zero" [@@noalloc]
    external one : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_one" [@@noalloc]
    external neg_one : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_neg_one" [@@noalloc]
    external nan : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_nan" [@@noalloc]
    external neg_infinity : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_neg_infinity" [@@noalloc]
    external infinity : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_infinity" [@@noalloc]
    external maxv : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_maxv" [@@noalloc]
    external minv : unit -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_minv" [@@noalloc]

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

    external cvt_i32 : (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_cvt_i32" [@@noalloc]
    external round : (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_round" [@@noalloc]

    external eq : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_eq" [@@noalloc]
    external lt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_lt" [@@noalloc]
    external le : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_le" [@@noalloc]
    external neq : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_ne" [@@noalloc]
    external nle : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_nle" [@@noalloc]
    external nlt : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_nlt" [@@noalloc]
    external ord : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_ord" [@@noalloc]
    external uord : (t [@unboxed]) -> (t [@unboxed]) -> bool = "caml_vec128_unreachable" "float32_uord" [@@noalloc]

    external add : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_add" [@@noalloc]
    external sub : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_sub" [@@noalloc]
    external mul : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_mul" [@@noalloc]
    external div : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_div" [@@noalloc]
    external min : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_min" [@@noalloc]
    external max : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_max" [@@noalloc]

    external rcp : (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_rcp" [@@noalloc]
    external sqrt : (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_sqrt" [@@noalloc]
    external rsqrt : (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float32_rsqrt" [@@noalloc]

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

    external c_round : (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float64_round" [@@noalloc]
    external c_min : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float64_min" [@@noalloc]
    external c_max : (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float64_max" [@@noalloc]
    external c_sqrt : (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "float64_sqrt" [@@noalloc]

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

    module Tests = struct
        external max : t -> t -> t = "" "caml_sse2_float64_max" [@@noalloc] [@@builtin] [@@unboxed]
        external min : t -> t -> t = "" "caml_sse2_float64_min" [@@noalloc] [@@builtin] [@@unboxed]
        external sqrt : t -> t = "" "caml_sse2_float64_sqrt" [@@noalloc] [@@builtin] [@@unboxed]
        external round : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "caml_sse41_float64_round"
            [@@noalloc] [@@builtin]

        let () =
            check_floats (fun l r -> eqf (max l r) (c_max l r));
            check_floats (fun l r -> eqf (min l r) (c_min l r));
            check_floats (fun l _ -> eqf (sqrt l) (c_sqrt l));
            check_floats (fun l _ -> eqf (round 0x8 l) (c_round l))
        ;;
    end
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

module Int32s = struct

    type t = int32

    external max_unsigned : t -> t -> t = "caml_vec128_unreachable" "uint32_max" [@@noalloc] [@@unboxed]
    external min_unsigned : t -> t -> t = "caml_vec128_unreachable" "uint32_min" [@@noalloc] [@@unboxed]

    external cvt_si16 : (t [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "int32_si16" [@@noalloc]
    external cvt_su16 : (t [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "int32_su16" [@@noalloc]

    let of_int32s a b c d =
        let a = Int64.of_int32 a |> Int64.logand 0xffffffffL in
        let b = Int64.of_int32 b |> Int64.logand 0xffffffffL in
        let c = Int64.of_int32 c |> Int64.logand 0xffffffffL in
        let d = Int64.of_int32 d |> Int64.logand 0xffffffffL in
        int32x4_of_int64s (Int64.(logor (shift_left b 32) a)) (Int64.(logor (shift_left d 32) c))
    ;;

    let check_ints f =
        let open Int32 in
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
            let i0 = Random.int32 Int32.max_int in
            let i1 = Random.int32 Int32.max_int in
            f (if Random.bool () then i0 else Int32.neg i0)
              (if Random.bool () then i1 else Int32.neg i1)
        done
    ;;
end

module Int16 = struct

    type t = int

    external abs : t -> t = "caml_vec128_unreachable" "int16_abs"
        [@@noalloc] [@@untagged]
    external add : t -> t -> t = "caml_vec128_unreachable" "int16_add"
        [@@noalloc] [@@untagged]
    external sub : t -> t -> t = "caml_vec128_unreachable" "int16_sub"
        [@@noalloc] [@@untagged]
    external adds : t -> t -> t = "caml_vec128_unreachable" "int16_adds"
        [@@noalloc] [@@untagged]
    external subs : t -> t -> t = "caml_vec128_unreachable" "int16_subs"
        [@@noalloc] [@@untagged]
    external mulsign : t -> t -> t = "caml_vec128_unreachable" "int16_mulsign"
        [@@noalloc] [@@untagged]
    external addsu : t -> t -> t = "caml_vec128_unreachable" "int16_addsu"
        [@@noalloc] [@@untagged]
    external subsu : t -> t -> t = "caml_vec128_unreachable" "int16_subsu"
        [@@noalloc] [@@untagged]
    external min : t -> t -> t = "caml_vec128_unreachable" "int16_min"
        [@@noalloc] [@@untagged]
    external max : t -> t -> t = "caml_vec128_unreachable" "int16_max"
        [@@noalloc] [@@untagged]
    external minu : t -> t -> t = "caml_vec128_unreachable" "int16_minu"
        [@@noalloc] [@@untagged]
    external maxu : t -> t -> t = "caml_vec128_unreachable" "int16_maxu"
        [@@noalloc] [@@untagged]
    external cmpeq : t -> t -> t = "caml_vec128_unreachable" "int16_cmpeq"
        [@@noalloc] [@@untagged]
    external cmpgt : t -> t -> t = "caml_vec128_unreachable" "int16_cmpgt"
        [@@noalloc] [@@untagged]
    external avgu : t -> t -> t = "caml_vec128_unreachable" "int16_avgu"
        [@@noalloc] [@@untagged]
    external shift_left : t -> int -> t = "caml_vec128_unreachable" "int16_shift_left"
        [@@noalloc] [@@untagged]
    external shift_right : t -> int -> t = "caml_vec128_unreachable" "int16_shift_right"
        [@@noalloc] [@@untagged]
    external shift_right_logical : t -> int -> t = "caml_vec128_unreachable" "int16_shift_right_logical"
        [@@noalloc] [@@untagged]

    external logand : t -> t -> t = "caml_vec128_unreachable" "int16_logand"
        [@@noalloc] [@@untagged]

    external cvtsx_i32 : (t [@untagged]) -> (int32 [@unboxed]) = "caml_vec128_unreachable" "int16_sxi32" [@@noalloc]
    external cvtzx_i32 : (t [@untagged]) -> (int32 [@unboxed]) = "caml_vec128_unreachable" "int16_zxi32" [@@noalloc]
    external cvtsx_i64 : (t [@untagged]) -> (int64 [@unboxed]) = "caml_vec128_unreachable" "int16_sxi64" [@@noalloc]
    external cvtzx_i64 : (t [@untagged]) -> (int64 [@unboxed]) = "caml_vec128_unreachable" "int16_zxi64" [@@noalloc]

    external cvt_si8 : (t [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "int16_si8" [@@noalloc]
    external cvt_su8 : (t [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "int16_su8" [@@noalloc]

    let of_ints a b c d e f g h =
        let a = Int64.of_int a |> Int64.logand 0xffffL in
        let b = Int64.of_int b |> Int64.logand 0xffffL in
        let c = Int64.of_int c |> Int64.logand 0xffffL in
        let d = Int64.of_int d |> Int64.logand 0xffffL in
        let e = Int64.of_int e |> Int64.logand 0xffffL in
        let f = Int64.of_int f |> Int64.logand 0xffffL in
        let g = Int64.of_int g |> Int64.logand 0xffffL in
        let h = Int64.of_int h |> Int64.logand 0xffffL in
        let low = Int64.(logor (shift_left b 16) a) in
        let high = Int64.(logor (shift_left d 16) c) in
        let _low = Int64.(logor (shift_left high 32) low) in
        let low = Int64.(logor (shift_left f 16) e) in
        let high = Int64.(logor (shift_left h 16) g) in
        let _high = Int64.(logor (shift_left high 32) low) in
        int16x8_of_int64s _low _high
    ;;

    let max_int = 0x7fff
    let min_int = 0x8000

    let check_ints f =
        Random.set_state (Random.State.make [|1234567890|]);
        f 0 0;
        f 0 1;
        f 1 1;
        f 0 (-1);
        f (-1) (-1);
        f 1 (-1);
        f max_int 0;
        f min_int 0;
        f max_int 1;
        f min_int 1;
        f max_int max_int;
        f min_int min_int;
        f max_int min_int;
        for i = 0 to 100_000 do
            let i0 = Random.int 0x10000 in
            let i1 = Random.int 0x10000 in
            f (if Random.bool () then i0 else (-i0))
              (if Random.bool () then i1 else (-i1))
        done
    ;;
end

module Int8 = struct

    type t = int

    external abs : t -> t = "caml_vec128_unreachable" "int8_abs"
        [@@noalloc] [@@untagged]
    external add : t -> t -> t = "caml_vec128_unreachable" "int8_add"
        [@@noalloc] [@@untagged]
    external sub : t -> t -> t = "caml_vec128_unreachable" "int8_sub"
        [@@noalloc] [@@untagged]
    external adds : t -> t -> t = "caml_vec128_unreachable" "int8_adds"
        [@@noalloc] [@@untagged]
    external subs : t -> t -> t = "caml_vec128_unreachable" "int8_subs"
        [@@noalloc] [@@untagged]
    external mulsign : t -> t -> t = "caml_vec128_unreachable" "int8_mulsign"
        [@@noalloc] [@@untagged]
    external addsu : t -> t -> t = "caml_vec128_unreachable" "int8_addsu"
        [@@noalloc] [@@untagged]
    external subsu : t -> t -> t = "caml_vec128_unreachable" "int8_subsu"
        [@@noalloc] [@@untagged]
    external min : t -> t -> t = "caml_vec128_unreachable" "int8_min"
        [@@noalloc] [@@untagged]
    external max : t -> t -> t = "caml_vec128_unreachable" "int8_max"
        [@@noalloc] [@@untagged]
    external minu : t -> t -> t = "caml_vec128_unreachable" "int8_minu"
        [@@noalloc] [@@untagged]
    external maxu : t -> t -> t = "caml_vec128_unreachable" "int8_maxu"
        [@@noalloc] [@@untagged]
    external cmpeq : t -> t -> t = "caml_vec128_unreachable" "int8_cmpeq"
        [@@noalloc] [@@untagged]
    external cmpgt : t -> t -> t = "caml_vec128_unreachable" "int8_cmpgt"
        [@@noalloc] [@@untagged]

    external avgu : t -> t -> t = "caml_vec128_unreachable" "int8_avgu"
        [@@noalloc] [@@untagged]
    external diffu : t -> t -> t = "caml_vec128_unreachable" "int8_diffu"
        [@@noalloc] [@@untagged]
    external cvtzx_i16 : (t [@untagged]) -> (Int16.t [@untagged]) = "caml_vec128_unreachable" "int8_zxi16" [@@noalloc]
    external cvtsx_i16 : (t [@untagged]) -> (Int16.t [@untagged]) = "caml_vec128_unreachable" "int8_sxi16" [@@noalloc]
    external cvtsx_i32 : (t [@untagged]) -> (int32 [@unboxed]) = "caml_vec128_unreachable" "int8_sxi32" [@@noalloc]
    external cvtzx_i32 : (t [@untagged]) -> (int32 [@unboxed]) = "caml_vec128_unreachable" "int8_zxi32" [@@noalloc]
    external cvtsx_i64 : (t [@untagged]) -> (int64 [@unboxed]) = "caml_vec128_unreachable" "int8_sxi64" [@@noalloc]
    external cvtzx_i64 : (t [@untagged]) -> (int64 [@unboxed]) = "caml_vec128_unreachable" "int8_zxi64" [@@noalloc]

    let of_ints a b c d e f g h =
        let a = Int64.of_int a |> Int64.logand 0xffL in
        let b = Int64.of_int b |> Int64.logand 0xffL in
        let c = Int64.of_int c |> Int64.logand 0xffL in
        let d = Int64.of_int d |> Int64.logand 0xffL in
        let e = Int64.of_int e |> Int64.logand 0xffL in
        let f = Int64.of_int f |> Int64.logand 0xffL in
        let g = Int64.of_int g |> Int64.logand 0xffL in
        let h = Int64.of_int h |> Int64.logand 0xffL in
        let ba = Int64.(logor (shift_left b 8) a) in
        let dc = Int64.(logor (shift_left d 8) c) in
        let fe = Int64.(logor (shift_left f 8) e) in
        let hg = Int64.(logor (shift_left h 8) g) in
        let dcba = Int64.(logor (shift_left dc 16) ba) in
        let hgfe = Int64.(logor (shift_left hg 16) fe) in
        let i = Int64.(logor (shift_left hgfe 32) dcba) in
        int8x16_of_int64s i i
    ;;

    let check_ints f =
        Random.set_state (Random.State.make [|1234567890|]);
        for i = 0 to 0xff do
            for j = 0 to 0xff do
                f i j
            done
        done
    ;;
end

module Float32x4 = struct

    type t = float32x4

    (* Creation / Destruction

       These are sufficient to implement set1/set4/get1/get4 etc in the user-level library.
       At least in the initial version, we will not provide set1/set4 intrinsics that
       produce constants when given constant args. Instead we provide explicit const intrinsics. *)

    external low_of : float -> t = "caml_vec128_unreachable" "caml_float32x4_low_of_float"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> float = "caml_vec128_unreachable" "caml_float32x4_low_to_float"
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

    external cmp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (int32x4 [@unboxed]) = "caml_vec128_unreachable" "caml_sse_float32x4_cmp"
        [@@noalloc] [@@builtin]

    external movemask_32 : (int32x4 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse_vec128_movemask_32"
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

    external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_sub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external mul : t -> t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_mul"
        [@@noalloc] [@@unboxed] [@@builtin]
    external div : t -> t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_div"
        [@@noalloc] [@@unboxed] [@@builtin]
    external max : t -> t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_max"
        [@@noalloc] [@@unboxed] [@@builtin]
    external min : t -> t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_min"
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

    external rcp : t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_rcp"
        [@@noalloc] [@@unboxed] [@@builtin]
    external rsqrt : t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_rsqrt"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sqrt : t -> t = "caml_vec128_unreachable" "caml_sse_float32x4_sqrt"
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


    external cvt_int32x4 : t -> int32x4 = "caml_vec128_unreachable" "caml_sse2_cvt_float32x4_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cvt_float64x2 : t -> float64x2 = "caml_vec128_unreachable" "caml_sse2_cvt_float32x4_float64x2"
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

    external addsub : t -> t -> t = "caml_vec128_unreachable" "caml_sse3_float32x4_addsub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hadd : t -> t -> t = "caml_vec128_unreachable" "caml_sse3_float32x4_hadd"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hsub : t -> t -> t = "caml_vec128_unreachable" "caml_sse3_float32x4_hsub"
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

    external dp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_float32x4_dp"
        [@@noalloc] [@@builtin]
    external round : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_float32x4_round"
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
            let result = round 0x8 fv in
            let expect = Float32.to_float32x4 (Float32.round f0) (Float32.round f1) (Float32.round f0) (Float32.round f1) in
            eq (float32x4_low_int64 result) (float32x4_high_int64 result)
            (float32x4_low_int64 expect) (float32x4_high_int64 expect)
        )
    ;;
end

module Float64x2 = struct

    type t = float64x2

    (* Creation / Destruction *)

    external low_of : float -> t = "caml_vec128_unreachable" "caml_float64x2_low_of_float"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> float = "caml_vec128_unreachable" "caml_float64x2_low_to_float"
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

    external cmp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (int64x2 [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_float64x2_cmp"
        [@@noalloc] [@@builtin]

    external movemask_64 : (int64x2 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_64"
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

    external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_sub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external mul : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_mul"
        [@@noalloc] [@@unboxed] [@@builtin]
    external div : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_div"
        [@@noalloc] [@@unboxed] [@@builtin]
    external max : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_max"
        [@@noalloc] [@@unboxed] [@@builtin]
    external min : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_min"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sqrt : t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_sqrt"
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
        Float64.check_floats (check_binop (Float.min |> preserve_nan |> preserve_zero) min);
        Float64.check_floats (fun f0 f1 ->
            failmsg := (fun () -> Printf.printf "sqrt %f | %f\n%!" f0 f1);
            let fv = to_float64x2 f0 f1 in
            let res = sqrt fv in
            eq (float64x2_low_int64 res) (float64x2_high_int64 res)
               (Int64.bits_of_float (Float.sqrt f0)) (Int64.bits_of_float (Float.sqrt f1))
        )
    ;;

    external cvt_int32x4 : t -> int32x4 = "caml_vec128_unreachable" "caml_sse2_cvt_float64x2_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cvt_float32x4 : t -> float32x4 = "caml_vec128_unreachable" "caml_sse2_cvt_float64x2_float32x4"
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

    external addsub : t -> t -> t = "caml_vec128_unreachable" "caml_sse3_float64x2_addsub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hadd : t -> t -> t = "caml_vec128_unreachable" "caml_sse3_float64x2_hadd"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hsub : t -> t -> t = "caml_vec128_unreachable" "caml_sse3_float64x2_hsub"
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

    external dp : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_float64x2_dp"
        [@@noalloc] [@@builtin]
    external round : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_float64x2_round"
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
            let result = round 0x8 fv in
            let expect = to_float64x2 (Float64.c_round f0) (Float64.c_round f1) in
            eq (float64x2_low_int64 result) (float64x2_high_int64 result)
            (float64x2_low_int64 expect) (float64x2_high_int64 expect)
        )
    ;;
end

module Int64x2 = struct

    type t = int64x2

    (* Creation / Destruction *)

    external low_of : int64 -> t = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> int64 = "caml_vec128_unreachable" "caml_int64x2_low_to_int64"
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

    external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_sub"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cmpeq : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int64x2_cmpeq"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cmpgt : t -> t -> t = "caml_vec128_unreachable" "caml_sse42_int64x2_cmpgt"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_sll"
        [@@noalloc] [@@unboxed] [@@builtin]
    external srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_srl"
        [@@noalloc] [@@unboxed] [@@builtin]

    external slli : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int64x2_slli"
        [@@noalloc] [@@builtin]
    external srli : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int64x2_srli"
        [@@noalloc] [@@builtin]

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
        Int64s.check_ints (check_binop Int64.sub sub);
        Int64s.check_ints (check_binop (fun l r -> if Int64.equal l r then 0xffffffffffffffffL else 0L) cmpeq);
        Int64s.check_ints (check_binop (fun l r -> if Int64.compare l r = 1 then 0xffffffffffffffffL else 0L) cmpgt);
        Int64s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%016Lx << %016Lx\n%!" l r);
            let v = int64x2_of_int64s l r in
            let shift = Int64.logand r 0x3fL in
            let result = sll v (int64x2_of_int64s shift 0L) in
            let expectl = Int64.shift_left l (Int64.to_int shift) in
            let expectr = Int64.shift_left r (Int64.to_int shift) in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
               expectl expectr
        );
        Int64s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%016Lx >> %016Lx\n%!" l r);
            let v = int64x2_of_int64s l r in
            let shift = Int64.logand r 0x3fL in
            let result = srl v (int64x2_of_int64s shift 0L) in
            let expectl = Int64.shift_right_logical l (Int64.to_int shift) in
            let expectr = Int64.shift_right_logical r (Int64.to_int shift) in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
               expectl expectr
        );
        Int64s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%016Lx|%016Lx << 7\n%!" l r);
            let v = int64x2_of_int64s l r in
            let result = slli 7 v in
            let expectl = Int64.shift_left l 7 in
            let expectr = Int64.shift_left r 7 in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
               expectl expectr
        );
        Int64s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%016Lx|%016Lx >> 7\n%!" l r);
            let v = int64x2_of_int64s l r in
            let result = srli 7 v in
            let expectl = Int64.shift_right_logical l 7 in
            let expectr = Int64.shift_right_logical r 7 in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
               expectl expectr
        )
    ;;

    external extract : (int [@untagged]) -> (t [@unboxed]) -> (int64 [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_int64x2_extract"
        [@@noalloc] [@@builtin]
    external insert : (int [@untagged]) ->  (t [@unboxed]) -> (int64 [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_int64x2_insert"
        [@@noalloc] [@@builtin]

    let () =
        let v0 = low_of 0L in
        let v1 = insert 0 v0 1L in
        let v2 = insert 1 v1 2L in
        let i0 = extract 0 v0 in
        let i1 = extract 0 v1 in
        let i2 = extract 0 v2 in
        let i3 = extract 1 v2 in
        eq i0 i1 0L 1L;
        eq i2 i3 1L 2L
    ;;
end

module Int32x4 = struct

    type t = int32x4

    (* Creation / Destruction *)

    external low_of : int32 -> t = "caml_vec128_unreachable" "caml_int32x4_low_of_int32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_to : t -> int32 = "caml_vec128_unreachable" "caml_int32x4_low_to_int32"
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

    (* Math *)

    external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sub"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cmpeq : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_cmpeq"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cmpgt : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_cmpgt"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sll"
        [@@noalloc] [@@unboxed] [@@builtin]
    external srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_srl"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sra : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sra"
        [@@noalloc] [@@unboxed] [@@builtin]

    external slli : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int32x4_slli"
        [@@noalloc] [@@builtin]
    external srli : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int32x4_srli"
        [@@noalloc] [@@builtin]
    external srai : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int32x4_srai"
        [@@noalloc] [@@builtin]

    external cvt_f64 : t -> float64x2 = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_float64x2"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cvt_f32 : t -> float32x4 = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_float32x4"
        [@@noalloc] [@@unboxed] [@@builtin]

    external abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int32x4_abs"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hadd : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int32x4_hadd"
        [@@noalloc] [@@unboxed] [@@builtin]
    external hsub : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int32x4_hsub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external mulsign : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int32x4_mulsign"
        [@@noalloc] [@@unboxed] [@@builtin]

    external max : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int32x4_max"
        [@@noalloc] [@@unboxed] [@@builtin]
    external max_unsigned : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int32x4_max_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]
    external min : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int32x4_min"
        [@@noalloc] [@@unboxed] [@@builtin]
    external min_unsigned : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int32x4_min_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtsx_i64 : t -> int64x2 = "caml_vec128_unreachable" "caml_sse41_cvtsx_int32x4_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]
    external cvtzx_i64 : t -> int64x2 = "caml_vec128_unreachable" "caml_sse41_cvtzx_int32x4_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvt_si16 : t -> t -> int16x8 = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_int16x8_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvt_su16 : t -> t -> int16x8 = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_int16x8_saturating_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    let check_binop scalar vector i0 i1 =
        failmsg := (fun () -> Printf.printf "%08lx | %08lx\n%!" i0 i1);
        let r0 = scalar i0 i1 in
        let r1 = scalar i1 i0 in
        let expect = Int32s.of_int32s r0 r1 r0 r1 in
        let v1 = Int32s.of_int32s i0 i1 i0 i1 in
        let v2 = Int32s.of_int32s i1 i0 i1 i0 in
        let result = vector v1 v2 in
        eq (int32x4_low_int64 result) (int32x4_high_int64 result)
           (int32x4_low_int64 expect) (int32x4_high_int64 expect)
    ;;
    let () =
        Int32s.check_ints (check_binop Int32.add add);
        Int32s.check_ints (check_binop Int32.sub sub);
        Int32s.check_ints (check_binop (fun l r -> if Int32.equal l r then 0xffffffffl else 0l) cmpeq);
        Int32s.check_ints (check_binop (fun l r -> if Int32.compare l r = 1 then 0xffffffffl else 0l) cmpgt);
        Int32s.check_ints (check_binop Int32.max max);
        Int32s.check_ints (check_binop Int32.min min);
        Int32s.check_ints (check_binop Int32s.max_unsigned max_unsigned);
        Int32s.check_ints (check_binop Int32s.min_unsigned min_unsigned);
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx << %08lx\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let shift = Int32.logand r 0x1fl in
            let result = sll v (Int32s.of_int32s shift 0l 0l 0l) in
            let expectl = Int32.shift_left l (Int32.to_int shift) in
            let expectr = Int32.shift_left r (Int32.to_int shift) in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx >> %08lx\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let shift = Int32.logand r 0x1fl in
            let result = srl v (Int32s.of_int32s shift 0l 0l 0l) in
            let expectl = Int32.shift_right_logical l (Int32.to_int shift) in
            let expectr = Int32.shift_right_logical r (Int32.to_int shift) in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx >>a %08lx\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let shift = Int32.logand r 0x1fl in
            let result = sra v (Int32s.of_int32s shift 0l 0l 0l) in
            let expectl = Int32.shift_right l (Int32.to_int shift) in
            let expectr = Int32.shift_right r (Int32.to_int shift) in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx << 7\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let result = slli 7 v in
            let expectl = Int32.shift_left l 7 in
            let expectr = Int32.shift_left r 7 in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
               (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx >> 7\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let result = srli 7 v in
            let expectl = Int32.shift_right_logical l 7 in
            let expectr = Int32.shift_right_logical r 7 in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
               (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx >>a 7\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let result = srai 7 v in
            let expectl = Int32.shift_right l 7 in
            let expectr = Int32.shift_right r 7 in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
               (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx cvt_f32\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let result = cvt_f32 v in
            let expectl = Int32.to_float l |> Int32.bits_of_float in
            let expectr = Int32.to_float r |> Int32.bits_of_float in
            let expect = Float32.to_float32x4 expectl expectr expectl expectr in
            eq (float32x4_low_int64 result) (float32x4_high_int64 result)
            (float32x4_low_int64 expect) (float32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx cvt_f64\n%!" l r);
            let v = Int32s.of_int32s l r 0l 0l in
            let result = cvt_f64 v in
            let expectl = Int32.to_float l |> Int64.bits_of_float in
            let expectr = Int32.to_float r |> Int64.bits_of_float in
            eq (float64x2_low_int64 result) (float64x2_high_int64 result)
            expectl expectr
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx abs\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let result = abs v in
            let expectl = Int32.abs l in
            let expectr = Int32.abs r in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx hadd\n%!" l r);
            let v0 = Int32s.of_int32s l l r r in
            let v1 = Int32s.of_int32s r r l l in
            let result = hadd v0 v1 in
            let expect = Int32s.of_int32s (Int32.add l l) (Int32.add r r) (Int32.add r r) (Int32.add l l) in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx hsub\n%!" l r);
            let v0 = Int32s.of_int32s l r r l in
            let v1 = Int32s.of_int32s r l l r in
            let result = hsub v0 v1 in
            let expect = Int32s.of_int32s (Int32.sub l r) (Int32.sub r l) (Int32.sub r l) (Int32.sub l r) in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx mulsign\n%!" l r);
            let v0 = Int32s.of_int32s l l r r in
            let v1 = Int32s.of_int32s l r l r in
            let result = mulsign v0 v1 in
            let mulsign x y = Int32.mul (Int32.compare y 0l |> Int32.of_int) x in
            let expect = Int32s.of_int32s (mulsign l l) (mulsign l r) (mulsign r l) (mulsign r r) in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx cvt_sx_i64\n%!" l r);
            let v = Int32s.of_int32s l r 0l 0l in
            let result = cvtsx_i64 v in
            let expectl = Int64.of_int32 l in
            let expectr = Int64.of_int32 r in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
            expectl expectr
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx cvt_zx_i64\n%!" l r);
            let v = Int32s.of_int32s l r 0l 0l in
            let result = cvtzx_i64 v in
            let expectl = Int64.of_int32 l |> Int64.logand 0xffffffffL in
            let expectr = Int64.of_int32 r |> Int64.logand 0xffffffffL in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
            expectl expectr
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx cvt_si16\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let result = cvt_si16 v v in
            let expectl = Int32s.cvt_si16 l in
            let expectr = Int32s.cvt_si16 r in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int32s.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08lx|%08lx cvt_su16\n%!" l r);
            let v = Int32s.of_int32s l r l r in
            let result = cvt_su16 v v in
            let expectl = Int32s.cvt_su16 l in
            let expectr = Int32s.cvt_su16 r in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
    ;;

    external extract : (int [@untagged]) -> (t [@unboxed]) -> (int32 [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_int32x4_extract"
        [@@noalloc] [@@builtin]
    external insert : (int [@untagged]) ->  (t [@unboxed]) -> (int32 [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_int32x4_insert"
        [@@noalloc] [@@builtin]

    let () =
        let v0 = low_of 0l in
        let v1 = insert 0 v0 1l in
        let v2 = insert 1 v1 2l in
        let v3 = insert 2 v2 3l in
        let v4 = insert 3 v3 4l in
        let i0 = extract 0 v0 in
        let i1 = extract 0 v1 in
        let i2 = extract 0 v2 in
        let i3 = extract 1 v2 in
        let i4 = extract 0 v3 in
        let i5 = extract 1 v3 in
        let i6 = extract 2 v3 in
        let i7 = extract 0 v4 in
        let i8 = extract 1 v4 in
        let i9 = extract 2 v4 in
        let i10 = extract 3 v4 in
        eql i0 i1 0l 1l;
        eql i2 i3 1l 2l;
        eql i4 i5 1l 2l;
        eql i6 i7 3l 1l;
        eql i8 i9 2l 3l;
        eql i10 0l 4l 0l;
    ;;
end

module Int16x8 = struct

    type t = int16x8

    (* Creation / Destruction *)

    external low_of : (int [@untagged]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_int16x8_low_of_int"
        [@@noalloc] [@@builtin]
    external low_to : (t [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_int16x8_low_to_int"
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

    external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_add"
        [@@noalloc] [@@unboxed] [@@builtin]

    external add_saturating : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_add_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external add_saturating_unsigned : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_add_saturating_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sub"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sub_saturating : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sub_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sub_saturating_unsigned : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sub_saturating_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external max : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_max"
        [@@noalloc] [@@unboxed] [@@builtin]

    external min : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_min"
        [@@noalloc] [@@unboxed] [@@builtin]

    external maxu : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int16x8_max_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external minu : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int16x8_min_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cmpeq : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_cmpeq"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cmpgt : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_cmpgt"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvt_si8 : t -> t -> int8x16 = "caml_vec128_unreachable" "caml_sse2_cvt_int16x8_int8x16_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvt_su8 : t -> t -> int8x16 = "caml_vec128_unreachable" "caml_sse2_cvt_int16x8_int8x16_saturating_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtsx_i32 : t -> int32x4 = "caml_vec128_unreachable" "caml_sse41_cvtsx_int16x8_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtsx_i64 : t -> int64x2 = "caml_vec128_unreachable" "caml_sse41_cvtsx_int16x8_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtzx_i32 : t -> int32x4 = "caml_vec128_unreachable" "caml_sse41_cvtzx_int16x8_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtzx_i64 : t -> int64x2 = "caml_vec128_unreachable" "caml_sse41_cvtzx_int16x8_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]

    external abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_abs"
        [@@noalloc] [@@unboxed] [@@builtin]

    external hadd : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_hadd"
        [@@noalloc] [@@unboxed] [@@builtin]

    external hadd_saturating : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_hadd_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external hsub : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub"
        [@@noalloc] [@@unboxed] [@@builtin]

    external hsub_saturating : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external mulsign : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_mulsign"
        [@@noalloc] [@@unboxed] [@@builtin]

    external avgu : t -> t -> t  = "caml_vec128_unreachable" "caml_sse2_int16x8_avg_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external minposu : t -> t = "caml_vec128_unreachable" "caml_sse41_int16x8_minpos_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    let check_binop scalar vector i0 i1 =
        failmsg := (fun () -> Printf.printf "%04x | %04x\n%!" i0 i1);
        let r0 = scalar i0 i1 in
        let r1 = scalar i1 i0 in
        let expect = Int16.of_ints r0 r1 r0 r1 r0 r1 r0 r1 in
        let v1 = Int16.of_ints i0 i1 i0 i1 i0 i1 i0 i1 in
        let v2 = Int16.of_ints i1 i0 i1 i0 i1 i0 i1 i0 in
        let result = vector v1 v2 in
        eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
    ;;
    let () =
        Int16.check_ints (check_binop Int16.add add);
        Int16.check_ints (check_binop Int16.sub sub);
        Int16.check_ints (check_binop Int16.adds add_saturating);
        Int16.check_ints (check_binop Int16.subs sub_saturating);
        Int16.check_ints (check_binop Int16.addsu add_saturating_unsigned);
        Int16.check_ints (check_binop Int16.subsu sub_saturating_unsigned);
        Int16.check_ints (check_binop Int16.max max);
        Int16.check_ints (check_binop Int16.min min);
        Int16.check_ints (check_binop Int16.maxu maxu);
        Int16.check_ints (check_binop Int16.minu minu);
        Int16.check_ints (check_binop Int16.cmpeq cmpeq);
        Int16.check_ints (check_binop Int16.cmpgt cmpgt);
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x cvt_sx_i64\n%!" l r);
            let v = Int16.of_ints l r 0 0 0 0 0 0 in
            let result = cvtsx_i64 v in
            let expectl = Int16.cvtsx_i64 l in
            let expectr = Int16.cvtsx_i64 r in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
            expectl expectr
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x cvt_zx_i64\n%!" l r);
            let v = Int16.of_ints l r 0 0 0 0 0 0 in
            let result = cvtzx_i64 v in
            let expectl = Int16.cvtzx_i64 l in
            let expectr = Int16.cvtzx_i64 r in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
            expectl expectr
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x cvt_si8\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let result = cvt_si8 v v in
            let expectl = Int16.cvt_si8 l in
            let expectr = Int16.cvt_si8 r in
            let expect = Int8.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int8x16_low_int64 result) (int8x16_high_int64 result)
            (int8x16_low_int64 expect) (int8x16_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x cvt_su8\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let result = cvt_su8 v v in
            let expectl = Int16.cvt_su8 l in
            let expectr = Int16.cvt_su8 r in
            let expect = Int8.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int8x16_low_int64 result) (int8x16_high_int64 result)
            (int8x16_low_int64 expect) (int8x16_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x cvt_sx_i32\n%!" l r);
            let v = Int16.of_ints l r l r 0 0 0 0 in
            let result = cvtsx_i32 v in
            let expectl = Int16.cvtsx_i32 l in
            let expectr = Int16.cvtsx_i32 r in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x cvt_zx_i32\n%!" l r);
            let v = Int16.of_ints l r l r 0 0 0 0 in
            let result = cvtzx_i32 v in
            let expectl = Int16.cvtzx_i32 l in
            let expectr = Int16.cvtzx_i32 r in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x abs\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let result = abs v in
            let expectl = Int16.abs l in
            let expectr = Int16.abs r in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x hadd\n%!" l r);
            let v0 = Int16.of_ints l l r r l l r r in
            let v1 = Int16.of_ints r r l l r r l l in
            let result = hadd v0 v1 in
            let expect = Int16.of_ints (Int16.add l l) (Int16.add r r) (Int16.add l l) (Int16.add r r) (Int16.add r r) (Int16.add l l) (Int16.add r r) (Int16.add l l) in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x hsub\n%!" l r);
            let v0 = Int16.of_ints l l r r l l r r in
            let v1 = Int16.of_ints r r l l r r l l in
            let result = hsub v0 v1 in
            let expect = Int16.of_ints (Int16.sub l l) (Int16.sub r r) (Int16.sub l l) (Int16.sub r r) (Int16.sub r r) (Int16.sub l l) (Int16.sub r r) (Int16.sub l l) in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x hadds\n%!" l r);
            let v0 = Int16.of_ints l l r r l l r r in
            let v1 = Int16.of_ints r r l l r r l l in
            let result = hadd_saturating v0 v1 in
            let expect = Int16.of_ints (Int16.adds l l) (Int16.adds r r) (Int16.adds l l) (Int16.adds r r) (Int16.adds r r) (Int16.adds l l) (Int16.adds r r) (Int16.adds l l) in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x hsubs\n%!" l r);
            let v0 = Int16.of_ints l l r r l l r r in
            let v1 = Int16.of_ints r r l l r r l l in
            let result = hsub_saturating v0 v1 in
            let expect = Int16.of_ints (Int16.subs l l) (Int16.subs r r) (Int16.subs l l) (Int16.subs r r) (Int16.subs r r) (Int16.subs l l) (Int16.subs r r) (Int16.subs l l) in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x mulsign\n%!" l r);
            let v0 = Int16.of_ints l l r r l l r r in
            let v1 = Int16.of_ints l r l r l r l r in
            let result = mulsign v0 v1 in
            let mulsign x y = Int16.mulsign x y in
            let expect = Int16.of_ints (mulsign l l) (mulsign l r) (mulsign r l) (mulsign r r) (mulsign l l) (mulsign l r) (mulsign r l) (mulsign r r) in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x avgu\n%!" l r);
            let v0 = Int16.of_ints l l r r l l r r in
            let v1 = Int16.of_ints l r l r l r l r in
            let result = avgu v0 v1 in
            let lr = Int16.avgu l r in
            let expect = Int16.of_ints l lr lr r l lr lr r in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%04x|%04x minposu\n%!" l r);
            let v0 = Int16.of_ints l r l r l r l r in
            let result = minposu v0 in
            let min_v = Int16.minu l r in
            let idx = if min_v = l then 0 else 1 in
            let expect = Int64.(logor (shift_left (of_int idx |> logand 0x3L) 16) (of_int min_v |> logand 0xffffL)) in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            expect 0L
        );
    ;;

    external extract : (int [@untagged]) -> (t [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse41_int16x8_extract"
        [@@noalloc] [@@builtin]
    external insert : (int [@untagged]) ->  (t [@unboxed]) -> (int [@untagged]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_int16x8_insert"
        [@@noalloc] [@@builtin]

    let () =
        let v0 = low_of 0 in
        let v1 = insert 0 v0 1 in
        let v2 = insert 1 v1 2 in
        let v3 = insert 2 v2 3 in
        let v4 = insert 3 v3 4 in
        let v5 = insert 4 v4 5 in
        let v6 = insert 5 v5 6 in
        let v7 = insert 6 v6 7 in
        let v8 = insert 7 v7 8 in
        let i0 = extract 0 v8 in
        let i1 = extract 1 v8 in
        let i2 = extract 2 v8 in
        let i3 = extract 3 v8 in
        let i4 = extract 4 v8 in
        let i5 = extract 5 v8 in
        let i6 = extract 6 v8 in
        let i7 = extract 7 v8 in
        eqi i0 i1 1 2;
        eqi i2 i3 3 4;
        eqi i4 i5 5 6;
        eqi i6 i7 7 8
    ;;

    external sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sll"
        [@@noalloc] [@@unboxed] [@@builtin]
    external srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_srl"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sra : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sra"
        [@@noalloc] [@@unboxed] [@@builtin]

    external slli : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int16x8_slli"
        [@@noalloc] [@@builtin]
    external srli : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int16x8_srli"
        [@@noalloc] [@@builtin]
    external srai : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_int16x8_srai"
        [@@noalloc] [@@builtin]


    let () =
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08x << %08x\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let shift = Int16.logand r 0xf in
            let result = sll v (Int16.of_ints shift 0 0 0 0 0 0 0) in
            let expectl = Int16.shift_left l shift in
            let expectr = Int16.shift_left r shift in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08x >> %08x\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let shift = Int16.logand r 0xf in
            let result = srl v (Int16.of_ints shift 0 0 0 0 0 0 0) in
            let expectl = Int16.shift_right_logical l shift in
            let expectr = Int16.shift_right_logical r shift in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08x >>a %08x\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let shift = Int16.logand r 0xf in
            let result = sra v (Int16.of_ints shift 0 0 0 0 0 0 0) in
            let expectl = Int16.shift_right l shift in
            let expectr = Int16.shift_right r shift in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08x|%08x << 7\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let result = slli 7 v in
            let expectl = Int16.shift_left l 7 in
            let expectr = Int16.shift_left r 7 in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08x|%08x >> 7\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let result = srli 7 v in
            let expectl = Int16.shift_right_logical l 7 in
            let expectr = Int16.shift_right_logical r 7 in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int16.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%08x|%08x >>a 7\n%!" l r);
            let v = Int16.of_ints l r l r l r l r in
            let result = srai 7 v in
            let expectl = Int16.shift_right l 7 in
            let expectr = Int16.shift_right r 7 in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        )
    ;;
end

module Int8x16 = struct

    type t = int8x16

    (* Creation / Destruction *)

    external low_of : (int [@untagged]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_int8x16_low_of_int"
        [@@noalloc] [@@builtin]
    external low_to : (t [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_int8x16_low_to_int"
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


    external add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_add"
        [@@noalloc] [@@unboxed] [@@builtin]

    external add_saturating : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_add_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external add_saturating_unsigned : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_add_saturating_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_sub"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sub_saturating : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_sub_saturating"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sub_saturating_unsigned : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_sub_saturating_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external max : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int8x16_max"
        [@@noalloc] [@@unboxed] [@@builtin]

    external min : t -> t -> t = "caml_vec128_unreachable" "caml_sse41_int8x16_min"
        [@@noalloc] [@@unboxed] [@@builtin]

    external maxu : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_max_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external minu : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_min_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cmpeq : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_cmpeq"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cmpgt : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_cmpgt"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtsx_i16 : t -> int16x8 = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int16x8"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtsx_i32 : t -> int32x4 = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtsx_i64 : t -> int64x2 = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtzx_i16 : t -> int16x8 = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int16x8"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtzx_i32 : t -> int32x4 = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]

    external cvtzx_i64 : t -> int64x2 = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]

    external abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int8x16_abs"
        [@@noalloc] [@@unboxed] [@@builtin]

    external mulsign : t -> t -> t = "caml_vec128_unreachable" "caml_ssse3_int8x16_mulsign"
        [@@noalloc] [@@unboxed] [@@builtin]

    external avgu : t -> t -> t  = "caml_vec128_unreachable" "caml_sse2_int8x16_avg_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external sadu : t -> t -> int64x2  = "caml_vec128_unreachable" "caml_sse2_int8x16_sad_unsigned"
        [@@noalloc] [@@unboxed] [@@builtin]

    external msadu : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (int16x8 [@unboxed])  = "caml_vec128_unreachable" "caml_sse41_int8x16_multi_sad_unsigned"
        [@@noalloc] [@@builtin]

    let check_binop scalar vector i0 i1 =
        failmsg := (fun () -> Printf.printf "%02x | %02x\n%!" i0 i1);
        let r0 = scalar i0 i1 in
        let r1 = scalar i1 i0 in
        let expect = Int8.of_ints r0 r1 r0 r1 r0 r1 r0 r1 in
        let v1 = Int8.of_ints i0 i1 i0 i1 i0 i1 i0 i1 in
        let v2 = Int8.of_ints i1 i0 i1 i0 i1 i0 i1 i0 in
        let result = vector v1 v2 in
        eq (int8x16_low_int64 result) (int8x16_high_int64 result)
            (int8x16_low_int64 expect) (int8x16_high_int64 expect)
    ;;
    let () =
        Int8.check_ints (check_binop Int8.add add);
        Int8.check_ints (check_binop Int8.sub sub);
        Int8.check_ints (check_binop Int8.adds add_saturating);
        Int8.check_ints (check_binop Int8.subs sub_saturating);
        Int8.check_ints (check_binop Int8.addsu add_saturating_unsigned);
        Int8.check_ints (check_binop Int8.subsu sub_saturating_unsigned);
        Int8.check_ints (check_binop Int8.max max);
        Int8.check_ints (check_binop Int8.min min);
        Int8.check_ints (check_binop Int8.maxu maxu);
        Int8.check_ints (check_binop Int8.minu minu);
        Int8.check_ints (check_binop Int8.cmpeq cmpeq);
        Int8.check_ints (check_binop Int8.cmpgt cmpgt);
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x cvt_sx_i64\n%!" l r);
            let v = Int8.of_ints l r 0 0 0 0 0 0 in
            let result = cvtsx_i64 v in
            let expectl = Int8.cvtsx_i64 l in
            let expectr = Int8.cvtsx_i64 r in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
            expectl expectr
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x cvt_zx_i64\n%!" l r);
            let v = Int8.of_ints l r 0 0 0 0 0 0 in
            let result = cvtzx_i64 v in
            let expectl = Int8.cvtzx_i64 l in
            let expectr = Int8.cvtzx_i64 r in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
            expectl expectr
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x cvt_sx_i32\n%!" l r);
            let v = Int8.of_ints l r l r 0 0 0 0 in
            let result = cvtsx_i32 v in
            let expectl = Int8.cvtsx_i32 l in
            let expectr = Int8.cvtsx_i32 r in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x cvt_zx_i32\n%!" l r);
            let v = Int8.of_ints l r l r 0 0 0 0 in
            let result = cvtzx_i32 v in
            let expectl = Int8.cvtzx_i32 l in
            let expectr = Int8.cvtzx_i32 r in
            let expect = Int32s.of_int32s expectl expectr expectl expectr in
            eq (int32x4_low_int64 result) (int32x4_high_int64 result)
            (int32x4_low_int64 expect) (int32x4_high_int64 expect)
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x cvt_sx_i16\n%!" l r);
            let v = Int8.of_ints l r l r l r l r in
            let result = cvtsx_i16 v in
            let expectl = Int8.cvtsx_i16 l in
            let expectr = Int8.cvtsx_i16 r in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x cvt_zx_i16\n%!" l r);
            let v = Int8.of_ints l r l r l r l r in
            let result = cvtzx_i16 v in
            let expectl = Int8.cvtzx_i16 l in
            let expectr = Int8.cvtzx_i16 r in
            let expect = Int16.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int16x8_low_int64 result) (int16x8_high_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x abs\n%!" l r);
            let v = Int8.of_ints l r l r l r l r in
            let result = abs v in
            let expectl = Int8.abs l in
            let expectr = Int8.abs r in
            let expect = Int8.of_ints expectl expectr expectl expectr expectl expectr expectl expectr in
            eq (int8x16_low_int64 result) (int8x16_high_int64 result)
            (int8x16_low_int64 expect) (int8x16_high_int64 expect)
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x mulsign\n%!" l r);
            let v0 = Int8.of_ints l l r r l l r r in
            let v1 = Int8.of_ints l r l r l r l r in
            let result = mulsign v0 v1 in
            let mulsign x y = Int8.mulsign x y in
            let expect = Int8.of_ints (mulsign l l) (mulsign l r) (mulsign r l) (mulsign r r) (mulsign l l) (mulsign l r) (mulsign r l) (mulsign r r) in
            eq (int8x16_low_int64 result) (int8x16_high_int64 result)
            (int8x16_low_int64 expect) (int8x16_high_int64 expect)
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x avgu\n%!" l r);
            let v0 = Int8.of_ints l l r r l l r r in
            let v1 = Int8.of_ints l r l r l r l r in
            let result = avgu v0 v1 in
            let lr = Int8.avgu l r in
            let expect = Int8.of_ints l lr lr r l lr lr r in
            eq (int8x16_low_int64 result) (int8x16_high_int64 result)
            (int8x16_low_int64 expect) (int8x16_high_int64 expect)
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x sadu\n%!" l r);
            let v0 = Int8.of_ints l l r r l l r r in
            let v1 = Int8.of_ints l r l r l r l r in
            let result = sadu v0 v1 in
            let lr = Int8.diffu l r in
            let expect = Int64.of_int (4 * lr) in
            eq (int64x2_low_int64 result) (int64x2_high_int64 result)
            expect expect
        );
        Int8.check_ints (fun l r ->
            failmsg := (fun () -> Printf.printf "%02x|%02x msadu\n%!" l r);
            let v0 = Int8.of_ints l l r r l l r r in
            let v1 = Int8.of_ints l r l r l r l r in
            let result = msadu 0 v0 v1 in
            let lr = 2 * Int8.diffu l r in
            let expect = Int16.of_ints lr lr lr lr lr lr lr lr in
            eq (int16x8_low_int64 result) (int16x8_low_int64 result)
            (int16x8_low_int64 expect) (int16x8_high_int64 expect)
        );
    ;;

    external extract : (int [@untagged]) -> (t [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse41_int8x16_extract"
        [@@noalloc] [@@builtin]
    external insert : (int [@untagged]) ->  (t [@unboxed]) -> (int [@untagged]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_int8x16_insert"
        [@@noalloc] [@@builtin]

    let () =
        let v0 = low_of 0 in
        let v1 = insert 0 v0 1 in
        let v2 = insert 1 v1 2 in
        let v3 = insert 2 v2 3 in
        let v4 = insert 3 v3 4 in
        let v5 = insert 4 v4 5 in
        let v6 = insert 5 v5 6 in
        let v7 = insert 6 v6 7 in
        let v8 = insert 7 v7 8 in
        let v9 = insert 8 v8 9 in
        let v10 = insert 9 v9 10 in
        let v11 = insert 10 v10 11 in
        let v12 = insert 11 v11 12 in
        let v13 = insert 12 v12 13 in
        let v14 = insert 13 v13 14 in
        let v15 = insert 14 v14 15 in
        let v16 = insert 15 v15 16 in
        let i1 = extract 0 v16 in
        let i2 = extract 1 v16 in
        let i3 = extract 2 v16 in
        let i4 = extract 3 v16 in
        let i5 = extract 4 v16 in
        let i6 = extract 5 v16 in
        let i7 = extract 6 v16 in
        let i8 = extract 7 v16 in
        let i9 = extract 8 v16 in
        let i10 = extract 9 v16 in
        let i11 = extract 10 v16 in
        let i12 = extract 11 v16 in
        let i13 = extract 12 v16 in
        let i14 = extract 13 v16 in
        let i15 = extract 14 v16 in
        let i16 = extract 15 v16 in
        eqi i1  i2  1 2;
        eqi i3  i4  3 4;
        eqi i5  i6  5 6;
        eqi i7  i8  7 8;
        eqi i9  i10 9 10;
        eqi i11 i12 11 12;
        eqi i13 i14 13 14;
        eqi i15 i16 15 16;
    ;;
end

module SSE_Util = struct

    type t = int32x4

    external high_64_to_low_64 : t -> t -> t = "caml_vec128_unreachable" "caml_sse_vec128_high_64_to_low_64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external low_64_to_high_64 : t -> t -> t = "caml_vec128_unreachable" "caml_sse_vec128_low_64_to_high_64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_high_32 : t -> t -> t = "caml_vec128_unreachable" "caml_sse_vec128_interleave_high_32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_low_32 : t -> t -> t = "caml_vec128_unreachable" "caml_sse_vec128_interleave_low_32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external shuffle_32 : (int [@untagged]) -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "caml_vec128_unreachable" "caml_sse_vec128_shuffle_32"
        [@@noalloc] [@@builtin]
    external movemask_32 : (t [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse_vec128_movemask_32"
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
    let () =
        let v = Int32s.of_int32s 0xffffffffl 0x80000000l 0x7fffffffl 0x0l in
        let i = movemask_32 v in
        eqi i 0 0b0011 0
    ;;
end

module SSE2_Util = struct

    external _and : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_and"
        [@@noalloc] [@@unboxed] [@@builtin]
    external andnot : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_andnot"
        [@@noalloc] [@@unboxed] [@@builtin]
    external _or : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_or"
        [@@noalloc] [@@unboxed] [@@builtin]
    external xor : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_xor"
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
        Int64s.check_ints (check_binop Int64.logand _and);
        Int64s.check_ints (check_binop (fun l r -> Int64.(logand (lognot l) r)) andnot);
        Int64s.check_ints (check_binop Int64.logor _or);
        Int64s.check_ints (check_binop Int64.logxor xor);
    ;;

    external movemask_8 : (int8x16 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_8"
        [@@noalloc] [@@builtin]
    external movemask_64 : (int64x2 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_64"
        [@@noalloc] [@@builtin]

    let () =
        let v0 = int64x2_of_int64s 0xffffffffffffffffL 0x8000000000000000L in
        let v1 = int64x2_of_int64s 0x7fffffffffffffffL 0x0L in
        let i0 = movemask_64 v0 in
        let i1 = movemask_64 v1 in
        eqi i0 i1 0b11 0b00;
        let v0 = Int8.of_ints 0xff 0x7f 0x80 0x0 0x1 0xcc 0x33 0x55 in
        let i0 = movemask_8 v0 in
        eqi i0 0 0b0010_0101_0010_0101 0
    ;;

    external shift_left_bytes : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_vec128_shift_left_bytes"
        [@@noalloc] [@@builtin]
    external shift_right_bytes : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_vec128_shift_right_bytes"
        [@@noalloc] [@@builtin]

    let () =
        let v0 = Int8.of_ints 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 in
        let v1 = shift_left_bytes 1 v0 in
        let v2 = shift_right_bytes 1 v0 in
        eq (int8x16_low_int64 v1) (int8x16_high_int64 v1)
        0x0605040302010000L 0x0605040302010007L;
        eq (int8x16_low_int64 v2) (int8x16_high_int64 v2)
        0x07060504030201L 0x0007060504030201L;
    ;;

    external shuffle_64 : (int [@untagged]) -> (int64x2 [@unboxed]) -> (int64x2 [@unboxed]) -> (int64x2 [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_64"
        [@@noalloc] [@@builtin]

    external shuffle_high_16 : (int [@untagged]) -> (int16x8 [@unboxed]) -> (int16x8 [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_high_16"
        [@@noalloc] [@@builtin]

    external shuffle_low_16 : (int [@untagged]) -> (int16x8 [@unboxed]) -> (int16x8 [@unboxed]) = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_low_16"
        [@@noalloc] [@@builtin]

    let () =
        let _12 = int64x2_of_int64s 1L 2L in
        let _34 = int64x2_of_int64s 3L 4L in
        let v0 = shuffle_64 0b00 _12 _34 in
        let v1 = shuffle_64 0b01 _12 _34 in
        let v2 = shuffle_64 0b10 _12 _34 in
        let v3 = shuffle_64 0b11 _12 _34 in
        eq (int64x2_low_int64 v0) (int64x2_high_int64 v0) 1L 3L;
        eq (int64x2_low_int64 v1) (int64x2_high_int64 v1) 2L 3L;
        eq (int64x2_low_int64 v2) (int64x2_high_int64 v2) 1L 4L;
        eq (int64x2_low_int64 v3) (int64x2_high_int64 v3) 2L 4L
    ;;
    let () =
        let v0 = Int16.of_ints 1 2 3 4 5 6 7 8 in
        let s0 = shuffle_high_16 0 v0 in
        let s1 = shuffle_high_16 0b01010101 v0 in
        let s2 = shuffle_high_16 0b10101010 v0 in
        let s3 = shuffle_high_16 0b11111111 v0 in
        eq (int16x8_low_int64 s0) (int16x8_high_int64 s0) 0x0004000300020001L 0x0005000500050005L;
        eq (int16x8_low_int64 s1) (int16x8_high_int64 s1) 0x0004000300020001L 0x0006000600060006L;
        eq (int16x8_low_int64 s2) (int16x8_high_int64 s2) 0x0004000300020001L 0x0007000700070007L;
        eq (int16x8_low_int64 s3) (int16x8_high_int64 s3) 0x0004000300020001L 0x0008000800080008L;
        let s0 = shuffle_low_16 0 v0 in
        let s1 = shuffle_low_16 0b01010101 v0 in
        let s2 = shuffle_low_16 0b10101010 v0 in
        let s3 = shuffle_low_16 0b11111111 v0 in
        eq (int16x8_low_int64 s0) (int16x8_high_int64 s0) 0x0001000100010001L 0x0008000700060005L;
        eq (int16x8_low_int64 s1) (int16x8_high_int64 s1) 0x0002000200020002L 0x0008000700060005L;
        eq (int16x8_low_int64 s2) (int16x8_high_int64 s2) 0x0003000300030003L 0x0008000700060005L;
        eq (int16x8_low_int64 s3) (int16x8_high_int64 s3) 0x0004000400040004L 0x0008000700060005L;
    ;;

    external interleave_high_8 : int8x16 -> int8x16 -> int8x16 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_low_8 : int8x16 -> int8x16 -> int8x16 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_high_16 : int16x8 -> int16x8 -> int16x8 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_16"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_low_16 : int16x8 -> int16x8 -> int16x8 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_16"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_high_64 : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_low_64 : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_64"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v0 = Int8.of_ints 0 1 2 3 4 5 6 7 in
        let v1 = Int8.of_ints 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
        let i0 = interleave_high_8 v0 v1 in
        let i1 = interleave_low_8 v0 v1 in
        eq (int8x16_low_int64 i0) (int8x16_high_int64 i0)
        0x0b030a0209010800L 0x0f070e060d050c04L;
        eq (int8x16_low_int64 i1) (int8x16_high_int64 i1)
        0x0b030a0209010800L 0x0f070e060d050c04L;
        let v0 = Int16.of_ints 0 1 2 3 4 5 6 7 in
        let v1 = Int16.of_ints 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
        let i0 = interleave_high_16 v0 v1 in
        let i1 = interleave_low_16 v0 v1 in
        eq (int16x8_low_int64 i0) (int16x8_high_int64 i0)
        0x000d_0005_000c_0004L 0x000f_0007_000e_0006L;
        eq (int16x8_low_int64 i1) (int16x8_high_int64 i1)
        0x0009_0001_0008_0000L 0x000b_0003_000a_0002L;
        let v0 = int64x2_of_int64s 0L 1L in
        let v1 = int64x2_of_int64s 2L 3L in
        let i0 = interleave_high_64 v0 v1 in
        let i1 = interleave_low_64 v0 v1 in
        eq (int64x2_low_int64 i0) (int64x2_high_int64 i0)
        1L 3L;
        eq (int64x2_low_int64 i1) (int64x2_high_int64 i1)
        0L 2L
    ;;
end

module SSE3_Util = struct

    external dup_low_64 : int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse3_vec128_dup_low_64"
        [@@noalloc] [@@unboxed] [@@builtin]
    external dup_odd_32 : int32x4 -> int32x4 = "caml_vec128_unreachable" "caml_sse3_vec128_dup_odd_32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external dup_even_32 : int32x4 -> int32x4 = "caml_vec128_unreachable" "caml_sse3_vec128_dup_even_32"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v0 = int64x2_of_int64s 1L 2L in
        let d0 = dup_low_64 v0 in
        eq (int64x2_low_int64 d0) (int64x2_high_int64 d0) 1L 1L;
        let v0 = Int32s.of_int32s 1l 2l 3l 4l in
        let d0 = dup_odd_32 v0 in
        let d1 = dup_even_32 v0 in
        eq (int32x4_low_int64 d0) (int32x4_high_int64 d0) 0x0000000200000002L 0x0000000400000004L;
        eq (int32x4_low_int64 d1) (int32x4_high_int64 d1) 0x0000000100000001L 0x0000000300000003L
    ;;
end

module SSSE3_Util = struct

    external shuffle_8 : int8x16 -> int8x16 -> int8x16 = "caml_vec128_unreachable" "caml_ssse3_vec128_shuffle_8"
        [@@noalloc] [@@unboxed] [@@builtin]

    external align_right_bytes : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) = "caml_vec128_unreachable" "caml_ssse3_vec128_align_right_bytes"
        [@@noalloc] [@@builtin]

    let () =
        let v0 = Int8.of_ints 0 1 2 3 4 5 6 7 in
        let sel0 = Int8.of_ints 0 0 0 0 0 0 0 0 in
        let sel1 = Int8.of_ints 0 1 2 3 4 5 6 7 in
        let sel2 = Int8.of_ints 15 15 15 15 15 15 15 15  in
        let s0 = shuffle_8 v0 sel0 in
        let s1 = shuffle_8 v0 sel1 in
        let s2 = shuffle_8 v0 sel2 in
        eq (int8x16_low_int64 s0) (int8x16_high_int64 s0) 0L 0L;
        eq (int8x16_low_int64 s1) (int8x16_high_int64 s1) 0x0706050403020100L 0x0706050403020100L;
        eq (int8x16_low_int64 s2) (int8x16_high_int64 s2) 0x0707070707070707L 0x0707070707070707L
    ;;

    let () =
        let v0 = Int8.of_ints 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 in
        let v1 = Int8.of_ints 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf in
        let v2 = align_right_bytes 3 v1 v0 in
        eq (int8x16_low_int64 v2) (int8x16_high_int64 v2)
        0x0201000706050403L 0x0a09080706050403L;
    ;;
end

module SSE41_Util = struct

    external blend_16 : (int [@untagged]) -> (int16x8 [@unboxed]) -> (int16x8 [@unboxed]) -> (int16x8 [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_16"
        [@@noalloc] [@@builtin]
    external blend_32 : (int [@untagged]) -> (int32x4 [@unboxed]) -> (int32x4 [@unboxed]) -> (int32x4 [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_32"
        [@@noalloc] [@@builtin]
    external blend_64 : (int [@untagged]) -> (int64x2 [@unboxed]) -> (int64x2 [@unboxed]) -> (int64x2 [@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_64"
        [@@noalloc] [@@builtin]

    external blendv_8 : int8x16 -> int8x16 -> int8x16 -> int8x16 = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external blendv_32 : int32x4 -> int32x4 -> int32x4 -> int32x4 = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_32"
        [@@noalloc] [@@unboxed] [@@builtin]
    external blendv_64 : int64x2 -> int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_64"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v0 = Int16.of_ints 0 1 2 3 4 5 6 7 in
        let v1 = Int16.of_ints 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
        let b0 = blend_16 0b01010101 v0 v1 in
        eq (int16x8_low_int64 b0) (int16x8_high_int64 b0)
        0x0003_000a_0001_0008L 0x0007_000e_0005_000cL;
        let v0 = Int32s.of_int32s 0l 1l 2l 3l in
        let v1 = Int32s.of_int32s 4l 5l 6l 7l in
        let b0 = blend_32 0b0101 v0 v1 in
        eq (int32x4_low_int64 b0) (int32x4_high_int64 b0)
        0x00000001_00000004L 0x00000003_00000006L;
        let v0 = int64x2_of_int64s 0L 1L in
        let v1 = int64x2_of_int64s 2L 3L in
        let b0 = blend_64 0b01 v0 v1 in
        eq (int64x2_low_int64 b0) (int64x2_high_int64 b0)
        2L 1L
    ;;

    let () =
        let v0 = Int8.of_ints 0 1   2   3   4   5   6   7 in
        let v1 = Int8.of_ints 8 9 0xa 0xb 0xc 0xd 0xe 0xf in
        let b0 = blendv_8 v0 v1 (Int8.of_ints 0xff 0x00 0xff 0x00 0xff 0x00 0xff 0x00) in
        eq (int8x16_low_int64 b0) (int8x16_high_int64 b0)
        0x07_0e_05_0c_03_0a_01_08L 0x07_0e_05_0c_03_0a_01_08L;
        let v0 = Int32s.of_int32s 0l 1l 2l 3l in
        let v1 = Int32s.of_int32s 4l 5l 6l 7l in
        let b0 = blendv_32 v0 v1 (Int32s.of_int32s 0xffffffffl 0x0l 0xffffffffl 0x0l) in
        eq (int32x4_low_int64 b0) (int32x4_high_int64 b0)
        0x00000001_00000004L 0x00000003_00000006L;
        let v0 = int64x2_of_int64s 0L 1L in
        let v1 = int64x2_of_int64s 2L 3L in
        let b0 = blendv_64 v0 v1 (int64x2_of_int64s 0xffffffffffffffffL 0x0L) in
        eq (int64x2_low_int64 b0) (int64x2_high_int64 b0)
        2L 1L
    ;;
end

module SSE42_String = struct

    (* These also work with int16x8s, given the 16-bit char encoding immediate bit *)

    external cmpestrm : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) -> (int [@untagged]) -> (int8x16 [@unboxed]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrm"
        [@@noalloc] [@@builtin]
    external cmpestra : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestra"
        [@@noalloc] [@@builtin]
    external cmpestrc : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrc"
        [@@noalloc] [@@builtin]
    external cmpestri : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestri"
        [@@noalloc] [@@builtin]
    external cmpestro : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestro"
        [@@noalloc] [@@builtin]
    external cmpestrs : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrs"
        [@@noalloc] [@@builtin]
    external cmpestrz : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpestrz"
        [@@noalloc] [@@builtin]

    external cmpistrm : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrm"
        [@@noalloc] [@@builtin]
    external cmpistra : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistra"
        [@@noalloc] [@@builtin]
    external cmpistrc : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrc"
        [@@noalloc] [@@builtin]
    external cmpistri : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistri"
        [@@noalloc] [@@builtin]
    external cmpistro : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistro"
        [@@noalloc] [@@builtin]
    external cmpistrs : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrs"
        [@@noalloc] [@@builtin]
    external cmpistrz : (int [@untagged]) -> (int8x16 [@unboxed]) -> (int8x16 [@unboxed]) -> (int [@untagged]) = "caml_vec128_unreachable" "caml_sse42_vec128_cmpistrz"
        [@@noalloc] [@@builtin]

    let vec_of_string str =
        assert (String.length str = 16);
        let i0 = ref 0L in
        for i = 7 downto 0 do
            i0 := Int64.shift_left !i0 8;
            i0 := Int64.logor !i0 (Char.code str.[i] |> Int64.of_int);
        done;
        let i1 = ref 0L in
        for i = 15 downto 8 do
            i1 := Int64.shift_left !i1 8;
            i1 := Int64.logor !i1 (Char.code str.[i] |> Int64.of_int);
        done;
        int8x16_of_int64s !i0 !i1
    ;;

    let sbyte = 0b0000_0010
    let cmp_eq_each = 0b0000_1000
    let negate = 0b0001_0000
    let msk_negate = 0b0011_0000
    let lst_sig = 0b0000_0000
    let mst_sig = 0b0100_0000
    let bit_msk = 0b0000_0000
    let byte_mask = 0b0100_0000


    let () =
        let v0 = vec_of_string "abcdefghijklmnop" in
        let v1 = vec_of_string "abcdefgh\000\000\000\000\000\000\000\000" in
        let s0 = cmpistra (sbyte lor cmp_eq_each lor negate) v0 v0 in
        let s1 = cmpistrc (sbyte lor cmp_eq_each) v0 v1 in
        let s2 = cmpistri (sbyte lor cmp_eq_each lor lst_sig) v0 v1 in
        let s3 = cmpistri (sbyte lor cmp_eq_each lor mst_sig) v0 v1 in
        let s4 = cmpistro (sbyte lor cmp_eq_each) v0 v1 in
        let s5 = cmpistrs (sbyte lor cmp_eq_each) v0 v1 in
        let s6 = cmpistrs (sbyte lor cmp_eq_each) v1 v0 in
        let s7 = cmpistrz (sbyte lor cmp_eq_each) v0 v1 in
        let s8 = cmpistrz (sbyte lor cmp_eq_each) v1 v0 in
        eqi s0 s1 1 1;
        eqi s2 s3 0 7;
        eqi s4 s5 1 0;
        eqi s6 s7 1 1;
        eqi s8 0 0 0;
        let m = cmpistrm (sbyte lor cmp_eq_each lor bit_msk) v0 v1 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffL 0L;
        let m = cmpistrm (sbyte lor cmp_eq_each lor bit_msk) v0 v0 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffffL 0L;
        let m = cmpistrm (sbyte lor cmp_eq_each lor byte_mask) v0 v1 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffffffffffffffffL 0L;
        let m = cmpistrm (sbyte lor cmp_eq_each lor byte_mask) v0 v0 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffffffffffffffffL 0xffffffffffffffffL;
    ;;

    let () =
        let v0 = vec_of_string "abcdefghijklmnop" in
        let v1 = vec_of_string "abcdefgh\000\000\000\000\000\000\000\000" in
        let s0 = cmpestra (sbyte lor cmp_eq_each lor negate) v0 v0 16 16 in
        let s1 = cmpestrc (sbyte lor cmp_eq_each) v0 v1 16 8 in
        let s2 = cmpestri (sbyte lor cmp_eq_each lor lst_sig) v0 v1 16 8 in
        let s3 = cmpestri (sbyte lor cmp_eq_each lor mst_sig) v0 v1 16 8 in
        let s4 = cmpestro (sbyte lor cmp_eq_each) v0 v1 16 8 in
        let s5 = cmpestrs (sbyte lor cmp_eq_each) v0 v1 16 8 in
        let s6 = cmpestrs (sbyte lor cmp_eq_each) v1 v0 8 16 in
        let s7 = cmpestrz (sbyte lor cmp_eq_each) v0 v1 16 8 in
        let s8 = cmpestrz (sbyte lor cmp_eq_each) v1 v0 8 16 in
        eqi s0 s1 1 1;
        eqi s2 s3 0 7;
        eqi s4 s5 1 0;
        eqi s6 s7 1 1;
        eqi s8 0 0 0;
        let m = cmpestrm (sbyte lor cmp_eq_each lor bit_msk) v0 v1 16 8 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffL 0L;
        let m = cmpestrm (sbyte lor cmp_eq_each lor bit_msk) v0 v0 16 16 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffffL 0L;
        let m = cmpestrm (sbyte lor cmp_eq_each lor byte_mask) v0 v1 16 8 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffffffffffffffffL 0L;
        let m = cmpestrm (sbyte lor cmp_eq_each lor byte_mask) v0 v0 16 16 in
        eq (int8x16_low_int64 m) (int8x16_high_int64 m)
            0xffffffffffffffffL 0xffffffffffffffffL;
    ;;
end
