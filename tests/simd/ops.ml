open Stdlib

let eq lv hv l h =
    if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
    if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h
;;

let eql lv hv l h =
    if l <> lv then Printf.printf "%016lx <> %016lx\n" lv l;
    if h <> hv then Printf.printf "%016lx <> %016lx\n" hv h
;;

let eqi lv hv l h =
    if l <> lv then Printf.printf "%016x <> %016x\n" lv l;
    if h <> hv then Printf.printf "%016x <> %016x\n" hv h
;;

let eqf lv hv l h =
    let open Float in
    if l <> lv then Printf.printf "%f <> %f\n" l lv;
    if h <> hv then Printf.printf "%f <> %f\n" h hv
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

    let check_floats f =
        Random.set_state (Random.State.make [|1234567890|]);
        f Float32.zero Float32.zero;
        f Float32.zero Float32.one;
        f Float32.one Float32.one;
        f Float32.zero Float32.neg_one;
        f Float32.neg_one Float32.neg_one;
        f Float32.one Float32.neg_one;
        f Float32.zero Float32.neg_zero;
        f Float32.nan Float32.zero;
        f Float32.infinity Float32.zero;
        f Float32.neg_infinity Float32.zero;
        f Float32.nan Float32.nan;
        f Float32.infinity Float32.infinity;
        f Float32.neg_infinity Float32.neg_infinity;
        f Float32.neg_infinity Float32.infinity;
        f Float32.infinity Float32.nan;
        f Float32.neg_infinity Float32.nan;
        f Float32.maxv Float32.infinity;
        f Float32.maxv Float32.neg_infinity;
        f Float32.minv Float32.infinity;
        f Float32.minv Float32.neg_infinity;
        f Float32.maxv Float32.maxv;
        f Float32.minv Float32.minv;
        f Float32.maxv Float32.minv;
        for i = 0 to 100_000 do
            let f0 = Random.int32 Int32.max_int in
            let f1 = Random.int32 Int32.max_int in
            f (if Random.bool () then f0 else Int32.neg f0) (if Random.bool () then f1 else Int32.neg f1)
        done
    ;;

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
        check_floats (check_cmp Float32.eq (fun l r -> cmp 0 l r));
        check_floats (check_cmp Float32.lt (fun l r -> cmp 1 l r));
        check_floats (check_cmp Float32.le (fun l r -> cmp 2 l r));
        check_floats (check_cmp Float32.uord (fun l r -> cmp 3 l r));
        check_floats (check_cmp Float32.neq (fun l r -> cmp 4 l r));
        check_floats (check_cmp Float32.nlt (fun l r -> cmp 5 l r));
        check_floats (check_cmp Float32.nle (fun l r -> cmp 6 l r));
        check_floats (check_cmp Float32.ord (fun l r -> cmp 7 l r))
    ;;

    external add : t -> t -> t = "" "caml_sse_float32x4_add"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sub : t -> t -> t = "" "caml_sse_float32x4_sub"
        [@@noalloc] [@@unboxed] [@@builtin]
    external mul : t -> t -> t = "" "caml_sse_float32x4_mul"
        [@@noalloc] [@@unboxed] [@@builtin]
    external div : t -> t -> t = "" "caml_sse_float32x4_div"
        [@@noalloc] [@@unboxed] [@@builtin]

    (* NOTE(max): these will need wrappers to have expected nan behavior *)
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
        check_floats (check_binop Float32.add add);
        check_floats (check_binop Float32.sub sub);
        check_floats (check_binop Float32.mul mul);
        check_floats (check_binop Float32.div div);
        check_floats (check_binop Float32.max max);
        check_floats (check_binop Float32.min min)
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
        check_floats (fun f _ -> check_unop Float32.rcp rcp f);
        check_floats (fun f _ -> check_unop Float32.sqrt sqrt f);
        check_floats (fun f _ -> check_unop Float32.rsqrt rsqrt f)
    ;;

    (* Shuffles *)

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

    let () =
        let eql = eq in
        let open Float32 in
        let _0011 = to_float32x4 zero zero one one in
        let _1100 = to_float32x4 one one zero zero in
        let _0101 = to_float32x4 zero one zero one in
        let _1010 = to_float32x4 one zero one zero in
        let _1111 = to_float32x4 one one one one in
        let _0000 = to_float32x4 zero zero zero zero in
        let res = high_64_to_low_64 _1100 _1100 in
        eql (float32x4_low_int64 _0000) (float32x4_high_int64 _0000)
            (float32x4_low_int64 res) (float32x4_high_int64 res);
        let res = low_64_to_high_64 _1100 _1100 in
        eql (float32x4_low_int64 _1111) (float32x4_high_int64 _1111)
            (float32x4_low_int64 res) (float32x4_high_int64 res);
        let res = interleave_high_32 _1100 _0011 in
        eql (float32x4_low_int64 _0101) (float32x4_high_int64 _0101)
            (float32x4_low_int64 res) (float32x4_high_int64 res);
        let res = interleave_low_32 _1100 _0011 in
        eql (float32x4_low_int64 _1010) (float32x4_high_int64 _1010)
            (float32x4_low_int64 res) (float32x4_high_int64 res)
    ;;
    let () =
        let eql = eq in
        let open Float32 in
        let two = add one one in
        let three = add two one in
        let _0123 = to_float32x4 zero one two three in
        let _0000 = to_float32x4 zero zero zero zero in
        let _1111 = to_float32x4 one one one one in
        let _2222 = to_float32x4 two two two two in
        let _3333 = to_float32x4 three three three three in
        let _r0000 = shuffle_32 0b00000000 _0123 _0123 in
        let _r1111 = shuffle_32 0b01010101 _0123 _0123 in
        let _r2222 = shuffle_32 0b10101010 _0123 _0123 in
        let _r3333 = shuffle_32 0b11111111 _0123 _0123 in
        eql (float32x4_low_int64 _0000) (float32x4_high_int64 _0000)
            (float32x4_low_int64 _r0000) (float32x4_high_int64 _r0000);
        eql (float32x4_low_int64 _1111) (float32x4_high_int64 _1111)
            (float32x4_low_int64 _r1111) (float32x4_high_int64 _r1111);
        eql (float32x4_low_int64 _2222) (float32x4_high_int64 _2222)
            (float32x4_low_int64 _r2222) (float32x4_high_int64 _r2222);
        eql (float32x4_low_int64 _3333) (float32x4_high_int64 _3333)
            (float32x4_low_int64 _r3333) (float32x4_high_int64 _r3333);
        let _1010 = to_float32x4 one zero one zero in
        let _0101 = to_float32x4 zero one zero one in
        let _r0101 = shuffle_32 0b01000100 _0123 _0123 in
        let _r1010 = shuffle_32 0b00010001 _0123 _0123 in
        let _r1111 = shuffle_32 0b11010010 _r1010 _r0101 in
        eql (float32x4_low_int64 _0101) (float32x4_high_int64 _0101)
            (float32x4_low_int64 _r0101) (float32x4_high_int64 _r0101);
        eql (float32x4_low_int64 _1010) (float32x4_high_int64 _1010)
            (float32x4_low_int64 _r1010) (float32x4_high_int64 _r1010);
        eql (float32x4_low_int64 _1111) (float32x4_high_int64 _1111)
            (float32x4_low_int64 _r1111) (float32x4_high_int64 _r1111);
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
end
