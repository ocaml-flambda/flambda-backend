open Stdlib

(* Technically the C stubs should use __m128i for int vectors, __m128 for float32,
   and __m128d for float64, but all three of these types are just 16-aligned
   16-byte blocks of memory, so the same stubs work for all types. *)

let eq lv hv l h =
    if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
    if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h
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

    external int64x2_of_int32x4 : int32x4 -> int64x2 = "" "caml_int64x2_of_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_int16x8 : int16x8 -> int64x2 = "" "caml_int64x2_of_int16x8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_int8x16 : int8x16 -> int64x2 = "" "caml_int64x2_of_int8x16"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_float32x4 : float32x4 -> int64x2 = "" "caml_int64x2_of_float32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int64x2_of_float64x2 : float64x2 -> int64x2 = "" "caml_int64x2_of_float64x2"
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

    external int32x4_of_int64x2 : int64x2 -> int32x4 = "" "caml_int32x4_of_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_int16x8 : int16x8 -> int32x4 = "" "caml_int32x4_of_int16x8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_int8x16 : int8x16 -> int32x4 = "" "caml_int32x4_of_int8x16"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_float32x4 : float32x4 -> int32x4 = "" "caml_int32x4_of_float32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int32x4_of_float64x2 : float64x2 -> int32x4 = "" "caml_int32x4_of_float64x2"
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

    external int16x8_of_int64x2 : int64x2 -> int16x8 = "" "caml_int16x8_of_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_int32x4 : int32x4 -> int16x8 = "" "caml_int16x8_of_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_int8x16 : int8x16 -> int16x8 = "" "caml_int16x8_of_int8x16"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_float32x4 : float32x4 -> int16x8 = "" "caml_int16x8_of_float32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int16x8_of_float64x2 : float64x2 -> int16x8 = "" "caml_int16x8_of_float64x2"
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

    external int8x16_of_int64x2 : int64x2 -> int8x16 = "" "caml_int8x16_of_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_int32x4 : int32x4 -> int8x16 = "" "caml_int8x16_of_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_int16x8 : int16x8 -> int8x16 = "" "caml_int8x16_of_int16x8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_float32x4 : float32x4 -> int8x16 = "" "caml_int8x16_of_float32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external int8x16_of_float64x2 : float64x2 -> int8x16 = "" "caml_int8x16_of_float64x2"
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

    external float32x4_of_int64x2 : int64x2 -> float32x4 = "" "caml_float32x4_of_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int32x4 : int32x4 -> float32x4 = "" "caml_float32x4_of_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int16x8 : int16x8 -> float32x4 = "" "caml_float32x4_of_int16x8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_int8x16 : int8x16 -> float32x4 = "" "caml_float32x4_of_int8x16"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float32x4_of_float64x2 : float64x2 -> float32x4 = "" "caml_float32x4_of_float64x2"
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

    external float64x2_of_int64x2 : int64x2 -> float64x2 = "" "caml_float64x2_of_int64x2"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int32x4 : int32x4 -> float64x2 = "" "caml_float64x2_of_int32x4"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int16x8 : int16x8 -> float64x2 = "" "caml_float64x2_of_int16x8"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_int8x16 : int8x16 -> float64x2 = "" "caml_float64x2_of_int8x16"
        [@@noalloc] [@@unboxed] [@@builtin]
    external float64x2_of_float32x4 : float32x4 -> float64x2 = "" "caml_float64x2_of_float32x4"
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
    external max : unit -> (t [@unboxed]) = "" "float32_max" [@@noalloc]
    external min : unit -> (t [@unboxed]) = "" "float32_min" [@@noalloc]

    let zero = zero ()
    let neg_zero = neg_zero ()
    let one = one ()
    let nan = nan ()
    let neg_infinity = neg_infinity ()
    let infinity = infinity ()
    let neg_one = neg_one ()
    let max = max ()
    let min = min ()

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

    external const1 : float -> t = "" "caml_float32x4_const1"
        [@@noalloc] [@@unboxed] [@@builtin]
    external const4 : float -> float -> float -> float -> t = "" "caml_float32x4_const4"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v1 = const1 1. in
        let v2 = const1 2. in
        let l1 = float32x4_low_int64 v1 in
        let h1 = float32x4_high_int64 v1 in
        let l2 = float32x4_low_int64 v2 in
        let h2 = float32x4_high_int64 v2 in
        eq l1 h1 0x3f8000003f800000L 0x3f8000003f800000L;
        eq l2 h2 0x4000000040000000L 0x4000000040000000L;
        let f0 = low_to v1 in
        let f1 = low_to v2 in
        eqf f0 f1 1. 2.
    ;;

    let () =
        let v1 = const4 1. 2. 3. 4. in
        let v2 = const4 5. 6. 7. 8. in
        let l1 = float32x4_low_int64 v1 in
        let h1 = float32x4_high_int64 v1 in
        let l2 = float32x4_low_int64 v2 in
        let h2 = float32x4_high_int64 v2 in
        eq l1 h1 0x400000003f800000L 0x4080000040400000L;
        eq l2 h2 0x40c0000040a00000L 0x4100000040e00000L;
        let f0 = low_to v1 in
        let f1 = low_to v2 in
        eqf f0 f1 1. 5.
    ;;

    (* Math *)

    external cmp : int -> (t [@unboxed]) -> (t [@unboxed]) -> (int32x4 [@unboxed]) = "" "caml_sse_float32x4_cmp"
        [@@noalloc] [@@builtin]

    let () =
        let test_cmp f0 f1 =
            let mask op =
                let low = if op f0 f1 then 0xffffffffL else 0L in
                let high = if op f1 f0 then 0xffffffff00000000L else 0L in
                Int64.logor low high
            in
            let v1 = Float32.to_float32x4 f0 f1 f0 f1 in
            let v2 = Float32.to_float32x4 f1 f0 f1 f0 in
            let _eq = cmp 0 v1 v2 in
            let lt = cmp 1 v1 v2 in
            let le = cmp 2 v1 v2 in
            let uord = cmp 3 v1 v2 in
            let neq = cmp 4 v1 v2 in
            let nlt = cmp 5 v1 v2 in
            let nle = cmp 6 v1 v2 in
            let ord = cmp 7 v1 v2 in
            let __eq = mask Float32.eq in
            let _lt = mask Float32.lt in
            let _le = mask Float32.le in
            let _uord = mask Float32.uord in
            let _neq = mask Float32.neq in
            let _nlt = mask Float32.nlt in
            let _nle = mask Float32.nle in
            let _ord = mask Float32.ord in
            eq (int32x4_low_int64 _eq) (int32x4_high_int64 _eq) __eq __eq;
            eq (int32x4_low_int64 lt) (int32x4_high_int64 lt) _lt _lt;
            eq (int32x4_low_int64 le) (int32x4_high_int64 le) _le _le;
            eq (int32x4_low_int64 uord) (int32x4_high_int64 uord) _uord _uord;
            eq (int32x4_low_int64 neq) (int32x4_high_int64 neq) _neq _neq;
            eq (int32x4_low_int64 nlt) (int32x4_high_int64 nlt) _nlt _nlt;
            eq (int32x4_low_int64 nle) (int32x4_high_int64 nle) _nle _nle;
            eq (int32x4_low_int64 ord) (int32x4_high_int64 ord) _ord _ord
        in
        test_cmp Float32.zero Float32.zero;
        test_cmp Float32.zero Float32.one;
        test_cmp Float32.zero Float32.neg_one;
        test_cmp Float32.one Float32.neg_one;
        test_cmp Float32.zero Float32.neg_zero;
        test_cmp Float32.nan Float32.zero;
        test_cmp Float32.infinity Float32.zero;
        test_cmp Float32.neg_infinity Float32.zero;
        test_cmp Float32.nan Float32.nan;
        test_cmp Float32.infinity Float32.infinity;
        test_cmp Float32.neg_infinity Float32.neg_infinity;
        test_cmp Float32.neg_infinity Float32.infinity;
        test_cmp Float32.infinity Float32.nan;
        test_cmp Float32.neg_infinity Float32.nan;
        test_cmp Float32.max Float32.infinity;
        test_cmp Float32.max Float32.neg_infinity;
        test_cmp Float32.min Float32.infinity;
        test_cmp Float32.min Float32.neg_infinity;
        test_cmp Float32.max Float32.max;
        test_cmp Float32.min Float32.min;
        test_cmp Float32.max Float32.min
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

    external rcp : t -> t = "" "caml_sse_float32x4_rcp"
        [@@noalloc] [@@unboxed] [@@builtin]
    external rsqrt : t -> t = "" "caml_sse_float32x4_rsqrt"
        [@@noalloc] [@@unboxed] [@@builtin]
    external sqrt : t -> t = "" "caml_sse_float32x4_sqrt"
        [@@noalloc] [@@unboxed] [@@builtin]

    (* Shuffles *)

    external move_high_to_low : t -> t -> t = "" "caml_sse_move_high_to_low"
        [@@noalloc] [@@unboxed] [@@builtin]
    external move_low_to_high : t -> t -> t = "" "caml_sse_move_low_to_high"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_high : t -> t -> t = "" "caml_sse_interleave_high"
        [@@noalloc] [@@unboxed] [@@builtin]
    external interleave_low : t -> t -> t = "" "caml_sse_interleave_low"
        [@@noalloc] [@@unboxed] [@@builtin]
    external shuffle : int -> (t [@unboxed]) -> (t [@unboxed]) -> (t [@unboxed]) = "" "caml_sse_shuffle"
        [@@noalloc] [@@builtin]
end

(* TODO: all of the above for float64x2, int64x2, int32x4, int16x8, int8x16 *)
(* TODO: masks *)