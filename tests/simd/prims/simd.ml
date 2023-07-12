open Stdlib

(* Technically the C stubs should use __m128i for int vectors, __m128 for float32,
   and __m128d for float64, but all three of these types are just 16-aligned
   16-byte blocks of memory, so the same stubs work for all types. *)

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

let eq lv hv l h =
  if l <> lv then Printf.printf "%Ld <> %Ld\n" l lv;
  if h <> hv then Printf.printf "%Ld <> %Ld\n" h hv
;;

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

(* Conversions *)

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

(* SSE: Float32x4 *)

external float32x4_cmp : int -> (float32x4 [@unboxed]) -> (float32x4 [@unboxed]) -> (int32x4 [@unboxed]) = "" "caml_sse_float32x4_cmp"
  [@@noalloc] [@@builtin]

external float32x4_add : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_float32x4_add"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_sub : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_float32x4_sub"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_mul : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_float32x4_mul"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_div : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_float32x4_div"
  [@@noalloc] [@@unboxed] [@@builtin]

external float32x4_max : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_float32x4_max"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_min : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_float32x4_min"
  [@@noalloc] [@@unboxed] [@@builtin]

external float32x4_rcp : float32x4 -> float32x4 = "" "caml_sse_float32x4_rcp"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_rsqrt : float32x4 -> float32x4 = "" "caml_sse_float32x4_rsqrt"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_sqrt : float32x4 -> float32x4 = "" "caml_sse_float32x4_sqrt"
  [@@noalloc] [@@unboxed] [@@builtin]

(* SSE: conversions / "constants" *)

external float32x4_set4 : float -> float -> float -> float -> float32x4 = "" "caml_sse_float32x4_set4"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_set1 : float -> float32x4 = "" "caml_sse_float32x4_set1"
  [@@noalloc] [@@unboxed] [@@builtin]

external float32x4_get : int -> (float32x4 [@unboxed]) -> (float [@unboxed]) = "" "caml_sse_float32x4_get"
  [@@noalloc] [@@builtin]

external float32x4_zero : unit -> (float32x4 [@unboxed]) = "" "caml_sse_zero"
  [@@noalloc] [@@builtin]

(* SSE: shuffles *)

external float32x4_move_high_to_low : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_move_high_to_low"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_move_low_to_high : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_move_low_to_high"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_interleave_high : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_interleave_high"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_interleave_low : float32x4 -> float32x4 -> float32x4 = "" "caml_sse_interleave_low"
  [@@noalloc] [@@unboxed] [@@builtin]
external float32x4_shuffle : int -> (float32x4 [@unboxed]) -> (float32x4 [@unboxed]) -> (float32x4 [@unboxed]) = "" "caml_sse_shuffle"
  [@@noalloc] [@@builtin]

let () =
  let z = float32x4_zero () in
  Printf.printf "%Ld %Ld" (float32x4_low_int64 z) (float32x4_high_int64 z)
