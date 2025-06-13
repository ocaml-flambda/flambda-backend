open Stdlib

external int64x4_of_int64s : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "vec256_of_int64s"
  [@@noalloc] [@@unboxed]

external int64x4_first_int64 : int64x4 -> int64 = "" "vec256_first_int64"
  [@@noalloc] [@@unboxed]

external int64x4_second_int64 : int64x4 -> int64 = "" "vec256_second_int64"
  [@@noalloc] [@@unboxed]

external int64x4_third_int64 : int64x4 -> int64 = "" "vec256_third_int64"
  [@@noalloc] [@@unboxed]

external int64x4_fourth_int64 : int64x4 -> int64 = "" "vec256_fourth_int64"
  [@@noalloc] [@@unboxed]

external lots_of_vectors :
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64x4 = "" "lots_of_vectors256"
  [@@noalloc] [@@unboxed]

external vectors_and_floats :
  int64x4 ->
  float ->
  int64x4 ->
  float ->
  int64x4 ->
  float ->
  int64x4 ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  float ->
  int64x4 = "" "vectors_and_floats256"
  [@@noalloc] [@@unboxed]

external vectors_and_floats_and_ints :
  int64x4 ->
  float ->
  int64x4 ->
  int64 ->
  int64x4 ->
  float ->
  int64x4 ->
  int64 ->
  int64 ->
  int64x4 ->
  int64x4 ->
  float ->
  float ->
  int64x4 ->
  int64x4 ->
  int64 ->
  int64 ->
  float ->
  int64x4 ->
  int64x4 ->
  int64x4 ->
  int64 ->
  int64 ->
  float ->
  int64x4 = "" "vectors_and_floats_and_ints256"
  [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d =
  let v1, v2, v3, v4 =
    ( int64x4_first_int64 v,
      int64x4_second_int64 v,
      int64x4_third_int64 v,
      int64x4_fourth_int64 v )
  in
  eq v1 a;
  eq v2 b;
  eq v3 c;
  eq v4 d

(* Vectors live across a probe handler *)
let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let v6 = int64x4_of_int64s 25L 26L 27L 28L in
  let v7 = int64x4_of_int64s 29L 30L 31L 32L in
  let v8 = int64x4_of_int64s 33L 34L 35L 36L in
  let v9 = int64x4_of_int64s 37L 38L 39L 40L in
  let v10 = int64x4_of_int64s 41L 42L 43L 44L in
  let v11 = int64x4_of_int64s 45L 46L 47L 48L in
  let v12 = int64x4_of_int64s 49L 50L 51L 52L in
  let v13 = int64x4_of_int64s 53L 54L 55L 56L in
  let v14 = int64x4_of_int64s 57L 58L 59L 60L in
  let v15 = int64x4_of_int64s 61L 62L 63L 64L in
  [%probe
    "hello" ~enabled_at_init:true
      (let xxx = int64x4_of_int64s (-1L) (-2L) (-3L) (-4L) in
       check xxx (-1L) (-2L) (-3L) (-4L))];
  let v =
    lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  in
  check v 496L 512L 528L 544L

let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let v6 = int64x4_of_int64s 25L 26L 27L 28L in
  let v7 = int64x4_of_int64s 29L 30L 31L 32L in
  let v8 = int64x4_of_int64s 33L 34L 35L 36L in
  let v9 = int64x4_of_int64s 37L 38L 39L 40L in
  let v10 = int64x4_of_int64s 41L 42L 43L 44L in
  [%probe
    "hello" ~enabled_at_init:true
      (let xxx = int64x4_of_int64s (-1L) (-2L) (-3L) (-4L) in
       check xxx (-1L) (-2L) (-3L) (-4L))];
  let v =
    vectors_and_floats v0 23. v1 24. v2 25. v3 26. 27. v4 v5 28. 29. v6 v7 30.
      31. 32. v8 v9 v10 33. 34. 35.
  in
  check v 377L 473L 253L 264L

let () =
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let v6 = int64x4_of_int64s 25L 26L 27L 28L in
  let v7 = int64x4_of_int64s 29L 30L 31L 32L in
  let v8 = int64x4_of_int64s 33L 34L 35L 36L in
  let v9 = int64x4_of_int64s 37L 38L 39L 40L in
  let v10 = int64x4_of_int64s 41L 42L 43L 44L in
  [%probe
    "hello" ~enabled_at_init:true
      (let xxx = int64x4_of_int64s (-1L) (-2L) (-3L) (-4L) in
       check xxx (-1L) (-2L) (-3L) (-4L))];
  let v =
    vectors_and_floats_and_ints v0 23. v1 24L v2 25. v3 26L 27L v4 v5 28. 29. v6
      v7 30L 31L 32. v8 v9 v10 33L 34L 35.
  in
  check v 377L 473L 253L 264L

let () =
  let i0 = 0 in
  let i1 = 1 in
  let i2 = 2 in
  let i3 = 3 in
  let f0 = 0. in
  let f1 = 1. in
  let f2 = 2. in
  let f3 = 3. in
  let f4 = 4. in
  let f5 = 5. in
  let f6 = 6. in
  let v0 = int64x4_of_int64s 1L 2L 3L 4L in
  let v1 = int64x4_of_int64s 5L 6L 7L 8L in
  let v2 = int64x4_of_int64s 9L 10L 11L 12L in
  let v3 = int64x4_of_int64s 13L 14L 15L 16L in
  let v4 = int64x4_of_int64s 17L 18L 19L 20L in
  let v5 = int64x4_of_int64s 21L 22L 23L 24L in
  let l = 123456789L in
  [%probe
    "hello" ~enabled_at_init:true
      (assert (i0 = 0);
       assert (i1 = 1);
       assert (i2 = 2);
       assert (i3 = 3);
       assert (f0 = 0.);
       assert (f1 = 1.);
       assert (f2 = 2.);
       assert (f3 = 3.);
       assert (f4 = 4.);
       assert (f5 = 5.);
       assert (f6 = 6.);
       check v0 1L 2L 3L 4L;
       check v1 5L 6L 7L 8L;
       check v2 9L 10L 11L 12L;
       check v3 13L 14L 15L 16L;
       check v4 17L 18L 19L 20L;
       check v5 21L 22L 23L 24L;
       assert (l = 123456789L))]
