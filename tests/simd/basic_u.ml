open Stdlib

(* !!!

Should be kept in sync with basic.ml.
CR-someday mslater: with layout polymorphism, the tests could be functorized.

!!! *)

[@@@ocaml.warning "-unused-type-declaration"]

external box_float : float# -> float = "%box_float"

external box_int64x2 : int64x2# -> int64x2 = "%box_vec128"
external unbox_int64x2 : int64x2 -> int64x2# = "%unbox_vec128"

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v l h =
  let vl, vh = int64x2_low_int64 v, int64x2_high_int64 v in
  eq vl l;
  eq vh h
;;

(* Unbox/Box *)
let () =
  let[@inline never] opaque_identity v = v in
  let v = unbox_int64x2 (int64x2_of_int64s 1L 2L) in
  let v = opaque_identity v in
  let v = box_int64x2 v in
  check v 1L 2L
;;

(* Unboxed *)

type int8x16 = int8x16#
type int16x8 = int16x8#
type int32x4 = int32x4#
type int64x2 = int64x2#
type float32x4 = float32x4#
type float64x2 = float64x2#

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v l h =
  let vl, vh = int64x2_low_int64 v, int64x2_high_int64 v in
  eq vl l;
  eq vh h
;;

(* Box/Unbox *)
let () =
  let v = box_int64x2 (int64x2_of_int64s 1L 2L) in
  let v = Sys.opaque_identity v in
  let v = unbox_int64x2 v in
  check v 1L 2L
;;

let[@inline never] combine v0 v1 =
  let l0, h0 = int64x2_low_int64 v0, int64x2_high_int64 v0 in
  let l1, h1 = int64x2_low_int64 v1, int64x2_high_int64 v1 in
  int64x2_of_int64s (Int64.add l0 l1) (Int64.add h0 h1)
;;

let[@inline never] combine_with_floats v0 f0 v1 f1 =
  let l0, h0 = int64x2_low_int64 v0, int64x2_high_int64 v0 in
  let l1, h1 = int64x2_low_int64 v1, int64x2_high_int64 v1 in
  let l, h = Int64.add l0 l1, Int64.add h0 h1 in
  let l = Int64.add (Int64.of_float f0) l in
  let h = Int64.add (Int64.of_float f1) h in
  int64x2_of_int64s l h
;;

(* Identity *)
let () =
  let v = int64x2_of_int64s 1L 2L in
  let v = Sys.opaque_identity v in
  check v 1L 2L
;;

(* Identity fn *)
let () =
  let v = int64x2_of_int64s 1L 2L in
  let[@inline never] id v = v in
  let v = id v in
  check v 1L 2L
;;

(* Pass to function *)
let () =
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let v = combine v0 v1 in
  check v 4L 6L
;;

(* Pass to function (inlined) *)
let () =
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let v = (combine[@inlined hint]) v0 v1 in
  check v 4L 6L
;;

(* Pass to function with floats *)
let () =
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let f0 = Sys.opaque_identity 5. in
  let v = combine_with_floats v0 f0 v1 6. in
  check v 9L 12L
;;

(* Pass to function with floats (inlined) *)
let () =
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let v = (combine_with_floats[@inlined hint]) v0 5. v1 6. in
  check v 9L 12L
;;

(* Capture in closure *)
let () =
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let f = combine v0 in
  let f = Sys.opaque_identity f in
  let v = f v1 in
  check v 4L 6L
;;

(* Capture vectors and floats in a closure *)
let () =
  let[@inline never] f v0 v1 f0 v2 f1 v3 =
    combine (combine_with_floats v0 f0 v1 f1) (combine v2 v3)
  in
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let v2 = int64x2_of_int64s 4L 5L in
  let v3 = int64x2_of_int64s 6L 7L in
  let f = f v0 v1 7. v2  in
  let f = Sys.opaque_identity f in
  let v = f 8. v3 in
  check v 21L 26L
;;

(* Capture vectors and floats in a closure (inlined) *)
let () =
  let[@inline always] f v0 v1 f0 v2 f1 v3 =
    (combine[@inlined hint])
      ((combine_with_floats[@inlined hint]) v0 f0 v1 f1)
      ((combine[@inlined hint]) v2 v3)
  in
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let v2 = int64x2_of_int64s 4L 5L in
  let v3 = int64x2_of_int64s 6L 7L in
  let f = f v0 v1 7. v2 in
  let v = f 8. v3 in
  check v 21L 26L
;;

(* Store in record *)
type record = { a : int64x2
              ; mutable b : int64x2
              ; c : float# }

let () =
  let record = { a = int64x2_of_int64s 1L 2L; b = int64x2_of_int64s 3L 4L; c = #5. } in
  check record.a 1L 2L;
  check record.b 3L 4L;
  let record = Sys.opaque_identity record in
  record.b <- int64x2_of_int64s 5L 6L;
  check record.a 1L 2L;
  check record.b 5L 6L;
  let v = combine_with_floats record.a (box_float record.c) record.b 6. in
  check v 11L 14L
;;

(* Store in variant *)
type variant = A of int64x2 | B of int64x2 | C of float

let () =
  let variant = A (int64x2_of_int64s 1L 2L) in
  let variant = Sys.opaque_identity variant in
  match variant with
  | A v -> check v 1L 2L
  | _ -> print_endline "fail";
  let variant = ref (C 5.) in
  let variant = Sys.opaque_identity variant in
  variant := B (int64x2_of_int64s 3L 4L);
  match !variant with
  | B v -> check v 3L 4L
  | _ -> print_endline "fail"
;;

(* Pass lots of vectors to an external *)
external lots_of_vectors :
  int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 ->
  int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 -> int64x2 ->
  int64x2
  = "" "lots_of_vectors" [@@noalloc] [@@unboxed]

let () =
    let v0 = int64x2_of_int64s 1L 2L in
    let v1 = int64x2_of_int64s 3L 4L in
    let v2 = int64x2_of_int64s 5L 6L in
    let v3 = int64x2_of_int64s 7L 8L in
    let v4 = int64x2_of_int64s 9L 10L in
    let v5 = int64x2_of_int64s 11L 12L in
    let v6 = int64x2_of_int64s 13L 14L in
    let v7 = int64x2_of_int64s 15L 16L in
    let v8 = int64x2_of_int64s 17L 18L in
    let v9 = int64x2_of_int64s 19L 20L in
    let v10 = int64x2_of_int64s 21L 22L in
    let v11 = int64x2_of_int64s 23L 24L in
    let v12 = int64x2_of_int64s 25L 26L in
    let v13 = int64x2_of_int64s 27L 28L in
    let v14 = int64x2_of_int64s 29L 30L in
    let v15 = int64x2_of_int64s 31L 32L in
    let v = lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 in
    check v 256L 272L
;;

(* Pass mixed floats/vectors to an external *)
external vectors_and_floats :
  int64x2 -> float -> int64x2 -> float -> int64x2 -> float -> int64x2 -> float ->
  float -> int64x2 -> int64x2 -> float -> float -> int64x2 -> int64x2 -> float ->
  float -> float -> int64x2 -> int64x2 -> int64x2 -> float -> float -> float ->
  int64x2
  = "" "vectors_and_floats" [@@noalloc] [@@unboxed]

let () =
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let v2 = int64x2_of_int64s 5L 6L in
  let v3 = int64x2_of_int64s 7L 8L in
  let v4 = int64x2_of_int64s 9L 10L in
  let v5 = int64x2_of_int64s 11L 12L in
  let v6 = int64x2_of_int64s 13L 14L in
  let v7 = int64x2_of_int64s 15L 16L in
  let v8 = int64x2_of_int64s 17L 18L in
  let v9 = int64x2_of_int64s 19L 20L in
  let v10 = int64x2_of_int64s 21L 22L in
  let v = vectors_and_floats v0 23. v1 24. v2 25. v3 26. 27. v4 v5 28. 29. v6 v7 30. 31. 32. v8 v9 v10 33. 34. 35. in
  check v 377L 253L
;;

(* Pass mixed ints/floats/vectors to an external *)
external vectors_and_floats_and_ints :
  int64x2 -> float -> int64x2 -> int64 -> int64x2 -> float -> int64x2 -> int64 ->
  int64 -> int64x2 -> int64x2 -> float -> float -> int64x2 -> int64x2 -> int64 ->
  int64 -> float -> int64x2 -> int64x2 -> int64x2 -> int64 -> int64 -> float ->
  int64x2
  = "" "vectors_and_floats_and_ints" [@@noalloc] [@@unboxed]

let () =
  let v0 = int64x2_of_int64s 1L 2L in
  let v1 = int64x2_of_int64s 3L 4L in
  let v2 = int64x2_of_int64s 5L 6L in
  let v3 = int64x2_of_int64s 7L 8L in
  let v4 = int64x2_of_int64s 9L 10L in
  let v5 = int64x2_of_int64s 11L 12L in
  let v6 = int64x2_of_int64s 13L 14L in
  let v7 = int64x2_of_int64s 15L 16L in
  let v8 = int64x2_of_int64s 17L 18L in
  let v9 = int64x2_of_int64s 19L 20L in
  let v10 = int64x2_of_int64s 21L 22L in
  let v = vectors_and_floats_and_ints v0 23. v1 24L v2 25. v3 26L 27L v4 v5 28. 29. v6 v7 30L 31L 32. v8 v9 v10 33L 34L 35. in
  check v 377L 253L
;;
