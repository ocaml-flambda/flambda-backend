module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) = "%box_int64" [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> t = "%unbox_int64" [@@warning "-187"]
end

module Int64x2 = struct
  type t = int64x2
  external add : t -> t -> t = "ocaml_simd_unreachable" "caml_sse2_int64x2_add"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_of
    :  int64#
    -> (t[@unboxed])
    = "ocaml_simd_unreachable" "caml_int64x2_low_of_int64"
  [@@noalloc] [@@builtin]

  external low_to
    :  (t[@unboxed])
    -> int64#
    = "ocaml_simd_unreachable" "caml_int64x2_low_to_int64"
  [@@noalloc] [@@builtin]

  external high_64_to_low_64
    :  t
    -> t
    -> t
    = "ocaml_simd_unreachable" "caml_sse_vec128_high_64_to_low_64"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_64_to_high_64
    :  t
    -> t
    -> t
    = "ocaml_simd_unreachable" "caml_sse_vec128_low_64_to_high_64"
  [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] set a b =
    let a = low_of a in
    let b = low_of b in
    low_64_to_high_64 a b
  ;;

  let of_int64s a b = set (Int64_u.of_int64 a) (Int64_u.of_int64 b)
  let low_int64 t = Int64_u.to_int64(low_to t)
  let high_int64 t = Int64_u.to_int64(low_to (high_64_to_low_64 t t))
;;
end

let add_pairs (a0, a1 : int64 * int64) (b0, b1 : int64 * int64) =
  let sum_vector = (Int64x2.add (Int64x2.of_int64s a0 a1) (Int64x2.of_int64s b0 b1)) in
  (Int64x2.low_int64 sum_vector, Int64x2.high_int64 sum_vector)
;;

let () =
  let sum0, sum1 = add_pairs (0L, 1L) (2L, 3L) in
  Printf.printf "%Lx %Lx\n" sum0 sum1
;;
