module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) = "%box_int64" [@@warning "-187"]

  let to_int x = to_int64 x |> Int64.to_int

  external of_int64 : (int64[@local_opt]) -> t = "%unbox_int64" [@@warning "-187"]

  let of_int x = Int64.of_int x |> of_int64
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
    :  onto:t
    -> from:t
    -> t
    = "ocaml_simd_unreachable" "caml_sse_vec128_high_64_to_low_64"
  [@@noalloc] [@@unboxed] [@@builtin]

  external low_64_to_high_64
    :  onto:t
    -> from:t
    -> t
    = "ocaml_simd_unreachable" "caml_sse_vec128_low_64_to_high_64"
  [@@noalloc] [@@unboxed] [@@builtin]

  let[@inline always] set a b =
    let a = low_of a in
    let b = low_of b in
    low_64_to_high_64 ~onto:a ~from:b
  ;;

  let of_ints a b = set (Int64_u.of_int a) (Int64_u.of_int b)

  let low_int t = Int64_u.to_int(low_to t)

  let high_int t = Int64_u.to_int(low_to (high_64_to_low_64 ~onto:t ~from:t))
;;
end

type t2 = {  d0 : int ;  d1: int }

let add_pairs_immutable_record (a : t2) (b: t2) : t2 =
  let sum_vector = (Int64x2.add (Int64x2.of_ints a.d0 a.d1) (Int64x2.of_ints b.d0 b.d1)) in
  { d0 = Int64x2.low_int sum_vector;
    d1 = Int64x2.high_int sum_vector }


type t4 = {  d0 : int ; d1: int ; d2 : int ; d3: int }

let add_fours_immutable_record (a : t4) (b: t4) : t4 =
  let sum_vector0 = (Int64x2.add (Int64x2.of_ints a.d0 a.d1) (Int64x2.of_ints b.d0 b.d1)) in
  let sum_vector1 = (Int64x2.add (Int64x2.of_ints a.d2 a.d3) (Int64x2.of_ints b.d2 b.d3)) in
  { d0 = Int64x2.low_int sum_vector0;
    d1 = Int64x2.high_int sum_vector0;
    d2 = Int64x2.low_int sum_vector1;
    d3 = Int64x2.high_int sum_vector1 }

let add_int_tuples (a0, a1 : int * int) (b0, b1 : int * int) =
  let sum_vector = (Int64x2.add (Int64x2.of_ints a0 a1) (Int64x2.of_ints b0 b1)) in
  (Int64x2.low_int sum_vector, Int64x2.high_int sum_vector)

let () =
  let sum_t2 = add_pairs_immutable_record { d0 = 8 ; d1 = 96 } { d0 = 80 ; d1 = 14 } in
  Printf.printf "sum_t2: { d0 = %d ; d1 = %d }\n" sum_t2.d0 sum_t2.d1;
  let sum_t4 = add_fours_immutable_record { d0 = 9 ; d1 = 12 ; d2 = 16 ; d3 = 98 } { d0 = 25 ; d1 = 85 ; d2 = 72 ; d3 = 48 } in
  Printf.printf "sum_t4: { d0 = %d ; d1 = %d ; d2 = %d ; d3 = %d }\n" sum_t4.d0 sum_t4.d1 sum_t4.d2 sum_t4.d3;
  let sum0, sum1 = add_int_tuples (48, 31) (4, 71) in
  Printf.printf "sum0: %d sum1: %d\n" sum0 sum1
;;
