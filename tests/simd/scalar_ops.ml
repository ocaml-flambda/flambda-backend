

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "caml_vec128_unreachable" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "caml_vec128_unreachable" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "caml_vec128_unreachable" "vec128_high_int64" [@@noalloc] [@@unboxed]

let eq lv hv l h =
  if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
  if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h
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
