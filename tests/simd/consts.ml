open Stdlib

[@@@ocaml.warning "-unused-module"]

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

let eqf32 lv hv l h =
    let f32 = Stdlib_beta.Float32.to_float in
    if l <> lv then Printf.printf "%f <> %f\n" (f32 l) (f32 lv);
    if h <> hv then Printf.printf "%f <> %f\n" (f32 h) (f32 hv);
;;

let eqf lv hv l h =
    if l <> lv then Printf.printf "%f <> %f\n" l lv;
    if h <> hv then Printf.printf "%f <> %f\n" h hv
;;

external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]
external int32x4_low_int64 : int32x4 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int32x4_high_int64 : int32x4 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]
external int16x8_low_int64 : int16x8 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int16x8_high_int64 : int16x8 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]
external int8x16_low_int64 : int8x16 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int8x16_high_int64 : int8x16 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]
external float32x4_low_int64 : float32x4 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float32x4_high_int64 : float32x4 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]
external float64x2_low_int64 : float64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float64x2_high_int64 : float64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

module Float32x4 = struct

    type t = float32x4

    external low_to : t -> float32 = "" "caml_float32x4_low_to_float32"
        [@@noalloc] [@@unboxed] [@@builtin]

    external const1 : float32 -> t = "" "caml_float32x4_const1"
        [@@noalloc] [@@unboxed] [@@builtin]
    external const4 : float32 -> float32 -> float32 -> float32 -> t = "" "caml_float32x4_const4"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v1 = const1 1.s in
        let v2 = const1 2.s in
        let l1 = float32x4_low_int64 v1 in
        let h1 = float32x4_high_int64 v1 in
        let l2 = float32x4_low_int64 v2 in
        let h2 = float32x4_high_int64 v2 in
        eq l1 h1 0x3f8000003f800000L 0x3f8000003f800000L;
        eq l2 h2 0x4000000040000000L 0x4000000040000000L;
        let f0 = low_to v1 in
        let f1 = low_to v2 in
        eqf32 f0 f1 1.s 2.s
    ;;

    let () =
        let v1 = const4 1.s 2.s 3.s 4.s in
        let v2 = const4 5.s 6.s 7.s 8.s in
        let l1 = float32x4_low_int64 v1 in
        let h1 = float32x4_high_int64 v1 in
        let l2 = float32x4_low_int64 v2 in
        let h2 = float32x4_high_int64 v2 in
        eq l1 h1 0x400000003f800000L 0x4080000040400000L;
        eq l2 h2 0x40c0000040a00000L 0x4100000040e00000L;
        let f0 = low_to v1 in
        let f1 = low_to v2 in
        eqf32 f0 f1 1.s 5.s
    ;;
end

module Float64x2 = struct

    type t = float64x2

    external low_to : t -> float = "" "caml_float64x2_low_to_float"
        [@@noalloc] [@@unboxed] [@@builtin]

    external const1 : float -> t = "" "caml_float64x2_const1"
        [@@noalloc] [@@unboxed] [@@builtin]
    external const2 : float -> float -> t = "" "caml_float64x2_const2"
        [@@noalloc] [@@unboxed] [@@builtin]

    let () =
        let v1 = const1 1. in
        let v2 = const1 2. in
        let l1 = float64x2_low_int64 v1 in
        let h1 = float64x2_high_int64 v1 in
        let l2 = float64x2_low_int64 v2 in
        let h2 = float64x2_high_int64 v2 in
        eq l1 h1 0x3ff0000000000000L 0x3ff0000000000000L;
        eq l2 h2 0x4000000000000000L 0x4000000000000000L;
        let f0 = low_to v1 in
        let f1 = low_to v2 in
        eqf f0 f1 1. 2.
    ;;

    let () =
        let v1 = const2 1. 2. in
        let v2 = const2 3. 4. in
        let l1 = float64x2_low_int64 v1 in
        let h1 = float64x2_high_int64 v1 in
        let l2 = float64x2_low_int64 v2 in
        let h2 = float64x2_high_int64 v2 in
        eq l1 h1 0x3ff0000000000000L 0x4000000000000000L;
        eq l2 h2 0x4008000000000000L 0x4010000000000000L;
        let f0 = low_to v1 in
        let f1 = low_to v2 in
        eqf f0 f1 1. 3.
    ;;
end

module Int64x2 = struct

    type t = int64x2

    external low_to : t -> int64 = "" "caml_int64x2_low_to_int64"
        [@@noalloc] [@@unboxed] [@@builtin]

    external const1 : int64 -> t = "" "caml_int64x2_const1"
        [@@noalloc] [@@unboxed] [@@builtin]
    external const2 : int64 -> int64 -> t = "" "caml_int64x2_const2"
        [@@noalloc] [@@unboxed] [@@builtin]

    let[@inline always] check1 i =
        let v = const1 i in
        let l = int64x2_low_int64 v in
        let h = int64x2_high_int64 v in
        eq l h i i;
        let _i = low_to v in
        eq _i 0L i 0L
    ;;
    let () =
        check1 0L;
        check1 1L;
        check1 (-1L);
        check1 0xffffL;
        check1 (-0xffffL);
        check1 Int64.min_int;
        check1 Int64.max_int
    ;;
    let[@inline always] check2 i j =
        let v = const2 i j in
        let l = int64x2_low_int64 v in
        let h = int64x2_high_int64 v in
        eq l h i j;
        let _i = low_to v in
        eq _i 0L i 0L
    ;;
    let () =
        check2 0L 1L;
        check2 2L 3L;
        check2 (-1L) (-2L);
        check2 Int64.min_int Int64.max_int;
        check2 Int64.max_int Int64.min_int
    ;;
end

module Int32x4 = struct

    type t = int32x4

    external low_to : t -> int32 = "" "caml_int32x4_low_to_int32"
        [@@noalloc] [@@unboxed] [@@builtin]

    external const1 : int32 -> t = "" "caml_int32x4_const1"
        [@@noalloc] [@@unboxed] [@@builtin]
    external const4 : int32 -> int32 -> int32 -> int32 -> t = "" "caml_int32x4_const4"
        [@@noalloc] [@@unboxed] [@@builtin]

    let i32 i = Int64.(of_int32 i |> logand 0xffffffffL)
    let[@inline always] check1 i =
        let i64 = Int64.(logor (shift_left (i32 i) 32) (i32 i)) in
        let v = const1 i in
        let l = int32x4_low_int64 v in
        let h = int32x4_high_int64 v in
        eq l h i64 i64;
        let _i = low_to v in
        eql _i 0l i 0l
    ;;
    let () =
        check1 0l;
        check1 1l;
        check1 (-1l);
        check1 (0xffl);
        check1 (-0xffl);
        check1 Int32.min_int;
        check1 Int32.max_int
    ;;
    let[@inline always] check4 i j k l =
        let v = const4 i j k l in
        let _l = int32x4_low_int64 v in
        let _h = int32x4_high_int64 v in
        let ij = Int64.(logor (shift_left (i32 j) 32) (i32 i)) in
        let kl = Int64.(logor (shift_left (i32 l) 32) (i32 k)) in
        eq _l _h ij kl;
        let _i = low_to v in
        eql _i 0l i 0l
    ;;
    let () =
        check4 0l 1l 2l 3l;
        check4 3l 4l 5l 6l;
        check4 (-1l) (-2l) (-3l) (-4l);
        check4 Int32.min_int Int32.max_int Int32.min_int Int32.max_int;
        check4 Int32.max_int Int32.min_int Int32.max_int Int32.min_int
    ;;
end

module Int16x8 = struct

    type t = int16x8

    external low_to : (t [@unboxed]) -> (int [@untagged]) = "" "caml_int16x8_low_to_int"
        [@@noalloc] [@@builtin]

    external const1 : (int [@untagged]) -> (t [@unboxed]) = "" "caml_int16x8_const1"
        [@@noalloc] [@@builtin]
    external const8 : (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (t [@unboxed]) = "" "caml_int16x8_const8"
        [@@noalloc] [@@builtin]

    let i16 i = Int64.(of_int i |> logand 0xffffL)
    let[@inline always] check1 i =
        let i64 = i16 i in
        let i64 = Int64.(logor (shift_left i64 16) i64) in
        let i64 = Int64.(logor (shift_left i64 32) i64) in
        let v = const1 i in
        let l = int16x8_low_int64 v in
        let h = int16x8_high_int64 v in
        eq l h i64 i64;
        let _i = low_to v in
        eqi _i 0 i 0
    ;;
    let () =
        check1 0;
        check1 1;
        check1 0xffff;
        check1 0x8000
    ;;
    let[@inline always] check8 a b c d e f g h =
        let l64 = Int64.(logor (logor (shift_left (i16 d) 48) (shift_left (i16 c) 32))
                               (logor (shift_left (i16 b) 16) (i16 a))) in
        let h64 = Int64.(logor (logor (shift_left (i16 h) 48) (shift_left (i16 g) 32))
                               (logor (shift_left (i16 f) 16) (i16 e))) in
        let v = const8 a b c d e f g h in
        let l = int16x8_low_int64 v in
        let h = int16x8_high_int64 v in
        eq l h l64 h64;
        let _a = low_to v in
        eqi _a 0 a 0
    ;;
    let () =
        check8 0 1 2 3 4 5 6 7;
        check8 8 9 10 11 12 13 14 15;
        check8 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff 0xffff;
        check8 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000 0x8000;
        check8 0xffff 0 0xffff 0 0xffff 0 0xffff 0
    ;;
end

module Int8x16 = struct

    type t = int8x16

    external low_to : (t [@unboxed]) -> (int [@untagged]) = "" "caml_int8x16_low_to_int"
        [@@noalloc] [@@builtin]

    external const1 : (int [@untagged]) -> (t [@unboxed]) = "" "caml_int8x16_const1"
        [@@noalloc] [@@builtin]
    external const16 : (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) ->
                       (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) ->
                       (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) ->
                       (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) ->
                       (t [@unboxed]) = "" "caml_int8x16_const16"
        [@@noalloc] [@@builtin]

    let i8 i = Int64.(of_int i |> logand 0xffL)
    let[@inline always] check1 i =
        let i64 = i8 i in
        let i64 = Int64.(logor (shift_left i64 8) i64) in
        let i64 = Int64.(logor (shift_left i64 16) i64) in
        let i64 = Int64.(logor (shift_left i64 32) i64) in
        let v = const1 i in
        let l = int8x16_low_int64 v in
        let h = int8x16_high_int64 v in
        eq l h i64 i64;
        let _i = low_to v in
        eqi _i 0 i 0
    ;;
    let () =
        check1 0;
        check1 1;
        check1 0xff;
        check1 0x80
    ;;
    let[@inline always] check16 a b c d e f g h i j k l m n o p =
        let l32 = Int64.(logor (logor (shift_left (i8 d) 24) (shift_left (i8 c) 16))
                               (logor (shift_left (i8 b) 8) (i8 a))) in
        let h32 = Int64.(logor (logor (shift_left (i8 h) 24) (shift_left (i8 g) 16))
                               (logor (shift_left (i8 f) 8) (i8 e))) in
        let l64 = Int64.(logor (shift_left h32 32) l32) in
        let l32 = Int64.(logor (logor (shift_left (i8 l) 24) (shift_left (i8 k) 16))
                               (logor (shift_left (i8 j) 8) (i8 i))) in
        let h32 = Int64.(logor (logor (shift_left (i8 p) 24) (shift_left (i8 o) 16))
                               (logor (shift_left (i8 n) 8) (i8 m))) in
        let h64 = Int64.(logor (shift_left h32 32) l32) in
        let v = const16 a b c d e f g h i j k l m n o p in
        let l = int8x16_low_int64 v in
        let h = int8x16_high_int64 v in
        eq l h l64 h64;
        let _a = low_to v in
        eqi _a 0 a 0
    ;;
    let () =
        check16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15;
        check16 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31;
        check16 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff;
        check16 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80 0x80;
        check16 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0 0xff 0
    ;;
end
