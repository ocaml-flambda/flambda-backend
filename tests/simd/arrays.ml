open Stdlib

[@@@ocaml.warning "-unused-value-declaration"]
[@@@ocaml.warning "-unused-module"]

external int8x16_of_int64s : int64 -> int64 -> int8x16 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int8x16_low_int64 : int8x16 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int8x16_high_int64 : int8x16 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external int32x4_of_int64s : int64 -> int64 -> int32x4 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int32x4_low_int64 : int32x4 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int32x4_high_int64 : int32x4 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external float64x2_low_int64 : float64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float64x2_high_int64 : float64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

let eq lv hv l h =
  if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
  if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h
;;

module Bytes = struct

  external get_int8x16_unaligned : bytes -> int -> int8x16 = "%caml_bytes_getu128"
  external get_int8x16_unaligned_unsafe : bytes -> int -> int8x16 = "%caml_bytes_getu128u"

  external set_int8x16_unaligned : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128"
  external set_int8x16_unaligned_unsafe : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128u"

  let data = Bytes.of_string "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = 0x0706050403020100L
  let high = 0x0f0e0d0c0b0a0908L

  (* Getters *)

  let () =
    let v = get_int8x16_unaligned data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned_unsafe data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned_unsafe data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    for bad = 9 to 24 do
      try
        let _ = get_int8x16_unaligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Setters *)

  let set_unaligned low high offset =
    let set = int8x16_of_int64s low high in
    set_int8x16_unaligned data offset set;
    let v = get_int8x16_unaligned data offset in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let set_unaligned_unsafe low high offset =
    let set = int8x16_of_int64s low high in
    set_int8x16_unaligned_unsafe data offset set;
    let v = get_int8x16_unaligned data offset in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    set_unaligned 0x1010101010101010L 0x1010101010101010L 0;
    set_unaligned 0x2020202020202020L 0x2020202020202020L 8;
    set_unaligned_unsafe 0x3030303030303030L 0x3030303030303030L 0;
    set_unaligned_unsafe 0x4040404040404040L 0x4040404040404040L 8;
    Random.init 1234;
    for _ = 1 to 1000 do
      set_unaligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
      set_unaligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9)
    done;
  ;;

  let () =
    let set = int8x16_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
    for bad = 9 to 24 do
      try
        let _ = set_int8x16_unaligned data bad set in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;
end

module String_ = struct

  external get_int8x16_unaligned : string -> int -> int8x16 = "%caml_string_getu128"
  external get_int8x16_unaligned_unsafe : string -> int -> int8x16 = "%caml_string_getu128u"

  let data = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = 0x0706050403020100L
  let high = 0x0f0e0d0c0b0a0908L

  (* Getters *)

  let () =
    let v = get_int8x16_unaligned data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned_unsafe data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned_unsafe data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    for bad = 9 to 24 do
      try
        let _ = get_int8x16_unaligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;
end

module Bigstring = struct

  open Bigarray
  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let bigstring_of_string s =
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  external get_int8x16_unaligned : bigstring -> int -> int8x16 = "%caml_bigstring_getu128"
  external get_int8x16_unaligned_unsafe : bigstring -> int -> int8x16 = "%caml_bigstring_getu128u"
  external get_int8x16_aligned : bigstring -> int -> int8x16 = "%caml_bigstring_geta128"
  external get_int8x16_aligned_unsafe : bigstring -> int -> int8x16 = "%caml_bigstring_geta128u"

  external set_int8x16_unaligned : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_setu128"
  external set_int8x16_unaligned_unsafe : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_setu128u"
  external set_int8x16_aligned : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_seta128"
  external set_int8x16_aligned_unsafe : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_seta128u"

  (* Data is allocated off-heap, and will always be 16-byte aligned. *)
  let data = bigstring_of_string "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = 0x0706050403020100L
  let high = 0x0f0e0d0c0b0a0908L

  (* Getters *)

  let () =
    let v = get_int8x16_unaligned data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned_unsafe data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_unaligned_unsafe data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    for bad = 9 to 24 do
      try
        let _ = get_int8x16_unaligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  let () =
    let v = get_int8x16_aligned data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_aligned_unsafe data 0 in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
    for bad = 1 to 8 do
      try
        let _ = get_int8x16_aligned data bad in
        assert false
      with | Invalid_argument s when s = "address was misaligned" -> ()
    done;
    for bad = 9 to 24 do
      try
        let _ = get_int8x16_aligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;

  (* Setters *)

  let set_unaligned low high offset =
    let set = int8x16_of_int64s low high in
    set_int8x16_unaligned data offset set;
    let v = get_int8x16_unaligned data offset in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let set_unaligned_unsafe low high offset =
    let set = int8x16_of_int64s low high in
    set_int8x16_unaligned_unsafe data offset set;
    let v = get_int8x16_unaligned data offset in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let set_aligned low high offset =
    let set = int8x16_of_int64s low high in
    set_int8x16_aligned data offset set;
    let v = get_int8x16_aligned data offset in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let set_aligned_unsafe low high offset =
    let set = int8x16_of_int64s low high in
    set_int8x16_aligned_unsafe data offset set;
    let v = get_int8x16_aligned_unsafe data offset in
    eq low high (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    set_unaligned 0x1010101010101010L 0x1010101010101010L 0;
    set_unaligned 0x2020202020202020L 0x2020202020202020L 8;
    set_unaligned_unsafe 0x3030303030303030L 0x3030303030303030L 0;
    set_unaligned_unsafe 0x4040404040404040L 0x4040404040404040L 8;
    set_aligned 0x5050505050505050L 0x5050505050505050L 0;
    set_aligned_unsafe 0x6060606060606060L 0x6060606060606060L 0;
    Random.init 1234;
    for _ = 1 to 1000 do
      set_unaligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
      set_unaligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) (Random.int 9);
      set_aligned (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) 0;
      set_aligned_unsafe (Random.int64 Int64.max_int) (Random.int64 Int64.max_int) 0;
    done;
  ;;

  let () =
    let set = int8x16_of_int64s 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL in
    for bad = 1 to 8 do
      try
        let _ = set_int8x16_aligned data bad set in
        assert false
      with | Invalid_argument s when s = "address was misaligned" -> ()
    done;
    for bad = 9 to 24 do
      try
        let _ = get_int8x16_aligned data bad in
        assert false
      with | Invalid_argument s when s = "index out of bounds" -> ()
    done;
  ;;
end

module Float_arrays = struct

  external interleave_low_64 : float64x2 -> float64x2 -> float64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_64"
      [@@noalloc] [@@unboxed] [@@builtin]

  external low_of : float -> float64x2 = "caml_vec128_unreachable" "caml_float64x2_low_of_float"
      [@@noalloc] [@@unboxed] [@@builtin]

  let f64x2 x y =
    let x = low_of x in
    let y = low_of y in
    interleave_low_64 x y

  external float_array_get_float64x2 : float array -> int -> float64x2 = "%caml_float_array_get128"
  external float_array_get_float64x2_unsafe : float array -> int -> float64x2 = "%caml_float_array_get128u"

  external float_iarray_get_float64x2 : float iarray -> int -> float64x2 = "%caml_float_array_get128"
  external float_iarray_get_float64x2_unsafe : float iarray -> int -> float64x2 = "%caml_float_array_get128u"

  external float_array_set_float64x2 : float array -> int -> float64x2 -> unit = "%caml_float_array_set128"
  external float_array_set_float64x2_unsafe : float array -> int -> float64x2 -> unit = "%caml_float_array_set128u"

  external floatarray_get_float64x2 : floatarray -> int -> float64x2 = "%caml_floatarray_get128"
  external floatarray_get_float64x2_unsafe : floatarray -> int -> float64x2 = "%caml_floatarray_get128u"

  external floatarray_set_float64x2 : floatarray -> int -> float64x2 -> unit = "%caml_floatarray_set128"
  external floatarray_set_float64x2_unsafe : floatarray -> int -> float64x2 -> unit = "%caml_floatarray_set128u"

  external unboxed_float_array_get_float64x2 : float# array -> int -> float64x2 = "%caml_unboxed_float_array_get128"
  external unboxed_float_array_get_float64x2_unsafe : float# array -> int -> float64x2 = "%caml_unboxed_float_array_get128u"

  external unboxed_float_array_set_float64x2 : float# array -> int -> float64x2 -> unit = "%caml_unboxed_float_array_set128"
  external unboxed_float_array_set_float64x2_unsafe : float# array -> int -> float64x2 -> unit = "%caml_unboxed_float_array_set128u"

  let float_array () = [| 0.0; 1.0; 2.0; 3.0 |]
  let float_iarray () = [: 0.0; 1.0; 2.0; 3.0 :]
  let floatarray () =
    let a = Array.Floatarray.create 4 in
    Array.Floatarray.set a 0 0.0;
    Array.Floatarray.set a 1 1.0;
    Array.Floatarray.set a 2 2.0;
    Array.Floatarray.set a 3 3.0;
    a
  ;;
  let unboxed_float_array () = [| #0.0; #1.0; #2.0; #3.0 |]

  let () =
    let float_array = float_array () in
    let _01 = f64x2 0.0 1.0 in
    let _12 = f64x2 1.0 2.0 in
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 _01) (float64x2_high_int64 _01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 _12) (float64x2_high_int64 _12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let _45 = f64x2 4.0 5.0 in
    let _67 = f64x2 6.0 7.0 in
    float_array_set_float64x2 float_array 0 _45;
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 _45) (float64x2_high_int64 _45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    float_array_set_float64x2 float_array 1 _67;
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 _67) (float64x2_high_int64 _67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let float_array = float_array () in
    let _01 = f64x2 0.0 1.0 in
    let _12 = f64x2 1.0 2.0 in
    let get = float_array_get_float64x2_unsafe float_array 0 in
    eq (float64x2_low_int64 _01) (float64x2_high_int64 _01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_array_get_float64x2_unsafe float_array 1 in
    eq (float64x2_low_int64 _12) (float64x2_high_int64 _12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let _45 = f64x2 4.0 5.0 in
    let _67 = f64x2 6.0 7.0 in
    float_array_set_float64x2_unsafe float_array 0 _45;
    let get = float_array_get_float64x2_unsafe float_array 0 in
    eq (float64x2_low_int64 _45) (float64x2_high_int64 _45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    float_array_set_float64x2_unsafe float_array 1 _67;
    let get = float_array_get_float64x2_unsafe float_array 1 in
    eq (float64x2_low_int64 _67) (float64x2_high_int64 _67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let a = float_array () in
    let _0 = f64x2 0.0 0.0 in
    let fail a i =
      try
        let _ = float_array_get_float64x2 a i in
        let _ = float_array_set_float64x2 a i _0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|0.0|] 0;
    fail [|0.0|] 1;
    fail [|0.0|] (-1)
  ;;

  let () =
    let float_array = float_array () in
    let _01 = f64x2 0.0 1.0 in
    let _12 = f64x2 1.0 2.0 in
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 _01) (float64x2_high_int64 _01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 _12) (float64x2_high_int64 _12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let _45 = f64x2 4.0 5.0 in
    let _67 = f64x2 6.0 7.0 in
    float_array_set_float64x2 float_array 0 _45;
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 _45) (float64x2_high_int64 _45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    float_array_set_float64x2 float_array 1 _67;
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 _67) (float64x2_high_int64 _67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let floatarray = floatarray () in
    let _01 = f64x2 0.0 1.0 in
    let _12 = f64x2 1.0 2.0 in
    let get = floatarray_get_float64x2_unsafe floatarray 0 in
    eq (float64x2_low_int64 _01) (float64x2_high_int64 _01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = floatarray_get_float64x2_unsafe floatarray 1 in
    eq (float64x2_low_int64 _12) (float64x2_high_int64 _12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let _45 = f64x2 4.0 5.0 in
    let _67 = f64x2 6.0 7.0 in
    floatarray_set_float64x2_unsafe floatarray 0 _45;
    let get = floatarray_get_float64x2_unsafe floatarray 0 in
    eq (float64x2_low_int64 _45) (float64x2_high_int64 _45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    floatarray_set_float64x2_unsafe floatarray 1 _67;
    let get = floatarray_get_float64x2_unsafe floatarray 1 in
    eq (float64x2_low_int64 _67) (float64x2_high_int64 _67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let a = floatarray () in
    let _0 = f64x2 0.0 0.0 in
    let fail a i =
      try
        let _ = floatarray_get_float64x2 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
      try
        let _ = floatarray_set_float64x2 a i _0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail (Array.Floatarray.create 0) 0;
    let a = Array.Floatarray.create 1 in
    Array.Floatarray.set a 0 0.0;
    fail a 0;
    fail a 1;
    fail a (-1)
  ;;

  let () =
    let float_iarray = float_iarray () in
    let _01 = f64x2 0.0 1.0 in
    let _12 = f64x2 1.0 2.0 in
    let get = float_iarray_get_float64x2_unsafe float_iarray 0 in
    eq (float64x2_low_int64 _01) (float64x2_high_int64 _01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_iarray_get_float64x2_unsafe float_iarray 1 in
    eq (float64x2_low_int64 _12) (float64x2_high_int64 _12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
  ;;

  let () =
    let a = float_iarray () in
    let _0 = f64x2 0.0 0.0 in
    let fail a i =
      try
        let _ = float_iarray_get_float64x2 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [::] 0;
    let a = [: 0.0 :] in
    fail a 0;
    fail a 1;
    fail a (-1)
  ;;

  let () =
    let unboxed_float_array = unboxed_float_array () in
    let _01 = f64x2 0.0 1.0 in
    let _12 = f64x2 1.0 2.0 in
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 0 in
    eq (float64x2_low_int64 _01) (float64x2_high_int64 _01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 1 in
    eq (float64x2_low_int64 _12) (float64x2_high_int64 _12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let _45 = f64x2 4.0 5.0 in
    let _67 = f64x2 6.0 7.0 in
    unboxed_float_array_set_float64x2 unboxed_float_array 0 _45;
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 0 in
    eq (float64x2_low_int64 _45) (float64x2_high_int64 _45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    unboxed_float_array_set_float64x2 unboxed_float_array 1 _67;
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 1 in
    eq (float64x2_low_int64 _67) (float64x2_high_int64 _67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let unboxed_float_array = unboxed_float_array () in
    let _01 = f64x2 0.0 1.0 in
    let _12 = f64x2 1.0 2.0 in
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 0 in
    eq (float64x2_low_int64 _01) (float64x2_high_int64 _01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 1 in
    eq (float64x2_low_int64 _12) (float64x2_high_int64 _12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let _45 = f64x2 4.0 5.0 in
    let _67 = f64x2 6.0 7.0 in
    unboxed_float_array_set_float64x2_unsafe unboxed_float_array 0 _45;
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 0 in
    eq (float64x2_low_int64 _45) (float64x2_high_int64 _45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    unboxed_float_array_set_float64x2_unsafe unboxed_float_array 1 _67;
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 1 in
    eq (float64x2_low_int64 _67) (float64x2_high_int64 _67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let a = unboxed_float_array () in
    let _0 = f64x2 0.0 0.0 in
    let fail a i =
      try
        let _ = unboxed_float_array_get_float64x2 a i in
        let _ = unboxed_float_array_set_float64x2 a i _0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|#0.0|] 0;
    fail [|#0.0|] 1;
    fail [|#0.0|] (-1)
  ;;
end

module Int_arrays = struct

  external int_array_get_int64x2 : int array -> int -> int64x2 = "%caml_int_array_get128"
  external int_array_get_int64x2_unsafe : int array -> int -> int64x2 = "%caml_int_array_get128u"

  external int_iarray_get_int64x2 : int iarray -> int -> int64x2 = "%caml_int_array_get128"
  external int_iarray_get_int64x2_unsafe : int iarray -> int -> int64x2 = "%caml_int_array_get128u"

  external int_array_set_int64x2 : int array -> int -> int64x2 -> unit = "%caml_int_array_set128"
  external int_array_set_int64x2_unsafe : int array -> int -> int64x2 -> unit = "%caml_int_array_set128u"

  external unboxed_int64_array_get_int64x2 : int64# array -> int -> int64x2 = "%caml_unboxed_int64_array_get128"
  external unboxed_int64_array_get_int64x2_unsafe : int64# array -> int -> int64x2 = "%caml_unboxed_int64_array_get128u"

  external unboxed_int64_array_set_int64x2 : int64# array -> int -> int64x2 -> unit = "%caml_unboxed_int64_array_set128"
  external unboxed_int64_array_set_int64x2_unsafe : int64# array -> int -> int64x2 -> unit = "%caml_unboxed_int64_array_set128u"

  external unboxed_nativeint_array_get_int64x2 : nativeint# array -> int -> int64x2 = "%caml_unboxed_nativeint_array_get128"
  external unboxed_nativeint_array_get_int64x2_unsafe : nativeint# array -> int -> int64x2 = "%caml_unboxed_nativeint_array_get128u"

  external unboxed_nativeint_array_set_int64x2 : nativeint# array -> int -> int64x2 -> unit = "%caml_unboxed_nativeint_array_set128"
  external unboxed_nativeint_array_set_int64x2_unsafe : nativeint# array -> int -> int64x2 -> unit = "%caml_unboxed_nativeint_array_set128u"

  external unboxed_int32_array_get_int32x4 : int32# array -> int -> int32x4 = "%caml_unboxed_int32_array_get128"
  external unboxed_int32_array_get_int32x4_unsafe : int32# array -> int -> int32x4 = "%caml_unboxed_int32_array_get128u"

  external unboxed_int32_array_set_int32x4 : int32# array -> int -> int32x4 -> unit = "%caml_unboxed_int32_array_set128"
  external unboxed_int32_array_set_int32x4_unsafe : int32# array -> int -> int32x4 -> unit = "%caml_unboxed_int32_array_set128u"

  let i64x2 x y = int64x2_of_int64s x y
  let i32x4 x y z w = int32x4_of_int64s Int64.(logor (shift_left (of_int32 y) 32) (of_int32 x)) Int64.(logor (shift_left (of_int32 w) 32) (of_int32 z))
  let tag i = Int64.(add (shift_left i 1) 1L)
  let int_array () = [| 0; 1; 2; 3 |]
  let int_iarray () = [: 0; 1; 2; 3 :]
  let unboxed_int64_array () = [| #0L; #1L; #2L; #3L |]
  let unboxed_nativeint_array () = [| #0n; #1n; #2n; #3n |]
  let unboxed_int32_array () = [| #0l; #1l; #2l; #3l; #4l; #5l; #6l; #7l |]

  let () =
    let int_array = int_array () in
    let _01 = i64x2 (tag 0L) (tag 1L) in
    let _12 = i64x2 (tag 1L) (tag 2L) in
    let get = int_array_get_int64x2 int_array 0 in
    eq (int64x2_low_int64 _01) (int64x2_high_int64 _01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = int_array_get_int64x2 int_array 1 in
    eq (int64x2_low_int64 _12) (int64x2_high_int64 _12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let _45 = i64x2 (tag 4L) (tag 5L) in
    let _67 = i64x2 (tag 6L) (tag 7L) in
    int_array_set_int64x2 int_array 0 _45;
    let get = int_array_get_int64x2 int_array 0 in
    eq (int64x2_low_int64 _45) (int64x2_high_int64 _45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    int_array_set_int64x2 int_array 1 _67;
    let get = int_array_get_int64x2 int_array 1 in
    eq (int64x2_low_int64 _67) (int64x2_high_int64 _67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let int_array = int_array () in
    let _01 = i64x2 (tag 0L) (tag 1L) in
    let _12 = i64x2 (tag 1L) (tag 2L) in
    let get = int_array_get_int64x2_unsafe int_array 0 in
    eq (int64x2_low_int64 _01) (int64x2_high_int64 _01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = int_array_get_int64x2_unsafe int_array 1 in
    eq (int64x2_low_int64 _12) (int64x2_high_int64 _12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let _45 = i64x2 (tag 4L) (tag 5L) in
    let _67 = i64x2 (tag 6L) (tag 7L) in
    int_array_set_int64x2 int_array 0 _45;
    let get = int_array_get_int64x2_unsafe int_array 0 in
    eq (int64x2_low_int64 _45) (int64x2_high_int64 _45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    int_array_set_int64x2 int_array 1 _67;
    let get = int_array_get_int64x2_unsafe int_array 1 in
    eq (int64x2_low_int64 _67) (int64x2_high_int64 _67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let a = int_array () in
    let _0 = i64x2 (tag 0L) (tag 0L) in
    let fail a i =
      try
        let _ = int_array_get_int64x2 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
      try
        let _ = int_array_set_int64x2 a i _0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|0|] 0;
    fail [|0|] 1;
    fail [|0|] (-1)
  ;;

  let () =
    let int_iarray = int_iarray () in
    let _01 = i64x2 (tag 0L) (tag 1L) in
    let _12 = i64x2 (tag 1L) (tag 2L) in
    let get = int_iarray_get_int64x2_unsafe int_iarray 0 in
    eq (int64x2_low_int64 _01) (int64x2_high_int64 _01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = int_iarray_get_int64x2_unsafe int_iarray 1 in
    eq (int64x2_low_int64 _12) (int64x2_high_int64 _12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
  ;;

  let () =
    let a = int_iarray () in
    let _0 = i64x2 (tag 0L) (tag 0L) in
    let fail a i =
      try
        let _ = int_iarray_get_int64x2 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [::] 0;
    fail [: 0 :] 0;
    fail [: 0 :] 1;
    fail [: 0 :] (-1)
  ;;

  let () =
    let unboxed_int64_array = unboxed_int64_array () in
    let _01 = i64x2 0L 1L in
    let _12 = i64x2 1L 2L in
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 0 in
    eq (int64x2_low_int64 _01) (int64x2_high_int64 _01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 1 in
    eq (int64x2_low_int64 _12) (int64x2_high_int64 _12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let _45 = i64x2 4L 5L in
    let _67 = i64x2 6L 7L in
    unboxed_int64_array_set_int64x2 unboxed_int64_array 0 _45;
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 0 in
    eq (int64x2_low_int64 _45) (int64x2_high_int64 _45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_int64_array_set_int64x2 unboxed_int64_array 1 _67;
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 1 in
    eq (int64x2_low_int64 _67) (int64x2_high_int64 _67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let unboxed_int64_array = unboxed_int64_array () in
    let _01 = i64x2 0L 1L in
    let _12 = i64x2 1L 2L in
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 0 in
    eq (int64x2_low_int64 _01) (int64x2_high_int64 _01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 1 in
    eq (int64x2_low_int64 _12) (int64x2_high_int64 _12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let _45 = i64x2 4L 5L in
    let _67 = i64x2 6L 7L in
    unboxed_int64_array_set_int64x2_unsafe unboxed_int64_array 0 _45;
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 0 in
    eq (int64x2_low_int64 _45) (int64x2_high_int64 _45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_int64_array_set_int64x2_unsafe unboxed_int64_array 1 _67;
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 1 in
    eq (int64x2_low_int64 _67) (int64x2_high_int64 _67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let a = unboxed_int64_array () in
    let _0 = i64x2 0L 0L in
    let fail a i =
      try
        let _ = unboxed_int64_array_get_int64x2 a i in
        let _ = unboxed_int64_array_set_int64x2 a i _0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|#0L|] 0;
    fail [|#0L|] 1;
    fail [|#0L|] (-1)
  ;;

  let () =
    let unboxed_nativeint_array = unboxed_nativeint_array () in
    let _01 = i64x2 0L 1L in
    let _12 = i64x2 1L 2L in
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 _01) (int64x2_high_int64 _01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 _12) (int64x2_high_int64 _12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let _45 = i64x2 4L 5L in
    let _67 = i64x2 6L 7L in
    unboxed_nativeint_array_set_int64x2 unboxed_nativeint_array 0 _45;
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 _45) (int64x2_high_int64 _45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_nativeint_array_set_int64x2 unboxed_nativeint_array 1 _67;
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 _67) (int64x2_high_int64 _67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let unboxed_nativeint_array = unboxed_nativeint_array () in
    let _01 = i64x2 0L 1L in
    let _12 = i64x2 1L 2L in
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 _01) (int64x2_high_int64 _01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 _12) (int64x2_high_int64 _12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let _45 = i64x2 4L 5L in
    let _67 = i64x2 6L 7L in
    unboxed_nativeint_array_set_int64x2_unsafe unboxed_nativeint_array 0 _45;
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 _45) (int64x2_high_int64 _45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_nativeint_array_set_int64x2_unsafe unboxed_nativeint_array 1 _67;
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 _67) (int64x2_high_int64 _67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let a = unboxed_nativeint_array () in
    let _0 = i64x2 0L 0L in
    let fail a i =
      try
        let _ = unboxed_nativeint_array_get_int64x2 a i in
        let _ = unboxed_nativeint_array_set_int64x2 a i _0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 3;
    fail a 4;
    fail [||] 0;
    fail [|#0n|] 0;
    fail [|#0n|] 1;
    fail [|#0n|] (-1)
  ;;

  let () =
    let unboxed_int32_array = unboxed_int32_array () in
    let _0123 = i32x4 0l 1l 2l 3l in
    let _2345 = i32x4 2l 3l 4l 5l in
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 0 in
    eq (int32x4_low_int64 _0123) (int32x4_high_int64 _0123)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 2 in
    eq (int32x4_low_int64 _2345) (int32x4_high_int64 _2345)
       (int32x4_low_int64 get) (int32x4_high_int64 get);

    let _4567 = i32x4 4l 5l 6l 7l in
    let _6789 = i32x4 6l 7l 8l 9l in
    unboxed_int32_array_set_int32x4 unboxed_int32_array 0 _4567;
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 0 in
    eq (int32x4_low_int64 _4567) (int32x4_high_int64 _4567)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    unboxed_int32_array_set_int32x4 unboxed_int32_array 1 _6789;
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 1 in
    eq (int32x4_low_int64 _6789) (int32x4_high_int64 _6789)
       (int32x4_low_int64 get) (int32x4_high_int64 get)
  ;;

  let () =
    let unboxed_int32_array = unboxed_int32_array () in
    let _0123 = i32x4 0l 1l 2l 3l in
    let _2345 = i32x4 2l 3l 4l 5l in
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 0 in
    eq (int32x4_low_int64 _0123) (int32x4_high_int64 _0123)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 2 in
    eq (int32x4_low_int64 _2345) (int32x4_high_int64 _2345)
       (int32x4_low_int64 get) (int32x4_high_int64 get);

    let _4567 = i32x4 4l 5l 6l 7l in
    let _6789 = i32x4 6l 7l 8l 9l in
    unboxed_int32_array_set_int32x4_unsafe unboxed_int32_array 0 _4567;
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 0 in
    eq (int32x4_low_int64 _4567) (int32x4_high_int64 _4567)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    unboxed_int32_array_set_int32x4_unsafe unboxed_int32_array 1 _6789;
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 1 in
    eq (int32x4_low_int64 _6789) (int32x4_high_int64 _6789)
       (int32x4_low_int64 get) (int32x4_high_int64 get)
  ;;

  let () =
    let a = unboxed_int32_array () in
    let _0 = i32x4 0l 0l 0l 0l in
    let fail a i =
      try
        let _ = unboxed_int32_array_get_int32x4 a i in
        let _ = unboxed_int32_array_set_int32x4 a i _0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 5;
    fail a 6;
    fail [||] 0;
    fail [|#0l|] 0;
    fail [|#0l|] 1;
    fail [|#0l|] 2;
    fail [|#0l|] 3;
    fail [|#0l;#1l|] 0;
    fail [|#0l;#1l|] 1;
    fail [|#0l;#1l|] 2;
    fail [|#0l;#1l|] 3;
    fail [|#0l;#1l;#2l|] 0;
    fail [|#0l;#1l;#2l|] 1;
    fail [|#0l;#1l;#2l|] 2;
    fail [|#0l;#1l;#2l|] 3;
    fail [|#0l|] (-1)
  ;;
end
