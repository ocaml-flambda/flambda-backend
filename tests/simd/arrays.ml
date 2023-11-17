
open Stdlib

external int8x16_of_int64s : int64 -> int64 -> int8x16 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int8x16_low_int64 : int8x16 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int8x16_high_int64 : int8x16 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

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
