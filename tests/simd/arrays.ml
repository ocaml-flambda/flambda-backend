
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
  external get_int8x16_aligned_unsafe : bytes -> int -> int8x16 = "%caml_bytes_geta128u"

  external set_int8x16_unaligned : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128"
  external set_int8x16_unaligned_unsafe : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128u"
  external set_int8x16_aligned_unsafe : bytes -> int -> int8x16 -> unit = "%caml_bytes_seta128u"

  let data = Bytes.of_string "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = 0x0706050403020100L
  let high = 0x0f0e0d0c0b0a0908L

  (* Data is statically allocated, so won't be moved by the gc *)
  let good, bad =
    if (((Obj.magic data : int) lsr 1) lsl 2) land 15 = 0 then 0, 8 else 8, 0
  ;;

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
    let expectl, expecth = if good = 0 then low, high else high, low in
    let v = get_int8x16_aligned_unsafe data good in
    eq expectl expecth (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  (* Setters *)

  let () =
    let set = int8x16_of_int64s high low in
    set_int8x16_unaligned data 0 set;
    let v = get_int8x16_unaligned data 0 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    set_int8x16_unaligned data 8 set;
    let v = get_int8x16_unaligned data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    let set = int8x16_of_int64s high low in
    set_int8x16_unaligned_unsafe data 0 set;
    let v = get_int8x16_unaligned data 0 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    set_int8x16_unaligned_unsafe data 8 set;
    let v = get_int8x16_unaligned data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    let set = int8x16_of_int64s high low in
    set_int8x16_aligned_unsafe data good set;
    let v = get_int8x16_aligned_unsafe data good in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v)
  ;;
end

module String_ = struct

  external get_int8x16_unaligned : string -> int -> int8x16 = "%caml_string_getu128"
  external get_int8x16_unaligned_unsafe : string -> int -> int8x16 = "%caml_string_getu128u"
  external get_int8x16_aligned_unsafe : string -> int -> int8x16 = "%caml_string_geta128u"

  let data = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = 0x0706050403020100L
  let high = 0x0f0e0d0c0b0a0908L

  (* Data is statically allocated, so won't be moved by the gc *)
  let good, bad =
    if (((Obj.magic data : int) lsr 1) lsl 2) land 15 = 0 then 0, 8 else 8, 0
  ;;

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
    let expectl, expecth = if good = 0 then low, high else high, low in
    let v = get_int8x16_aligned_unsafe data good in
    eq expectl expecth (int8x16_low_int64 v) (int8x16_high_int64 v);
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

  let data = bigstring_of_string "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x00\x01\x02\x03\x04\x05\x06\x07"

  let low = 0x0706050403020100L
  let high = 0x0f0e0d0c0b0a0908L

  (* Data is allocated off-heap, so can never be moved by the gc *)
  let good, bad =
    try
      let _ = get_int8x16_aligned data 0 in
      0, 1
    with | Invalid_argument s when s = "address was misaligned" ->  1, 0
  ;;

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
    let expectl, expecth = if good = 0 then low, high else high, low in
    let v = get_int8x16_aligned data (good * 8) in
    eq expectl expecth (int8x16_low_int64 v) (int8x16_high_int64 v);
    let v = get_int8x16_aligned_unsafe data (good * 8) in
    eq expectl expecth (int8x16_low_int64 v) (int8x16_high_int64 v);
    try
      let _ = get_int8x16_aligned data (bad * 8) in
      assert false
    with | Invalid_argument s when s = "address was misaligned" -> ();
    for bad = 1 to 7 do
      try
        let _ = get_int8x16_aligned data bad in
        assert false
      with | Invalid_argument s when s = "address was misaligned" -> ()
    done;
  ;;

  (* Setters *)

  let () =
    let set = int8x16_of_int64s high low in
    set_int8x16_unaligned data 0 set;
    let v = get_int8x16_unaligned data 0 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    set_int8x16_unaligned data 8 set;
    let v = get_int8x16_unaligned data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    let set = int8x16_of_int64s high low in
    set_int8x16_unaligned_unsafe data 0 set;
    let v = get_int8x16_unaligned data 0 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    set_int8x16_unaligned_unsafe data 8 set;
    let v = get_int8x16_unaligned data 8 in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
  ;;

  let () =
    let set = int8x16_of_int64s high low in
    set_int8x16_aligned data (good * 8) set;
    let v = get_int8x16_aligned data (good * 8) in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v);
    try
      let _ = set_int8x16_aligned data (bad * 8) set in
      assert false
    with | Invalid_argument s when s = "address was misaligned" -> ();
    for bad = 1 to 7 do
      try
        let _ = set_int8x16_aligned data bad set in
        assert false
      with | Invalid_argument s when s = "address was misaligned" -> ()
    done;
  ;;

  let () =
    let set = int8x16_of_int64s high low in
    set_int8x16_aligned_unsafe data (good * 8) set;
    let v = get_int8x16_aligned_unsafe data (good * 8) in
    eq high low (int8x16_low_int64 v) (int8x16_high_int64 v)
  ;;
end
