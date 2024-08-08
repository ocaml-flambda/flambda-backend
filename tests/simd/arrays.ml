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

external float32x4_low_int64 : float32x4 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external float32x4_high_int64 : float32x4 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

let eq lv hv l h =
  if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
  if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h
;;

module Bytes (Primitives : sig
  val get_int8x16_unaligned : bytes -> int -> int8x16
  val get_int8x16_unaligned_unsafe : bytes -> int -> int8x16
  val set_int8x16_unaligned : bytes -> int -> int8x16 -> unit
  val set_int8x16_unaligned_unsafe : bytes -> int -> int8x16 -> unit
end) =
struct
  open Primitives

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

module _ = Bytes(struct
  external get_int8x16_unaligned : bytes -> int -> int8x16 = "%caml_bytes_getu128"
  external get_int8x16_unaligned_unsafe : bytes -> int -> int8x16 = "%caml_bytes_getu128u"

  external set_int8x16_unaligned : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128"
  external set_int8x16_unaligned_unsafe : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128u"
end)

module _ = Bytes(struct
  external get_int8x16_unaligned : bytes -> int32# -> int8x16 = "%caml_bytes_getu128_indexed_by_int32#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x16_unaligned_unsafe : bytes -> int32# -> int8x16 = "%caml_bytes_getu128u_indexed_by_int32#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int32_u.of_int i)

  external set_int8x16_unaligned : bytes -> int32# -> int8x16 -> unit = "%caml_bytes_setu128_indexed_by_int32#"
  let set_int8x16_unaligned b i v = set_int8x16_unaligned b (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external set_int8x16_unaligned_unsafe : bytes -> int32# -> int8x16 -> unit = "%caml_bytes_setu128u_indexed_by_int32#"
  let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int32_u.of_int i) v
end)

module _ = Bytes(struct
  external get_int8x16_unaligned : bytes -> int64# -> int8x16 = "%caml_bytes_getu128_indexed_by_int64#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x16_unaligned_unsafe : bytes -> int64# -> int8x16 = "%caml_bytes_getu128u_indexed_by_int64#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int64_u.of_int i)

  external set_int8x16_unaligned : bytes -> int64# -> int8x16 -> unit = "%caml_bytes_setu128_indexed_by_int64#"
  let set_int8x16_unaligned b i v = set_int8x16_unaligned b (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external set_int8x16_unaligned_unsafe : bytes -> int64# -> int8x16 -> unit = "%caml_bytes_setu128u_indexed_by_int64#"
  let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int64_u.of_int i) v
end)

module _ = Bytes(struct
  external get_int8x16_unaligned : bytes -> nativeint# -> int8x16 = "%caml_bytes_getu128_indexed_by_nativeint#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x16_unaligned_unsafe : bytes -> nativeint# -> int8x16 = "%caml_bytes_getu128u_indexed_by_nativeint#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external set_int8x16_unaligned : bytes -> nativeint# -> int8x16 -> unit = "%caml_bytes_setu128_indexed_by_nativeint#"
  let set_int8x16_unaligned b i v = set_int8x16_unaligned b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external set_int8x16_unaligned_unsafe : bytes -> nativeint# -> int8x16 -> unit = "%caml_bytes_setu128u_indexed_by_nativeint#"
  let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
end)

module String_ (Primitives : sig
  val get_int8x16_unaligned : string -> int -> int8x16
  val get_int8x16_unaligned_unsafe : string -> int -> int8x16
end) =
struct
  open Primitives

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

module _ = String_(struct
  external get_int8x16_unaligned : string -> int -> int8x16 = "%caml_string_getu128"
  external get_int8x16_unaligned_unsafe : string -> int -> int8x16 = "%caml_string_getu128u"
end)

module _ = String_(struct
  external get_int8x16_unaligned : string -> int32# -> int8x16 = "%caml_string_getu128_indexed_by_int32#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x16_unaligned_unsafe : string -> int32# -> int8x16 = "%caml_string_getu128u_indexed_by_int32#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int32_u.of_int i)
end)

module _ = String_(struct
  external get_int8x16_unaligned : string -> int64# -> int8x16 = "%caml_string_getu128_indexed_by_int64#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x16_unaligned_unsafe : string -> int64# -> int8x16 = "%caml_string_getu128u_indexed_by_int64#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int64_u.of_int i)
end)

module _ = String_(struct
  external get_int8x16_unaligned : string -> nativeint# -> int8x16 = "%caml_string_getu128_indexed_by_nativeint#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x16_unaligned_unsafe : string -> nativeint# -> int8x16 = "%caml_string_getu128u_indexed_by_nativeint#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
end)

open struct
  open Bigarray
  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  module Bigstring (Primitives : sig
    val get_int8x16_unaligned : bigstring -> int -> int8x16
    val get_int8x16_unaligned_unsafe : bigstring -> int -> int8x16
    val get_int8x16_aligned : bigstring -> int -> int8x16
    val get_int8x16_aligned_unsafe : bigstring -> int -> int8x16

    val set_int8x16_unaligned : bigstring -> int -> int8x16 -> unit
    val set_int8x16_unaligned_unsafe : bigstring -> int -> int8x16 -> unit
    val set_int8x16_aligned : bigstring -> int -> int8x16 -> unit
    val set_int8x16_aligned_unsafe : bigstring -> int -> int8x16 -> unit
  end) =
  struct
    open Primitives

    let bigstring_of_string s =
      let a = Array1.create char c_layout (String.length s) in
      for i = 0 to String.length s - 1 do
        a.{i} <- s.[i]
      done;
      a

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

  module _ = Bigstring(struct
    external get_int8x16_unaligned : bigstring -> int -> int8x16 = "%caml_bigstring_getu128"
    external get_int8x16_unaligned_unsafe : bigstring -> int -> int8x16 = "%caml_bigstring_getu128u"
    external get_int8x16_aligned : bigstring -> int -> int8x16 = "%caml_bigstring_geta128"
    external get_int8x16_aligned_unsafe : bigstring -> int -> int8x16 = "%caml_bigstring_geta128u"

    external set_int8x16_unaligned : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_setu128"
    external set_int8x16_unaligned_unsafe : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_setu128u"
    external set_int8x16_aligned : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_seta128"
    external set_int8x16_aligned_unsafe : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_seta128u"
  end)

  module _ = Bigstring(struct
    external get_int8x16_unaligned : bigstring -> int32# -> int8x16 = "%caml_bigstring_getu128_indexed_by_int32#"
    let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x16_unaligned_unsafe : bigstring -> int32# -> int8x16 = "%caml_bigstring_getu128u_indexed_by_int32#"
    let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x16_aligned : bigstring -> int32# -> int8x16 = "%caml_bigstring_geta128_indexed_by_int32#"
    let get_int8x16_aligned b i = get_int8x16_aligned b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x16_aligned_unsafe : bigstring -> int32# -> int8x16 = "%caml_bigstring_geta128u_indexed_by_int32#"
    let get_int8x16_aligned_unsafe b i = get_int8x16_aligned_unsafe b (Stdlib_upstream_compatible.Int32_u.of_int i)

    external set_int8x16_unaligned : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_setu128_indexed_by_int32#"
    let set_int8x16_unaligned b i v = set_int8x16_unaligned b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x16_unaligned_unsafe : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_setu128u_indexed_by_int32#"
    let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x16_aligned : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_seta128_indexed_by_int32#"
    let set_int8x16_aligned b i v = set_int8x16_aligned b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x16_aligned_unsafe : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_seta128u_indexed_by_int32#"
    let set_int8x16_aligned_unsafe b i v = set_int8x16_aligned_unsafe b (Stdlib_upstream_compatible.Int32_u.of_int i) v
  end)

  module _ = Bigstring(struct
    external get_int8x16_unaligned : bigstring -> int64# -> int8x16 = "%caml_bigstring_getu128_indexed_by_int64#"
    let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x16_unaligned_unsafe : bigstring -> int64# -> int8x16 = "%caml_bigstring_getu128u_indexed_by_int64#"
    let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x16_aligned : bigstring -> int64# -> int8x16 = "%caml_bigstring_geta128_indexed_by_int64#"
    let get_int8x16_aligned b i = get_int8x16_aligned b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x16_aligned_unsafe : bigstring -> int64# -> int8x16 = "%caml_bigstring_geta128u_indexed_by_int64#"
    let get_int8x16_aligned_unsafe b i = get_int8x16_aligned_unsafe b (Stdlib_upstream_compatible.Int64_u.of_int i)

    external set_int8x16_unaligned : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_setu128_indexed_by_int64#"
    let set_int8x16_unaligned b i v = set_int8x16_unaligned b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x16_unaligned_unsafe : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_setu128u_indexed_by_int64#"
    let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x16_aligned : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_seta128_indexed_by_int64#"
    let set_int8x16_aligned b i v = set_int8x16_aligned b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x16_aligned_unsafe : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_seta128u_indexed_by_int64#"
    let set_int8x16_aligned_unsafe b i v = set_int8x16_aligned_unsafe b (Stdlib_upstream_compatible.Int64_u.of_int i) v
  end)

  module _ = Bigstring(struct
    external get_int8x16_unaligned : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_getu128_indexed_by_nativeint#"
    let get_int8x16_unaligned b i = get_int8x16_unaligned b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x16_unaligned_unsafe : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_getu128u_indexed_by_nativeint#"
    let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x16_aligned : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_geta128_indexed_by_nativeint#"
    let get_int8x16_aligned b i = get_int8x16_aligned b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x16_aligned_unsafe : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_geta128u_indexed_by_nativeint#"
    let get_int8x16_aligned_unsafe b i = get_int8x16_aligned_unsafe b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

    external set_int8x16_unaligned : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_setu128_indexed_by_nativeint#"
    let set_int8x16_unaligned b i v = set_int8x16_unaligned b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x16_unaligned_unsafe : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_setu128u_indexed_by_nativeint#"
    let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x16_aligned : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_seta128_indexed_by_nativeint#"
    let set_int8x16_aligned b i v = set_int8x16_aligned b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x16_aligned_unsafe : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_seta128u_indexed_by_nativeint#"
    let set_int8x16_aligned_unsafe b i v = set_int8x16_aligned_unsafe b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  end)
end

(* CR layouts: Test more edge-cases for unboxed indices *)

(* CR layouts: Consider generating bindings? *)

module Float_arrays = struct

  external interleave_low_32 : float32x4 -> float32x4 -> float32x4 = "caml_vec128_unreachable" "caml_sse_vec128_interleave_low_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_64s : float32x4 -> float32x4 -> float32x4 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_64 : float64x2 -> float64x2 -> float64x2 = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_of64 : float -> float64x2 = "caml_vec128_unreachable" "caml_float64x2_low_of_float"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_of32 : float32 -> float32x4 = "caml_vec128_unreachable" "caml_float32x4_low_of_float32"
    [@@noalloc] [@@unboxed] [@@builtin]

  let f64x2 x y =
    let x = low_of64 x in
    let y = low_of64 y in
    interleave_low_64 x y

  let f32x4 x y z w =
    let x = low_of32 x in
    let y = low_of32 y in
    let z = low_of32 z in
    let w = low_of32 w in
    let xy = interleave_low_32 x y in
    let zw = interleave_low_32 z w in
    interleave_low_64s xy zw

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

  external unboxed_float32_array_get_float32x4 : float32# array -> int -> float32x4 = "%caml_unboxed_float32_array_get128"
  external unboxed_float32_array_get_float32x4_unsafe : float32# array -> int -> float32x4 = "%caml_unboxed_float32_array_get128u"

  external unboxed_float32_array_set_float32x4 : float32# array -> int -> float32x4 -> unit = "%caml_unboxed_float32_array_set128"
  external unboxed_float32_array_set_float32x4_unsafe : float32# array -> int -> float32x4 -> unit = "%caml_unboxed_float32_array_set128u"

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
  let unboxed_float32_array () = [| #0.0s; #1.0s; #2.0s; #3.0s; #4.0s; #5.0s; #6.0s; #7.0s |]

  let () =
    let float_array = float_array () in
    let f_01 = f64x2 0.0 1.0 in
    let f_12 = f64x2 1.0 2.0 in
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 f_01) (float64x2_high_int64 f_01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 f_12) (float64x2_high_int64 f_12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let f_45 = f64x2 4.0 5.0 in
    let f_67 = f64x2 6.0 7.0 in
    float_array_set_float64x2 float_array 0 f_45;
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 f_45) (float64x2_high_int64 f_45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    float_array_set_float64x2 float_array 1 f_67;
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 f_67) (float64x2_high_int64 f_67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let float_array = float_array () in
    let f_01 = f64x2 0.0 1.0 in
    let f_12 = f64x2 1.0 2.0 in
    let get = float_array_get_float64x2_unsafe float_array 0 in
    eq (float64x2_low_int64 f_01) (float64x2_high_int64 f_01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_array_get_float64x2_unsafe float_array 1 in
    eq (float64x2_low_int64 f_12) (float64x2_high_int64 f_12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let f_45 = f64x2 4.0 5.0 in
    let f_67 = f64x2 6.0 7.0 in
    float_array_set_float64x2_unsafe float_array 0 f_45;
    let get = float_array_get_float64x2_unsafe float_array 0 in
    eq (float64x2_low_int64 f_45) (float64x2_high_int64 f_45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    float_array_set_float64x2_unsafe float_array 1 f_67;
    let get = float_array_get_float64x2_unsafe float_array 1 in
    eq (float64x2_low_int64 f_67) (float64x2_high_int64 f_67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let a = float_array () in
    let f_0 = f64x2 0.0 0.0 in
    let fail a i =
      try
        let _ = float_array_get_float64x2 a i in
        let _ = float_array_set_float64x2 a i f_0 in
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
    let f_01 = f64x2 0.0 1.0 in
    let f_12 = f64x2 1.0 2.0 in
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 f_01) (float64x2_high_int64 f_01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 f_12) (float64x2_high_int64 f_12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let f_45 = f64x2 4.0 5.0 in
    let f_67 = f64x2 6.0 7.0 in
    float_array_set_float64x2 float_array 0 f_45;
    let get = float_array_get_float64x2 float_array 0 in
    eq (float64x2_low_int64 f_45) (float64x2_high_int64 f_45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    float_array_set_float64x2 float_array 1 f_67;
    let get = float_array_get_float64x2 float_array 1 in
    eq (float64x2_low_int64 f_67) (float64x2_high_int64 f_67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let floatarray = floatarray () in
    let f_01 = f64x2 0.0 1.0 in
    let f_12 = f64x2 1.0 2.0 in
    let get = floatarray_get_float64x2_unsafe floatarray 0 in
    eq (float64x2_low_int64 f_01) (float64x2_high_int64 f_01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = floatarray_get_float64x2_unsafe floatarray 1 in
    eq (float64x2_low_int64 f_12) (float64x2_high_int64 f_12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let f_45 = f64x2 4.0 5.0 in
    let f_67 = f64x2 6.0 7.0 in
    floatarray_set_float64x2_unsafe floatarray 0 f_45;
    let get = floatarray_get_float64x2_unsafe floatarray 0 in
    eq (float64x2_low_int64 f_45) (float64x2_high_int64 f_45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    floatarray_set_float64x2_unsafe floatarray 1 f_67;
    let get = floatarray_get_float64x2_unsafe floatarray 1 in
    eq (float64x2_low_int64 f_67) (float64x2_high_int64 f_67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let a = floatarray () in
    let f_0 = f64x2 0.0 0.0 in
    let fail a i =
      try
        let _ = floatarray_get_float64x2 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
      try
        let _ = floatarray_set_float64x2 a i f_0 in
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
    let f_01 = f64x2 0.0 1.0 in
    let f_12 = f64x2 1.0 2.0 in
    let get = float_iarray_get_float64x2_unsafe float_iarray 0 in
    eq (float64x2_low_int64 f_01) (float64x2_high_int64 f_01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = float_iarray_get_float64x2_unsafe float_iarray 1 in
    eq (float64x2_low_int64 f_12) (float64x2_high_int64 f_12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
  ;;

  let () =
    let a = float_iarray () in
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
    let f_01 = f64x2 0.0 1.0 in
    let f_12 = f64x2 1.0 2.0 in
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 0 in
    eq (float64x2_low_int64 f_01) (float64x2_high_int64 f_01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 1 in
    eq (float64x2_low_int64 f_12) (float64x2_high_int64 f_12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let f_45 = f64x2 4.0 5.0 in
    let f_67 = f64x2 6.0 7.0 in
    unboxed_float_array_set_float64x2 unboxed_float_array 0 f_45;
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 0 in
    eq (float64x2_low_int64 f_45) (float64x2_high_int64 f_45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    unboxed_float_array_set_float64x2 unboxed_float_array 1 f_67;
    let get = unboxed_float_array_get_float64x2 unboxed_float_array 1 in
    eq (float64x2_low_int64 f_67) (float64x2_high_int64 f_67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let unboxed_float_array = unboxed_float_array () in
    let f_01 = f64x2 0.0 1.0 in
    let f_12 = f64x2 1.0 2.0 in
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 0 in
    eq (float64x2_low_int64 f_01) (float64x2_high_int64 f_01)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 1 in
    eq (float64x2_low_int64 f_12) (float64x2_high_int64 f_12)
       (float64x2_low_int64 get) (float64x2_high_int64 get);

    let f_45 = f64x2 4.0 5.0 in
    let f_67 = f64x2 6.0 7.0 in
    unboxed_float_array_set_float64x2_unsafe unboxed_float_array 0 f_45;
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 0 in
    eq (float64x2_low_int64 f_45) (float64x2_high_int64 f_45)
       (float64x2_low_int64 get) (float64x2_high_int64 get);
    unboxed_float_array_set_float64x2_unsafe unboxed_float_array 1 f_67;
    let get = unboxed_float_array_get_float64x2_unsafe unboxed_float_array 1 in
    eq (float64x2_low_int64 f_67) (float64x2_high_int64 f_67)
       (float64x2_low_int64 get) (float64x2_high_int64 get)
  ;;

  let () =
    let a = unboxed_float_array () in
    let f_0 = f64x2 0.0 0.0 in
    let fail a i =
      try
        let _ = unboxed_float_array_get_float64x2 a i in
        let _ = unboxed_float_array_set_float64x2 a i f_0 in
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

  let () =
    let unboxed_float32_array = unboxed_float32_array () in
    let f_0123 = f32x4 0.0s 1.0s 2.0s 3.0s in
    let f_3456 = f32x4 3.0s 4.0s 5.0s 6.0s in
    let get = unboxed_float32_array_get_float32x4 unboxed_float32_array 0 in
    eq (float32x4_low_int64 f_0123) (float32x4_high_int64 f_0123)
       (float32x4_low_int64 get) (float32x4_high_int64 get);
    let get = unboxed_float32_array_get_float32x4 unboxed_float32_array 3 in
    eq (float32x4_low_int64 f_3456) (float32x4_high_int64 f_3456)
       (float32x4_low_int64 get) (float32x4_high_int64 get);

    let f_89ab = f32x4 8.0s 9.0s 10.0s 11.0s in
    let f_cdef = f32x4 12.0s 13.0s 14.0s 15.0s in
    unboxed_float32_array_set_float32x4 unboxed_float32_array 0 f_89ab;
    let get = unboxed_float32_array_get_float32x4 unboxed_float32_array 0 in
    eq (float32x4_low_int64 f_89ab) (float32x4_high_int64 f_89ab)
       (float32x4_low_int64 get) (float32x4_high_int64 get);
    unboxed_float32_array_set_float32x4 unboxed_float32_array 3 f_cdef;
    let get = unboxed_float32_array_get_float32x4 unboxed_float32_array 3 in
    eq (float32x4_low_int64 f_cdef) (float32x4_high_int64 f_cdef)
       (float32x4_low_int64 get) (float32x4_high_int64 get)
  ;;

  let () =
    let unboxed_float32_array = unboxed_float32_array () in
    let f_0123 = f32x4 0.0s 1.0s 2.0s 3.0s in
    let f_3456 = f32x4 3.0s 4.0s 5.0s 6.0s in
    let get = unboxed_float32_array_get_float32x4_unsafe unboxed_float32_array 0 in
    eq (float32x4_low_int64 f_0123) (float32x4_high_int64 f_0123)
       (float32x4_low_int64 get) (float32x4_high_int64 get);
    let get = unboxed_float32_array_get_float32x4_unsafe unboxed_float32_array 3 in
    eq (float32x4_low_int64 f_3456) (float32x4_high_int64 f_3456)
       (float32x4_low_int64 get) (float32x4_high_int64 get);

    let f_89ab = f32x4 8.0s 9.0s 10.0s 11.0s in
    let f_cdef = f32x4 12.0s 13.0s 14.0s 15.0s in
    unboxed_float32_array_set_float32x4_unsafe unboxed_float32_array 0 f_89ab;
    let get = unboxed_float32_array_get_float32x4_unsafe unboxed_float32_array 0 in
    eq (float32x4_low_int64 f_89ab) (float32x4_high_int64 f_89ab)
       (float32x4_low_int64 get) (float32x4_high_int64 get);
    unboxed_float32_array_set_float32x4_unsafe unboxed_float32_array 3 f_cdef;
    let get = unboxed_float32_array_get_float32x4_unsafe unboxed_float32_array 3 in
    eq (float32x4_low_int64 f_cdef) (float32x4_high_int64 f_cdef)
       (float32x4_low_int64 get) (float32x4_high_int64 get)
  ;;

  let () =
    let a = unboxed_float32_array () in
    let f_0 = f32x4 0.0s 0.0s 0.0s 0.0s in
    let fail a i =
      try
        let _ = unboxed_float32_array_get_float32x4 a i in
        let _ = unboxed_float32_array_set_float32x4 a i f_0 in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ()
    in
    fail a (-1);
    fail a 5;
    fail a 6;
    fail [||] 0;
    fail [|#0.0s|] 0;
    fail [|#0.0s;#0.0s|] 0;
    fail [|#0.0s;#0.0s;#0.0s|] 0;
    fail [|#0.0s;#0.0s;#0.0s;#0.0s|] 1;
    fail [|#0.0s|] (-1)
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
    let i_01 = i64x2 (tag 0L) (tag 1L) in
    let i_12 = i64x2 (tag 1L) (tag 2L) in
    let get = int_array_get_int64x2 int_array 0 in
    eq (int64x2_low_int64 i_01) (int64x2_high_int64 i_01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = int_array_get_int64x2 int_array 1 in
    eq (int64x2_low_int64 i_12) (int64x2_high_int64 i_12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let i_45 = i64x2 (tag 4L) (tag 5L) in
    let i_67 = i64x2 (tag 6L) (tag 7L) in
    int_array_set_int64x2 int_array 0 i_45;
    let get = int_array_get_int64x2 int_array 0 in
    eq (int64x2_low_int64 i_45) (int64x2_high_int64 i_45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    int_array_set_int64x2 int_array 1 i_67;
    let get = int_array_get_int64x2 int_array 1 in
    eq (int64x2_low_int64 i_67) (int64x2_high_int64 i_67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let int_array = int_array () in
    let i_01 = i64x2 (tag 0L) (tag 1L) in
    let i_12 = i64x2 (tag 1L) (tag 2L) in
    let get = int_array_get_int64x2_unsafe int_array 0 in
    eq (int64x2_low_int64 i_01) (int64x2_high_int64 i_01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = int_array_get_int64x2_unsafe int_array 1 in
    eq (int64x2_low_int64 i_12) (int64x2_high_int64 i_12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let i_45 = i64x2 (tag 4L) (tag 5L) in
    let i_67 = i64x2 (tag 6L) (tag 7L) in
    int_array_set_int64x2 int_array 0 i_45;
    let get = int_array_get_int64x2_unsafe int_array 0 in
    eq (int64x2_low_int64 i_45) (int64x2_high_int64 i_45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    int_array_set_int64x2 int_array 1 i_67;
    let get = int_array_get_int64x2_unsafe int_array 1 in
    eq (int64x2_low_int64 i_67) (int64x2_high_int64 i_67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let a = int_array () in
    let i_0 = i64x2 (tag 0L) (tag 0L) in
    let fail a i =
      try
        let _ = int_array_get_int64x2 a i in
        Printf.printf "Did not fail on index %d\n" i
      with | Invalid_argument s when s = "index out of bounds" -> ();
      try
        let _ = int_array_set_int64x2 a i i_0 in
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
    let i_01 = i64x2 (tag 0L) (tag 1L) in
    let i_12 = i64x2 (tag 1L) (tag 2L) in
    let get = int_iarray_get_int64x2_unsafe int_iarray 0 in
    eq (int64x2_low_int64 i_01) (int64x2_high_int64 i_01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = int_iarray_get_int64x2_unsafe int_iarray 1 in
    eq (int64x2_low_int64 i_12) (int64x2_high_int64 i_12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
  ;;

  let () =
    let a = int_iarray () in
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
    let i_01 = i64x2 0L 1L in
    let i_12 = i64x2 1L 2L in
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 0 in
    eq (int64x2_low_int64 i_01) (int64x2_high_int64 i_01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 1 in
    eq (int64x2_low_int64 i_12) (int64x2_high_int64 i_12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let i_45 = i64x2 4L 5L in
    let i_67 = i64x2 6L 7L in
    unboxed_int64_array_set_int64x2 unboxed_int64_array 0 i_45;
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 0 in
    eq (int64x2_low_int64 i_45) (int64x2_high_int64 i_45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_int64_array_set_int64x2 unboxed_int64_array 1 i_67;
    let get = unboxed_int64_array_get_int64x2 unboxed_int64_array 1 in
    eq (int64x2_low_int64 i_67) (int64x2_high_int64 i_67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let unboxed_int64_array = unboxed_int64_array () in
    let i_01 = i64x2 0L 1L in
    let i_12 = i64x2 1L 2L in
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 0 in
    eq (int64x2_low_int64 i_01) (int64x2_high_int64 i_01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 1 in
    eq (int64x2_low_int64 i_12) (int64x2_high_int64 i_12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let i_45 = i64x2 4L 5L in
    let i_67 = i64x2 6L 7L in
    unboxed_int64_array_set_int64x2_unsafe unboxed_int64_array 0 i_45;
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 0 in
    eq (int64x2_low_int64 i_45) (int64x2_high_int64 i_45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_int64_array_set_int64x2_unsafe unboxed_int64_array 1 i_67;
    let get = unboxed_int64_array_get_int64x2_unsafe unboxed_int64_array 1 in
    eq (int64x2_low_int64 i_67) (int64x2_high_int64 i_67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let a = unboxed_int64_array () in
    let i_0 = i64x2 0L 0L in
    let fail a i =
      try
        let _ = unboxed_int64_array_get_int64x2 a i in
        let _ = unboxed_int64_array_set_int64x2 a i i_0 in
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
    let i_01 = i64x2 0L 1L in
    let i_12 = i64x2 1L 2L in
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 i_01) (int64x2_high_int64 i_01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 i_12) (int64x2_high_int64 i_12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let i_45 = i64x2 4L 5L in
    let i_67 = i64x2 6L 7L in
    unboxed_nativeint_array_set_int64x2 unboxed_nativeint_array 0 i_45;
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 i_45) (int64x2_high_int64 i_45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_nativeint_array_set_int64x2 unboxed_nativeint_array 1 i_67;
    let get = unboxed_nativeint_array_get_int64x2 unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 i_67) (int64x2_high_int64 i_67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let unboxed_nativeint_array = unboxed_nativeint_array () in
    let i_01 = i64x2 0L 1L in
    let i_12 = i64x2 1L 2L in
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 i_01) (int64x2_high_int64 i_01)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 i_12) (int64x2_high_int64 i_12)
       (int64x2_low_int64 get) (int64x2_high_int64 get);

    let i_45 = i64x2 4L 5L in
    let i_67 = i64x2 6L 7L in
    unboxed_nativeint_array_set_int64x2_unsafe unboxed_nativeint_array 0 i_45;
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 0 in
    eq (int64x2_low_int64 i_45) (int64x2_high_int64 i_45)
       (int64x2_low_int64 get) (int64x2_high_int64 get);
    unboxed_nativeint_array_set_int64x2_unsafe unboxed_nativeint_array 1 i_67;
    let get = unboxed_nativeint_array_get_int64x2_unsafe unboxed_nativeint_array 1 in
    eq (int64x2_low_int64 i_67) (int64x2_high_int64 i_67)
       (int64x2_low_int64 get) (int64x2_high_int64 get)
  ;;

  let () =
    let a = unboxed_nativeint_array () in
    let i_0 = i64x2 0L 0L in
    let fail a i =
      try
        let _ = unboxed_nativeint_array_get_int64x2 a i in
        let _ = unboxed_nativeint_array_set_int64x2 a i i_0 in
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
    let i_0123 = i32x4 0l 1l 2l 3l in
    let i_2345 = i32x4 2l 3l 4l 5l in
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 0 in
    eq (int32x4_low_int64 i_0123) (int32x4_high_int64 i_0123)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 2 in
    eq (int32x4_low_int64 i_2345) (int32x4_high_int64 i_2345)
       (int32x4_low_int64 get) (int32x4_high_int64 get);

    let i_4567 = i32x4 4l 5l 6l 7l in
    let i_6789 = i32x4 6l 7l 8l 9l in
    unboxed_int32_array_set_int32x4 unboxed_int32_array 0 i_4567;
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 0 in
    eq (int32x4_low_int64 i_4567) (int32x4_high_int64 i_4567)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    unboxed_int32_array_set_int32x4 unboxed_int32_array 1 i_6789;
    let get = unboxed_int32_array_get_int32x4 unboxed_int32_array 1 in
    eq (int32x4_low_int64 i_6789) (int32x4_high_int64 i_6789)
       (int32x4_low_int64 get) (int32x4_high_int64 get)
  ;;

  let () =
    let unboxed_int32_array = unboxed_int32_array () in
    let i_0123 = i32x4 0l 1l 2l 3l in
    let i_2345 = i32x4 2l 3l 4l 5l in
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 0 in
    eq (int32x4_low_int64 i_0123) (int32x4_high_int64 i_0123)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 2 in
    eq (int32x4_low_int64 i_2345) (int32x4_high_int64 i_2345)
       (int32x4_low_int64 get) (int32x4_high_int64 get);

    let i_4567 = i32x4 4l 5l 6l 7l in
    let i_6789 = i32x4 6l 7l 8l 9l in
    unboxed_int32_array_set_int32x4_unsafe unboxed_int32_array 0 i_4567;
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 0 in
    eq (int32x4_low_int64 i_4567) (int32x4_high_int64 i_4567)
       (int32x4_low_int64 get) (int32x4_high_int64 get);
    unboxed_int32_array_set_int32x4_unsafe unboxed_int32_array 1 i_6789;
    let get = unboxed_int32_array_get_int32x4_unsafe unboxed_int32_array 1 in
    eq (int32x4_low_int64 i_6789) (int32x4_high_int64 i_6789)
       (int32x4_low_int64 get) (int32x4_high_int64 get)
  ;;

  let () =
    let a = unboxed_int32_array () in
    let i_0 = i32x4 0l 0l 0l 0l in
    let fail a i =
      try
        let _ = unboxed_int32_array_get_int32x4 a i in
        let _ = unboxed_int32_array_set_int32x4 a i i_0 in
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
