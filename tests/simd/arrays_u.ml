open Stdlib

[@@@ocaml.warning "-unused-value-declaration"]
[@@@ocaml.warning "-unused-type-declaration"]
[@@@ocaml.warning "-unused-module"]

type int8x16 = int8x16#
type int16x8 = int16x8#
type int32x4 = int32x4#
type int64x2 = int64x2#
type float32x4 = float32x4#
type float64x2 = float64x2#

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

let assert_raises_out_of_bounds thunk =
  try
    thunk ();
    assert false
  with
  | Invalid_argument s when s = "index out of bounds" -> ()
  | _ -> assert false
;;

module Bytes (Primitives : sig
  val get_int8x16_unaligned : bytes -> int -> int8x16
  val get_int8x16_unaligned_unsafe : bytes -> int -> int8x16
  val set_int8x16_unaligned : bytes -> int -> int8x16 -> unit
  val set_int8x16_unaligned_unsafe : bytes -> int -> int8x16 -> unit
  val extra_checks : bytes -> unit
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

  (* Extra checks *)

  let () = extra_checks data
end

module _ = Bytes(struct
  external get_int8x16_unaligned : bytes -> int -> int8x16 = "%caml_bytes_getu128#"
  external get_int8x16_unaligned_unsafe : bytes -> int -> int8x16 = "%caml_bytes_getu128u#"

  external set_int8x16_unaligned : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128#"
  external set_int8x16_unaligned_unsafe : bytes -> int -> int8x16 -> unit = "%caml_bytes_setu128u#"

  let extra_checks bytes =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x16_unaligned bytes index (int8x16_of_int64s 1L 2L)))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x16_unaligned_prim : bytes -> int32# -> int8x16 = "%caml_bytes_getu128#_indexed_by_int32#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x16_unaligned_unsafe_prim : bytes -> int32# -> int8x16 = "%caml_bytes_getu128u#_indexed_by_int32#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

  external set_int8x16_unaligned_prim : bytes -> int32# -> int8x16 -> unit = "%caml_bytes_setu128#_indexed_by_int32#"
  let set_int8x16_unaligned b i v = set_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
  external set_int8x16_unaligned_unsafe_prim : bytes -> int32# -> int8x16 -> unit = "%caml_bytes_setu128u#_indexed_by_int32#"
  let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x16_unaligned_prim bytes index (int8x16_of_int64s 1L 2L)))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x16_unaligned_prim : bytes -> int64# -> int8x16 = "%caml_bytes_getu128#_indexed_by_int64#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x16_unaligned_unsafe_prim : bytes -> int64# -> int8x16 = "%caml_bytes_getu128u#_indexed_by_int64#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

  external set_int8x16_unaligned_prim : bytes -> int64# -> int8x16 -> unit = "%caml_bytes_setu128#_indexed_by_int64#"
  let set_int8x16_unaligned b i v = set_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
  external set_int8x16_unaligned_unsafe_prim : bytes -> int64# -> int8x16 -> unit = "%caml_bytes_setu128u#_indexed_by_int64#"
  let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x16_unaligned_prim bytes index (int8x16_of_int64s 1L 2L)))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = Bytes(struct
  external get_int8x16_unaligned_prim : bytes -> nativeint# -> int8x16 = "%caml_bytes_getu128#_indexed_by_nativeint#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x16_unaligned_unsafe_prim : bytes -> nativeint# -> int8x16 = "%caml_bytes_getu128u#_indexed_by_nativeint#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  external set_int8x16_unaligned_prim : bytes -> nativeint# -> int8x16 -> unit = "%caml_bytes_setu128#_indexed_by_nativeint#"
  let set_int8x16_unaligned b i v = set_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
  external set_int8x16_unaligned_unsafe_prim : bytes -> nativeint# -> int8x16 -> unit = "%caml_bytes_setu128u#_indexed_by_nativeint#"
  let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

  let extra_checks bytes =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned_prim bytes index in
          ());
        assert_raises_out_of_bounds (fun () ->
          set_int8x16_unaligned_prim bytes index (int8x16_of_int64s 1L 2L)))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module String_ (Primitives : sig
  val get_int8x16_unaligned : string -> int -> int8x16
  val get_int8x16_unaligned_unsafe : string -> int -> int8x16
  val extra_checks : string -> unit
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

  (* Extra checks *)

  let () = extra_checks data
end

module _ = String_(struct
  external get_int8x16_unaligned : string -> int -> int8x16 = "%caml_string_getu128#"
  external get_int8x16_unaligned_unsafe : string -> int -> int8x16 = "%caml_string_getu128u#"

  let extra_checks string =
    List.iter
      (fun index ->
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned string index in
          ()))
      Int.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x16_unaligned_prim : string -> int32# -> int8x16 = "%caml_string_getu128#_indexed_by_int32#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
  external get_int8x16_unaligned_unsafe_prim : string -> int32# -> int8x16 = "%caml_string_getu128u#_indexed_by_int32#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned_prim string index in
          ()))
      Int32.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x16_unaligned_prim : string -> int64# -> int8x16 = "%caml_string_getu128#_indexed_by_int64#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
  external get_int8x16_unaligned_unsafe_prim : string -> int64# -> int8x16 = "%caml_string_getu128u#_indexed_by_int64#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned_prim string index in
          ()))
      Int64.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
end)

module _ = String_(struct
  external get_int8x16_unaligned_prim : string -> nativeint# -> int8x16 = "%caml_string_getu128#_indexed_by_nativeint#"
  let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
  external get_int8x16_unaligned_unsafe_prim : string -> nativeint# -> int8x16 = "%caml_string_getu128u#_indexed_by_nativeint#"
  let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

  let extra_checks string =
    List.iter
      (fun index ->
        let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
        assert_raises_out_of_bounds (fun () ->
          let _ = get_int8x16_unaligned_prim string index in
          ()))
      Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
  ;;
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

    val extra_checks : bigstring -> unit
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

    (* Extra checks *)

    let () = extra_checks data
  end

  module _ = Bigstring(struct
    external get_int8x16_unaligned : bigstring -> int -> int8x16 = "%caml_bigstring_getu128#"
    external get_int8x16_unaligned_unsafe : bigstring -> int -> int8x16 = "%caml_bigstring_getu128u#"
    external get_int8x16_aligned : bigstring -> int -> int8x16 = "%caml_bigstring_geta128#"
    external get_int8x16_aligned_unsafe : bigstring -> int -> int8x16 = "%caml_bigstring_geta128u#"

    external set_int8x16_unaligned : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_setu128#"
    external set_int8x16_unaligned_unsafe : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_setu128u#"
    external set_int8x16_aligned : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_seta128#"
    external set_int8x16_aligned_unsafe : bigstring -> int -> int8x16 -> unit = "%caml_bigstring_seta128u#"

    let extra_checks bigstring =
      List.iter
        (fun index ->
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_unaligned bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_unaligned bigstring index (int8x16_of_int64s 1L 2L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_aligned bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_aligned bigstring index (int8x16_of_int64s 1L 2L)))
        Int.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x16_unaligned_prim : bigstring -> int32# -> int8x16 = "%caml_bigstring_getu128#_indexed_by_int32#"
    let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x16_unaligned_unsafe_prim : bigstring -> int32# -> int8x16 = "%caml_bigstring_getu128u#_indexed_by_int32#"
    let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x16_aligned_prim : bigstring -> int32# -> int8x16 = "%caml_bigstring_geta128#_indexed_by_int32#"
    let get_int8x16_aligned b i = get_int8x16_aligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)
    external get_int8x16_aligned_unsafe_prim : bigstring -> int32# -> int8x16 = "%caml_bigstring_geta128u#_indexed_by_int32#"
    let get_int8x16_aligned_unsafe b i = get_int8x16_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i)

    external set_int8x16_unaligned_prim : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_setu128#_indexed_by_int32#"
    let set_int8x16_unaligned b i v = set_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x16_unaligned_unsafe_prim : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_setu128u#_indexed_by_int32#"
    let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x16_aligned_prim : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_seta128#_indexed_by_int32#"
    let set_int8x16_aligned b i v = set_int8x16_aligned_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v
    external set_int8x16_aligned_unsafe_prim : bigstring -> int32# -> int8x16 -> unit = "%caml_bigstring_seta128u#_indexed_by_int32#"
    let set_int8x16_aligned_unsafe b i v = set_int8x16_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int32_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int32_u.of_int32 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_unaligned_prim bigstring index (int8x16_of_int64s 1L 2L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_aligned_prim bigstring index (int8x16_of_int64s 1L 2L)))
        Int32.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x16_unaligned_prim : bigstring -> int64# -> int8x16 = "%caml_bigstring_getu128#_indexed_by_int64#"
    let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x16_unaligned_unsafe_prim : bigstring -> int64# -> int8x16 = "%caml_bigstring_getu128u#_indexed_by_int64#"
    let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x16_aligned_prim : bigstring -> int64# -> int8x16 = "%caml_bigstring_geta128#_indexed_by_int64#"
    let get_int8x16_aligned b i = get_int8x16_aligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)
    external get_int8x16_aligned_unsafe_prim : bigstring -> int64# -> int8x16 = "%caml_bigstring_geta128u#_indexed_by_int64#"
    let get_int8x16_aligned_unsafe b i = get_int8x16_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i)

    external set_int8x16_unaligned_prim : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_setu128#_indexed_by_int64#"
    let set_int8x16_unaligned b i v = set_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x16_unaligned_unsafe_prim : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_setu128u#_indexed_by_int64#"
    let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x16_aligned_prim : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_seta128#_indexed_by_int64#"
    let set_int8x16_aligned b i v = set_int8x16_aligned_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v
    external set_int8x16_aligned_unsafe_prim : bigstring -> int64# -> int8x16 -> unit = "%caml_bigstring_seta128u#_indexed_by_int64#"
    let set_int8x16_aligned_unsafe b i v = set_int8x16_aligned_unsafe_prim b (Stdlib_upstream_compatible.Int64_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Int64_u.of_int64 index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_unaligned_prim bigstring index (int8x16_of_int64s 1L 2L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_aligned_prim bigstring index (int8x16_of_int64s 1L 2L)))
        Int64.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)

  module _ = Bigstring(struct
    external get_int8x16_unaligned_prim : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_getu128#_indexed_by_nativeint#"
    let get_int8x16_unaligned b i = get_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x16_unaligned_unsafe_prim : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_getu128u#_indexed_by_nativeint#"
    let get_int8x16_unaligned_unsafe b i = get_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x16_aligned_prim : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_geta128#_indexed_by_nativeint#"
    let get_int8x16_aligned b i = get_int8x16_aligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)
    external get_int8x16_aligned_unsafe_prim : bigstring -> nativeint# -> int8x16 = "%caml_bigstring_geta128u#_indexed_by_nativeint#"
    let get_int8x16_aligned_unsafe b i = get_int8x16_aligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i)

    external set_int8x16_unaligned_prim : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_setu128#_indexed_by_nativeint#"
    let set_int8x16_unaligned b i v = set_int8x16_unaligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x16_unaligned_unsafe_prim : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_setu128u#_indexed_by_nativeint#"
    let set_int8x16_unaligned_unsafe b i v = set_int8x16_unaligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x16_aligned_prim : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_seta128#_indexed_by_nativeint#"
    let set_int8x16_aligned b i v = set_int8x16_aligned_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v
    external set_int8x16_aligned_unsafe_prim : bigstring -> nativeint# -> int8x16 -> unit = "%caml_bigstring_seta128u#_indexed_by_nativeint#"
    let set_int8x16_aligned_unsafe b i v = set_int8x16_aligned_unsafe_prim b (Stdlib_upstream_compatible.Nativeint_u.of_int i) v

    let extra_checks bigstring =
      List.iter
        (fun index ->
          let index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint index in
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_unaligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_unaligned_prim bigstring index (int8x16_of_int64s 1L 2L));
          assert_raises_out_of_bounds (fun () ->
            let _ = get_int8x16_aligned_prim bigstring index in
            ());
          assert_raises_out_of_bounds (fun () ->
            set_int8x16_aligned_prim bigstring index (int8x16_of_int64s 1L 2L)))
        Nativeint.[ min_int; add min_int one; sub zero one; max_int ]
    ;;
  end)
end
