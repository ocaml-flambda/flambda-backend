(* TEST
 expect;
*)

open struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t
end

(* Poke an unoccupied point in the cartesian product *)

external bigstring_get_128_aligned : bigstring -> int -> int8x16
  = "%caml_bigstring_geta128"

[%%expect
{|
type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external bigstring_get_128_aligned : bigstring -> int -> int8x16
  = "%caml_bigstring_geta128"
|}]

external bigstring_get_64_unboxed : bigstring -> int -> int64#
  = "%caml_bigstring_get64#"

[%%expect
{|
external bigstring_get_64_unboxed : bigstring -> int -> int64#
  = "%caml_bigstring_get64#"
|}]

[%%expect {|
|}]

external string_get_128_aligned : string -> int -> int8x16
  = "%caml_string_geta128"

[%%expect
{|
Lines 1-2, characters 0-26:
1 | external string_get_128_aligned : string -> int -> int8x16
2 |   = "%caml_string_geta128"
Error: Unknown builtin primitive "%caml_string_geta128"
|}]

(* Systematic test for string setters *)

external set_string : string -> int -> int -> unit = "%caml_string_set16"

[%%expect
{|
external set_string : string -> int -> int -> unit = "%caml_string_set16"
|}]

external set_string : string -> nativeint# -> int -> unit
  = "%caml_string_set16_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int -> unit
  = "%caml_string_set16_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> int -> unit
  = "%caml_string_set16u_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int -> unit
  = "%caml_string_set16u_indexed_by_nativeint#"
|}]

external set_string : string -> int -> int32 -> unit = "%caml_string_set32"

[%%expect
{|
external set_string : string -> int -> int32 -> unit = "%caml_string_set32"
|}]

external set_string : string -> nativeint# -> int32 -> unit
  = "%caml_string_set32_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int32 -> unit
  = "%caml_string_set32_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_nativeint#"
|}]

external set_string : string -> int -> int64 -> unit = "%caml_string_set64"

[%%expect
{|
external set_string : string -> int -> int64 -> unit = "%caml_string_set64"
|}]

external set_string : string -> nativeint# -> int64 -> unit
  = "%caml_string_set64_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int64 -> unit
  = "%caml_string_set64_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_nativeint#"
|}]

external set_string : string -> int -> float32 -> unit =
  "%caml_string_setf32"

[%%expect
{|
external set_string : string -> int -> float32 -> unit
  = "%caml_string_setf32"
|}]

external set_string : string -> nativeint# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_nativeint#"
|}]

external set_string : string -> nativeint# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_nativeint#"

[%%expect
{|
external set_string : string -> nativeint# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_nativeint#"
|}]

external set_string : string -> int32# -> int -> unit
  = "%caml_string_set16_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int -> unit
  = "%caml_string_set16_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int -> unit
  = "%caml_string_set16u_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int -> unit
  = "%caml_string_set16u_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int32 -> unit
  = "%caml_string_set32_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int32 -> unit
  = "%caml_string_set32_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int64 -> unit
  = "%caml_string_set64_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int64 -> unit
  = "%caml_string_set64_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_int32#"
|}]

external set_string : string -> int32# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_int32#"
|}]

external set_string : string -> int32# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_int32#"
|}]

external set_string : string -> int32# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_int32#"
|}]

external set_string : string -> int32# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_int32#"
|}]

external set_string : string -> int32# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_int32#"

[%%expect
{|
external set_string : string -> int32# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_int32#"
|}]

external set_string : string -> int64# -> int -> unit
  = "%caml_string_set16_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int -> unit
  = "%caml_string_set16_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int -> unit
  = "%caml_string_set16u_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int -> unit
  = "%caml_string_set16u_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int32 -> unit
  = "%caml_string_set32_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int32 -> unit
  = "%caml_string_set32_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int64 -> unit
  = "%caml_string_set64_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int64 -> unit
  = "%caml_string_set64_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_int64#"
|}]

external set_string : string -> int64# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_int64#"
|}]

external set_string : string -> int64# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_int64#"
|}]

external set_string : string -> int64# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_int64#"
|}]

external set_string : string -> int64# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_int64#"
|}]

external set_string : string -> int64# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_int64#"

[%%expect
{|
external set_string : string -> int64# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_int64#"
|}]
