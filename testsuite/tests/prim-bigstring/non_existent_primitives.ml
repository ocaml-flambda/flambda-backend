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

(* There are no unboxed setters for strings *)

external bigstring_set_f32_by_int64 : bigstring -> int64# -> float32# -> unit
  = "%caml_bigstring_setf32u#_indexed_by_int64#"

[%%expect
{|
external bigstring_set_f32_by_int64 : bigstring -> int64# -> float32# -> unit
  = "%caml_bigstring_setf32u#_indexed_by_int64#"
|}]

external string_set_f32_by_int64 : string -> int64# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_int64#"

[%%expect
{|
Lines 1-2, characters 0-45:
1 | external string_set_f32_by_int64 : string -> int64# -> float32# -> unit
2 |   = "%caml_string_setf32u#_indexed_by_int64#"
Error: Unknown builtin primitive "%caml_string_setf32u#_indexed_by_int64#"
|}]

(* But we do keep the boxed ones *)

external string_set64 : string -> int -> int64 -> unit = "%caml_string_set64"

[%%expect
{|
external string_set64 : string -> int -> int64 -> unit = "%caml_string_set64"
|}]
