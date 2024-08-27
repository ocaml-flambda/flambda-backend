(* TEST
 expect;
*)

open struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t
end

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

external string_get_64_unboxed : string -> int -> int64#
  = "%caml_string_get64#"

[%%expect
{|
Lines 1-2, characters 0-25:
1 | external string_get_64_unboxed : string -> int -> int64#
2 |   = "%caml_string_get64#"
Error: Unknown builtin primitive "%caml_string_get64#"
|}]
