# 2 "int64.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

(* Module [Int64]: 64-bit integers *)

external neg : (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_neg"
external add : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_add"
external sub : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_sub"
external mul : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_mul"
external div : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_div"
external rem : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_mod"
external logand : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_and"
external logor : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_or"
external logxor : (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_xor"
external shift_left : (int64[@local_opt]) -> int -> (int64[@local_opt]) @@ portable = "%int64_lsl"
external shift_right : (int64[@local_opt]) -> int -> (int64[@local_opt]) @@ portable = "%int64_asr"
external shift_right_logical : (int64[@local_opt]) -> int -> (int64[@local_opt]) @@ portable = "%int64_lsr"
external of_int : int -> (int64[@local_opt]) @@ portable = "%int64_of_int"
external to_int : (int64[@local_opt]) -> int @@ portable = "%int64_to_int"
external of_float : float -> int64 @@ portable
  = "caml_int64_of_float" "caml_int64_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external to_float : int64 -> float @@ portable
  = "caml_int64_to_float" "caml_int64_to_float_unboxed"
  [@@unboxed] [@@noalloc]
external of_int32 : int32 -> int64 @@ portable = "%int64_of_int32"
external to_int32 : int64 -> int32 @@ portable = "%int64_to_int32"
external of_nativeint : nativeint -> int64 @@ portable = "%int64_of_nativeint"
external to_nativeint : int64 -> nativeint @@ portable = "%int64_to_nativeint"

let zero = 0L
let one = 1L
let minus_one = -1L
let[@inline available] succ n = add n 1L
let[@inline available] pred n = sub n 1L
let[@inline available] abs n = if n >= 0L then n else neg n
let min_int = 0x8000000000000000L
let max_int = 0x7FFFFFFFFFFFFFFFL
let[@inline available] lognot n = logxor n (-1L)

let unsigned_to_int =
  let max_int = of_int Stdlib.max_int in
  fun[@inline available] n ->
    if n >= 0L && n <= max_int then
      Some (to_int n)
    else
      None

external format : string -> int64 -> string @@ portable = "caml_int64_format"
let[@inline available] to_string n = format "%d" n

external of_string : string -> (int64[@unboxed]) @@ portable
  = "caml_int64_of_string" "caml_int64_of_string_unboxed"

let[@inline available] of_string_opt s =
  try Some (of_string s)
  with Failure _ -> None

external bits_of_float : float -> int64 @@ portable
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external float_of_bits : int64 -> float @@ portable
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

type t = int64

let[@inline available] compare (x: t) (y: t) = Stdlib.compare x y
let[@inline available] equal (x: t) (y: t) = x = y

let[@inline available] unsigned_compare n m =
  compare (sub n min_int) (sub m min_int)

let unsigned_lt n m =
  sub n min_int < sub m min_int

let[@inline available] min x y : t = if x <= y then x else y
let[@inline available] max x y : t = if x >= y then x else y

(* Unsigned division from signed division of the same bitness.
   See Warren Jr., Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3.
*)
let[@inline available] unsigned_div n d =
  if d < zero then
    if unsigned_lt n d then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_lt r d then q else succ q

let[@inline available] unsigned_rem n d =
  sub n (mul ((unsigned_div[@inlined]) n d) d)

(* [caml_hash_exn] doesn't raise on int64s, so it's safe for
   it to be marked as [@@noalloc].
 *)
external seeded_hash_param :
  int -> int -> int -> int64 -> int @@ portable = "caml_hash_exn" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
