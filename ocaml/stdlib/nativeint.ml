# 1 "nativeint.ml"
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

(* Module [Nativeint]: processor-native integers *)

external neg: (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_neg"
external add: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_add"
external sub: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_sub"
external mul: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_mul"
external div: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_div"
external rem: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_mod"
external logand: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_and"
external logor: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_or"
external logxor: (nativeint[@local_opt]) -> (nativeint[@local_opt]) -> (nativeint[@local_opt]) = "%nativeint_xor"
external shift_left: (nativeint[@local_opt]) -> int -> (nativeint[@local_opt]) = "%nativeint_lsl"
external shift_right: (nativeint[@local_opt]) -> int -> (nativeint[@local_opt]) = "%nativeint_asr"
external shift_right_logical: (nativeint[@local_opt]) -> int -> (nativeint[@local_opt]) = "%nativeint_lsr"
external of_int: int -> (nativeint[@local_opt]) = "%nativeint_of_int"
external to_int: (nativeint[@local_opt]) -> int = "%nativeint_to_int"
external of_float : float -> nativeint
  = "caml_nativeint_of_float" "caml_nativeint_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external to_float : nativeint -> float
  = "caml_nativeint_to_float" "caml_nativeint_to_float_unboxed"
  [@@unboxed] [@@noalloc]
external of_int32: int32 -> nativeint = "%nativeint_of_int32"
external to_int32: nativeint -> int32 = "%nativeint_to_int32"

let zero = 0n
let one = 1n
let minus_one = -1n
let[@inline available] succ n = add n 1n
let[@inline available] pred n = sub n 1n
let[@inline available] abs n = if n >= 0n then n else neg n
let size = Sys.word_size
let min_int = shift_left 1n (size - 1)
let max_int = sub min_int 1n
let[@inline available] lognot n = logxor n (-1n)

let unsigned_to_int =
  let max_int = of_int Stdlib.max_int in
  fun[@inline available] n ->
    if compare zero n <= 0 && compare n max_int <= 0 then
      Some (to_int n)
    else
      None

external format : string -> nativeint -> string = "caml_nativeint_format"
let[@inline available] to_string n = format "%d" n

external of_string: string -> (nativeint[@unboxed])
  = "caml_nativeint_of_string" "caml_nativeint_of_string_unboxed"

let[@inline available] of_string_opt s =
  (* TODO: expose a non-raising primitive directly. *)
  try Some (of_string s)
  with Failure _ -> None

type t = nativeint

let[@inline available] compare (x: t) (y: t) = Stdlib.compare x y
let[@inline available] equal (x: t) (y: t) = compare x y = 0

let[@inline available] unsigned_compare n m =
  compare (sub n min_int) (sub m min_int)

let[@inline available] min x y : t = if x <= y then x else y
let[@inline available] max x y : t = if x >= y then x else y

(* Unsigned division from signed division of the same
   bitness. See Warren Jr., Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3.
*)
let[@inline available] unsigned_div n d =
  if d < zero then
    if unsigned_compare n d < 0 then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_compare r d >= 0 then succ q else q

let[@inline available] unsigned_rem n d =
  sub n (mul ((unsigned_div[@inlined]) n d) d)

(* [caml_hash_exn] doesn't raise on nativeints, so it's safe for
   it to be marked as [@@noalloc].
 *)
external seeded_hash_param :
  int -> int -> int -> nativeint -> int = "caml_hash_exn" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
