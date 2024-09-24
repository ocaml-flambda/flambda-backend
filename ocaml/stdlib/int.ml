(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

type t = int

let zero = 0
let one = 1
let minus_one = -1
external neg : (int[@local_opt]) -> int @@ portable = "%negint"
external add : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%addint"
external sub : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%subint"
external mul : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%mulint"
external div : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%divint"
external rem : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%modint"
external succ : (int[@local_opt]) -> int @@ portable = "%succint"
external pred : (int[@local_opt]) -> int @@ portable = "%predint"
let abs x = if x >= 0 then x else -x
let max_int = (-1) lsr 1
let min_int = max_int + 1
external logand : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%andint"
external logor : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%orint"
external logxor : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%xorint"
let lognot x = logxor x (-1)
external shift_left : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%lslint"
external shift_right : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%asrint"
external shift_right_logical : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%lsrint"
let equal : int -> int -> bool = ( = )
let compare : int -> int -> int = Stdlib.compare
let min x y : t = if x <= y then x else y
let max x y : t = if x >= y then x else y
external to_float : (int[@local_opt]) -> (float[@local_opt]) @@ portable = "%floatofint"
external of_float : (float[@local_opt]) -> int @@ portable = "%intoffloat"

(*
external int_of_string : string -> int = "caml_int_of_string"
let of_string s = try Some (int_of_string s) with Failure _ -> None
*)

external format_int : string -> int -> string @@ portable = "caml_format_int"
let to_string x = format_int "%d" x

(* [caml_hash_exn] doesn't raise on ints, so it's safe for
   it to be marked as [@@noalloc].
 *)
external seeded_hash_param :
  int -> int -> int -> int -> int @@ portable = "caml_hash_exn" [@@noalloc]
let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
