# 1 "bool.ml"
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

type t = bool = false | true

external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
let equal : bool -> bool -> bool = ( = )
let compare : bool -> bool -> int = Stdlib.compare
external to_int : bool -> int = "%identity"
let to_float = function false -> 0. | true -> 1.

(*
let of_string = function
| "false" -> Some false
| "true" -> Some true
| _ -> None
*)

let to_string = function false -> "false" | true -> "true"

(* [caml_hash_exn] doesn't raise on booleans, so it's safe for
   it to be marked as [@@noalloc].
 *)
external seeded_hash_param :
  int -> int -> int -> bool -> int = "caml_hash_exn" [@@noalloc]

let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
