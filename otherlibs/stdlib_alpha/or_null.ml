(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Diana Kalinichenko, Jane Street, New York                *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t : value_or_null = 'a or_null [@@or_null_reexport]

let null = Null
let this v = This v
let value t ~default = match t with Null -> default | This v -> v
let get = function Null -> invalid_arg "or_null is Null" | This v -> v
let bind t f = match t with Null -> Null | This v -> f v
let map f t = match t with Null -> Null | This v -> This (f v)
let fold ~null ~this t = match t with Null -> null | This v -> this v
let iter f t = match t with Null -> () | This v -> f v
let is_null = function Null -> true | This _ -> false
let is_this = function Null -> false | This _ -> true

let equal eq t0 t1 =
  match (t0, t1) with
  | Null, Null -> true
  | This v0, This v1 -> eq v0 v1
  | _ -> false

let compare cmp t0 t1 =
  match (t0, t1) with
  | Null, Null -> 0
  | Null, This _ -> -1
  | This _, Null -> 1
  | This v0, This v1 -> cmp v0 v1

let to_result ~null = function Null -> Error null | This v -> Ok v
let to_list = function Null -> [] | This v -> [ v ]
let to_seq = function Null -> Seq.empty | This v -> Seq.return v
let to_option = function Null -> None | This v -> Some v
let of_option = function None -> Null | Some v -> This v
