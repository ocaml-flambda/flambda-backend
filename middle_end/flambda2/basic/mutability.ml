(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = Mutable | Immutable | Immutable_unique

let print ppf t =
  match t with
  | Mutable -> Format.pp_print_string ppf "Mutable"
  | Immutable -> Format.pp_print_string ppf "Immutable"
  | Immutable_unique ->
    Format.pp_print_string ppf "Immutable_unique"

let compare t1 t2 =
  match t1, t2 with
  | Mutable, Mutable | Immutable, Immutable
  | Immutable_unique, Immutable_unique -> 0
  | Mutable, (Immutable | Immutable_unique) -> -1
  | Immutable, Immutable_unique -> -1
  | Immutable, Mutable -> 1
  | Immutable_unique, (Mutable | Immutable) -> 1

let join t1 t2 =
  match t1, t2 with
  | Immutable, Immutable -> Immutable
  | Immutable_unique, Immutable_unique
  | Immutable, Immutable_unique
  | Immutable_unique, Immutable -> Immutable_unique
  | Mutable, (Mutable | Immutable | Immutable_unique)
  | (Immutable | Immutable_unique), Mutable -> Mutable

(* CR mshinwell: This function should be renamed, or else produce
   Lambda.mutable_flag *)
let to_lambda t : Asttypes.mutable_flag =
  match t with
  | Mutable -> Mutable
  | Immutable -> Immutable
  | Immutable_unique -> Immutable

let is_mutable t =
  match t with
  | Mutable -> true
  | Immutable | Immutable_unique -> false
