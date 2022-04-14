(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Vincent Laviron, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Duplicatable
  | Not_duplicatable

let [@ocamlformat "disable"] print ppf dup =
  match dup with
  | Duplicatable -> Format.fprintf ppf "duplicatable"
  | Not_duplicatable -> Format.fprintf ppf "non-duplicatable"

let compare dup1 dup2 =
  match dup1, dup2 with
  | Duplicatable, Duplicatable -> 0
  | Duplicatable, Not_duplicatable -> -1
  | Not_duplicatable, Not_duplicatable -> 0
  | Not_duplicatable, Duplicatable -> 1

let join dup1 dup2 =
  match dup1, dup2 with
  | Duplicatable, Duplicatable -> Duplicatable
  | Duplicatable, Not_duplicatable
  | Not_duplicatable, Not_duplicatable
  | Not_duplicatable, Duplicatable ->
    Not_duplicatable
