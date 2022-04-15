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
  | Delay
  | Strict

let [@ocamlformat "disable"] print ppf dup =
  match dup with
  | Delay -> Format.fprintf ppf "duplicatable"
  | Strict -> Format.fprintf ppf "non-duplicatable"

let compare dup1 dup2 =
  match dup1, dup2 with
  | Delay, Delay -> 0
  | Delay, Strict -> -1
  | Strict, Strict -> 0
  | Strict, Delay -> 1

let join dup1 dup2 =
  match dup1, dup2 with
  | Delay, Delay -> Delay
  | Delay, Strict | Strict, Strict | Strict, Delay -> Strict
