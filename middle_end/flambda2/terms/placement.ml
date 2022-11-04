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

let print ppf = function
  | Delay -> Format.fprintf ppf "Delay"
  | Strict -> Format.fprintf ppf "Strict"

let compare placement1 placement2 =
  match placement1, placement2 with
  | Delay, Delay -> 0
  | Delay, Strict -> -1
  | Strict, Strict -> 0
  | Strict, Delay -> 1

let join placement1 placement2 =
  match placement1, placement2 with
  | Delay, Delay -> Delay
  | Delay, Strict | Strict, Strict | Strict, Delay -> Strict
