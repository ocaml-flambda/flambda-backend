(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Default
  | Error

let print ppf t =
  match t with
  | Default -> Format.pp_print_string ppf "Default"
  | Error -> Format.pp_print_string ppf "Error"

let equal t1 t2 =
  match t1, t2 with
  | Default, Default | Error, Error -> true
  | Default, Error | Error, Default -> false

let is_default t = match t with Default -> true | Error -> false

let from_lambda (attr : Lambda.poll_attribute) =
  match attr with Default_poll -> Default | Error_poll -> Error

let to_lambda t : Lambda.poll_attribute =
  match t with Default -> Default_poll | Error -> Error_poll
