(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Treat_as_unreachable
  | Halt_and_catch_fire

let print ppf t =
  match t with
  | Treat_as_unreachable -> Format.pp_print_string ppf "Treat_as_unreachable"
  | Halt_and_catch_fire -> Format.pp_print_string ppf "Halt_and_catch_fire"

let compare t1 t2 =
  match t1, t2 with
  | Treat_as_unreachable, Halt_and_catch_fire -> -1
  | Halt_and_catch_fire, Treat_as_unreachable -> 1
  | Treat_as_unreachable, Treat_as_unreachable -> 0
  | Halt_and_catch_fire, Halt_and_catch_fire -> 0
