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

type 'a t =
  | Present of 'a
  | Deleted

let print print_contents ppf t =
  match t with
  | Present contents -> print_contents ppf contents
  | Deleted -> Format.pp_print_string ppf "Deleted"

let equal equal_contents t1 t2 =
  match t1, t2 with
  | Present contents1, Present contents2 -> equal_contents contents1 contents2
  | Deleted, Deleted -> true
  | Present _, Deleted | Deleted, Present _ -> false
