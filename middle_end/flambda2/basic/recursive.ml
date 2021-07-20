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

type t = Non_recursive | Recursive

let print ppf t =
  match t with
  | Non_recursive -> Format.pp_print_string ppf "Non_recursive"
  | Recursive -> Format.pp_print_string ppf "Recursive"

let equal t1 t2 =
  match t1, t2 with
  | Non_recursive, Non_recursive
  | Recursive, Recursive -> true
  | Non_recursive, Recursive
  | Recursive, Non_recursive -> false
