(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                     Anton Lorenzen, Jane Street                        *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Must_stay_here
  | May_be_pushed_down

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Must_stay_here -> Format.pp_print_string ppf "Must_stay_here"
  | May_be_pushed_down -> Format.pp_print_string ppf "May_be_pushed_down"

let compare t1 t2 =
  match t1, t2 with
  | Must_stay_here, Must_stay_here | May_be_pushed_down, May_be_pushed_down -> 0
  | Must_stay_here, May_be_pushed_down -> -1
  | May_be_pushed_down, Must_stay_here -> 1

let join t1 t2 =
  match t1, t2 with
  | May_be_pushed_down, May_be_pushed_down -> May_be_pushed_down
  | Must_stay_here, May_be_pushed_down
  | May_be_pushed_down, Must_stay_here
  | Must_stay_here, Must_stay_here ->
    Must_stay_here

let from_lambda (ubr : Lambda.unique_barrier) : t =
  match ubr with
  | May_be_pushed_down -> May_be_pushed_down
  | Must_stay_here -> Must_stay_here
