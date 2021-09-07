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
  | Const of 'a
  | Var of Variable.t

let [@ocamlformat "disable"] print print_const ppf t =
  match t with
  | Const cst -> print_const ppf cst
  | Var var -> Variable.print ppf var

let compare compare_const t1 t2 =
  match t1, t2 with
  | Const cst1, Const cst2 -> compare_const cst1 cst2
  | Const _, Var _ -> -1
  | Var _, Const _ -> 1
  | Var var1, Var var2 -> Variable.compare var1 var2

let free_names t =
  match t with
  | Const _ -> Name_occurrences.empty
  | Var var -> Name_occurrences.singleton_variable var Name_mode.normal

let apply_renaming t perm =
  match t with
  | Const _ -> t
  | Var var ->
    let var' = Renaming.apply_variable perm var in
    if var == var' then t
    else Var var'

let value_map t ~default ~f =
  match t with
  | Const cst -> f cst
  | Var _ -> default
