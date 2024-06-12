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

type 'a t =
  | Const of 'a
  | Var of Variable.t * Debuginfo.t

let [@ocamlformat "disable"] print print_const ppf t =
  match t with
  | Const cst -> print_const ppf cst
  | Var (var, _dbg) -> Variable.print ppf var

let compare compare_const t1 t2 =
  match t1, t2 with
  | Const cst1, Const cst2 -> compare_const cst1 cst2
  | Const _, Var _ -> -1
  | Var _, Const _ -> 1
  | Var (var1, _dbg1), Var (var2, _dbg2) -> Variable.compare var1 var2

let hash hash_const t =
  match t with
  | Const cst -> Hashtbl.hash (0, hash_const cst)
  | Var (var, _dbg) -> Hashtbl.hash (1, Variable.hash var)

let free_names t =
  match t with
  | Const _ -> Name_occurrences.empty
  | Var (var, _dbg) -> Name_occurrences.singleton_variable var Name_mode.normal

let apply_renaming t renaming =
  match t with
  | Const _ -> t
  | Var (var, dbg) ->
    let var' = Renaming.apply_variable renaming var in
    if var == var' then t else Var (var', dbg)

let value_map t ~default ~f =
  match t with Const cst -> f cst | Var _ -> default
