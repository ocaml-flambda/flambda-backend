(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

include Reg_width_things.Rec_info_expr

let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t = print ppf t

let rec apply_renaming orig perm =
  match orig with
  | Const _ -> orig
  | Var dv ->
    let new_dv = Renaming.apply_variable perm dv in
    if dv == new_dv then orig else var new_dv
  | Succ t ->
    let new_t = apply_renaming t perm in
    if t == new_t then orig else succ new_t
  | Unroll_to (unroll_depth, t) ->
    let new_t = apply_renaming t perm in
    if t == new_t then orig else unroll_to unroll_depth new_t

let rec free_names_in_mode t mode =
  match t with
  | Const _ -> Name_occurrences.empty
  | Var dv -> Name_occurrences.singleton_variable dv Name_mode.normal
  | Succ t | Unroll_to (_, t) -> free_names_in_mode t mode

let free_names t = free_names_in_mode t Name_mode.normal

let free_names_in_types t = free_names_in_mode t Name_mode.in_types

let invariant _ _ = ()

let rec all_ids_for_export = function
  | Const _ -> Ids_for_export.empty
  | Var dv -> Ids_for_export.add_variable Ids_for_export.empty dv
  | Succ t | Unroll_to (_, t) -> all_ids_for_export t
