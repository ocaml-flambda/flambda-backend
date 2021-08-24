(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

include Reg_width_things.Coercion

let print_with_cache ~cache:_ ppf t = print ppf t

let free_names t =
  match t with
  | Id -> Name_occurrences.empty
  | Change_depth { from; to_ } ->
    Name_occurrences.union
      (Rec_info_expr.free_names from)
      (Rec_info_expr.free_names to_)

let free_names_in_types t =
  match t with
  | Id -> Name_occurrences.empty
  | Change_depth { from; to_ } ->
    Name_occurrences.union
      (Rec_info_expr.free_names_in_types from)
      (Rec_info_expr.free_names_in_types to_)

let apply_renaming t renaming =
  match t with
  | Id -> t
  | Change_depth { from; to_  } ->
    let new_from = Rec_info_expr.apply_renaming from renaming in
    let new_to_ = Rec_info_expr.apply_renaming to_ renaming in
    if new_from == from && new_to_ == to_ then t else
      change_depth ~from:new_from ~to_:new_to_

let compose_exn t1 ~then_:t2 =
  match compose t1 ~then_:t2 with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Invalid composition: %a@ >>@ %a" print t1 print t2

let all_ids_for_export t =
  match t with
  | Id -> Ids_for_export.empty
  | Change_depth { from; to_; } ->
    Ids_for_export.union (Rec_info_expr.all_ids_for_export from)
      (Rec_info_expr.all_ids_for_export to_)
