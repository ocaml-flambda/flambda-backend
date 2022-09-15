(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nathanaëlle Courant, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

let simple_is_my_closure dacc simple =
  Simple.pattern_match' simple
    ~const:(fun _ -> false)
    ~symbol:(fun _ ~coercion:_ -> false)
    ~var:(fun v ~coercion:_ ->
      let closure_info = DE.closure_info (DA.denv dacc) in
      match closure_info with
      | Closure { my_closure; _ } -> Variable.equal v my_closure
      | Not_in_a_closure -> false
      | In_a_set_of_closures_but_not_yet_in_a_specific_closure -> false)

let update_dacc_for_my_closure_use_simple dacc simple =
  let my_closure_used = simple_is_my_closure dacc simple in
  DA.mark_my_closure_used dacc my_closure_used

let update_dacc_for_my_closure_use_list dacc l =
  List.fold_left update_dacc_for_my_closure_use_simple dacc l

let update_dacc_for_my_closure_use_prim dacc (prim : P.t) =
  match[@ocaml.warning "-4"] prim with
  | Nullary _ -> dacc
  | Unary (Project_value_slot _, _) -> dacc
  | Unary (_, arg) -> update_dacc_for_my_closure_use_simple dacc arg
  | Binary (_, arg1, arg2) ->
    let dacc = update_dacc_for_my_closure_use_simple dacc arg1 in
    update_dacc_for_my_closure_use_simple dacc arg2
  | Ternary (_, arg1, arg2, arg3) ->
    let dacc = update_dacc_for_my_closure_use_simple dacc arg1 in
    let dacc = update_dacc_for_my_closure_use_simple dacc arg2 in
    update_dacc_for_my_closure_use_simple dacc arg3
  | Variadic (_, args) -> update_dacc_for_my_closure_use_list dacc args
