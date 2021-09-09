(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

let simplify_coercion dacc (coercion : Coercion.t) =
  match coercion with
  | Id -> coercion
  | Change_depth { from; to_ } ->
    let from' = Simplify_rec_info_expr.simplify_rec_info_expr dacc from in
    let to_' = Simplify_rec_info_expr.simplify_rec_info_expr dacc to_ in
    if from' == from && to_' == to_
    then coercion
    else Coercion.change_depth ~from:from' ~to_:to_'
