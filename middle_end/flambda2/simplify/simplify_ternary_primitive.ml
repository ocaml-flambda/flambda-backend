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

open! Simplify_import

let simplify_ternary_primitive dacc (prim : P.ternary_primitive) ~arg1
    ~arg1_ty:_ ~arg2 ~arg2_ty:_ ~arg3 ~arg3_ty:_ dbg ~result_var =
  let result_var' = Bound_var.var result_var in
  match prim with
  | Block_set _ | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _ ->
    let prim : P.t = Ternary (prim, arg1, arg2, arg3) in
    let named = Named.create_prim prim dbg in
    let ty = T.unknown (P.result_kind' prim) in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.reachable named, env_extension, [arg1; arg2; arg3], dacc
