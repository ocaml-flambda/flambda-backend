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

let simplify_array_set (array_kind : P.Array_kind.t) init_or_assign dacc dbg
    ~arg1 ~arg1_ty:array_ty ~arg2 ~arg2_ty:_ ~arg3 ~arg3_ty:_ ~result_var =
  let result_var' = Bound_var.var result_var in
  let elt_kind = P.Array_kind.element_kind array_kind |> K.With_subkind.kind in
  let array_kind =
    Simplify_common.specialise_array_kind dacc array_kind ~array_ty
  in
  (* CR-someday mshinwell: should do a meet on the new value too *)
  let args = [arg1; arg2; arg3] in
  match array_kind with
  | Bottom ->
    let ty = T.bottom K.value (* Unit *) in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.invalid (), env_extension, args, dacc
  | Ok array_kind ->
    let elt_kind' =
      P.Array_kind.element_kind array_kind |> K.With_subkind.kind
    in
    assert (K.equal elt_kind elt_kind');
    let prim : P.t =
      Ternary (Array_set (array_kind, init_or_assign), arg1, arg2, arg3)
    in
    let named = Named.create_prim prim dbg in
    let ty = T.unknown (P.result_kind' prim) in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.reachable named, env_extension, args, dacc

let simplify_ternary_primitive dacc (prim : P.ternary_primitive) ~arg1 ~arg1_ty
    ~arg2 ~arg2_ty ~arg3 ~arg3_ty dbg ~result_var =
  let result_var' = Bound_var.var result_var in
  match prim with
  | Array_set (array_kind, init_or_assign) ->
    simplify_array_set array_kind init_or_assign dacc dbg ~arg1 ~arg1_ty ~arg2
      ~arg2_ty ~arg3 ~arg3_ty ~result_var
  | Block_set _ | Bytes_or_bigstring_set _ | Bigarray_set _ ->
    let prim : P.t = Ternary (prim, arg1, arg2, arg3) in
    let named = Named.create_prim prim dbg in
    let ty = T.unknown (P.result_kind' prim) in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.reachable named, env_extension, [arg1; arg2; arg3], dacc
