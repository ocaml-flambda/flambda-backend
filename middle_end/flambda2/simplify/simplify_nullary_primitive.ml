(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Pierre Chambart & Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

let simplify_nullary_primitive dacc original_prim (prim : P.nullary_primitive)
    dbg ~result_var =
  match prim with
  | Invalid _result_kind -> Simplify_primitive_result.create_invalid dacc
  | Optimised_out result_kind ->
    (match Bound_var.name_mode result_var with
    | Phantom -> ()
    | Normal | In_types ->
      Misc.fatal_errorf
        "The [Optimised_out] primitive should only be used in bindings of \
         phantom variables");
    let named = Named.create_prim original_prim dbg in
    let ty = T.unknown result_kind in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
  | Probe_is_enabled { name = _ } ->
    let named = Named.create_prim original_prim dbg in
    let ty = T.any_naked_bool in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
  | Begin_region { ghost = _ } | Begin_try_region { ghost = _ } ->
    let named = Named.create_prim original_prim dbg in
    let ty = T.any_region in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
  | Enter_inlined_apply { dbg } ->
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
          DE.merge_inlined_debuginfo denv ~from_apply_expr:dbg)
    in
    let named = Named.create_simple Simple.const_unit in
    let ty = T.this_tagged_immediate Targetint_31_63.zero in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
  | Dls_get ->
    let named = Named.create_prim original_prim dbg in
    let ty = T.any_value in
    let dacc = DA.add_variable dacc result_var ty in
    Simplify_primitive_result.create named ~try_reify:false dacc
