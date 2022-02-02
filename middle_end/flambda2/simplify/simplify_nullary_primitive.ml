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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

let simplify_nullary_primitive dacc original_prim (prim : P.nullary_primitive)
    dbg ~result_var =
  match prim with
  | Optimised_out result_kind ->
    begin
      match Name_mode.descr (Bound_var.name_mode result_var) with
      | Phantom -> ()
      | Normal | In_types ->
        Misc.fatal_errorf
          "The 'optimised_out' primitive should only be used in bindings of \
           phantom variables"
    end;
    let named = Named.create_prim original_prim dbg in
    let ty = T.unknown result_kind in
    let dacc = DA.add_variable dacc result_var ty in
    Simplified_named.reachable named ~try_reify:false, dacc
  | Probe_is_enabled { name = _ } ->
    let named = Named.create_prim original_prim dbg in
    let ty = T.any_naked_bool in
    let dacc = DA.add_variable dacc result_var ty in
    Simplified_named.reachable named ~try_reify:false, dacc
  | Begin_region ->
    let named = Named.create_prim original_prim dbg in
    let ty = T.any_region in
    let dacc = DA.add_variable dacc result_var ty in
    Simplified_named.reachable named ~try_reify:false, dacc
