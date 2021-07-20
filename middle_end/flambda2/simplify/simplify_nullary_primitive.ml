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

let simplify_nullary_primitive dacc
      (prim : P.nullary_primitive) dbg ~result_var =
  match prim with
  | Optimised_out result_kind ->
    begin match Name_mode.descr (Var_in_binding_pos.name_mode result_var) with
    | Phantom -> ()
    | Normal | In_types ->
      Misc.fatal_errorf "The 'optimised_out' primitive should only be used \
                         in bindings of phantom variables"
    end;
    let named = Named.create_prim (Nullary prim) dbg in
    let ty = T.unknown result_kind in
    let var = Name.var (Var_in_binding_pos.var result_var) in
    let env_extension = TEE.one_equation var ty in
    Simplified_named.reachable named, env_extension, [], dacc


