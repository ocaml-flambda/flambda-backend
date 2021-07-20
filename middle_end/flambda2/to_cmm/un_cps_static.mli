(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Translation of statically-allocated constants to Cmm. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

val static_set_of_closures
   : Un_cps_env.t
  -> Symbol.t Closure_id.Map.t
  -> Set_of_closures.t
  -> Cmm.expression option
  -> Un_cps_env.t * Cmm.data_item list * Cmm.expression option

val static_consts
   : Un_cps_env.t
  -> Un_cps_result.t
  -> params_and_body:(
        Un_cps_env.t
     -> Un_cps_result.t
     -> string
     -> Flambda.Function_params_and_body.t
     -> Cmm.fundecl * Un_cps_result.t)
  -> Bound_symbols.t
  -> Static_const.Group.t
  -> Un_cps_env.t * Un_cps_result.t * Cmm.expression option
