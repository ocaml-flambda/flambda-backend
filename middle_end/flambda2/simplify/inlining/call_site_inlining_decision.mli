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

open! Flambda.Import

val make_decision :
  Downwards_acc.t ->
  simplify_expr:Expr.t Simplify_common.expr_simplifier ->
  function_type:Flambda2_types.Function_type.t ->
  apply:Apply.t ->
  return_arity:[`Unarized] Flambda_arity.t ->
  Call_site_inlining_decision_type.t

val get_rec_info :
  Downwards_acc.t ->
  function_type:Flambda2_types.Function_type.t ->
  Rec_info_expr.t
