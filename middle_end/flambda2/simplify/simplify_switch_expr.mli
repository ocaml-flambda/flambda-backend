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

val simplify_switch :
  simplify_let_with_bound_pattern:
    (simplify_expr_with_bound_pattern:
       (Bound_pattern.t * Flambda.Expr.t) Simplify_common.expr_simplifier ->
    simplify_function_body:Simplify_common.simplify_function_body ->
    Flambda.Let.t Simplify_common.expr_simplifier) ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Flambda.Switch.t Simplify_common.expr_simplifier
