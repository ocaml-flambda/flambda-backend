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

type t =
  | Linearly_used_and_inlinable of
      { params : Bound_parameters.t;
        handler : Rebuilt_expr.t;
        free_names_of_handler : Name_occurrences.t;
        cost_metrics_of_handler : Cost_metrics.t
      }
  | Non_inlinable_zero_arity of { handler : Rebuilt_expr.t Or_unknown.t }
  | Non_inlinable_non_zero_arity of { arity : [`Unarized] Flambda_arity.t }
  | Toplevel_or_function_return_or_exn_continuation of
      { arity : [`Unarized] Flambda_arity.t }
  | Invalid of { arity : [`Unarized] Flambda_arity.t }

let [@ocamlformat "disable"] print are_rebuilding_terms ppf t =
  match t with
  | Linearly_used_and_inlinable { params; handler;
      free_names_of_handler; cost_metrics_of_handler } ->
    Format.fprintf ppf "@[<hov 1>(Linearly_used_and_inlinable@ \
        @[<hov 1>(params@ %a)@]@ \
        @[<hov 1>(handler@ %a)@]@ \
        @[<hov 1>(free_names_of_handler@ %a)@]@ \
        @[<hov 1>(cost_metrics_of_handler@ %a)@]\
        )@]"
      Bound_parameters.print params
      (Rebuilt_expr.print are_rebuilding_terms) handler
      Name_occurrences.print free_names_of_handler
      Cost_metrics.print cost_metrics_of_handler
  | Non_inlinable_zero_arity { handler } ->
    Format.fprintf ppf "@[<hov 1>(Non_inlinable_zero_arity@ \
        @[<hov 1>(handler@ %a)@]\
        )@]"
      (Or_unknown.print (Rebuilt_expr.print are_rebuilding_terms)) handler
  | Non_inlinable_non_zero_arity { arity } ->
    Format.fprintf ppf "@[<hov 1>(Non_inlinable_non_zero_arity@ \
        @[<hov 1>(arity@ %a)@]\
        )@]"
      Flambda_arity.print arity
  | Toplevel_or_function_return_or_exn_continuation { arity } ->
    Format.fprintf ppf
      "@[<hov 1>(Toplevel_or_function_return_or_exn_continuation@ \
        @[<hov 1>(arity@ %a)@]\
        )@]"
      Flambda_arity.print arity
  | Invalid { arity } ->
    Format.fprintf ppf "@[<hov 1>(Invalid@ \
        @[<hov 1>(arity@ %a)@]\
        )@]"
      Flambda_arity.print arity

let arity t =
  match t with
  | Linearly_used_and_inlinable
      { params;
        handler = _;
        free_names_of_handler = _;
        cost_metrics_of_handler = _
      } ->
    Bound_parameters.arity params
  | Non_inlinable_zero_arity _ -> Flambda_arity.nullary
  | Non_inlinable_non_zero_arity { arity }
  | Toplevel_or_function_return_or_exn_continuation { arity }
  | Invalid { arity } ->
    arity
