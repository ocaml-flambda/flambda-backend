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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Linearly_used_and_inlinable of {
      params : Kinded_parameter.t list;
      handler : Rebuilt_expr.t;
      free_names_of_handler : Name_occurrences.t;
      cost_metrics_of_handler : Flambda.Cost_metrics.t;
    }
  | Non_inlinable_zero_arity of {
      handler : Rebuilt_expr.t Or_unknown.t;
    }
  | Non_inlinable_non_zero_arity of {
      arity : Flambda_arity.With_subkinds.t;
    }
  | Toplevel_or_function_return_or_exn_continuation of {
      arity : Flambda_arity.With_subkinds.t;
    }
  | Unreachable of { arity : Flambda_arity.With_subkinds.t; }

(* CR mshinwell: Write a proper printer *)
let print ppf t =
  match t with
  | Linearly_used_and_inlinable { params = _; handler = _;
      free_names_of_handler = _; cost_metrics_of_handler = _ } ->
    Format.pp_print_string ppf "Linearly_used_and_inlinable _"
  | Non_inlinable_zero_arity { handler = _; } ->
    Format.pp_print_string ppf "Non_inlinable_zero_arity _"
  | Non_inlinable_non_zero_arity { arity = _; } ->
    Format.pp_print_string ppf "Non_inlinable_non_zero_arity _"
  | Toplevel_or_function_return_or_exn_continuation { arity = _; } ->
    Format.pp_print_string ppf
      "Toplevel_or_function_return_or_exn_continuation _"
  | Unreachable { arity = _; } -> Format.pp_print_string ppf "Unreachable _"

let arity t =
  match t with
  | Linearly_used_and_inlinable { params; handler = _;
      free_names_of_handler = _; cost_metrics_of_handler = _ } ->
    Kinded_parameter.List.arity_with_subkinds params
  | Non_inlinable_zero_arity _ -> []
  | Non_inlinable_non_zero_arity { arity; }
  | Toplevel_or_function_return_or_exn_continuation { arity; }
  | Unreachable { arity; } ->
    arity
