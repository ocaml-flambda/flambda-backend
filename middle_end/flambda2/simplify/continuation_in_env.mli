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
  | Linearly_used_and_inlinable of
      { params : Kinded_parameter.t list;
            (** To avoid re-opening name abstractions, we store the opened
                parameters and handler here. Note that the properties of
                [Name_abstraction] mean that this is safe even if the expression
                were to be substituted in multiple places, with corresponding
                bindings of the [params]. There is no requirement for binders to
                use fresh names when name abstractions are being constructed;
                they just have to match the ones in the terms being closed
                over. *)
        handler : Rebuilt_expr.t;
            (** [free_names_of_handler] includes entries for any occurrences of
                the [params] in the [handler]. *)
        free_names_of_handler : Name_occurrences.t;
        cost_metrics_of_handler : Flambda.Cost_metrics.t
      }
  | Non_inlinable_zero_arity of
      { handler : Rebuilt_expr.t Or_unknown.t
            (** The handler, if available, is stored for
                [Simplify_switch_expr]. *)
      }
  | Non_inlinable_non_zero_arity of { arity : Flambda_arity.With_subkinds.t }
  | Toplevel_or_function_return_or_exn_continuation of
      { arity : Flambda_arity.With_subkinds.t }
  | Unreachable of { arity : Flambda_arity.With_subkinds.t }

val print : Format.formatter -> t -> unit

val arity : t -> Flambda_arity.With_subkinds.t
