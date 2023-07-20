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

type simplified_named =
  | Simple of Simple.t
  | Prim of Flambda_primitive.t * Debuginfo.t
  | Set_of_closures of Set_of_closures.t
  | Rec_info of Rec_info_expr.t

let to_named = function
  | Simple simple -> Named.create_simple simple
  | Prim (prim, dbg) -> Named.create_prim prim dbg
  | Set_of_closures set -> Named.create_set_of_closures set
  | Rec_info rec_info_expr -> Named.create_rec_info rec_info_expr

type t =
  { named : simplified_named;
    cost_metrics : Cost_metrics.t;
    free_names : Name_occurrences.t;
    kind : Flambda_kind.t
  }

let create kind (named : Named.t) =
  let (simplified_named : simplified_named), cost_metrics =
    match named with
    | Simple simple ->
      Simple simple, Cost_metrics.from_size (Code_size.simple simple)
    | Prim (prim, dbg) ->
      Prim (prim, dbg), Cost_metrics.from_size (Code_size.prim prim)
    | Set_of_closures _ ->
      Misc.fatal_errorf
        "Cannot use [Simplified_named.create] on [Set_of_closures];@ use \
         [create_with_known_free_names] instead:@ %a"
        Named.print named
    | Static_consts _ ->
      Misc.fatal_errorf
        "Cannot create [Simplified_named] from [Static_consts];@ use the \
         lifted constant infrastructure instead:@ %a"
        Named.print named
    | Rec_info rec_info_expr -> Rec_info rec_info_expr, Cost_metrics.zero
  in
  { named = simplified_named;
    cost_metrics;
    free_names = Named.free_names named;
    kind
  }

let create_with_known_free_names ~find_code_characteristics kind (named : Named.t)
    ~free_names =
  let (simplified_named : simplified_named), cost_metrics =
    match named with
    | Simple simple ->
      Simple simple, Cost_metrics.from_size (Code_size.simple simple)
    | Prim (prim, dbg) ->
      Prim (prim, dbg), Cost_metrics.from_size (Code_size.prim prim)
    | Set_of_closures set ->
      ( Set_of_closures set,
        Cost_metrics.set_of_closures ~find_code_characteristics set )
    | Static_consts _ ->
      Misc.fatal_errorf
        "Cannot create [Simplified_named] from [Static_consts];@ use the \
         lifted constant infrastructure instead:@ %a"
        Named.print named
    | Rec_info rec_info_expr -> Rec_info rec_info_expr, Cost_metrics.zero
  in
  { named = simplified_named; cost_metrics; free_names; kind }

let print ppf { named; _ } = Named.print ppf (to_named named)

let cost_metrics { cost_metrics; _ } = cost_metrics

let update_cost_metrics cost_metrics t = { t with cost_metrics }
