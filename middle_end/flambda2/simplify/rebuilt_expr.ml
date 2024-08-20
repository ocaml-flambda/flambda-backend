(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module ART = Are_rebuilding_terms

type t = Expr.t

type rebuilt_expr = t

let to_expr t are_rebuilding =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    Misc.fatal_error
      "Cannot ask [Rebuilt_expr] for the built expression when \
       [UA.do_not_rebuild_terms] is set"
  else t

let to_apply_cont t =
  match Expr.descr t with
  | Apply_cont apply_cont -> Some apply_cont
  | Let _ | Let_cont _ | Apply _ | Switch _ | Invalid _ -> None

let can_be_removed_as_invalid t are_rebuilding =
  if ART.do_not_rebuild_terms are_rebuilding
  then false
  else
    match Expr.descr t with
    | Invalid _ ->
      if Flambda_features.Debug.keep_invalid_handlers () then false else true
    | Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ -> false

let [@ocamlformat "disable"] print are_rebuilding ppf t =
  if ART.do_not_rebuild_terms are_rebuilding then
    Format.fprintf ppf "<unavailable, terms not being rebuilt>"
  else
    Expr.print ppf t

let term_not_rebuilt = Expr.create_invalid Code_not_rebuilt

let create_let are_rebuilding bound_vars defining_expr ~body ~free_names_of_body
    =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let.create bound_vars defining_expr ~body
      ~free_names_of_body:(Known free_names_of_body)
    |> Expr.create_let

let create_apply are_rebuilding apply =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else Expr.create_apply apply

let create_apply_cont apply_cont = Expr.create_apply_cont apply_cont

module Function_params_and_body = struct
  type t = Function_params_and_body.t

  let create ~return_continuation ~exn_continuation params ~body
      ~free_names_of_body ~my_closure ~my_region ~my_ghost_region ~my_depth =
    Function_params_and_body.create ~return_continuation ~exn_continuation
      params ~body ~free_names_of_body:(Known free_names_of_body) ~my_closure
      ~my_region ~my_ghost_region ~my_depth

  let to_function_params_and_body t are_rebuilding =
    if ART.do_not_rebuild_terms are_rebuilding
    then
      Misc.fatal_error
        "Cannot ask for function params and body when not rebuilding terms"
    else t

  let is_my_closure_used t = Function_params_and_body.is_my_closure_used t
end

module Continuation_handler = struct
  type t = Continuation_handler.t

  let print ~cont ~recursive ppf ch =
    Continuation_handler.print ~cont ~recursive ppf ch

  let dummy =
    Continuation_handler.create Bound_parameters.empty ~handler:term_not_rebuilt
      ~free_names_of_handler:Unknown ~is_exn_handler:false ~is_cold:false

  let create are_rebuilding params ~handler ~free_names_of_handler
      ~is_exn_handler ~is_cold =
    if ART.do_not_rebuild_terms are_rebuilding
    then dummy
    else
      Continuation_handler.create params ~handler
        ~free_names_of_handler:(Known free_names_of_handler) ~is_exn_handler
        ~is_cold

  let create' are_rebuilding params ~handler ~is_exn_handler ~is_cold =
    if ART.do_not_rebuild_terms are_rebuilding
    then dummy
    else
      Continuation_handler.create params ~handler ~free_names_of_handler:Unknown
        ~is_exn_handler ~is_cold
end

let create_non_recursive_let_cont are_rebuilding cont handler ~body
    ~free_names_of_body =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let_cont.create_non_recursive cont handler ~body
      ~free_names_of_body:(Known free_names_of_body)

let create_non_recursive_let_cont' are_rebuilding cont handler ~body
    ~num_free_occurrences_of_cont_in_body ~is_applied_with_traps =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let_cont.create_non_recursive' ~cont handler ~body
      ~num_free_occurrences_of_cont_in_body:
        (Known num_free_occurrences_of_cont_in_body) ~is_applied_with_traps

let create_non_recursive_let_cont_without_free_names are_rebuilding cont handler
    ~body =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else
    Let_cont.create_non_recursive cont handler ~body ~free_names_of_body:Unknown

let create_recursive_let_cont are_rebuilding ~invariant_params handlers ~body =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else Let_cont.create_recursive ~invariant_params handlers ~body

let create_switch are_rebuilding switch =
  if ART.do_not_rebuild_terms are_rebuilding
  then term_not_rebuilt
  else Expr.create_switch switch

let create_invalid reason = Expr.create_invalid reason

let bind_no_simplification are_rebuilding ~bindings ~body ~cost_metrics_of_body
    ~free_names_of_body =
  ListLabels.fold_left (List.rev bindings)
    ~init:(body, cost_metrics_of_body, free_names_of_body)
    ~f:(fun
         (expr, cost_metrics, free_names)
         (var, size_of_defining_expr, defining_expr)
       ->
      let expr =
        create_let are_rebuilding
          (Bound_pattern.singleton var)
          defining_expr ~body:expr ~free_names_of_body:free_names
      in
      let free_names =
        Name_occurrences.union
          (Named.free_names defining_expr)
          (Name_occurrences.remove_var free_names ~var:(Bound_var.var var))
      in
      let is_phantom = Name_mode.is_phantom (Bound_var.name_mode var) in
      let cost_metrics_of_defining_expr =
        Cost_metrics.from_size size_of_defining_expr
      in
      let cost_metrics =
        Cost_metrics.( + ) cost_metrics
          (Cost_metrics.increase_due_to_let_expr ~is_phantom
             ~cost_metrics_of_defining_expr)
      in
      expr, cost_metrics, free_names)
