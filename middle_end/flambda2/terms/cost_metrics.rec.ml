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

(** * * Here for legacy and debugging purposes * *)

let rec expr_size ~find_code expr size =
  match Expr.descr expr with
  | Let let_expr ->
    Let_expr.pattern_match let_expr ~f:(fun bindable_let_bound ~body ->
        let name_mode = Bindable_let_bound.name_mode bindable_let_bound in
        let size =
          if Name_mode.is_phantom name_mode
          then size
          else named_size ~find_code (Let_expr.defining_expr let_expr) size
        in
        expr_size ~find_code body size)
  | Let_cont (Non_recursive { handler; _ }) ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun _cont ~body ->
        expr_size ~find_code body size
        |> continuation_handler_size ~find_code
             (Non_recursive_let_cont_handler.handler handler))
  | Let_cont (Recursive handlers) ->
    Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body handlers ->
        let size = expr_size ~find_code body size in
        let handlers = Continuation_handlers.to_map handlers in
        Continuation.Map.fold
          (fun _cont handler size ->
            continuation_handler_size ~find_code handler size)
          handlers size)
  | Apply apply ->
    let call_cost = Code_size.apply apply |> Code_size.to_int in
    size + call_cost
  | Apply_cont e ->
    let call_cost = Code_size.apply_cont e |> Code_size.to_int in
    size + call_cost
  | Switch switch -> size + (5 * Switch.num_arms switch)
  | Invalid _ -> size

and named_size ~find_code (named : Named.t) size =
  match named with
  | Simple simple ->
    Simple.pattern_match simple
      ~const:(fun _ -> size + 1)
      ~name:(fun _ ~coercion:_ -> size)
  | Set_of_closures set_of_closures ->
    let func_decls = Set_of_closures.function_decls set_of_closures in
    let funs = Function_declarations.funs func_decls in
    let num_clos_vars =
      Set_of_closures.closure_elements set_of_closures
      |> Var_within_closure.Map.cardinal
    in
    Closure_id.Map.fold
      (fun _ code_id size ->
        let code = find_code code_id in
        let arity = List.length (Code.params_arity code) in
        let size =
          match Code.params_and_body code with
          | Present params_and_body ->
            Function_params_and_body.pattern_match params_and_body
              ~f:(fun
                   ~return_continuation:_
                   _exn_continuation
                   _params
                   ~body
                   ~my_closure:_
                   ~is_my_closure_used:_
                   ~my_depth:_
                 -> expr_size ~find_code body size)
          | Deleted -> size
        in
        size + if arity <= 1 then 2 else 3)
      funs
      (Code_size.to_int Code_size.alloc_size + num_clos_vars + size)
  | Prim (prim, _dbg) -> size + (Code_size.prim prim |> Code_size.to_int)
  | Static_consts _ -> size
  | Rec_info _ -> size

and continuation_handler_size ~find_code handler size =
  Continuation_handler.pattern_match handler ~f:(fun _params ~handler ->
      expr_size ~find_code handler size)

(** * * End for legacy and debugging purposes * *)

type t = { size : Code_size.t; removed : Removed_operations.t }

type code_characteristics = { cost_metrics : t; params_arity : int }

let zero = { size = Code_size.zero; removed = Removed_operations.zero }

let size t = t.size

let [@ocamlformat "disable"] print ppf t = Format.fprintf ppf "@[size: %a removed: {%a}]"
                    Code_size.print t.size
                    Removed_operations.print t.removed

let from_size size = { size; removed = Removed_operations.zero }

let notify_added ~code_size t =
  { t with size = Code_size.( + ) t.size code_size }

let notify_removed ~operation t =
  { t with removed = Removed_operations.( + ) t.removed operation }

let expr_size ~find_code e = Code_size.of_int (expr_size ~find_code e 0)

let ( + ) a b =
  { size = Code_size.( + ) a.size b.size;
    removed = Removed_operations.( + ) a.removed b.removed
  }

(* The metrics for a set of closures are the sum of the metrics for each closure
   it contains. The intuition behind it is that if we do inline a function f in
   which a set of closure is defined then we will copy the body of all functions
   referred by this set of closure as they are dependent upon f. *)
(*
 * A set of closures introduces implicitly an alloc whose size (as in OCaml 4.11)
 * is:
 *   total number of closure variables + sum of s(arity) for each closure
 * where s(a) = if a = 1 then 2 else 3
 *)
let set_of_closures ~find_code_characteristics set_of_closures =
  let func_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs func_decls in
  let num_clos_vars =
    Set_of_closures.closure_elements set_of_closures
    |> Var_within_closure.Map.cardinal
  in
  let cost_metrics, num_words =
    Closure_id.Map.fold
      (fun _ code_id (metrics, num_words) ->
        let { cost_metrics; params_arity } =
          find_code_characteristics code_id
        in
        ( metrics + cost_metrics,
          (* CR poechsel: valid until OCaml 4.13, as for named_size *)
          Stdlib.( + ) num_words (if params_arity <= 1 then 2 else 3) ))
      funs (zero, num_clos_vars)
  in
  let alloc_size =
    Code_size.( + ) Code_size.alloc_size (Code_size.of_int num_words)
  in
  cost_metrics + from_size alloc_size

let increase_due_to_let_expr ~is_phantom ~cost_metrics_of_defining_expr =
  if is_phantom then zero else cost_metrics_of_defining_expr

let increase_due_to_let_cont_non_recursive ~cost_metrics_of_handler =
  cost_metrics_of_handler

let increase_due_to_let_cont_recursive ~cost_metrics_of_handlers =
  cost_metrics_of_handlers

let evaluate ~args (t : t) =
  Code_size.evaluate ~args t.size -. Removed_operations.evaluate ~args t.removed
