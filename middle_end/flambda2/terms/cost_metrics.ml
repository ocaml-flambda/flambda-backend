(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Cost metrics are a group of metrics tracking the impact of simplifying an
    expression. One of these is an approximation of the size of the generated
    machine code for this expression. It also tracks the number of operations
    that should have been executed but were removed by the simplifier.*)

type t =
  { size : Code_size.t;
    removed : Removed_operations.t
  }

type code_characteristics =
  { cost_metrics : t;
    params_arity : int
  }

let zero = { size = Code_size.zero; removed = Removed_operations.zero }

let size t = t.size

let removed t = t.removed

let print ppf t =
  Format.fprintf ppf "@[<hov 1>size: %a removed: {%a}@]" Code_size.print t.size
    Removed_operations.print t.removed

let from_size size = { size; removed = Removed_operations.zero }

let notify_added ~code_size t =
  { t with size = Code_size.( + ) t.size code_size }

let notify_removed ~operation t =
  { t with removed = Removed_operations.( + ) t.removed operation }

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
 *   total number of value slots + sum of s(arity) for each closure
 * where s(a) = if a = 1 then 2 else 3
 *)
let set_of_closures ~find_code_characteristics set_of_closures =
  let func_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs func_decls in
  let num_clos_vars =
    Set_of_closures.value_slots set_of_closures |> Value_slot.Map.cardinal
  in
  let cost_metrics, num_words =
    Function_slot.Map.fold
      (fun _ (code_id : Function_declarations.code_id_in_function_declaration)
           (metrics, num_words) ->
        match code_id with
        | Deleted { function_slot_size } ->
          metrics, Stdlib.( + ) num_words function_slot_size
        | Code_id code_id ->
          let { cost_metrics; params_arity } =
            find_code_characteristics code_id
          in
          ( metrics + cost_metrics,
            (* CR poechsel: valid until OCaml 4.12, as for named_size *)
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

let equal { size = size1; removed = removed1 }
    { size = size2; removed = removed2 } =
  Code_size.equal size1 size2 && Removed_operations.equal removed1 removed2
