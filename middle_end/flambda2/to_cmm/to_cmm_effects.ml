(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

type effects_and_coeffects_classification =
  | Pure
  | Effect
  | Coeffect_only

let classify_by_effects_and_coeffects effs =
  (* See the comments on type [classification] in the .mli. *)
  match (effs : Effects_and_coeffects.t) with
  | Arbitrary_effects, (Has_coeffects | No_coeffects)
  | Only_generative_effects _, (Has_coeffects | No_coeffects) ->
    Effect
  | No_effects, Has_coeffects -> Coeffect_only
  | No_effects, No_coeffects -> Pure

type let_expr_classification =
  | Regular
  | Drop_defining_expr
  | Inline

let classify_let_expr var
    ~(effects_and_coeffects_of_defining_expr : Effects_and_coeffects.t)
    ~(num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t) =
  match Variable.Map.find var num_normal_occurrences_of_bound_vars with
  | exception Not_found -> Regular
  | Zero -> begin
    match
      classify_by_effects_and_coeffects effects_and_coeffects_of_defining_expr
    with
    | Coeffect_only | Pure -> Drop_defining_expr
    | Effect ->
      Regular
      (* Could be Inline technically, but it doesn't matter since it can only be
         flushed by the env. *)
  end
  | One -> begin
    match effects_and_coeffects_of_defining_expr with
    (* The decision here is whether to consider the binding for inlining or not.
       It is always correct to consider an effectful expression for inlining, as
       the environment is going to handle the details of preserving the effects
       and coeffects ordering (if inlining without reordering is impossible then
       the expressions will be bound at some safe place instead).

       So the decision here is about readability of the generated Cmm code
       (effectful expressions as arguments to other primitives make it hard to
       follow the order of evaluation) and locality (for deeply nested
       expressions, binding the sub-expressions outside can keep them alive for
       longer than strictly necessary).

       The current choice of always inlining pure expressions and expressions
       with only generative effects is guided by the relatively common case of
       initialisation of huge static structures (including arrays). Without
       inlining, all intermediate results would be live for long periods of time
       and the default register allocator would have trouble dealing with that
       (it's quadratic in the number of registers live at the same time).

       Deep expressions involving arbitrary effects are less common, so inlining
       for these expressions is controlled by the global [inline_effects_in_cmm]
       setting. *)
    | Only_generative_effects _, _ -> Inline
    | Arbitrary_effects, _ ->
      if Flambda_features.Expert.inline_effects_in_cmm ()
      then Inline
      else Regular
    | No_effects, _ -> Inline
  end
  | More_than_one -> Regular

type continuation_handler_classification =
  | Regular
  | Inline

let cont_is_known_to_have_exactly_one_occurrence k (num : _ Or_unknown.t) =
  match num with
  | Unknown -> false
  | Known num -> (
    match (num : Num_occurrences.t) with
    | One -> true
    | More_than_one -> false
    | Zero ->
      Misc.fatal_errorf
        "Found unused let-bound continuation %a, this should not happen"
        Continuation.print k)

let classify_continuation_handler k handler ~num_free_occurrences
    ~is_applied_with_traps : continuation_handler_classification =
  if (not (Continuation_handler.is_exn_handler handler))
     && (not is_applied_with_traps)
     && cont_is_known_to_have_exactly_one_occurrence k num_free_occurrences
  then Inline
  else Regular
