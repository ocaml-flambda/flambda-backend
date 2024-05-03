(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import
module U = Unboxing_types
module Extra_param_and_args = U.Extra_param_and_args

let is_unboxing_beneficial_for_epa (epa : Extra_param_and_args.t) =
  Apply_cont_rewrite_id.Map.exists
    (fun _ extra_arg ->
      match (extra_arg : EPA.Extra_arg.t) with
      | Already_in_scope _ -> true
      | New_let_binding _ | New_let_binding_with_named_args _ -> false)
    epa.args

let rec filter_non_beneficial_decisions decision : U.decision =
  match (decision : U.decision) with
  | Do_not_unbox _ -> decision
  | Unbox (Unique_tag_and_size { tag; shape; fields }) ->
    let is_unboxing_beneficial, fields =
      List.fold_left_map
        (fun is_unboxing_beneficial ({ epa; decision; kind } : U.field_decision)
             : (_ * U.field_decision) ->
          let is_unboxing_beneficial =
            is_unboxing_beneficial || is_unboxing_beneficial_for_epa epa
          in
          let decision = filter_non_beneficial_decisions decision in
          is_unboxing_beneficial, { epa; decision; kind })
        false fields
    in
    if is_unboxing_beneficial
    then Unbox (Unique_tag_and_size { tag; shape; fields })
    else Do_not_unbox Not_beneficial
  | Unbox (Closure_single_entry { function_slot; vars_within_closure }) ->
    let is_unboxing_beneficial = ref false in
    let vars_within_closure =
      Value_slot.Map.map
        (fun ({ epa; decision; kind } : U.field_decision) : U.field_decision ->
          is_unboxing_beneficial
            := !is_unboxing_beneficial || is_unboxing_beneficial_for_epa epa;
          let decision = filter_non_beneficial_decisions decision in
          { epa; decision; kind })
        vars_within_closure
    in
    if !is_unboxing_beneficial
    then Unbox (Closure_single_entry { function_slot; vars_within_closure })
    else Do_not_unbox Not_beneficial
  | Unbox (Variant { tag; const_ctors; fields_by_tag }) ->
    let is_unboxing_beneficial = ref false in
    let fields_by_tag =
      Tag.Scannable.Map.map
        (fun (shape, fields) ->
          ( shape,
            List.map
              (fun ({ epa; decision; kind } : U.field_decision) :
                   U.field_decision ->
                is_unboxing_beneficial
                  := !is_unboxing_beneficial
                     || is_unboxing_beneficial_for_epa epa;
                let decision = filter_non_beneficial_decisions decision in
                { epa; decision; kind })
              fields ))
        fields_by_tag
    in
    if !is_unboxing_beneficial
    then Unbox (Variant { tag; const_ctors; fields_by_tag })
    else Do_not_unbox Not_beneficial
  | Unbox (Number (Naked_immediate, _)) as decision ->
    (* At worst, this unboxing untags an integer *)
    decision
  | Unbox
      (Number
        ( ( Naked_float | Naked_float32 | Naked_int32 | Naked_int64
          | Naked_nativeint | Naked_vec128 ),
          epa )) as decision ->
    if is_unboxing_beneficial_for_epa epa
    then decision
    else Do_not_unbox Not_beneficial
