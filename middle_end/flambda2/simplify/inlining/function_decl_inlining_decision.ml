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

[@@@ocaml.warning "+a-30-40-41-42"]

let make_decision ~inlining_arguments:args ~inline ~stub ~cost_metrics:metrics
    ~is_a_functor : Function_decl_inlining_decision_type.t =
  (* At present, we follow Closure, taking inlining decisions without first
     examining call sites. *)
  match (inline : Inline_attribute.t) with
  | Never_inline -> Never_inline_attribute
  | Always_inline -> Attribute_inline
  | Default_inline | Unroll _ | Available_inline ->
    if stub
    then Stub
    else
      let large_function_size =
        Inlining_arguments.large_function_size args |> Code_size.of_int
      in
      let small_function_size =
        Inlining_arguments.small_function_size args |> Code_size.of_int
      in
      let size = Cost_metrics.size metrics in
      let is_small = Code_size.( <= ) size small_function_size in
      let is_large = Code_size.( <= ) large_function_size size in
      if is_a_functor
      then Functor { size }
      else if is_large && not (Inline_attribute.equal inline Available_inline)
      then Function_body_too_large large_function_size
      else if is_small
      then
        Small_function { size = Cost_metrics.size metrics; small_function_size }
      else
        Speculatively_inlinable
          { size = Cost_metrics.size metrics;
            small_function_size;
            large_function_size
          }
