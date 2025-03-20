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

(* CR mshinwell: We need to emit [Warnings.Inlining_impossible] as required.

   When in fallback-inlining mode: if we want to follow Closure we should not
   complain about function declarations with e.g. [@inline always] if the
   function contains other functions and therefore cannot be inlined. We should
   however contain at call sites if inlining is requested but cannot be done for
   this reason. I think this will probably all happen without any specific code
   once [Inlining_impossible] handling is implemented for the
   non-fallback-inlining cases. *)

type t =
  | Missing_code
  | Definition_says_not_to_inline
  | Environment_says_never_inline
  | Argument_types_not_useful
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Never_inlined_attribute
  | Speculatively_not_inline of
      { cost_metrics : Cost_metrics.t;
        evaluated_to : float;
        threshold : float
      }
  | Attribute_always
  | Begin_unrolling of int
  | Continue_unrolling
  | Definition_says_inline of { was_inline_always : bool }
  | Speculatively_inline of
      { cost_metrics : Cost_metrics.t;
        evaluated_to : float;
        threshold : float
      }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Missing_code -> Format.fprintf ppf "Missing_code"
  | Definition_says_not_to_inline ->
    Format.fprintf ppf "Definition_says_not_to_inline"
  | Environment_says_never_inline ->
    Format.fprintf ppf "Environment_says_never_inline"
  | Argument_types_not_useful ->
    Format.fprintf ppf "Argument_types_not_useful"
  | Unrolling_depth_exceeded ->
    Format.fprintf ppf "Unrolling_depth_exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf ppf "Max_inlining_depth_exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf ppf "Recursion_depth_exceeded"
  | Never_inlined_attribute ->
    Format.fprintf ppf "Never_inlined_attribute"
  | Attribute_always ->
    Format.fprintf ppf "Attribute_always"
  | Definition_says_inline { was_inline_always } ->
    Format.fprintf ppf
      "@[<hov 1>(Definition_says_inline@ \
        @[<hov 1>(was_inline_always@ %b)@])\
        @]"
      was_inline_always
  | Begin_unrolling unroll_to ->
    Format.fprintf ppf
      "@[<hov 1>(Begin_unrolling@ \
        @[<hov 1>(unroll_to@ %d)@]\
        )@]"
      unroll_to
  | Continue_unrolling ->
    Format.fprintf ppf "Continue_unrolling"
  | Speculatively_not_inline { cost_metrics; threshold; evaluated_to; } ->
    Format.fprintf ppf
      "@[<hov 1>(Speculatively_not_inline@ \
        @[<hov 1>(cost_metrics@ %a)@]@ \
        @[<hov 1>(evaluated_to@ %f)@]@ \
        @[<hov 1>(threshold@ %f)@]\
        )@]"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold
  | Speculatively_inline { cost_metrics; threshold; evaluated_to; } ->
    Format.fprintf ppf
      "@[<hov 1>(Speculatively_inline@ \
        @[<hov 1>(cost_metrics@ %a)@]@ \
        @[<hov 1>(evaluated_to@ %f)@]@ \
        @[<hov 1>(threshold@ %f)@]\
        )@]"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold

type can_inline =
  | Do_not_inline of { erase_attribute_if_ignored : bool }
  | Inline of
      { unroll_to : int option;
        was_inline_always : bool
      }

let can_inline (t : t) : can_inline =
  match t with
  | Missing_code | Environment_says_never_inline | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded | Speculatively_not_inline _
  | Definition_says_not_to_inline | Argument_types_not_useful ->
    (* If there's an [@inlined] attribute on this, something's gone wrong *)
    Do_not_inline { erase_attribute_if_ignored = false }
  | Never_inlined_attribute ->
    (* If there's an [@inlined] attribute on this, something's gone wrong *)
    Do_not_inline { erase_attribute_if_ignored = false }
  | Unrolling_depth_exceeded ->
    (* If there's an [@unrolled] attribute on this, then we'll ignore the
       attribute when we stop unrolling, which is fine *)
    Do_not_inline { erase_attribute_if_ignored = true }
  | Begin_unrolling unroll_to ->
    Inline { unroll_to = Some unroll_to; was_inline_always = false }
  | Continue_unrolling ->
    let was_inline_always =
      (* This could be [true] since the user asked to unroll this far, but the
         warning would be confusing. We should use something more informative
         than a [bool] here to describe what warning should be raised if we
         don't inline. *)
      false
    in
    Inline { unroll_to = None; was_inline_always }
  | Definition_says_inline { was_inline_always } ->
    Inline { unroll_to = None; was_inline_always }
  | Speculatively_inline _ ->
    Inline { unroll_to = None; was_inline_always = false }
  | Attribute_always -> Inline { unroll_to = None; was_inline_always = true }
