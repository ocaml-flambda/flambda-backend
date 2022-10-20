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
  | Unroll_attribute_used_with_loopified_function
  | Speculatively_not_inline of
      { cost_metrics : Cost_metrics.t;
        evaluated_to : float;
        threshold : float
      }
  | Attribute_always
  | Attribute_unroll of int
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
  | Unroll_attribute_used_with_loopified_function ->
    Format.fprintf ppf "Unroll_attribute_used_with_loopified_function"
  | Attribute_always ->
    Format.fprintf ppf "Attribute_always"
  | Definition_says_inline { was_inline_always } ->
    Format.fprintf ppf
      "@[<hov 1>(Definition_says_inline@ \
        @[<hov 1>(was_inline_always@ %b)@])\
        @]"
      was_inline_always
  | Attribute_unroll unroll_to ->
    Format.fprintf ppf
      "@[<hov 1>(Attribute_unroll@ \
        @[<hov 1>(unroll_to@ %d)@]\
        )@]"
      unroll_to
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
  | Do_not_inline of
      { warn_if_attribute_ignored : bool;
        because_of_definition : bool
      }
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
    Do_not_inline
      { warn_if_attribute_ignored = true; because_of_definition = true }
  | Never_inlined_attribute ->
    (* If there's an [@inlined] attribute on this, something's gone wrong *)
    Do_not_inline
      { warn_if_attribute_ignored = true; because_of_definition = true }
  | Unrolling_depth_exceeded ->
    (* If there's an [@unrolled] attribute on this, then we'll ignore the
       attribute when we stop unrolling, which is fine *)
    Do_not_inline
      { warn_if_attribute_ignored = false; because_of_definition = true }
  | Unroll_attribute_used_with_loopified_function ->
    (* We have an [@unrolled] attribute, but can't unroll loopified functions *)
    Do_not_inline
      { warn_if_attribute_ignored = true; because_of_definition = true }
  | Attribute_unroll unroll_to ->
    Inline { unroll_to = Some unroll_to; was_inline_always = false }
  | Definition_says_inline { was_inline_always } ->
    Inline { unroll_to = None; was_inline_always }
  | Speculatively_inline _ ->
    Inline { unroll_to = None; was_inline_always = false }
  | Attribute_always -> Inline { unroll_to = None; was_inline_always = true }

let report_reason fmt t =
  match (t : t) with
  | Missing_code ->
    Format.fprintf fmt
      "the@ code@ could@ not@ be@ found@ (is@ a@ .cmx@ file@ missing?)"
  | Definition_says_not_to_inline ->
    Format.fprintf fmt
      "this@ function@ was@ deemed@ at@ the@ point@ of@ its@ definition@ to@ \
       never@ be@ inlinable"
  | Environment_says_never_inline ->
    Format.fprintf fmt "the@ environment@ says@ never@ to@ inline"
  | Argument_types_not_useful ->
    Format.fprintf fmt
      "there@ was@ no@ useful@ information@ about@ the@ arguments"
  | Unrolling_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ unrolling@ depth@ has@ been@ exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ inlining@ depth@ has@ been@ exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ recursion@ depth@ has@ been@ exceeded"
  | Never_inlined_attribute ->
    Format.fprintf fmt "the@ call@ has@ an@ attribute@ forbidding@ inlining"
  | Unroll_attribute_used_with_loopified_function ->
    Format.fprintf fmt
      "the@ code@ of@ this@ function@ has@ been@ transformed@ to@ a@ loop,@ \
       which@ cannot@ be@ unrolled@ yet@ (consider@ adding@ [@loop never]@ to@ \
       the@ definition)"
  | Attribute_always ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@inline always]@ attribute"
  | Attribute_unroll n ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n
  | Definition_says_inline { was_inline_always = _ } ->
    Format.fprintf fmt
      "this@ function@ was@ decided@ to@ be@ always@ inlined@ at@ its@ \
       definition@ site (annotated@ by@ [@inlined always]@ or@ determined@ to@ \
       be@ small@ enough)"
  | Speculatively_not_inline { cost_metrics; evaluated_to; threshold } ->
    Format.fprintf fmt
      "the@ function@ was@ not@ inlined@ after@ speculation@ as@ its@ cost@ \
       metrics were=%a,@ which@ was@ evaluated@ to@ %f > threshold %f"
      Cost_metrics.print cost_metrics evaluated_to threshold
  | Speculatively_inline { cost_metrics; evaluated_to; threshold } ->
    Format.fprintf fmt
      "the@ function@ was@ inlined@ after@ speculation@ as@ its@ cost@ metrics \
       were=%a,@ which@ was@ evaluated@ to@ %f <= threshold %f"
      Cost_metrics.print cost_metrics evaluated_to threshold

let report fmt t =
  Format.fprintf fmt
    "@[<v>The function call %s been inlined@ because @[<hov>%a@]@]"
    (match can_inline t with Inline _ -> "has" | Do_not_inline _ -> "has not")
    report_reason t
