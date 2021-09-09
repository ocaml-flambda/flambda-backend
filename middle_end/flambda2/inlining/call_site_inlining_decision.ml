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

open! Flambda.Import

module DE = Downwards_env
module DA = Downwards_acc
module UA = Upwards_acc
module UE = Upwards_env

(* CR mshinwell: We need to emit [Warnings.Inlining_impossible] as
   required.

   When in fallback-inlining mode: if we want to follow Closure we should
   not complain about function declarations with e.g. [@inline always]
   if the function contains other functions and therefore cannot be
   inlined.  We should however contain at call sites if inlining is
   requested but cannot be done for this reason.  I think this will probably
   all happen without any specific code once [Inlining_impossible]
   handling is implemented for the non-fallback-inlining cases. *)

type t =
  | Environment_says_never_inline
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Never_inline_attribute
  | Speculatively_not_inline of {
      cost_metrics: Cost_metrics.t;
      evaluated_to: float;
      threshold: float;
    }
  | Attribute_always
  | Attribute_unroll of int
  | Definition_says_inline
  | Speculatively_inline of {
      cost_metrics: Cost_metrics.t;
      evaluated_to: float;
      threshold: float;
    }

let print ppf t =
  match t with
  | Environment_says_never_inline ->
    Format.fprintf ppf "Environment_says_never_inline"
  | Unrolling_depth_exceeded ->
    Format.fprintf ppf "Unrolling_depth_exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf ppf "Max_inlining_depth_exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf ppf "Recursion_depth_exceeded"
  | Never_inline_attribute ->
    Format.fprintf ppf "Never_inline_attribute"
  | Attribute_always ->
    Format.fprintf ppf "Attribute_unroll"
  | Definition_says_inline ->
    Format.fprintf ppf "Definition_says_inline"
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
  | Do_not_inline of { warn_if_attribute_ignored : bool; }
  | Inline of { unroll_to : int option; }

let can_inline (t : t) : can_inline =
  match t with
  | Environment_says_never_inline
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Speculatively_not_inline _
  | Never_inline_attribute ->
    (* If there's an inline attribute on this, something's gone wrong *)
    Do_not_inline { warn_if_attribute_ignored = true }
  | Unrolling_depth_exceeded ->
    (* If there's an [@unrolled] attribute on this, then we'll ignore the
        attribute when we stop unrolling, which is fine *)
    Do_not_inline { warn_if_attribute_ignored = false }
  | Attribute_unroll unroll_to -> Inline { unroll_to = Some unroll_to; }
  | Definition_says_inline
  | Speculatively_inline _
  | Attribute_always -> Inline { unroll_to = None; }

let report_reason fmt t =
  match (t : t) with
  | Environment_says_never_inline ->
    Format.fprintf fmt "the@ environment@ says@ never@ to@ inline"
  | Unrolling_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ unrolling@ depth@ has@ been@ exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ inlining@ depth@ has@ been@ exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ recursion@ depth@ has@ been@ exceeded"
  | Never_inline_attribute ->
    Format.fprintf fmt "the@ call@ has@ an@ attribute@ forbidding@ inlining"
  | Attribute_always ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@inline always]@ attribute"
  | Attribute_unroll n ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n
  | Definition_says_inline ->
    Format.fprintf fmt "this@ function@ was@ decided@ to@ be@ always@ \
                        inlined@ at@ its@ definition@ site (annotated@ by@ \
                        [@inlined always]@ or@ determined@ to@ be@ small@ \
                        enough)"
  | Speculatively_not_inline { cost_metrics; evaluated_to; threshold } ->
    Format.fprintf fmt
      "the@ function@ was@ not@ inlined@ after@ speculation@ as@ \
        its@ cost@ metrics were=%a,@ which@ was@ evaluated@ \
        to@ %f > threshold %f"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold
  | Speculatively_inline { cost_metrics; evaluated_to; threshold } ->
    Format.fprintf fmt
      "the@ function@ was@ inlined@ after@ speculation@ as@ \
        its@ cost@ metrics were=%a,@ which@ was@ evaluated@ \
        to@ %f <= threshold %f"
      Cost_metrics.print cost_metrics
      evaluated_to
      threshold

let report fmt t =
  Format.fprintf fmt
    "@[<v>The function call %s been inlined@ because @[<hov>%a@]@]"
    (match can_inline t with
      | Inline _ -> "has"
      | Do_not_inline _ -> "has not")
    report_reason t

(* CR mshinwell: Overhaul handling of the inlining depth tracking so that
   it takes into account the depth of closures (or code), as per
   conversation with lwhite. *)

(* CR mshinwell: This parameter needs to be configurable *)
let max_rec_depth = 1

module I = Flambda_type.Function_declaration_type.Inlinable

let speculative_inlining dacc ~apply ~function_decl ~simplify_expr
      ~return_arity =
  let dacc = DA.set_do_not_rebuild_terms_and_disable_inlining dacc in
  (* CR-someday poechsel: [Inlining_transforms.inline] is preparing the body
     for inlining. Right know it may be called twice (once there and once in
     [simplify_apply_expr]) on the same apply expr. It should be possible to
     only call it once and remove some allocations.
  *)
  let dacc, expr =
    (* The only way for [unroll_to] not to be None is when an explicit
       Unroll annotation is provided by the user. If this is the case then
       inliner will always inline the function and will not call
       [speculative_inlining]. Thus inside of [speculative_inlining] we
       will always have [unroll_to] = None. We are not disabling unrolling
       when speculating, it just happens that no unrolling can happen while
       speculating right now.
    *)
    Inlining_transforms.inline dacc ~apply ~unroll_to:None function_decl
  in
  let scope = DE.get_continuation_scope_level (DA.denv dacc) in
  let dummy_toplevel_cont =
    Continuation.create ~name:"dummy_toplevel_continuation" ()
  in
  let dacc =
    DA.map_data_flow dacc ~f:(fun _ ->
      Data_flow.init_toplevel dummy_toplevel_cont [] Data_flow.empty)
  in
  let _, uacc =
    simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
      let exn_continuation = Apply.exn_continuation apply in
      let dacc =
        DA.map_data_flow dacc ~f:(
          Data_flow.exit_continuation dummy_toplevel_cont)
      in
      let data_flow = DA.data_flow dacc in
      (* The dataflow analysis *)
      let function_return_cont =
        match Apply.continuation apply with
        | Never_returns -> Continuation.create ()
        | Return cont -> cont
      in
      (* When doing the speculative analysis, in order to not blow up,
         the data_flow analysis is only done on the speculatively inlined
         body; however the reachable code_ids part of the data flow analysis
         is only correct at toplevel when all information about the
         code_age relation and used_closure vars is available (for the whole
         compilation unit). Thus we here provide empty/dummy values
         for the used_closure_vars and code_age_relation, and ignore the
         reachable_code_id part of the data_flow analysis. *)
      let { required_names; reachable_code_ids = _; } : Data_flow.result =
        Data_flow.analyze data_flow
          ~code_age_relation:Code_age_relation.empty
          ~used_closure_vars:Unknown
          ~return_continuation:function_return_cont
          ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
      in
      let uenv = UE.add_exn_continuation UE.empty exn_continuation scope in
      let uenv =
        match Apply.continuation apply with
        | Never_returns -> uenv
        | Return return_continuation ->
          UE.add_return_continuation uenv return_continuation scope
            return_arity
      in
      let uacc = UA.create ~required_names ~reachable_code_ids:Unknown uenv dacc in
      rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc)
    )
  in
  UA.cost_metrics uacc

let might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity : t =
  let denv = DA.denv dacc in
  let env_prohibits_inlining = not (DE.can_inline denv) in
  if I.must_be_inlined function_decl then
    Definition_says_inline
  else if env_prohibits_inlining then
    Environment_says_never_inline
  else
    let cost_metrics =
      speculative_inlining ~apply dacc ~simplify_expr ~return_arity
        ~function_decl
    in
    let args =
      Apply.inlining_arguments apply
      |> Inlining_arguments.meet (DA.denv dacc |> DE.inlining_arguments)
    in
    let evaluated_to = Cost_metrics.evaluate ~args cost_metrics in
    let threshold = Inlining_arguments.threshold args in
    let is_under_inline_threshold =
      Float.compare evaluated_to threshold <= 0
    in
    if is_under_inline_threshold then
      Speculatively_inline { cost_metrics; evaluated_to; threshold }
    else
      Speculatively_not_inline { cost_metrics; evaluated_to; threshold }

let make_decision dacc ~simplify_expr ~function_decl ~apply ~return_arity : t =
  let function_decl_rec_info = I.rec_info function_decl in
  let function_decl_rec_info =
    match
      Flambda_type.prove_rec_info (DA.typing_env dacc) function_decl_rec_info
    with
    | Proved rec_info -> rec_info
    | Unknown -> Rec_info_expr.unknown
    | Invalid -> Rec_info_expr.do_not_inline
  in
  let inline = Apply.inline apply in
  match inline with
  | Never_inline -> Never_inline_attribute
  | Default_inline | Unroll _ | Always_inline | Hint_inline ->
    (* The unrolling process is rather subtle, but it boils down to two steps:

       1. We see an [@unrolled n] annotation (with n > 0) on an apply expression
          whose [rec_info] has the unrolling state [Not_unrolling]. When we
          inline the body, we bind [my_depth] to a rec_info whose unrolling
          state is [Unrolling { remaining_depth = n }].

       2. When we see that application again, its rec_info will have the
          unrolling state [Unrolling { remaining_depth = n - 1 }] (because its
          depth is [succ my_depth]). At that point, we short-circuit most of the
          inlining logic and inline if and only if n > 0.

       Here we're performing step _2_ (but only, of course, if we performed step
       1 in a previous call to this function). *)
    let unrolling_depth =
      Simplify_rec_info_expr.known_remaining_unrolling_depth
        dacc function_decl_rec_info
    in
    match unrolling_depth with
    | Some 0 ->
      Unrolling_depth_exceeded
    | Some _ ->
      might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity
    | None ->
      (* CR lmaurer: This seems semantically dodgy: If we really think of a free
         depth variable as [Unknown], then we shouldn't be considering inlining
         here, because we don't _know_ that we're not unrolling. The behavior is
         what we want, though (and is consistent with FLambda 1): If there's a
         free depth variable, that means this is an internal recursive call,
         which means we consider unrolling if [@unrolled] appears. If it's known
         that the unrolling depth is zero, that means we're inlining into
         another function and we're done unrolling, so we immediately stop
         inlining.

         So this seems to be working for the moment, but I wonder what are the
         ramifications of treating unknown-ness as an observable property this
         way. Are we relying on monotonicity somewhere? *)
      let apply_inlining_state = Apply.inlining_state apply in
      if Inlining_state.is_depth_exceeded apply_inlining_state
      then
        Max_inlining_depth_exceeded
      else
        match inline with
        | Never_inline -> assert false
        | Default_inline ->
          if
            Simplify_rec_info_expr.depth_may_be_at_least
              dacc function_decl_rec_info (max_rec_depth + 1)
          then
            Recursion_depth_exceeded
          else
            might_inline dacc ~apply ~function_decl ~simplify_expr ~return_arity
        | Unroll unroll_to ->
          if Simplify_rec_info_expr.can_unroll dacc function_decl_rec_info then
            (* This sets off step 1 in the comment above; see
              [Inlining_transforms] for how [unroll_to] is ultimately handled. *)
            Attribute_unroll unroll_to
          else
            Unrolling_depth_exceeded
        | Always_inline | Hint_inline ->
          Attribute_always
