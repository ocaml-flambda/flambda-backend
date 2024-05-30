(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module DA = Downwards_acc
module DE = Downwards_env
module K = Flambda_kind
module T = Flambda2_types
module TE = T.Typing_env

let compute_succ ~(depth : int Or_infinity.t)
    ~(unrolling : Rec_info_expr.Unrolling_state.t) =
  let depth : int Or_infinity.t =
    match depth with Finite n -> Finite (n + 1) | Infinity -> Infinity
  in
  let unrolling =
    match unrolling with
    | Not_unrolling | Do_not_unroll -> unrolling
    | Unrolling { remaining_depth } ->
      if remaining_depth <= 0
      then unrolling
      else
        let remaining_depth = remaining_depth - 1 in
        Rec_info_expr.Unrolling_state.unrolling ~remaining_depth
  in
  Rec_info_expr.const ~depth ~unrolling

let compute_unroll_to ~depth ~old_unrolling_state ~unroll_to =
  (* Take the maximum of the two unroll depths. This allows an external caller
     to specify more unrolling than the recursive call sites do. *)
  let unrolling =
    match (old_unrolling_state : Rec_info_expr.Unrolling_state.t) with
    | Not_unrolling ->
      Rec_info_expr.Unrolling_state.unrolling ~remaining_depth:unroll_to
    | Unrolling { remaining_depth } ->
      if remaining_depth >= unroll_to
      then old_unrolling_state
      else Rec_info_expr.Unrolling_state.unrolling ~remaining_depth:unroll_to
    | Do_not_unroll -> old_unrolling_state
  in
  Rec_info_expr.const ~depth ~unrolling

type on_unknown =
  | Leave_unevaluated
  | Assume_value of Rec_info_expr.t

let rec simplify_rec_info_expr0 denv orig ~on_unknown : Rec_info_expr.t =
  match (orig : Rec_info_expr.t) with
  | Const _ -> orig
  | Var dv -> (
    let ty = TE.find (DE.typing_env denv) (Name.var dv) (Some K.rec_info) in
    match T.meet_rec_info (DE.typing_env denv) ty with
    | Known_result rec_info_expr ->
      (* All bound names are fresh, so fine to use the same environment *)
      simplify_rec_info_expr0 denv rec_info_expr ~on_unknown
    | Need_meet -> (
      match on_unknown with
      | Leave_unevaluated -> orig
      | Assume_value value -> value)
    | Invalid ->
      (* Shouldn't currently be possible *)
      Misc.fatal_errorf "Invalid result from [check_rec_info] of %a" T.print ty)
  | Succ ri -> (
    match simplify_rec_info_expr0 denv ri ~on_unknown with
    | Const { depth; unrolling } -> compute_succ ~depth ~unrolling
    | (Var _ | Succ _ | Unroll_to _) as new_ri ->
      if ri == new_ri then orig else Rec_info_expr.succ new_ri)
  | Unroll_to (unroll_depth, ri) -> (
    match simplify_rec_info_expr0 denv ri ~on_unknown with
    | Const { depth; unrolling } ->
      compute_unroll_to ~depth ~old_unrolling_state:unrolling
        ~unroll_to:unroll_depth
    | (Var _ | Succ _ | Unroll_to _) as new_ri ->
      if ri == new_ri then orig else Rec_info_expr.unroll_to unroll_depth new_ri
    )

let simplify_rec_info_expr dacc rec_info_expr =
  let ans =
    simplify_rec_info_expr0 (DA.denv dacc) rec_info_expr
      ~on_unknown:Leave_unevaluated
  in
  ans

module Evaluated_rec_info_expr = struct
  type t =
    { depth : int Or_infinity.t;
      unrolling : Rec_info_expr.Unrolling_state.t
    }

  let [@ocamlformat "disable"] print ppf { depth; unrolling } =
    Rec_info_expr.print ppf (Rec_info_expr.const ~depth ~unrolling)
end

let evaluate_rec_info_expr dacc rec_info_expr =
  match
    simplify_rec_info_expr0 (DA.denv dacc) rec_info_expr
      ~on_unknown:(Assume_value Rec_info_expr.unknown)
  with
  | Const { depth; unrolling } -> { Evaluated_rec_info_expr.depth; unrolling }
  | Var _ | Succ _ | Unroll_to _ ->
    Misc.fatal_errorf "Unable to evaluate@ %a@ with@ dacc@ %a"
      Rec_info_expr.print rec_info_expr DA.print dacc

let depth_may_exceed dacc rec_info_expr bound =
  let { Evaluated_rec_info_expr.depth; _ } =
    evaluate_rec_info_expr dacc rec_info_expr
  in
  Or_infinity.compare ~f:Int.compare depth (Finite bound) > 0

let known_remaining_unrolling_depth dacc rec_info_expr =
  match evaluate_rec_info_expr dacc rec_info_expr with
  | { unrolling = Unrolling { remaining_depth }; _ } -> Some remaining_depth
  | { unrolling = Not_unrolling | Do_not_unroll; _ } -> None

let can_unroll dacc rec_info_expr =
  match evaluate_rec_info_expr dacc rec_info_expr with
  | { unrolling = Do_not_unroll; _ } -> false
  | { unrolling = Not_unrolling | Unrolling _; _ } -> true
