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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type cse_result =
  | Applied of (Simplified_named.t * DA.t)
  | Not_applied of DA.t

let apply_cse dacc ~original_prim =
  match P.Eligible_for_cse.create original_prim with
  | None -> None
  | Some with_fixed_value -> (
    match DE.find_cse (DA.denv dacc) with_fixed_value with
    | None -> None
    | Some simple -> (
      let canonical =
        TE.get_canonical_simple_exn (DA.typing_env dacc) simple
          ~min_name_mode:NM.normal ~name_mode_of_existing_simple:NM.normal
      in
      match canonical with exception Not_found -> None | simple -> Some simple))

let try_cse dacc ~original_prim ~min_name_mode ~result_var : cse_result =
  (* CR-someday mshinwell: Use [meet] and [reify] for CSE? (discuss with
     lwhite) *)
  (* CR-someday mshinwell: Find example that suggested we needed to allow
     In_types name mode for CSE primitive arguments. *)
  if not (Name_mode.equal min_name_mode Name_mode.normal)
  then Not_applied dacc
  else
    let result_var' = VB.var result_var in
    match apply_cse dacc ~original_prim with
    | Some replace_with ->
      let named = Named.create_simple replace_with in
      let ty = T.alias_type_of (P.result_kind' original_prim) replace_with in
      let dacc = DA.add_variable dacc result_var ty in
      let simplified_named =
        let cost_metrics =
          Cost_metrics.notify_removed
            ~operation:(Removed_operations.prim original_prim)
            Cost_metrics.zero
        in
        Simplified_named.reachable named ~try_reify:(Some ty)
        |> Simplified_named.update_cost_metrics cost_metrics
      in
      Applied (simplified_named, dacc)
    | None ->
      let dacc =
        match P.Eligible_for_cse.create original_prim with
        | None -> dacc
        | Some eligible_prim ->
          let bound_to = Simple.var result_var' in
          DA.map_denv dacc ~f:(fun denv ->
              DE.add_cse denv eligible_prim ~bound_to)
      in
      Not_applied dacc

let simplify_primitive dacc (prim : P.t) dbg ~result_var =
  let min_name_mode = Bound_var.name_mode result_var in
  match prim with
  | Nullary prim' ->
    Simplify_nullary_primitive.simplify_nullary_primitive dacc prim prim' dbg
      ~result_var
  | Unary (unary_prim, orig_arg) -> (
    let arg_ty = S.simplify_simple dacc orig_arg ~min_name_mode in
    let arg = T.get_alias_exn arg_ty in
    let original_prim : P.t =
      if orig_arg == arg then prim else Unary (unary_prim, arg)
    in
    match try_cse dacc ~original_prim ~min_name_mode ~result_var with
    | Applied result -> result
    | Not_applied dacc ->
      Simplify_unary_primitive.simplify_unary_primitive dacc original_prim
        unary_prim ~arg ~arg_ty dbg ~result_var)
  | Binary (binary_prim, orig_arg1, orig_arg2) -> (
    let arg1_ty = S.simplify_simple dacc orig_arg1 ~min_name_mode in
    let arg1 = T.get_alias_exn arg1_ty in
    let arg2_ty = S.simplify_simple dacc orig_arg2 ~min_name_mode in
    let arg2 = T.get_alias_exn arg2_ty in
    let original_prim : P.t =
      if orig_arg1 == arg1 && orig_arg2 == arg2
      then prim
      else Binary (binary_prim, arg1, arg2)
    in
    match try_cse dacc ~original_prim ~min_name_mode ~result_var with
    | Applied result -> result
    | Not_applied dacc ->
      Simplify_binary_primitive.simplify_binary_primitive dacc original_prim
        binary_prim ~arg1 ~arg1_ty ~arg2 ~arg2_ty dbg ~result_var)
  | Ternary (ternary_prim, orig_arg1, orig_arg2, orig_arg3) -> (
    let arg1_ty = S.simplify_simple dacc orig_arg1 ~min_name_mode in
    let arg1 = T.get_alias_exn arg1_ty in
    let arg2_ty = S.simplify_simple dacc orig_arg2 ~min_name_mode in
    let arg2 = T.get_alias_exn arg2_ty in
    let arg3_ty = S.simplify_simple dacc orig_arg3 ~min_name_mode in
    let arg3 = T.get_alias_exn arg3_ty in
    let original_prim : P.t =
      if orig_arg1 == arg1 && orig_arg2 == arg2 && orig_arg3 = arg3
      then prim
      else Ternary (ternary_prim, arg1, arg2, arg3)
    in
    match try_cse dacc ~original_prim ~min_name_mode ~result_var with
    | Applied result -> result
    | Not_applied dacc ->
      Simplify_ternary_primitive.simplify_ternary_primitive dacc original_prim
        ternary_prim ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3 ~arg3_ty dbg
        ~result_var)
  | Variadic (variadic_prim, orig_args) -> (
    let args_with_tys =
      ListLabels.fold_right orig_args ~init:[] ~f:(fun arg args_rev ->
          let arg_ty = S.simplify_simple dacc arg ~min_name_mode in
          let arg = T.get_alias_exn arg_ty in
          (arg, arg_ty) :: args_rev)
    in
    let original_prim : P.t =
      Variadic (variadic_prim, List.map fst args_with_tys)
    in
    match try_cse dacc ~original_prim ~min_name_mode ~result_var with
    | Applied result -> result
    | Not_applied dacc ->
      Simplify_variadic_primitive.simplify_variadic_primitive dacc original_prim
        variadic_prim ~args_with_tys dbg ~result_var)
