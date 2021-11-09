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
  | Applied of (Simplified_named.t * TEE.t * Simple.t list * DA.t)
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

let try_cse dacc ~original_prim ~simplified_args_with_tys ~min_name_mode
    ~result_var : cse_result =
  (* CR-someday mshinwell: Use [meet] and [reify] for CSE? (discuss with
     lwhite) *)
  (* CR-someday mshinwell: Find example that suggested we needed to allow
     In_types name mode for CSE primitive arguments. *)
  if not (Name_mode.equal min_name_mode Name_mode.normal)
  then Not_applied dacc
  else
    let result_var = VB.var result_var in
    match apply_cse dacc ~original_prim with
    | Some replace_with ->
      let named = Named.create_simple replace_with in
      let ty = T.alias_type_of (P.result_kind' original_prim) replace_with in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      let args = List.map fst simplified_args_with_tys in
      let simplified_named =
        let cost_metrics =
          Cost_metrics.notify_removed
            ~operation:(Removed_operations.prim original_prim)
            Cost_metrics.zero
        in
        Simplified_named.reachable named
        |> Simplified_named.update_cost_metrics cost_metrics
      in
      Applied (simplified_named, env_extension, args, dacc)
    | None ->
      let dacc =
        match P.Eligible_for_cse.create original_prim with
        | None -> dacc
        | Some eligible_prim ->
          let bound_to = Simple.var result_var in
          DA.map_denv dacc ~f:(fun denv ->
              DE.add_cse denv eligible_prim ~bound_to)
      in
      Not_applied dacc

let simplify_primitive dacc (prim : P.t) dbg ~result_var =
  let min_name_mode = Bound_var.name_mode result_var in
  let result_var' = Bound_var.var result_var in
  let args_rev =
    ListLabels.fold_left (P.args prim) ~init:[]
      ~f:(fun args_rev arg ->
        let arg_ty = S.simplify_simple dacc arg ~min_name_mode in
        let arg = T.get_alias_exn arg_ty in
        (arg, arg_ty) :: args_rev)
  in
  let args = List.rev args_rev in
  match
    try_cse dacc ~original_prim:prim ~simplified_args_with_tys:args
      ~min_name_mode ~result_var
  with
  | Applied result -> result
  | Not_applied dacc -> (
    match prim, args with
    | Nullary prim, [] ->
      Simplify_nullary_primitive.simplify_nullary_primitive dacc prim dbg
        ~result_var
    | Unary (prim, _), [(arg, arg_ty)] ->
      Simplify_unary_primitive.simplify_unary_primitive dacc prim ~arg ~arg_ty
        dbg ~result_var
    | Binary (prim, _, _), [(arg1, arg1_ty); (arg2, arg2_ty)] ->
      Simplify_binary_primitive.simplify_binary_primitive dacc prim ~arg1
        ~arg1_ty ~arg2 ~arg2_ty dbg ~result_var
    | ( Ternary (prim, _, _, _),
        [(arg1, arg1_ty); (arg2, arg2_ty); (arg3, arg3_ty)] ) ->
      Simplify_ternary_primitive.simplify_ternary_primitive dacc prim ~arg1
        ~arg1_ty ~arg2 ~arg2_ty ~arg3 ~arg3_ty dbg ~result_var
    | Variadic (variadic_prim, _), args_with_tys ->
      Simplify_variadic_primitive.simplify_variadic_primitive dacc
        variadic_prim ~args_with_tys dbg ~result_var
    | (Nullary _ | Unary _ | Binary _ | Ternary _), ([] | _ :: _) ->
      Misc.fatal_errorf "Mismatch between primitive %a and args %a" P.print
        prim Simple.List.print (List.map fst args))
