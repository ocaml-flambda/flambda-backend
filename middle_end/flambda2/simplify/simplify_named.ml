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

open! Simplify_import

let create_lifted_constant (dacc, lifted_constants)
    (pat : Bound_symbols.Pattern.t) static_const =
  match pat with
  | Block_like symbol ->
    let typ =
      (* CR mshinwell: Maybe the types should be returned from
         [Simplify_static_const] to avoid this lookup. *)
      TE.find (DA.typing_env dacc) (Name.symbol symbol) (Some K.value)
    in
    (* The [symbol_projections] map is empty because we only introduce symbol
       projections when lifting -- and [static_const] has already been
       lifted. *)
    let lifted_constant =
      LC.create_block_like symbol static_const (DA.denv dacc)
        ~symbol_projections:Variable.Map.empty typ
    in
    let dacc =
      match Rebuilt_static_const.to_const static_const with
      | None | Some (Code _ | Deleted_code) -> dacc
      | Some (Static_const static_const) ->
        DA.consider_constant_for_sharing dacc symbol static_const
    in
    dacc, lifted_constant :: lifted_constants
  | Code code_id ->
    let lifted_constant = LC.create_code code_id static_const in
    dacc, lifted_constant :: lifted_constants
  | Set_of_closures closure_symbols ->
    let closure_symbols_with_types =
      Closure_id.Lmap.map
        (fun symbol ->
          let typ =
            TE.find (DA.typing_env dacc) (Name.symbol symbol) (Some K.value)
          in
          symbol, typ)
        closure_symbols
    in
    let lifted_constant =
      LC.create_set_of_closures (DA.denv dacc)
        ~closure_symbols_with_types
          (* Same comment as above re. [symbol_projections]. *)
        ~symbol_projections:Variable.Map.empty static_const
    in
    dacc, lifted_constant :: lifted_constants

(* It is important that every set of closures returned by this function (in
   [bindings_outermost_first]) arises from simplification in
   [Simplify_set_of_closures], and not some other path such as reification. This
   ensures that the returned [dacc] is equipped with the free name information
   for such sets. See comment in [Simplify_let_expr], function [rebuild_let]. *)

let simplify_named0 dacc (bound_pattern : Bound_pattern.t) (named : Named.t)
    ~simplify_toplevel =
  match named with
  | Simple simple ->
    let bound_var = Bound_pattern.must_be_singleton bound_pattern in
    let min_name_mode = Bound_var.name_mode bound_var in
    let ty = S.simplify_simple dacc simple ~min_name_mode in
    let new_simple = T.get_alias_exn ty in
    let dacc = DA.add_variable dacc bound_var ty in
    let defining_expr =
      if simple == new_simple
      then Simplified_named.reachable named ~try_reify:None
      else
        Simplified_named.reachable (Named.create_simple simple) ~try_reify:None
    in
    Simplify_named_result.have_simplified_to_single_term dacc bound_pattern
      defining_expr ~original_defining_expr:named
  | Prim (prim, dbg) -> (
    let bound_var = Bound_pattern.must_be_singleton bound_pattern in
    let simplified_named, dacc =
      Simplify_primitive.simplify_primitive dacc prim dbg ~result_var:bound_var
    in
    if Flambda_features.check_invariants ()
       && not (TE.mem (DA.typing_env dacc) (Name.var (Bound_var.var bound_var)))
    then
      Misc.fatal_errorf "Primitive %a = %a did not yield a result var"
        Bound_var.print bound_var P.print prim;
    match simplified_named with
    | Reachable_try_reify { ty; _ } ->
      (* CR mshinwell: Add check along the lines of: types are unknown whenever
         [not (P.With_fixed_value.eligible prim)] holds. *)
      (* Primitives with generative effects correspond to allocations. Without
         this check, we could end up lifting definitions that have a type that
         looks like an allocation but that are instead a projection from a
         bigger structure. *)
      let allow_lifting =
        (* CR mshinwell: We probably shouldn't lift if the let binding is going
           to be deleted, as lifting may cause [Dominator]-scoped bindings to be
           inserted, that cannot be deleted. However this situation probably
           doesn't arise that much, and won't be an issue once we can lift
           [Dominator]-scoped bindings. *)
        P.only_generative_effects prim
        && Name_mode.is_normal (Bound_var.name_mode bound_var)
      in
      let defining_expr, dacc =
        Reification.try_to_reify dacc simplified_named ~bound_to:bound_var
          ~allow_lifting ty
      in
      Simplify_named_result.have_simplified_to_single_term dacc bound_pattern
        defining_expr ~original_defining_expr:named
    | Reachable _ | Invalid _ ->
      Simplify_named_result.have_simplified_to_single_term dacc bound_pattern
        simplified_named ~original_defining_expr:named)
  | Set_of_closures set_of_closures ->
    Simplify_set_of_closures.simplify_non_lifted_set_of_closures dacc
      bound_pattern set_of_closures ~simplify_toplevel
  | Static_consts static_consts ->
    let { Bound_pattern.bound_symbols } =
      Bound_pattern.must_be_symbols bound_pattern
    in
    let binds_symbols = Bound_symbols.binds_symbols bound_symbols in
    if binds_symbols && not (DE.at_unit_toplevel (DA.denv dacc))
    then
      Misc.fatal_errorf
        "[Let] binding symbols is only allowed at the toplevel of compilation \
         units (not even at the toplevel of function bodies):@ %a@ =@ %a"
        Bound_pattern.print bound_pattern Named.print named;
    let bound_symbols, static_consts, dacc =
      try
        Simplify_static_const.simplify_static_consts dacc bound_symbols
          static_consts ~simplify_toplevel
      with Misc.Fatal_error ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf
          "\n\
           %sContext is:%s simplifying 'let symbol' binding of@ %a@ with \
           downwards accumulator:@ %a\n"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Bound_symbols.print bound_symbols DA.print dacc;
        Printexc.raise_with_backtrace Misc.Fatal_error bt
    in
    let dacc, lifted_constants =
      ListLabels.fold_left2
        (Bound_symbols.to_list bound_symbols)
        (Rebuilt_static_const.Group.to_list static_consts)
        ~init:(dacc, []) ~f:create_lifted_constant
    in
    let dacc =
      DA.add_to_lifted_constant_accumulator dacc
        (LCS.singleton (LC.concat lifted_constants))
    in
    (* We don't need to return any bindings; [Simplify_expr.simplify_let] will
       create the "let symbol" binding when it sees the lifted constant. *)
    Simplify_named_result.have_simplified_to_zero_terms dacc
  | Rec_info rec_info_expr ->
    (* We could simplify away things like [let depth x = y in ...], but those
       don't actually happen (as of this writing). We could also do CSE,
       though. *)
    let bound_var = Bound_pattern.must_be_singleton bound_pattern in
    let new_rec_info_expr =
      Simplify_rec_info_expr.simplify_rec_info_expr dacc rec_info_expr
    in
    let ty = T.this_rec_info rec_info_expr in
    let dacc = DA.add_variable dacc bound_var ty in
    let defining_expr =
      if rec_info_expr == new_rec_info_expr
      then Simplified_named.reachable named ~try_reify:None
      else
        Simplified_named.reachable
          (Named.create_rec_info new_rec_info_expr)
          ~try_reify:None
    in
    Simplify_named_result.have_simplified_to_single_term dacc bound_pattern
      defining_expr ~original_defining_expr:named

let removed_operations (named : Named.t) result =
  let descr = Simplify_named_result.descr result in
  let zero = Removed_operations.zero in
  match named with
  | Set_of_closures _ -> begin
    match descr with
    | Multiple_bindings_to_symbols _ -> Removed_operations.alloc
    | Single_term { simplified_defining_expr; _ } -> begin
      match simplified_defining_expr with
      | Reachable { named = Set_of_closures _; _ }
      | Reachable_try_reify { named = Set_of_closures _; _ } ->
        (* Nothing was deleted, there is no need to adjust the negative
           benefit *)
        zero
      | Invalid _
      | Reachable { named = Prim _; _ }
      | Reachable { named = Simple _; _ }
      | Reachable { named = Rec_info _; _ }
      | Reachable_try_reify { named = Prim _; _ }
      | Reachable_try_reify { named = Simple _; _ }
      | Reachable_try_reify { named = Rec_info _; _ } ->
        assert false
    end
    | Zero_terms -> assert false
  end
  | Static_consts _ -> begin
    match descr with
    | Zero_terms -> zero
    | Single_term _ | Multiple_bindings_to_symbols _ -> assert false
  end
  | Simple _ -> begin
    match descr with
    | Single_term { simplified_defining_expr; _ } -> begin
      match simplified_defining_expr with
      | Reachable { named = Simple _; _ }
      | Reachable_try_reify { named = Simple _; _ } ->
        (* A simple has 0 benefit. *)
        zero
      | Invalid _
      | Reachable { named = Set_of_closures _; _ }
      | Reachable { named = Prim _; _ }
      | Reachable { named = Rec_info _; _ }
      | Reachable_try_reify { named = Set_of_closures _; _ }
      | Reachable_try_reify { named = Prim _; _ }
      | Reachable_try_reify { named = Rec_info _; _ } ->
        assert false
    end
    | Zero_terms | Multiple_bindings_to_symbols _ -> assert false
  end
  | Prim (original_prim, _) -> begin
    match descr with
    | Single_term { simplified_defining_expr; _ } -> begin
      match simplified_defining_expr with
      | Reachable { named = Prim (rewritten_prim, _); _ }
      | Reachable_try_reify { named = Prim (rewritten_prim, _); _ } ->
        if Flambda_primitive.equal original_prim rewritten_prim
        then zero
        else Removed_operations.prim original_prim
      | Reachable { named = Simple _; _ }
      | Reachable { named = Set_of_closures _; _ }
      | Reachable_try_reify { named = Simple _; _ }
      | Reachable_try_reify { named = Set_of_closures _; _ }
      | Invalid _ ->
        Removed_operations.prim original_prim
      | Reachable { named = Rec_info _; _ }
      | Reachable_try_reify { named = Rec_info _; _ } ->
        assert false
    end
    | Zero_terms | Multiple_bindings_to_symbols _ -> assert false
  end
  | Rec_info _ -> zero

let simplify_named dacc bound_pattern named ~simplify_toplevel =
  try
    let simplified_named =
      simplify_named0 ~simplify_toplevel dacc bound_pattern named
    in
    simplified_named, removed_operations named simplified_named
  with Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf
      "\n\
       %sContext is:%s simplifying [Let] binding@ %a =@ %a@ with downwards \
       accumulator:@ %a\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Bound_pattern.print bound_pattern Named.print named DA.print dacc;
    Printexc.raise_with_backtrace Misc.Fatal_error bt
