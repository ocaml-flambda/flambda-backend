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

open! Simplify_import

let create_lifted_constant (dacc, lifted_constants)
    (pat : Bound_static.Pattern.t) static_const =
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
      Function_slot.Lmap.map
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
    ~simplify_function_body : Simplify_named_result.t Or_invalid.t =
  match named with
  | Simple simple ->
    let bound_var = Bound_pattern.must_be_singleton bound_pattern in
    let min_name_mode = Bound_var.name_mode bound_var in
    let ty = S.simplify_simple dacc simple ~min_name_mode in
    let new_simple = T.get_alias_exn ty in
    let dacc = DA.add_variable dacc bound_var ty in
    let defining_expr =
      if simple == new_simple
      then Simplified_named.create named
      else Simplified_named.create (Named.create_simple new_simple)
    in
    Ok
      (Simplify_named_result.create dacc
         [ { Expr_builder.let_bound = bound_pattern;
             simplified_defining_expr = defining_expr;
             original_defining_expr = Some named
           } ])
  | Prim (prim, dbg) -> (
    let bound_var = Bound_pattern.must_be_singleton bound_pattern in
    let dbg = DE.add_inlined_debuginfo (DA.denv dacc) dbg in
    let { Simplify_primitive_result.simplified_named;
          extra_bindings;
          try_reify;
          dacc
        } =
      Simplify_primitive.simplify_primitive dacc prim dbg ~result_var:bound_var
    in
    match simplified_named with
    | Invalid -> Invalid
    | Ok simplified_named ->
      if Flambda_features.check_invariants ()
         && not
              (TE.mem (DA.typing_env dacc) (Name.var (Bound_var.var bound_var)))
      then
        Misc.fatal_errorf "Primitive %a = %a did not yield a result var"
          Bound_var.print bound_var P.print prim;
      if not try_reify
      then
        Ok
          (* Any additional bindings apart from the original one being
             simplified are inserted before such original binding, allowing the
             simplified version of that binding to reference the new
             variables. *)
          (Simplify_named_result.create dacc
             (extra_bindings
             @ [ { Expr_builder.let_bound = bound_pattern;
                   simplified_defining_expr = simplified_named;
                   original_defining_expr = Some named
                 } ]))
      else
        (* Primitives with generative effects correspond to allocations. Without
           this check, we could end up lifting definitions that have a type that
           looks like an allocation but that are instead a projection from a
           bigger structure. *)
        let allow_lifting =
          (* CR mshinwell: Perhaps this could be relaxed to
             [at_most_generative_effects], but there are concerns about
             compilation speed *)
          P.only_generative_effects prim
          && Name_mode.is_normal (Bound_var.name_mode bound_var)
        in
        let defining_expr, dacc =
          Reification.try_to_reify dacc dbg simplified_named ~bound_to:bound_var
            ~kind_of_bound_to:(P.result_kind' prim) ~allow_lifting
        in
        Or_invalid.map defining_expr ~f:(fun defining_expr ->
            Simplify_named_result.create dacc
              (extra_bindings
              @ [ { Expr_builder.let_bound = bound_pattern;
                    simplified_defining_expr = defining_expr;
                    original_defining_expr = Some named
                  } ])))
  | Set_of_closures set_of_closures ->
    Ok
      (Simplify_set_of_closures.simplify_non_lifted_set_of_closures dacc
         bound_pattern set_of_closures ~simplify_function_body)
  | Static_consts static_consts ->
    let bound_static = Bound_pattern.must_be_static bound_pattern in
    let binds_symbols = Bound_static.binds_symbols bound_static in
    if binds_symbols && not (DE.at_unit_toplevel (DA.denv dacc))
    then
      Misc.fatal_errorf
        "[Let] binding symbols is only allowed at the toplevel of compilation \
         units (not even at the toplevel of function bodies):@ %a@ =@ %a"
        Bound_pattern.print bound_pattern Named.print named;
    let bound_static, static_consts, dacc =
      try
        Simplify_static_const.simplify_static_consts dacc bound_static
          static_consts ~simplify_function_body
      with Misc.Fatal_error ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf
          "\n\
           %tContext is:%t simplifying 'let symbol' binding of@ %a@ with \
           downwards accumulator:@ %a\n"
          Flambda_colours.error Flambda_colours.pop Bound_static.print
          bound_static DA.print dacc;
        Printexc.raise_with_backtrace Misc.Fatal_error bt
    in
    let dacc, lifted_constants =
      ListLabels.fold_left2
        (Bound_static.to_list bound_static)
        (Rebuilt_static_const.Group.to_list static_consts)
        ~init:(dacc, []) ~f:create_lifted_constant
    in
    let dacc =
      DA.add_to_lifted_constant_accumulator dacc
        (LCS.singleton (LC.concat lifted_constants))
    in
    (* We don't need to return any bindings; [Simplify_expr.simplify_let] will
       create the "let symbol" binding when it sees the lifted constant. *)
    Ok (Simplify_named_result.create dacc [])
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
      then Simplified_named.create named
      else Simplified_named.create (Named.create_rec_info new_rec_info_expr)
    in
    Ok
      (Simplify_named_result.create dacc
         [ { Expr_builder.let_bound = bound_pattern;
             simplified_defining_expr = defining_expr;
             original_defining_expr = Some named
           } ])

let removed_operations ~(original : Named.t) (result : _ Or_invalid.t) =
  let zero = Removed_operations.zero in
  match result with
  | Invalid ->
    (* We're not removing any instructions that would actually have been
       executed. *)
    zero
  | Ok result -> (
    match original with
    | Set_of_closures _ ->
      if Simplify_named_result.was_lifted_set_of_closures result
         || Simplify_named_result.no_bindings result
      then Removed_operations.alloc
      else zero
    | Static_consts _ ->
      (* There are no operations to remove in a [Static_consts] binding. *)
      zero
    | Simple _ ->
      (* [Simple]s only simplify to other [Simple]s. *)
      zero
    | Prim (original_prim, _) ->
      if List.exists
           (fun ({ simplified_defining_expr; _ } :
                  Expr_builder.binding_to_place) ->
             match simplified_defining_expr with
             | { named = Prim (rewritten_prim, _); _ } ->
               Flambda_primitive.equal original_prim rewritten_prim
             | { named = Simple _ | Set_of_closures _ | Rec_info _; _ } -> false)
           (Simplify_named_result.bindings_to_place result)
      then zero
      else Removed_operations.prim original_prim
    | Rec_info _ -> zero)

let simplify_named dacc bound_pattern named ~simplify_function_body =
  try
    let simplified_named_or_invalid =
      simplify_named0 ~simplify_function_body dacc bound_pattern named
    in
    ( simplified_named_or_invalid,
      removed_operations ~original:named simplified_named_or_invalid )
  with Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf
      "\n\
       %tContext is:%t simplifying [Let] binding@ %a =@ %a@ with downwards \
       accumulator:@ %a\n"
      Flambda_colours.error Flambda_colours.pop Bound_pattern.print
      bound_pattern Named.print named DA.print dacc;
    Printexc.raise_with_backtrace Misc.Fatal_error bt
