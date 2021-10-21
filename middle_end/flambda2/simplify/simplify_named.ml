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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

let record_any_symbol_projection dacc (defining_expr : Simplified_named.t)
    (prim : P.t) args bound_pattern ~bound_var named =
  (* Projections from symbols bound to variables are important to remember,
     since if such a variable occurs in a set of closures environment or other
     value that can potentially be lifted, the knowledge that the variable is
     equal to a symbol projection can make the difference between being able to
     lift and not being able to lift. We try to avoid recording symbol
     projections whose answer is known (in particular the answer is a symbol or
     a constant), since such symbol projection knowledge doesn't affect lifting
     decisions. *)
  let can_record_proj =
    (* We only need to record a projection if the defining expression remains as
       a [Prim]. In particular if the defining expression simplified to a
       variable (via the [Simple] constructor), then in the event that the
       variable is itself a symbol projection, the environment will already know
       this fact.

       We don't need to record a projection if we are currently at toplevel,
       since any variable involved in a constant to be lifted from that position
       will also be at toplevel. *)
    (not (DE.at_unit_toplevel (DA.denv dacc)))
    &&
    match defining_expr with
    | Reachable { named = Prim _; _ } -> true
    | Reachable { named = Simple _ | Set_of_closures _ | Rec_info _; _ }
    | Invalid _ ->
      false
  in
  let proj =
    let module SP = Symbol_projection in
    (* The [args] being queried here are the post-simplification arguments of
       the primitive, so we can directly read off whether they are symbols or
       constants, as needed. *)
    match prim with
    | Nullary (Optimised_out _) | Nullary (Probe_is_enabled _) -> None
    | Unary (Project_var { project_from; var }, _) when can_record_proj -> begin
      match args with
      | [closure] ->
        Simple.pattern_match' closure
          ~const:(fun _ -> None)
          ~symbol:(fun symbol_projected_from ~coercion:_ ->
            Some
              (SP.create symbol_projected_from
                 (SP.Projection.project_var project_from var)))
          ~var:(fun _ ~coercion:_ -> None)
      | [] | _ :: _ ->
        Misc.fatal_errorf "Expected one argument:@ %a@ =@ %a"
          Bound_pattern.print bound_pattern Named.print named
    end
    | Binary (Block_load _, _, _) when can_record_proj -> begin
      match args with
      | [block; index] ->
        Simple.pattern_match index
          ~const:(fun const ->
            match Reg_width_const.descr const with
            | Tagged_immediate imm ->
              Simple.pattern_match' block
                ~const:(fun _ -> None)
                ~symbol:(fun symbol_projected_from ~coercion:_ ->
                  let index = Targetint_31_63.to_targetint imm in
                  Some
                    (SP.create symbol_projected_from
                       (SP.Projection.block_load ~index)))
                ~var:(fun _ ~coercion:_ -> None)
            | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
            | Naked_nativeint _ ->
              Misc.fatal_errorf "Kind error for [Block_load] index:@ %a@ =@ %a"
                Bound_pattern.print bound_pattern Named.print named)
          ~name:(fun _ ~coercion:_ -> None)
      | [] | _ :: _ ->
        Misc.fatal_errorf "Expected two arguments:@ %a@ =@ %a"
          Bound_pattern.print bound_pattern Named.print named
    end
    | Unary
        ( ( Duplicate_block _ | Duplicate_array _ | Is_int | Get_tag
          | Array_length _ | Bigarray_length _ | String_length _
          | Int_as_pointer | Opaque_identity | Int_arith _ | Float_arith _
          | Num_conv _ | Boolean_not | Reinterpret_int64_as_float
          | Unbox_number _ | Box_number _ | Select_closure _ | Project_var _ ),
          _ )
    | Binary
        ( ( Block_load _ | Array_load _ | String_or_bigstring_load _
          | Bigarray_load _ | Phys_equal _ | Int_arith _ | Int_shift _
          | Int_comp _ | Float_arith _ | Float_comp _ ),
          _,
          _ )
    | Ternary
        ( (Block_set _ | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _),
          _,
          _,
          _ )
    | Variadic ((Make_block _ | Make_array _), _) ->
      None
  in
  match proj with
  | None -> dacc
  | Some proj ->
    let var = Bound_var.var bound_var in
    DA.map_denv dacc ~f:(fun denv -> DE.add_symbol_projection denv var proj)

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
      then Simplified_named.reachable named
      else Simplified_named.reachable (Named.create_simple simple)
    in
    Simplify_named_result.have_simplified_to_single_term dacc bound_pattern
      defining_expr ~original_defining_expr:named
  | Prim (prim, dbg) ->
    let bound_var = Bound_pattern.must_be_singleton bound_pattern in
    let term, env_extension, simplified_args, dacc =
      (* [simplified_args] has to be returned from [simplify_primitive] because
         in at least one case (for [Project_var]), the simplifier may return
         something other than a [Prim] as the [term]. However we need the
         simplified arguments of the actual primitive for the symbol projection
         check below. *)
      Simplify_primitive.simplify_primitive dacc prim dbg ~result_var:bound_var
    in
    let kind = P.result_kind' prim in
    let dacc =
      (* CR mshinwell: It's a bit weird that the env_extension is added to the
         typing env here; couldn't it just have been returned already added to
         [dacc]? *)
      DA.map_denv dacc ~f:(fun denv ->
          DE.add_variable_and_extend_typing_environment denv bound_var
            (T.unknown kind) env_extension)
    in
    (* CR mshinwell: Add check along the lines of: types are unknown whenever
       [not (P.With_fixed_value.eligible prim)] holds. *)
    (* Primitives with generative effects correspond to allocations. Without
       this check, we could end up lifting definitions that have a type that
       looks like an allocation but that are instead a projection from a bigger
       structure. *)
    let allow_lifting =
      (* CR mshinwell: We probably shouldn't lift if the let binding is going to
         be deleted, as lifting may cause [Dominator]-scoped bindings to be
         inserted, that cannot be deleted. However this situation probably
         doesn't arise that much, and won't be an issue once we can lift
         [Dominator]-scoped bindings. *)
      P.only_generative_effects prim
      && Name_mode.is_normal (Bound_var.name_mode bound_var)
    in
    let defining_expr, dacc, ty =
      Reification.try_to_reify dacc term ~bound_to:bound_var
        ~kind_of_bound_to:kind ~allow_lifting
    in
    let defining_expr =
      if T.is_bottom (DA.typing_env dacc) ty
      then Simplified_named.invalid ()
      else defining_expr
    in
    let dacc =
      record_any_symbol_projection dacc defining_expr prim simplified_args
        bound_pattern ~bound_var named
    in
    Simplify_named_result.have_simplified_to_single_term dacc bound_pattern
      defining_expr ~original_defining_expr:named
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
    let dacc = DA.add_lifted_constant dacc (LC.concat lifted_constants) in
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
      then Simplified_named.reachable named
      else Simplified_named.reachable (Named.create_rec_info new_rec_info_expr)
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
      | Reachable { named = Set_of_closures _; _ } ->
        (* Nothing was deleted, there is no need to adjust the negative
           benefit *)
        zero
      | Invalid _
      | Reachable { named = Prim _; _ }
      | Reachable { named = Simple _; _ }
      | Reachable { named = Rec_info _; _ } ->
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
      | Reachable { named = Simple _; _ } ->
        (* A simple has 0 benefit.*)
        zero
      | Invalid _
      | Reachable { named = Set_of_closures _; _ }
      | Reachable { named = Prim _; _ }
      | Reachable { named = Rec_info _; _ } ->
        assert false
    end
    | Zero_terms | Multiple_bindings_to_symbols _ -> assert false
  end
  | Prim (original_prim, _) -> begin
    match descr with
    | Single_term { simplified_defining_expr; _ } -> begin
      match simplified_defining_expr with
      | Reachable { named = Prim (rewritten_prim, _); _ } ->
        if Flambda_primitive.equal original_prim rewritten_prim
        then zero
        else Removed_operations.prim original_prim
      | Reachable { named = Simple _; _ }
      | Reachable { named = Set_of_closures _; _ }
      | Invalid _ ->
        Removed_operations.prim original_prim
      | Reachable { named = Rec_info _; _ } -> assert false
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
