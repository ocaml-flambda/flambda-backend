(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import
module BP = Bound_parameter
module LC = Lifted_constant
module LCS = Lifted_constant_state
module P = Flambda_primitive
module RE = Rebuilt_expr
module UA = Upwards_acc
module UE = Upwards_env
module DA = Downwards_acc
module VB = Bound_var

type let_creation_result =
  | Defining_expr_deleted_at_compile_time
  | Defining_expr_deleted_at_runtime
  | Nothing_deleted_at_runtime

let equal_let_creation_results r1 r2 =
  match r1, r2 with
  | Defining_expr_deleted_at_compile_time, Defining_expr_deleted_at_compile_time
  | Defining_expr_deleted_at_runtime, Defining_expr_deleted_at_runtime
  | Nothing_deleted_at_runtime, Nothing_deleted_at_runtime ->
    true
  | ( ( Defining_expr_deleted_at_compile_time | Defining_expr_deleted_at_runtime
      | Nothing_deleted_at_runtime ),
      _ ) ->
    false

let add_set_of_closures_offsets ~is_phantom named uacc =
  let add_set uacc set_of_closures =
    match UA.slot_offsets uacc with
    | Unknown -> uacc
    | Known slot_offsets ->
      let slot_offsets =
        Slot_offsets.add_set_of_closures slot_offsets ~is_phantom
          set_of_closures
      in
      UA.with_slot_offsets uacc (Known slot_offsets)
  in
  let add_offsets_from_code uacc code =
    match UA.slot_offsets uacc with
    | Unknown -> uacc
    | Known slot_offsets ->
      let dacc = UA.creation_dacc uacc in
      let code_id = Code.code_id code in
      let from_function =
        try Code_id.Map.find code_id (DA.slot_offsets dacc)
        with Not_found ->
          Misc.fatal_errorf "Missing offsets for code ID %a" Code_id.print
            code_id
      in
      let slot_offsets =
        Slot_offsets.add_offsets_from_function slot_offsets ~from_function
      in
      UA.with_slot_offsets uacc (Known slot_offsets)
  in
  match (named : Named.t) with
  | Set_of_closures s -> add_set uacc s
  | Rec_info _ | Simple _ | Prim _ -> uacc
  | Static_consts group ->
    Static_const_group.to_list group
    |> List.fold_left
         (fun acc static_const_or_code ->
           match (static_const_or_code : Static_const_or_code.t) with
           | Static_const (Set_of_closures s) -> add_set acc s
           | Code code -> add_offsets_from_code acc code
           | Deleted_code
           | Static_const
               ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
               | Boxed_nativeint _ | Immutable_float_block _
               | Immutable_float_array _ | Mutable_string _ | Immutable_string _
               | Empty_array ) ->
             acc)
         uacc

let create_let uacc (bound_vars : Bound_pattern.t) (defining_expr : Named.t)
    ~free_names_of_defining_expr ~body ~cost_metrics_of_defining_expr =
  (* The name occurrences component of [uacc] is expected to be in the state
     described in the comment at the top of [Simplify_let.rebuild_let]. *)
  let generate_phantom_lets = UA.generate_phantom_lets uacc in
  let free_names_of_body = UA.name_occurrences uacc in
  let bound_vars, keep_binding, let_creation_result =
    let greatest_name_mode =
      match bound_vars with
      | Singleton bound_var ->
        (* We avoid the closure allocation (below) in this case. *)
        Name_occurrences.greatest_name_mode_var free_names_of_body
          (VB.var bound_var)
      | Set_of_closures _ ->
        Bound_pattern.fold_all_bound_vars bound_vars
          ~init:Name_mode.Or_absent.absent
          ~f:(fun (greatest_name_mode : Name_mode.Or_absent.t) bound_var ->
            let name_mode =
              Name_occurrences.greatest_name_mode_var free_names_of_body
                (VB.var bound_var)
            in
            match name_mode, greatest_name_mode with
            | Absent, Absent -> Name_mode.Or_absent.absent
            | Absent, Present _ -> greatest_name_mode
            | Present _, Absent -> name_mode
            | Present name_mode, Present greatest_name_mode ->
              Name_mode.join_in_terms name_mode greatest_name_mode
              |> Name_mode.Or_absent.present)
      | Static _ -> assert false
      (* see below *)
    in
    let declared_name_mode = Bound_pattern.name_mode bound_vars in
    begin
      match
        Name_mode.Or_absent.compare_partial_order greatest_name_mode
          (Name_mode.Or_absent.present declared_name_mode)
      with
      | None -> ()
      | Some c ->
        if c <= 0
        then ()
        else
          Misc.fatal_errorf
            "[Let]-binding declares variable(s) %a (mode %a) to be bound to@ \
             %a,@ but there exist occurrences for such variable(s) at a higher \
             mode@ (>= %a)@ in the body (free names %a):@ %a"
            Bound_pattern.print bound_vars Name_mode.print declared_name_mode
            Named.print defining_expr Name_mode.Or_absent.print
            greatest_name_mode Name_occurrences.print free_names_of_body
            (RE.print (UA.are_rebuilding_terms uacc))
            body
    end;
    if not (Named.at_most_generative_effects defining_expr)
    then begin
      if not (Name_mode.is_normal declared_name_mode)
      then
        Misc.fatal_errorf
          "Cannot [Let]-bind non-normal variable(s) to a [Named] that has more \
           than generative effects:@ %a@ =@ %a"
          Bound_pattern.print bound_vars Named.print defining_expr;
      bound_vars, Some Name_mode.normal, Nothing_deleted_at_runtime
    end
    else
      let is_depth =
        match defining_expr with
        | Rec_info _ -> true
        | Simple _ | Prim _ | Set_of_closures _ | Static_consts _ -> false
      in
      let has_uses = Name_mode.Or_absent.is_present greatest_name_mode in
      let can_phantomise =
        (not is_depth)
        && Bound_pattern.exists_all_bound_vars bound_vars ~f:(fun bound_var ->
               Variable.user_visible (VB.var bound_var))
      in
      let will_delete_binding =
        (* CR-someday mshinwell: This should detect whether there is any
           provenance info associated with the variable. If there isn't, the
           [Let] can be deleted even if debugging information is being
           generated. *)
        not (has_uses || (generate_phantom_lets && can_phantomise))
      in
      if will_delete_binding
      then bound_vars, None, Defining_expr_deleted_at_compile_time
      else
        let name_mode =
          match greatest_name_mode with
          | Absent -> Name_mode.phantom
          | Present name_mode -> name_mode
        in
        assert (Name_mode.can_be_in_terms name_mode);
        let bound_vars = Bound_pattern.with_name_mode bound_vars name_mode in
        if Name_mode.is_normal name_mode
        then bound_vars, Some name_mode, Nothing_deleted_at_runtime
        else
          (* CR lmaurer for poechsel: This seems to suggest (and the code toward
             the end of [make_new_let_bindings] seems to assume) that we're
             phantomising the binding right now, but in fact it may have been
             phantom already if there has already been a simplifier pass.
             Presumably this will cause double-counting of deleted code, which
             is to say, the second pass will get as much credit for phantomising
             as the first one did. *)
          bound_vars, Some name_mode, Defining_expr_deleted_at_runtime
  in
  (* CR-someday mshinwell: When leaving behind phantom lets, maybe we should
     turn the defining expressions into simpler ones by using the type, if
     possible. For example an Unbox_naked_int64 or something could potentially
     turn into a variable. This defining expression usually never exists as the
     types propagate the information forward. mshinwell: this might be done now
     in Simplify_named, check. *)
  match keep_binding with
  | None -> body, uacc, let_creation_result
  | Some name_mode ->
    let is_phantom = Name_mode.is_phantom name_mode in
    let free_names_of_body = UA.name_occurrences uacc in
    let free_names_of_defining_expr =
      if not is_phantom
      then free_names_of_defining_expr
      else
        Name_occurrences.downgrade_occurrences_at_strictly_greater_name_mode
          free_names_of_defining_expr name_mode
    in
    let free_names_of_let =
      let without_bound_vars =
        Bound_pattern.fold_all_bound_vars bound_vars ~init:free_names_of_body
          ~f:(fun free_names bound_var ->
            Name_occurrences.remove_var free_names (VB.var bound_var))
      in
      Name_occurrences.union without_bound_vars free_names_of_defining_expr
    in
    let uacc =
      UA.add_cost_metrics_and_with_name_occurrences uacc
        (Cost_metrics.increase_due_to_let_expr ~is_phantom
           ~cost_metrics_of_defining_expr)
        free_names_of_let
    in
    let uacc =
      if Are_rebuilding_terms.do_not_rebuild_terms
           (UA.are_rebuilding_terms uacc)
      then uacc
      else add_set_of_closures_offsets ~is_phantom defining_expr uacc
    in
    ( RE.create_let
        (UA.are_rebuilding_terms uacc)
        bound_vars defining_expr ~body ~free_names_of_body,
      uacc,
      let_creation_result )

let create_coerced_singleton_let uacc var defining_expr
    ~coercion_from_defining_expr_to_var ~free_names_of_defining_expr ~body
    ~cost_metrics_of_defining_expr =
  if Coercion.is_id coercion_from_defining_expr_to_var
  then
    create_let uacc
      (Bound_pattern.singleton var)
      defining_expr ~free_names_of_defining_expr ~body
      ~cost_metrics_of_defining_expr
  else
    match (defining_expr : Named.t) with
    | Simple simple ->
      let defining_expr =
        Simple.apply_coercion_exn simple coercion_from_defining_expr_to_var
        |> Named.create_simple
      in
      create_let uacc
        (Bound_pattern.singleton var)
        defining_expr ~free_names_of_defining_expr ~body
        ~cost_metrics_of_defining_expr
    | Prim _ | Set_of_closures _ | Static_consts _ | Rec_info _ -> (
      let uncoerced_var =
        let name = "uncoerced_" ^ Variable.raw_name (VB.var var) in
        Variable.create name
      in
      (* Generate [let var = uncoerced_var @ <coercion>] *)
      let ((body, uacc, inner_result) as inner) =
        let defining_simple =
          Simple.with_coercion (Simple.var uncoerced_var)
            coercion_from_defining_expr_to_var
        in
        let defining_expr = defining_simple |> Named.create_simple in
        let free_names_of_defining_expr = Named.free_names defining_expr in
        let cost_metrics_of_defining_expr =
          Code_size.simple defining_simple |> Cost_metrics.from_size
        in
        create_let uacc
          (Bound_pattern.singleton var)
          defining_expr ~free_names_of_defining_expr ~body
          ~cost_metrics_of_defining_expr
      in
      let generate_outer_binding name_mode =
        (* Generate [let uncoerced_var = <defining_expr>] *)
        let ((_body, _uacc, outer_result) as outer) =
          let bound =
            Bound_pattern.singleton (VB.create uncoerced_var name_mode)
          in
          create_let uacc bound defining_expr ~free_names_of_defining_expr ~body
            ~cost_metrics_of_defining_expr
        in
        (* Can't somehow end up with one but not the other *)
        assert (equal_let_creation_results inner_result outer_result);
        outer
      in
      match inner_result with
      | Defining_expr_deleted_at_compile_time ->
        (* No need to wrap what's not there after all *)
        inner
      | Defining_expr_deleted_at_runtime ->
        generate_outer_binding Name_mode.phantom
      | Nothing_deleted_at_runtime -> generate_outer_binding Name_mode.normal)

let make_new_let_bindings uacc
    ~(bindings_outermost_first : Simplify_named_result.binding_to_place list)
    ~body =
  (* The name occurrences component of [uacc] is expected to be in the state
     described in the comment at the top of [Simplify_let.rebuild_let]. *)
  ListLabels.fold_left (List.rev bindings_outermost_first) ~init:(body, uacc)
    ~f:(fun
         (expr, uacc)
         ({ let_bound; simplified_defining_expr; original_defining_expr } :
           Simplify_named_result.binding_to_place)
       ->
      match (simplified_defining_expr : Simplified_named.t) with
      | Invalid ->
        let uacc =
          UA.with_name_occurrences uacc ~name_occurrences:Name_occurrences.empty
          |> UA.notify_added ~code_size:Code_size.invalid
        in
        ( RE.create_invalid
            (Defining_expr_of_let (let_bound, original_defining_expr)),
          uacc )
      | Reachable
          { named = defining_expr;
            free_names = free_names_of_defining_expr;
            cost_metrics = cost_metrics_of_defining_expr
          }
      | Reachable_try_reify
          { named = defining_expr;
            free_names = free_names_of_defining_expr;
            cost_metrics = cost_metrics_of_defining_expr
          } ->
        let defining_expr = Simplified_named.to_named defining_expr in
        let expr, uacc, creation_result =
          match (let_bound : Bound_pattern.t) with
          | Singleton _ | Set_of_closures _ ->
            create_let uacc let_bound defining_expr ~free_names_of_defining_expr
              ~body:expr ~cost_metrics_of_defining_expr
          | Static _ ->
            (* Since [Simplified_named] doesn't permit the [Static_consts] case,
               this must be a malformed binding. *)
            Misc.fatal_errorf
              "Mismatch between bound name(s) and defining expression:@ %a@ =@ \
               %a"
              Bound_pattern.print let_bound Named.print defining_expr
        in
        let uacc =
          match creation_result with
          | Nothing_deleted_at_runtime -> uacc
          | Defining_expr_deleted_at_compile_time
          | Defining_expr_deleted_at_runtime -> begin
            match (original_defining_expr : Named.t) with
            | Prim (prim, _dbg) ->
              UA.notify_removed ~operation:(Removed_operations.prim prim) uacc
            | Set_of_closures _ ->
              UA.notify_removed ~operation:Removed_operations.alloc uacc
            | Simple _ | Static_consts _ | Rec_info _ -> uacc
          end
        in
        expr, uacc)

let create_raw_let_symbol uacc bound_static static_consts ~body =
  (* Upon entry to this function, [UA.name_occurrences uacc] must precisely
     indicate the free names of [body]. *)
  let bindable = Bound_pattern.static bound_static in
  let free_names_of_static_consts =
    Rebuilt_static_const.Group.free_names static_consts
  in
  let free_names_of_body = UA.name_occurrences uacc in
  let free_names_of_let =
    (* Care: these bindings can be recursive (e.g. via a set of closures). *)
    let name_occurrences =
      Name_occurrences.union free_names_of_static_consts free_names_of_body
    in
    Code_id_or_symbol.Set.fold
      (fun code_id_or_sym free_names ->
        Name_occurrences.remove_code_id_or_symbol free_names code_id_or_sym)
      (Bound_static.everything_being_defined bound_static)
      name_occurrences
  in
  let uacc =
    UA.with_name_occurrences uacc ~name_occurrences:free_names_of_let
    |> UA.add_cost_metrics
         (Cost_metrics.increase_due_to_let_expr
            ~is_phantom:
              false
              (* Static consts always have zero cost metrics at present. *)
            ~cost_metrics_of_defining_expr:Cost_metrics.zero)
  in
  if Are_rebuilding_terms.do_not_rebuild_terms (UA.are_rebuilding_terms uacc)
  then RE.term_not_rebuilt, uacc
  else
    let defining_expr = Rebuilt_static_const.Group.to_named static_consts in
    let uacc =
      add_set_of_closures_offsets ~is_phantom:false defining_expr uacc
    in
    ( RE.create_let
        (UA.are_rebuilding_terms uacc)
        bindable defining_expr ~body ~free_names_of_body,
      uacc )

let create_let_symbol0 uacc (bound_static : Bound_static.t)
    (static_consts : Rebuilt_static_const.Group.t) ~body =
  (* Upon entry to this function, [UA.name_occurrences uacc] must precisely
     indicate the free names of [body]. *)
  let will_bind_code = Bound_static.binds_code bound_static in
  (* Turn pieces of code that are only referenced in [newer_version_of] fields
     into [Deleted_code]. *)
  let code_ids_to_make_deleted =
    if not will_bind_code
    then Code_id.Set.empty
    else
      let all_code_ids_bound_names =
        Bound_static.code_being_defined bound_static
      in
      Code_id.Set.fold
        (fun bound_code_id result ->
          let can_make_deleted =
            match UA.reachable_code_ids uacc with
            | Unknown -> false
            | Known { live_code_ids; ancestors_of_live_code_ids = _ } ->
              not (Code_id.Set.mem bound_code_id live_code_ids)
          in
          if can_make_deleted
          then Code_id.Set.add bound_code_id result
          else result)
        all_code_ids_bound_names Code_id.Set.empty
  in
  let static_consts =
    if not will_bind_code
    then static_consts
    else
      Rebuilt_static_const.Group.map static_consts ~f:(fun static_const ->
          Rebuilt_static_const.make_code_deleted static_const
            ~if_code_id_is_member_of:code_ids_to_make_deleted)
  in
  let expr, uacc =
    create_raw_let_symbol uacc bound_static static_consts ~body
  in
  let uacc =
    if not will_bind_code
    then uacc
    else
      Rebuilt_static_const.Group.pieces_of_code_for_cmx static_consts
      |> UA.remember_code_for_cmx uacc
  in
  expr, uacc

let remove_unused_value_slots uacc static_const =
  Rebuilt_static_const.map_set_of_closures static_const
    ~f:(fun set_of_closures ->
      let name_occurrences = UA.used_value_slots uacc in
      let value_slots =
        Value_slot.Map.filter
          (fun value_slot _ ->
            Name_occurrences.value_slot_is_used_or_imported name_occurrences
              value_slot)
          (Set_of_closures.value_slots set_of_closures)
      in
      Set_of_closures.create ~value_slots
        (Set_of_closures.alloc_mode set_of_closures)
        (Set_of_closures.function_decls set_of_closures))

let create_let_symbols uacc lifted_constant ~body =
  let bound_static = LC.bound_static lifted_constant in
  let symbol_projections = LC.symbol_projections lifted_constant in
  let static_consts =
    Rebuilt_static_const.Group.map
      (LC.defining_exprs lifted_constant)
      ~f:(remove_unused_value_slots uacc)
  in
  let expr, uacc = create_let_symbol0 uacc bound_static static_consts ~body in
  Variable.Map.fold
    (fun var proj (expr, uacc) ->
      let rec apply_projection proj coercion_from_proj_to_var =
        match LC.apply_projection lifted_constant proj with
        | Some simple ->
          let stop_here () =
            let coercion_from_simple_to_var =
              (* Since [simple] is just an evaluated form of [proj] *)
              coercion_from_proj_to_var
            in
            ( Named.create_simple simple,
              coercion_from_simple_to_var,
              Code_size.simple simple )
          in
          (* If the projection is from one of the symbols bound by the "let
             symbol" that we've just created, we'll always end up here, avoiding
             any problem about where to do the projection versus the
             initialisation of a possibly-recursive group of symbols. We may end
             up with a "variable = variable" [Let] here, but [To_cmm] (or a
             subsequent pass of [Simplify]) will remove it. This is the same
             situation as when continuations are inlined; we can't use a name
             permutation to resolve the problem as both [var] and [var'] may
             occur in [expr], and permuting could cause an unbound name.

             It is possible for one projection to yield a variable that is in
             turn defined by another symbol projection, so we need to expand
             transitively. *)
          Simple.pattern_match' simple
            ~const:(fun _ -> stop_here ())
            ~symbol:(fun _ ~coercion:_ ->
              (* We're not dropping the coercion: it gets used in [stop_here] *)
              stop_here ())
            ~var:(fun var' ~coercion:coercion_from_var'_to_simple ->
              match Variable.Map.find var' symbol_projections with
              | exception Not_found -> stop_here ()
              | proj' ->
                let coercion_from_var'_to_proj =
                  (* Since [simple] is just an evaluated form of [proj] *)
                  coercion_from_var'_to_simple
                in
                let coercion_from_proj'_to_proj =
                  (* Since [proj'] is just an expanded form of [var'] *)
                  coercion_from_var'_to_proj
                in
                let coercion_from_proj'_to_var =
                  Coercion.compose_exn coercion_from_proj'_to_proj
                    ~then_:coercion_from_proj_to_var
                in
                apply_projection proj' coercion_from_proj'_to_var)
        | None ->
          let prim : P.t =
            let symbol = Simple.symbol (Symbol_projection.symbol proj) in
            match Symbol_projection.projection proj with
            | Block_load { index } ->
              let index = Simple.const_int index in
              let block_access_kind : P.Block_access_kind.t =
                Values
                  { tag = Known Tag.Scannable.zero;
                    size = Unknown;
                    field_kind = Any_value
                  }
              in
              Binary (Block_load (block_access_kind, Immutable), symbol, index)
            | Project_value_slot { project_from; value_slot } ->
              Unary (Project_value_slot { project_from; value_slot }, symbol)
          in
          ( Named.create_prim prim Debuginfo.none,
            coercion_from_proj_to_var,
            Code_size.prim prim )
      in
      (* It's possible that this might create duplicates of the same projection
         operation, but it's unlikely there will be a significant number, and
         since we're at toplevel we tolerate them. *)
      let ( defining_expr,
            coercion_from_defining_expr_to_var,
            code_size_of_defining_expr ) =
        apply_projection proj Coercion.id
      in
      let cost_metrics_of_defining_expr =
        Cost_metrics.from_size code_size_of_defining_expr
      in
      let free_names_of_defining_expr = Named.free_names defining_expr in
      let expr, uacc, _ =
        create_coerced_singleton_let uacc
          (VB.create var Name_mode.normal)
          defining_expr ~coercion_from_defining_expr_to_var
          ~free_names_of_defining_expr ~body:expr ~cost_metrics_of_defining_expr
      in
      (* Not removing any operation here as the let bindings would have been
         created for the first time here.*)
      expr, uacc)
    symbol_projections (expr, uacc)

let place_lifted_constants uacc ~lifted_constants_from_defining_expr
    ~lifted_constants_from_body ~put_bindings_around_body ~body =
  (* Lifted constants are placed as soon as they reach toplevel. *)
  let uacc = UA.with_lifted_constants uacc LCS.empty in
  (* Place constants whose definitions must go at the current binding. *)
  let place_constants uacc ~around constants =
    LCS.fold_innermost_first constants ~init:(around, uacc)
      ~f:(fun (body, uacc) lifted_const ->
        create_let_symbols uacc lifted_const ~body)
  in
  let body, uacc =
    place_constants uacc ~around:body lifted_constants_from_body
  in
  let body, uacc = put_bindings_around_body uacc ~body in
  place_constants uacc ~around:body lifted_constants_from_defining_expr

let create_switch uacc ~condition_dbg ~scrutinee ~arms =
  if Targetint_31_63.Map.cardinal arms < 1
  then
    ( RE.create_invalid Zero_switch_arms,
      UA.notify_added ~code_size:Code_size.invalid uacc )
  else
    let change_to_apply_cont action =
      let uacc =
        UA.add_free_names uacc (Apply_cont.free_names action)
        |> UA.notify_added ~code_size:(Code_size.apply_cont action)
      in
      (* The resulting [Debuginfo] on the [Apply_cont] will be arbitrarily
         chosen from amongst the [Debuginfo] values on the arms, but this seems
         fine. *)
      RE.create_apply_cont action, uacc
    in
    match Targetint_31_63.Map.get_singleton arms with
    | Some (_discriminant, action) -> change_to_apply_cont action
    | None -> (
      let actions =
        Apply_cont_expr.Set.of_list (Targetint_31_63.Map.data arms)
      in
      match Apply_cont_expr.Set.get_singleton actions with
      | Some action -> change_to_apply_cont action
      | None ->
        let switch = Switch.create ~condition_dbg ~scrutinee ~arms in
        let uacc =
          UA.add_free_names uacc (Switch.free_names switch)
          |> UA.notify_added ~code_size:(Code_size.switch switch)
        in
        RE.create_switch (UA.are_rebuilding_terms uacc) switch, uacc)

let rebuild_invalid uacc reason ~after_rebuild =
  after_rebuild (RE.create_invalid reason) uacc

type rewrite_use_ctx =
  | Apply_cont
  | Apply_expr of Simple.t list

type rewrite_use_result =
  | Apply_cont of Apply_cont.t
  | Expr of
      (apply_cont_to_expr:
         (Apply_cont.t -> RE.t * Cost_metrics.t * Name_occurrences.t) ->
      RE.t * Cost_metrics.t * Name_occurrences.t)

let no_rewrite apply_cont = Apply_cont apply_cont

let rewrite_use uacc rewrite ~ctx id apply_cont : rewrite_use_result =
  let args = Apply_cont.args apply_cont in
  let original_params' = Apply_cont_rewrite.original_params rewrite in
  let original_params = Bound_parameters.to_list original_params' in
  if List.compare_lengths args original_params <> 0
  then
    Misc.fatal_errorf
      "Arguments to this [Apply_cont]@ (%a)@ do not match@ [original_params] \
       (%a):@ %a"
      Apply_cont.print apply_cont Bound_parameters.print original_params'
      Simple.List.print args;
  let original_params_with_args = List.combine original_params args in
  let args =
    let used_params = Apply_cont_rewrite.used_params rewrite in
    List.filter_map
      (fun (original_param, arg) ->
        if BP.Set.mem original_param used_params then Some arg else None)
      original_params_with_args
  in
  let extra_args_list = Apply_cont_rewrite.extra_args rewrite id in
  let extra_args_rev, extra_lets, _ =
    List.fold_left
      (fun (extra_args_rev, extra_lets, required_by_other_extra_args)
           ( (arg : Continuation_extra_params_and_args.Extra_arg.t),
             (used : Apply_cont_rewrite.used) ) ->
        let extra_arg, extra_let, free_names =
          match arg with
          | Already_in_scope simple -> simple, [], Name_occurrences.empty
          | New_let_binding (temp, prim) ->
            let extra_let =
              ( Bound_var.create temp Name_mode.normal,
                Code_size.prim prim,
                Named.create_prim prim Debuginfo.none )
            in
            Simple.var temp, [extra_let], Flambda_primitive.free_names prim
          | New_let_binding_with_named_args (temp, gen_prim) ->
            let prim =
              match (ctx : rewrite_use_ctx) with
              | Apply_expr args -> gen_prim args
              | Apply_cont ->
                Misc.fatal_errorf
                  "Apply_cont rewrites should not need to name arguments, \
                   since they are already named."
            in
            let extra_let =
              ( Bound_var.create temp Name_mode.normal,
                Code_size.prim prim,
                Named.create_prim prim Debuginfo.none )
            in
            Simple.var temp, [extra_let], Flambda_primitive.free_names prim
        in
        let extra_args_rev =
          match used with
          | Used -> extra_arg :: extra_args_rev
          | Unused -> extra_args_rev
        in
        let required_let =
          match used with
          | Used -> true
          | Unused ->
            let defined_names = Simple.free_names extra_arg in
            Name_occurrences.inter_domain_is_non_empty defined_names
              required_by_other_extra_args
        in
        if required_let
        then
          ( extra_args_rev,
            extra_let @ extra_lets,
            Name_occurrences.union free_names required_by_other_extra_args )
        else extra_args_rev, extra_lets, required_by_other_extra_args)
      ([], [], Name_occurrences.empty)
      extra_args_list
  in
  let args = args @ List.rev extra_args_rev in
  let apply_cont = Apply_cont.update_args apply_cont ~args in
  match extra_lets with
  | [] -> Apply_cont apply_cont
  | _ :: _ ->
    let build_expr ~apply_cont_to_expr =
      let body, cost_metrics_of_body, free_names_of_body =
        apply_cont_to_expr apply_cont
      in
      RE.bind_no_simplification
        (UA.are_rebuilding_terms uacc)
        ~bindings:extra_lets ~body ~cost_metrics_of_body ~free_names_of_body
    in
    Expr build_expr

(* CR-someday mshinwell: The code of this function could maybe be improved. *)
let rewrite_exn_continuation rewrite id exn_cont =
  let exn_cont_arity = Exn_continuation.arity exn_cont in
  let original_params' = Apply_cont_rewrite.original_params rewrite in
  let original_params = Bound_parameters.to_list original_params' in
  let original_params_arity =
    Bound_parameters.arity_with_subkinds original_params'
  in
  if not
       (Flambda_arity.With_subkinds.equal exn_cont_arity original_params_arity)
  then
    Misc.fatal_errorf
      "Arity of exception continuation %a does not match@ [original_params] \
       (%a)"
      Exn_continuation.print exn_cont Bound_parameters.print original_params';
  assert (Flambda_arity.With_subkinds.cardinal exn_cont_arity >= 1);
  let pre_existing_extra_params_with_args =
    List.combine (List.tl original_params)
      (Exn_continuation.extra_args exn_cont)
  in
  let extra_args0 =
    let used_params = Apply_cont_rewrite.used_params rewrite in
    List.filter_map
      (fun (pre_existing_extra_param, arg) ->
        if BP.Set.mem pre_existing_extra_param used_params
        then Some arg
        else None)
      pre_existing_extra_params_with_args
  in
  let extra_args1 =
    let extra_args_list =
      List.filter_map
        (fun ( (arg : Continuation_extra_params_and_args.Extra_arg.t),
               (used : Apply_cont_rewrite.used) ) ->
          match used with
          | Used -> Some arg
          | Unused -> (
            match arg with
            | Already_in_scope _ -> None
            | New_let_binding _ | New_let_binding_with_named_args _ ->
              Misc.fatal_error "[New_let_binding] not expected here"))
        (Apply_cont_rewrite.extra_args rewrite id)
    in
    let used_extra_params =
      Apply_cont_rewrite.used_extra_params rewrite |> Bound_parameters.to_list
    in
    assert (List.compare_lengths used_extra_params extra_args_list = 0);
    List.map2
      (fun param (arg : Continuation_extra_params_and_args.Extra_arg.t) ->
        match arg with
        | Already_in_scope simple -> simple, BP.kind param
        | New_let_binding _ | New_let_binding_with_named_args _ ->
          Misc.fatal_error "[New_let_binding] not expected here")
      used_extra_params extra_args_list
  in
  let extra_args = extra_args0 @ extra_args1 in
  Exn_continuation.create
    ~exn_handler:(Exn_continuation.exn_handler exn_cont)
    ~extra_args

type add_wrapper_for_fixed_arity_continuation0_result =
  | This_continuation of Continuation.t
  | Apply_cont of Apply_cont.t
  | New_wrapper of
      Continuation.t
      * RE.Continuation_handler.t
      * Name_occurrences.t
      * Cost_metrics.t

type cont_or_apply_cont =
  | Continuation of Continuation.t
  | Apply_cont of Apply_cont.t

let add_wrapper_for_fixed_arity_continuation0 uacc cont_or_apply_cont ~use_id
    arity : add_wrapper_for_fixed_arity_continuation0_result =
  let uenv = UA.uenv uacc in
  let cont =
    match cont_or_apply_cont with
    | Continuation cont -> cont
    | Apply_cont apply_cont -> Apply_cont.continuation apply_cont
  in
  let original_cont = cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
  match UE.find_apply_cont_rewrite uenv original_cont with
  | None -> This_continuation cont
  | Some rewrite when Apply_cont_rewrite.does_nothing rewrite ->
    let arity = Flambda_arity.With_subkinds.to_arity arity in
    let arity_in_rewrite =
      Apply_cont_rewrite.original_params_arity rewrite
      |> Flambda_arity.With_subkinds.to_arity
    in
    if not (Flambda_arity.equal arity arity_in_rewrite)
    then
      Misc.fatal_errorf
        "Arity %a provided to fixed-arity-wrapper addition function does not \
         match arity %a in rewrite:@ %a"
        Flambda_arity.print arity Flambda_arity.print arity_in_rewrite
        Apply_cont_rewrite.print rewrite;
    This_continuation cont
  | Some rewrite -> (
    let new_wrapper params expr ~free_names ~cost_metrics =
      let new_cont = Continuation.create () in
      let new_handler =
        RE.Continuation_handler.create
          (UA.are_rebuilding_terms uacc)
          params ~handler:expr ~free_names_of_handler:free_names
          ~is_exn_handler:false
      in
      let free_names =
        ListLabels.fold_left (Bound_parameters.to_list params) ~init:free_names
          ~f:(fun free_names param ->
            Name_occurrences.remove_var free_names (Bound_parameter.var param))
      in
      New_wrapper (new_cont, new_handler, free_names, cost_metrics)
    in
    match cont_or_apply_cont with
    | Continuation cont -> (
      (* In this case, any generated [Apply_cont] will sit inside a wrapper that
         binds [kinded_params]. *)
      let params =
        List.map
          (fun _kind -> Variable.create "param")
          (Flambda_arity.With_subkinds.to_list arity)
      in
      let params =
        List.map2 BP.create params (Flambda_arity.With_subkinds.to_list arity)
      in
      let args = List.map BP.simple params in
      let params = Bound_parameters.create params in
      let apply_cont = Apply_cont.create cont ~args ~dbg:Debuginfo.none in
      let ctx = Apply_expr args in
      match rewrite_use uacc rewrite use_id ~ctx apply_cont with
      | Apply_cont apply_cont ->
        let cost_metrics =
          Cost_metrics.from_size (Code_size.apply_cont apply_cont)
        in
        new_wrapper params
          (RE.create_apply_cont apply_cont)
          ~free_names:(Apply_cont.free_names apply_cont)
          ~cost_metrics
      | Expr build_expr ->
        let expr, cost_metrics, free_names =
          build_expr ~apply_cont_to_expr:(fun apply_cont ->
              ( RE.create_apply_cont apply_cont,
                Cost_metrics.from_size (Code_size.apply_cont apply_cont),
                Apply_cont.free_names apply_cont ))
        in
        new_wrapper params expr ~free_names ~cost_metrics)
    | Apply_cont apply_cont -> (
      let apply_cont = Apply_cont.update_continuation apply_cont cont in
      match rewrite_use uacc rewrite ~ctx:Apply_cont use_id apply_cont with
      | Apply_cont apply_cont -> Apply_cont apply_cont
      | Expr build_expr ->
        let expr, cost_metrics, free_names =
          build_expr ~apply_cont_to_expr:(fun apply_cont ->
              ( RE.create_apply_cont apply_cont,
                Cost_metrics.from_size (Code_size.apply_cont apply_cont),
                Apply_cont.free_names apply_cont ))
        in
        new_wrapper Bound_parameters.empty expr ~free_names ~cost_metrics))

type add_wrapper_for_switch_arm_result =
  | Apply_cont of Apply_cont.t
  | New_wrapper of
      Continuation.t
      * RE.Continuation_handler.t
      * Name_occurrences.t
      * Cost_metrics.t

let add_wrapper_for_switch_arm uacc apply_cont ~use_id arity :
    add_wrapper_for_switch_arm_result =
  match
    add_wrapper_for_fixed_arity_continuation0 uacc (Apply_cont apply_cont)
      ~use_id arity
  with
  | This_continuation cont ->
    Apply_cont (Apply_cont.update_continuation apply_cont cont)
  | Apply_cont apply_cont -> Apply_cont apply_cont
  | New_wrapper (cont, wrapper, free_names_of_handler, cost_metrics) ->
    New_wrapper (cont, wrapper, free_names_of_handler, cost_metrics)

let add_wrapper_for_fixed_arity_continuation uacc cont ~use_id arity ~around =
  match
    add_wrapper_for_fixed_arity_continuation0 uacc (Continuation cont) ~use_id
      arity
  with
  | This_continuation cont -> around uacc cont
  | Apply_cont _ -> assert false
  | New_wrapper
      (new_cont, new_handler, free_names_of_handler, cost_metrics_of_handler) ->
    let body, uacc = around uacc new_cont in
    let free_names_of_body = UA.name_occurrences uacc in
    let expr =
      RE.create_non_recursive_let_cont
        (UA.are_rebuilding_terms uacc)
        new_cont new_handler ~body ~free_names_of_body
    in
    let free_names =
      Name_occurrences.union free_names_of_handler free_names_of_body
    in
    let free_names = Name_occurrences.remove_continuation free_names new_cont in
    let uacc =
      let added =
        Cost_metrics.increase_due_to_let_cont_non_recursive
          ~cost_metrics_of_handler
      in
      UA.with_name_occurrences uacc ~name_occurrences:free_names
      |> UA.add_cost_metrics added
    in
    expr, uacc

let add_wrapper_for_fixed_arity_apply uacc ~use_id arity apply =
  match Apply.continuation apply with
  | Never_returns ->
    let uacc =
      UA.add_free_names uacc (Apply.free_names apply)
      |> UA.notify_added ~code_size:(Code_size.apply apply)
    in
    RE.create_apply (UA.are_rebuilding_terms uacc) apply, uacc
  | Return cont ->
    add_wrapper_for_fixed_arity_continuation uacc cont ~use_id arity
      ~around:(fun uacc return_cont ->
        let exn_cont =
          UE.resolve_exn_continuation_aliases (UA.uenv uacc)
            (Apply.exn_continuation apply)
        in
        let apply =
          Apply.with_continuations apply (Return return_cont) exn_cont
        in
        let uacc =
          UA.add_free_names uacc (Apply.free_names apply)
          |> UA.notify_added ~code_size:(Code_size.apply apply)
        in
        RE.create_apply (UA.are_rebuilding_terms uacc) apply, uacc)
