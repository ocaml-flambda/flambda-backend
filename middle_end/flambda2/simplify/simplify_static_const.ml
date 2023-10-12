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

open! Flambda.Import
open! Simplify_import

let simplify_field_of_block dacc (field : Field_of_static_block.t) =
  match field with
  | Symbol sym -> field, T.alias_type_of K.value (Simple.symbol sym)
  | Tagged_immediate i -> field, T.this_tagged_immediate i
  | Dynamically_computed (var, dbg) ->
    let min_name_mode = Name_mode.normal in
    let ty = S.simplify_simple dacc (Simple.var var) ~min_name_mode in
    let simple = T.get_alias_exn ty in
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ ->
        (* CR mshinwell: It's safe to drop the coercion, but perhaps not
           ideal. *)
        Name.pattern_match name
          ~var:(fun var ->
            Field_of_static_block.Dynamically_computed (var, dbg), ty)
          ~symbol:(fun sym -> Field_of_static_block.Symbol sym, ty))
      ~const:(fun const ->
        match Reg_width_const.descr const with
        | Tagged_immediate imm -> Field_of_static_block.Tagged_immediate imm, ty
        | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
        | Naked_nativeint _ | Naked_vec128 _ ->
          (* CR mshinwell: This should be "invalid" and propagate up *)
          field, ty)

let simplify_or_variable dacc type_for_const (or_variable : _ Or_variable.t)
    kind =
  let denv = DA.denv dacc in
  match or_variable with
  | Const const -> or_variable, type_for_const const
  | Var (var, _dbg) ->
    (* CR mshinwell: There should be some kind of reification here *)
    or_variable, TE.find (DE.typing_env denv) (Name.var var) (Some kind)

let simplify_static_const_of_kind_value dacc (static_const : Static_const.t)
    ~result_sym : Rebuilt_static_const.t * DA.t =
  let bind_result_sym typ =
    DA.map_denv dacc ~f:(fun denv ->
        let denv = DE.define_symbol denv result_sym K.value in
        DE.add_equation_on_symbol denv result_sym typ)
  in
  match static_const with
  | Block (tag, is_mutable, fields) ->
    let fields_with_tys =
      List.map (fun field -> simplify_field_of_block dacc field) fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let ty =
      (* Same as Simplify_variadic_primitive.simplify_make_block_of_values *)
      let tag = Tag.Scannable.to_tag tag in
      let fields = field_tys in
      match is_mutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind:K.value ~fields
          Alloc_mode.For_types.heap
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.value ~fields
          Alloc_mode.For_types.heap
      | Mutable -> T.any_value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_block
        (DA.are_rebuilding_terms dacc)
        tag is_mutable ~fields,
      dacc )
  | Boxed_float or_var ->
    let or_var, ty =
      simplify_or_variable dacc
        (fun f -> T.this_boxed_float f Alloc_mode.For_types.heap)
        or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_float
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Boxed_int32 or_var ->
    let or_var, ty =
      simplify_or_variable dacc
        (fun f -> T.this_boxed_int32 f Alloc_mode.For_types.heap)
        or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_int32
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Boxed_int64 or_var ->
    let or_var, ty =
      simplify_or_variable dacc
        (fun f -> T.this_boxed_int64 f Alloc_mode.For_types.heap)
        or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_int64
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Boxed_nativeint or_var ->
    let or_var, ty =
      simplify_or_variable dacc
        (fun f -> T.this_boxed_nativeint f Alloc_mode.For_types.heap)
        or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_nativeint
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Boxed_vec128 or_var ->
    let or_var, ty =
      simplify_or_variable dacc
        (fun f -> T.this_boxed_vec128 f Alloc_mode.For_types.heap)
        or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_vec128
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Immutable_float_block fields ->
    let fields_with_tys =
      List.map
        (fun field ->
          simplify_or_variable dacc
            (fun f -> T.this_naked_float f)
            field K.naked_float)
        fields
    in
    let fields, _field_tys = List.split fields_with_tys in
    let dacc = bind_result_sym T.any_value in
    ( Rebuilt_static_const.create_immutable_float_block
        (DA.are_rebuilding_terms dacc)
        fields,
      dacc )
  | Immutable_float_array fields ->
    let fields_with_tys =
      List.map
        (fun field ->
          simplify_or_variable dacc
            (fun f -> T.this_naked_float f)
            field K.naked_float)
        fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let dacc =
      bind_result_sym
        (T.immutable_array ~element_kind:(Ok K.With_subkind.naked_float)
           ~fields:field_tys Alloc_mode.For_types.heap)
    in
    ( Rebuilt_static_const.create_immutable_float_array
        (DA.are_rebuilding_terms dacc)
        fields,
      dacc )
  | Immutable_int32_array fields ->
    let fields_with_tys =
      List.map
        (fun field ->
          simplify_or_variable dacc
            (fun f -> T.this_naked_int32 f)
            field K.naked_int32)
        fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let dacc =
      bind_result_sym
        (T.immutable_array ~element_kind:(Ok K.With_subkind.naked_int32)
           ~fields:field_tys Alloc_mode.For_types.heap)
    in
    ( Rebuilt_static_const.create_immutable_int32_array
        (DA.are_rebuilding_terms dacc)
        fields,
      dacc )
  | Immutable_int64_array fields ->
    let fields_with_tys =
      List.map
        (fun field ->
          simplify_or_variable dacc
            (fun f -> T.this_naked_int64 f)
            field K.naked_int64)
        fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let dacc =
      bind_result_sym
        (T.immutable_array ~element_kind:(Ok K.With_subkind.naked_int64)
           ~fields:field_tys Alloc_mode.For_types.heap)
    in
    ( Rebuilt_static_const.create_immutable_int64_array
        (DA.are_rebuilding_terms dacc)
        fields,
      dacc )
  | Immutable_nativeint_array fields ->
    let fields_with_tys =
      List.map
        (fun field ->
          simplify_or_variable dacc
            (fun f -> T.this_naked_nativeint f)
            field K.naked_nativeint)
        fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let dacc =
      bind_result_sym
        (T.immutable_array ~element_kind:(Ok K.With_subkind.naked_nativeint)
           ~fields:field_tys Alloc_mode.For_types.heap)
    in
    ( Rebuilt_static_const.create_immutable_nativeint_array
        (DA.are_rebuilding_terms dacc)
        fields,
      dacc )
  | Immutable_value_array fields ->
    let fields_with_tys =
      List.map (fun field -> simplify_field_of_block dacc field) fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let dacc =
      bind_result_sym
        (T.immutable_array ~element_kind:(Ok K.With_subkind.any_value)
           ~fields:field_tys Alloc_mode.For_types.heap)
    in
    ( Rebuilt_static_const.create_immutable_value_array
        (DA.are_rebuilding_terms dacc)
        fields,
      dacc )
  | Empty_array array_kind ->
    let dacc =
      bind_result_sym
        (T.array_of_length ~element_kind:Bottom
           ~length:(T.this_tagged_immediate Targetint_31_63.zero)
           Alloc_mode.For_types.heap)
    in
    ( Rebuilt_static_const.create_empty_array
        (DA.are_rebuilding_terms dacc)
        array_kind,
      dacc )
  | Mutable_string { initial_value } ->
    let str_ty = T.mutable_string ~size:(String.length initial_value) in
    let dacc = bind_result_sym str_ty in
    ( Rebuilt_static_const.create_mutable_string
        (DA.are_rebuilding_terms dacc)
        ~initial_value,
      dacc )
  | Immutable_string str ->
    let ty = T.this_immutable_string str in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_immutable_string
        (DA.are_rebuilding_terms dacc)
        str,
      dacc )
  | Set_of_closures _ ->
    Misc.fatal_errorf
      "[Set_of_closures] values cannot be bound by a [Block_like] binding:@ %a"
      SC.print static_const

let simplify_static_consts dacc (bound_static : Bound_static.t) static_consts
    ~simplify_function_body =
  let bound_static_list = Bound_static.to_list bound_static in
  let static_consts_list = Static_const_group.to_list static_consts in
  if List.compare_lengths bound_static_list static_consts_list <> 0
  then
    Misc.fatal_errorf "Bound symbols don't match static constants:@ %a@ =@ %a"
      Bound_static.print bound_static Static_const_group.print static_consts;
  (* The closure symbols are bound recursively across all of the definitions. We
     can start by giving these type [Unknown], since simplification of the
     constants that are neither pieces of code nor closures will not look at the
     structure of these closure symbols' definitions. *)
  let dacc =
    Static_const_group.match_against_bound_static static_consts bound_static
      ~init:dacc
      ~set_of_closures:(fun dacc ~closure_symbols _ ->
        Function_slot.Lmap.fold
          (fun _ closure_symbol dacc ->
            DA.with_denv dacc
              (DE.define_symbol (DA.denv dacc) closure_symbol K.value))
          closure_symbols dacc)
      ~code:(fun dacc _ _ -> dacc)
      ~deleted_code:(fun dacc _ -> dacc)
      ~block_like:(fun dacc _ _ -> dacc)
  in
  let all_code = Static_const_group.pieces_of_code static_consts in
  (* Next we simplify all the constants that are not closures. The ordering of
     the bindings is respected. This step also adds code into the environment.
     We can do that here because we're not simplifying the code (which may
     contain recursive references to symbols and/or code IDs being defined).
     This step will give values such as blocks various types involving aliases
     whose types in turn may currently be imprecise, for example if they
     reference a mutually-defined closure, but will be able to be refined
     further. *)
  (* CR vlaviron: With the exception of stubs, code bindings in the input term
     are never going to be simplified directly. Instead, when a closure that
     binds them is encountered, a specialised version of the code is created,
     simplified, and bound to a new code ID. But as a consequence, we never
     traverse the code and in particular we do not compute slot offset
     constraints for the body. In the common case, a code ID is only used in a
     single set of closures and all occurrences of the old code ID will be
     replaced by the new code ID computed while simplifying the closure. The old
     code binding will then be deleted, and will not cause problems.

     However, there are some hypothetical cases where the old code ID could end
     up in the result term. The most likely case is if a code ID appears in more
     than one set of closures, then it will get two distinct specialised
     versions, and the code age relation will keep the old code ID in case a
     join needs to be performed between the specialised versions. Another
     potential case is if a direct application of this code ID exists somewhere,
     and for some reason the type of the closure is either not available or does
     not have a more precise code ID.

     I suspect we will eventually need to deal with this in a more principled
     way (maybe by making offset constraints part of the required fields to
     create code, similar to the free names), but for now we're relying on the
     fact that Closure_conversion never produces such examples, and Simplify
     only has a single round. *)
  let bound_static', static_consts', dacc =
    Static_const_group.match_against_bound_static static_consts bound_static
      ~init:([], [], dacc)
      ~code:(fun (bound_static, static_consts, dacc) code_id code ->
        let code, static_const, dacc =
          if Code.stub code
          then
            let dacc, prior_lifted_constants =
              DA.get_and_clear_lifted_constants dacc
            in
            let static_const, dacc_after_function =
              Simplify_set_of_closures.simplify_stub_function dacc code
                ~all_code ~simplify_function_body
            in
            let dacc_after_function =
              DA.add_to_lifted_constant_accumulator dacc_after_function
                prior_lifted_constants
            in
            match Rebuilt_static_const.to_const static_const with
            | None ->
              (* Not rebuilding terms: return the original code *)
              code, Rebuilt_static_const.create_code' code, dacc
            | Some static_const_or_code -> (
              match Static_const_or_code.to_code static_const_or_code with
              | None -> assert false
              | Some code -> code, static_const, dacc_after_function)
          else code, Rebuilt_static_const.create_code' code, dacc
        in
        let dacc =
          DA.map_denv dacc ~f:(fun denv -> DE.define_code denv ~code_id ~code)
        in
        ( Bound_static.Pattern.code code_id :: bound_static,
          static_const :: static_consts,
          dacc ))
      ~deleted_code:(fun acc _code_id -> acc)
      ~set_of_closures:(fun acc ~closure_symbols:_ _ -> acc)
      ~block_like:
        (fun (bound_static, static_consts, dacc) symbol static_const ->
        let static_const, dacc =
          simplify_static_const_of_kind_value dacc static_const
            ~result_sym:symbol
        in
        ( Bound_static.Pattern.block_like symbol :: bound_static,
          static_const :: static_consts,
          dacc ))
  in
  let bound_static' = Bound_static.create bound_static' in
  let static_consts' = Rebuilt_static_const.Group.create static_consts' in
  (* We now collect together all of the closures, from all of the sets being
     defined, and simplify them together. It's important to do this step of
     simplification at the end to maximise the information available, since this
     can be highly beneficial to simplifying [Code] (which will be done as part
     of simplifying the closures). *)
  let closure_bound_names_all_sets, all_sets_of_closures_and_symbols =
    Static_const_group.match_against_bound_static static_consts bound_static
      ~init:([], [])
      ~code:(fun acc _ _ -> acc)
      ~deleted_code:(fun acc _ -> acc)
      ~block_like:(fun acc _ _ -> acc)
      ~set_of_closures:
        (fun (closure_bound_names_all_sets, sets_of_closures) ~closure_symbols
             set_of_closures ->
        let closure_bound_names =
          Function_slot.Lmap.fold
            (fun function_slot symbol closure_bound_names_all_sets ->
              Function_slot.Map.add function_slot
                (Bound_name.create_symbol symbol)
                closure_bound_names_all_sets)
            closure_symbols Function_slot.Map.empty
        in
        ( closure_bound_names :: closure_bound_names_all_sets,
          (closure_symbols, set_of_closures) :: sets_of_closures ))
  in
  let bound_static'', static_consts'', dacc =
    Simplify_set_of_closures.simplify_lifted_sets_of_closures dacc
      ~all_sets_of_closures_and_symbols ~closure_bound_names_all_sets
      ~simplify_function_body
  in
  (* The ordering of these lists doesn't matter as they will go through
     [Sort_lifted_constants] before the terms are constructed. *)
  ( Bound_static.concat bound_static' bound_static'',
    Rebuilt_static_const.Group.concat static_consts' static_consts'',
    dacc )
