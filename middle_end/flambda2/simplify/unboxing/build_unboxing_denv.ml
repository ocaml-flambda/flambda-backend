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

open! Simplify_import
module U = Unboxing_types

let add_equation_on_var denv var shape =
  let kind = T.kind shape in
  let var_type = T.alias_type_of kind (Simple.var var) in
  match T.meet (DE.typing_env denv) var_type shape with
  | Ok (_ty, env_extension) ->
    DE.map_typing_env denv ~f:(fun tenv ->
        TE.add_env_extension tenv env_extension)
  | Bottom ->
    Misc.fatal_errorf "Meet failed whereas prove and meet previously succeeded"

let denv_of_number_decision naked_kind shape param_var naked_var denv : DE.t =
  let naked_name = VB.create naked_var Name_mode.normal in
  let denv = DE.define_variable denv naked_name naked_kind in
  add_equation_on_var denv param_var shape

let rec denv_of_decision denv ~param_var (decision : U.decision) : DE.t =
  match decision with
  | Do_not_unbox _ -> denv
  | Unbox (Unique_tag_and_size { tag; shape; fields }) ->
    let denv =
      Misc.Stdlib.List.fold_lefti
        (fun index denv ({ epa = { param = var; _ }; _ } : U.field_decision) ->
          let v = VB.create var Name_mode.normal in
          DE.define_variable denv v (K.Block_shape.element_kind shape index))
        denv fields
    in
    let type_of_var index (field : U.field_decision) =
      T.alias_type_of
        (K.Block_shape.element_kind shape index)
        (Simple.var field.epa.param)
    in
    let field_types = List.mapi type_of_var fields in
    let shape =
      T.immutable_block ~is_unique:false tag ~shape ~fields:field_types
        (Alloc_mode.For_types.unknown ())
    in
    let denv = add_equation_on_var denv param_var shape in
    List.fold_left
      (fun denv (field : U.field_decision) ->
        denv_of_decision denv ~param_var:field.epa.param field.decision)
      denv fields
  | Unbox (Closure_single_entry { function_slot; vars_within_closure }) ->
    let denv =
      Value_slot.Map.fold
        (fun _ ({ epa = { param = var; _ }; kind; _ } : U.field_decision) denv ->
          let v = VB.create var Name_mode.normal in
          DE.define_variable denv v (K.With_subkind.kind kind))
        vars_within_closure denv
    in
    let map =
      Value_slot.Map.map
        (fun ({ epa = { param = var; _ }; kind; _ } : U.field_decision) ->
          var, kind)
        vars_within_closure
    in
    let shape =
      T.closure_with_at_least_these_value_slots
        ~this_function_slot:function_slot map
    in
    let denv = add_equation_on_var denv param_var shape in
    Value_slot.Map.fold
      (fun _ (field : U.field_decision) denv ->
        denv_of_decision denv ~param_var:field.epa.param field.decision)
      vars_within_closure denv
  | Unbox (Variant { tag; const_ctors; fields_by_tag }) ->
    (* Adapt the denv for the tag *)
    let tag_v = VB.create tag.param Name_mode.normal in
    let denv = DE.define_variable denv tag_v K.naked_immediate in
    let denv =
      DE.add_equation_on_variable denv tag.param
        (T.get_tag_for_block ~block:(Simple.var param_var))
    in
    let get_tag_prim =
      P.Eligible_for_cse.create_get_tag ~block:(Name.var param_var)
    in
    let denv = DE.add_cse denv get_tag_prim ~bound_to:(Simple.var tag.param) in
    (* Same thing for is_int *)
    let denv =
      match const_ctors with
      | Zero -> denv
      | At_least_one { is_int; _ } ->
        let is_int_v = VB.create is_int.param Name_mode.normal in
        let denv = DE.define_variable denv is_int_v K.naked_immediate in
        let denv =
          DE.add_equation_on_variable denv is_int.param
            (T.is_int_for_scrutinee ~scrutinee:(Simple.var param_var))
        in
        let is_int_prim =
          P.Eligible_for_cse.create_is_int ~variant_only:true
            ~immediate_or_block:(Name.var param_var)
        in
        let denv =
          DE.add_cse denv is_int_prim ~bound_to:(Simple.var is_int.param)
        in
        denv
    in
    let denv, const_ctors =
      match const_ctors with
      | Zero -> denv, T.bottom K.naked_immediate
      | At_least_one { ctor = Do_not_unbox _; _ } ->
        denv, T.unknown K.naked_immediate
      | At_least_one { ctor = Unbox (Number (Naked_immediate, ctor_epa)); _ } ->
        let v = VB.create ctor_epa.param Name_mode.normal in
        let denv = DE.define_variable denv v K.naked_immediate in
        let ty =
          T.alias_type_of K.naked_immediate (Simple.var ctor_epa.param)
        in
        denv, ty
      | At_least_one
          { ctor =
              Unbox
                ( Unique_tag_and_size _ | Variant _ | Closure_single_entry _
                | Number
                    ( ( Naked_float | Naked_float32 | Naked_int32 | Naked_int64
                      | Naked_nativeint | Naked_vec128 ),
                      _ ) );
            is_int = _
          } ->
        Misc.fatal_errorf
          "Variant constant constructor unboxed with a kind other than \
           naked_immediate."
    in
    let denv =
      Tag.Scannable.Map.fold
        (fun _ (shape, block_fields) denv ->
          Misc.Stdlib.List.fold_lefti
            (fun index denv ({ epa = { param = var; _ }; _ } : U.field_decision) ->
              let v = VB.create var Name_mode.normal in
              DE.define_variable denv v (K.Block_shape.element_kind shape index))
            denv block_fields)
        fields_by_tag denv
    in
    let non_const_ctors =
      Tag.Scannable.Map.map
        (fun (shape, block_fields) ->
          ( shape,
            List.map
              (fun (field : U.field_decision) ->
                T.alias_type_of K.value (Simple.var field.epa.param))
              block_fields ))
        fields_by_tag
    in
    let shape =
      T.variant ~const_ctors ~non_const_ctors (Alloc_mode.For_types.unknown ())
    in
    let denv = add_equation_on_var denv param_var shape in
    (* Recurse on the fields *)
    Tag.Scannable.Map.fold
      (fun _ (_shape, block_fields) denv ->
        List.fold_left
          (fun denv (field : U.field_decision) ->
            denv_of_decision denv ~param_var:field.epa.param field.decision)
          denv block_fields)
      fields_by_tag denv
  | Unbox (Number (Naked_immediate, { param = naked_immediate; args = _ })) ->
    let shape = T.tagged_immediate_alias_to ~naked_immediate in
    denv_of_number_decision K.naked_immediate shape param_var naked_immediate
      denv
  | Unbox (Number (Naked_float32, { param = naked_float32; args = _ })) ->
    let shape =
      T.boxed_float32_alias_to ~naked_float32 (Alloc_mode.For_types.unknown ())
    in
    denv_of_number_decision K.naked_float32 shape param_var naked_float32 denv
  | Unbox (Number (Naked_float, { param = naked_float; args = _ })) ->
    let shape =
      T.boxed_float_alias_to ~naked_float (Alloc_mode.For_types.unknown ())
    in
    denv_of_number_decision K.naked_float shape param_var naked_float denv
  | Unbox (Number (Naked_int32, { param = naked_int32; args = _ })) ->
    let shape =
      T.boxed_int32_alias_to ~naked_int32 (Alloc_mode.For_types.unknown ())
    in
    denv_of_number_decision K.naked_int32 shape param_var naked_int32 denv
  | Unbox (Number (Naked_int64, { param = naked_int64; args = _ })) ->
    let shape =
      T.boxed_int64_alias_to ~naked_int64 (Alloc_mode.For_types.unknown ())
    in
    denv_of_number_decision K.naked_int64 shape param_var naked_int64 denv
  | Unbox (Number (Naked_nativeint, { param = naked_nativeint; args = _ })) ->
    let shape =
      T.boxed_nativeint_alias_to ~naked_nativeint
        (Alloc_mode.For_types.unknown ())
    in
    denv_of_number_decision K.naked_nativeint shape param_var naked_nativeint
      denv
  | Unbox (Number (Naked_vec128, { param = naked_vec128; args = _ })) ->
    let shape =
      T.boxed_vec128_alias_to ~naked_vec128 (Alloc_mode.For_types.unknown ())
    in
    denv_of_number_decision K.naked_vec128 shape param_var naked_vec128 denv
