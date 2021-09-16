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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import
module Field_of_block = Static_const.Field_of_block

(* CR-someday mshinwell: Finish improved simplification using types *)

let simplify_field_of_block dacc (field : Field_of_block.t) =
  match field with
  | Symbol sym -> field, T.alias_type_of K.value (Simple.symbol sym)
  | Tagged_immediate i -> field, T.this_tagged_immediate i
  | Dynamically_computed var ->
    let min_name_mode = Name_mode.normal in
    let ty = S.simplify_simple dacc (Simple.var var) ~min_name_mode in
    let simple = T.get_alias_exn ty in
    Simple.pattern_match simple
      ~name:(fun name ~coercion ->
        (* CR lmaurer: This will break if you try and put a coerced thing in a
           block *)
        assert (Coercion.is_id coercion);
        Name.pattern_match name
          ~var:(fun var -> Field_of_block.Dynamically_computed var, ty)
          ~symbol:(fun sym -> Field_of_block.Symbol sym, ty))
      ~const:(fun const ->
        match Reg_width_const.descr const with
        | Tagged_immediate imm -> Field_of_block.Tagged_immediate imm, ty
        | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
        | Naked_nativeint _ ->
          (* CR mshinwell: This should be "invalid" and propagate up *)
          field, ty)

let simplify_or_variable dacc type_for_const (or_variable : _ Or_variable.t)
    kind =
  let denv = DA.denv dacc in
  match or_variable with
  | Const const -> or_variable, type_for_const const
  | Var var ->
    (* CR mshinwell: This needs to check the type of the variable according to
       the various cases below. *)
    (* CR mshinwell: This should be calling [simplify_simple] *)
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
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.value ~fields
      | Mutable -> T.any_value ()
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_block
        (DA.are_rebuilding_terms dacc)
        tag is_mutable ~fields,
      dacc )
  (* CR mshinwell: Need to reify to change Equals types into new terms *)
  | Boxed_float or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_float f) or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_float
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Boxed_int32 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int32 f) or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_int32
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Boxed_int64 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int64 f) or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_int64
        (DA.are_rebuilding_terms dacc)
        or_var,
      dacc )
  | Boxed_nativeint or_var ->
    let or_var, ty =
      simplify_or_variable dacc
        (fun f -> T.this_boxed_nativeint f)
        or_var K.value
    in
    let dacc = bind_result_sym ty in
    ( Rebuilt_static_const.create_boxed_nativeint
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
    let dacc = bind_result_sym (T.any_value ()) in
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
    let fields, _field_tys = List.split fields_with_tys in
    let dacc = bind_result_sym (T.any_value ()) in
    ( Rebuilt_static_const.create_immutable_float_array
        (DA.are_rebuilding_terms dacc)
        fields,
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
  | Code _ | Set_of_closures _ ->
    Misc.fatal_errorf
      "[Code] and [Set_of_closures] cannot be bound by a [Block_like] \
       binding:@ %a"
      SC.print static_const

let simplify_static_consts dacc (bound_symbols : Bound_symbols.t) static_consts
    ~simplify_toplevel =
  let bound_symbols_list = Bound_symbols.to_list bound_symbols in
  let static_consts_list = Static_const.Group.to_list static_consts in
  if List.compare_lengths bound_symbols_list static_consts_list <> 0
  then
    Misc.fatal_errorf "Bound symbols don't match static constants:@ %a@ =@ %a"
      Bound_symbols.print bound_symbols Static_const.Group.print static_consts;
  (* The closure symbols are bound recursively across all of the definitions. We
     can start by giving these type [Unknown], since simplification of the
     constants that are neither pieces of code nor closures will not look at the
     structure of these closure symbols' definitions. *)
  let dacc =
    Static_const.Group.match_against_bound_symbols static_consts bound_symbols
      ~init:dacc
      ~set_of_closures:(fun dacc ~closure_symbols _ ->
        Closure_id.Lmap.fold
          (fun _ closure_symbol dacc ->
            DA.with_denv dacc
              (DE.define_symbol (DA.denv dacc) closure_symbol K.value))
          closure_symbols dacc)
      ~code:(fun dacc _ _ -> dacc)
      ~block_like:(fun dacc _ _ -> dacc)
  in
  (* Next we simplify all the constants that are not closures. The ordering of
     the bindings is respected. This step also adds code into the environment.
     We can do that here because we're not simplifying the code (which may
     contain recursive references to symbols and/or code IDs being defined). *)
  let bound_symbols', static_consts', dacc =
    Static_const.Group.match_against_bound_symbols static_consts bound_symbols
      ~init:([], [], dacc)
      ~code:(fun (bound_symbols, static_consts, dacc) code_id code ->
        let dacc =
          match Code.params_and_body code with
          | Deleted -> dacc
          | Present _ ->
            DA.map_denv dacc ~f:(fun denv -> DE.define_code denv ~code_id ~code)
        in
        let static_const = Rebuilt_static_const.create_code' code in
        ( Bound_symbols.Pattern.code code_id :: bound_symbols,
          static_const :: static_consts,
          dacc ))
      ~set_of_closures:(fun acc ~closure_symbols:_ _ -> acc)
      ~block_like:
        (fun (bound_symbols, static_consts, dacc) symbol static_const ->
        let static_const, dacc =
          simplify_static_const_of_kind_value dacc static_const
            ~result_sym:symbol
        in
        ( Bound_symbols.Pattern.block_like symbol :: bound_symbols,
          static_const :: static_consts,
          dacc ))
  in
  let bound_symbols' = Bound_symbols.create bound_symbols' in
  let static_consts' = Rebuilt_static_const.Group.create static_consts' in
  (* We now collect together all of the closures, from all of the sets being
     defined, and simplify them together. *)
  let closure_bound_names_all_sets, all_sets_of_closures_and_symbols =
    Static_const.Group.match_against_bound_symbols static_consts bound_symbols
      ~init:([], [])
      ~code:(fun acc _ _ -> acc)
      ~block_like:(fun acc _ _ -> acc)
      ~set_of_closures:
        (fun (closure_bound_names_all_sets, sets_of_closures) ~closure_symbols
             set_of_closures ->
        let closure_bound_names =
          Closure_id.Lmap.fold
            (fun closure_id symbol closure_bound_names_all_sets ->
              Closure_id.Map.add closure_id
                (Bound_name.symbol symbol)
                closure_bound_names_all_sets)
            closure_symbols Closure_id.Map.empty
        in
        ( closure_bound_names :: closure_bound_names_all_sets,
          (closure_symbols, set_of_closures) :: sets_of_closures ))
  in
  let bound_symbols'', static_consts'', dacc =
    Simplify_set_of_closures.simplify_lifted_sets_of_closures dacc
      ~all_sets_of_closures_and_symbols ~closure_bound_names_all_sets
      ~simplify_toplevel
  in
  (* The ordering of these lists doesn't matter as they will go through
     [Sort_lifted_constants] before the terms are constructed. *)
  ( Bound_symbols.concat bound_symbols' bound_symbols'',
    Rebuilt_static_const.Group.concat static_consts' static_consts'',
    dacc )
