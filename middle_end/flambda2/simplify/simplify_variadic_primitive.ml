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

let simplify_make_block ~original_prim ~field_kind tag ~shape
    ~(mutable_or_immutable : Mutability.t) alloc_mode dacc ~original_term _dbg
    ~args_with_tys ~result_var =
  let args, _arg_tys = List.split args_with_tys in
  if List.compare_lengths shape args <> 0
  then
    Misc.fatal_errorf
      "Shape in [Make_block] of different length from argument list:@ %a"
      Named.print original_term;
  let result =
    let typing_env = DA.typing_env dacc in
    List.fold_left2
      (fun env_extension arg arg_kind : _ Or_bottom.t ->
        let open Or_bottom.Let_syntax in
        let<* env_extension = env_extension in
        Simple.pattern_match' arg
          ~var:(fun _ ~coercion:_ : _ Or_bottom.t ->
            let<* _ty, env_extension' =
              T.meet typing_env
                (T.alias_type_of (K.With_subkind.kind arg_kind) arg)
                (T.unknown_with_subkind arg_kind)
            in
            let<+ env_extension =
              T.Typing_env_extension.meet typing_env env_extension
                env_extension'
            in
            env_extension)
          ~const:(fun _ : _ Or_bottom.t -> Ok env_extension)
          ~symbol:(fun _ ~coercion:_ : _ Or_bottom.t -> Ok env_extension))
      (Or_bottom.Ok TEE.empty) args shape
  in
  match result with
  | Bottom -> SPR.create_invalid dacc
  | Ok env_extension ->
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
          DE.map_typing_env denv ~f:(fun typing_env ->
              TE.add_env_extension typing_env env_extension))
    in
    let ty =
      let fields =
        List.map2
          (fun arg kind_with_subkind ->
            T.alias_type_of (K.With_subkind.kind kind_with_subkind) arg)
          args shape
      in
      let alloc_mode = Alloc_mode.For_allocations.as_type alloc_mode in
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind alloc_mode ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind alloc_mode ~fields
      | Mutable -> T.mutable_block alloc_mode
    in
    let dacc = DA.add_variable dacc result_var ty in
    let dacc =
      match mutable_or_immutable with
      | Immutable_unique | Mutable -> dacc
      | Immutable -> (
        match P.Eligible_for_cse.create original_prim with
        | None -> dacc
        | Some prim ->
          DA.map_denv dacc ~f:(fun denv ->
              DE.add_cse denv prim
                ~bound_to:(Simple.var (Bound_var.var result_var))))
    in
    SPR.create original_term ~try_reify:true dacc

let simplify_make_block_of_floats ~original_prim ~mutable_or_immutable
    alloc_mode dacc ~original_term dbg ~args_with_tys ~result_var =
  let shape = List.map (fun _ -> K.With_subkind.naked_float) args_with_tys in
  simplify_make_block ~original_prim ~field_kind:K.naked_float
    Tag.double_array_tag ~shape ~mutable_or_immutable alloc_mode dacc
    ~original_term dbg ~args_with_tys ~result_var

let simplify_make_array (array_kind : P.Array_kind.t)
    ~(mutable_or_immutable : Mutability.t) alloc_mode dacc ~original_term dbg
    ~args_with_tys ~result_var =
  let args, tys = List.split args_with_tys in
  let length =
    match Targetint_31_63.of_int_option (List.length args) with
    | Some ti -> T.this_tagged_immediate ti
    | None -> T.unknown K.value
  in
  let element_kind =
    (* Remember that the element subkinds cannot in general be deduced from the
       types of the array members, it must be obtained from the array kind
       annotations that came via [Lambda]. *)
    P.Array_kind.element_kind array_kind
  in
  let initial_element_type = T.unknown_with_subkind element_kind in
  let typing_env = DA.typing_env dacc in
  let env_extension =
    List.fold_left
      (fun env_extension element_type ->
        let open Or_bottom.Let_syntax in
        let<* env_extension = env_extension in
        let<* _, env_extension' =
          T.meet typing_env initial_element_type element_type
        in
        TEE.meet typing_env env_extension env_extension')
      (Or_bottom.Ok TEE.empty) tys
  in
  match env_extension with
  | Bottom -> SPR.create_invalid dacc
  | Ok env_extension ->
    let ty =
      let alloc_mode = Alloc_mode.For_allocations.as_type alloc_mode in
      match mutable_or_immutable with
      | Mutable ->
        T.mutable_array ~element_kind:(Ok element_kind) ~length alloc_mode
      | Immutable ->
        T.immutable_array ~element_kind:(Ok element_kind) ~fields:tys alloc_mode
      | Immutable_unique ->
        Misc.fatal_errorf "Immutable_unique is not expected for arrays:@ %a"
          Named.print original_term
    in
    let named =
      Named.create_prim
        (Variadic
           (Make_array (array_kind, mutable_or_immutable, alloc_mode), args))
        dbg
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
          DE.add_variable_and_extend_typing_environment denv result_var ty
            env_extension)
    in
    SPR.create named ~try_reify:true dacc

(* XXX layouts: Don't really know what I'm doing here.  In particular for [ty]
   I've picked [T.any_block] because exisiting more specific types for blocks
   all want the fields to have the same kind.  I don't eve know of [any_block]
   is correct - perhaps it's meant to be normal blocks below no scan tag.
*)
let simplify_make_abstract_block ~original_prim ~kind
      ~(mutable_or_immutable : Mutability.t) _alloc_mode
      dacc ~original_term _dbg ~args_with_tys ~result_var =
  let args, _arg_tys = List.split args_with_tys in
  if P.Abstract_block_kind.length kind <> List.length args
  then
    Misc.fatal_errorf
      "Shape in [Make_abstract_block] of different length from argument list:@ %a"
      Named.print original_term;
  let (_, result) =
    let typing_env = DA.typing_env dacc in
    List.fold_left
      (fun (idx, env_extension) arg : (int * (_ Or_bottom.t)) ->
        (idx+1,
         let open Or_bottom.Let_syntax in
         let<* env_extension = env_extension in
         let arg_kind = P.Abstract_block_kind.element_kind idx kind in
          Simple.pattern_match' arg
            ~var:(fun _ ~coercion:_ : _ Or_bottom.t ->
              let<* _ty, env_extension' =
                T.meet typing_env
                  (T.alias_type_of arg_kind arg)
                  (T.unknown arg_kind)
              in
              let<+ env_extension =
                T.Typing_env_extension.meet typing_env env_extension
                  env_extension'
              in
              env_extension)
            ~const:(fun _ : _ Or_bottom.t -> Ok env_extension)
            ~symbol:(fun _ ~coercion:_ : _ Or_bottom.t -> Ok env_extension)))
      (0, Or_bottom.Ok TEE.empty) args
  in
  match result with
  | Bottom -> SPR.create_invalid dacc
  | Ok env_extension ->
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
          DE.map_typing_env denv ~f:(fun typing_env ->
              TE.add_env_extension typing_env env_extension))
    in
    let ty = T.any_block in
    let dacc = DA.add_variable dacc result_var ty in
    let dacc =
      match mutable_or_immutable with
      | Immutable_unique | Mutable -> dacc
      | Immutable -> (
        match P.Eligible_for_cse.create original_prim with
        | None -> dacc
        | Some prim ->
          DA.map_denv dacc ~f:(fun denv ->
              DE.add_cse denv prim
                ~bound_to:(Simple.var (Bound_var.var result_var))))
    in
    SPR.create original_term ~try_reify:true dacc

let simplify_variadic_primitive dacc original_prim (prim : P.variadic_primitive)
    ~args_with_tys dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Make_block (Values (tag, shape), mutable_or_immutable, alloc_mode) ->
      let tag = Tag.Scannable.to_tag tag in
      simplify_make_block ~original_prim ~field_kind:K.value tag ~shape
        ~mutable_or_immutable alloc_mode
    | Make_block (Naked_floats, mutable_or_immutable, alloc_mode) ->
      simplify_make_block_of_floats ~original_prim ~mutable_or_immutable
        alloc_mode
    | Make_array (array_kind, mutable_or_immutable, alloc_mode) ->
      simplify_make_array array_kind ~mutable_or_immutable alloc_mode
    | Make_abstract_block (kind, mutable_or_immutable, alloc_mode) ->
      simplify_make_abstract_block ~original_prim ~kind ~mutable_or_immutable
        alloc_mode
  in
  simplifier dacc ~original_term dbg ~args_with_tys ~result_var
