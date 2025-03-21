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

let simplify_make_block ~original_prim ~(block_kind : P.Block_kind.t)
    ~(mutable_or_immutable : Mutability.t) alloc_mode dacc ~original_term _dbg
    ~args_with_tys ~result_var =
  let typing_env = DA.typing_env dacc in
  let typing_env : _ Or_bottom.t =
    match block_kind with
    | Naked_floats | Mixed _ ->
      (* No useful subkind information *)
      Ok typing_env
    | Values (_tag, field_kinds) ->
      if List.compare_lengths args_with_tys field_kinds <> 0
      then
        Misc.fatal_errorf
          "Shape in [Make_block] of different length from argument list:@ %a"
          Named.print original_term;
      List.fold_left2
        (fun typing_env arg_kind (arg, _arg_ty) : _ Or_bottom.t ->
          let open Or_bottom.Let_syntax in
          let<* typing_env = typing_env in
          Simple.pattern_match' arg
            ~var:(fun _ ~coercion:_ : _ Or_bottom.t ->
              let<+ _ty, typing_env =
                T.meet typing_env
                  (T.alias_type_of (K.With_subkind.kind arg_kind) arg)
                  (T.unknown_with_subkind arg_kind)
              in
              typing_env)
            ~const:(fun _ : _ Or_bottom.t -> Ok typing_env)
            ~symbol:(fun _ ~coercion:_ : _ Or_bottom.t -> Ok typing_env))
        (Or_bottom.Ok typing_env) field_kinds args_with_tys
  in
  match typing_env with
  | Bottom -> SPR.create_invalid dacc
  | Ok typing_env ->
    let dacc =
      DA.map_denv dacc ~f:(fun denv -> DE.with_typing_env denv typing_env)
    in
    let ty =
      let fields = List.map snd args_with_tys in
      let alloc_mode = Alloc_mode.For_allocations.as_type alloc_mode in
      let tag, shape = P.Block_kind.to_shape block_kind in
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~shape alloc_mode ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~shape alloc_mode ~fields
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

let simplify_make_array (array_kind : P.Array_kind.t)
    ~(mutable_or_immutable : Mutability.t) alloc_mode dacc ~original_term dbg
    ~args_with_tys ~result_var =
  let args, tys = List.split args_with_tys in
  let length =
    match Targetint_31_63.of_int_option (List.length args) with
    | Some ti -> T.this_tagged_immediate ti
    | None -> T.unknown K.value
  in
  let element_kinds = P.Array_kind.element_kinds array_kind in
  let element_kind =
    (* CR mshinwell: support unboxed product arrays in the type system *)
    (* Remember that the element subkinds cannot in general be deduced from the
       types of the array members, it must be obtained from the array kind
       annotations that came via [Lambda]. *)
    match P.Array_kind.element_kinds array_kind with
    | [kind] -> Some kind
    | _ :: _ -> None
    | [] ->
      Misc.fatal_errorf
        "Empty list of element kinds given for array kind:@ %a@ %a"
        P.Array_kind.print array_kind Debuginfo.print_compact dbg
  in
  let num_element_kinds = List.length element_kinds in
  if List.length args mod num_element_kinds <> 0
  then
    Misc.fatal_errorf
      "Array length not a multiple of the length of the unboxed product kind \
       list:@ array_kind=%a@ num args=%d@ %a"
      P.Array_kind.print array_kind (List.length args) Named.print original_term;
  let env_extension =
    let typing_env = DA.typing_env dacc in
    match element_kind with
    | None -> Or_bottom.Ok typing_env
    | Some element_kind ->
      let initial_element_type = T.unknown_with_subkind element_kind in
      List.fold_left
        (fun typing_env element_type ->
          let open Or_bottom.Let_syntax in
          let<* typing_env = typing_env in
          let<+ _, typing_env =
            T.meet typing_env initial_element_type element_type
          in
          typing_env)
        (Or_bottom.Ok typing_env) tys
  in
  match env_extension with
  | Bottom -> SPR.create_invalid dacc
  | Ok typing_env ->
    let ty =
      let alloc_mode = Alloc_mode.For_allocations.as_type alloc_mode in
      let element_kind : _ Or_unknown_or_bottom.t =
        match element_kind with
        | None -> (* Array of unboxed products *) Unknown
        | Some element_kind -> Ok element_kind
      in
      match mutable_or_immutable with
      | Mutable -> T.mutable_array ~element_kind ~length alloc_mode
      | Immutable -> T.immutable_array ~element_kind ~fields:tys alloc_mode
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
          let denv = DE.with_typing_env denv typing_env in
          DE.add_variable denv result_var ty)
    in
    SPR.create named ~try_reify:true dacc

let simplify_begin_region dacc ~original_term _dbg ~args_with_tys:_ ~result_var
    =
  let ty = T.any_region in
  let dacc = DA.add_variable dacc result_var ty in
  Simplify_primitive_result.create original_term ~try_reify:false dacc

let simplify_variadic_primitive dacc original_prim (prim : P.variadic_primitive)
    ~args_with_tys dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Begin_region { ghost = _ } | Begin_try_region { ghost = _ } ->
      simplify_begin_region
    | Make_block (block_kind, mutable_or_immutable, alloc_mode) ->
      simplify_make_block ~original_prim ~block_kind ~mutable_or_immutable
        alloc_mode
    | Make_array (array_kind, mutable_or_immutable, alloc_mode) ->
      simplify_make_array array_kind ~mutable_or_immutable alloc_mode
  in
  simplifier dacc ~original_term dbg ~args_with_tys ~result_var
