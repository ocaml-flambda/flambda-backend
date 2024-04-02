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

(* Allow [simplify_make_block] to fold over the fields, whether they are backed
   by a list (for non-mixed blocks) or an array (for mixed blocks). *)
module Block_shape = struct
  type t =
    | Not_mixed of
        { field_kind : K.t;
          shape : K.With_subkind.t list
        }
    | Mixed of P.Mixed_block_kind.t

  type possibly_refined_kind =
    | Just_kind of K.t
    | With_subkind of K.With_subkind.t

  let length = function
    | Not_mixed { shape; _ } -> List.length shape
    | Mixed shape -> P.Mixed_block_kind.length shape

  let fold_left_fields f init t =
    match t with
    | Not_mixed { shape; _ } ->
      List.fold_left (fun acc x -> f acc (With_subkind x)) init shape
    | Mixed mixed ->
      P.Mixed_block_kind.fold_left (fun acc x -> f acc (Just_kind x)) init mixed
end

let simplify_make_block ~original_prim tag ~(block_shape : Block_shape.t)
    ~(mutable_or_immutable : Mutability.t) alloc_mode dacc ~original_term _dbg
    ~args_with_tys ~result_var =
  let args, _arg_tys = List.split args_with_tys in
  if List.compare_length_with args (Block_shape.length block_shape) <> 0
  then
    Misc.fatal_errorf
      "Shape in [Make_block] of different length from argument list:@ %a"
      Named.print original_term;
  let remaining_args, result =
    let typing_env = DA.typing_env dacc in
    Block_shape.fold_left_fields
      (fun (args, env_extension) arg_kind : (_ * _ Or_bottom.t) ->
        let arg, args =
          match args with
          | arg :: args -> arg, args
          | [] ->
            Misc.fatal_error
              "We already checked that [args] and [block_shape] have the same \
               length"
        in
        let result =
          let open Or_bottom.Let_syntax in
          let<* env_extension = env_extension in
          Simple.pattern_match' arg
            ~var:(fun _ ~coercion:_ : _ Or_bottom.t ->
              let<* _ty, env_extension' =
                match arg_kind with
                | Just_kind arg_kind ->
                  T.meet typing_env
                    (T.alias_type_of arg_kind arg)
                    (T.unknown arg_kind)
                | With_subkind arg_kind ->
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
            ~symbol:(fun _ ~coercion:_ : _ Or_bottom.t -> Ok env_extension)
        in
        args, result)
      (args, Or_bottom.Ok TEE.empty)
      block_shape
  in
  let () =
    match remaining_args with
    | [] -> ()
    | _ :: _ ->
      Misc.fatal_error
        "We already checked that [args] and [block_shape] have the same length"
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
      match block_shape with
      | Mixed _ -> T.any_value
      | Not_mixed { shape; field_kind } -> (
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
        | Mutable -> T.mutable_block alloc_mode)
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
  simplify_make_block ~original_prim
    ~block_shape:(Not_mixed { field_kind = K.naked_float; shape })
    Tag.double_array_tag ~mutable_or_immutable alloc_mode dacc ~original_term
    dbg ~args_with_tys ~result_var

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

let simplify_make_mixed_block ~original_prim ~kind
    ~(mutable_or_immutable : Mutability.t) alloc_mode dacc ~original_term dbg
    ~args_with_tys ~result_var =
  simplify_make_block ~original_prim
    ~mutable_or_immutable
      (* CR mixed blocks v1: [Tag.zero] will need to change when we allow mixed
         blocks in inline records. *)
    ~block_shape:(Mixed kind) Tag.zero alloc_mode dacc ~original_term dbg
    ~args_with_tys ~result_var

let simplify_variadic_primitive dacc original_prim (prim : P.variadic_primitive)
    ~args_with_tys dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Make_block (Values (tag, shape), mutable_or_immutable, alloc_mode) ->
      let tag = Tag.Scannable.to_tag tag in
      simplify_make_block ~original_prim tag
        ~block_shape:(Not_mixed { field_kind = K.value; shape })
        ~mutable_or_immutable alloc_mode
    | Make_block (Naked_floats, mutable_or_immutable, alloc_mode) ->
      simplify_make_block_of_floats ~original_prim ~mutable_or_immutable
        alloc_mode
    | Make_array (array_kind, mutable_or_immutable, alloc_mode) ->
      simplify_make_array array_kind ~mutable_or_immutable alloc_mode
    | Make_mixed_block (kind, mutable_or_immutable, alloc_mode) ->
      simplify_make_mixed_block ~original_prim ~kind ~mutable_or_immutable
        alloc_mode
  in
  simplifier dacc ~original_term dbg ~args_with_tys ~result_var
