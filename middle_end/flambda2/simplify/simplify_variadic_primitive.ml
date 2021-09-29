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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open! Simplify_import

let simplify_make_block_of_values dacc _prim dbg tag ~shape
    ~(mutable_or_immutable : Mutability.t) args_with_tys ~result_var =
  let denv = DA.denv dacc in
  let args, _arg_tys = List.split args_with_tys in
  let invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.invalid (), env_extension, args, dacc
  in
  if List.compare_lengths shape args <> 0
  then
    (* CR mshinwell: improve message *)
    Misc.fatal_errorf
      "GC value_kind indications in [Make_block] don't match up 1:1 with \
       arguments: %a"
      Simple.List.print args;
  (* CR mshinwell: This could probably be done more neatly. *)
  let found_bottom = ref false in
  let fields =
    List.map2
      (fun ((arg : Simple.t), arg_ty) _block_of_values_kind ->
        (* CR mshinwell: There should be a meet against a skeleton type computed
           from [block_of_values_kind]. *)
        if T.is_bottom (DE.typing_env denv) arg_ty then found_bottom := true;
        Simple.pattern_match arg
          ~const:(fun _ -> arg_ty)
          ~name:(fun _ ~coercion:_ -> T.alias_type_of K.value arg))
      args_with_tys shape
  in
  if !found_bottom
  then invalid ()
  else begin
    assert (List.compare_lengths fields shape = 0);
    let term : Named.t =
      Named.create_prim
        (Variadic (Make_block (Values (tag, shape), mutable_or_immutable), args))
        dbg
    in
    let tag = Tag.Scannable.to_tag tag in
    let ty =
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind:K.value ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.value ~fields
      | Mutable -> T.any_value ()
    in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.reachable term, env_extension, args, dacc
  end

let simplify_make_block_of_floats dacc _prim dbg
    ~(mutable_or_immutable : Mutability.t) args_with_tys ~result_var =
  let denv = DA.denv dacc in
  let args = List.map fst args_with_tys in
  let invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.invalid (), env_extension, args, dacc
  in
  (* CR mshinwell: This could probably be done more neatly. *)
  let found_bottom = ref false in
  let fields =
    List.map
      (fun ((arg : Simple.t), arg_ty) ->
        (* CR gbury: we should review all similar pieces of code in the file and
           aim to remove the T.is_bottom checks (kind of like #336 did in the
           simplifier). *)
        if T.is_bottom (DE.typing_env denv) arg_ty then found_bottom := true;
        Simple.pattern_match arg
          ~const:(fun _ -> arg_ty)
          ~name:(fun _ ~coercion:_ -> T.alias_type_of K.naked_float arg))
      args_with_tys
  in
  if !found_bottom
  then invalid ()
  else
    let term : Named.t =
      Named.create_prim
        (Variadic (Make_block (Naked_floats, mutable_or_immutable), args))
        dbg
    in
    let tag = Tag.double_array_tag in
    let ty =
      match mutable_or_immutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind:K.naked_float ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.naked_float ~fields
      | Mutable -> T.any_value ()
    in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Simplified_named.reachable term, env_extension, args, dacc

let simplify_variadic_primitive dacc (prim : P.variadic_primitive)
    ~args_with_tys dbg ~result_var =
  let result_var' = Bound_var.var result_var in
  match prim with
  | Make_block (Values (tag, shape), mutable_or_immutable) ->
    simplify_make_block_of_values dacc prim dbg tag ~shape ~mutable_or_immutable
      args_with_tys ~result_var:result_var'
  | Make_block (Naked_floats, mutable_or_immutable) ->
    simplify_make_block_of_floats dacc prim dbg ~mutable_or_immutable
      args_with_tys ~result_var:result_var'
  | Make_array _ ->
    (* CR mshinwell: The typing here needs to be improved *)
    let args, _tys = List.split args_with_tys in
    let named = Named.create_prim (Variadic (prim, args)) dbg in
    let length =
      match Targetint_31_63.Imm.of_int_option (List.length args) with
      | Some ti -> T.this_tagged_immediate (Targetint_31_63.int ti)
      | None -> T.unknown K.value
    in
    let ty = T.array_of_length ~length in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.reachable named, env_extension, args, dacc
