(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Aspen Smith, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

let simplify_atomic_compare_and_set_or_exchange_args
    (args_kind : P.Block_access_field_kind.t) dacc ~comparison_value_ty
    ~new_value_ty : P.Block_access_field_kind.t =
  match args_kind with
  | Immediate ->
    (* No further specialization can be done *)
    Immediate
  | Any_value ->
    (* Unlike (for example) a normal write to a block, these primitives can be
       specialized based on the observed types of the arguments. (In the case of
       compare-exchange information can also be propagated based on the result
       type; see below.)

       For example for [Atomic_compare_and_set], observe that if both the second
       and third arguments (value with which to compare, and new value,
       respectively) are immediate, then there is no need to generate a GC
       barrier -- not only because the new value is immediate, but crucially
       also because the old value cannot be a pointer if the operation is to
       perform a write. *)
    let is_immediate ty =
      match T.prove_is_not_a_pointer (DA.typing_env dacc) ty with
      | Proved true -> true
      | Proved false | Unknown -> false
    in
    if is_immediate comparison_value_ty && is_immediate new_value_ty
    then Immediate
    else Any_value


let simplify_atomic_compare_and_set_field (args_kind : P.Block_access_field_kind.t)
    ~original_prim:_ dacc ~original_term:_ dbg ~arg1:atomic ~arg1_ty:_
    ~arg2:field ~arg2_ty:_ ~arg3:expected ~arg3_ty:comparison_value_ty ~arg4:desired
    ~arg4_ty:new_value_ty ~result_var =
  let args_kind =
    simplify_atomic_compare_and_set_or_exchange_args args_kind dacc
      ~comparison_value_ty ~new_value_ty
  in
  let new_term =
    Named.create_prim
      (Quaternary (Atomic_compare_and_set_field args_kind, atomic, field, expected, desired))
      dbg
  in
  let dacc = DA.add_variable dacc result_var T.any_tagged_bool in
  SPR.create new_term ~try_reify:false dacc

let simplify_atomic_compare_exchange_field
    ~(atomic_kind : P.Block_access_field_kind.t)
    ~(args_kind : P.Block_access_field_kind.t) ~original_prim:_ dacc
    ~original_term:_ dbg ~arg1:atomic ~arg1_ty:_ ~arg2:field ~arg2_ty:_
    ~arg3:expected ~arg3_ty:comparison_value_ty ~arg4:desired ~arg4_ty:new_value_ty
    ~result_var =
  let args_kind =
    simplify_atomic_compare_and_set_or_exchange_args args_kind dacc
      ~comparison_value_ty ~new_value_ty
  in
  let new_term =
    Named.create_prim
      (Quaternary
         (Atomic_compare_exchange_field { atomic_kind; args_kind },
          atomic, field, expected, desired))
      dbg
  in
  let result_var_ty =
    match atomic_kind with
    | Immediate -> T.any_tagged_immediate_or_null
    | Any_value -> T.any_value
  in
  let dacc = DA.add_variable dacc result_var result_var_ty in
  SPR.create new_term ~try_reify:false dacc

let simplify_quaternary_primitive dacc original_prim (prim : P.quaternary_primitive)
    ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3 ~arg3_ty ~arg4 ~arg4_ty dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Atomic_compare_and_set_field access_kind ->
      simplify_atomic_compare_and_set_field access_kind ~original_prim
    | Atomic_compare_exchange_field { atomic_kind; args_kind } ->
      simplify_atomic_compare_exchange_field ~atomic_kind ~args_kind ~original_prim
  in
  simplifier dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3
    ~arg3_ty ~arg4 ~arg4_ty ~result_var
