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

let simplify_array_set (array_kind : P.Array_kind.t)
    (array_set_kind : P.Array_set_kind.t) dacc ~original_term dbg ~arg1:array
    ~arg1_ty:array_ty ~arg2:index ~arg2_ty:_ ~arg3:new_value ~arg3_ty:_
    ~result_var =
  let orig_array_kind = array_kind in
  let array_kind =
    Simplify_common.specialise_array_kind dacc array_kind ~array_ty
  in
  match array_kind with
  | Bottom -> SPR.create_invalid dacc
  | Ok array_kind ->
    let () =
      match array_kind with
      | Immediates -> ()
      | Values -> (
        match array_set_kind with
        | Values _ -> ()
        | Immediates
        (* We don't expect specialisation regressions from Immediates to
           Values. *)
        | Naked_floats | Naked_float32s | Naked_int32s | Naked_int64s
        | Naked_nativeints | Naked_vec128s ->
          Misc.fatal_errorf
            "Didn't expect array specialisation to yield array kind %a from \
             array set kind %a (original array kind %a):@ %a"
            P.Array_kind.print array_kind P.Array_set_kind.print array_set_kind
            P.Array_kind.print orig_array_kind Named.print original_term)
      | Naked_floats | Naked_float32s | Naked_int32s | Naked_int64s
      | Naked_nativeints | Naked_vec128s | Unboxed_product _ ->
        ()
    in
    let named =
      Named.create_prim
        (Ternary
           (Array_set (array_kind, array_set_kind), array, index, new_value))
        dbg
    in
    let unit_ty = Flambda2_types.this_tagged_immediate Targetint_31_63.zero in
    let dacc = DA.add_variable dacc result_var unit_ty in
    SPR.create named ~try_reify:false dacc

let simplify_bytes_or_bigstring_set _bytes_like_value _string_accessor_width
    dacc ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_
    ~arg3_ty:_ ~result_var =
  SPR.create_unit dacc ~result_var ~original_term

let simplify_bigarray_set ~num_dimensions:_ _bigarray_kind _bigarray_layout dacc
    ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_ ~arg3_ty:_
    ~result_var =
  SPR.create_unit dacc ~result_var ~original_term

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
      match T.prove_is_immediate (DA.typing_env dacc) ty with
      | Proved true -> true
      | Proved false | Unknown -> false
    in
    if is_immediate comparison_value_ty && is_immediate new_value_ty
    then Immediate
    else Any_value

let simplify_atomic_compare_and_set (args_kind : P.Block_access_field_kind.t)
    ~original_prim:_ dacc ~original_term:_ dbg ~arg1:atomic ~arg1_ty:_
    ~arg2:comparison_value ~arg2_ty:comparison_value_ty ~arg3:new_value
    ~arg3_ty:new_value_ty ~result_var =
  let args_kind =
    simplify_atomic_compare_and_set_or_exchange_args args_kind dacc
      ~comparison_value_ty ~new_value_ty
  in
  let new_term =
    Named.create_prim
      (Ternary
         (Atomic_compare_and_set args_kind, atomic, comparison_value, new_value))
      dbg
  in
  let dacc = DA.add_variable dacc result_var T.any_tagged_bool in
  SPR.create new_term ~try_reify:false dacc

let simplify_atomic_compare_exchange
    ~(atomic_kind : P.Block_access_field_kind.t)
    ~(args_kind : P.Block_access_field_kind.t) ~original_prim:_ dacc
    ~original_term:_ dbg ~arg1:atomic ~arg1_ty:_ ~arg2:comparison_value
    ~arg2_ty:comparison_value_ty ~arg3:new_value ~arg3_ty:new_value_ty
    ~result_var =
  (* This primitive can have its arguments specialised as for compare-and-set
     (see above). However we can also propagate information about its result
     type. Since this relates to all possible values that the atomic can hold,
     we have to use the information provided by the frontend.

     Recall that the compare-exchange returns the old value. *)
  let args_kind =
    simplify_atomic_compare_and_set_or_exchange_args args_kind dacc
      ~comparison_value_ty ~new_value_ty
  in
  let new_term =
    Named.create_prim
      (Ternary
         ( Atomic_compare_exchange { atomic_kind; args_kind },
           atomic,
           comparison_value,
           new_value ))
      dbg
  in
  let result_var_ty =
    match atomic_kind (* N.B. not [args_kind] *) with
    | Immediate -> T.any_tagged_immediate_or_null
    | Any_value -> T.any_value
  in
  let dacc = DA.add_variable dacc result_var result_var_ty in
  SPR.create new_term ~try_reify:false dacc

let simplify_ternary_primitive dacc original_prim (prim : P.ternary_primitive)
    ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3 ~arg3_ty dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Array_set (array_kind, width) -> simplify_array_set array_kind width
    | Bytes_or_bigstring_set (bytes_like_value, string_accessor_width) ->
      simplify_bytes_or_bigstring_set bytes_like_value string_accessor_width
    | Bigarray_set (num_dimensions, bigarray_kind, bigarray_layout) ->
      simplify_bigarray_set ~num_dimensions bigarray_kind bigarray_layout
    | Atomic_compare_and_set access_kind ->
      simplify_atomic_compare_and_set access_kind ~original_prim
    | Atomic_compare_exchange { atomic_kind; args_kind } ->
      simplify_atomic_compare_exchange ~atomic_kind ~args_kind ~original_prim
  in
  simplifier dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3
    ~arg3_ty ~result_var
