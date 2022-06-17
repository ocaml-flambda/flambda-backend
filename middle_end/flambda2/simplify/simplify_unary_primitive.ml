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
module A = Number_adjuncts
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64

let simplify_project_function_slot ~move_from ~move_to ~min_name_mode dacc
    ~original_term ~arg:closure ~arg_ty:closure_ty ~result_var =
  let typing_env = DA.typing_env dacc in
  match
    T.prove_project_function_slot_simple typing_env ~min_name_mode closure_ty
      move_to
  with
  | Invalid ->
    let ty = T.bottom K.value in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create_invalid dacc
  | Proved simple ->
    let reachable = SPR.create (Named.create_simple simple) ~try_reify:true in
    let dacc =
      DA.add_variable dacc result_var (T.alias_type_of K.value simple)
    in
    reachable dacc
  | Unknown ->
    let result = Simple.var (Bound_var.var result_var) in
    let closures =
      Function_slot.Map.empty
      |> Function_slot.Map.add move_from closure
      |> Function_slot.Map.add move_to result
    in
    Simplify_common.simplify_projection dacc ~original_term
      ~deconstructing:closure_ty
      ~shape:
        (T.closure_with_at_least_these_function_slots
           ~this_function_slot:move_from closures)
      ~result_var ~result_kind:K.value

let simplify_project_value_slot function_slot value_slot ~min_name_mode dacc
    ~original_term ~arg:closure ~arg_ty:closure_ty ~result_var =
  let typing_env = DA.typing_env dacc in
  match
    T.prove_project_value_slot_simple typing_env ~min_name_mode closure_ty
      value_slot
  with
  | Invalid ->
    let ty = T.bottom K.value in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create_invalid dacc
  | Proved simple ->
    (* Owing to the semantics of [Simplify_set_of_closures] when computing the
       types of value slots -- in particular because it allows depth variables
       to exist in such types that are not in scope in the body of the function
       -- we need to ensure that any [Simple] retrieved here from the closure
       environment is simplified. This will ensure that if it is not in scope,
       any associated coercion will be erased appropriately. *)
    let simple =
      if Coercion.is_id (Simple.coercion simple)
      then simple
      else
        let ty = S.simplify_simple dacc simple ~min_name_mode in
        T.get_alias_exn ty
    in
    let reachable = SPR.create (Named.create_simple simple) ~try_reify:true in
    let dacc =
      DA.add_variable dacc result_var (T.alias_type_of K.value simple)
    in
    reachable dacc
  | Unknown ->
    let result =
      Simplify_common.simplify_projection dacc ~original_term
        ~deconstructing:closure_ty
        ~shape:
          (T.closure_with_at_least_this_value_slot
             ~this_function_slot:function_slot value_slot
             ~value_slot_var:(Bound_var.var result_var))
        ~result_var ~result_kind:K.value
    in
    let dacc =
      (* See comments on the [Block_load] cases in [Simplify_binary_primitive]
         that explain what is going on with symbol projections. *)
      let module SP = Symbol_projection in
      let dacc = result.dacc in
      Simple.pattern_match' closure
        ~const:(fun _ -> dacc)
        ~symbol:(fun symbol_projected_from ~coercion:_ ->
          let proj =
            SP.create symbol_projected_from
              (SP.Projection.project_value_slot function_slot value_slot)
          in
          let var = Bound_var.var result_var in
          DA.map_denv dacc ~f:(fun denv ->
              DE.add_symbol_projection denv var proj))
        ~var:(fun _ ~coercion:_ -> dacc)
    in
    let dacc = DA.add_use_of_value_slot dacc value_slot in
    SPR.with_dacc result dacc

let simplify_unbox_number (boxable_number_kind : K.Boxable_number.t) dacc
    ~original_term ~arg ~arg_ty:boxed_number_ty ~result_var =
  let shape, result_kind =
    let result_var = Bound_var.var result_var in
    match boxable_number_kind with
    | Naked_float ->
      T.boxed_float_alias_to ~naked_float:result_var Unknown, K.naked_float
    | Naked_int32 ->
      T.boxed_int32_alias_to ~naked_int32:result_var Unknown, K.naked_int32
    | Naked_int64 ->
      T.boxed_int64_alias_to ~naked_int64:result_var Unknown, K.naked_int64
    | Naked_nativeint ->
      ( T.boxed_nativeint_alias_to ~naked_nativeint:result_var Unknown,
        K.naked_nativeint )
  in
  let alloc_mode =
    T.prove_alloc_mode_of_boxed_number (DA.typing_env dacc) boxed_number_ty
  in
  let result =
    Simplify_common.simplify_projection dacc ~original_term
      ~deconstructing:boxed_number_ty ~shape ~result_var ~result_kind
  in
  let dacc =
    let dacc = result.dacc in
    (* We can only add the inverse CSE equation if we know the alloc mode for
       certain and it is [Heap]. (As per [Flambda_primitive] we don't currently
       CSE local allocations.) *)
    match alloc_mode with
    | Unknown | Known Local -> dacc
    | Known Heap ->
      let boxing_prim : P.t =
        Unary
          ( Box_number (boxable_number_kind, Heap),
            Simple.var (Bound_var.var result_var) )
      in
      DA.map_denv dacc ~f:(fun denv ->
          DE.add_cse denv
            (P.Eligible_for_cse.create_exn boxing_prim)
            ~bound_to:arg)
  in
  SPR.with_dacc result dacc

let simplify_untag_immediate dacc ~original_term ~arg ~arg_ty:boxed_number_ty
    ~result_var =
  let shape, result_kind =
    let result_var = Bound_var.var result_var in
    T.tagged_immediate_alias_to ~naked_immediate:result_var, K.naked_immediate
  in
  let result =
    Simplify_common.simplify_projection dacc ~original_term
      ~deconstructing:boxed_number_ty ~shape ~result_var ~result_kind
  in
  let dacc =
    let dacc = result.dacc in
    let tagging_prim : P.t =
      Unary (Tag_immediate, Simple.var (Bound_var.var result_var))
    in
    DA.map_denv dacc ~f:(fun denv ->
        DE.add_cse denv
          (P.Eligible_for_cse.create_exn tagging_prim)
          ~bound_to:arg)
  in
  SPR.with_dacc result dacc

let simplify_box_number (boxable_number_kind : K.Boxable_number.t) alloc_mode
    dacc ~original_term ~arg:_ ~arg_ty:naked_number_ty ~result_var =
  (* CR mshinwell: This should check the kind of [naked_number_ty] (or the
     creation functions used below should). *)
  let ty =
    match boxable_number_kind with
    | Naked_float -> T.box_float naked_number_ty (Known alloc_mode)
    | Naked_int32 -> T.box_int32 naked_number_ty (Known alloc_mode)
    | Naked_int64 -> T.box_int64 naked_number_ty (Known alloc_mode)
    | Naked_nativeint -> T.box_nativeint naked_number_ty (Known alloc_mode)
  in
  let dacc = DA.add_variable dacc result_var ty in
  SPR.create original_term ~try_reify:true dacc

let simplify_tag_immediate dacc ~original_term ~arg:_ ~arg_ty:naked_number_ty
    ~result_var =
  let ty = T.tag_immediate naked_number_ty in
  let dacc = DA.add_variable dacc result_var ty in
  SPR.create original_term ~try_reify:true dacc

let simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty:_
    ~result_var ~make_shape =
  let dacc = DA.add_variable dacc result_var (make_shape scrutinee) in
  SPR.create original_term ~try_reify:true dacc

let simplify_is_int dacc ~original_term ~arg:scrutinee ~arg_ty:scrutinee_ty
    ~result_var =
  simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty
    ~result_var ~make_shape:(fun scrutinee -> T.is_int_for_scrutinee ~scrutinee)

let simplify_get_tag dacc ~original_term ~arg:scrutinee ~arg_ty:scrutinee_ty
    ~result_var =
  simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty
    ~result_var ~make_shape:(fun block -> T.get_tag_for_block ~block)

let simplify_array_length dacc ~original_term ~arg:_ ~arg_ty:array_ty
    ~result_var =
  let result = Simple.var (Bound_var.var result_var) in
  Simplify_common.simplify_projection dacc ~original_term
    ~deconstructing:array_ty
    ~shape:
      (T.array_of_length ~element_kind:Unknown
         ~length:(T.alias_type_of K.value result))
    ~result_var ~result_kind:K.value

(* CR-someday mshinwell: Consider whether "string length" should be treated like
   a projection (cf. "array length"). *)
let simplify_string_length dacc ~original_term ~arg:_ ~arg_ty:str_ty ~result_var
    =
  match T.prove_strings (DA.typing_env dacc) str_ty with
  | Proved str_infos -> (
    if String_info.Set.is_empty str_infos
    then SPR.create_invalid dacc
    else
      let lengths =
        String_info.Set.elements str_infos
        |> List.map String_info.size
        |> List.map Targetint_31_63.int
        |> Targetint_31_63.Set.of_list
      in
      match Targetint_31_63.Set.get_singleton lengths with
      | Some length ->
        let ty = T.this_naked_immediate length in
        let dacc = DA.add_variable dacc result_var ty in
        let named =
          Named.create_simple
            (Simple.untagged_const_int (Targetint_31_63.to_targetint length))
        in
        SPR.create named ~try_reify:false dacc
      | None ->
        let ty = T.these_naked_immediates lengths in
        let dacc = DA.add_variable dacc result_var ty in
        SPR.create original_term ~try_reify:false dacc)
  | Unknown ->
    let ty = T.unknown K.naked_immediate in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:false dacc
  | Invalid -> SPR.create_invalid dacc

module Unary_int_arith (I : A.Int_number_kind) = struct
  let simplify (op : P.unary_int_arith_op) dacc ~original_term ~arg:_ ~arg_ty
      ~result_var =
    match I.unboxed_prover (DA.typing_env dacc) arg_ty with
    | Proved ints -> (
      assert (not (I.Num.Set.is_empty ints));
      let f =
        match op with
        | Neg -> I.Num.neg
        | Swap_byte_endianness -> I.Num.swap_byte_endianness
      in
      let possible_results = I.Num.Set.map f ints in
      let ty = I.these_unboxed possible_results in
      let dacc = DA.add_variable dacc result_var ty in
      match I.Num.Set.get_singleton possible_results with
      | None -> SPR.create original_term ~try_reify:false dacc
      | Some i ->
        let named = Named.create_simple (Simple.const (I.Num.to_const i)) in
        SPR.create named ~try_reify:false dacc)
    | Unknown ->
      let dacc =
        DA.add_variable dacc result_var
          (T.unknown
             (K.Standard_int_or_float.to_kind I.standard_int_or_float_kind))
      in
      SPR.create original_term ~try_reify:false dacc
    | Invalid -> SPR.create_invalid dacc
end

module Unary_int_arith_tagged_immediate =
  Unary_int_arith (A.For_tagged_immediates)
module Unary_int_arith_naked_immediate = Unary_int_arith (A.For_naked_immediates)
module Unary_int_arith_naked_int32 = Unary_int_arith (A.For_int32s)
module Unary_int_arith_naked_int64 = Unary_int_arith (A.For_int64s)
module Unary_int_arith_naked_nativeint = Unary_int_arith (A.For_nativeints)

module Make_simplify_int_conv (N : A.Number_kind) = struct
  let simplify ~(dst : K.Standard_int_or_float.t) dacc ~original_term ~arg
      ~arg_ty ~result_var =
    if K.Standard_int_or_float.equal N.standard_int_or_float_kind dst
    then
      let dacc = DA.add_variable dacc result_var arg_ty in
      SPR.create (Named.create_simple arg) ~try_reify:false dacc
    else
      let proof = N.unboxed_prover (DA.typing_env dacc) arg_ty in
      let module Num = N.Num in
      match proof with
      | Proved is -> (
        assert (Num.Set.cardinal is > 0);
        let module For_kind (P : sig
          module Result_num : Container_types.S

          val num_to_result_num : Num.t -> Result_num.t

          val these : Result_num.Set.t -> T.t

          val const : Result_num.t -> Reg_width_const.t
        end) =
        struct
          let result =
            let res_ns =
              Num.Set.fold
                (fun n res_ns ->
                  P.Result_num.Set.add (P.num_to_result_num n) res_ns)
                is P.Result_num.Set.empty
            in
            let ty = P.these res_ns in
            let dacc = DA.add_variable dacc result_var ty in
            match P.Result_num.Set.get_singleton res_ns with
            | None -> SPR.create original_term ~try_reify:false dacc
            | Some f ->
              let named = Named.create_simple (Simple.const (P.const f)) in
              SPR.create named ~try_reify:false dacc
        end in
        match dst with
        | Tagged_immediate ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Targetint_31_63

            let num_to_result_num = Num.to_immediate

            let these = T.these_tagged_immediates

            let const = Reg_width_const.tagged_immediate
          end) in
          M.result
        | Naked_immediate ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Targetint_31_63

            let num_to_result_num = Num.to_immediate

            let these = T.these_naked_immediates

            let const = Reg_width_const.naked_immediate
          end) in
          M.result
        | Naked_float ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Float

            let num_to_result_num = Num.to_naked_float

            let these = T.these_naked_floats

            let const = Reg_width_const.naked_float
          end) in
          M.result
        | Naked_int32 ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Int32

            let num_to_result_num = Num.to_naked_int32

            let these = T.these_naked_int32s

            let const = Reg_width_const.naked_int32
          end) in
          M.result
        | Naked_int64 ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Int64

            let num_to_result_num = Num.to_naked_int64

            let these = T.these_naked_int64s

            let const = Reg_width_const.naked_int64
          end) in
          M.result
        | Naked_nativeint ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Targetint_32_64

            let num_to_result_num = Num.to_naked_nativeint

            let these = T.these_naked_nativeints

            let const = Reg_width_const.naked_nativeint
          end) in
          M.result)
      | Unknown ->
        let ty = T.unknown (K.Standard_int_or_float.to_kind dst) in
        let dacc = DA.add_variable dacc result_var ty in
        SPR.create original_term ~try_reify:false dacc
      | Invalid -> SPR.create_invalid dacc
end

module Simplify_int_conv_tagged_immediate =
  Make_simplify_int_conv (A.For_tagged_immediates)
module Simplify_int_conv_naked_immediate =
  Make_simplify_int_conv (A.For_naked_immediates)
module Simplify_int_conv_naked_float = Make_simplify_int_conv (A.For_floats)
module Simplify_int_conv_naked_int32 = Make_simplify_int_conv (A.For_int32s)
module Simplify_int_conv_naked_int64 = Make_simplify_int_conv (A.For_int64s)
module Simplify_int_conv_naked_nativeint =
  Make_simplify_int_conv (A.For_nativeints)

let simplify_boolean_not dacc ~original_term ~arg:_ ~arg_ty ~result_var =
  let denv = DA.denv dacc in
  let typing_env = DE.typing_env denv in
  let proof = T.prove_equals_tagged_immediates typing_env arg_ty in
  match proof with
  | Proved imms -> (
    let imms =
      Targetint_31_63.Set.filter_map
        (fun imm ->
          if Targetint_31_63.equal imm Targetint_31_63.zero
          then Some Targetint_31_63.one
          else if Targetint_31_63.equal imm Targetint_31_63.one
          then Some Targetint_31_63.zero
          else None)
        imms
    in
    if Targetint_31_63.Set.is_empty imms
    then SPR.create_invalid dacc
    else
      let ty = T.these_tagged_immediates imms in
      let dacc = DA.add_variable dacc result_var ty in
      match Targetint_31_63.Set.get_singleton imms with
      | None -> SPR.create original_term ~try_reify:false dacc
      | Some imm ->
        let named =
          Named.create_simple
            (Simple.const_int (Targetint_31_63.to_targetint imm))
        in
        SPR.create named ~try_reify:false dacc)
  | Unknown ->
    (* CR-someday mshinwell: This could say something like (in the type) "when
       the input is 0, the value is 1" and vice-versa. *)
    let ty = T.these_tagged_immediates Targetint_31_63.all_bools in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:false dacc
  | Invalid -> SPR.create_invalid dacc

let simplify_reinterpret_int64_as_float dacc ~original_term ~arg:_ ~arg_ty
    ~result_var =
  let typing_env = DE.typing_env (DA.denv dacc) in
  let proof = T.prove_naked_int64s typing_env arg_ty in
  match proof with
  | Proved int64s -> (
    let floats =
      Int64.Set.fold
        (fun int64 floats -> Float.Set.add (Float.of_bits int64) floats)
        int64s Float.Set.empty
    in
    let ty = T.these_naked_floats floats in
    let dacc = DA.add_variable dacc result_var ty in
    match Float.Set.get_singleton floats with
    | None -> SPR.create original_term ~try_reify:false dacc
    | Some f ->
      let named =
        Named.create_simple (Simple.const (Reg_width_const.naked_float f))
      in
      SPR.create named ~try_reify:false dacc)
  | Unknown ->
    let dacc = DA.add_variable dacc result_var T.any_naked_float in
    SPR.create original_term ~try_reify:false dacc
  | Invalid -> SPR.create_invalid dacc

let simplify_float_arith_op (op : P.unary_float_arith_op) dacc ~original_term
    ~arg:_ ~arg_ty ~result_var =
  let module F = Numeric_types.Float_by_bit_pattern in
  let denv = DA.denv dacc in
  let proof = T.prove_naked_floats (DE.typing_env denv) arg_ty in
  match proof with
  | Proved fs when DE.propagating_float_consts denv -> (
    assert (not (Float.Set.is_empty fs));
    let f =
      match op with Abs -> F.IEEE_semantics.abs | Neg -> F.IEEE_semantics.neg
    in
    let possible_results = F.Set.map f fs in
    let ty = T.these_naked_floats possible_results in
    let dacc = DA.add_variable dacc result_var ty in
    match Float.Set.get_singleton fs with
    | None -> SPR.create original_term ~try_reify:false dacc
    | Some f ->
      let named =
        Named.create_simple (Simple.const (Reg_width_const.naked_float f))
      in
      SPR.create named ~try_reify:false dacc)
  | Proved _ | Unknown ->
    let ty = T.unknown K.naked_float in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:false dacc
  | Invalid -> SPR.create_invalid dacc

let simplify_is_boxed_float dacc ~original_term ~arg:_ ~arg_ty ~result_var =
  assert (Flambda_features.flat_float_array ());
  match T.prove_is_or_is_not_a_boxed_float (DA.typing_env dacc) arg_ty with
  | Proved is_a_boxed_float ->
    let imm = Targetint_31_63.bool is_a_boxed_float in
    let ty = T.this_naked_immediate imm in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create
      (Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm)))
      ~try_reify:false dacc
  | Unknown ->
    let ty = T.unknown K.naked_immediate in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:false dacc
  | Invalid | Wrong_kind -> SPR.create_invalid dacc

let simplify_is_flat_float_array dacc ~original_term ~arg:_ ~arg_ty ~result_var
    =
  assert (Flambda_features.flat_float_array ());
  let[@inline] known_answer ~is_flat_float_array =
    let imm = Targetint_31_63.bool is_flat_float_array in
    let ty = T.this_naked_immediate imm in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create
      (Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm)))
      ~try_reify:false dacc
  in
  match
    T.prove_is_array_with_element_kind (DA.typing_env dacc) arg_ty
      ~element_kind:K.With_subkind.naked_float
  with
  | Proved Exact -> known_answer ~is_flat_float_array:true
  | Proved Incompatible -> known_answer ~is_flat_float_array:false
  | Proved Compatible | Unknown ->
    let ty = T.unknown K.naked_immediate in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:false dacc
  | Invalid -> SPR.create_invalid dacc

let simplify_opaque_identity dacc ~original_term ~arg:_ ~arg_ty:_ ~result_var =
  let ty = T.unknown K.value in
  let dacc = DA.add_variable dacc result_var ty in
  SPR.create original_term ~try_reify:false dacc

let simplify_end_region dacc ~original_term ~arg:_ ~arg_ty:_ ~result_var =
  let ty = T.this_tagged_immediate Targetint_31_63.zero in
  let dacc = DA.add_variable dacc result_var ty in
  SPR.create original_term ~try_reify:false dacc

let simplify_unary_primitive dacc original_prim (prim : P.unary_primitive) ~arg
    ~arg_ty dbg ~result_var =
  let min_name_mode = Bound_var.name_mode result_var in
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Project_value_slot { project_from; value_slot } ->
      simplify_project_value_slot project_from value_slot ~min_name_mode
    | Project_function_slot { move_from; move_to } ->
      simplify_project_function_slot ~move_from ~move_to ~min_name_mode
    | Unbox_number boxable_number_kind ->
      simplify_unbox_number boxable_number_kind
    | Box_number (boxable_number_kind, alloc_mode) ->
      simplify_box_number boxable_number_kind alloc_mode
    | Tag_immediate -> simplify_tag_immediate
    | Untag_immediate -> simplify_untag_immediate
    | Is_int -> simplify_is_int
    | Get_tag -> simplify_get_tag
    | Array_length -> simplify_array_length
    | String_length _ -> simplify_string_length
    | Int_arith (kind, op) -> (
      match kind with
      | Tagged_immediate -> Unary_int_arith_tagged_immediate.simplify op
      | Naked_immediate -> Unary_int_arith_naked_immediate.simplify op
      | Naked_int32 -> Unary_int_arith_naked_int32.simplify op
      | Naked_int64 -> Unary_int_arith_naked_int64.simplify op
      | Naked_nativeint -> Unary_int_arith_naked_nativeint.simplify op)
    | Float_arith op -> simplify_float_arith_op op
    | Num_conv { src; dst } -> (
      match src with
      | Tagged_immediate -> Simplify_int_conv_tagged_immediate.simplify ~dst
      | Naked_immediate -> Simplify_int_conv_naked_immediate.simplify ~dst
      | Naked_float -> Simplify_int_conv_naked_float.simplify ~dst
      | Naked_int32 -> Simplify_int_conv_naked_int32.simplify ~dst
      | Naked_int64 -> Simplify_int_conv_naked_int64.simplify ~dst
      | Naked_nativeint -> Simplify_int_conv_naked_nativeint.simplify ~dst)
    | Boolean_not -> simplify_boolean_not
    | Reinterpret_int64_as_float -> simplify_reinterpret_int64_as_float
    | Is_boxed_float -> simplify_is_boxed_float
    | Is_flat_float_array -> simplify_is_flat_float_array
    | Int_as_pointer | Bigarray_length _ | Duplicate_array _ | Duplicate_block _
    | Opaque_identity ->
      simplify_opaque_identity
    | End_region -> simplify_end_region
  in
  simplifier dacc ~original_term ~arg ~arg_ty ~result_var
