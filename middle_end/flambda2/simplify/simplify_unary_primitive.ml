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
  match
    T.meet_project_function_slot_simple (DA.typing_env dacc) ~min_name_mode
      closure_ty move_to
  with
  | Invalid -> SPR.create_invalid dacc
  | Known_result simple ->
    DA.add_variable dacc result_var (T.alias_type_of K.value simple)
    |> SPR.create (Named.create_simple simple) ~try_reify:true
  | Need_meet ->
    let closures =
      Function_slot.Map.empty
      |> Function_slot.Map.add move_from closure
      |> Function_slot.Map.add move_to (Simple.var (Bound_var.var result_var))
    in
    Simplify_common.simplify_projection dacc ~original_term
      ~deconstructing:closure_ty
      ~shape:
        (T.closure_with_at_least_these_function_slots
           ~this_function_slot:move_from closures)
      ~result_var ~result_kind:K.value

let simplify_project_value_slot function_slot value_slot ~min_name_mode dacc
    ~original_term ~arg:closure ~arg_ty:closure_ty ~result_var =
  let kind = Value_slot.kind value_slot in
  let result =
    (* We try a faster method before falling back to [simplify_projection]. *)
    match
      T.meet_project_value_slot_simple (DA.typing_env dacc) ~min_name_mode
        closure_ty value_slot
    with
    | Invalid -> SPR.create_invalid dacc
    | Known_result simple ->
      (* Owing to the semantics of [Simplify_set_of_closures] when computing the
         types of value slots -- in particular because it allows depth variables
         to exist in such types that are not in scope in the body of the
         function -- we need to ensure that any [Simple] retrieved here from the
         closure environment is simplified. This will ensure that if it is not
         in scope, any associated coercion will be erased appropriately. *)
      let simple =
        if Coercion.is_id (Simple.coercion simple)
        then simple
        else T.get_alias_exn (S.simplify_simple dacc simple ~min_name_mode)
      in
      let dacc =
        DA.add_variable dacc result_var
          (T.alias_type_of (K.With_subkind.kind kind) simple)
      in
      SPR.create (Named.create_simple simple) ~try_reify:true dacc
    | Need_meet ->
      let result =
        Simplify_common.simplify_projection dacc ~original_term
          ~deconstructing:closure_ty
          ~shape:
            (T.closure_with_at_least_this_value_slot
               ~this_function_slot:function_slot value_slot
               ~value_slot_var:(Bound_var.var result_var) ~value_slot_kind:kind)
          ~result_var ~result_kind:(K.With_subkind.kind kind)
      in
      let dacc = DA.add_use_of_value_slot result.dacc value_slot in
      SPR.with_dacc result dacc
  in
  let dacc =
    Simplify_common.add_symbol_projection result.dacc ~projected_from:closure
      (Symbol_projection.Projection.project_value_slot function_slot value_slot)
      ~projection_bound_to:result_var ~kind
  in
  SPR.with_dacc result dacc

let simplify_unbox_number (boxable_number_kind : K.Boxable_number.t) dacc
    ~original_term ~arg ~arg_ty:boxed_number_ty ~result_var =
  let result_var' = Bound_var.var result_var in
  let shape, result_kind =
    match boxable_number_kind with
    | Naked_float ->
      ( T.boxed_float_alias_to ~naked_float:result_var'
          (Alloc_mode.For_types.unknown ()),
        K.naked_float )
    | Naked_int32 ->
      ( T.boxed_int32_alias_to ~naked_int32:result_var'
          (Alloc_mode.For_types.unknown ()),
        K.naked_int32 )
    | Naked_int64 ->
      ( T.boxed_int64_alias_to ~naked_int64:result_var'
          (Alloc_mode.For_types.unknown ()),
        K.naked_int64 )
    | Naked_nativeint ->
      ( T.boxed_nativeint_alias_to ~naked_nativeint:result_var'
          (Alloc_mode.For_types.unknown ()),
        K.naked_nativeint )
    | Naked_vec128 ->
      ( T.boxed_vec128_alias_to ~naked_vec128:result_var'
          (Alloc_mode.For_types.unknown ()),
        K.naked_vec128 )
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
    | Unknown | Proved (Local | Heap_or_local) -> dacc
    | Proved Heap ->
      DA.map_denv dacc ~f:(fun denv ->
          DE.add_cse denv
            (P.Eligible_for_cse.create_exn
               (Unary
                  ( Box_number
                      (boxable_number_kind, Alloc_mode.For_allocations.heap),
                    Simple.var result_var' )))
            ~bound_to:arg)
  in
  SPR.with_dacc result dacc

let simplify_untag_immediate dacc ~original_term ~arg ~arg_ty:boxed_number_ty
    ~result_var =
  let result_var' = Bound_var.var result_var in
  let shape, result_kind =
    T.tagged_immediate_alias_to ~naked_immediate:result_var', K.naked_immediate
  in
  let result =
    Simplify_common.simplify_projection dacc ~original_term
      ~deconstructing:boxed_number_ty ~shape ~result_var ~result_kind
  in
  let dacc =
    DA.map_denv result.dacc ~f:(fun denv ->
        DE.add_cse denv
          (P.Eligible_for_cse.create_exn
             (Unary (Tag_immediate, Simple.var result_var')))
          ~bound_to:arg)
  in
  SPR.with_dacc result dacc

let simplify_box_number (boxable_number_kind : K.Boxable_number.t) alloc_mode
    dacc ~original_term ~arg:_ ~arg_ty:naked_number_ty ~result_var =
  let ty =
    let alloc_mode = Alloc_mode.For_allocations.as_type alloc_mode in
    match boxable_number_kind with
    | Naked_float -> T.box_float naked_number_ty alloc_mode
    | Naked_int32 -> T.box_int32 naked_number_ty alloc_mode
    | Naked_int64 -> T.box_int64 naked_number_ty alloc_mode
    | Naked_nativeint -> T.box_nativeint naked_number_ty alloc_mode
    | Naked_vec128 -> T.box_vec128 naked_number_ty alloc_mode
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
  (* CR vlaviron: We could use prover functions to simplify but it's probably
     not going to help that much.

     Example: Option.is_none is compiled to a single [Is_int] primitive
     (followed by [Tag_immediate]), and if called on a value with statically
     known shape then the type that will be propagated is not the most precise
     ([Is_int x] instead of a constant). However, in practice the information
     can be recovered both when switching on the value (through regular meet) or
     when trying to lift a block containing the value (through reify). *)
  let dacc = DA.add_variable dacc result_var (make_shape scrutinee) in
  SPR.create original_term ~try_reify:true dacc

let simplify_is_int ~variant_only dacc ~original_term ~arg:scrutinee
    ~arg_ty:scrutinee_ty ~result_var =
  if variant_only
  then
    simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty
      ~result_var ~make_shape:(fun scrutinee ->
        T.is_int_for_scrutinee ~scrutinee)
  else
    match T.prove_is_int (DA.typing_env dacc) scrutinee_ty with
    | Proved b ->
      let ty = T.this_naked_immediate (Targetint_31_63.bool b) in
      let dacc = DA.add_variable dacc result_var ty in
      SPR.create original_term ~try_reify:false dacc
    | Unknown ->
      SPR.create_unknown dacc ~result_var K.naked_immediate ~original_term

let simplify_get_tag dacc ~original_term ~arg:scrutinee ~arg_ty:scrutinee_ty
    ~result_var =
  simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty
    ~result_var ~make_shape:(fun block -> T.get_tag_for_block ~block)

let simplify_array_length _array_kind dacc ~original_term ~arg:_
    ~arg_ty:array_ty ~result_var =
  (* CR mshinwell: we could make use of [array_kind], but maybe not for much.
     Need to be careful in the float case because of the float array
     optimisation (see lambda_to_flambda_primitives.ml and flambda2.ml). *)
  let result = Simple.var (Bound_var.var result_var) in
  Simplify_common.simplify_projection dacc ~original_term
    ~deconstructing:array_ty
    ~shape:
      (T.array_of_length ~element_kind:Unknown
         ~length:(T.alias_type_of K.value result)
         (Alloc_mode.For_types.unknown ()))
    ~result_var ~result_kind:K.value

(* CR-someday mshinwell: Consider whether "string length" should be treated like
   a projection (cf. "array length"). *)
let simplify_string_length dacc ~original_term ~arg:_ ~arg_ty:str_ty ~result_var
    =
  match T.meet_strings (DA.typing_env dacc) str_ty with
  | Known_result str_infos ->
    if String_info.Set.is_empty str_infos
    then SPR.create_invalid dacc
    else
      let lengths =
        String_info.Set.elements str_infos
        |> List.map String_info.size |> Targetint_31_63.Set.of_list
      in
      let ty = T.these_naked_immediates lengths in
      let dacc = DA.add_variable dacc result_var ty in
      SPR.create original_term ~try_reify:true dacc
  | Need_meet ->
    SPR.create_unknown dacc ~result_var K.naked_immediate ~original_term
  | Invalid -> SPR.create_invalid dacc

module Unary_int_arith (I : A.Int_number_kind) = struct
  let simplify (op : P.unary_int_arith_op) dacc ~original_term ~arg:_ ~arg_ty
      ~result_var =
    match I.unboxed_prover (DA.typing_env dacc) arg_ty with
    | Known_result ints ->
      assert (not (I.Num.Set.is_empty ints));
      let f =
        match op with
        | Neg -> I.Num.neg
        | Swap_byte_endianness -> I.Num.swap_byte_endianness
      in
      let possible_results = I.Num.Set.map f ints in
      let ty = I.these_unboxed possible_results in
      let dacc = DA.add_variable dacc result_var ty in
      SPR.create original_term ~try_reify:true dacc
    | Need_meet ->
      let result_kind =
        K.Standard_int_or_float.to_kind I.standard_int_or_float_kind
      in
      SPR.create_unknown dacc ~result_var result_kind ~original_term
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
      | Known_result is -> (
        assert (Num.Set.cardinal is > 0);
        let module For_kind (P : sig
          module Result_num : Container_types.S

          val num_to_result_num : Num.t -> Result_num.t

          val these : Result_num.Set.t -> T.t
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
            SPR.create original_term ~try_reify:true dacc
        end in
        match dst with
        | Tagged_immediate ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Targetint_31_63

            let num_to_result_num = Num.to_immediate

            let these = T.these_tagged_immediates
          end) in
          M.result
        | Naked_immediate ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Targetint_31_63

            let num_to_result_num = Num.to_immediate

            let these = T.these_naked_immediates
          end) in
          M.result
        | Naked_float ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Float

            let num_to_result_num = Num.to_naked_float

            let these = T.these_naked_floats
          end) in
          M.result
        | Naked_int32 ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Int32

            let num_to_result_num = Num.to_naked_int32

            let these = T.these_naked_int32s
          end) in
          M.result
        | Naked_int64 ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Int64

            let num_to_result_num = Num.to_naked_int64

            let these = T.these_naked_int64s
          end) in
          M.result
        | Naked_nativeint ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Targetint_32_64

            let num_to_result_num = Num.to_naked_nativeint

            let these = T.these_naked_nativeints
          end) in
          M.result)
      | Need_meet ->
        let result_kind = K.Standard_int_or_float.to_kind dst in
        SPR.create_unknown dacc ~result_var result_kind ~original_term
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
  let proof = T.meet_equals_tagged_immediates typing_env arg_ty in
  match proof with
  | Known_result imms ->
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
      SPR.create original_term ~try_reify:true dacc
  | Need_meet ->
    (* CR-someday mshinwell: This could say something like (in the type) "when
       the input is 0, the value is 1" and vice-versa. *)
    let ty = T.these_tagged_immediates Targetint_31_63.all_bools in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:false dacc
  | Invalid -> SPR.create_invalid dacc

let simplify_reinterpret_int64_as_float dacc ~original_term ~arg:_ ~arg_ty
    ~result_var =
  let typing_env = DE.typing_env (DA.denv dacc) in
  let proof = T.meet_naked_int64s typing_env arg_ty in
  match proof with
  | Known_result int64s ->
    let floats =
      Int64.Set.fold
        (fun int64 floats -> Float.Set.add (Float.of_bits int64) floats)
        int64s Float.Set.empty
    in
    let ty = T.these_naked_floats floats in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:true dacc
  | Need_meet ->
    let dacc = DA.add_variable dacc result_var T.any_naked_float in
    SPR.create original_term ~try_reify:false dacc
  | Invalid -> SPR.create_invalid dacc

let simplify_float_arith_op (op : P.unary_float_arith_op) dacc ~original_term
    ~arg:_ ~arg_ty ~result_var =
  let module F = Numeric_types.Float_by_bit_pattern in
  let denv = DA.denv dacc in
  let proof = T.meet_naked_floats (DE.typing_env denv) arg_ty in
  match proof with
  | Known_result fs when DE.propagating_float_consts denv ->
    assert (not (Float.Set.is_empty fs));
    let f =
      match op with Abs -> F.IEEE_semantics.abs | Neg -> F.IEEE_semantics.neg
    in
    let possible_results = F.Set.map f fs in
    let ty = T.these_naked_floats possible_results in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:true dacc
  | Known_result _ | Need_meet ->
    SPR.create_unknown dacc ~result_var K.naked_float ~original_term
  | Invalid -> SPR.create_invalid dacc

let simplify_is_boxed_float dacc ~original_term ~arg:_ ~arg_ty ~result_var =
  (* CR mshinwell: see CRs in lambda_to_flambda_primitives.ml

     assert (Flambda_features.flat_float_array ()); *)
  match T.prove_is_or_is_not_a_boxed_float (DA.typing_env dacc) arg_ty with
  | Proved is_a_boxed_float ->
    let imm = Targetint_31_63.bool is_a_boxed_float in
    let ty = T.this_naked_immediate imm in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create original_term ~try_reify:true dacc
  | Unknown ->
    SPR.create_unknown dacc ~result_var K.naked_immediate ~original_term

let simplify_is_flat_float_array dacc ~original_term ~arg:_ ~arg_ty ~result_var
    =
  (* CR mshinwell: see CRs in lambda_to_flambda_primitives.ml

     assert (Flambda_features.flat_float_array ()); *)
  match
    T.meet_is_naked_number_array (DA.typing_env dacc) arg_ty Naked_float
  with
  | Known_result is_flat_float_array ->
    let imm = Targetint_31_63.bool is_flat_float_array in
    let ty = T.this_naked_immediate imm in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create
      (Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm)))
      ~try_reify:false dacc
  | Need_meet ->
    SPR.create_unknown dacc ~result_var K.naked_immediate ~original_term
  | Invalid -> SPR.create_invalid dacc

let simplify_opaque_identity dacc ~kind ~original_term ~arg:_ ~arg_ty:_
    ~result_var =
  SPR.create_unknown dacc ~result_var kind ~original_term

let simplify_end_region dacc ~original_term ~arg:_ ~arg_ty:_ ~result_var =
  let ty = T.this_tagged_immediate Targetint_31_63.zero in
  let dacc = DA.add_variable dacc result_var ty in
  SPR.create original_term ~try_reify:false dacc

let simplify_end_try_region dacc ~original_term ~arg:_ ~arg_ty:_ ~result_var =
  let ty = T.this_tagged_immediate Targetint_31_63.zero in
  let dacc = DA.add_variable dacc result_var ty in
  SPR.create original_term ~try_reify:false dacc

let simplify_int_as_pointer ~mode:_ dacc ~original_term ~arg:_ ~arg_ty:_
    ~result_var =
  SPR.create_unknown dacc ~result_var K.value ~original_term

let simplify_bigarray_length ~dimension:_ dacc ~original_term ~arg:_ ~arg_ty:_
    ~result_var =
  SPR.create_unknown dacc ~result_var K.naked_immediate ~original_term

let simplify_duplicate_array ~kind:_ ~(source_mutability : Mutability.t)
    ~(destination_mutability : Mutability.t) dacc ~original_term ~arg:_ ~arg_ty
    ~result_var =
  (* This simplification should eliminate bounds checks on array literals. *)
  match source_mutability, destination_mutability with
  | Immutable, Mutable -> (
    match T.meet_is_immutable_array (DA.typing_env dacc) arg_ty with
    | Invalid -> SPR.create_invalid dacc
    | Need_meet ->
      let dacc = DA.add_variable dacc result_var T.any_value in
      SPR.create original_term ~try_reify:false dacc
    | Known_result (element_kind, length, alloc_mode) ->
      let ty = T.mutable_array ~element_kind ~length alloc_mode in
      let dacc = DA.add_variable dacc result_var ty in
      SPR.create original_term ~try_reify:false dacc)
  | ( (Immutable | Immutable_unique | Mutable),
      (Immutable | Immutable_unique | Mutable) ) ->
    Misc.fatal_errorf
      "Combination of mutabilities not supported for [Duplicate_array]:@ %a"
      Named.print original_term

let simplify_duplicate_block ~kind:_ dacc ~original_term ~arg:_ ~arg_ty
    ~result_var =
  (* Any alias in the type to the whole block will be dropped, but aliases
     inside the type (e.g. in fields) can remain. *)
  let ty = T.remove_outermost_alias (DA.typing_env dacc) arg_ty in
  let dacc = DA.add_variable dacc result_var ty in
  SPR.create original_term ~try_reify:false dacc

let simplify_obj_dup dbg dacc ~original_term ~arg ~arg_ty ~result_var =
  (* This must respect the semantics of physical equality. *)
  let typing_env = DA.typing_env dacc in
  let[@inline] elide_primitive () =
    let dacc = DA.add_variable dacc result_var arg_ty in
    SPR.create (Named.create_simple arg) ~try_reify:true dacc
  in
  (* CR mshinwell: We could consider extending this to handle normal blocks in
     addition to boxed numbers. *)
  match T.prove_is_a_boxed_or_tagged_number typing_env arg_ty with
  | Proved (Tagged_immediate | Boxed (Heap, _, _)) -> elide_primitive ()
  | Proved (Boxed ((Heap_or_local | Local), boxable_number, contents_ty)) ->
    let extra_bindings, contents, contents_ty, dacc =
      match
        TE.get_alias_then_canonical_simple_exn ~min_name_mode:NM.normal
          typing_env contents_ty
      with
      | exception Not_found ->
        (* Add a projection so we have a variable bound to the contents of the
           boxed value. This means that when the contents are used directly,
           e.g. after unboxing of the boxed value, the duplicated block itself
           can become unused. This might have the effect of moving a projection
           earlier in the event that it already exists later, but this is
           probably fine: this operation isn't that common. *)
        let contents_var = Variable.create "obj_dup_contents" in
        let contents_expr =
          Named.create_prim (Unary (Unbox_number boxable_number, arg)) dbg
        in
        let bind_contents =
          { Expr_builder.let_bound =
              Bound_pattern.singleton (Bound_var.create contents_var NM.normal);
            simplified_defining_expr = Simplified_named.create contents_expr;
            original_defining_expr = None
          }
        in
        let contents_simple = Simple.var contents_var in
        let dacc =
          DA.add_variable dacc
            (Bound_var.create contents_var NM.normal)
            contents_ty
        in
        ( [bind_contents],
          contents_simple,
          T.alias_type_of (T.kind contents_ty) contents_simple,
          dacc )
      | contents -> [], contents, contents_ty, dacc
    in
    let boxer =
      match boxable_number with
      | Naked_float -> T.box_float
      | Naked_int32 -> T.box_int32
      | Naked_int64 -> T.box_int64
      | Naked_nativeint -> T.box_nativeint
      | Naked_vec128 -> T.box_vec128
    in
    let ty = boxer contents_ty Alloc_mode.For_types.heap in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create ~extra_bindings
      (Named.create_prim
         (Unary
            ( Box_number (boxable_number, Alloc_mode.For_allocations.heap),
              contents ))
         dbg)
      ~try_reify:true dacc
  | Unknown -> (
    match T.prove_strings typing_env arg_ty with
    | Proved (Heap, _) -> elide_primitive ()
    | Proved ((Heap_or_local | Local), _) | Unknown ->
      SPR.create_unknown dacc ~result_var K.value ~original_term)

let simplify_get_header ~original_prim dacc ~original_term ~arg:_ ~arg_ty:_
    ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_atomic_load
    (_block_access_field_kind : P.Block_access_field_kind.t) ~original_prim dacc
    ~original_term ~arg:_ ~arg_ty:_ ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

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
    | Is_int { variant_only } -> simplify_is_int ~variant_only
    | Get_tag -> simplify_get_tag
    | Array_length array_kind -> simplify_array_length array_kind
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
    | Int_as_pointer mode -> simplify_int_as_pointer ~mode
    | Bigarray_length { dimension } -> simplify_bigarray_length ~dimension
    | Duplicate_array { kind; source_mutability; destination_mutability } ->
      simplify_duplicate_array ~kind ~source_mutability ~destination_mutability
    | Duplicate_block { kind } -> simplify_duplicate_block ~kind
    | Opaque_identity { middle_end_only = _; kind } ->
      simplify_opaque_identity ~kind
    | End_region -> simplify_end_region
    | End_try_region -> simplify_end_try_region
    | Obj_dup -> simplify_obj_dup dbg
    | Get_header -> simplify_get_header ~original_prim
    | Atomic_load block_access_field_kind ->
      simplify_atomic_load block_access_field_kind ~original_prim
  in
  simplifier dacc ~original_term ~arg ~arg_ty ~result_var
