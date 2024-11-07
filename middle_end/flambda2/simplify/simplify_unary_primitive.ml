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
module Float32 = Numeric_types.Float32_by_bit_pattern
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
        else
          let _ty, simple = S.simplify_simple dacc simple ~min_name_mode in
          simple
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
    | Naked_float32 ->
      ( T.boxed_float32_alias_to ~naked_float32:result_var'
          (Alloc_mode.For_types.unknown ()),
        K.naked_float32 )
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
    | Naked_float32 -> T.box_float32 naked_number_ty alloc_mode
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
        | Naked_float32 ->
          let module M = For_kind [@inlined hint] (struct
            module Result_num = Float32

            let num_to_result_num = Num.to_naked_float32

            let these = T.these_naked_float32s
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
module Simplify_int_conv_naked_float32 = Make_simplify_int_conv (A.For_float32s)
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

module Make_simplify_reinterpret_64_bit_word (P : sig
  module Src : Container_types.S

  module Dst : Container_types.S

  val prover : TE.t -> T.t -> Src.Set.t meet_shortcut

  val convert : Src.t -> Dst.t

  val these : Dst.Set.t -> T.t

  val any_dst : T.t
end) =
struct
  let simplify dacc ~original_term ~arg:_ ~arg_ty ~result_var =
    let typing_env = DE.typing_env (DA.denv dacc) in
    let proof = P.prover typing_env arg_ty in
    match proof with
    | Known_result src ->
      let dst =
        P.Src.Set.fold
          (fun src dst -> P.Dst.Set.add (P.convert src) dst)
          src P.Dst.Set.empty
      in
      let ty = P.these dst in
      let dacc = DA.add_variable dacc result_var ty in
      SPR.create original_term ~try_reify:true dacc
    | Need_meet ->
      let dacc = DA.add_variable dacc result_var P.any_dst in
      SPR.create original_term ~try_reify:false dacc
    | Invalid -> SPR.create_invalid dacc
end

module Simplify_reinterpret_unboxed_int64_as_unboxed_float64 =
Make_simplify_reinterpret_64_bit_word (struct
  module Src = Int64
  module Dst = Float

  let prover = T.meet_naked_int64s

  let convert = Float.of_bits

  let these = T.these_naked_floats

  let any_dst = T.any_naked_float
end)

module Simplify_reinterpret_unboxed_float64_as_unboxed_int64 =
Make_simplify_reinterpret_64_bit_word (struct
  module Src = Float
  module Dst = Int64

  let prover = T.meet_naked_floats

  let convert = Float.to_bits

  let these = T.these_naked_int64s

  let any_dst = T.any_naked_int64
end)

module Simplify_reinterpret_unboxed_int64_as_tagged_int63 =
Make_simplify_reinterpret_64_bit_word (struct
  module Src = Int64
  module Dst = Targetint_31_63

  let prover = T.meet_naked_int64s

  (* This primitive is logical OR with 1 on machine words, but here, we are
     working in the tagged world. As such a different computation is
     required. *)
  let convert i = Targetint_31_63.of_int64 (Int64.shift_right_logical i 1)

  let these = T.these_tagged_immediates

  let any_dst = T.any_tagged_immediate
end)

module Simplify_reinterpret_tagged_int63_as_unboxed_int64 =
Make_simplify_reinterpret_64_bit_word (struct
  module Src = Targetint_31_63
  module Dst = Int64

  let prover = T.meet_equals_tagged_immediates

  (* This primitive is the identity on machine words, but as above, we are
     working in the tagged world. *)
  let convert i = Int64.add (Int64.mul (Targetint_31_63.to_int64 i) 2L) 1L

  let these = T.these_naked_int64s

  let any_dst = T.any_naked_int64
end)

let simplify_reinterpret_64_bit_word (reinterpret : P.Reinterpret_64_bit_word.t)
    dacc ~original_term ~arg ~arg_ty ~result_var =
  match reinterpret with
  | Unboxed_int64_as_unboxed_float64 ->
    Simplify_reinterpret_unboxed_int64_as_unboxed_float64.simplify dacc
      ~original_term ~arg ~arg_ty ~result_var
  | Unboxed_float64_as_unboxed_int64 ->
    Simplify_reinterpret_unboxed_float64_as_unboxed_int64.simplify dacc
      ~original_term ~arg ~arg_ty ~result_var
  | Unboxed_int64_as_tagged_int63 ->
    Simplify_reinterpret_unboxed_int64_as_tagged_int63.simplify dacc
      ~original_term ~arg ~arg_ty ~result_var
  | Tagged_int63_as_unboxed_int64 ->
    Simplify_reinterpret_tagged_int63_as_unboxed_int64.simplify dacc
      ~original_term ~arg ~arg_ty ~result_var

module Make_simplify_float_arith_op (FP : sig
  module F : Numeric_types.Float_by_bit_pattern

  val meet : T.Typing_env.t -> T.t -> F.Set.t T.meet_shortcut

  val these : F.Set.t -> T.t

  val kind : K.t
end) =
struct
  let simplify (op : P.unary_float_arith_op) dacc ~original_term ~arg:_ ~arg_ty
      ~result_var =
    let module F = FP.F in
    let denv = DA.denv dacc in
    let proof = FP.meet (DE.typing_env denv) arg_ty in
    match proof with
    | Known_result fs when DE.propagating_float_consts denv ->
      assert (not (F.Set.is_empty fs));
      let f =
        match op with
        | Abs -> F.IEEE_semantics.abs
        | Neg -> F.IEEE_semantics.neg
      in
      let possible_results = F.Set.map f fs in
      let ty = FP.these possible_results in
      let dacc = DA.add_variable dacc result_var ty in
      SPR.create original_term ~try_reify:true dacc
    | Known_result _ | Need_meet ->
      SPR.create_unknown dacc ~result_var FP.kind ~original_term
    | Invalid -> SPR.create_invalid dacc
end

module Simplify_float_arith_op = Make_simplify_float_arith_op (struct
  module F = Float

  let meet = T.meet_naked_floats

  let these = T.these_naked_floats

  let kind = K.naked_float
end)

module Simplify_float32_arith_op = Make_simplify_float_arith_op (struct
  module F = Float32

  let meet = T.meet_naked_float32s

  let these = T.these_naked_float32s

  let kind = K.naked_float32
end)

let simplify_float_arith_op = Simplify_float_arith_op.simplify

let simplify_float32_arith_op = Simplify_float32_arith_op.simplify

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
    match T.meet_is_array (DA.typing_env dacc) arg_ty with
    | Invalid -> SPR.create_invalid dacc
    | Need_meet
    | Known_result
        { element_kind = _;
          length = _;
          contents = Known Mutable | Unknown;
          alloc_mode = _
        } ->
      let dacc = DA.add_variable dacc result_var T.any_value in
      SPR.create original_term ~try_reify:false dacc
    | Known_result
        { element_kind;
          length = _;
          contents = Known (Immutable { fields });
          alloc_mode
        } ->
      let length =
        T.this_tagged_immediate (Array.length fields |> Targetint_31_63.of_int)
      in
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
      | Naked_float32 -> T.box_float32
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

let[@inline always] simplify_immutable_block_load0
    (access_kind : P.Block_access_kind.t) ~field ~min_name_mode dacc
    ~original_term _dbg ~arg:block ~arg_ty:block_ty ~result_var =
  let result_kind = P.Block_access_kind.element_kind_for_load access_kind in
  let result_var' = Bound_var.var result_var in
  let typing_env = DA.typing_env dacc in
  match
    T.meet_block_field_simple typing_env ~min_name_mode ~field_kind:result_kind
      block_ty field
  with
  | Invalid -> SPR.create_invalid dacc
  | Known_result simple ->
    let dacc =
      DA.add_variable dacc result_var (T.alias_type_of result_kind simple)
    in
    SPR.create (Named.create_simple simple) ~try_reify:false dacc
  | Need_meet -> (
    let n = Targetint_31_63.add field Targetint_31_63.one in
    (* CR-someday mshinwell: We should be able to use the size in the
       [access_kind] to constrain the type of the block *)
    let tag, shape =
      match access_kind with
      | Values { tag; _ } ->
        ( Or_unknown.map tag ~f:Tag.Scannable.to_tag,
          K.Block_shape.Scannable Value_only )
      | Naked_floats { size } ->
        ( (match size with
          | Known size ->
            (* We don't expect blocks of naked floats of size zero (it doesn't
               seem that the frontend currently emits code to create such
               blocks) and so it isn't clear whether such blocks should have tag
               zero (like zero-sized naked float arrays) or another tag. *)
            if Targetint_31_63.equal size Targetint_31_63.zero
            then Or_unknown.Unknown
            else Or_unknown.Known Tag.double_array_tag
          | Unknown -> Or_unknown.Unknown),
          K.Block_shape.Float_record )
      | Mixed { tag; size = _; field_kind = _; shape } ->
        ( Or_unknown.map tag ~f:Tag.Scannable.to_tag,
          K.Block_shape.Scannable (Mixed_record shape) )
    in
    let result =
      Simplify_common.simplify_projection dacc ~original_term
        ~deconstructing:block_ty
        ~shape:
          (T.immutable_block_with_size_at_least ~tag ~n ~shape
             ~field_n_minus_one:result_var')
        ~result_var ~result_kind
    in
    match result.simplified_named with
    | Invalid -> result
    | Ok _ -> (
      (* If the type contains enough information to actually build a primitive
         to make the corresponding block, then we add a CSE equation, to try to
         avoid duplicate allocations in the future. This should help with cases
         such as "Some x -> Some x". *)
      let dacc = result.dacc in
      match
        T.prove_unique_fully_constructed_immutable_heap_block
          (DA.typing_env dacc) block_ty
      with
      | Unknown -> result
      | Proved (tag, shape_from_type, _size, field_simples) -> (
        match Tag.Scannable.of_tag tag with
        | None -> result
        | Some tag -> (
          let block_kind : P.Block_kind.t =
            match access_kind with
            | Values _ ->
              let arity =
                List.map (fun _ -> K.With_subkind.any_value) field_simples
              in
              Values (tag, arity)
            | Naked_floats _ -> Naked_floats
            | Mixed { shape; _ } ->
              (match shape_from_type with
              | Scannable (Mixed_record shape_from_type)
                when K.Mixed_block_shape.equal shape shape_from_type ->
                ()
              | Scannable Value_only | Float_record | Scannable (Mixed_record _)
                ->
                Misc.fatal_error
                  "Block access kind disagrees with block shape from type");
              Mixed (tag, shape)
          in
          let prim =
            P.Eligible_for_cse.create
              (Variadic
                 ( Make_block
                     (block_kind, Immutable, Alloc_mode.For_allocations.heap),
                   field_simples ))
          in
          match prim with
          | None -> result
          | Some prim ->
            let dacc =
              DA.map_denv dacc ~f:(fun denv ->
                  DE.add_cse denv prim ~bound_to:block)
            in
            SPR.with_dacc result dacc))))

let simplify_immutable_block_load access_kind ~field ~min_name_mode dacc
    ~original_term ~dbg ~arg ~arg_ty ~result_var =
  let result =
    simplify_immutable_block_load0 access_kind ~field ~min_name_mode dacc
      ~original_term dbg ~arg ~arg_ty ~result_var
  in
  let dacc' =
    let kind = P.Block_access_kind.element_subkind_for_load access_kind in
    Simplify_common.add_symbol_projection result.dacc ~projected_from:arg
      (Symbol_projection.Projection.block_load ~index:field)
      ~projection_bound_to:result_var ~kind
  in
  SPR.with_dacc result dacc'

let simplify_mutable_block_load _access_kind ~field:_ ~original_prim dacc
    ~original_term ~dbg:_ ~arg ~arg_ty:_ ~result_var =
  if Simple.is_const arg
  then SPR.create_invalid dacc
  else
    SPR.create_unknown dacc ~result_var
      (P.result_kind' original_prim)
      ~original_term

let simplify_unary_primitive dacc original_prim (prim : P.unary_primitive) ~arg
    ~arg_ty dbg ~result_var =
  let min_name_mode = Bound_var.name_mode result_var in
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Block_load { kind; mut = Immutable | Immutable_unique; field } ->
      simplify_immutable_block_load kind ~field ~min_name_mode ~dbg
    | Block_load { kind; mut = Mutable; field } ->
      simplify_mutable_block_load kind ~field ~original_prim ~dbg
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
    | Float_arith (Float64, op) -> simplify_float_arith_op op
    | Float_arith (Float32, op) -> simplify_float32_arith_op op
    | Num_conv { src; dst } -> (
      match src with
      | Tagged_immediate -> Simplify_int_conv_tagged_immediate.simplify ~dst
      | Naked_immediate -> Simplify_int_conv_naked_immediate.simplify ~dst
      | Naked_float32 -> Simplify_int_conv_naked_float32.simplify ~dst
      | Naked_float -> Simplify_int_conv_naked_float.simplify ~dst
      | Naked_int32 -> Simplify_int_conv_naked_int32.simplify ~dst
      | Naked_int64 -> Simplify_int_conv_naked_int64.simplify ~dst
      | Naked_nativeint -> Simplify_int_conv_naked_nativeint.simplify ~dst)
    | Boolean_not -> simplify_boolean_not
    | Reinterpret_64_bit_word reinterpret ->
      simplify_reinterpret_64_bit_word reinterpret
    | Is_boxed_float -> simplify_is_boxed_float
    | Is_flat_float_array -> simplify_is_flat_float_array
    | Int_as_pointer mode -> simplify_int_as_pointer ~mode
    | Bigarray_length { dimension } -> simplify_bigarray_length ~dimension
    | Duplicate_array { kind; source_mutability; destination_mutability } ->
      simplify_duplicate_array ~kind ~source_mutability ~destination_mutability
    | Duplicate_block { kind } -> simplify_duplicate_block ~kind
    | Opaque_identity { middle_end_only = _; kind } ->
      simplify_opaque_identity ~kind
    | End_region { ghost = _ } -> simplify_end_region
    | End_try_region { ghost = _ } -> simplify_end_try_region
    | Obj_dup -> simplify_obj_dup dbg
    | Get_header -> simplify_get_header ~original_prim
    | Atomic_load block_access_field_kind ->
      simplify_atomic_load block_access_field_kind ~original_prim
  in
  simplifier dacc ~original_term ~arg ~arg_ty ~result_var
