(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module K = Flambda_kind
module RWC = Reg_width_const
module TG = Type_grammar

let unknown (kind : K.t) =
  match kind with
  | Value -> TG.any_value
  | Naked_number Naked_immediate -> TG.any_naked_immediate
  | Naked_number Naked_float32 -> TG.any_naked_float32
  | Naked_number Naked_float -> TG.any_naked_float
  | Naked_number Naked_int32 -> TG.any_naked_int32
  | Naked_number Naked_int64 -> TG.any_naked_int64
  | Naked_number Naked_nativeint -> TG.any_naked_nativeint
  | Naked_number Naked_vec128 -> TG.any_naked_vec128
  | Rec_info -> TG.any_rec_info
  | Region -> TG.any_region

let unknown_like t = unknown (TG.kind t)

let unknown_from_shape (shape : K.Block_shape.t) index =
  unknown (K.Block_shape.element_kind shape index)

let bottom (kind : K.t) =
  match kind with
  | Value -> TG.bottom_value
  | Naked_number Naked_immediate -> TG.bottom_naked_immediate
  | Naked_number Naked_float32 -> TG.bottom_naked_float32
  | Naked_number Naked_float -> TG.bottom_naked_float
  | Naked_number Naked_int32 -> TG.bottom_naked_int32
  | Naked_number Naked_int64 -> TG.bottom_naked_int64
  | Naked_number Naked_nativeint -> TG.bottom_naked_nativeint
  | Naked_number Naked_vec128 -> TG.bottom_naked_vec128
  | Rec_info -> TG.bottom_rec_info
  | Region -> TG.bottom_region

let bottom_like t = bottom (TG.kind t)

let these_naked_immediates is = TG.these_naked_immediates is

let these_naked_float32s fs = TG.these_naked_float32s fs

let these_naked_floats fs = TG.these_naked_floats fs

let these_naked_int32s is = TG.these_naked_int32s is

let these_naked_int64s is = TG.these_naked_int64s is

let these_naked_nativeints is = TG.these_naked_nativeints is

let these_naked_vec128s vs = TG.these_naked_vec128s vs

let any_tagged_immediate =
  TG.create_variant ~is_unique:false ~immediates:Unknown
    ~blocks:(Known TG.Row_like_for_blocks.bottom)

let these_tagged_immediates0 imms =
  match Targetint_31_63.Set.get_singleton imms with
  | Some imm -> TG.this_tagged_immediate imm
  | _ ->
    if Targetint_31_63.Set.is_empty imms
    then TG.bottom_value
    else
      TG.create_variant ~is_unique:false
        ~immediates:(Known (these_naked_immediates imms))
        ~blocks:(Known TG.Row_like_for_blocks.bottom)

let these_tagged_immediates imms = these_tagged_immediates0 imms

let any_tagged_bool = these_tagged_immediates Targetint_31_63.all_bools

let any_naked_bool = TG.these_naked_immediates Targetint_31_63.all_bools

let this_boxed_float32 f alloc_mode =
  TG.box_float32 (TG.this_naked_float32 f) alloc_mode

let this_boxed_float f alloc_mode =
  TG.box_float (TG.this_naked_float f) alloc_mode

let this_boxed_int32 i alloc_mode =
  TG.box_int32 (TG.this_naked_int32 i) alloc_mode

let this_boxed_int64 i alloc_mode =
  TG.box_int64 (TG.this_naked_int64 i) alloc_mode

let this_boxed_nativeint i alloc_mode =
  TG.box_nativeint (TG.this_naked_nativeint i) alloc_mode

let this_boxed_vec128 i alloc_mode =
  TG.box_vec128 (TG.this_naked_vec128 i) alloc_mode

let these_boxed_float32s fs alloc_mode =
  TG.box_float32 (these_naked_float32s fs) alloc_mode

let these_boxed_floats fs alloc_mode =
  TG.box_float (these_naked_floats fs) alloc_mode

let these_boxed_int32s is alloc_mode =
  TG.box_int32 (these_naked_int32s is) alloc_mode

let these_boxed_int64s is alloc_mode =
  TG.box_int64 (these_naked_int64s is) alloc_mode

let these_boxed_nativeints is alloc_mode =
  TG.box_nativeint (these_naked_nativeints is) alloc_mode

let any_boxed_float32 =
  TG.box_float32 TG.any_naked_float32 (Alloc_mode.For_types.unknown ())

let any_boxed_float =
  TG.box_float TG.any_naked_float (Alloc_mode.For_types.unknown ())

let any_boxed_int32 =
  TG.box_int32 TG.any_naked_int32 (Alloc_mode.For_types.unknown ())

let any_boxed_int64 =
  TG.box_int64 TG.any_naked_int64 (Alloc_mode.For_types.unknown ())

let any_boxed_nativeint =
  TG.box_nativeint TG.any_naked_nativeint (Alloc_mode.For_types.unknown ())

let any_boxed_vec128 =
  TG.box_vec128 TG.any_naked_vec128 (Alloc_mode.For_types.unknown ())

let any_block =
  TG.create_variant ~is_unique:false
    ~immediates:(Known TG.bottom_naked_immediate) ~blocks:Unknown

let blocks_with_these_tags tags alloc_mode : _ Or_unknown.t =
  if not (Tag.Set.for_all Tag.is_structured_block tags)
  then Unknown
  else
    let tags = Tag.Map.of_set (fun _ -> Or_unknown.Unknown) tags in
    let blocks =
      TG.Row_like_for_blocks.create_blocks_with_these_tags tags alloc_mode
    in
    Known
      (TG.create_variant ~is_unique:false
         ~immediates:(Known TG.bottom_naked_immediate) ~blocks:(Known blocks))

let immutable_block ~is_unique tag ~shape alloc_mode ~fields =
  match Targetint_31_63.of_int_option (List.length fields) with
  | None ->
    (* CR-someday mshinwell: This should be a special kind of error. *)
    Misc.fatal_error "Block too long for target"
  | Some _size ->
    TG.create_variant ~is_unique ~immediates:(Known TG.bottom_naked_immediate)
      ~blocks:
        (Known
           (TG.Row_like_for_blocks.create ~shape ~field_tys:fields (Closed tag)
              alloc_mode))

let immutable_block_with_size_at_least ~tag ~n ~shape ~field_n_minus_one =
  let n = Targetint_31_63.to_int n in
  let field_tys =
    List.init n (fun index ->
        let field_kind = K.Block_shape.element_kind shape index in
        if index < n - 1
        then unknown field_kind
        else TG.alias_type_of field_kind (Simple.var field_n_minus_one))
  in
  TG.create_variant ~is_unique:false
    ~immediates:(Known (bottom K.naked_immediate))
    ~blocks:
      (Known
         (TG.Row_like_for_blocks.create ~shape ~field_tys (Open tag)
            (Alloc_mode.For_types.unknown ())))

let variant ~const_ctors ~non_const_ctors alloc_mode =
  let blocks =
    let shape_and_field_tys_by_tag =
      Tag.Scannable.Map.fold
        (fun tag ty non_const_ctors ->
          Tag.Map.add (Tag.Scannable.to_tag tag) ty non_const_ctors)
        non_const_ctors Tag.Map.empty
    in
    TG.Row_like_for_blocks.create_exactly_multiple ~shape_and_field_tys_by_tag
      alloc_mode
  in
  TG.create_variant ~is_unique:false ~immediates:(Known const_ctors)
    ~blocks:(Known blocks)

let exactly_this_closure function_slot ~all_function_slots_in_set:function_types
    ~all_closure_types_in_set:closure_types
    ~all_value_slots_in_set:value_slot_types alloc_mode =
  let closure_types = TG.Product.Function_slot_indexed.create closure_types in
  let closures_entry =
    let value_slot_types =
      TG.Product.Value_slot_indexed.create value_slot_types
    in
    TG.Closures_entry.create ~function_types ~closure_types ~value_slot_types
  in
  let by_function_slot =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Function_slot.Map.keys function_types)
        (Value_slot.Map.keys value_slot_types)
    in
    TG.Row_like_for_closures.create_exactly function_slot
      set_of_closures_contents closures_entry
  in
  TG.create_closures alloc_mode by_function_slot

let closure_with_at_least_these_function_slots ~this_function_slot
    function_slots_and_bindings =
  let function_slot_components_by_index =
    Function_slot.Map.map
      (fun bound_to -> TG.alias_type_of K.value bound_to)
      function_slots_and_bindings
  in
  let function_types =
    Function_slot.Map.map
      (fun _ -> Or_unknown_or_bottom.Unknown)
      function_slots_and_bindings
  in
  let closure_types =
    TG.Product.Function_slot_indexed.create function_slot_components_by_index
  in
  let closures_entry =
    TG.Closures_entry.create ~function_types ~closure_types
      ~value_slot_types:TG.Product.Value_slot_indexed.top
  in
  let by_function_slot =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Function_slot.Map.keys function_slot_components_by_index)
        Value_slot.Set.empty
    in
    TG.Row_like_for_closures.create_at_least this_function_slot
      set_of_closures_contents closures_entry
  in
  TG.create_closures (Alloc_mode.For_types.unknown ()) by_function_slot

let closure_with_at_least_these_value_slots ~this_function_slot value_slots =
  let value_slot_types =
    let type_of_var (v, kind) =
      TG.alias_type_of (K.With_subkind.kind kind) (Simple.var v)
    in
    let value_slot_components_by_index =
      Value_slot.Map.map type_of_var value_slots
    in
    TG.Product.Value_slot_indexed.create value_slot_components_by_index
  in
  let closures_entry =
    TG.Closures_entry.create ~function_types:Function_slot.Map.empty
      ~closure_types:TG.Product.Function_slot_indexed.top ~value_slot_types
  in
  let by_function_slot =
    let set_of_closures_contents =
      Set_of_closures_contents.create Function_slot.Set.empty
        (Value_slot.Map.keys value_slots)
    in
    TG.Row_like_for_closures.create_at_least this_function_slot
      set_of_closures_contents closures_entry
  in
  TG.create_closures (Alloc_mode.For_types.unknown ()) by_function_slot

let closure_with_at_least_this_value_slot ~this_function_slot value_slot
    ~value_slot_var ~value_slot_kind =
  closure_with_at_least_these_value_slots ~this_function_slot
    (Value_slot.Map.singleton value_slot (value_slot_var, value_slot_kind))

let type_for_const const =
  match RWC.descr const with
  | Naked_immediate i -> TG.this_naked_immediate i
  | Tagged_immediate i -> TG.this_tagged_immediate i
  | Naked_float32 f -> TG.this_naked_float32 f
  | Naked_float f -> TG.this_naked_float f
  | Naked_int32 n -> TG.this_naked_int32 n
  | Naked_int64 n -> TG.this_naked_int64 n
  | Naked_nativeint n -> TG.this_naked_nativeint n
  | Naked_vec128 n -> TG.this_naked_vec128 n

let kind_for_const const = TG.kind (type_for_const const)

let is_alias_of_name ty name =
  match TG.get_alias_exn ty with
  | exception Not_found -> false
  | simple ->
    Simple.pattern_match simple
      ~name:(fun name' ~coercion:_ -> Name.equal name name')
      ~const:(fun _ -> false)

let check_equation name ty =
  if Flambda_features.check_invariants ()
  then
    if is_alias_of_name ty name
    then
      Misc.fatal_errorf "Directly recursive equation@ %a = %a@ disallowed"
        Name.print name TG.print ty

let arity_of_list ts =
  Flambda_arity.create_singletons
    (List.map (fun ty -> Flambda_kind.With_subkind.anything (TG.kind ty)) ts)

let rec unknown_with_subkind ?(alloc_mode = Alloc_mode.For_types.unknown ())
    (kind : Flambda_kind.With_subkind.t) =
  (* CR mshinwell: use [alloc_mode] more *)
  match Flambda_kind.With_subkind.subkind kind with
  | Anything -> (
    match Flambda_kind.With_subkind.kind kind with
    | Value -> TG.any_value
    | Naked_number Naked_immediate -> TG.any_naked_immediate
    | Naked_number Naked_float32 -> TG.any_naked_float32
    | Naked_number Naked_float -> TG.any_naked_float
    | Naked_number Naked_int32 -> TG.any_naked_int32
    | Naked_number Naked_int64 -> TG.any_naked_int64
    | Naked_number Naked_nativeint -> TG.any_naked_nativeint
    | Naked_number Naked_vec128 -> TG.any_naked_vec128
    | Rec_info -> TG.any_rec_info
    | Region -> TG.any_region)
  | Boxed_float -> any_boxed_float
  | Boxed_float32 -> any_boxed_float32
  | Boxed_int32 -> any_boxed_int32
  | Boxed_int64 -> any_boxed_int64
  | Boxed_nativeint -> any_boxed_nativeint
  | Boxed_vec128 -> any_boxed_vec128
  | Tagged_immediate -> any_tagged_immediate
  | Variant { consts; non_consts } ->
    let const_ctors = these_naked_immediates consts in
    let non_const_ctors =
      Tag.Scannable.Map.map
        (fun (shape, fields) ->
          shape, List.map (fun subkind -> unknown_with_subkind subkind) fields)
        non_consts
    in
    variant ~const_ctors ~non_const_ctors alloc_mode
  | Float_block { num_fields } ->
    immutable_block ~is_unique:false Tag.double_array_tag
      ~shape:Flambda_kind.Block_shape.Float_record
      ~fields:(List.init num_fields (fun _ -> TG.any_naked_float))
      alloc_mode
  | Float_array ->
    TG.mutable_array ~element_kind:(Ok Flambda_kind.With_subkind.naked_float)
      ~length:any_tagged_immediate alloc_mode
  | Unboxed_float32_array ->
    TG.mutable_array ~element_kind:(Ok Flambda_kind.With_subkind.naked_float32)
      ~length:any_tagged_immediate alloc_mode
  | Unboxed_int32_array ->
    TG.mutable_array ~element_kind:(Ok Flambda_kind.With_subkind.naked_int32)
      ~length:any_tagged_immediate alloc_mode
  | Unboxed_int64_array ->
    TG.mutable_array ~element_kind:(Ok Flambda_kind.With_subkind.naked_int64)
      ~length:any_tagged_immediate alloc_mode
  | Unboxed_nativeint_array ->
    TG.mutable_array
      ~element_kind:(Ok Flambda_kind.With_subkind.naked_nativeint)
      ~length:any_tagged_immediate alloc_mode
  | Immediate_array ->
    TG.mutable_array
      ~element_kind:(Ok Flambda_kind.With_subkind.tagged_immediate)
      ~length:any_tagged_immediate alloc_mode
  | Value_array ->
    TG.mutable_array ~element_kind:(Ok Flambda_kind.With_subkind.any_value)
      ~length:any_tagged_immediate alloc_mode
  | Generic_array ->
    TG.mutable_array ~element_kind:Unknown ~length:any_tagged_immediate
      alloc_mode
  | Null -> TG.the_null

let unknown_types_from_arity arity =
  List.map
    (unknown_with_subkind ?alloc_mode:None)
    (Flambda_arity.unarized_components arity)
