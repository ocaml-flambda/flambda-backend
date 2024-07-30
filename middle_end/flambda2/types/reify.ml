(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Float32 = Numeric_types.Float32_by_bit_pattern
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module TE = Typing_env
module TG = Type_grammar

type to_lift =
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        shape : Flambda_kind.Scannable_block_shape.t;
        fields : Simple.t list
      }
  | Boxed_float32 of Float32.t
  | Boxed_float of Float.t
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint_32_64.t
  | Boxed_vec128 of Vector_types.Vec128.Bit_pattern.t
  | Immutable_float32_array of { fields : Float32.t list }
  | Immutable_float_array of { fields : Float.t list }
  | Immutable_int32_array of { fields : Int32.t list }
  | Immutable_int64_array of { fields : Int64.t list }
  | Immutable_nativeint_array of { fields : Targetint_32_64.t list }
  | Immutable_value_array of { fields : Simple.t list }
  | Empty_array of Empty_array_kind.t

type reification_result =
  | Lift of to_lift
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

let try_to_reify_fields env ~var_allowed alloc_mode
    ~field_types_and_expected_kinds =
  let field_simples =
    List.filter_map
      (fun (field_type, field_kind) : Simple.t option ->
        match
          Provers.prove_equals_to_simple_of_kind env field_type field_kind
        with
        | Proved simple when not (Coercion.is_id (Simple.coercion simple)) ->
          (* CR-someday lmaurer: Support lifting things whose fields have
             coercions. *)
          None
        | Proved simple ->
          Simple.pattern_match' simple
            ~var:(fun var ~coercion:_ ->
              if var_allowed alloc_mode var then Some simple else None)
            ~symbol:(fun _sym ~coercion:_ -> Some simple)
            ~const:(fun const ->
              match Reg_width_const.descr const with
              | Tagged_immediate _imm -> Some simple
              | Naked_immediate _ | Naked_float _ | Naked_float32 _
              | Naked_int32 _ | Naked_vec128 _ | Naked_int64 _
              | Naked_nativeint _ ->
                (* This should never happen, as we should have got a kind error
                   instead *)
                None)
        | Unknown -> None)
      field_types_and_expected_kinds
  in
  if List.compare_lengths field_types_and_expected_kinds field_simples = 0
  then Some field_simples
  else None

module Make_lift_array_of_naked_numbers (P : sig
  module N : Container_types.S

  val prove : TE.t -> TG.t -> N.Set.t Provers.meet_shortcut

  val build_to_lift : fields:N.t list -> to_lift
end) =
struct
  open P

  let lift env ~fields ~try_canonical_simple =
    let fields_rev =
      List.fold_left
        (fun (fields_rev : _ Or_unknown_or_bottom.t) field :
             _ Or_unknown_or_bottom.t ->
          match fields_rev with
          | Unknown | Bottom -> fields_rev
          | Ok fields_rev -> (
            match prove env field with
            | Need_meet -> Unknown
            | Invalid -> Bottom
            | Known_result fs -> (
              match N.Set.get_singleton fs with
              | None -> Unknown
              | Some f -> Ok (f :: fields_rev))))
        (Or_unknown_or_bottom.Ok []) fields
    in
    match fields_rev with
    | Unknown -> try_canonical_simple ()
    | Bottom -> Invalid
    | Ok fields_rev -> Lift (build_to_lift ~fields:(List.rev fields_rev))
end

module Lift_array_of_naked_float32s = Make_lift_array_of_naked_numbers (struct
  module N = Float32

  let prove = Provers.meet_naked_float32s

  let build_to_lift ~fields = Immutable_float32_array { fields }
end)

module Lift_array_of_naked_floats = Make_lift_array_of_naked_numbers (struct
  module N = Float

  let prove = Provers.meet_naked_floats

  let build_to_lift ~fields = Immutable_float_array { fields }
end)

module Lift_array_of_naked_int32s = Make_lift_array_of_naked_numbers (struct
  module N = Int32

  let prove = Provers.meet_naked_int32s

  let build_to_lift ~fields = Immutable_int32_array { fields }
end)

module Lift_array_of_naked_int64s = Make_lift_array_of_naked_numbers (struct
  module N = Int64

  let prove = Provers.meet_naked_int64s

  let build_to_lift ~fields = Immutable_int64_array { fields }
end)

module Lift_array_of_naked_nativeints = Make_lift_array_of_naked_numbers (struct
  module N = Targetint_32_64

  let prove = Provers.meet_naked_nativeints

  let build_to_lift ~fields = Immutable_nativeint_array { fields }
end)

(* CR mshinwell: Think more to identify all the cases that should be in this
   function. *)
let reify ~allowed_if_free_vars_defined_in ~var_is_defined_at_toplevel
    ~var_is_symbol_projection env t : reification_result =
  let min_name_mode = Name_mode.normal in
  let var_allowed (alloc_mode : Alloc_mode.For_types.t) var =
    (* It is only safe to lift a [Local] allocation if it can be guaranteed that
       no locally-allocated value is reachable from it: therefore, any variables
       involved in the definition of an (inconstant) value to be lifted have
       their types checked to ensure they cannot hold locally-allocated values.
       Conversely, [Heap] allocations can be lifted even if inconstant, because
       the OCaml type system will have validated the correctness of the original
       non-lifted terms; any places in the compiler where new [Local] blocks are
       created (e.g. during partial application wrapper expansion) will have
       been checked to ensure they do not break the invariants; and finally
       because the Flambda 2 type system accurately propagates the allocation
       modes (and if it loses information there, we won't lift).

       Also see comment in [Simplify_set_of_closures.
       type_value_slots_and_make_lifting_decision_for_one_set]. *)
    TE.mem ~min_name_mode allowed_if_free_vars_defined_in (Name.var var)
    && (var_is_symbol_projection var
       || var_is_defined_at_toplevel var
          &&
          match alloc_mode with
          | Heap -> true
          | Heap_or_local | Local -> (
            match Provers.never_holds_locally_allocated_values env var with
            | Proved () -> true
            | Unknown -> false))
  in
  let canonical_simple =
    match TE.get_alias_then_canonical_simple_exn env ~min_name_mode t with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  match canonical_simple with
  | Some canonical_simple when not (Simple.is_var canonical_simple) ->
    (* Don't lift things that are already bound to symbols. Apart from anything
       else, this could cause aliases between symbols, which are currently
       forbidden (every symbol has the same binding time). *)
    Simple canonical_simple
  | canonical_simple_opt -> (
    let try_canonical_simple () =
      match canonical_simple_opt with
      | None -> Cannot_reify
      | Some canonical_simple -> Simple canonical_simple
    in
    match
      Expand_head.expand_head env t |> Expand_head.Expanded_type.descr_oub
    with
    | Value (Ok (Variant { is_unique; blocks; immediates })) -> (
      match blocks, immediates with
      | Known blocks, Known imms ->
        if Expand_head.is_bottom env imms
        then
          match TG.Row_like_for_blocks.get_singleton blocks with
          | None ->
            (* CR mshinwell: could recognise tagged immediates *)
            try_canonical_simple ()
          | Some (tag, shape, size, field_types, alloc_mode) -> (
            assert (
              Targetint_31_63.equal size
                (TG.Product.Int_indexed.width field_types));
            let field_types = TG.Product.Int_indexed.components field_types in
            let field_types_and_expected_kinds =
              match shape with
              | Float_record ->
                List.map (fun ty -> ty, Flambda_kind.naked_float) field_types
              | Scannable (Mixed_record shape) ->
                Flambda_kind.Mixed_block_shape.field_kinds shape
                |> Array.to_list |> List.combine field_types
              | Scannable Value_only ->
                List.map (fun ty -> ty, Flambda_kind.value) field_types
            in
            match shape with
            | Float_record ->
              (* CR mshinwell: lift these and also support arrays (below) with
                 [Simple]s not just numbers in them *)
              try_canonical_simple ()
            | Scannable shape -> (
              let tag =
                match Tag.Scannable.of_tag tag with
                | Some tag -> tag
                | None ->
                  Misc.fatal_errorf
                    "Value-only or mixed block type has tag %a, which is not a \
                     scannable tag"
                    Tag.print tag
              in
              match
                try_to_reify_fields env ~var_allowed alloc_mode
                  ~field_types_and_expected_kinds
              with
              | Some fields ->
                Lift (Immutable_block { tag; is_unique; shape; fields })
              | None -> try_canonical_simple ()))
        else if TG.Row_like_for_blocks.is_bottom blocks
        then
          match Provers.meet_naked_immediates env imms with
          | Known_result imms -> (
            match Targetint_31_63.Set.get_singleton imms with
            | None -> try_canonical_simple ()
            | Some imm ->
              Simple (Simple.const (Reg_width_const.tagged_immediate imm)))
          | Need_meet -> try_canonical_simple ()
          | Invalid -> Invalid
        else try_canonical_simple ()
      | Known _, Unknown | Unknown, Known _ | Unknown, Unknown ->
        try_canonical_simple ())
    | Value (Ok (Mutable_block _)) -> try_canonical_simple ()
    | Value (Ok (Closures { by_function_slot = _; alloc_mode = _ })) ->
      try_canonical_simple ()
      (* CR vlaviron: This rather complicated code could be useful, but since a
         while ago Reification simply ignores List_set_of_closures results. So
         I've commented out the code for now, and if we want to turn it on again
         later we will need to think about issues like code duplication,
         mutually recursive functions, and so on. *)
      (* Example:
       * include (struct
       *   module type T = sig
       *     val n : int
       *   end
       *
       *   module F(T : T) = struct
       *     let f x = x + T.n [@@inline never]
       *   end [@@inline never]
       *
       *   module T = struct let n = 42 end
       *
       *   module A = F(T)
       *
       *   let toto = A.f
       * end :
       * sig
       *   val toto : int -> int
       * end) *)
      (* (\* CR mshinwell: Here and above, move to separate function. *\)
       * match TG.Row_like_for_closures.get_singleton by_function_slot with
       * | None -> try_canonical_simple ()
       * | Some ((function_slot, contents), closures_entry) ->
       *   (\* CR mshinwell: What about if there were multiple entries in the
       *      row-like structure for the same function slot? This is ruled out by
       *      [get_singleton] at the moment. We should probably choose the best
       *      entry from the [Row_like] structure. *\)
       *   let function_slots = Set_of_closures_contents.closures contents in
       *   (\* CR mshinwell: Should probably check
       *      [Set_of_closures_contents.value_slots contents]? *\)
       *   if not (Function_slot.Set.mem function_slot function_slots)
       *   then
       *     Misc.fatal_errorf
       *       "Function slot %a expected in set-of-closures-contents in closure \
       *        type@ %a"
       *       Function_slot.print function_slot TG.print t;
       *   let function_types_with_value_slots =
       *     Function_slot.Set.fold
       *       (fun function_slot function_types_with_value_slots ->
       *         match
       *           TG.Closures_entry.find_function_type closures_entry
       *             function_slot
       *         with
       *         | Bottom | Unknown -> function_types_with_value_slots
       *         | Ok function_type ->
       *           (\* CR mshinwell: We're ignoring [coercion] *\)
       *           let value_slot_types =
       *             TG.Closures_entry.value_slot_types closures_entry
       *           in
       *           let value_slot_simples =
       *             Value_slot.Map.filter_map
       *               (fun _value_slot value_slot_type ->
       *                 match
       *                   Provers
       *                   .prove_equals_to_var_or_symbol_or_tagged_immediate env
       *                     value_slot_type
       *                 with
       *                 | Proved (Var var, coercion) ->
       *                   if var_allowed alloc_mode var
       *                   then
       *                     Some (Simple.with_coercion (Simple.var var) coercion)
       *                   else None
       *                 | Proved (Symbol sym, coercion) ->
       *                   Some (Simple.with_coercion (Simple.symbol sym) coercion)
       *                 | Proved (Tagged_immediate imm, coercion) ->
       *                   Some
       *                     (Simple.with_coercion
       *                        (Simple.const
       *                           (Reg_width_const.tagged_immediate imm))
       *                        coercion)
       *                 | Unknown | Invalid -> None)
       *               value_slot_types
       *           in
       *           if Value_slot.Map.cardinal value_slot_types
       *              <> Value_slot.Map.cardinal value_slot_simples
       *           then function_types_with_value_slots
       *           else
       *             Function_slot.Map.add function_slot
       *               (function_type, value_slot_simples)
       *               function_types_with_value_slots)
       *       function_slots Function_slot.Map.empty
       *   in
       *   if Function_slot.Set.cardinal function_slots
       *      <> Function_slot.Map.cardinal function_types_with_value_slots
       *   then try_canonical_simple ()
       *   else
       *     let function_types =
       *       Function_slot.Map.map
       *         (fun (function_decl, _) -> function_decl)
       *         function_types_with_value_slots
       *     in
       *     let value_slots =
       *       Function_slot.Map.fold
       *         (fun _function_slot (_function_decl, value_slot_simples)
       *              all_value_slots ->
       *           Value_slot.Map.fold
       *             (fun value_slot simple all_value_slots ->
       *               begin
       *                 match Value_slot.Map.find value_slot all_value_slots with
       *                 | exception Not_found -> ()
       *                 | existing_simple ->
       *                   if not (Simple.equal simple existing_simple)
       *                   then
       *                     Misc.fatal_errorf
       *                       "Disagreement on %a and %a (value slot %a)@ whilst \
       *                        reifying set-of-closures from:@ %a"
       *                       Simple.print simple Simple.print existing_simple
       *                       Value_slot.print value_slot TG.print t
       *               end;
       *               Value_slot.Map.add value_slot simple all_value_slots)
       *             value_slot_simples all_value_slots)
       *         function_types_with_value_slots Value_slot.Map.empty
       *     in
       *     Lift_set_of_closures { function_slot; function_types; value_slots } *)
    | Naked_immediate (Ok (Naked_immediates imms)) -> (
      match Targetint_31_63.Set.get_singleton imms with
      | None -> try_canonical_simple ()
      | Some i -> Simple (Simple.const (Reg_width_const.naked_immediate i)))
    | Naked_immediate (Ok (Is_int scrutinee_ty)) -> (
      match Provers.meet_is_int_variant_only env scrutinee_ty with
      | Known_result true -> Simple Simple.untagged_const_true
      | Known_result false -> Simple Simple.untagged_const_false
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid)
    | Naked_immediate (Ok (Get_tag block_ty)) -> (
      match Provers.prove_get_tag env block_ty with
      | Proved tags -> (
        let is =
          Tag.Set.fold
            (fun tag is ->
              Targetint_31_63.Set.add (Tag.to_targetint_31_63 tag) is)
            tags Targetint_31_63.Set.empty
        in
        match Targetint_31_63.Set.get_singleton is with
        | None -> try_canonical_simple ()
        | Some i -> Simple (Simple.const (Reg_width_const.naked_immediate i)))
      | Unknown -> try_canonical_simple ())
    | Naked_float32 (Ok fs) -> (
      match Float32.Set.get_singleton (fs :> Float32.Set.t) with
      | None -> try_canonical_simple ()
      | Some f -> Simple (Simple.const (Reg_width_const.naked_float32 f)))
    | Naked_float (Ok fs) -> (
      match Float.Set.get_singleton (fs :> Float.Set.t) with
      | None -> try_canonical_simple ()
      | Some f -> Simple (Simple.const (Reg_width_const.naked_float f)))
    | Naked_int32 (Ok ns) -> (
      match Int32.Set.get_singleton (ns :> Int32.Set.t) with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int32 n)))
    | Naked_int64 (Ok ns) -> (
      match Int64.Set.get_singleton (ns :> Int64.Set.t) with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int64 n)))
    | Naked_nativeint (Ok ns) -> (
      match Targetint_32_64.Set.get_singleton (ns :> Targetint_32_64.Set.t) with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_nativeint n)))
    | Naked_vec128 (Ok ns) -> (
      match
        Vector_types.Vec128.Bit_pattern.Set.get_singleton
          (ns :> Vector_types.Vec128.Bit_pattern.Set.t)
      with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_vec128 n)))
    (* CR-someday mshinwell: These could lift at toplevel when [ty_naked_float]
       is an alias type. That would require checking the alloc mode. *)
    | Value (Ok (Boxed_float (ty_naked_float, _alloc_mode))) -> (
      match Provers.meet_naked_floats env ty_naked_float with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result fs -> (
        (* CR mshinwell: This (and the other cases below including that for
           immutable float arrays) seem not to be taking advantage of the fact
           that [Static_const] permits variables in the arguments of e.g.
           [Boxed_float]. *)
        match Float.Set.get_singleton fs with
        | None -> try_canonical_simple ()
        | Some f -> Lift (Boxed_float f)))
    | Value (Ok (Boxed_float32 (ty_naked_float32, _alloc_mode))) -> (
      match Provers.meet_naked_float32s env ty_naked_float32 with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result fs -> (
        (* CR mshinwell: This (and the other cases below including that for
           immutable float32 arrays) seem not to be taking advantage of the fact
           that [Static_const] permits variables in the arguments of e.g.
           [Boxed_float32]. *)
        match Float32.Set.get_singleton fs with
        | None -> try_canonical_simple ()
        | Some f -> Lift (Boxed_float32 f)))
    | Value (Ok (Boxed_int32 (ty_naked_int32, _alloc_mode))) -> (
      match Provers.meet_naked_int32s env ty_naked_int32 with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result ns -> (
        match Int32.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int32 n)))
    | Value (Ok (Boxed_int64 (ty_naked_int64, _alloc_mode))) -> (
      match Provers.meet_naked_int64s env ty_naked_int64 with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result ns -> (
        match Int64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int64 n)))
    | Value (Ok (Boxed_nativeint (ty_naked_nativeint, _alloc_mode))) -> (
      match Provers.meet_naked_nativeints env ty_naked_nativeint with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result ns -> (
        match Targetint_32_64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_nativeint n)))
    | Value (Ok (Boxed_vec128 (ty_naked_vec128, _alloc_mode))) -> (
      match Provers.meet_naked_vec128s env ty_naked_vec128 with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result ns -> (
        match Vector_types.Vec128.Bit_pattern.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_vec128 n)))
    | Value
        (Ok
          (Array
            { contents = Unknown | Known Mutable;
              length;
              element_kind;
              alloc_mode = _
            })) -> (
      match Provers.meet_equals_single_tagged_immediate env length with
      | Known_result length -> (
        if not (Targetint_31_63.equal length Targetint_31_63.zero)
        then try_canonical_simple ()
        else
          match element_kind with
          | Ok element_kind ->
            let array_kind =
              Empty_array_kind.of_element_kind
                (Flambda_kind.With_subkind.kind element_kind)
            in
            Lift (Empty_array array_kind)
          | Unknown | Bottom -> try_canonical_simple ())
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid)
    | Value
        (Ok
          (Array
            { contents = Known (Immutable { fields });
              length = _;
              alloc_mode;
              element_kind
            })) -> (
      match fields with
      | [||] -> (
        match element_kind with
        | Ok element_kind ->
          let array_kind =
            Empty_array_kind.of_element_kind
              (Flambda_kind.With_subkind.kind element_kind)
          in
          Lift (Empty_array array_kind)
        | Unknown | Bottom -> try_canonical_simple ())
      | _ -> (
        let fields = Array.to_list fields in
        match element_kind with
        | Unknown -> try_canonical_simple ()
        | Bottom ->
          (* CR someday vlaviron: we could use [Lift Empty_array] here *)
          try_canonical_simple ()
        | Ok element_kind -> (
          let kind = Flambda_kind.With_subkind.kind element_kind in
          match kind with
          | Value -> (
            let field_types_and_expected_kinds =
              List.map (fun ty -> ty, Flambda_kind.value) fields
            in
            match
              try_to_reify_fields env ~var_allowed alloc_mode
                ~field_types_and_expected_kinds
            with
            | Some fields -> Lift (Immutable_value_array { fields })
            | None -> try_canonical_simple ())
          | Naked_number Naked_float ->
            Lift_array_of_naked_floats.lift env ~fields ~try_canonical_simple
          | Naked_number Naked_float32 ->
            Lift_array_of_naked_float32s.lift env ~fields ~try_canonical_simple
          | Naked_number Naked_int32 ->
            Lift_array_of_naked_int32s.lift env ~fields ~try_canonical_simple
          | Naked_number Naked_int64 ->
            Lift_array_of_naked_int64s.lift env ~fields ~try_canonical_simple
          | Naked_number Naked_nativeint ->
            Lift_array_of_naked_nativeints.lift env ~fields
              ~try_canonical_simple
          | Naked_number (Naked_immediate | Naked_vec128) | Region | Rec_info ->
            Misc.fatal_errorf
              "Unexpected kind %a in immutable array case when reifying type:@ \
               %a@ in env:@ %a"
              Flambda_kind.print kind TG.print t TE.print env)))
    | Value Bottom
    | Naked_immediate Bottom
    | Naked_float32 Bottom
    | Naked_float Bottom
    | Naked_int32 Bottom
    | Naked_int64 Bottom
    | Naked_nativeint Bottom
    | Naked_vec128 Bottom
    | Rec_info Bottom
    | Region Bottom ->
      Invalid
    | Value Unknown
    | Value (Ok (String _))
    | Value (Ok Null)
    | Naked_immediate Unknown
    | Naked_float32 Unknown
    | Naked_float Unknown
    | Naked_int32 Unknown
    | Naked_int64 Unknown
    | Naked_vec128 Unknown
    | Naked_nativeint Unknown
    | Rec_info Unknown
    | Region (Unknown | Ok _)
    | Rec_info (Ok _) ->
      try_canonical_simple ())
