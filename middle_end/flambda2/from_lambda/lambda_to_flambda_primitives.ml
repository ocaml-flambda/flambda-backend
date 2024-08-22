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

module H = Lambda_to_flambda_primitives_helpers
module K = Flambda_kind
module I = K.Standard_int
module I_or_f = K.Standard_int_or_float
module L = Lambda
module P = Flambda_primitive

let convert_integer_comparison_prim (comp : L.integer_comparison) :
    P.binary_primitive =
  match comp with
  | Ceq -> Phys_equal Eq
  | Cne -> Phys_equal Neq
  | Clt -> Int_comp (Tagged_immediate, Yielding_bool (Lt Signed))
  | Cgt -> Int_comp (Tagged_immediate, Yielding_bool (Gt Signed))
  | Cle -> Int_comp (Tagged_immediate, Yielding_bool (Le Signed))
  | Cge -> Int_comp (Tagged_immediate, Yielding_bool (Ge Signed))

let convert_boxed_integer_comparison_prim (kind : L.boxed_integer)
    (comp : L.integer_comparison) : P.binary_primitive =
  match kind, comp with
  | Pint32, Ceq -> Int_comp (Naked_int32, Yielding_bool Eq)
  | Pint32, Cne -> Int_comp (Naked_int32, Yielding_bool Neq)
  | Pint32, Clt -> Int_comp (Naked_int32, Yielding_bool (Lt Signed))
  | Pint32, Cgt -> Int_comp (Naked_int32, Yielding_bool (Gt Signed))
  | Pint32, Cle -> Int_comp (Naked_int32, Yielding_bool (Le Signed))
  | Pint32, Cge -> Int_comp (Naked_int32, Yielding_bool (Ge Signed))
  | Pint64, Ceq -> Int_comp (Naked_int64, Yielding_bool Eq)
  | Pint64, Cne -> Int_comp (Naked_int64, Yielding_bool Neq)
  | Pint64, Clt -> Int_comp (Naked_int64, Yielding_bool (Lt Signed))
  | Pint64, Cgt -> Int_comp (Naked_int64, Yielding_bool (Gt Signed))
  | Pint64, Cle -> Int_comp (Naked_int64, Yielding_bool (Le Signed))
  | Pint64, Cge -> Int_comp (Naked_int64, Yielding_bool (Ge Signed))
  | Pnativeint, Ceq -> Int_comp (Naked_nativeint, Yielding_bool Eq)
  | Pnativeint, Cne -> Int_comp (Naked_nativeint, Yielding_bool Neq)
  | Pnativeint, Clt -> Int_comp (Naked_nativeint, Yielding_bool (Lt Signed))
  | Pnativeint, Cgt -> Int_comp (Naked_nativeint, Yielding_bool (Gt Signed))
  | Pnativeint, Cle -> Int_comp (Naked_nativeint, Yielding_bool (Le Signed))
  | Pnativeint, Cge -> Int_comp (Naked_nativeint, Yielding_bool (Ge Signed))

let convert_float_comparison (comp : L.float_comparison) : unit P.comparison =
  match comp with
  | CFeq -> Eq
  | CFneq -> Neq
  | CFlt -> Lt ()
  | CFgt -> Gt ()
  | CFle -> Le ()
  | CFge -> Ge ()
  | CFnlt | CFngt | CFnle | CFnge ->
    Misc.fatal_error
      "Negated floating-point comparisons should have been removed by \
       [Lambda_to_flambda]"

let boxable_number_of_boxed_integer (bint : L.boxed_integer) :
    K.Boxable_number.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let standard_int_of_boxed_integer (bint : L.boxed_integer) : K.Standard_int.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let standard_int_or_float_of_boxed_integer (bint : L.boxed_integer) :
    K.Standard_int_or_float.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let convert_block_access_field_kind i_or_p : P.Block_access_field_kind.t =
  match i_or_p with L.Immediate -> Immediate | L.Pointer -> Any_value

let convert_init_or_assign (i_or_a : L.initialization_or_assignment) :
    P.Init_or_assign.t =
  match i_or_a with
  | Assignment mode -> Assignment (Alloc_mode.For_assignments.from_lambda mode)
  | Heap_initialization -> Initialization
  | Root_initialization ->
    Misc.fatal_error "[Root_initialization] should not appear in Flambda input"

let convert_block_shape (shape : L.block_shape) ~num_fields =
  match shape with
  | None -> List.init num_fields (fun _field -> K.With_subkind.any_value)
  | Some shape ->
    let shape_length = List.length shape in
    if num_fields <> shape_length
    then
      Misc.fatal_errorf
        "Flambda_arity.of_block_shape: num_fields is %d yet the shape has %d \
         fields"
        num_fields shape_length;
    List.map K.With_subkind.from_lambda_value_kind shape

let check_float_array_optimisation_enabled name =
  if not (Flambda_features.flat_float_array ())
  then
    Misc.fatal_errorf
      "[%s] is not expected when the float array optimisation is disabled" name
      ()

type converted_array_kind =
  | Array_kind of P.Array_kind.t
  | Float_array_opt_dynamic

let convert_array_kind (kind : L.array_kind) : converted_array_kind =
  match kind with
  | Pgenarray ->
    check_float_array_optimisation_enabled "Pgenarray";
    Float_array_opt_dynamic
  | Paddrarray -> Array_kind Values
  | Pintarray -> Array_kind Immediates
  | Pfloatarray | Punboxedfloatarray Pfloat64 -> Array_kind Naked_floats
  | Punboxedfloatarray Pfloat32 -> Array_kind Naked_float32s
  | Punboxedintarray Pint32 -> Array_kind Naked_int32s
  | Punboxedintarray Pint64 -> Array_kind Naked_int64s
  | Punboxedintarray Pnativeint -> Array_kind Naked_nativeints

let convert_array_kind_for_length kind : P.Array_kind_for_length.t =
  match convert_array_kind kind with
  | Array_kind array_kind -> Array_kind array_kind
  | Float_array_opt_dynamic -> Float_array_opt_dynamic

module Array_ref_kind = struct
  type t =
    | Immediates
    | Values
    | Naked_floats_to_be_boxed of L.alloc_mode
    | Naked_floats
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
end

type converted_array_ref_kind =
  | Array_ref_kind of Array_ref_kind.t
  | Float_array_opt_dynamic_ref of L.alloc_mode

let convert_array_ref_kind (kind : L.array_ref_kind) : converted_array_ref_kind
    =
  match kind with
  | Pgenarray_ref mode ->
    (* CR mshinwell: We can't check this because of the translations of
       primitives for Obj.size, Obj.field and Obj.set_field, which can be used
       both on arrays and blocks. We should probably propagate the "%obj_..."
       primitives which these functions use all the way to the middle end. Then
       this check could be reinstated for all normal cases.

       check_float_array_optimisation_enabled (); *)
    Float_array_opt_dynamic_ref mode
  | Paddrarray_ref -> Array_ref_kind Values
  | Pintarray_ref -> Array_ref_kind Immediates
  | Pfloatarray_ref mode -> Array_ref_kind (Naked_floats_to_be_boxed mode)
  | Punboxedfloatarray_ref Pfloat64 -> Array_ref_kind Naked_floats
  | Punboxedfloatarray_ref Pfloat32 -> Array_ref_kind Naked_float32s
  | Punboxedintarray_ref Pint32 -> Array_ref_kind Naked_int32s
  | Punboxedintarray_ref Pint64 -> Array_ref_kind Naked_int64s
  | Punboxedintarray_ref Pnativeint -> Array_ref_kind Naked_nativeints

let convert_array_ref_kind_for_length array_ref_kind : P.Array_kind_for_length.t
    =
  match convert_array_ref_kind array_ref_kind with
  | Float_array_opt_dynamic_ref _ -> Float_array_opt_dynamic
  | Array_ref_kind Values -> Array_kind Values
  | Array_ref_kind Immediates -> Array_kind Immediates
  | Array_ref_kind (Naked_floats | Naked_floats_to_be_boxed _) ->
    Array_kind Naked_floats
  | Array_ref_kind Naked_float32s -> Array_kind Naked_float32s
  | Array_ref_kind Naked_int32s -> Array_kind Naked_int32s
  | Array_ref_kind Naked_int64s -> Array_kind Naked_int64s
  | Array_ref_kind Naked_nativeints -> Array_kind Naked_nativeints

module Array_set_kind = struct
  type t =
    | Immediates
    | Values of P.Init_or_assign.t
    | Naked_floats
    | Naked_floats_to_be_unboxed
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
end

type converted_array_set_kind =
  | Array_set_kind of Array_set_kind.t
  | Float_array_opt_dynamic_set of Alloc_mode.For_assignments.t

let convert_intermediate_array_set_kind (kind : Array_set_kind.t) :
    P.Array_set_kind.t =
  match kind with
  | Immediates -> Immediates
  | Values init_or_assign -> Values init_or_assign
  | Naked_floats | Naked_floats_to_be_unboxed -> Naked_floats
  | Naked_float32s -> Naked_float32s
  | Naked_int32s -> Naked_int32s
  | Naked_int64s -> Naked_int64s
  | Naked_nativeints -> Naked_nativeints

let convert_array_set_kind (kind : L.array_set_kind) : converted_array_set_kind
    =
  match kind with
  | Pgenarray_set mode ->
    (* CR mshinwell: see CR in [convert_array_ref_kind] above

       check_float_array_optimisation_enabled (); *)
    Float_array_opt_dynamic_set (Alloc_mode.For_assignments.from_lambda mode)
  | Paddrarray_set mode ->
    Array_set_kind
      (Values (Assignment (Alloc_mode.For_assignments.from_lambda mode)))
  | Pintarray_set -> Array_set_kind Immediates
  | Pfloatarray_set -> Array_set_kind Naked_floats_to_be_unboxed
  | Punboxedfloatarray_set Pfloat64 -> Array_set_kind Naked_floats
  | Punboxedfloatarray_set Pfloat32 -> Array_set_kind Naked_float32s
  | Punboxedintarray_set Pint32 -> Array_set_kind Naked_int32s
  | Punboxedintarray_set Pint64 -> Array_set_kind Naked_int64s
  | Punboxedintarray_set Pnativeint -> Array_set_kind Naked_nativeints

let convert_array_set_kind_for_length array_set_kind : P.Array_kind_for_length.t
    =
  match convert_array_set_kind array_set_kind with
  | Float_array_opt_dynamic_set _ -> Float_array_opt_dynamic
  | Array_set_kind (Values _) -> Array_kind Values
  | Array_set_kind Immediates -> Array_kind Immediates
  | Array_set_kind (Naked_floats | Naked_floats_to_be_unboxed) ->
    Array_kind Naked_floats
  | Array_set_kind Naked_float32s -> Array_kind Naked_float32s
  | Array_set_kind Naked_int32s -> Array_kind Naked_int32s
  | Array_set_kind Naked_int64s -> Array_kind Naked_int64s
  | Array_set_kind Naked_nativeints -> Array_kind Naked_nativeints

type converted_duplicate_array_kind =
  | Duplicate_array_kind of P.Duplicate_array_kind.t
  | Float_array_opt_dynamic

let convert_array_kind_to_duplicate_array_kind (kind : L.array_kind) :
    converted_duplicate_array_kind =
  match kind with
  | Pgenarray ->
    check_float_array_optimisation_enabled "Pgenarray";
    Float_array_opt_dynamic
  | Paddrarray -> Duplicate_array_kind Values
  | Pintarray -> Duplicate_array_kind Immediates
  | Pfloatarray | Punboxedfloatarray Pfloat64 ->
    Duplicate_array_kind (Naked_floats { length = None })
  | Punboxedfloatarray Pfloat32 ->
    Duplicate_array_kind (Naked_float32s { length = None })
  | Punboxedintarray Pint32 ->
    Duplicate_array_kind (Naked_int32s { length = None })
  | Punboxedintarray Pint64 ->
    Duplicate_array_kind (Naked_int64s { length = None })
  | Punboxedintarray Pnativeint ->
    Duplicate_array_kind (Naked_nativeints { length = None })

let convert_field_read_semantics (sem : L.field_read_semantics) : Mutability.t =
  match sem with Reads_agree -> Immutable | Reads_vary -> Mutable

let bigarray_dim_bound b dimension =
  H.Prim (Unary (Bigarray_length { dimension }, b))

let tag_int (arg : H.expr_primitive) : H.expr_primitive =
  Unary (Tag_immediate, Prim arg)

let untag_int (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Untag_immediate, arg))

let unbox_float32 (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Unbox_number K.Boxable_number.Naked_float32, arg))

let box_float32 (mode : L.alloc_mode) (arg : H.expr_primitive) ~current_region :
    H.expr_primitive =
  Unary
    ( Box_number
        ( K.Boxable_number.Naked_float32,
          Alloc_mode.For_allocations.from_lambda mode ~current_region ),
      Prim arg )

let box_float (mode : L.alloc_mode) (arg : H.expr_primitive) ~current_region :
    H.expr_primitive =
  Unary
    ( Box_number
        ( K.Boxable_number.Naked_float,
          Alloc_mode.For_allocations.from_lambda mode ~current_region ),
      Prim arg )

let unbox_float (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Unbox_number K.Boxable_number.Naked_float, arg))

let box_bint bi mode (arg : H.expr_primitive) ~current_region : H.expr_primitive
    =
  Unary
    ( Box_number
        ( boxable_number_of_boxed_integer bi,
          Alloc_mode.For_allocations.from_lambda mode ~current_region ),
      Prim arg )

let unbox_bint bi (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Unbox_number (boxable_number_of_boxed_integer bi), arg))

let box_vec128 mode (arg : H.expr_primitive) ~current_region : H.expr_primitive
    =
  Unary
    ( Box_number
        ( Naked_vec128,
          Alloc_mode.For_allocations.from_lambda mode ~current_region ),
      Prim arg )

let unbox_vec128 (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Unbox_number Naked_vec128, arg))

let bint_unary_prim bi mode prim arg1 =
  box_bint bi mode
    (Unary
       (Int_arith (standard_int_of_boxed_integer bi, prim), unbox_bint bi arg1))

let bint_binary_prim bi mode prim arg1 arg2 =
  box_bint bi mode
    (Binary
       ( Int_arith (standard_int_of_boxed_integer bi, prim),
         unbox_bint bi arg1,
         unbox_bint bi arg2 ))

let bint_shift bi mode prim arg1 arg2 =
  box_bint bi mode
    (Binary
       ( Int_shift (standard_int_of_boxed_integer bi, prim),
         unbox_bint bi arg1,
         untag_int arg2 ))

let convert_index_to_tagged_int index (index_kind : Lambda.array_index_kind) =
  match index_kind with
  | Ptagged_int_index -> index
  | Punboxed_int_index bint ->
    H.Prim
      (Unary
         ( Num_conv
             { src = standard_int_or_float_of_boxed_integer bint;
               dst = Tagged_immediate
             },
           index ))

let check_non_negative_imm imm prim_name =
  if not (Targetint_31_63.is_non_negative imm)
  then
    Misc.fatal_errorf "%s with negative index %a" prim_name
      Targetint_31_63.print imm

(* Smart constructor for checked accesses *)
let checked_access ~dbg ~primitive ~conditions : H.expr_primitive =
  Checked
    { primitive;
      validity_conditions = conditions;
      failure = Index_out_of_bounds;
      dbg
    }

let checked_alignment ~dbg ~primitive ~conditions : H.expr_primitive =
  Checked
    { primitive;
      validity_conditions = conditions;
      failure = Address_was_misaligned;
      dbg
    }

let check_bound ~(index_kind : Lambda.array_index_kind) ~(bound_kind : I.t)
    ~index ~bound : H.expr_primitive =
  let (comp_kind : I.t), index, bound =
    let convert_bound_to dst =
      H.Prim
        (Unary (Num_conv { src = I_or_f.of_standard_int bound_kind; dst }, bound))
    in
    (* The reason why we convert the bound instead of the index value is because
       of edge cases around large negative numbers.

       Given [-9223372036854775807] as a [Naked_int64] index, its bit
       representation is
       [0b1000000000000000000000000000000000000000000000000000000000000001]. If
       we convert that into a [Tagged_immediate], it becomes [0b11] and the
       bounds check would pass in cases that we should reject.

       This also has the added benefit of producing better assembly code.
       Usually saving one instruction compared to tagging the index value. *)
    match index_kind with
    | Ptagged_int_index ->
      I.Naked_immediate, untag_int index, convert_bound_to Naked_immediate
    | Punboxed_int_index bint ->
      ( standard_int_of_boxed_integer bint,
        index,
        convert_bound_to (standard_int_or_float_of_boxed_integer bint) )
  in
  Binary (Int_comp (comp_kind, Yielding_bool (Lt Unsigned)), index, bound)

(* This computes the maximum of a given value [x] with zero, in an optimized
   way. It takes as named argument the size (in bytes) of an integer register on
   the target architecture.

   It is equivalent to the `max_or_zero` function in `cmm_helpers.ml` *)
let max_with_zero ~size_int x =
  let register_bitsize_minus_one =
    H.Simple
      (Simple.const
         (Reg_width_const.naked_immediate
            (Targetint_31_63.of_int ((size_int * 8) - 1))))
  in
  let sign =
    H.Prim
      (Binary (Int_shift (Naked_nativeint, Asr), x, register_bitsize_minus_one))
  in
  let minus_one =
    H.Simple
      (Simple.const
         (Reg_width_const.naked_nativeint (Targetint_32_64.of_int (-1))))
  in
  let sign_negation =
    H.Prim (Binary (Int_arith (Naked_nativeint, Xor), sign, minus_one))
  in
  let ret =
    H.Prim (Binary (Int_arith (Naked_nativeint, And), sign_negation, x))
  in
  ret

(* actual (strict) upper bound for an index in a string-like read/write *)
let actual_max_length_for_string_like_access ~size_int
    ~(access_size : Flambda_primitive.string_accessor_width) length =
  (* offset to subtract from the length depending on the size of the
     read/write *)
  let length_offset_of_size size =
    let offset =
      match (size : Flambda_primitive.string_accessor_width) with
      | Eight -> 0
      | Sixteen -> 1
      | Thirty_two | Single -> 3
      | Sixty_four -> 7
      | One_twenty_eight _ -> 15
    in
    Targetint_31_63.of_int offset
  in
  match access_size with
  | Eight -> length (* micro-optimization *)
  | Sixteen | Thirty_two | Single | Sixty_four | One_twenty_eight _ ->
    let offset = length_offset_of_size access_size in
    let reduced_length =
      H.Prim
        (Binary
           ( Int_arith (Naked_immediate, Sub),
             length,
             Simple (Simple.const (Reg_width_const.naked_immediate offset)) ))
    in
    (* We need to convert the length into a naked_nativeint because the
       optimised version of the max_with_zero function needs to be on
       machine-width integers to work (or at least on an integer number of bytes
       to work). *)
    let reduced_length_nativeint =
      H.Prim
        (Unary
           ( Num_conv { src = Naked_immediate; dst = Naked_nativeint },
             reduced_length ))
    in
    let nativeint_res = max_with_zero ~size_int reduced_length_nativeint in
    H.Prim
      (Unary
         ( Num_conv { src = Naked_nativeint; dst = Naked_immediate },
           nativeint_res ))

(* String-like validity conditions *)

let string_like_access_validity_condition ~size_int ~access_size ~length
    ~index_kind index : H.expr_primitive =
  check_bound ~index_kind ~bound_kind:Naked_immediate ~index
    ~bound:
      (actual_max_length_for_string_like_access ~size_int ~access_size length)

let string_or_bytes_access_validity_condition ~size_int str kind access_size
    ~index_kind index : H.expr_primitive =
  string_like_access_validity_condition ~index_kind index ~size_int ~access_size
    ~length:(Prim (Unary (String_length kind, str)))

let bigstring_access_validity_condition ~size_int big_str access_size
    ~index_kind index : H.expr_primitive =
  string_like_access_validity_condition ~index_kind index ~size_int ~access_size
    ~length:(bigarray_dim_bound big_str 1)

let bigstring_alignment_validity_condition bstr alignment tagged_index :
    H.expr_primitive =
  Binary
    ( Int_comp (I.Naked_immediate, Yielding_bool Eq),
      Prim
        (Binary (Bigarray_get_alignment alignment, bstr, untag_int tagged_index)),
      Simple Simple.untagged_const_zero )

let checked_string_or_bytes_access ~dbg ~size_int ~access_size ~primitive kind
    string ~index_kind index =
  (match (access_size : P.string_accessor_width) with
  | One_twenty_eight { aligned = true } ->
    Misc.fatal_error
      "flambda2 cannot yet check string/bytes aligned access safety"
  | Eight | Sixteen | Thirty_two | Single | Sixty_four
  | One_twenty_eight { aligned = false } ->
    ());
  checked_access ~dbg ~primitive
    ~conditions:
      [ string_or_bytes_access_validity_condition ~size_int string kind
          access_size ~index_kind index ]

let checked_bigstring_access ~dbg ~size_int ~access_size ~primitive arg1
    ~index_kind arg2 =
  let primitive =
    match (access_size : P.string_accessor_width) with
    | One_twenty_eight { aligned = true } ->
      checked_alignment ~dbg ~primitive
        ~conditions:
          [ bigstring_alignment_validity_condition arg1 16
              (convert_index_to_tagged_int arg2 index_kind) ]
    | Eight | Sixteen | Thirty_two | Single | Sixty_four
    | One_twenty_eight { aligned = false } ->
      primitive
  in
  checked_access ~dbg ~primitive
    ~conditions:
      [ bigstring_access_validity_condition ~size_int arg1 access_size
          ~index_kind arg2 ]

(* String-like loads *)
let string_like_load ~dbg ~unsafe
    ~(access_size : Flambda_primitive.string_accessor_width) ~size_int
    (kind : P.string_like_value) mode ~boxed string ~index_kind index
    ~current_region =
  let unsafe_load =
    let index = convert_index_to_tagged_int index index_kind in
    let wrap =
      match access_size, mode with
      | (Eight | Sixteen), None ->
        assert (not boxed);
        tag_int
      | Thirty_two, Some mode ->
        if boxed then box_bint Pint32 mode ~current_region else Fun.id
      | Single, Some mode ->
        if boxed then box_float32 mode ~current_region else Fun.id
      | Sixty_four, Some mode ->
        if boxed then box_bint Pint64 mode ~current_region else Fun.id
      | One_twenty_eight _, Some mode ->
        if boxed then box_vec128 mode ~current_region else Fun.id
      | (Eight | Sixteen), Some _
      | (Thirty_two | Single | Sixty_four | One_twenty_eight _), None ->
        Misc.fatal_error "Inconsistent alloc_mode for string or bytes load"
    in
    wrap (Binary (String_or_bigstring_load (kind, access_size), string, index))
  in
  if unsafe
  then unsafe_load
  else
    let check_access =
      match kind with
      | String -> checked_string_or_bytes_access String
      | Bytes -> checked_string_or_bytes_access Bytes
      | Bigstring -> checked_bigstring_access
    in
    check_access ~dbg ~size_int ~access_size ~primitive:unsafe_load string
      ~index_kind index

let get_header obj mode ~current_region =
  let wrap hd = box_bint Pnativeint mode hd ~current_region in
  wrap (Unary (Get_header, obj))

(* Bytes-like set *)
let bytes_like_set ~dbg ~unsafe
    ~(access_size : Flambda_primitive.string_accessor_width) ~size_int
    (kind : P.bytes_like_value) ~boxed bytes ~index_kind index new_value =
  let unsafe_set =
    let index = convert_index_to_tagged_int index index_kind in
    let wrap =
      match access_size with
      | Eight | Sixteen ->
        assert (not boxed);
        untag_int
      | Thirty_two -> if boxed then unbox_bint Pint32 else Fun.id
      | Single -> if boxed then unbox_float32 else Fun.id
      | Sixty_four -> if boxed then unbox_bint Pint64 else Fun.id
      | One_twenty_eight _ -> if boxed then unbox_vec128 else Fun.id
    in
    H.Ternary
      (Bytes_or_bigstring_set (kind, access_size), bytes, index, wrap new_value)
  in
  if unsafe
  then unsafe_set
  else
    let check_access =
      match kind with
      | Bytes -> checked_string_or_bytes_access Bytes
      | Bigstring -> checked_bigstring_access
    in
    check_access ~dbg ~size_int ~access_size ~primitive:unsafe_set bytes
      ~index_kind index

(* Array vector load/store *)

let array_vector_access_validity_condition array ~size_int
    (array_kind : P.Array_kind.t) index =
  let width_in_scalars =
    match array_kind with
    | Naked_floats | Immediates | Naked_int64s | Naked_nativeints -> 2
    | Naked_int32s | Naked_float32s -> 4
    | Values ->
      Misc.fatal_error
        "Attempted to load/store a SIMD vector from/to a value array."
  in
  let length_untagged =
    untag_int (H.Prim (Unary (Array_length (Array_kind array_kind), array)))
  in
  let reduced_length_untagged =
    H.Prim
      (Binary
         ( Int_arith (Naked_immediate, Sub),
           length_untagged,
           Simple
             (Simple.untagged_const_int
                (Targetint_31_63.of_int (width_in_scalars - 1))) ))
  in
  (* We need to convert the length into a naked_nativeint because the optimised
     version of the max_with_zero function needs to be on machine-width integers
     to work (or at least on an integer number of bytes to work). *)
  let reduced_length_nativeint =
    H.Prim
      (Unary
         ( Num_conv { src = Naked_immediate; dst = Naked_nativeint },
           reduced_length_untagged ))
  in
  let nativeint_bound = max_with_zero ~size_int reduced_length_nativeint in
  check_bound ~index_kind:Ptagged_int_index ~bound_kind:Naked_nativeint ~index
    ~bound:nativeint_bound

let check_array_vector_access ~dbg ~size_int ~array array_kind ~index primitive
    : H.expr_primitive =
  checked_access ~primitive
    ~conditions:
      [array_vector_access_validity_condition ~size_int array array_kind index]
    ~dbg

let array_like_load_128 ~dbg ~size_int ~unsafe ~mode ~boxed ~current_region
    array_kind array index =
  let primitive =
    H.Binary (Array_load (array_kind, Vec128, Mutable), array, index)
  in
  let primitive =
    if boxed then box_vec128 mode ~current_region primitive else primitive
  in
  if unsafe
  then primitive
  else
    check_array_vector_access ~dbg ~size_int ~array array_kind ~index primitive

let array_like_set_128 ~dbg ~size_int ~unsafe ~boxed array_kind array index
    new_value =
  let new_value = if boxed then unbox_vec128 new_value else new_value in
  let primitive =
    H.Ternary (Array_set (array_kind, Vec128), array, index, new_value)
  in
  if unsafe
  then primitive
  else
    check_array_vector_access ~dbg ~size_int ~array
      (P.Array_set_kind.array_kind array_kind)
      ~index primitive

(* Bigarray accesses *)
let bigarray_box_or_tag_raw_value_to_read kind alloc_mode =
  let error what =
    Misc.fatal_errorf "Don't know how to box %s after reading it in a bigarray"
      what
  in
  match P.Bigarray_kind.element_kind kind with
  | Value -> Fun.id
  | Naked_number Naked_immediate -> fun arg -> H.Unary (Tag_immediate, Prim arg)
  | Naked_number Naked_float32 ->
    fun arg -> H.Unary (Box_number (Naked_float32, alloc_mode), Prim arg)
  | Naked_number Naked_float ->
    fun arg -> H.Unary (Box_number (Naked_float, alloc_mode), Prim arg)
  | Naked_number Naked_int32 ->
    fun arg -> H.Unary (Box_number (Naked_int32, alloc_mode), Prim arg)
  | Naked_number Naked_int64 ->
    fun arg -> H.Unary (Box_number (Naked_int64, alloc_mode), Prim arg)
  | Naked_number Naked_nativeint ->
    fun arg -> H.Unary (Box_number (Naked_nativeint, alloc_mode), Prim arg)
  | Naked_number Naked_vec128 ->
    fun arg -> H.Unary (Box_number (Naked_vec128, alloc_mode), Prim arg)
  | Region -> error "a region expression"
  | Rec_info -> error "recursion info"

let bigarray_unbox_or_untag_value_to_store kind =
  let error what =
    Misc.fatal_errorf "Don't know how to unbox %s to store it in a bigarray"
      what
  in
  match P.Bigarray_kind.element_kind kind with
  | Value -> Fun.id
  | Naked_number Naked_immediate ->
    fun arg -> H.Prim (Unary (Untag_immediate, arg))
  | Naked_number Naked_float32 ->
    fun arg -> H.Prim (Unary (Unbox_number Naked_float32, arg))
  | Naked_number Naked_float ->
    fun arg -> H.Prim (Unary (Unbox_number Naked_float, arg))
  | Naked_number Naked_int32 ->
    fun arg -> H.Prim (Unary (Unbox_number Naked_int32, arg))
  | Naked_number Naked_int64 ->
    fun arg -> H.Prim (Unary (Unbox_number Naked_int64, arg))
  | Naked_number Naked_nativeint ->
    fun arg -> H.Prim (Unary (Unbox_number Naked_nativeint, arg))
  | Naked_number Naked_vec128 ->
    fun arg -> H.Prim (Unary (Unbox_number Naked_vec128, arg))
  | Region -> error "a region expression"
  | Rec_info -> error "recursion info"

(* CR Gbury: this function in effect duplicates the bigarray_length access: one
   is done in the validity check, and one in the final offset computation,
   whereas cmmgen let-binds this access. It might matter for the performance,
   although the processor cache might make it not matter at all. *)
let bigarray_indexing layout b args =
  let num_dim = List.length args in
  let rec aux dim delta_dim = function
    | [] -> assert false
    | [index] ->
      let bound = bigarray_dim_bound b dim in
      let check =
        check_bound ~index_kind:Ptagged_int_index ~bound_kind:Naked_immediate
          ~index ~bound
      in
      [check], index
    | index :: r ->
      let checks, rem = aux (dim + delta_dim) delta_dim r in
      let bound = bigarray_dim_bound b dim in
      let check =
        check_bound ~index_kind:Ptagged_int_index ~bound_kind:Naked_immediate
          ~index ~bound
      in
      (* CR gbury: because we tag bound, and the tagged multiplication untags
         it, we might be left with a needless zero-extend here. *)
      let tmp =
        H.Prim
          (Binary
             ( Int_arith (I.Tagged_immediate, Mul),
               rem,
               Prim (Unary (Tag_immediate, bound)) ))
      in
      let offset =
        H.Prim (Binary (Int_arith (I.Tagged_immediate, Add), tmp, index))
      in
      check :: checks, offset
  in
  match (layout : P.Bigarray_layout.t) with
  | C -> aux num_dim (-1) (List.rev args)
  | Fortran ->
    aux 1 1
      (List.map
         (fun idx ->
           H.Prim
             (Binary
                ( Int_arith (I.Tagged_immediate, Sub),
                  idx,
                  H.Simple (Simple.const_int Targetint_31_63.one) )))
         args)

let bigarray_access ~dbg ~unsafe ~access layout b indexes =
  let num_dim = List.length indexes in
  let checks, offset = bigarray_indexing layout b indexes in
  let primitive = access num_dim offset in
  if unsafe
  then primitive
  else checked_access ~dbg ~conditions:checks ~primitive

let bigarray_load ~dbg ~unsafe kind layout b indexes =
  let access num_dim offset =
    H.Binary (Bigarray_load (num_dim, kind, layout), b, offset)
  in
  bigarray_access ~dbg ~unsafe ~access layout b indexes

let bigarray_set ~dbg ~unsafe kind layout b indexes value =
  let access num_dim offset =
    H.Ternary (Bigarray_set (num_dim, kind, layout), b, offset, value)
  in
  bigarray_access ~dbg ~unsafe ~access layout b indexes

(* Array accesses *)
let array_access_validity_condition array array_kind index
    ~(index_kind : L.array_index_kind) =
  let arr_len_as_tagged_imm = H.Prim (Unary (Array_length array_kind, array)) in
  [ check_bound ~index_kind ~bound_kind:Tagged_immediate ~index
      ~bound:arr_len_as_tagged_imm ]

let check_array_access ~dbg ~array array_kind ~index ~index_kind primitive :
    H.expr_primitive =
  checked_access ~primitive
    ~conditions:
      (array_access_validity_condition array array_kind index ~index_kind)
    ~dbg

let array_load_unsafe ~array ~index (array_ref_kind : Array_ref_kind.t)
    ~current_region : H.expr_primitive =
  match array_ref_kind with
  | Immediates -> Binary (Array_load (Immediates, Scalar, Mutable), array, index)
  | Values -> Binary (Array_load (Values, Scalar, Mutable), array, index)
  | Naked_floats_to_be_boxed mode ->
    box_float mode
      (Binary (Array_load (Naked_floats, Scalar, Mutable), array, index))
      ~current_region
  | Naked_floats ->
    Binary (Array_load (Naked_floats, Scalar, Mutable), array, index)
  | Naked_float32s ->
    Binary (Array_load (Naked_float32s, Scalar, Mutable), array, index)
  | Naked_int32s ->
    Binary (Array_load (Naked_int32s, Scalar, Mutable), array, index)
  | Naked_int64s ->
    Binary (Array_load (Naked_int64s, Scalar, Mutable), array, index)
  | Naked_nativeints ->
    Binary (Array_load (Naked_nativeints, Scalar, Mutable), array, index)

let array_set_unsafe ~array ~index ~new_value
    (array_set_kind : Array_set_kind.t) : H.expr_primitive =
  let new_value =
    match array_set_kind with
    | Immediates | Values _ | Naked_floats | Naked_float32s | Naked_int32s
    | Naked_int64s | Naked_nativeints ->
      new_value
    | Naked_floats_to_be_unboxed -> unbox_float new_value
  in
  let array_set_kind = convert_intermediate_array_set_kind array_set_kind in
  Ternary (Array_set (array_set_kind, Scalar), array, index, new_value)

let[@inline always] match_on_array_ref_kind ~array array_ref_kind f :
    H.expr_primitive =
  match convert_array_ref_kind array_ref_kind with
  | Array_ref_kind array_ref_kind -> f array_ref_kind
  | Float_array_opt_dynamic_ref mode ->
    (* CR keryan: we should push the ITE as low as possible to avoid duplicating
       too much *)
    If_then_else
      ( Unary (Is_flat_float_array, array),
        f (Array_ref_kind.Naked_floats_to_be_boxed mode),
        f Array_ref_kind.Values )

let[@inline always] match_on_array_set_kind ~array array_ref_kind f :
    H.expr_primitive =
  match convert_array_set_kind array_ref_kind with
  | Array_set_kind array_set_kind -> f array_set_kind
  | Float_array_opt_dynamic_set mode ->
    (* CR keryan: we should push the ITE as low as possible to avoid duplicating
       too much *)
    If_then_else
      ( Unary (Is_flat_float_array, array),
        f Array_set_kind.Naked_floats_to_be_unboxed,
        f (Array_set_kind.Values (Assignment mode)) )

(* Safe arith (div/mod by zero) *)
let checked_arith_op ~dbg (bi : Lambda.boxed_integer option) op mode arg1 arg2
    ~current_region : H.expr_primitive =
  let primitive, kind, zero, arg_wrap =
    match bi, mode with
    | None, None ->
      ( H.Binary (Int_arith (I.Tagged_immediate, op), arg1, arg2),
        I.Tagged_immediate,
        Reg_width_const.tagged_immediate Targetint_31_63.zero,
        Fun.id )
    | Some bi, Some mode ->
      let kind, zero =
        match bi with
        | Pint32 -> I.Naked_int32, Reg_width_const.naked_int32 0l
        | Pint64 -> I.Naked_int64, Reg_width_const.naked_int64 0L
        | Pnativeint ->
          ( I.Naked_nativeint,
            Reg_width_const.naked_nativeint Targetint_32_64.zero )
      in
      ( bint_binary_prim bi mode op arg1 arg2 ~current_region,
        kind,
        zero,
        unbox_bint bi )
    | _, _ -> Misc.fatal_error "Inconsistent allocation mode"
  in
  (* CR gbury: try and avoid the unboxing duplication of arg2. (the simplifier
     might cse the duplication away, but it won't be the case for classic
     mode). *)
  Checked
    { primitive;
      validity_conditions =
        [ Binary
            ( Int_comp (kind, Yielding_bool Neq),
              arg_wrap arg2,
              Simple (Simple.const zero) ) ];
      failure = Division_by_zero;
      dbg
    }

let bbswap bi si mode arg ~current_region : H.expr_primitive =
  let mode = Alloc_mode.For_allocations.from_lambda mode ~current_region in
  Unary
    ( Box_number (bi, mode),
      Prim
        (Unary
           ( Int_arith (si, Swap_byte_endianness),
             Prim (Unary (Unbox_number bi, arg)) )) )

let opaque layout arg ~middle_end_only : H.expr_primitive list =
  let kinds = Flambda_arity.unarize (Flambda_arity.from_lambda_list [layout]) in
  if List.compare_lengths kinds arg <> 0
  then
    Misc.fatal_error
      "Popaque/Pobj_magic layout does not have the same length as unarized \
       argument";
  List.map2
    (fun arg_component kind : H.expr_primitive ->
      let kind = K.With_subkind.kind kind in
      Unary (Opaque_identity { middle_end_only; kind }, arg_component))
    arg kinds

(* Primitive conversion *)
let convert_lprim ~big_endian (prim : L.primitive) (args : Simple.t list list)
    (dbg : Debuginfo.t) ~current_region ~current_ghost_region :
    H.expr_primitive list =
  let orig_args = args in
  let args =
    List.map (List.map (fun arg : H.simple_or_prim -> Simple arg)) args
  in
  let size_int =
    assert (Targetint.size mod 8 = 0);
    Targetint.size / 8
  in
  match prim, args with
  | Pmakeblock (tag, mutability, shape, mode), _ ->
    let args = List.flatten args in
    let mode = Alloc_mode.For_allocations.from_lambda mode ~current_region in
    let tag = Tag.Scannable.create_exn tag in
    let shape = convert_block_shape shape ~num_fields:(List.length args) in
    let mutability = Mutability.from_lambda mutability in
    [Variadic (Make_block (Values (tag, shape), mutability, mode), args)]
  | Pmake_unboxed_product layouts, _ ->
    if List.compare_lengths layouts args <> 0
    then
      Misc.fatal_errorf "Pmake_unboxed_product: expected %d arguments, got %d"
        (List.length layouts) (List.length args);
    List.map (fun arg : H.expr_primitive -> Simple arg) (List.flatten orig_args)
  | Punboxed_product_field (n, layouts), [_] ->
    let layouts_array = Array.of_list layouts in
    if n < 0 || n >= Array.length layouts_array
    then Misc.fatal_errorf "Invalid field index %d for Punboxed_product_field" n;
    let field_arity_component =
      (* N.B. The arity of the field being projected may in itself be an unboxed
         product. *)
      layouts_array.(n) |> Flambda_arity.Component_for_creation.from_lambda
    in
    let field_arity = Flambda_arity.create [field_arity_component] in
    let num_fields_prior_to_projected_fields =
      Misc.Stdlib.List.split_at n layouts
      |> fst
      |> List.map Flambda_arity.Component_for_creation.from_lambda
      |> Flambda_arity.create |> Flambda_arity.cardinal_unarized
    in
    let num_projected_fields = Flambda_arity.cardinal_unarized field_arity in
    let projected_args =
      List.hd orig_args |> Array.of_list
      |> (fun a ->
           Array.sub a num_fields_prior_to_projected_fields num_projected_fields)
      |> Array.to_list
    in
    List.map (fun arg : H.expr_primitive -> Simple arg) projected_args
  | Pmakefloatblock (mutability, mode), _ ->
    let args = List.flatten args in
    let mode = Alloc_mode.For_allocations.from_lambda mode ~current_region in
    let mutability = Mutability.from_lambda mutability in
    [ Variadic
        (Make_block (Naked_floats, mutability, mode), List.map unbox_float args)
    ]
  | Pmakeufloatblock (mutability, mode), _ ->
    let args = List.flatten args in
    let mode = Alloc_mode.For_allocations.from_lambda mode ~current_region in
    let mutability = Mutability.from_lambda mutability in
    [Variadic (Make_block (Naked_floats, mutability, mode), args)]
  | Pmakemixedblock (tag, mutability, shape, mode), _ ->
    let args = List.flatten args in
    let args =
      List.mapi
        (fun i arg ->
          match Lambda.get_mixed_block_element shape i with
          | Value_prefix
          | Flat_suffix (Float64 | Float32 | Imm | Bits32 | Bits64 | Word) ->
            arg
          | Flat_suffix Float_boxed -> unbox_float arg)
        args
    in
    let mode = Alloc_mode.For_allocations.from_lambda mode ~current_region in
    let mutability = Mutability.from_lambda mutability in
    let tag = Tag.Scannable.create_exn tag in
    let shape = K.Mixed_block_shape.from_lambda shape in
    [Variadic (Make_block (Mixed (tag, shape), mutability, mode), args)]
  | Pmakearray (lambda_array_kind, mutability, mode), _ -> (
    let args = List.flatten args in
    let mode = Alloc_mode.For_allocations.from_lambda mode ~current_region in
    let array_kind = convert_array_kind lambda_array_kind in
    let mutability = Mutability.from_lambda mutability in
    match array_kind with
    | Array_kind array_kind ->
      let args =
        match lambda_array_kind with
        | Pgenarray | Paddrarray | Pintarray
        | Punboxedfloatarray (Pfloat64 | Pfloat32)
        | Punboxedintarray (Pint32 | Pint64 | Pnativeint) ->
          args
        | Pfloatarray -> List.map unbox_float args
      in
      [Variadic (Make_array (array_kind, mutability, mode), args)]
    | Float_array_opt_dynamic -> (
      (* If this is an empty array we can just give it array kind [Values].
         (Even empty flat float arrays have tag zero.) *)
      match args with
      | [] ->
        [ Variadic
            (Make_array (Values, Immutable, Alloc_mode.For_allocations.heap), [])
        ]
      | elt :: _ ->
        (* Test the first element to see if it's a boxed float: if it is, this
           array must be created as a flat float array. *)
        [ If_then_else
            ( Unary (Is_boxed_float, elt),
              Variadic
                ( Make_array (Naked_floats, mutability, mode),
                  List.map unbox_float args ),
              Variadic (Make_array (Values, mutability, mode), args) ) ]))
  | Popaque layout, [arg] -> opaque layout arg ~middle_end_only:false
  | Pobj_magic layout, [arg] -> opaque layout arg ~middle_end_only:true
  | Pduprecord (repr, num_fields), [[arg]] ->
    let kind : P.Duplicate_block_kind.t =
      match repr with
      | Record_boxed _ ->
        Values
          { tag = Tag.Scannable.zero;
            length = Targetint_31_63.of_int num_fields
          }
      | Record_float | Record_ufloat ->
        Naked_floats { length = Targetint_31_63.of_int num_fields }
      | Record_inlined (_, Constructor_mixed _, _) | Record_mixed _ -> Mixed
      | Record_inlined
          ( Ordinary { runtime_tag; _ },
            Constructor_uniform_value,
            Variant_boxed _ ) ->
        Values
          { tag = Tag.Scannable.create_exn runtime_tag;
            length = Targetint_31_63.of_int num_fields
          }
      | Record_inlined (Extension _, shape, Variant_extensible) -> (
        match shape with
        | Constructor_uniform_value ->
          Values
            { tag = Tag.Scannable.zero;
              (* The "+1" is because there is an extra field containing the
                 hashed constructor. *)
              length = Targetint_31_63.of_int (num_fields + 1)
            }
        | Constructor_mixed _ ->
          (* CR layouts v5.9: support this *)
          Misc.fatal_error "Mixed blocks extensible variants are not supported")
      | Record_inlined (Extension _, _, _)
      | Record_inlined (Ordinary _, _, (Variant_unboxed | Variant_extensible))
      | Record_unboxed ->
        Misc.fatal_errorf "Cannot handle record kind for Pduprecord: %a"
          Printlambda.primitive prim
    in
    [Unary (Duplicate_block { kind }, arg)]
  | Pnegint, [[arg]] -> [Unary (Int_arith (I.Tagged_immediate, Neg), arg)]
  | Paddint, [[arg1]; [arg2]] ->
    [Binary (Int_arith (I.Tagged_immediate, Add), arg1, arg2)]
  | Psubint, [[arg1]; [arg2]] ->
    [Binary (Int_arith (I.Tagged_immediate, Sub), arg1, arg2)]
  | Pmulint, [[arg1]; [arg2]] ->
    [Binary (Int_arith (I.Tagged_immediate, Mul), arg1, arg2)]
  | Pandint, [[arg1]; [arg2]] ->
    [Binary (Int_arith (I.Tagged_immediate, And), arg1, arg2)]
  | Porint, [[arg1]; [arg2]] ->
    [Binary (Int_arith (I.Tagged_immediate, Or), arg1, arg2)]
  | Pxorint, [[arg1]; [arg2]] ->
    [Binary (Int_arith (I.Tagged_immediate, Xor), arg1, arg2)]
  | Plslint, [[arg1]; [arg2]] ->
    [Binary (Int_shift (I.Tagged_immediate, Lsl), arg1, untag_int arg2)]
  | Plsrint, [[arg1]; [arg2]] ->
    [Binary (Int_shift (I.Tagged_immediate, Lsr), arg1, untag_int arg2)]
  | Pasrint, [[arg1]; [arg2]] ->
    [Binary (Int_shift (I.Tagged_immediate, Asr), arg1, untag_int arg2)]
  | Pnot, [[arg]] -> [Unary (Boolean_not, arg)]
  | Pintcomp comp, [[arg1]; [arg2]] ->
    [tag_int (Binary (convert_integer_comparison_prim comp, arg1, arg2))]
  | Pbintcomp (kind, comp), [[arg1]; [arg2]] ->
    let arg1 = unbox_bint kind arg1 in
    let arg2 = unbox_bint kind arg2 in
    [ tag_int
        (Binary (convert_boxed_integer_comparison_prim kind comp, arg1, arg2))
    ]
  | Punboxed_int_comp (kind, comp), [[arg1]; [arg2]] ->
    [ tag_int
        (Binary (convert_boxed_integer_comparison_prim kind comp, arg1, arg2))
    ]
  | Pfloatoffloat32 mode, [[arg]] ->
    let src = K.Standard_int_or_float.Naked_float32 in
    let dst = K.Standard_int_or_float.Naked_float in
    [ box_float mode
        (Unary (Num_conv { src; dst }, unbox_float32 arg))
        ~current_region ]
  | Pfloat32offloat mode, [[arg]] ->
    let src = K.Standard_int_or_float.Naked_float in
    let dst = K.Standard_int_or_float.Naked_float32 in
    [ box_float32 mode
        (Unary (Num_conv { src; dst }, unbox_float arg))
        ~current_region ]
  | Pintoffloat Pfloat64, [[arg]] ->
    let src = K.Standard_int_or_float.Naked_float in
    let dst = K.Standard_int_or_float.Tagged_immediate in
    [Unary (Num_conv { src; dst }, unbox_float arg)]
  | Pfloatofint (Pfloat64, mode), [[arg]] ->
    let src = K.Standard_int_or_float.Tagged_immediate in
    let dst = K.Standard_int_or_float.Naked_float in
    [box_float mode (Unary (Num_conv { src; dst }, arg)) ~current_region]
  | Pnegfloat (Pfloat64, mode), [[arg]] ->
    [ box_float mode
        (Unary (Float_arith (Float64, Neg), unbox_float arg))
        ~current_region ]
  | Pabsfloat (Pfloat64, mode), [[arg]] ->
    [ box_float mode
        (Unary (Float_arith (Float64, Abs), unbox_float arg))
        ~current_region ]
  | Paddfloat (Pfloat64, mode), [[arg1]; [arg2]] ->
    [ box_float mode
        (Binary (Float_arith (Float64, Add), unbox_float arg1, unbox_float arg2))
        ~current_region ]
  | Psubfloat (Pfloat64, mode), [[arg1]; [arg2]] ->
    [ box_float mode
        (Binary (Float_arith (Float64, Sub), unbox_float arg1, unbox_float arg2))
        ~current_region ]
  | Pmulfloat (Pfloat64, mode), [[arg1]; [arg2]] ->
    [ box_float mode
        (Binary (Float_arith (Float64, Mul), unbox_float arg1, unbox_float arg2))
        ~current_region ]
  | Pdivfloat (Pfloat64, mode), [[arg1]; [arg2]] ->
    [ box_float mode
        (Binary (Float_arith (Float64, Div), unbox_float arg1, unbox_float arg2))
        ~current_region ]
  | Pfloatcomp (Pfloat64, comp), [[arg1]; [arg2]] ->
    [ tag_int
        (Binary
           ( Float_comp (Float64, Yielding_bool (convert_float_comparison comp)),
             unbox_float arg1,
             unbox_float arg2 )) ]
  | Punboxed_float_comp (Pfloat64, comp), [[arg1]; [arg2]] ->
    [ tag_int
        (Binary
           ( Float_comp (Float64, Yielding_bool (convert_float_comparison comp)),
             arg1,
             arg2 )) ]
  | Punbox_float Pfloat64, [[arg]] -> [Unary (Unbox_number Naked_float, arg)]
  | Pbox_float (Pfloat64, mode), [[arg]] ->
    [ Unary
        ( Box_number
            ( Naked_float,
              Alloc_mode.For_allocations.from_lambda mode ~current_region ),
          arg ) ]
  | Pintoffloat Pfloat32, [[arg]] ->
    let src = K.Standard_int_or_float.Naked_float32 in
    let dst = K.Standard_int_or_float.Tagged_immediate in
    [Unary (Num_conv { src; dst }, unbox_float32 arg)]
  | Pfloatofint (Pfloat32, mode), [[arg]] ->
    let src = K.Standard_int_or_float.Tagged_immediate in
    let dst = K.Standard_int_or_float.Naked_float32 in
    [box_float32 mode (Unary (Num_conv { src; dst }, arg)) ~current_region]
  | Pnegfloat (Pfloat32, mode), [[arg]] ->
    [ box_float32 mode
        (Unary (Float_arith (Float32, Neg), unbox_float32 arg))
        ~current_region ]
  | Pabsfloat (Pfloat32, mode), [[arg]] ->
    [ box_float32 mode
        (Unary (Float_arith (Float32, Abs), unbox_float32 arg))
        ~current_region ]
  | Paddfloat (Pfloat32, mode), [[arg1]; [arg2]] ->
    [ box_float32 mode
        (Binary
           (Float_arith (Float32, Add), unbox_float32 arg1, unbox_float32 arg2))
        ~current_region ]
  | Psubfloat (Pfloat32, mode), [[arg1]; [arg2]] ->
    [ box_float32 mode
        (Binary
           (Float_arith (Float32, Sub), unbox_float32 arg1, unbox_float32 arg2))
        ~current_region ]
  | Pmulfloat (Pfloat32, mode), [[arg1]; [arg2]] ->
    [ box_float32 mode
        (Binary
           (Float_arith (Float32, Mul), unbox_float32 arg1, unbox_float32 arg2))
        ~current_region ]
  | Pdivfloat (Pfloat32, mode), [[arg1]; [arg2]] ->
    [ box_float32 mode
        (Binary
           (Float_arith (Float32, Div), unbox_float32 arg1, unbox_float32 arg2))
        ~current_region ]
  | Pfloatcomp (Pfloat32, comp), [[arg1]; [arg2]] ->
    [ tag_int
        (Binary
           ( Float_comp (Float32, Yielding_bool (convert_float_comparison comp)),
             unbox_float32 arg1,
             unbox_float32 arg2 )) ]
  | Punboxed_float_comp (Pfloat32, comp), [[arg1]; [arg2]] ->
    [ tag_int
        (Binary
           ( Float_comp (Float32, Yielding_bool (convert_float_comparison comp)),
             arg1,
             arg2 )) ]
  | Punbox_float Pfloat32, [[arg]] -> [Unary (Unbox_number Naked_float32, arg)]
  | Pbox_float (Pfloat32, mode), [[arg]] ->
    [ Unary
        ( Box_number
            ( Naked_float32,
              Alloc_mode.For_allocations.from_lambda mode ~current_region ),
          arg ) ]
  | Punbox_int bi, [[arg]] ->
    let kind = boxable_number_of_boxed_integer bi in
    [Unary (Unbox_number kind, arg)]
  | Pbox_int (bi, mode), [[arg]] ->
    let kind = boxable_number_of_boxed_integer bi in
    [ Unary
        ( Box_number
            (kind, Alloc_mode.For_allocations.from_lambda mode ~current_region),
          arg ) ]
  | Pfield_computed sem, [[obj]; [field]] ->
    let block_access : P.Block_access_kind.t =
      Values { tag = Unknown; size = Unknown; field_kind = Any_value }
    in
    [ Binary
        (Block_load (block_access, convert_field_read_semantics sem), obj, field)
    ]
  | ( Psetfield_computed (imm_or_pointer, init_or_assign),
      [[obj]; [field]; [value]] ) ->
    let field_kind = convert_block_access_field_kind imm_or_pointer in
    let block_access : P.Block_access_kind.t =
      Values { tag = Unknown; size = Unknown; field_kind }
    in
    [ Ternary
        ( Block_set (block_access, convert_init_or_assign init_or_assign),
          obj,
          field,
          value ) ]
  | Parraylength kind, [[arg]] ->
    let array_kind = convert_array_kind_for_length kind in
    [Unary (Array_length array_kind, arg)]
  | Pduparray (kind, mutability), [[arg]] -> (
    let duplicate_array_kind =
      convert_array_kind_to_duplicate_array_kind kind
    in
    let source_mutability = Mutability.Immutable in
    let destination_mutability = Mutability.from_lambda mutability in
    match duplicate_array_kind with
    | Duplicate_array_kind duplicate_array_kind ->
      [ Unary
          ( Duplicate_array
              { kind = duplicate_array_kind;
                source_mutability;
                destination_mutability
              },
            arg ) ]
    | Float_array_opt_dynamic ->
      [ If_then_else
          ( Unary (Is_flat_float_array, arg),
            Unary
              ( Duplicate_array
                  { kind = Naked_floats { length = None };
                    source_mutability;
                    destination_mutability
                  },
                arg ),
            Unary
              ( Duplicate_array
                  { kind = Values; source_mutability; destination_mutability },
                arg ) ) ])
  | Pstringlength, [[arg]] -> [tag_int (Unary (String_length String, arg))]
  | Pbyteslength, [[arg]] -> [tag_int (Unary (String_length Bytes, arg))]
  | Pstringrefu, [[str]; [index]] ->
    [ string_like_load ~unsafe:true ~dbg ~size_int ~access_size:Eight String
        None ~boxed:false str ~index_kind:Ptagged_int_index index
        ~current_region ]
  | Pbytesrefu, [[bytes]; [index]] ->
    [ string_like_load ~unsafe:true ~dbg ~size_int ~access_size:Eight Bytes None
        ~boxed:false bytes ~index_kind:Ptagged_int_index index ~current_region
    ]
  | Pstringrefs, [[str]; [index]] ->
    [ string_like_load ~unsafe:false ~dbg ~size_int ~access_size:Eight String
        ~boxed:false None str ~index_kind:Ptagged_int_index index
        ~current_region ]
  | Pbytesrefs, [[bytes]; [index]] ->
    [ string_like_load ~unsafe:false ~dbg ~size_int ~access_size:Eight Bytes
        ~boxed:false None bytes ~index_kind:Ptagged_int_index index
        ~current_region ]
  | Pstring_load_16 { unsafe; index_kind }, [[str]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Sixteen String
        ~boxed:false None str ~index_kind index ~current_region ]
  | Pbytes_load_16 { unsafe; index_kind }, [[bytes]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Sixteen Bytes
        ~boxed:false None bytes ~index_kind index ~current_region ]
  | Pstring_load_32 { unsafe; index_kind; mode; boxed }, [[str]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Thirty_two String
        ~boxed (Some mode) str ~index_kind index ~current_region ]
  | Pstring_load_f32 { unsafe; index_kind; mode; boxed }, [[str]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Single String ~boxed
        (Some mode) str ~index_kind index ~current_region ]
  | Pbytes_load_32 { unsafe; index_kind; mode; boxed }, [[bytes]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Thirty_two Bytes
        ~boxed (Some mode) bytes ~index_kind index ~current_region ]
  | Pbytes_load_f32 { unsafe; index_kind; mode; boxed }, [[bytes]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Single Bytes ~boxed
        (Some mode) bytes ~index_kind index ~current_region ]
  | Pstring_load_64 { unsafe; index_kind; mode; boxed }, [[str]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Sixty_four String
        ~boxed (Some mode) str ~index_kind index ~current_region ]
  | Pbytes_load_64 { unsafe; index_kind; mode; boxed }, [[bytes]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Sixty_four Bytes
        ~boxed (Some mode) bytes ~index_kind index ~current_region ]
  | Pstring_load_128 { unsafe; index_kind; mode; boxed }, [[str]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int
        ~access_size:(One_twenty_eight { aligned = false })
        String ~boxed (Some mode) str ~index_kind index ~current_region ]
  | Pbytes_load_128 { unsafe; index_kind; mode; boxed }, [[str]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int
        ~access_size:(One_twenty_eight { aligned = false })
        Bytes ~boxed (Some mode) str ~index_kind index ~current_region ]
  | Pbytes_set_16 { unsafe; index_kind }, [[bytes]; [index]; [new_value]] ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Sixteen Bytes
        ~boxed:false bytes ~index_kind index new_value ]
  | Pbytes_set_32 { unsafe; index_kind; boxed }, [[bytes]; [index]; [new_value]]
    ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Thirty_two Bytes ~boxed
        bytes ~index_kind index new_value ]
  | Pbytes_set_f32 { unsafe; index_kind; boxed }, [[bytes]; [index]; [new_value]]
    ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Single Bytes ~boxed
        bytes ~index_kind index new_value ]
  | Pbytes_set_64 { unsafe; index_kind; boxed }, [[bytes]; [index]; [new_value]]
    ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Sixty_four Bytes ~boxed
        bytes ~index_kind index new_value ]
  | Pbytes_set_128 { unsafe; index_kind; boxed }, [[bytes]; [index]; [new_value]]
    ->
    [ bytes_like_set ~unsafe ~dbg ~size_int
        ~access_size:(One_twenty_eight { aligned = false })
        Bytes ~boxed bytes ~index_kind index new_value ]
  | Pisint { variant_only }, [[arg]] ->
    [tag_int (Unary (Is_int { variant_only }, arg))]
  | Pisout, [[arg1]; [arg2]] ->
    [ tag_int
        (Binary
           ( Int_comp (I.Tagged_immediate, Yielding_bool (Lt Unsigned)),
             arg1,
             arg2 )) ]
  | Pbintofint (bi, mode), [[arg]] ->
    let dst = standard_int_or_float_of_boxed_integer bi in
    [ box_bint bi mode
        (Unary (Num_conv { src = I_or_f.Tagged_immediate; dst }, arg))
        ~current_region ]
  | Pintofbint bi, [[arg]] ->
    let src = standard_int_or_float_of_boxed_integer bi in
    [Unary (Num_conv { src; dst = I_or_f.Tagged_immediate }, unbox_bint bi arg)]
  | Pcvtbint (source, destination, mode), [[arg]] ->
    [ box_bint destination mode
        (Unary
           ( Num_conv
               { src = standard_int_or_float_of_boxed_integer source;
                 dst = standard_int_or_float_of_boxed_integer destination
               },
             unbox_bint source arg ))
        ~current_region ]
  | Pnegbint (bi, mode), [[arg]] ->
    [bint_unary_prim bi mode Neg arg ~current_region]
  | Paddbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_binary_prim bi mode Add arg1 arg2 ~current_region]
  | Psubbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_binary_prim bi mode Sub arg1 arg2 ~current_region]
  | Pmulbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_binary_prim bi mode Mul arg1 arg2 ~current_region]
  | Pandbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_binary_prim bi mode And arg1 arg2 ~current_region]
  | Porbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_binary_prim bi mode Or arg1 arg2 ~current_region]
  | Pxorbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_binary_prim bi mode Xor arg1 arg2 ~current_region]
  | Plslbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_shift bi mode Lsl arg1 arg2 ~current_region]
  | Plsrbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_shift bi mode Lsr arg1 arg2 ~current_region]
  | Pasrbint (bi, mode), [[arg1]; [arg2]] ->
    [bint_shift bi mode Asr arg1 arg2 ~current_region]
  | Poffsetint n, [[arg]] ->
    let const =
      Simple.const (Reg_width_const.tagged_immediate (Targetint_31_63.of_int n))
    in
    [Binary (Int_arith (I.Tagged_immediate, Add), arg, Simple const)]
  | Pfield (index, _int_or_ptr, sem), [[arg]] ->
    (* CR mshinwell: make use of the int-or-ptr flag (new in OCaml 5)? *)
    let imm = Targetint_31_63.of_int index in
    check_non_negative_imm imm "Pfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let mutability = convert_field_read_semantics sem in
    let block_access : P.Block_access_kind.t =
      Values { tag = Unknown; size = Unknown; field_kind = Any_value }
    in
    [Binary (Block_load (block_access, mutability), arg, Simple field)]
  | Pfloatfield (field, sem, mode), [[arg]] ->
    let imm = Targetint_31_63.of_int field in
    check_non_negative_imm imm "Pfloatfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let mutability = convert_field_read_semantics sem in
    let block_access : P.Block_access_kind.t =
      Naked_floats { size = Unknown }
    in
    [ box_float mode
        (Binary (Block_load (block_access, mutability), arg, Simple field))
        ~current_region ]
  | Pufloatfield (field, sem), [[arg]] ->
    let imm = Targetint_31_63.of_int field in
    check_non_negative_imm imm "Pufloatfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let mutability = convert_field_read_semantics sem in
    let block_access : P.Block_access_kind.t =
      Naked_floats { size = Unknown }
    in
    [Binary (Block_load (block_access, mutability), arg, Simple field)]
  | Pmixedfield (field, read, shape, sem), [[arg]] -> (
    let imm = Targetint_31_63.of_int field in
    check_non_negative_imm imm "Pmixedfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let mutability = convert_field_read_semantics sem in
    let block_access : P.Block_access_kind.t =
      let field_kind : P.Mixed_block_access_field_kind.t =
        match read with
        (* CR mshinwell: make use of the int-or-ptr flag (new in OCaml 5)? *)
        | Mread_value_prefix _int_or_pointer -> Value_prefix Any_value
        | Mread_flat_suffix read ->
          Flat_suffix
            (match read with
            | Flat_read flat_element ->
              K.Flat_suffix_element.from_lambda flat_element
            | Flat_read_float_boxed _ -> K.Flat_suffix_element.naked_float)
      in
      let shape = K.Mixed_block_shape.from_lambda shape in
      Mixed { tag = Unknown; field_kind; shape; size = Unknown }
    in
    let block_access : H.expr_primitive =
      Binary (Block_load (block_access, mutability), arg, Simple field)
    in
    match read with
    | Mread_value_prefix _ | Mread_flat_suffix (Flat_read _) -> [block_access]
    | Mread_flat_suffix (Flat_read_float_boxed mode) ->
      [box_float mode block_access ~current_region])
  | ( Psetfield (index, immediate_or_pointer, initialization_or_assignment),
      [[block]; [value]] ) ->
    let field_kind = convert_block_access_field_kind immediate_or_pointer in
    let imm = Targetint_31_63.of_int index in
    check_non_negative_imm imm "Psetfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let init_or_assign = convert_init_or_assign initialization_or_assignment in
    let block_access : P.Block_access_kind.t =
      Values { tag = Unknown; size = Unknown; field_kind }
    in
    [ Ternary
        (Block_set (block_access, init_or_assign), block, Simple field, value)
    ]
  | Psetfloatfield (field, initialization_or_assignment), [[block]; [value]] ->
    let imm = Targetint_31_63.of_int field in
    check_non_negative_imm imm "Psetfloatfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let block_access : P.Block_access_kind.t =
      Naked_floats { size = Unknown }
    in
    let init_or_assign = convert_init_or_assign initialization_or_assignment in
    [ Ternary
        ( Block_set (block_access, init_or_assign),
          block,
          Simple field,
          unbox_float value ) ]
  | Psetufloatfield (field, initialization_or_assignment), [[block]; [value]] ->
    let imm = Targetint_31_63.of_int field in
    check_non_negative_imm imm "Psetufloatfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let block_access : P.Block_access_kind.t =
      Naked_floats { size = Unknown }
    in
    let init_or_assign = convert_init_or_assign initialization_or_assignment in
    [ Ternary
        (Block_set (block_access, init_or_assign), block, Simple field, value)
    ]
  | ( Psetmixedfield (field, write, shape, initialization_or_assignment),
      [[block]; [value]] ) ->
    let imm = Targetint_31_63.of_int field in
    check_non_negative_imm imm "Psetmixedfield";
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let block_access : P.Block_access_kind.t =
      Mixed
        { field_kind =
            (match write with
            | Mwrite_value_prefix immediate_or_pointer ->
              Value_prefix
                (convert_block_access_field_kind immediate_or_pointer)
            | Mwrite_flat_suffix flat ->
              Flat_suffix (K.Flat_suffix_element.from_lambda flat));
          shape = K.Mixed_block_shape.from_lambda shape;
          tag = Unknown;
          size = Unknown
        }
    in
    let init_or_assign = convert_init_or_assign initialization_or_assignment in
    let value =
      match write with
      | Mwrite_value_prefix _
      | Mwrite_flat_suffix (Imm | Float64 | Float32 | Bits32 | Bits64 | Word) ->
        value
      | Mwrite_flat_suffix Float_boxed -> unbox_float value
    in
    [ Ternary
        (Block_set (block_access, init_or_assign), block, Simple field, value)
    ]
  | Pdivint Unsafe, [[arg1]; [arg2]] ->
    [Binary (Int_arith (I.Tagged_immediate, Div), arg1, arg2)]
  | Pdivint Safe, [[arg1]; [arg2]] ->
    [checked_arith_op ~dbg None Div None arg1 arg2 ~current_region]
  | Pmodint Safe, [[arg1]; [arg2]] ->
    [checked_arith_op ~dbg None Mod None arg1 arg2 ~current_region]
  | Pdivbint { size = Pint32; is_safe = Safe; mode }, [[arg1]; [arg2]] ->
    [ checked_arith_op ~dbg (Some Pint32) Div (Some mode) arg1 arg2
        ~current_region ]
  | Pmodbint { size = Pint32; is_safe = Safe; mode }, [[arg1]; [arg2]] ->
    [ checked_arith_op ~dbg (Some Pint32) Mod (Some mode) arg1 arg2
        ~current_region ]
  | Pdivbint { size = Pint64; is_safe = Safe; mode }, [[arg1]; [arg2]] ->
    [ checked_arith_op ~dbg (Some Pint64) Div (Some mode) arg1 arg2
        ~current_region ]
  | Pmodbint { size = Pint64; is_safe = Safe; mode }, [[arg1]; [arg2]] ->
    [ checked_arith_op ~dbg (Some Pint64) Mod (Some mode) arg1 arg2
        ~current_region ]
  | Pdivbint { size = Pnativeint; is_safe = Safe; mode }, [[arg1]; [arg2]] ->
    [ checked_arith_op ~dbg (Some Pnativeint) Div (Some mode) arg1 arg2
        ~current_region ]
  | Pmodbint { size = Pnativeint; is_safe = Safe; mode }, [[arg1]; [arg2]] ->
    [ checked_arith_op ~dbg (Some Pnativeint) Mod (Some mode) arg1 arg2
        ~current_region ]
  | Parrayrefu (array_ref_kind, array_index_kind), [[array]; [index]] ->
    (* For this and the following cases we will end up relying on the backend to
       CSE the two accesses to the array's header word in the [Pgenarray]
       case. *)
    [ match_on_array_ref_kind ~array array_ref_kind
        (array_load_unsafe ~array
           ~index:(convert_index_to_tagged_int index array_index_kind)
           ~current_region) ]
  | Parrayrefs (array_ref_kind, array_index_kind), [[array]; [index]] ->
    let array_kind = convert_array_ref_kind_for_length array_ref_kind in
    [ check_array_access ~dbg ~array array_kind ~index
        ~index_kind:array_index_kind
        (match_on_array_ref_kind ~array array_ref_kind
           (array_load_unsafe ~array
              ~index:(convert_index_to_tagged_int index array_index_kind)
              ~current_region)) ]
  | ( Parraysetu (array_set_kind, array_index_kind),
      [[array]; [index]; [new_value]] ) ->
    [ match_on_array_set_kind ~array array_set_kind
        (array_set_unsafe ~array
           ~index:(convert_index_to_tagged_int index array_index_kind)
           ~new_value) ]
  | ( Parraysets (array_set_kind, array_index_kind),
      [[array]; [index]; [new_value]] ) ->
    let array_kind = convert_array_set_kind_for_length array_set_kind in
    [ check_array_access ~dbg ~array array_kind ~index
        ~index_kind:array_index_kind
        (match_on_array_set_kind ~array array_set_kind
           (array_set_unsafe ~array
              ~index:(convert_index_to_tagged_int index array_index_kind)
              ~new_value)) ]
  | Pbytessetu (* unsafe *), [[bytes]; [index]; [new_value]] ->
    [ bytes_like_set ~unsafe:true ~dbg ~size_int ~access_size:Eight Bytes
        ~boxed:false bytes ~index_kind:Ptagged_int_index index new_value ]
  | Pbytessets, [[bytes]; [index]; [new_value]] ->
    [ bytes_like_set ~unsafe:false ~dbg ~size_int ~access_size:Eight Bytes
        ~boxed:false bytes ~index_kind:Ptagged_int_index index new_value ]
  | Poffsetref n, [[block]] ->
    let block_access : P.Block_access_kind.t =
      Values
        { tag = Known Tag.Scannable.zero;
          size = Known Targetint_31_63.one;
          field_kind = Immediate
        }
    in
    let old_ref_value =
      H.Prim
        (Binary
           (Block_load (block_access, Mutable), block, Simple Simple.const_zero))
    in
    let new_ref_value =
      H.Prim
        (Binary
           ( Int_arith (Tagged_immediate, Add),
             Simple (Simple.const_int (Targetint_31_63.of_int n)),
             old_ref_value ))
    in
    [ Ternary
        ( Block_set
            (block_access, Assignment (Alloc_mode.For_assignments.local ())),
          block,
          Simple Simple.const_zero,
          new_ref_value ) ]
  | Pctconst const, _ -> (
    match const with
    | Big_endian -> [Simple (Simple.const_bool big_endian)]
    | Word_size ->
      [Simple (Simple.const_int (Targetint_31_63.of_int (8 * size_int)))]
    | Int_size ->
      [Simple (Simple.const_int (Targetint_31_63.of_int ((8 * size_int) - 1)))]
    | Max_wosize ->
      [ Simple
          (Simple.const_int
             (Targetint_31_63.of_int
                ((1 lsl ((8 * size_int) - (10 + Config.reserved_header_bits)))
                - 1))) ]
    | Ostype_unix ->
      [Simple (Simple.const_bool (String.equal Sys.os_type "Unix"))]
    | Ostype_win32 ->
      [Simple (Simple.const_bool (String.equal Sys.os_type "Win32"))]
    | Ostype_cygwin ->
      [Simple (Simple.const_bool (String.equal Sys.os_type "Cygwin"))]
    | Backend_type ->
      [Simple Simple.const_zero] (* constructor 0 is the same as Native here *)
    | Runtime5 -> [Simple (Simple.const_bool Config.runtime5)])
  | Pbswap16, [[arg]] ->
    [ tag_int
        (Unary (Int_arith (Naked_immediate, Swap_byte_endianness), untag_int arg))
    ]
  | Pbbswap (Pint32, mode), [[arg]] ->
    [bbswap Naked_int32 Naked_int32 mode arg ~current_region]
  | Pbbswap (Pint64, mode), [[arg]] ->
    [bbswap Naked_int64 Naked_int64 mode arg ~current_region]
  | Pbbswap (Pnativeint, mode), [[arg]] ->
    [bbswap Naked_nativeint Naked_nativeint mode arg ~current_region]
  | Pint_as_pointer mode, [[arg]] ->
    (* This is not a stack allocation, but nonetheless has a region
       constraint. *)
    let mode =
      Alloc_mode.For_allocations.from_lambda mode
        ~current_region:current_ghost_region
    in
    [Unary (Int_as_pointer mode, arg)]
  | Pbigarrayref (unsafe, num_dimensions, kind, layout), args -> (
    let args =
      List.map
        (function
          | [arg] -> arg
          | [] | _ :: _ :: _ ->
            Misc.fatal_errorf "Non-singleton arguments for Pbigarrayref: %a %a"
              Printlambda.primitive prim H.print_list_of_lists_of_simple_or_prim
              args)
        args
    in
    match
      P.Bigarray_kind.from_lambda kind, P.Bigarray_layout.from_lambda layout
    with
    | Some kind, Some layout ->
      let b, indexes =
        match args with
        | b :: indexes ->
          if List.compare_length_with indexes num_dimensions <> 0
          then Misc.fatal_errorf "Bad index arity for Pbigarrayref";
          b, indexes
        | [] -> Misc.fatal_errorf "Pbigarrayref is missing its arguments"
      in
      let box =
        bigarray_box_or_tag_raw_value_to_read kind
          Alloc_mode.For_allocations.heap
      in
      [box (bigarray_load ~dbg ~unsafe kind layout b indexes)]
    | None, _ ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown kind should have been removed by Lambda_to_flambda."
    | _, None ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown layout should have been removed by Lambda_to_flambda.")
  | Pbigarrayset (unsafe, num_dimensions, kind, layout), args -> (
    let args =
      List.map
        (function
          | [arg] -> arg
          | [] | _ :: _ :: _ ->
            Misc.fatal_errorf "Non-singleton arguments for Pbigarrayset: %a %a"
              Printlambda.primitive prim H.print_list_of_lists_of_simple_or_prim
              args)
        args
    in
    match
      P.Bigarray_kind.from_lambda kind, P.Bigarray_layout.from_lambda layout
    with
    | Some kind, Some layout ->
      let b, indexes, value =
        match args with
        | b :: args ->
          let indexes, value = Misc.split_last args in
          if List.compare_length_with indexes num_dimensions <> 0
          then Misc.fatal_errorf "Bad index arity for Pbigarrayset";
          b, indexes, value
        | [] -> Misc.fatal_errorf "Pbigarrayset is missing its arguments"
      in
      let unbox = bigarray_unbox_or_untag_value_to_store kind in
      [bigarray_set ~dbg ~unsafe kind layout b indexes (unbox value)]
    | None, _ ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown kind should have been removed by Lambda_to_flambda."
    | _, None ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown layout should have been removed by Lambda_to_flambda.")
  | Pbigarraydim dimension, [[arg]] ->
    [tag_int (Unary (Bigarray_length { dimension }, arg))]
  | Pbigstring_load_16 { unsafe; index_kind }, [[big_str]; [index]] ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Sixteen Bigstring
        ~boxed:false None big_str ~index_kind index ~current_region ]
  | Pbigstring_load_32 { unsafe; index_kind; mode; boxed }, [[big_str]; [index]]
    ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Thirty_two Bigstring
        (Some mode) ~boxed big_str ~index_kind index ~current_region ]
  | Pbigstring_load_f32 { unsafe; index_kind; mode; boxed }, [[big_str]; [index]]
    ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Single Bigstring
        (Some mode) ~boxed big_str ~index_kind index ~current_region ]
  | Pbigstring_load_64 { unsafe; index_kind; mode; boxed }, [[big_str]; [index]]
    ->
    [ string_like_load ~unsafe ~dbg ~size_int ~access_size:Sixty_four Bigstring
        (Some mode) ~boxed big_str ~index_kind index ~current_region ]
  | ( Pbigstring_load_128 { unsafe; aligned; index_kind; mode; boxed },
      [[big_str]; [index]] ) ->
    [ string_like_load ~unsafe ~dbg ~size_int
        ~access_size:(One_twenty_eight { aligned })
        Bigstring (Some mode) ~boxed big_str ~index_kind index ~current_region
    ]
  | Pbigstring_set_16 { unsafe; index_kind }, [[bigstring]; [index]; [new_value]]
    ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Sixteen Bigstring
        ~boxed:false bigstring ~index_kind index new_value ]
  | ( Pbigstring_set_32 { unsafe; index_kind; boxed },
      [[bigstring]; [index]; [new_value]] ) ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Thirty_two Bigstring
        ~boxed bigstring ~index_kind index new_value ]
  | ( Pbigstring_set_f32 { unsafe; index_kind; boxed },
      [[bigstring]; [index]; [new_value]] ) ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Single Bigstring ~boxed
        bigstring ~index_kind index new_value ]
  | ( Pbigstring_set_64 { unsafe; index_kind; boxed },
      [[bigstring]; [index]; [new_value]] ) ->
    [ bytes_like_set ~unsafe ~dbg ~size_int ~access_size:Sixty_four Bigstring
        ~boxed bigstring ~index_kind index new_value ]
  | ( Pbigstring_set_128 { unsafe; aligned; index_kind; boxed },
      [[bigstring]; [index]; [new_value]] ) ->
    [ bytes_like_set ~unsafe ~dbg ~size_int
        ~access_size:(One_twenty_eight { aligned })
        Bigstring ~boxed bigstring ~index_kind index new_value ]
  | Pfloat_array_load_128 { unsafe; mode; boxed }, [[array]; [index]] ->
    check_float_array_optimisation_enabled "Pfloat_array_load_128";
    [ array_like_load_128 ~dbg ~size_int ~current_region ~unsafe ~mode ~boxed
        Naked_floats array index ]
  | Pfloatarray_load_128 { unsafe; mode; boxed }, [[array]; [index]]
  | Punboxed_float_array_load_128 { unsafe; mode; boxed }, [[array]; [index]] ->
    [ array_like_load_128 ~dbg ~size_int ~current_region ~unsafe ~mode ~boxed
        Naked_floats array index ]
  | Punboxed_float32_array_load_128 { unsafe; mode; boxed }, [[array]; [index]]
    ->
    [ array_like_load_128 ~dbg ~size_int ~current_region ~unsafe ~mode ~boxed
        Naked_float32s array index ]
  | Pint_array_load_128 { unsafe; mode; boxed }, [[array]; [index]] ->
    if Targetint.size <> 64
    then Misc.fatal_error "[Pint_array_load_128]: immediates must be 64 bits.";
    [ array_like_load_128 ~dbg ~size_int ~current_region ~unsafe ~mode ~boxed
        Immediates array index ]
  | Punboxed_int64_array_load_128 { unsafe; mode; boxed }, [[array]; [index]] ->
    [ array_like_load_128 ~dbg ~size_int ~current_region ~unsafe ~mode ~boxed
        Naked_int64s array index ]
  | Punboxed_nativeint_array_load_128 { unsafe; mode; boxed }, [[array]; [index]]
    ->
    if Targetint.size <> 64
    then
      Misc.fatal_error
        "[Punboxed_nativeint_array_load_128]: nativeint must be 64 bits.";
    [ array_like_load_128 ~dbg ~size_int ~current_region ~unsafe ~mode ~boxed
        Naked_nativeints array index ]
  | Punboxed_int32_array_load_128 { unsafe; mode; boxed }, [[array]; [index]] ->
    [ array_like_load_128 ~dbg ~size_int ~current_region ~unsafe ~mode ~boxed
        Naked_int32s array index ]
  | Pfloat_array_set_128 { unsafe; boxed }, [[array]; [index]; [new_value]] ->
    check_float_array_optimisation_enabled "Pfloat_array_set_128";
    [ array_like_set_128 ~dbg ~size_int ~unsafe ~boxed Naked_floats array index
        new_value ]
  | Pfloatarray_set_128 { unsafe; boxed }, [[array]; [index]; [new_value]]
  | ( Punboxed_float_array_set_128 { unsafe; boxed },
      [[array]; [index]; [new_value]] ) ->
    [ array_like_set_128 ~dbg ~size_int ~unsafe ~boxed Naked_floats array index
        new_value ]
  | ( Punboxed_float32_array_set_128 { unsafe; boxed },
      [[array]; [index]; [new_value]] ) ->
    [ array_like_set_128 ~dbg ~size_int ~unsafe ~boxed Naked_float32s array
        index new_value ]
  | Pint_array_set_128 { unsafe; boxed }, [[array]; [index]; [new_value]] ->
    if Targetint.size <> 64
    then Misc.fatal_error "[Pint_array_set_128]: immediates must be 64 bits.";
    [ array_like_set_128 ~dbg ~size_int ~unsafe ~boxed Immediates array index
        new_value ]
  | ( Punboxed_int64_array_set_128 { unsafe; boxed },
      [[array]; [index]; [new_value]] ) ->
    [ array_like_set_128 ~dbg ~size_int ~unsafe ~boxed Naked_int64s array index
        new_value ]
  | ( Punboxed_nativeint_array_set_128 { unsafe; boxed },
      [[array]; [index]; [new_value]] ) ->
    if Targetint.size <> 64
    then
      Misc.fatal_error
        "[Punboxed_nativeint_array_set_128]: nativeint must be 64 bits.";
    [ array_like_set_128 ~dbg ~size_int ~unsafe ~boxed Naked_nativeints array
        index new_value ]
  | ( Punboxed_int32_array_set_128 { unsafe; boxed },
      [[array]; [index]; [new_value]] ) ->
    [ array_like_set_128 ~dbg ~size_int ~unsafe ~boxed Naked_int32s array index
        new_value ]
  | Pcompare_ints, [[i1]; [i2]] ->
    [ tag_int
        (Binary
           ( Int_comp
               (Tagged_immediate, Yielding_int_like_compare_functions Signed),
             i1,
             i2 )) ]
  | Pcompare_floats Pfloat64, [[f1]; [f2]] ->
    [ tag_int
        (Binary
           ( Float_comp (Float64, Yielding_int_like_compare_functions ()),
             Prim (Unary (Unbox_number Naked_float, f1)),
             Prim (Unary (Unbox_number Naked_float, f2)) )) ]
  | Pcompare_floats Pfloat32, [[f1]; [f2]] ->
    [ tag_int
        (Binary
           ( Float_comp (Float32, Yielding_int_like_compare_functions ()),
             Prim (Unary (Unbox_number Naked_float32, f1)),
             Prim (Unary (Unbox_number Naked_float32, f2)) )) ]
  | Pcompare_bints int_kind, [[i1]; [i2]] ->
    let unboxing_kind = boxable_number_of_boxed_integer int_kind in
    [ tag_int
        (Binary
           ( Int_comp
               ( standard_int_of_boxed_integer int_kind,
                 Yielding_int_like_compare_functions Signed ),
             Prim (Unary (Unbox_number unboxing_kind, i1)),
             Prim (Unary (Unbox_number unboxing_kind, i2)) )) ]
  | Pprobe_is_enabled { name }, [] ->
    [tag_int (Nullary (Probe_is_enabled { name }))]
  | Pobj_dup, [[v]] -> [Unary (Obj_dup, v)]
  | Pget_header m, [[obj]] -> [get_header obj m ~current_region]
  | Patomic_load { immediate_or_pointer }, [[atomic]] ->
    [ Unary
        ( Atomic_load (convert_block_access_field_kind immediate_or_pointer),
          atomic ) ]
  | Patomic_exchange, [[atomic]; [new_value]] ->
    [Binary (Atomic_exchange, atomic, new_value)]
  | Patomic_cas, [[atomic]; [old_value]; [new_value]] ->
    [Ternary (Atomic_compare_and_set, atomic, old_value, new_value)]
  | Patomic_fetch_add, [[atomic]; [i]] ->
    [Binary (Atomic_fetch_and_add, atomic, i)]
  | Pdls_get, _ -> [Nullary Dls_get]
  | Preinterpret_unboxed_int64_as_tagged_int63, [[i]] ->
    if not (Target_system.is_64_bit ())
    then
      Misc.fatal_error
        "Preinterpret_unboxed_int64_as_tagged_int63 can only be used on 64-bit \
         targets";
    [Unary (Reinterpret_64_bit_word Unboxed_int64_as_tagged_int63, i)]
  | Preinterpret_tagged_int63_as_unboxed_int64, [[i]] ->
    if not (Target_system.is_64_bit ())
    then
      Misc.fatal_error
        "Preinterpret_tagged_int63_as_unboxed_int64 can only be used on 64-bit \
         targets";
    [Unary (Reinterpret_64_bit_word Tagged_int63_as_unboxed_int64, i)]
  | ( ( Pmodint Unsafe
      | Pdivbint { is_safe = Unsafe; size = _; mode = _ }
      | Pmodbint { is_safe = Unsafe; size = _; mode = _ }
      | Psetglobal _ | Praise _ | Pccall _ ),
      _ ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Primitive %a (%a) shouldn't be \
       here, either a bug in [Closure_conversion] or the wrong number of \
       arguments"
      Printlambda.primitive prim H.print_list_of_lists_of_simple_or_prim args
  | Pprobe_is_enabled _, _ :: _ ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for nullary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_simple_or_prim
      (List.flatten args)
  | ( ( Pfield _ | Pnegint | Pnot | Poffsetint _ | Pintoffloat _
      | Pfloatofint (_, _)
      | Pfloatoffloat32 _ | Pfloat32offloat _
      | Pnegfloat (_, _)
      | Pabsfloat (_, _)
      | Pstringlength | Pbyteslength | Pbintofint _ | Pintofbint _ | Pnegbint _
      | Popaque _ | Pduprecord _ | Parraylength _ | Pduparray _ | Pfloatfield _
      | Pcvtbint _ | Poffsetref _ | Pbswap16 | Pbbswap _ | Pisint _
      | Pint_as_pointer _ | Pbigarraydim _ | Pobj_dup | Pobj_magic _
      | Punbox_float _
      | Pbox_float (_, _)
      | Punbox_int _ | Pbox_int _ | Punboxed_product_field _ | Pget_header _
      | Pufloatfield _ | Patomic_load _ | Pmixedfield _
      | Preinterpret_unboxed_int64_as_tagged_int63
      | Preinterpret_tagged_int63_as_unboxed_int64 ),
      ([] | _ :: _ :: _ | [([] | _ :: _ :: _)]) ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for unary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_lists_of_simple_or_prim args
  | ( ( Paddint | Psubint | Pmulint | Pandint | Porint | Pxorint | Plslint
      | Plsrint | Pasrint | Pdivint _ | Pmodint _ | Psetfield _ | Pintcomp _
      | Paddfloat (_, _)
      | Psubfloat (_, _)
      | Pmulfloat (_, _)
      | Pdivfloat (_, _)
      | Pfloatcomp (_, _)
      | Punboxed_float_comp (_, _)
      | Pstringrefu | Pbytesrefu | Pstringrefs | Pbytesrefs | Pstring_load_16 _
      | Pstring_load_32 _ | Pstring_load_f32 _ | Pstring_load_64 _
      | Pstring_load_128 _ | Pbytes_load_16 _ | Pbytes_load_32 _
      | Pbytes_load_f32 _ | Pbytes_load_64 _ | Pbytes_load_128 _ | Pisout
      | Paddbint _ | Psubbint _ | Pmulbint _ | Pandbint _ | Porbint _
      | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _ | Pfield_computed _
      | Pdivbint _ | Pmodbint _ | Psetfloatfield _ | Psetufloatfield _
      | Pbintcomp _ | Punboxed_int_comp _ | Psetmixedfield _
      | Pbigstring_load_16 _ | Pbigstring_load_32 _ | Pbigstring_load_f32 _
      | Pbigstring_load_64 _ | Pbigstring_load_128 _ | Pfloatarray_load_128 _
      | Pfloat_array_load_128 _ | Pint_array_load_128 _
      | Punboxed_float_array_load_128 _ | Punboxed_float32_array_load_128 _
      | Punboxed_int32_array_load_128 _ | Punboxed_int64_array_load_128 _
      | Punboxed_nativeint_array_load_128 _
      | Parrayrefu
          ( ( Pgenarray_ref _ | Paddrarray_ref | Pintarray_ref
            | Pfloatarray_ref _ | Punboxedfloatarray_ref _
            | Punboxedintarray_ref _ ),
            _ )
      | Parrayrefs
          ( ( Pgenarray_ref _ | Paddrarray_ref | Pintarray_ref
            | Pfloatarray_ref _ | Punboxedfloatarray_ref _
            | Punboxedintarray_ref _ ),
            _ )
      | Pcompare_ints | Pcompare_floats _ | Pcompare_bints _ | Patomic_exchange
      | Patomic_fetch_add ),
      ( []
      | [_]
      | _ :: _ :: _ :: _
      | [_; ([] | _ :: _ :: _)]
      | [([] | _ :: _ :: _); _] ) ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for binary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_lists_of_simple_or_prim args
  | ( ( Psetfield_computed _ | Pbytessetu | Pbytessets
      | Parraysetu
          ( ( Pgenarray_set _ | Paddrarray_set _ | Pintarray_set
            | Pfloatarray_set | Punboxedfloatarray_set _
            | Punboxedintarray_set _ ),
            _ )
      | Parraysets
          ( ( Pgenarray_set _ | Paddrarray_set _ | Pintarray_set
            | Pfloatarray_set | Punboxedfloatarray_set _
            | Punboxedintarray_set _ ),
            _ )
      | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_f32 _ | Pbytes_set_64 _
      | Pbytes_set_128 _ | Pbigstring_set_16 _ | Pbigstring_set_32 _
      | Pbigstring_set_f32 _ | Pbigstring_set_64 _ | Pbigstring_set_128 _
      | Pfloatarray_set_128 _ | Pfloat_array_set_128 _ | Pint_array_set_128 _
      | Punboxed_float_array_set_128 _ | Punboxed_float32_array_set_128 _
      | Punboxed_int32_array_set_128 _ | Punboxed_int64_array_set_128 _
      | Punboxed_nativeint_array_set_128 _ | Patomic_cas ),
      ( []
      | [_]
      | [_; _]
      | _ :: _ :: _ :: _ :: _
      | [_; _; ([] | _ :: _ :: _)]
      | [_; ([] | _ :: _ :: _); _]
      | [([] | _ :: _ :: _); _; _] ) ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for ternary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_lists_of_simple_or_prim args
  | ( ( Pignore | Psequand | Psequor | Pbytes_of_string | Pbytes_to_string
      | Parray_of_iarray | Parray_to_iarray | Prunstack | Pperform | Presume
      | Preperform ),
      _ ) ->
    Misc.fatal_errorf
      "[%a] should have been removed by [Lambda_to_flambda.transform_primitive]"
      Printlambda.primitive prim
  | Pgetglobal _, _ | Pgetpredef _, _ ->
    Misc.fatal_errorf
      "[%a] should have been handled by [Closure_conversion.close_primitive]"
      Printlambda.primitive prim

module Acc = Closure_conversion_aux.Acc
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

let convert_and_bind acc ~big_endian exn_cont ~register_const0
    (prim : L.primitive) ~(args : Simple.t list list) (dbg : Debuginfo.t)
    ~current_region ~current_ghost_region
    (cont : Acc.t -> Flambda.Named.t list -> Expr_with_acc.t) : Expr_with_acc.t
    =
  let exprs =
    convert_lprim ~big_endian prim args dbg ~current_region
      ~current_ghost_region
  in
  H.bind_recs acc exn_cont ~register_const0 exprs dbg cont
