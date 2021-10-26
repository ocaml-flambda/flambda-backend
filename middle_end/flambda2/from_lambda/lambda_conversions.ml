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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind
module KS = Flambda_kind.With_subkind
module L = Lambda
module P = Flambda_primitive

let check_float_array_optimisation_enabled () =
  if not (Flambda_features.flat_float_array ())
  then
    Misc.fatal_error
      "[Pgenarray] is not expected when the float array optimisation is \
       disabled"

let rec value_kind (vk : L.value_kind) =
  match vk with
  | Pgenval -> KS.any_value
  | Pfloatval -> KS.boxed_float
  | Pboxedintval Pint32 -> KS.boxed_int32
  | Pboxedintval Pint64 -> KS.boxed_int64
  | Pboxedintval Pnativeint -> KS.boxed_nativeint
  | Pintval -> KS.tagged_immediate
  | Pblock { tag; fields } ->
    (* If we have [Obj.double_array_tag] here, this is always an all-float
       block, not an array. *)
    if tag = Obj.double_array_tag
    then KS.float_block ~num_fields:(List.length fields)
    else KS.block (Tag.create_exn tag) (List.map value_kind fields)

let inline_attribute (attr : L.inline_attribute) : Inline_attribute.t =
  match attr with
  | Always_inline -> Always_inline
  | Never_inline -> Never_inline
  | Ready_inline -> Ready_inline
  | Unroll i -> Unroll i
  | Default_inline -> Default_inline

let inlined_attribute (attr : L.inlined_attribute) : Inlined_attribute.t =
  match attr with
  | Always_inlined -> Always_inlined
  | Never_inlined -> Never_inlined
  | Hint_inlined -> Hint_inlined
  | Unroll i -> Unroll i
  | Default_inlined -> Default_inlined

let kind_of_primitive_native_repr (repr : Primitive.native_repr) =
  match repr with
  | Same_as_ocaml_repr -> K.value
  | Unboxed_float -> K.naked_float
  | Unboxed_integer Pnativeint -> K.naked_nativeint
  | Unboxed_integer Pint32 -> K.naked_int32
  | Unboxed_integer Pint64 -> K.naked_int64
  | Untagged_int -> K.naked_immediate

let method_kind (kind : L.meth_kind) : Call_kind.method_kind =
  match kind with Self -> Self | Public -> Public | Cached -> Cached

let raise_kind (kind : L.raise_kind) : Trap_action.raise_kind =
  match kind with
  | Raise_regular -> Regular
  | Raise_reraise -> Reraise
  | Raise_notrace -> No_trace

let convert_block_of_values_field (value_kind : L.value_kind) :
    P.Block_of_values_field.t =
  match value_kind with
  | Pgenval -> Any_value
  | Pfloatval -> Boxed_float
  | Pboxedintval Pint32 -> Boxed_int32
  | Pboxedintval Pint64 -> Boxed_int64
  | Pboxedintval Pnativeint -> Boxed_nativeint
  | Pintval -> Immediate
  | Pblock _ -> Any_value

let convert_block_shape (shape : L.block_shape) ~num_fields =
  match shape with
  | None ->
    List.init num_fields (fun _field : P.Block_of_values_field.t -> Any_value)
  | Some shape ->
    let shape_length = List.length shape in
    if num_fields <> shape_length
    then
      Misc.fatal_errorf
        "Flambda_arity.of_block_shape: num_fields is %d yet the shape has %d \
         fields"
        num_fields shape_length;
    List.map convert_block_of_values_field shape

let convert_mutable_flag (flag : Lambda.mutable_flag) : Mutability.t =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable
  | Immutable_unique -> Immutable_unique

let convert_integer_comparison_prim (comp : L.integer_comparison) :
    P.binary_primitive =
  match comp with
  | Ceq -> Phys_equal (K.value, Eq)
  | Cne -> Phys_equal (K.value, Neq)
  | Clt -> Int_comp (Tagged_immediate, Signed, Yielding_bool Lt)
  | Cgt -> Int_comp (Tagged_immediate, Signed, Yielding_bool Gt)
  | Cle -> Int_comp (Tagged_immediate, Signed, Yielding_bool Le)
  | Cge -> Int_comp (Tagged_immediate, Signed, Yielding_bool Ge)

let convert_boxed_integer_comparison_prim (kind : L.boxed_integer)
    (comp : L.integer_comparison) : P.binary_primitive =
  match kind, comp with
  | Pint32, Ceq -> Phys_equal (K.naked_int32, Eq)
  | Pint32, Cne -> Phys_equal (K.naked_int32, Neq)
  | Pint32, Clt -> Int_comp (Naked_int32, Signed, Yielding_bool Lt)
  | Pint32, Cgt -> Int_comp (Naked_int32, Signed, Yielding_bool Gt)
  | Pint32, Cle -> Int_comp (Naked_int32, Signed, Yielding_bool Le)
  | Pint32, Cge -> Int_comp (Naked_int32, Signed, Yielding_bool Ge)
  | Pint64, Ceq -> Phys_equal (K.naked_int64, Eq)
  | Pint64, Cne -> Phys_equal (K.naked_int64, Neq)
  | Pint64, Clt -> Int_comp (Naked_int64, Signed, Yielding_bool Lt)
  | Pint64, Cgt -> Int_comp (Naked_int64, Signed, Yielding_bool Gt)
  | Pint64, Cle -> Int_comp (Naked_int64, Signed, Yielding_bool Le)
  | Pint64, Cge -> Int_comp (Naked_int64, Signed, Yielding_bool Ge)
  | Pnativeint, Ceq -> Phys_equal (K.naked_nativeint, Eq)
  | Pnativeint, Cne -> Phys_equal (K.naked_nativeint, Neq)
  | Pnativeint, Clt -> Int_comp (Naked_nativeint, Signed, Yielding_bool Lt)
  | Pnativeint, Cgt -> Int_comp (Naked_nativeint, Signed, Yielding_bool Gt)
  | Pnativeint, Cle -> Int_comp (Naked_nativeint, Signed, Yielding_bool Le)
  | Pnativeint, Cge -> Int_comp (Naked_nativeint, Signed, Yielding_bool Ge)

let convert_float_comparison (comp : L.float_comparison) : P.comparison =
  match comp with
  | CFeq -> Eq
  | CFneq -> Neq
  | CFlt -> Lt
  | CFgt -> Gt
  | CFle -> Le
  | CFge -> Ge
  | CFnlt | CFngt | CFnle | CFnge ->
    Misc.fatal_error
      "Negated floating-point comparisons should have been removed by \
       [Lambda_to_flambda]"

let boxable_number_of_boxed_integer (bint : L.boxed_integer) :
    Flambda_kind.Boxable_number.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let standard_int_of_boxed_integer (bint : L.boxed_integer) :
    Flambda_kind.Standard_int.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let standard_int_or_float_of_boxed_integer (bint : L.boxed_integer) :
    Flambda_kind.Standard_int_or_float.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let convert_block_access_field_kind i_or_p : P.Block_access_field_kind.t =
  match i_or_p with L.Immediate -> Immediate | L.Pointer -> Any_value

let convert_init_or_assign (i_or_a : L.initialization_or_assignment) :
    P.Init_or_assign.t =
  match i_or_a with
  | Assignment -> Assignment
  | Heap_initialization -> Initialization
  | Root_initialization ->
    Misc.fatal_error "[Root_initialization] should not appear in Flambda input"

let convert_array_kind (kind : L.array_kind) : P.Array_kind.t =
  match kind with
  | Pgenarray ->
    check_float_array_optimisation_enabled ();
    Float_array_opt_dynamic
  | Paddrarray -> Values
  | Pintarray -> Immediates
  | Pfloatarray -> Naked_floats

let convert_array_kind_to_duplicate_array_kind (kind : L.array_kind) :
    P.Duplicate_array_kind.t =
  match kind with
  | Pgenarray ->
    check_float_array_optimisation_enabled ();
    Float_array_opt_dynamic
  | Paddrarray -> Values
  | Pintarray -> Immediates
  | Pfloatarray -> Naked_floats { length = None }

let convert_bigarray_kind (kind : L.bigarray_kind) : P.bigarray_kind option =
  match kind with
  | Pbigarray_unknown -> None
  | Pbigarray_float32 -> Some Float32
  | Pbigarray_float64 -> Some Float64
  | Pbigarray_sint8 -> Some Sint8
  | Pbigarray_uint8 -> Some Uint8
  | Pbigarray_sint16 -> Some Sint16
  | Pbigarray_uint16 -> Some Uint16
  | Pbigarray_int32 -> Some Int32
  | Pbigarray_int64 -> Some Int64
  | Pbigarray_caml_int -> Some Int_width_int
  | Pbigarray_native_int -> Some Targetint_width_int
  | Pbigarray_complex32 -> Some Complex32
  | Pbigarray_complex64 -> Some Complex64

let convert_bigarray_layout (layout : L.bigarray_layout) :
    P.bigarray_layout option =
  match layout with
  | Pbigarray_unknown_layout -> None
  | Pbigarray_c_layout -> Some C
  | Pbigarray_fortran_layout -> Some Fortran

let convert_field_read_semantics (sem : L.field_read_semantics) : Mutability.t =
  match sem with Reads_agree -> Immutable | Reads_vary -> Mutable

let convert_lambda_block_size (size : int) : _ Or_unknown.t =
  Known (Targetint_31_63.Imm.of_int size)
