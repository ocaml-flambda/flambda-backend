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

[@@@ocaml.warning "+a-30-40-41-42"]

module C = Lambda_conversions
module H = Lambda_to_flambda_primitives_helpers
module I = Flambda_kind.Standard_int
module I_or_f = Flambda_kind.Standard_int_or_float
module K = Flambda_kind
module L = Lambda
module P = Flambda_primitive

let tag_int (arg : H.expr_primitive) : H.expr_primitive =
  Unary (Box_number Untagged_immediate, Prim arg)

let untag_int (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Unbox_number Untagged_immediate, arg))

let box_float (arg : H.expr_primitive) : H.expr_primitive =
  Unary (Box_number Flambda_kind.Boxable_number.Naked_float, Prim arg)

let unbox_float (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Unbox_number Flambda_kind.Boxable_number.Naked_float, arg))

let box_bint bi (arg : H.expr_primitive) : H.expr_primitive =
  Unary (Box_number (C.boxable_number_of_boxed_integer bi), Prim arg)

let unbox_bint bi (arg : H.simple_or_prim) : H.simple_or_prim =
  Prim (Unary (Unbox_number (C.boxable_number_of_boxed_integer bi), arg))

let tagged_immediate_as_naked_nativeint (arg : H.simple_or_prim) :
    H.simple_or_prim =
  arg
(* XXX *)

let bint_binary_prim bi prim arg1 arg2 =
  box_bint bi
    (Binary
       ( Int_arith (C.standard_int_of_boxed_integer bi, prim),
         unbox_bint bi arg1,
         unbox_bint bi arg2 ))

let bint_shift bi prim arg1 arg2 =
  box_bint bi
    (Binary
       ( Int_shift (C.standard_int_of_boxed_integer bi, prim),
         unbox_bint bi arg1,
         untag_int arg2 ))

(* offset to subtract from a string length depending on the size of the
   read/write *)
let length_offset_of_size size =
  let offset =
    match (size : Flambda_primitive.string_accessor_width) with
    | Eight -> 0
    | Sixteen -> 1
    | Thirty_two -> 3
    | Sixty_four -> 7
  in
  Targetint_31_63.int (Targetint_31_63.Imm.of_int offset)

(* This computes the maximum of a given value [x] with zero, in an optimized
   way. It takes as named argument the size (in bytes) of an integer register on
   the target architecture.

   It is equivalent to the `max_or_zero` function in `cmm_helpers.ml` *)
let max_with_zero ~size_int x =
  let register_bitsize_minus_one =
    H.Simple
      (Simple.const
         (Reg_width_const.naked_immediate
            (Targetint_31_63.int
               (Targetint_31_63.Imm.of_int ((size_int * 8) - 1)))))
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

let array_access_validity_condition array index =
  [ H.Binary
      ( Int_comp (Tagged_immediate, Unsigned, Yielding_bool Lt),
        index,
        Prim (Unary (Array_length, array)) ) ]

(* actual (strict) upper bound for an index in a string read/write *)
let actual_max_length ~size_int ~access_size length =
  match (access_size : Flambda_primitive.string_accessor_width) with
  | Eight -> length (* micro-optimization *)
  | Sixteen | Thirty_two | Sixty_four ->
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

let string_or_bytes_access_validity_condition ~size_int str kind access_size
    index : H.expr_primitive =
  Binary
    ( Int_comp (I.Naked_immediate, Unsigned, Yielding_bool Lt),
      untag_int index,
      actual_max_length ~size_int ~access_size
        (Prim (Unary (String_length kind, str))) )

let string_or_bytes_ref ~size_int kind arg1 arg2 dbg : H.expr_primitive =
  Checked
    { primitive =
        tag_int (Binary (String_or_bigstring_load (kind, Eight), arg1, arg2));
      validity_conditions =
        [ string_or_bytes_access_validity_condition ~size_int arg1 String Eight
            arg2 ];
      failure = Index_out_of_bounds;
      dbg
    }

let bigstring_access_validity_condition ~size_int bstr access_size index :
    H.expr_primitive =
  Binary
    ( Int_comp (I.Naked_immediate, Unsigned, Yielding_bool Lt),
      untag_int index,
      actual_max_length ~size_int ~access_size
        (Prim (Unary (Bigarray_length { dimension = 1 }, bstr))) )

(* CR mshinwell: Same problems as previous function *)
let bigstring_ref ~size_int access_size arg1 arg2 dbg : H.expr_primitive =
  let wrap =
    match (access_size : Flambda_primitive.string_accessor_width) with
    | Eight | Sixteen -> tag_int
    | Thirty_two -> box_bint Pint32
    | Sixty_four -> box_bint Pint64
  in
  Checked
    { primitive =
        wrap
          (Binary (String_or_bigstring_load (Bigstring, access_size), arg1, arg2));
      validity_conditions =
        [bigstring_access_validity_condition ~size_int arg1 access_size arg2];
      failure = Index_out_of_bounds;
      dbg
    }

let bigarray_box_raw_value_read kind =
  let error what =
    Misc.fatal_errorf "Don't know how to unbox %s to store it in a bigarray"
      what
  in
  match P.element_kind_of_bigarray_kind kind with
  | Value -> Fun.id
  | Naked_number k ->
    let bi = K.Boxable_number.of_naked_number_kind k in
    fun arg -> H.Unary (Box_number bi, Prim arg)
  | Fabricated -> error "a fabricated expression"
  | Rec_info -> error "recursion info"

let bigarray_unbox_value_to_store kind =
  let error what =
    Misc.fatal_errorf "Don't know how to unbox %s to store it in a bigarray"
      what
  in
  match P.element_kind_of_bigarray_kind kind with
  | Value -> Fun.id
  | Naked_number k ->
    let bi = K.Boxable_number.of_naked_number_kind k in
    fun arg -> H.Prim (Unary (Unbox_number bi, arg))
  | Fabricated -> error "a fabricated expression"
  | Rec_info -> error "recursion info"

let bigarray_dim_bound b dimension =
  H.Prim (Unary (Bigarray_length { dimension }, b))

let bigarray_check_bound idx bound =
  H.Binary (Int_comp (I.Naked_immediate, Unsigned, Yielding_bool Lt), idx, bound)

(* CR Gbury: this function in effect duplicates the bigarray_length access: one
   is done in the validity check, and one in the final offset computation,
   whereas cmmgen let-binds this access. It might matter for the performance,
   although the processor cache might make it not matter at all. *)
let bigarray_indexing layout b args =
  let num_dim = List.length args in
  let rec aux dim delta_dim = function
    | [] -> assert false
    | [idx] ->
      let bound = bigarray_dim_bound b dim in
      let idxn = untag_int idx in
      let check = bigarray_check_bound idxn bound in
      [check], idx
    | idx :: r ->
      let checks, rem = aux (dim + delta_dim) delta_dim r in
      let bound = bigarray_dim_bound b dim in
      let idxn = untag_int idx in
      let check = bigarray_check_bound idxn bound in
      (* CR gbury: because we tag bound, and the tagged multiplication untags
         it, we might be left with a needless zero-extend here. *)
      let tmp =
        H.Prim
          (Binary
             ( Int_arith (I.Tagged_immediate, Mul),
               rem,
               Prim (Unary (Box_number Untagged_immediate, bound)) ))
      in
      let offset =
        H.Prim (Binary (Int_arith (I.Tagged_immediate, Add), tmp, idx))
      in
      check :: checks, offset
  in
  match (layout : P.bigarray_layout) with
  | C -> aux num_dim (-1) (List.rev args)
  | Fortran ->
    aux 1 1
      (List.map
         (fun idx ->
           H.Prim
             (Binary
                ( Int_arith (I.Tagged_immediate, Sub),
                  idx,
                  H.Simple (Simple.const_int Targetint_31_63.Imm.one) )))
         args)

let bigarray_ref ~dbg ~unsafe kind layout b indexes =
  let num_dim = List.length indexes in
  let checks, offset = bigarray_indexing layout b indexes in
  let primitive = H.Binary (Bigarray_load (num_dim, kind, layout), b, offset) in
  if unsafe
  then primitive
  else
    H.Checked
      { validity_conditions = checks;
        failure = Index_out_of_bounds;
        primitive;
        dbg
      }

let bigarray_set ~dbg ~unsafe kind layout b indexes value =
  let num_dim = List.length indexes in
  let checks, offset = bigarray_indexing layout b indexes in
  let primitive =
    H.Ternary (Bigarray_set (num_dim, kind, layout), b, offset, value)
  in
  if unsafe
  then primitive
  else
    H.Checked
      { validity_conditions = checks;
        failure = Index_out_of_bounds;
        primitive;
        dbg
      }

let convert_lprim ~big_endian (prim : L.primitive) (args : Simple.t list)
    (dbg : Debuginfo.t) : H.expr_primitive =
  let args = List.map (fun arg : H.simple_or_prim -> Simple arg) args in
  let size_int =
    assert (Targetint.size mod 8 = 0);
    Targetint.size / 8
  in
  match prim, args with
  | Pmakeblock (tag, mutability, shape), _ ->
    let tag = Tag.Scannable.create_exn tag in
    let shape = C.convert_block_shape shape ~num_fields:(List.length args) in
    let mutability = C.convert_mutable_flag mutability in
    Variadic (Make_block (Values (tag, shape), mutability), args)
  | Pmakefloatblock mutability, _ ->
    let mutability = C.convert_mutable_flag mutability in
    Variadic (Make_block (Naked_floats, mutability), List.map unbox_float args)
  | Pmakearray (array_kind, mutability), _ -> (
    let array_kind = C.convert_array_kind array_kind in
    let mutability = C.convert_mutable_flag mutability in
    match array_kind with
    | Array_kind array_kind ->
      let args =
        match array_kind with
        | Immediates | Values -> args
        | Naked_floats -> List.map unbox_float args
      in
      Variadic (Make_array (array_kind, mutability), args)
    | Float_array_opt_dynamic -> (
      (* If this is an empty array we can just give it array kind [Values].
         (Even empty flat float arrays have tag zero.) *)
      match args with
      | [] -> Variadic (Make_array (Values, Immutable), [])
      | elt :: _ ->
        (* Test the first element to see if it's a boxed float: if it is, this
           array must be created as a flat float array. *)
        If_then_else
          ( Unary (Is_boxed_float, elt),
            Variadic
              (Make_array (Naked_floats, Immutable), List.map unbox_float args),
            Variadic (Make_array (Values, Immutable), args) )))
  | Popaque, [arg] -> Unary (Opaque_identity, arg)
  | Pduprecord (repr, num_fields), [arg] ->
    let kind : P.Duplicate_block_kind.t =
      match repr with
      | Record_regular ->
        Values
          { tag = Tag.Scannable.zero;
            length = Targetint_31_63.Imm.of_int num_fields
          }
      | Record_float ->
        Naked_floats { length = Targetint_31_63.Imm.of_int num_fields }
      | Record_unboxed _ -> Misc.fatal_error "Pduprecord of unboxed record"
      | Record_inlined tag ->
        Values
          { tag = Tag.Scannable.create_exn tag;
            length = Targetint_31_63.Imm.of_int num_fields
          }
      | Record_extension _ ->
        Values
          { tag = Tag.Scannable.zero;
            (* The "+1" is because there is an extra field containing the hashed
               constructor. *)
            length = Targetint_31_63.Imm.of_int (num_fields + 1)
          }
    in
    Unary
      ( Duplicate_block
          { kind;
            source_mutability = Mutable;
            destination_mutability = Mutable
          },
        arg )
  | Pnegint, [arg] -> Unary (Int_arith (I.Tagged_immediate, Neg), arg)
  | Paddint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Add), arg1, arg2)
  | Psubint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Sub), arg1, arg2)
  | Pmulint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Mul), arg1, arg2)
  | Pandint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, And), arg1, arg2)
  | Porint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Or), arg1, arg2)
  | Pxorint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Xor), arg1, arg2)
  | Plslint, [arg1; arg2] ->
    Binary (Int_shift (I.Tagged_immediate, Lsl), arg1, untag_int arg2)
  | Plsrint, [arg1; arg2] ->
    Binary (Int_shift (I.Tagged_immediate, Lsr), arg1, untag_int arg2)
  | Pasrint, [arg1; arg2] ->
    Binary (Int_shift (I.Tagged_immediate, Asr), arg1, untag_int arg2)
  | Pnot, [arg] -> Unary (Boolean_not, arg)
  | Pintcomp comp, [arg1; arg2] ->
    tag_int (Binary (C.convert_integer_comparison_prim comp, arg1, arg2))
  | Pbintcomp (kind, comp), [arg1; arg2] ->
    let arg1 = unbox_bint kind arg1 in
    let arg2 = unbox_bint kind arg2 in
    tag_int
      (Binary (C.convert_boxed_integer_comparison_prim kind comp, arg1, arg2))
  | Pintoffloat, [arg] ->
    let src = K.Standard_int_or_float.Naked_float in
    let dst = K.Standard_int_or_float.Tagged_immediate in
    Unary (Num_conv { src; dst }, unbox_float arg)
  | Pfloatofint, [arg] ->
    let src = K.Standard_int_or_float.Tagged_immediate in
    let dst = K.Standard_int_or_float.Naked_float in
    box_float (Unary (Num_conv { src; dst }, arg))
  | Pnegfloat, [arg] -> box_float (Unary (Float_arith Neg, unbox_float arg))
  | Pabsfloat, [arg] -> box_float (Unary (Float_arith Abs, unbox_float arg))
  | Paddfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Add, unbox_float arg1, unbox_float arg2))
  | Psubfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Sub, unbox_float arg1, unbox_float arg2))
  | Pmulfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Mul, unbox_float arg1, unbox_float arg2))
  | Pdivfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Div, unbox_float arg1, unbox_float arg2))
  | Pfloatcomp comp, [arg1; arg2] ->
    tag_int
      (Binary
         ( Float_comp (Yielding_bool (C.convert_float_comparison comp)),
           unbox_float arg1,
           unbox_float arg2 ))
  | Pfield_computed sem, [obj; field] ->
    let block_access : P.Block_access_kind.t =
      (* Pfield_computed is only used for class access, on blocks of tag
         [Object_tag], created in [CamlinternalOO]. *)
      Values
        { tag = Known Tag.Scannable.object_tag;
          size = Unknown;
          field_kind = Any_value
        }
    in
    Binary
      (Block_load (block_access, C.convert_field_read_semantics sem), obj, field)
  | Psetfield_computed (imm_or_pointer, init_or_assign), [obj; field; value] ->
    (* Same note as for [Pfield_computed] above. *)
    let field_kind = C.convert_block_access_field_kind imm_or_pointer in
    let block_access : P.Block_access_kind.t =
      Values
        { tag = Known Tag.Scannable.object_tag; size = Unknown; field_kind }
    in
    Ternary
      ( Block_set (block_access, C.convert_init_or_assign init_or_assign),
        obj,
        field,
        value )
  | Parraylength _kind, [arg] ->
    (* See check in flambda2.ml that ensures we don't need to propagate the
       array kind. *)
    Unary (Array_length, arg)
  | Pduparray (kind, mutability), [arg] -> (
    let duplicate_array_kind =
      C.convert_array_kind_to_duplicate_array_kind kind
    in
    (* CR mshinwell: Check that [Pduparray] is only applied to immutable
       arrays *)
    (* CR mshinwell: Check that [mutability] is the destination mutability *)
    let source_mutability = Mutability.Immutable in
    let destination_mutability = C.convert_mutable_flag mutability in
    match duplicate_array_kind with
    | Duplicate_array_kind duplicate_array_kind ->
      Unary
        ( Duplicate_array
            { kind = duplicate_array_kind;
              source_mutability;
              destination_mutability
            },
          arg )
    | Float_array_opt_dynamic ->
      If_then_else
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
              arg ) ))
  | Pstringlength, [arg] ->
    (* CR mshinwell: Decide whether things such as String_length should return
       tagged or untagged integers. Probably easiest to match Cmm_helpers for
       now and change individual cases later for better codegen if required. *)
    tag_int (Unary (String_length String, arg))
  | Pbyteslength, [arg] -> tag_int (Unary (String_length Bytes, arg))
  | Pstringrefu, [arg1; arg2] ->
    tag_int (Binary (String_or_bigstring_load (String, Eight), arg1, arg2))
  | Pbytesrefu, [arg1; arg2] ->
    tag_int (Binary (String_or_bigstring_load (Bytes, Eight), arg1, arg2))
  | Pbytesrefs, [arg1; arg2] ->
    string_or_bytes_ref ~size_int Bytes arg1 arg2 dbg
  | Pstringrefs, [arg1; arg2] ->
    string_or_bytes_ref ~size_int String arg1 arg2 dbg
  | Pstring_load_16 true (* unsafe *), [arg1; arg2]
  | Pbytes_load_16 true (* unsafe *), [arg1; arg2] ->
    tag_int (Binary (String_or_bigstring_load (String, Sixteen), arg1, arg2))
  | Pstring_load_32 true (* unsafe *), [arg1; arg2]
  | Pbytes_load_32 true (* unsafe *), [arg1; arg2] ->
    Unary
      ( Box_number Naked_int32,
        Prim
          (Binary (String_or_bigstring_load (String, Thirty_two), arg1, arg2))
      )
  | Pstring_load_64 true (* unsafe *), [arg1; arg2]
  | Pbytes_load_64 true (* unsafe *), [arg1; arg2] ->
    Unary
      ( Box_number Naked_int64,
        Prim
          (Binary (String_or_bigstring_load (String, Sixty_four), arg1, arg2))
      )
  | Pstring_load_16 false, [str; index] ->
    Checked
      { primitive =
          tag_int
            (Binary (String_or_bigstring_load (String, Sixteen), str, index));
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int str String
              Sixteen index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pstring_load_32 false, [str; index] ->
    Checked
      { primitive =
          Unary
            ( Box_number Naked_int32,
              Prim
                (Binary
                   (String_or_bigstring_load (String, Thirty_two), str, index))
            );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int str String
              Thirty_two index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pstring_load_64 false, [str; index] ->
    Checked
      { primitive =
          Unary
            ( Box_number Naked_int64,
              Prim
                (Binary
                   (String_or_bigstring_load (String, Sixty_four), str, index))
            );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int str String
              Sixty_four index ];
        failure = Index_out_of_bounds;
        dbg
      }
  (* CR mshinwell: factor out *)
  | Pbytes_load_16 false, [bytes; index] ->
    Checked
      { primitive =
          tag_int
            (Binary (String_or_bigstring_load (Bytes, Sixteen), bytes, index));
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int bytes Bytes
              Sixteen index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pbytes_load_32 false, [bytes; index] ->
    Checked
      { primitive =
          Unary
            ( Box_number Naked_int32,
              Prim
                (Binary
                   (String_or_bigstring_load (Bytes, Thirty_two), bytes, index))
            );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int bytes Bytes
              Thirty_two index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pbytes_load_64 false, [bytes; index] ->
    Checked
      { primitive =
          Unary
            ( Box_number Naked_int64,
              Prim
                (Binary
                   (String_or_bigstring_load (Bytes, Sixty_four), bytes, index))
            );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int bytes Bytes
              Sixty_four index ];
        failure = Index_out_of_bounds;
        dbg
      }
  (* CR mshinwell: Change [Lambda] to have a [Safe] / [Unsafe] variant *)
  | Pbytes_set_16 true, [bytes; index; new_value] ->
    Ternary
      ( Bytes_or_bigstring_set (Bytes, Sixteen),
        bytes,
        index,
        untag_int new_value )
  | Pbytes_set_32 true, [bytes; index; new_value] ->
    Ternary
      ( Bytes_or_bigstring_set (Bytes, Thirty_two),
        bytes,
        index,
        Prim (Unary (Unbox_number Naked_int32, new_value)) )
  | Pbytes_set_64 true, [bytes; index; new_value] ->
    Ternary
      ( Bytes_or_bigstring_set (Bytes, Sixty_four),
        bytes,
        index,
        Prim (Unary (Unbox_number Naked_int64, new_value)) )
  | Pbytes_set_16 false, [bytes; index; new_value] ->
    Checked
      { primitive =
          Ternary
            ( Bytes_or_bigstring_set (Bytes, Sixteen),
              bytes,
              index,
              untag_int new_value );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int bytes Bytes
              Sixteen index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pbytes_set_32 false, [bytes; index; new_value] ->
    Checked
      { primitive =
          Ternary
            ( Bytes_or_bigstring_set (Bytes, Thirty_two),
              bytes,
              index,
              Prim (Unary (Unbox_number Naked_int32, new_value)) );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int bytes Bytes
              Thirty_two index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pbytes_set_64 false, [bytes; index; new_value] ->
    Checked
      { primitive =
          Ternary
            ( Bytes_or_bigstring_set (Bytes, Sixty_four),
              bytes,
              index,
              Prim (Unary (Unbox_number Naked_int64, new_value)) );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int bytes Bytes
              Sixty_four index ];
        failure = Index_out_of_bounds;
        dbg
      }
  (* CR mshinwell: To do: | Pbittest, [arg1; arg2] -> *)
  (*   Binary (Bit_test, arg1, arg2) *)
  | Pisint, [arg] -> tag_int (Unary (Is_int, arg))
  | Pisout, [arg1; arg2] ->
    tag_int
      (Binary
         ( Int_comp (I.Tagged_immediate, Unsigned, Yielding_bool Lt),
           tagged_immediate_as_naked_nativeint arg1,
           tagged_immediate_as_naked_nativeint arg2 ))
  | Pbintofint bi, [arg] ->
    let dst = C.standard_int_or_float_of_boxed_integer bi in
    Unary
      ( Box_number (C.boxable_number_of_boxed_integer bi),
        Prim (Unary (Num_conv { src = I_or_f.Tagged_immediate; dst }, arg)) )
  | Pintofbint bi, [arg] ->
    let src = C.standard_int_or_float_of_boxed_integer bi in
    Unary
      ( Num_conv { src; dst = I_or_f.Tagged_immediate },
        Prim (Unary (Unbox_number (C.boxable_number_of_boxed_integer bi), arg))
      )
  | Pcvtbint (source, destination), [arg] ->
    box_bint destination
      (Unary
         ( Num_conv
             { src = C.standard_int_or_float_of_boxed_integer source;
               dst = C.standard_int_or_float_of_boxed_integer destination
             },
           unbox_bint source arg ))
  | Pnegbint bi, [arg] ->
    box_bint bi
      (Unary
         (Int_arith (C.standard_int_of_boxed_integer bi, Neg), unbox_bint bi arg))
  | Paddbint bi, [arg1; arg2] -> bint_binary_prim bi Add arg1 arg2
  | Psubbint bi, [arg1; arg2] -> bint_binary_prim bi Sub arg1 arg2
  | Pmulbint bi, [arg1; arg2] -> bint_binary_prim bi Mul arg1 arg2
  | Pandbint bi, [arg1; arg2] -> bint_binary_prim bi And arg1 arg2
  | Porbint bi, [arg1; arg2] -> bint_binary_prim bi Or arg1 arg2
  | Pxorbint bi, [arg1; arg2] -> bint_binary_prim bi Xor arg1 arg2
  | Plslbint bi, [arg1; arg2] -> bint_shift bi Lsl arg1 arg2
  | Plsrbint bi, [arg1; arg2] -> bint_shift bi Lsr arg1 arg2
  | Pasrbint bi, [arg1; arg2] -> bint_shift bi Asr arg1 arg2
  | Poffsetint n, [arg] ->
    let const =
      Simple.const
        (Reg_width_const.tagged_immediate
           (Targetint_31_63.int (Targetint_31_63.Imm.of_int n)))
    in
    Binary (Int_arith (I.Tagged_immediate, Add), arg, Simple const)
  | Pfield (index, sem), [arg] ->
    let imm = Targetint_31_63.int (Targetint_31_63.Imm.of_int index) in
    if not (Targetint_31_63.is_non_negative imm)
    then
      Misc.fatal_errorf "Pfield with negative index %a" Targetint_31_63.print
        imm;
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let mutability = C.convert_field_read_semantics sem in
    let block_access : P.Block_access_kind.t =
      Values { tag = Unknown; size = Unknown; field_kind = Any_value }
    in
    Binary (Block_load (block_access, mutability), arg, Simple field)
  | Pfloatfield (field, sem), [arg] ->
    let imm = Targetint_31_63.int (Targetint_31_63.Imm.of_int field) in
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let mutability = C.convert_field_read_semantics sem in
    let block_access : P.Block_access_kind.t =
      Naked_floats { size = Unknown }
    in
    box_float
      (Binary (Block_load (block_access, mutability), arg, Simple field))
  | ( Psetfield (index, immediate_or_pointer, initialization_or_assignment),
      [block; value] ) ->
    let field_kind = C.convert_block_access_field_kind immediate_or_pointer in
    let imm = Targetint_31_63.int (Targetint_31_63.Imm.of_int index) in
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let block_access : P.Block_access_kind.t =
      Values { tag = Unknown; size = Unknown; field_kind }
    in
    Ternary
      ( Block_set
          (block_access, C.convert_init_or_assign initialization_or_assignment),
        block,
        Simple field,
        value )
  | Psetfloatfield (field, init_or_assign), [block; value] ->
    let imm = Targetint_31_63.int (Targetint_31_63.Imm.of_int field) in
    let field = Simple.const (Reg_width_const.tagged_immediate imm) in
    let block_access : P.Block_access_kind.t =
      Naked_floats { size = Unknown }
    in
    Ternary
      ( Block_set (block_access, C.convert_init_or_assign init_or_assign),
        block,
        Simple field,
        unbox_float value )
  | Pdivint Unsafe, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Div), arg1, arg2)
  | Pdivint Safe, [arg1; arg2] ->
    Checked
      { primitive = Binary (Int_arith (I.Tagged_immediate, Div), arg1, arg2);
        validity_conditions =
          [ Binary
              ( Phys_equal (K.value, Neq),
                arg2,
                Simple
                  (Simple.const
                     (Reg_width_const.tagged_immediate
                        (Targetint_31_63.int Targetint_31_63.Imm.zero))) ) ];
        failure = Division_by_zero;
        dbg
      }
  | Pmodint Safe, [arg1; arg2] ->
    Checked
      { primitive = Binary (Int_arith (I.Tagged_immediate, Mod), arg1, arg2);
        validity_conditions =
          [ Binary
              ( Phys_equal (K.value, Neq),
                arg2,
                Simple
                  (Simple.const
                     (Reg_width_const.tagged_immediate
                        (Targetint_31_63.int Targetint_31_63.Imm.zero))) ) ];
        failure = Division_by_zero;
        dbg
      }
  | Pdivbint { size = Pint32; is_safe = Safe }, [arg1; arg2] ->
    (* The duplicate unboxing generated in the Pdivbint/Pmodbint cases will be
       removed by the simplifier. *)
    (* CR mshinwell: Factor these cases out *)
    Checked
      { primitive =
          box_bint Pint32
            (Binary
               ( Int_arith (I.Naked_int32, Div),
                 unbox_bint Pint32 arg1,
                 unbox_bint Pint32 arg2 ));
        validity_conditions =
          [ Binary
              ( Phys_equal (K.naked_int32, Neq),
                unbox_bint Pint32 arg2,
                Simple (Simple.const (Reg_width_const.naked_int32 0l)) ) ];
        failure = Division_by_zero;
        dbg
      }
  | Pmodbint { size = Pint32; is_safe = Safe }, [arg1; arg2] ->
    Checked
      { primitive =
          box_bint Pint32
            (Binary
               ( Int_arith (I.Naked_int32, Mod),
                 unbox_bint Pint32 arg1,
                 unbox_bint Pint32 arg2 ));
        validity_conditions =
          [ Binary
              ( Phys_equal (K.naked_int32, Neq),
                unbox_bint Pint32 arg2,
                Simple (Simple.const (Reg_width_const.naked_int32 0l)) ) ];
        failure = Division_by_zero;
        dbg
      }
  | Pdivbint { size = Pint64; is_safe = Safe }, [arg1; arg2] ->
    Checked
      { primitive =
          box_bint Pint64
            (Binary
               ( Int_arith (I.Naked_int64, Div),
                 unbox_bint Pint64 arg1,
                 unbox_bint Pint64 arg2 ));
        validity_conditions =
          [ Binary
              ( Phys_equal (K.naked_int64, Neq),
                unbox_bint Pint64 arg2,
                Simple (Simple.const (Reg_width_const.naked_int64 0L)) ) ];
        failure = Division_by_zero;
        dbg
      }
  | Pmodbint { size = Pint64; is_safe = Safe }, [arg1; arg2] ->
    Checked
      { primitive =
          box_bint Pint64
            (Binary
               ( Int_arith (I.Naked_int64, Mod),
                 unbox_bint Pint64 arg1,
                 unbox_bint Pint64 arg2 ));
        validity_conditions =
          [ Binary
              ( Phys_equal (K.naked_int64, Neq),
                unbox_bint Pint64 arg2,
                Simple (Simple.const (Reg_width_const.naked_int64 0L)) ) ];
        failure = Division_by_zero;
        dbg
      }
  | Pdivbint { size = Pnativeint; is_safe = Safe }, [arg1; arg2] ->
    Checked
      { primitive =
          box_bint Pnativeint
            (Binary
               ( Int_arith (I.Naked_nativeint, Div),
                 unbox_bint Pnativeint arg1,
                 unbox_bint Pnativeint arg2 ));
        validity_conditions =
          [ Binary
              ( Phys_equal (K.naked_nativeint, Neq),
                unbox_bint Pnativeint arg2,
                Simple
                  (Simple.const
                     (Reg_width_const.naked_nativeint Targetint_32_64.zero)) )
          ];
        failure = Division_by_zero;
        dbg
      }
  | Pmodbint { size = Pnativeint; is_safe = Safe }, [arg1; arg2] ->
    Checked
      { primitive =
          box_bint Pnativeint
            (Binary
               ( Int_arith (I.Naked_nativeint, Mod),
                 unbox_bint Pnativeint arg1,
                 unbox_bint Pnativeint arg2 ));
        validity_conditions =
          [ Binary
              ( Phys_equal (K.naked_nativeint, Neq),
                unbox_bint Pnativeint arg2,
                Simple
                  (Simple.const
                     (Reg_width_const.naked_nativeint Targetint_32_64.zero)) )
          ];
        failure = Division_by_zero;
        dbg
      }
  | Parrayrefu array_kind, [array; index] -> (
    match C.convert_array_kind array_kind with
    | Array_kind ((Immediates | Values) as array_kind) ->
      Binary (Array_load (array_kind, Mutable), array, index)
    | Array_kind Naked_floats ->
      box_float (Binary (Array_load (Naked_floats, Mutable), array, index))
    | Float_array_opt_dynamic ->
      If_then_else
        ( Unary (Is_flat_float_array, array),
          box_float (Binary (Array_load (Naked_floats, Mutable), array, index)),
          Binary (Array_load (Values, Mutable), array, index) ))
  | Parrayrefs array_kind, [array; index] -> (
    let[@inline always] build_term (array_kind : P.Array_kind.t) :
        H.expr_primitive =
      let primitive : H.expr_primitive =
        Binary (Array_load (array_kind, Mutable), array, index)
      in
      let primitive =
        match array_kind with
        | Immediates | Values -> primitive
        | Naked_floats -> box_float primitive
      in
      Checked
        { primitive;
          validity_conditions = array_access_validity_condition array index;
          failure = Index_out_of_bounds;
          dbg
        }
    in
    match C.convert_array_kind array_kind with
    | Array_kind array_kind -> build_term array_kind
    | Float_array_opt_dynamic ->
      If_then_else
        ( Unary (Is_flat_float_array, array),
          build_term Naked_floats,
          build_term Values ))
  | Parraysetu array_kind, [array; index; new_value] -> (
    match C.convert_array_kind array_kind with
    | Array_kind ((Immediates | Values) as array_kind) ->
      Ternary (Array_set (array_kind, Assignment), array, index, new_value)
    | Array_kind Naked_floats ->
      Ternary
        ( Array_set (Naked_floats, Assignment),
          array,
          index,
          unbox_float new_value )
    | Float_array_opt_dynamic ->
      If_then_else
        ( Unary (Is_flat_float_array, array),
          Ternary
            ( Array_set (Naked_floats, Assignment),
              array,
              index,
              unbox_float new_value ),
          Ternary (Array_set (Values, Assignment), array, index, new_value) ))
  | Parraysets array_kind, [array; index; new_value] -> (
    (* For these cases (and the [Parrayrefs] ones above) we will end up relying
       on the backend to CSE the two accesses to the array's header word in the
       [Pgenarray] case. *)
    let[@inline always] build_term (array_kind : P.Array_kind.t) :
        H.expr_primitive =
      let primitive : H.expr_primitive =
        Ternary
          ( Array_set (array_kind, Assignment),
            array,
            index,
            match array_kind with
            | Immediates | Values -> new_value
            | Naked_floats -> unbox_float new_value )
      in
      Checked
        { primitive;
          validity_conditions = array_access_validity_condition array index;
          failure = Index_out_of_bounds;
          dbg
        }
    in
    match C.convert_array_kind array_kind with
    | Array_kind array_kind -> build_term array_kind
    | Float_array_opt_dynamic ->
      If_then_else
        ( Unary (Is_flat_float_array, array),
          build_term Naked_floats,
          build_term Values ))
  | Pbytessetu, [bytes; index; new_value] ->
    Ternary
      (Bytes_or_bigstring_set (Bytes, Eight), bytes, index, untag_int new_value)
  | Pbytessets, [bytes; index; new_value] ->
    Checked
      { primitive =
          Ternary
            ( Bytes_or_bigstring_set (Bytes, Eight),
              bytes,
              index,
              untag_int new_value );
        validity_conditions =
          [ string_or_bytes_access_validity_condition ~size_int bytes Bytes
              Eight index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Poffsetref n, [block] ->
    let block_access : P.Block_access_kind.t =
      Values
        { tag = Known Tag.Scannable.zero;
          size = Known Targetint_31_63.Imm.one;
          field_kind = Immediate
        }
    in
    Ternary
      ( Block_set (block_access, Assignment),
        block,
        Simple Simple.const_zero,
        Prim
          (Binary
             ( Int_arith (Tagged_immediate, Add),
               Simple (Simple.const_int (Targetint_31_63.Imm.of_int n)),
               Prim
                 (Binary
                    ( Block_load (block_access, Mutable),
                      block,
                      Simple Simple.const_zero )) )) )
  | Pctconst const, _ -> begin
    (* CR mshinwell: This doesn't seem to be zero-arity like it should be *)
    (* CR pchambart: It's not obvious when this one should be converted.
       mshinwell: Have put an implementation here for now. *)
    match const with
    | Big_endian -> Simple (Simple.const_bool big_endian)
    | Word_size ->
      Simple (Simple.const_int (Targetint_31_63.Imm.of_int (8 * size_int)))
    | Int_size ->
      Simple
        (Simple.const_int (Targetint_31_63.Imm.of_int ((8 * size_int) - 1)))
    | Max_wosize ->
      (* CR mshinwell: This depends on the number of bits in the header reserved
         for profinfo, no? If this computation is wrong then the one in
         [Closure] (and maybe Flambda 1) is wrong. *)
      Simple
        (Simple.const_int
           (Targetint_31_63.Imm.of_int ((1 lsl ((8 * size_int) - 10)) - 1)))
    | Ostype_unix -> Simple (Simple.const_bool (Sys.os_type = "Unix"))
    | Ostype_win32 -> Simple (Simple.const_bool (Sys.os_type = "Win32"))
    | Ostype_cygwin -> Simple (Simple.const_bool (Sys.os_type = "Cygwin"))
    | Backend_type ->
      Simple Simple.const_zero (* constructor 0 is the same as Native here *)
  end
  | Pbswap16, [arg] ->
    tag_int
      (Unary (Int_arith (Naked_immediate, Swap_byte_endianness), untag_int arg))
  | Pbbswap Pint32, [arg] ->
    Unary
      ( Box_number Naked_int32,
        Prim
          (Unary
             ( Int_arith (Naked_int32, Swap_byte_endianness),
               Prim (Unary (Unbox_number Naked_int32, arg)) )) )
  | Pbbswap Pint64, [arg] ->
    Unary
      ( Box_number Naked_int64,
        Prim
          (Unary
             ( Int_arith (Naked_int64, Swap_byte_endianness),
               Prim (Unary (Unbox_number Naked_int64, arg)) )) )
  | Pbbswap Pnativeint, [arg] ->
    Unary
      ( Box_number Naked_nativeint,
        Prim
          (Unary
             ( Int_arith (Naked_nativeint, Swap_byte_endianness),
               Prim (Unary (Unbox_number Naked_nativeint, arg)) )) )
  | Pint_as_pointer, [arg] -> Unary (Int_as_pointer, arg)
  | Pbigarrayref (unsafe, num_dimensions, kind, layout), args -> begin
    match C.convert_bigarray_kind kind, C.convert_bigarray_layout layout with
    | Some kind, Some layout ->
      let b, indexes =
        match args with
        | b :: indexes ->
          if List.compare_length_with indexes num_dimensions <> 0
          then Misc.fatal_errorf "Bad index arity for Pbigarrayref";
          b, indexes
        | [] -> Misc.fatal_errorf "Pbigarrayref is missing its arguments"
      in
      let box = bigarray_box_raw_value_read kind in
      box (bigarray_ref ~dbg ~unsafe kind layout b indexes)
    | None, _ ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown kind should have been removed by Lambda_to_flambda."
    | _, None ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown layout should have been removed by Lambda_to_flambda."
  end
  | Pbigarrayset (unsafe, num_dimensions, kind, layout), args -> begin
    match C.convert_bigarray_kind kind, C.convert_bigarray_layout layout with
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
      let unbox = bigarray_unbox_value_to_store kind in
      bigarray_set ~dbg ~unsafe kind layout b indexes (unbox value)
    | None, _ ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown kind should have been removed by Lambda_to_flambda."
    | _, None ->
      Misc.fatal_errorf
        "Lambda_to_flambda_primitives.convert_lprim: Pbigarrayref primitives \
         with an unknown layout should have been removed by Lambda_to_flambda."
  end
  | Pbigarraydim dimension, [arg] ->
    tag_int (Unary (Bigarray_length { dimension }, arg))
  | Pbigstring_load_16 true, [arg1; arg2] ->
    tag_int (Binary (String_or_bigstring_load (Bigstring, Sixteen), arg1, arg2))
  | Pbigstring_load_16 false, [arg1; arg2] ->
    bigstring_ref ~size_int Sixteen arg1 arg2 dbg
  | Pbigstring_load_32 true, [arg1; arg2] ->
    box_bint Pint32
      (Binary (String_or_bigstring_load (Bigstring, Thirty_two), arg1, arg2))
  | Pbigstring_load_32 false, [arg1; arg2] ->
    bigstring_ref ~size_int Thirty_two arg1 arg2 dbg
  | Pbigstring_load_64 true, [arg1; arg2] ->
    box_bint Pint64
      (Binary (String_or_bigstring_load (Bigstring, Sixty_four), arg1, arg2))
  | Pbigstring_load_64 false, [arg1; arg2] ->
    bigstring_ref ~size_int Sixty_four arg1 arg2 dbg
  | Pbigstring_set_16 true, [bigstring; index; new_value] ->
    Ternary
      ( Bytes_or_bigstring_set (Bigstring, Sixteen),
        bigstring,
        index,
        untag_int new_value )
  | Pbigstring_set_32 true, [bigstring; index; new_value] ->
    Ternary
      ( Bytes_or_bigstring_set (Bigstring, Thirty_two),
        bigstring,
        index,
        Prim (Unary (Unbox_number Naked_int32, new_value)) )
  | Pbigstring_set_64 true, [bigstring; index; new_value] ->
    Ternary
      ( Bytes_or_bigstring_set (Bigstring, Sixty_four),
        bigstring,
        index,
        Prim (Unary (Unbox_number Naked_int64, new_value)) )
  | Pbigstring_set_16 false, [bigstring; index; new_value] ->
    Checked
      { primitive =
          Ternary
            ( Bytes_or_bigstring_set (Bigstring, Sixteen),
              bigstring,
              index,
              untag_int new_value );
        validity_conditions =
          [bigstring_access_validity_condition ~size_int bigstring Sixteen index];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pbigstring_set_32 false, [bigstring; index; new_value] ->
    Checked
      { primitive =
          Ternary
            ( Bytes_or_bigstring_set (Bigstring, Thirty_two),
              bigstring,
              index,
              Prim (Unary (Unbox_number Naked_int32, new_value)) );
        validity_conditions =
          [ bigstring_access_validity_condition ~size_int bigstring Thirty_two
              index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pbigstring_set_64 false, [bigstring; index; new_value] ->
    Checked
      { primitive =
          Ternary
            ( Bytes_or_bigstring_set (Bigstring, Sixty_four),
              bigstring,
              index,
              Prim (Unary (Unbox_number Naked_int64, new_value)) );
        validity_conditions =
          [ bigstring_access_validity_condition ~size_int bigstring Sixty_four
              index ];
        failure = Index_out_of_bounds;
        dbg
      }
  | Pcompare_ints, [i1; i2] ->
    tag_int
      (Binary
         ( Int_comp
             (Tagged_immediate, Signed, Yielding_int_like_compare_functions),
           i1,
           i2 ))
  | Pcompare_floats, [f1; f2] ->
    tag_int
      (Binary
         ( Float_comp Yielding_int_like_compare_functions,
           Prim (Unary (Unbox_number Naked_float, f1)),
           Prim (Unary (Unbox_number Naked_float, f2)) ))
  | Pcompare_bints int_kind, [i1; i2] ->
    let unboxing_kind = C.boxable_number_of_boxed_integer int_kind in
    tag_int
      (Binary
         ( Int_comp
             ( C.standard_int_of_boxed_integer int_kind,
               Signed,
               Yielding_int_like_compare_functions ),
           Prim (Unary (Unbox_number unboxing_kind, i1)),
           Prim (Unary (Unbox_number unboxing_kind, i2)) ))
  | Pprobe_is_enabled { name }, [] ->
    tag_int (Nullary (Probe_is_enabled { name }))
  | ( ( Pmodint Unsafe
      | Pdivbint { is_safe = Unsafe; size = _ }
      | Pmodbint { is_safe = Unsafe; size = _ }
      | Psetglobal _ | Praise _ | Pccall _ ),
      _ ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Primitive %a (%a) shouldn't be \
       here, either a bug in [Closure_conversion] or the wrong number of \
       arguments"
      Printlambda.primitive prim H.print_list_of_simple_or_prim args
  | Pprobe_is_enabled _, _ :: _ ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for nullary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_simple_or_prim args
  | ( ( Pfield _ | Pnegint | Pnot | Poffsetint _ | Pintoffloat | Pfloatofint
      | Pnegfloat | Pabsfloat | Pstringlength | Pbyteslength | Pbintofint _
      | Pintofbint _ | Pnegbint _ | Popaque | Pduprecord _ | Parraylength _
      | Pduparray _ | Pfloatfield _ | Pcvtbint _ | Poffsetref _ | Pbswap16
      | Pbbswap _ | Pisint | Pint_as_pointer | Pbigarraydim _ ),
      ([] | _ :: _ :: _) ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for unary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_simple_or_prim args
  | ( ( Paddint | Psubint | Pmulint | Pandint | Porint | Pxorint | Plslint
      | Plsrint | Pasrint | Pdivint _ | Pmodint _ | Psetfield _ | Pintcomp _
      | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat | Pfloatcomp _
      | Pstringrefu | Pbytesrefu | Pstringrefs | Pbytesrefs | Pstring_load_16 _
      | Pstring_load_32 _ | Pstring_load_64 _ | Pbytes_load_16 _
      | Pbytes_load_32 _ | Pbytes_load_64 _ | Pisout | Paddbint _ | Psubbint _
      | Pmulbint _ | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _
      | Plsrbint _ | Pasrbint _ | Pfield_computed _ | Pdivbint _ | Pmodbint _
      | Psetfloatfield _ | Pbintcomp _ | Pbigstring_load_16 _
      | Pbigstring_load_32 _ | Pbigstring_load_64 _
      | Parrayrefu (Pgenarray | Paddrarray | Pintarray | Pfloatarray)
      | Parrayrefs (Pgenarray | Paddrarray | Pintarray | Pfloatarray)
      | Pcompare_ints | Pcompare_floats | Pcompare_bints _ ),
      ([] | [_] | _ :: _ :: _ :: _) ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for binary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_simple_or_prim args
  | ( ( Psetfield_computed _ | Pbytessetu | Pbytessets
      | Parraysetu (Pgenarray | Paddrarray | Pintarray | Pfloatarray)
      | Parraysets (Pgenarray | Paddrarray | Pintarray | Pfloatarray)
      | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_64 _
      | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _ ),
      ([] | [_] | [_; _] | _ :: _ :: _ :: _ :: _) ) ->
    Misc.fatal_errorf
      "Closure_conversion.convert_primitive: Wrong arity for ternary primitive \
       %a (%a)"
      Printlambda.primitive prim H.print_list_of_simple_or_prim args
  | ( ( Pidentity | Pignore | Prevapply | Pdirapply | Psequand | Psequor
      | Pbytes_of_string | Pbytes_to_string ),
      _ ) ->
    Misc.fatal_errorf
      "[%a] should have been removed by [Lambda_to_flambda.transform_primitive]"
      Printlambda.primitive prim
  | Pgetglobal _, _ ->
    Misc.fatal_errorf
      "[%a] should have been handled by [Closure_conversion.close_primitive]"
      Printlambda.primitive prim

module Acc = Closure_conversion_aux.Acc
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

let convert_and_bind acc ~big_endian exn_cont ~register_const_string
    (prim : L.primitive) ~(args : Simple.t list) (dbg : Debuginfo.t)
    (cont : Acc.t -> Flambda.Named.t option -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  let expr = convert_lprim ~big_endian prim args dbg in
  H.bind_rec acc exn_cont ~register_const_string expr dbg (fun acc named ->
      cont acc (Some named))
