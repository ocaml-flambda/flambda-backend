(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Env = To_cmm_env
module Ece = Effects_and_coeffects
module EO = Exported_offsets
module K = Flambda_kind
module P = Flambda_primitive

(* Note about [Int32]: values of this kind are stored in 64-bit registers and
   must be sign extended. We do this immediately after every operation unless it
   is known that the sign extension can be elided. *)

(* Cmm helpers *)
module C = struct
  include Cmm_helpers
  include Cmm_builtins
  include To_cmm_shared
end

(* Closure offsets *)

let function_slot_offset env function_slot =
  match EO.function_slot_offset (Env.exported_offsets env) function_slot with
  | Some res -> res
  | None ->
    Misc.fatal_errorf "Missing offset for function slot %a" Function_slot.print
      function_slot

let value_slot_offset env value_slot =
  match EO.value_slot_offset (Env.exported_offsets env) value_slot with
  | Some res -> res
  | None ->
    Misc.fatal_errorf "Missing offset for value slot %a" Value_slot.print
      value_slot

(* Boxed numbers *)

let unbox_number ~dbg kind arg =
  match (kind : K.Boxable_number.t) with
  | Naked_float -> C.unbox_float dbg arg
  | Naked_float32 -> C.unbox_float32 dbg arg
  | Naked_vec128 -> C.unbox_vec128 dbg arg
  | Naked_int32 | Naked_int64 | Naked_nativeint ->
    let primitive_kind = K.Boxable_number.primitive_kind kind in
    C.unbox_int dbg primitive_kind arg

let box_number ~dbg kind alloc_mode arg =
  let alloc_mode = Alloc_mode.For_allocations.to_lambda alloc_mode in
  match (kind : K.Boxable_number.t) with
  | Naked_float32 -> C.box_float32 dbg alloc_mode arg
  | Naked_float -> C.box_float dbg alloc_mode arg
  | Naked_vec128 -> C.box_vec128 dbg alloc_mode arg
  | Naked_int32 | Naked_int64 | Naked_nativeint ->
    let primitive_kind = K.Boxable_number.primitive_kind kind in
    C.box_int_gen dbg primitive_kind alloc_mode arg

(* Block creation and access. For these functions, [index] is a tagged
   integer. *)

(* Blocks of size 0 (i.e. with an empty list of fields) must have a black
   header, or the GC will fail (cf `make_alloc_generic` in cmm_helpers.ml).

   This means they must either be statically allocated, or be pointers to one of
   the entries of the atom_table (see `startup_aux.c`).

   Both `make_alloc` and `make_float_alloc` from `cmm_helpers.ml` already check
   for that, but with an assertion, which does not produce helpful error
   messages. *)
let check_alloc_fields = function
  | [] ->
    Misc.fatal_error
      "Dynamically allocated blocks cannot have size 0 (empty arrays have to \
       be lifted so they can be statically allocated)"
  | _ -> ()

let make_block ~dbg kind alloc_mode args =
  check_alloc_fields args;
  let mode = Alloc_mode.For_allocations.to_lambda alloc_mode in
  match (kind : P.Block_kind.t) with
  | Values (tag, _) ->
    let tag = Tag.Scannable.to_int tag in
    C.make_alloc ~mode dbg ~tag args
  | Naked_floats ->
    let tag = Tag.to_int Tag.double_array_tag in
    C.make_float_alloc ~mode dbg ~tag args
  | Mixed (tag, shape) ->
    let value_prefix_size = K.Mixed_block_shape.value_prefix_size shape in
    let args_memory_chunks =
      Array.fold_right
        (fun kind acc ->
          let chunk =
            To_cmm_shared.memory_chunk_of_kind (K.With_subkind.anything kind)
          in
          chunk :: acc)
        (K.Mixed_block_shape.field_kinds shape)
        []
    in
    let tag = Tag.Scannable.to_int tag in
    C.make_mixed_alloc ~mode dbg ~tag ~value_prefix_size args args_memory_chunks

let block_load ~dbg (kind : P.Block_access_kind.t) (mutability : Mutability.t)
    ~block ~index =
  let mutability = Mutability.to_asttypes mutability in
  let load_func =
    match kind with
    | Mixed { field_kind = Value_prefix Any_value; _ }
    | Values { field_kind = Any_value; _ } ->
      C.get_field_computed Pointer
    | Mixed { field_kind = Value_prefix Immediate; _ }
    | Values { field_kind = Immediate; _ } ->
      C.get_field_computed Immediate
    | Naked_floats _ -> C.unboxed_float_array_ref
    | Mixed { field_kind = Flat_suffix field_kind; _ } -> (
      match field_kind with
      | Tagged_immediate -> C.get_field_computed Immediate
      | Naked_float -> C.unboxed_float_array_ref
      | Naked_float32 -> C.get_field_unboxed_float32
      | Naked_int32 -> C.get_field_unboxed_int32
      | Naked_int64 | Naked_nativeint -> C.get_field_unboxed_int64_or_nativeint)
  in
  load_func mutability ~block ~index dbg

let block_set ~dbg (kind : P.Block_access_kind.t) (init : P.Init_or_assign.t)
    ~block ~index ~new_value =
  let init_or_assign = P.Init_or_assign.to_lambda init in
  let set_func =
    match kind with
    | Mixed { field_kind = Value_prefix Any_value; _ }
    | Values { field_kind = Any_value; _ } ->
      C.setfield_computed Pointer init_or_assign
    | Mixed { field_kind = Value_prefix Immediate; _ }
    | Values { field_kind = Immediate; _ } ->
      C.setfield_computed Immediate init_or_assign
    | Naked_floats _ -> C.float_array_set
    | Mixed { field_kind = Flat_suffix field_kind; _ } -> (
      match field_kind with
      | Tagged_immediate -> C.setfield_computed Immediate init_or_assign
      | Naked_float -> C.float_array_set
      | Naked_float32 -> C.setfield_unboxed_float32
      | Naked_int32 -> C.setfield_unboxed_int32
      | Naked_int64 | Naked_nativeint -> C.setfield_unboxed_int64_or_nativeint)
  in
  C.return_unit dbg (set_func block index new_value dbg)

(* Array creation and access. For these functions, [index] is a tagged
   integer. *)

let make_array ~dbg kind alloc_mode args =
  check_alloc_fields args;
  let mode = Alloc_mode.For_allocations.to_lambda alloc_mode in
  match (kind : P.Array_kind.t) with
  | Immediates | Values -> C.make_alloc ~mode dbg ~tag:0 args
  | Naked_floats ->
    C.make_float_alloc ~mode dbg ~tag:(Tag.to_int Tag.double_array_tag) args
  | Naked_float32s -> C.allocate_unboxed_float32_array ~elements:args mode dbg
  | Naked_int32s -> C.allocate_unboxed_int32_array ~elements:args mode dbg
  | Naked_int64s -> C.allocate_unboxed_int64_array ~elements:args mode dbg
  | Naked_nativeints ->
    C.allocate_unboxed_nativeint_array ~elements:args mode dbg
  | Naked_vec128s -> C.allocate_unboxed_vec128_array ~elements:args mode dbg

let array_length ~dbg arr (kind : P.Array_kind.t) =
  match kind with
  | Immediates | Values | Naked_floats ->
    (* [Paddrarray] may be a lie sometimes, but we know for certain that the bit
       width of floats is equal to the machine word width (see flambda2.ml). *)
    assert (C.wordsize_shift = C.numfloat_shift);
    C.arraylength Paddrarray arr dbg
  | Naked_float32s -> C.unboxed_float32_array_length arr dbg
  | Naked_int32s -> C.unboxed_int32_array_length arr dbg
  | Naked_int64s | Naked_nativeints ->
    (* These need a special case as they are represented by custom blocks, even
       though the contents are of word width. *)
    C.unboxed_int64_or_nativeint_array_length arr dbg
  | Naked_vec128s -> C.unboxed_vec128_array_length arr dbg

let array_load_128 ~dbg ~element_width_log2 ~has_custom_ops arr index =
  let index =
    C.lsl_int (C.untag_int index dbg) (Cconst_int (element_width_log2, dbg)) dbg
  in
  let index =
    (* Skip custom_ops pointer *)
    if has_custom_ops
    then C.add_int index (Cconst_int (Arch.size_addr, dbg)) dbg
    else index
  in
  C.unaligned_load_128 arr index dbg

let array_set_128 ~dbg ~element_width_log2 ~has_custom_ops arr index new_value =
  let index =
    C.lsl_int (C.untag_int index dbg) (Cconst_int (element_width_log2, dbg)) dbg
  in
  let index =
    (* Skip custom_ops pointer *)
    if has_custom_ops
    then C.add_int index (Cconst_int (Arch.size_addr, dbg)) dbg
    else index
  in
  C.unaligned_set_128 arr index new_value dbg

let array_load ~dbg (kind : P.Array_kind.t)
    (accessor_width : P.array_accessor_width) ~arr ~index =
  (* CR mshinwell: refactor this function in the same way as [block_load] *)
  match kind, accessor_width with
  | Immediates, Scalar -> C.int_array_ref arr index dbg
  | (Naked_int64s | Naked_nativeints), Scalar ->
    C.unboxed_int64_or_nativeint_array_ref arr index dbg
  | Values, Scalar -> C.addr_array_ref arr index dbg
  | Naked_floats, Scalar ->
    C.unboxed_float_array_ref Mutable ~block:arr ~index dbg
  | Naked_float32s, Scalar -> C.unboxed_float32_array_ref arr index dbg
  | Naked_int32s, Scalar -> C.unboxed_int32_array_ref arr index dbg
  | (Immediates | Naked_floats), Vec128 ->
    array_load_128 ~dbg ~element_width_log2:3 ~has_custom_ops:false arr index
  | (Naked_int64s | Naked_nativeints), Vec128 ->
    array_load_128 ~dbg ~element_width_log2:3 ~has_custom_ops:true arr index
  | (Naked_int32s | Naked_float32s), Vec128 ->
    array_load_128 ~dbg ~element_width_log2:2 ~has_custom_ops:true arr index
  | Naked_vec128s, (Scalar | Vec128) ->
    array_load_128 ~dbg ~element_width_log2:4 ~has_custom_ops:true arr index
  | Values, Vec128 ->
    Misc.fatal_error "Attempted to load a SIMD vector from a value array."

let addr_array_store init ~arr ~index ~new_value dbg =
  (* CR mshinwell: refactor this function in the same way as [block_load] *)
  match (init : P.Init_or_assign.t) with
  | Assignment Heap -> C.addr_array_set_heap arr index new_value dbg
  | Assignment Local -> C.addr_array_set_local arr index new_value dbg
  | Initialization -> C.addr_array_initialize arr index new_value dbg

let array_set ~dbg (kind : P.Array_set_kind.t)
    (accessor_width : P.array_accessor_width) ~arr ~index ~new_value =
  (* CR mshinwell: refactor this function in the same way as [block_load] *)
  let expr =
    match kind, accessor_width with
    | Immediates, Scalar -> C.int_array_set arr index new_value dbg
    | Values init, Scalar -> addr_array_store init ~arr ~index ~new_value dbg
    | Naked_floats, Scalar -> C.float_array_set arr index new_value dbg
    | Naked_float32s, Scalar ->
      C.unboxed_float32_array_set arr ~index ~new_value dbg
    | Naked_int32s, Scalar ->
      C.unboxed_int32_array_set arr ~index ~new_value dbg
    | (Naked_int64s | Naked_nativeints), Scalar ->
      C.unboxed_int64_or_nativeint_array_set arr ~index ~new_value dbg
    | (Immediates | Naked_floats), Vec128 ->
      array_set_128 ~dbg ~element_width_log2:3 ~has_custom_ops:false arr index
        new_value
    | (Naked_int64s | Naked_nativeints), Vec128 ->
      array_set_128 ~dbg ~element_width_log2:3 ~has_custom_ops:true arr index
        new_value
    | (Naked_int32s | Naked_float32s), Vec128 ->
      array_set_128 ~dbg ~element_width_log2:2 ~has_custom_ops:true arr index
        new_value
    | Naked_vec128s, (Scalar | Vec128) ->
      array_set_128 ~dbg ~element_width_log2:4 ~has_custom_ops:true arr index
        new_value
    | Values _, Vec128 ->
      Misc.fatal_error "Attempted to store a SIMD vector to a value array."
  in
  C.return_unit dbg expr

(* Bigarrays. For these functions, [index] is a tagged integer, representing the
   desired position in the bigarray in units of the [elt_size] (so not
   necessarily in bytes, words, etc). *)

let bigarray_load_or_store ~dbg kind ~bigarray ~index f =
  let elt_kind = P.Bigarray_kind.to_lambda kind in
  let elt_size = C.bigarray_elt_size_in_bytes elt_kind in
  let elt_chunk = C.bigarray_word_kind elt_kind in
  f ~dbg ~elt_kind ~elt_size ~elt_chunk ~bigarray ~index

let bigarray_load ~dbg kind ~bigarray ~index =
  bigarray_load_or_store ~dbg kind ~bigarray ~index C.bigarray_load

let bigarray_store ~dbg kind ~bigarray ~index ~new_value =
  bigarray_load_or_store ~dbg kind ~bigarray ~index
    (C.bigarray_store ~new_value)

(* String and bytes access. For these functions, [index] is an untagged
   integer. *)

let string_like_load_aux ~dbg width ~str ~index =
  match (width : P.string_accessor_width) with
  | Eight -> C.load ~dbg Byte_unsigned Mutable ~addr:(C.add_int str index dbg)
  | Sixteen -> C.unaligned_load_16 str index dbg
  | Thirty_two -> C.sign_extend_32 dbg (C.unaligned_load_32 str index dbg)
  | Single -> C.unaligned_load_f32 str index dbg
  | Sixty_four -> C.unaligned_load_64 str index dbg
  | One_twenty_eight { aligned = true } -> C.aligned_load_128 str index dbg
  | One_twenty_eight { aligned = false } -> C.unaligned_load_128 str index dbg

let string_like_load ~dbg kind width ~str ~index =
  match (kind : P.string_like_value) with
  | String | Bytes -> string_like_load_aux ~dbg width ~str ~index
  | Bigstring ->
    let ba_data_addr = C.field_address str 1 dbg in
    let ba_data = C.load ~dbg Word_int Mutable ~addr:ba_data_addr in
    C.bind "ba_data" ba_data (fun str ->
        string_like_load_aux ~dbg width ~str ~index)

let bytes_or_bigstring_set_aux ~dbg width ~bytes ~index ~new_value =
  match (width : P.string_accessor_width) with
  | Eight ->
    let addr = C.add_int bytes index dbg in
    C.store ~dbg Byte_unsigned Assignment ~addr ~new_value
  | Sixteen -> C.unaligned_set_16 bytes index new_value dbg
  | Thirty_two -> C.unaligned_set_32 bytes index new_value dbg
  | Single -> C.unaligned_set_f32 bytes index new_value dbg
  | Sixty_four -> C.unaligned_set_64 bytes index new_value dbg
  | One_twenty_eight { aligned = false } ->
    C.unaligned_set_128 bytes index new_value dbg
  | One_twenty_eight { aligned = true } ->
    C.aligned_set_128 bytes index new_value dbg

let bytes_or_bigstring_set ~dbg kind width ~bytes ~index ~new_value =
  let expr =
    match (kind : P.bytes_like_value) with
    | Bytes -> bytes_or_bigstring_set_aux ~dbg width ~bytes ~index ~new_value
    | Bigstring ->
      let addr = C.field_address bytes 1 dbg in
      let ba_data = C.load ~dbg Word_int Mutable ~addr in
      C.bind "ba_data" ba_data (fun bytes ->
          bytes_or_bigstring_set_aux ~dbg width ~bytes ~index ~new_value)
  in
  C.return_unit dbg expr

(* Handling of dead function and value slots *)

let dead_slots_msg dbg function_slots value_slots =
  let aux s pp fmt = function
    | [] -> ()
    | _ :: _ as l ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "@[<h>%s: %a@]@ " s (Format.pp_print_list ~pp_sep pp) l
  in
  Format.asprintf "@[<hv>[%a]@ Unreachable code because of@ %a%a@]"
    Debuginfo.print_compact dbg
    (aux "dead function slots" Function_slot.print)
    function_slots
    (aux "dead value slots" Value_slot.print)
    value_slots

(* Arithmetic primitives *)

let unary_int_arith_primitive _env dbg kind op arg =
  match (kind : K.Standard_int.t), (op : P.unary_int_arith_op) with
  | Tagged_immediate, Neg -> C.negint arg dbg
  | Tagged_immediate, Swap_byte_endianness ->
    (* This isn't currently needed since [Lambda_to_flambda_primitives] always
       untags the integer first. *)
    Misc.fatal_error "Not yet implemented"
  | Naked_immediate, Swap_byte_endianness ->
    (* This case should not have a sign extension, confusingly, because it
       arises from the [Pbswap16] Lambda primitive. That operation does not
       affect the sign of the resulting value. *)
    C.bswap16 arg dbg
  (* Negation needs a sign-extension for 32-bit and 63-bit values *)
  | Naked_immediate, Neg ->
    C.sign_extend_63 dbg (C.sub_int (C.int ~dbg 0) (C.low_63 dbg arg) dbg)
  | Naked_int32, Neg ->
    C.sign_extend_32 dbg (C.sub_int (C.int ~dbg 0) (C.low_32 dbg arg) dbg)
  (* General case *)
  | (Naked_int64 | Naked_nativeint), Neg -> C.sub_int (C.int ~dbg 0) arg dbg
  (* Byte swaps of 32-bit integers on 64-bit targets need a sign-extension in
     order to match the Lambda semantics (where the swap might affect the
     sign). *)
  | Naked_int32, Swap_byte_endianness ->
    C.sign_extend_32 dbg (C.bbswap Pint32 arg dbg)
  | Naked_int64, Swap_byte_endianness -> C.bbswap Pint64 arg dbg
  | Naked_nativeint, Swap_byte_endianness -> C.bbswap Pnativeint arg dbg

let unary_float_arith_primitive _env dbg width op arg =
  match (width : P.float_bitwidth), (op : P.unary_float_arith_op) with
  | Float64, Abs -> C.float_abs ~dbg arg
  | Float64, Neg -> C.float_neg ~dbg arg
  | Float32, Abs -> C.float32_abs ~dbg arg
  | Float32, Neg -> C.float32_neg ~dbg arg

let arithmetic_conversion dbg src dst arg =
  let open K.Standard_int_or_float in
  match src, dst with
  (* Float-Float conversions *)
  | Naked_float32, Naked_float32 -> None, arg
  | Naked_float, Naked_float -> None, arg
  | Naked_float, Naked_float32 -> None, C.float32_of_float ~dbg arg
  | Naked_float32, Naked_float -> None, C.float_of_float32 ~dbg arg
  (* Conversions to and from tagged ints *)
  | ( (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
      Tagged_immediate ) ->
    None, C.tag_int arg dbg
  | Tagged_immediate, (Naked_int64 | Naked_nativeint) ->
    Some (Env.Untag arg), C.untag_int arg dbg
  (* Operations resulting in int32s must take care to sign extend the result *)
  (* CR-someday xclerc: untag_int followed by sign_extend_32 sounds suboptimal,
     as it performs asr 1; lsl 32; asr 32 while we could do lsl 31; asr 32
     instead. (Beware of the optimizations / pattern matching in the helpers
     though.) *)
  | Tagged_immediate, Naked_int32 ->
    None, C.sign_extend_32 dbg (C.untag_int arg dbg)
  | Tagged_immediate, Naked_immediate ->
    None, C.sign_extend_63 dbg (C.untag_int arg dbg)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Naked_int32
    ->
    None, C.sign_extend_32 dbg arg
  (* No-op conversions *)
  | Tagged_immediate, Tagged_immediate
  | Naked_int32, (Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_int64, (Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_nativeint, (Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_immediate, (Naked_int64 | Naked_nativeint | Naked_immediate) ->
    None, arg
  (* Int-Float32 conversions *)
  | Tagged_immediate, Naked_float32 ->
    None, C.float32_of_int ~dbg (C.untag_int arg dbg)
  | ( (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint),
      Naked_float32 ) ->
    None, C.float32_of_int ~dbg arg
  | Naked_float32, Tagged_immediate ->
    None, C.tag_int (C.int_of_float32 ~dbg arg) dbg
  | Naked_float32, (Naked_int64 | Naked_nativeint) ->
    None, C.int_of_float32 ~dbg arg
  | Naked_float32, Naked_int32 ->
    None, C.sign_extend_32 dbg (C.int_of_float32 ~dbg arg)
  | Naked_float32, Naked_immediate ->
    None, C.sign_extend_63 dbg (C.int_of_float32 ~dbg arg)
    (* Int-Float conversions *)
  | Tagged_immediate, Naked_float ->
    None, C.float_of_int ~dbg (C.untag_int arg dbg)
  | (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint), Naked_float
    ->
    None, C.float_of_int ~dbg arg
  | Naked_float, Tagged_immediate ->
    None, C.tag_int (C.int_of_float ~dbg arg) dbg
  | Naked_float, (Naked_int64 | Naked_nativeint) ->
    None, C.int_of_float ~dbg arg
  | Naked_float, Naked_int32 ->
    None, C.sign_extend_32 dbg (C.int_of_float ~dbg arg)
  | Naked_float, Naked_immediate ->
    None, C.sign_extend_63 dbg (C.int_of_float ~dbg arg)

let phys_equal _env dbg op x y =
  match (op : P.equality_comparison) with
  (* General case *)
  | Eq -> C.eq ~dbg x y
  | Neq -> C.neq ~dbg x y

let binary_int_arith_primitive _env dbg (kind : K.Standard_int.t)
    (op : P.binary_int_arith_op) x y =
  match kind with
  | Tagged_immediate -> (
    match op with
    | Add -> C.add_int_caml x y dbg
    | Sub -> C.sub_int_caml x y dbg
    | Mul -> C.mul_int_caml x y dbg
    | Div -> C.div_int_caml Unsafe x y dbg
    | Mod -> C.mod_int_caml Unsafe x y dbg
    | And -> C.and_int_caml x y dbg
    | Or -> C.or_int_caml x y dbg
    | Xor -> C.xor_int_caml x y dbg)
  | Naked_int32 -> (
    (* Operations on 32-bit integer arguments must return something in the range
       of 32-bit integers, hence the sign_extensions here. The [C.low_32]
       operations (see above in [sign_extend_32_can_delay_overflow]) are used to
       avoid unnecessary sign extensions, e.g. when chaining additions together.
       Also see comment below about [C.low_32] in the [Div] and [Mod] cases. *)
    let sign_extend_32_can_delay_overflow f =
      C.sign_extend_32 dbg (f (C.low_32 dbg x) (C.low_32 dbg y) dbg)
    in
    match op with
    | Add -> sign_extend_32_can_delay_overflow C.add_int
    | Sub -> sign_extend_32_can_delay_overflow C.sub_int
    | Mul -> sign_extend_32_can_delay_overflow C.mul_int
    | Xor -> sign_extend_32_can_delay_overflow C.xor_int
    | And -> sign_extend_32_can_delay_overflow C.and_int
    | Or -> sign_extend_32_can_delay_overflow C.or_int
    | Div ->
      (* Note that it would be wrong to apply [C.low_32] to [x] and/or [y] here
         -- likewise in the next [Mod] case. [C.safe_div_bi] and [C.safe_mod_bi]
         require sign-extended input for both the numerator and denominator.

         Some background: The problem arises in cases like: (num1 * num2) /
         num3. If an overflow occurs in the multiplication, then we must deal
         with it by sign-extending before the division. Whereas (num1 * num2) *
         num3 can delay the sign-extension until the very end, even in the case
         of overflow in the middle. So in a way, div and mod are regular
         functions, while all the others are special as they can delay overflow
         handling.

         We don't have 32-bit registers in Cmm, so sign extension really means
         modulo 2^31. (If we had 32-bit virtual registers, we could use them in
         Cmm without sign extension and let the backend insert sign extensions
         if it doesn't support operations on 32-bit physical registers. There
         was a prototype developed of this but it was quite complicated and
         didn't get merged.) *)
      C.sign_extend_32 dbg (C.safe_div_bi Unsafe x y Pint32 dbg)
    | Mod -> C.sign_extend_32 dbg (C.safe_mod_bi Unsafe x y Pint32 dbg))
  | Naked_immediate -> (
    let sign_extend_63_can_delay_overflow f =
      C.sign_extend_63 dbg (f (C.low_63 dbg x) (C.low_63 dbg y) dbg)
    in
    match op with
    | Add -> sign_extend_63_can_delay_overflow C.add_int
    | Sub -> sign_extend_63_can_delay_overflow C.sub_int
    | Mul -> sign_extend_63_can_delay_overflow C.mul_int
    | Xor -> sign_extend_63_can_delay_overflow C.xor_int
    | And -> sign_extend_63_can_delay_overflow C.and_int
    | Or -> sign_extend_63_can_delay_overflow C.or_int
    | Div -> C.sign_extend_63 dbg (C.safe_div_bi Unsafe x y Pint64 dbg)
    | Mod -> C.sign_extend_63 dbg (C.safe_mod_bi Unsafe x y Pint64 dbg))
  | Naked_int64 | Naked_nativeint -> (
    (* Machine-width integers, no sign extension required. *)
    let bi : Primitive.boxed_integer =
      match kind with
      | Naked_int64 -> Pint64
      | Naked_nativeint -> Pnativeint
      | Naked_int32 | Naked_immediate | Tagged_immediate -> assert false
    in
    match op with
    | Add -> C.add_int x y dbg
    | Sub -> C.sub_int x y dbg
    | Mul -> C.mul_int x y dbg
    | And -> C.and_int x y dbg
    | Or -> C.or_int x y dbg
    | Xor -> C.xor_int x y dbg
    | Div -> C.safe_div_bi Unsafe x y bi dbg
    | Mod -> C.safe_mod_bi Unsafe x y bi dbg)

let binary_int_shift_primitive _env dbg kind op x y =
  match (kind : K.Standard_int.t), (op : P.int_shift_op) with
  (* caml primitives for these have no native/unboxed version *)
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> C.lsl_int_caml_raw ~dbg x y
  | Tagged_immediate, Lsr -> C.lsr_int_caml_raw ~dbg x y
  | Tagged_immediate, Asr -> C.asr_int_caml_raw ~dbg x y
  (* See comments on [binary_int_arity_primitive], above, about sign extension
     and use of [C.low_32]. *)
  | Naked_int32, Lsl -> C.sign_extend_32 dbg (C.lsl_int (C.low_32 dbg x) y dbg)
  | Naked_int32, Lsr ->
    (* Ensure that the top half of the register is cleared, as some of those
       bits are likely to get shifted into the result. *)
    let arg = C.zero_extend_32 dbg x in
    C.sign_extend_32 dbg (C.lsr_int arg y dbg)
  | Naked_int32, Asr -> C.sign_extend_32 dbg (C.asr_int x y dbg)
  | Naked_immediate, Lsl ->
    C.sign_extend_63 dbg (C.lsl_int (C.low_63 dbg x) y dbg)
  | Naked_immediate, Lsr ->
    (* Same comment as in the [Naked_int32] case above re. zero extension. *)
    let arg = C.zero_extend_63 dbg x in
    C.sign_extend_63 dbg (C.lsr_int arg y dbg)
  | Naked_immediate, Asr -> C.sign_extend_63 dbg (C.asr_int x y dbg)
  (* Naked ints *)
  | (Naked_int64 | Naked_nativeint), Lsl -> C.lsl_int x y dbg
  | (Naked_int64 | Naked_nativeint), Lsr -> C.lsr_int x y dbg
  | (Naked_int64 | Naked_nativeint), Asr -> C.asr_int x y dbg

let binary_int_comp_primitive _env dbg kind cmp x y =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (cmp : P.signed_or_unsigned P.comparison) )
  with
  (* [x] and [y] are expressions yielding well-formed tagged immediates, that is
     to say, their least significant bit (LSB) is 1. However when comparing
     tagged immediates, there always exists one argument (i.e. either [x] or
     [y]) for which the setting of that LSB makes no difference to the result.
     This means that we can optimise in the case where the argument in question
     contains a tagging operation (or logical OR operation setting the last bit)
     by removing such operation.

     See middle_end/flambda2/z3/comparisons.smt2 for a Z3 script to prove
     this. *)
  | Tagged_immediate, Lt Signed -> C.lt ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Le Signed -> C.le ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Gt Signed -> C.gt ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Ge Signed -> C.ge ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Lt Unsigned -> C.ult ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Le Unsigned -> C.ule ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Gt Unsigned -> C.ugt ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Ge Unsigned -> C.uge ~dbg x (C.ignore_low_bit_int y)
  (* Naked integers. *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lt Signed
    ->
    C.lt ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Le Signed
    ->
    C.le ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Gt Signed
    ->
    C.gt ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Ge Signed
    ->
    C.ge ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lt Unsigned
    ->
    C.ult ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Le Unsigned
    ->
    C.ule ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Gt Unsigned
    ->
    C.ugt ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Ge Unsigned
    ->
    C.uge ~dbg x y
  | ( ( Tagged_immediate | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      Eq ) ->
    C.eq ~dbg x y
  | ( ( Tagged_immediate | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      Neq ) ->
    C.neq ~dbg x y

let binary_int_comp_primitive_yielding_int _env dbg _kind
    (signed : P.signed_or_unsigned) x y =
  match signed with
  | Signed -> C.mk_compare_ints_untagged dbg x y
  | Unsigned ->
    Misc.fatal_error
      "Translation of [Int_comp] yielding an integer -1, 0 or 1 in unsigned \
       mode is not yet implemented"

let binary_float_arith_primitive _env dbg width op x y =
  match (width : P.float_bitwidth), (op : P.binary_float_arith_op) with
  | Float64, Add -> C.float_add ~dbg x y
  | Float64, Sub -> C.float_sub ~dbg x y
  | Float64, Mul -> C.float_mul ~dbg x y
  | Float64, Div -> C.float_div ~dbg x y
  | Float32, Add -> C.float32_add ~dbg x y
  | Float32, Sub -> C.float32_sub ~dbg x y
  | Float32, Mul -> C.float32_mul ~dbg x y
  | Float32, Div -> C.float32_div ~dbg x y

let binary_float_comp_primitive _env dbg width op x y =
  match (width : P.float_bitwidth), (op : unit P.comparison) with
  | Float64, Eq -> C.float_eq ~dbg x y
  | Float64, Neq -> C.float_neq ~dbg x y
  | Float64, Lt () -> C.float_lt ~dbg x y
  | Float64, Gt () -> C.float_gt ~dbg x y
  | Float64, Le () -> C.float_le ~dbg x y
  | Float64, Ge () -> C.float_ge ~dbg x y
  | Float32, Eq -> C.float32_eq ~dbg x y
  | Float32, Neq -> C.float32_neq ~dbg x y
  | Float32, Lt () -> C.float32_lt ~dbg x y
  | Float32, Gt () -> C.float32_gt ~dbg x y
  | Float32, Le () -> C.float32_le ~dbg x y
  | Float32, Ge () -> C.float32_ge ~dbg x y

let binary_float_comp_primitive_yielding_int _env dbg width x y =
  match (width : P.float_bitwidth) with
  | Float64 -> C.mk_compare_floats_untagged dbg x y
  | Float32 -> C.mk_compare_float32s_untagged dbg x y

(* Primitives *)

let nullary_primitive _env res dbg prim =
  match (prim : P.nullary_primitive) with
  | Invalid _ ->
    let message = "Invalid primitive" in
    let expr, res = C.invalid res ~message in
    None, res, expr
  | Optimised_out _ -> Misc.fatal_errorf "TODO: phantom let-bindings in to_cmm"
  | Probe_is_enabled { name } ->
    (* CR gbury: we should never manually build cmm expression in this file. We
       should instead always use smart constructors defined in `cmm_helpers` or
       `to_cmm_shared.ml` *)
    let expr = Cmm.Cop (Cprobe_is_enabled { name }, [], dbg) in
    None, res, expr
  | Begin_region { ghost = false } | Begin_try_region { ghost = false } ->
    None, res, C.beginregion ~dbg
  | Begin_region { ghost = true } | Begin_try_region { ghost = true } ->
    None, res, C.int ~dbg 0
  | Enter_inlined_apply _ ->
    Misc.fatal_errorf
      "The primitive [Enter_inlined_apply] should not be translated by \
       [to_cmm_primitive] but should instead be handled in [to_cmm_expr] to \
       correctly adjust the inlined debuginfo in the env."
  | Dls_get -> None, res, C.dls_get ~dbg

let unary_primitive env res dbg f arg =
  match (f : P.unary_primitive) with
  | Duplicate_array _ | Duplicate_block _ | Obj_dup ->
    ( None,
      res,
      C.extcall ~dbg ~alloc:true ~returns:true ~is_c_builtin:false
        ~effects:Arbitrary_effects ~coeffects:Has_coeffects ~ty_args:[]
        "caml_obj_dup" Cmm.typ_val [arg] )
  | Is_int _ -> None, res, C.and_int arg (C.int ~dbg 1) dbg
  | Get_tag -> None, res, C.get_tag arg dbg
  | Array_length (Array_kind array_kind) ->
    None, res, array_length ~dbg arg array_kind
  | Array_length Float_array_opt_dynamic ->
    (* See flambda2.ml (and comment in [array_length], above). *)
    None, res, array_length ~dbg arg Values
  | Bigarray_length { dimension } ->
    ( None,
      res,
      C.load ~dbg Word_int Mutable
        ~addr:(C.field_address arg (4 + dimension) dbg) )
  | String_length _ -> None, res, C.string_length arg dbg
  | Int_as_pointer _ -> None, res, C.int_as_pointer arg dbg
  | Opaque_identity { middle_end_only = true; kind = _ } -> None, res, arg
  | Opaque_identity { middle_end_only = false; kind = _ } ->
    None, res, C.opaque arg dbg
  | Int_arith (kind, op) ->
    None, res, unary_int_arith_primitive env dbg kind op arg
  | Float_arith (width, op) ->
    None, res, unary_float_arith_primitive env dbg width op arg
  | Num_conv { src; dst } ->
    let extra, expr = arithmetic_conversion dbg src dst arg in
    extra, res, expr
  | Boolean_not -> None, res, C.mk_not dbg arg
  | Reinterpret_64_bit_word reinterpret ->
    let cmm =
      match reinterpret with
      | Tagged_int63_as_unboxed_int64 -> arg
      | Unboxed_int64_as_tagged_int63 -> C.or_int (C.int 1 ~dbg) arg dbg
      | Unboxed_int64_as_unboxed_float64 -> C.int64_as_float ~dbg arg
      | Unboxed_float64_as_unboxed_int64 -> C.float_as_int64 ~dbg arg
    in
    None, res, cmm
  | Unbox_number kind -> None, res, unbox_number ~dbg kind arg
  | Untag_immediate -> Some (Env.Untag arg), res, C.untag_int arg dbg
  | Box_number (kind, alloc_mode) ->
    None, res, box_number ~dbg kind alloc_mode arg
  | Tag_immediate ->
    (* We could return [Env.Tag] here, but probably unnecessary at the
       moment. *)
    None, res, C.tag_int arg dbg
  | Project_function_slot { move_from = c1; move_to = c2 } -> (
    match function_slot_offset env c1, function_slot_offset env c2 with
    | ( Live_function_slot { offset = c1_offset; _ },
        Live_function_slot { offset = c2_offset; _ } ) ->
      (* Normal case. *)
      let diff = c2_offset - c1_offset in
      None, res, C.infix_field_address ~dbg arg diff
    | Dead_function_slot, Live_function_slot _ ->
      (* Code whose projections involve dead slots (ones that have been removed)
         should be unreachable. *)
      let message = dead_slots_msg dbg [c1] [] in
      let expr, res = C.invalid res ~message in
      None, res, expr
    | Live_function_slot _, Dead_function_slot ->
      let message = dead_slots_msg dbg [c2] [] in
      let expr, res = C.invalid res ~message in
      None, res, expr
    | Dead_function_slot, Dead_function_slot ->
      let message = dead_slots_msg dbg [c1; c2] [] in
      let expr, res = C.invalid res ~message in
      None, res, expr)
  | Project_value_slot { project_from; value_slot } -> (
    let kind = Value_slot.kind value_slot in
    match
      value_slot_offset env value_slot, function_slot_offset env project_from
    with
    | Live_value_slot { offset; _ }, Live_function_slot { offset = base; _ } ->
      let memory_chunk = To_cmm_shared.memory_chunk_of_kind kind in
      let expr =
        C.get_field_gen_given_memory_chunk memory_chunk Asttypes.Immutable arg
          (offset - base) dbg
      in
      None, res, expr
    | Dead_value_slot, Live_function_slot _ ->
      let message = dead_slots_msg dbg [] [value_slot] in
      let expr, res = C.invalid res ~message in
      None, res, expr
    | Live_value_slot _, Dead_function_slot ->
      let message = dead_slots_msg dbg [project_from] [] in
      let expr, res = C.invalid res ~message in
      None, res, expr
    | Dead_value_slot, Dead_function_slot ->
      let message = dead_slots_msg dbg [project_from] [value_slot] in
      let expr, res = C.invalid res ~message in
      None, res, expr)
  | Is_boxed_float ->
    (* As a note, this omits the [Is_in_value_area] check that exists in
       [caml_make_array], which is used by non-Flambda 2 compilers. This seems
       reasonable given known existing use cases of naked pointers and the fact
       that they will be forbidden entirely in OCaml 5. *)
    ( None,
      res,
      C.ite
        (C.and_int arg (C.int 1 ~dbg) dbg)
        ~dbg ~then_:(C.int 0 ~dbg) ~then_dbg:dbg
        ~else_:(C.eq (C.get_tag arg dbg) (C.int Obj.double_tag ~dbg) ~dbg)
        ~else_dbg:dbg )
  | Is_flat_float_array ->
    None, res, C.eq ~dbg (C.get_tag arg dbg) (C.floatarray_tag dbg)
  | End_region { ghost = false } | End_try_region { ghost = false } ->
    None, res, C.return_unit dbg (C.endregion ~dbg arg)
  | End_region { ghost = true } | End_try_region { ghost = true } ->
    None, res, C.unit ~dbg
  | Get_header -> None, res, C.get_header arg dbg
  | Atomic_load block_access_kind ->
    let imm_or_ptr : Lambda.immediate_or_pointer =
      match block_access_kind with
      | Any_value -> Pointer
      | Immediate -> Immediate
    in
    None, res, C.atomic_load ~dbg imm_or_ptr arg

let binary_primitive env dbg f x y =
  match (f : P.binary_primitive) with
  | Block_load (kind, mut) -> block_load ~dbg kind mut ~block:x ~index:y
  | Array_load (kind, width, _mut) -> array_load ~dbg kind width ~arr:x ~index:y
  | String_or_bigstring_load (kind, width) ->
    string_like_load ~dbg kind width ~str:x ~index:y
  | Bigarray_load (_dimensions, kind, _layout) ->
    bigarray_load ~dbg kind ~bigarray:x ~index:y
  | Phys_equal op -> phys_equal env dbg op x y
  | Int_arith (kind, op) -> binary_int_arith_primitive env dbg kind op x y
  | Int_shift (kind, op) -> binary_int_shift_primitive env dbg kind op x y
  | Int_comp (kind, Yielding_bool cmp) ->
    binary_int_comp_primitive env dbg kind cmp x y
  | Int_comp (kind, Yielding_int_like_compare_functions signed) ->
    binary_int_comp_primitive_yielding_int env dbg kind signed x y
  | Float_arith (width, op) -> binary_float_arith_primitive env dbg width op x y
  | Float_comp (width, Yielding_bool cmp) ->
    binary_float_comp_primitive env dbg width cmp x y
  | Float_comp (width, Yielding_int_like_compare_functions ()) ->
    binary_float_comp_primitive_yielding_int env dbg width x y
  | Bigarray_get_alignment align -> C.bigstring_get_alignment x y align dbg
  | Atomic_exchange -> C.atomic_exchange ~dbg x y
  | Atomic_fetch_and_add -> C.atomic_fetch_and_add ~dbg x y

let ternary_primitive _env dbg f x y z =
  match (f : P.ternary_primitive) with
  | Block_set (block_access, init) ->
    block_set ~dbg block_access init ~block:x ~index:y ~new_value:z
  | Array_set (array_set_kind, width) ->
    array_set ~dbg array_set_kind width ~arr:x ~index:y ~new_value:z
  | Bytes_or_bigstring_set (kind, width) ->
    bytes_or_bigstring_set ~dbg kind width ~bytes:x ~index:y ~new_value:z
  | Bigarray_set (_dimensions, kind, _layout) ->
    bigarray_store ~dbg kind ~bigarray:x ~index:y ~new_value:z
  | Atomic_compare_and_set ->
    C.atomic_compare_and_set ~dbg x ~old_value:y ~new_value:z

let variadic_primitive _env dbg f args =
  match (f : P.variadic_primitive) with
  | Make_block (kind, _mut, alloc_mode) -> make_block ~dbg kind alloc_mode args
  | Make_array (kind, _mut, alloc_mode) -> make_array ~dbg kind alloc_mode args

let arg ?consider_inlining_effectful_expressions ~dbg env res simple =
  C.simple ?consider_inlining_effectful_expressions ~dbg env res simple

let arg_list ?consider_inlining_effectful_expressions ~dbg env res l =
  let aux (list, free_vars, env, res, effs) x =
    let To_cmm_env.{ env; res; expr } =
      arg ?consider_inlining_effectful_expressions ~dbg env res x
    in
    let free_vars = Backend_var.Set.union free_vars expr.free_vars in
    expr.cmm :: list, free_vars, env, res, Ece.join expr.effs effs
  in
  let args, free_vars, env, res, effs =
    List.fold_left aux
      ([], Backend_var.Set.empty, env, res, Ece.pure_can_be_duplicated)
      l
  in
  List.rev args, free_vars, env, res, effs

let arg_list' ?consider_inlining_effectful_expressions ~dbg env res l =
  let aux (list, env, res, effs) x =
    let To_cmm_env.{ env; res; expr } =
      arg ?consider_inlining_effectful_expressions ~dbg env res x
    in
    expr :: list, env, res, Ece.join expr.effs effs
  in
  let args, env, res, effs =
    List.fold_left aux ([], env, res, Ece.pure_can_be_duplicated) l
  in
  List.rev args, env, res, effs

let trans_prim : To_cmm_env.t To_cmm_env.trans_prim =
  { nullary = nullary_primitive;
    unary = unary_primitive;
    binary =
      (fun env res dbg prim x y ->
        let cmm = binary_primitive env dbg prim x y in
        None, res, cmm);
    ternary =
      (fun env res dbg prim x y z ->
        let cmm = ternary_primitive env dbg prim x y z in
        None, res, cmm);
    variadic =
      (fun env res dbg prim args ->
        let cmm = variadic_primitive env dbg prim args in
        None, res, cmm)
  }

let consider_inlining_effectful_expressions p =
  (* By default we are very conservative about the inlining of effectful
     expressions into the arguments of primitives. We consider inlining in the
     following cases:

     - in the case where the primitive compiles directly to an allocation.
     Unlike for most primitives, inlining of the arguments gives a real benefit
     for these, by keeping live ranges shorter (which could be critical for
     register allocation performance in cases such as initialisation of very
     large arrays). We are also confident that the code for compiling
     allocations does not incorrectly reorder or duplicate arguments, whereas we
     are not universally confident about that for the other Cmm translation
     functions.

     This criterion should not be relaxed for any primitive until it is certain
     that the Cmm translation for such primitive both respects right-to-left
     evaluation order and does not duplicate any arguments. *)
  match[@ocaml.warning "-4"] (p : P.t) with
  | Variadic ((Make_block _ | Make_array _), _) -> Some true
  | Nullary _ | Unary _ | Binary _ | Ternary _ -> None

let prim_simple env res dbg p =
  let consider_inlining_effectful_expressions =
    consider_inlining_effectful_expressions p
  in
  let arg = arg ?consider_inlining_effectful_expressions ~dbg in
  (* Somewhat counter-intuitively, the left-to-right translation below (e.g. [x]
     before [y] in the [Binary] case) correctly matches right-to-left evaluation
     order---ensuring maximal inlining---since [arg_list] translates the first
     [Simple] in the list first. Consider in pseudo-code:

     let x = <effect-x> in let y = <effect-y> in Make_block [y; x]

     We would like both [x] and [y] to be inlined. The environment will have [y]
     on the most recent stage since it was the most recent binding. The [Simple]
     for [y] will be translated first by the code below, meaning inlining is
     permitted (since [y] is on the most recent stage), producing Make_block
     [effect-y; y]. Then the [Simple] for [x] will be translated, producing the
     desired output Make_block [effect-y; effect-x]. The backend will compile
     this to run effect-x before effect-y by virtue of right-to-left evaluation
     order. This therefore matches the original source code. *)
  match (p : P.t) with
  | Nullary prim ->
    let free_vars = Backend_var.Set.empty in
    let extra, res, expr = nullary_primitive env res dbg prim in
    Env.simple expr free_vars, extra, env, res, Ece.pure
  | Unary (unary, x) ->
    let To_cmm_env.{ env; res; expr = x } = arg env res x in
    let extra, res, expr = unary_primitive env res dbg unary x.cmm in
    Env.simple expr x.free_vars, extra, env, res, x.effs
  | Binary (binary, x, y) ->
    let To_cmm_env.{ env; res; expr = x } = arg env res x in
    let To_cmm_env.{ env; res; expr = y } = arg env res y in
    let free_vars = Backend_var.Set.union x.free_vars y.free_vars in
    let effs = Ece.join x.effs y.effs in
    let expr = binary_primitive env dbg binary x.cmm y.cmm in
    Env.simple expr free_vars, None, env, res, effs
  | Ternary (ternary, x, y, z) ->
    let To_cmm_env.{ env; res; expr = x } = arg env res x in
    let To_cmm_env.{ env; res; expr = y } = arg env res y in
    let To_cmm_env.{ env; res; expr = z } = arg env res z in
    let free_vars =
      Backend_var.Set.union
        (Backend_var.Set.union x.free_vars y.free_vars)
        z.free_vars
    in
    let effs = Ece.join (Ece.join x.effs y.effs) z.effs in
    let expr = ternary_primitive env dbg ternary x.cmm y.cmm z.cmm in
    Env.simple expr free_vars, None, env, res, effs
  | Variadic (((Make_block _ | Make_array _) as variadic), l) ->
    let args, free_vars, env, res, effs =
      arg_list ?consider_inlining_effectful_expressions ~dbg env res l
    in
    let expr = variadic_primitive env dbg variadic args in
    Env.simple expr free_vars, None, env, res, effs

let prim_complex env res dbg p =
  let consider_inlining_effectful_expressions =
    consider_inlining_effectful_expressions p
  in
  let arg = arg ?consider_inlining_effectful_expressions ~dbg in
  (* see comment in [prim_simple] *)
  let prim', args, effs, env, res =
    match (p : P.t) with
    | Nullary prim ->
      let prim' = P.Without_args.Nullary prim in
      prim', [], Ece.pure_can_be_duplicated, env, res
    | Unary (unary, x) ->
      let prim' = P.Without_args.Unary unary in
      let To_cmm_env.{ env; res; expr = x } = arg env res x in
      prim', [x], x.effs, env, res
    | Binary (binary, x, y) ->
      let prim' = P.Without_args.Binary binary in
      let To_cmm_env.{ env; res; expr = x } = arg env res x in
      let To_cmm_env.{ env; res; expr = y } = arg env res y in
      let effs = Ece.join x.effs y.effs in
      prim', [x; y], effs, env, res
    | Ternary (ternary, x, y, z) ->
      let prim' = P.Without_args.Ternary ternary in
      let To_cmm_env.{ env; res; expr = x } = arg env res x in
      let To_cmm_env.{ env; res; expr = y } = arg env res y in
      let To_cmm_env.{ env; res; expr = z } = arg env res z in
      let effs = Ece.join (Ece.join x.effs y.effs) z.effs in
      prim', [x; y; z], effs, env, res
    | Variadic (((Make_block _ | Make_array _) as variadic), l) ->
      let prim' = P.Without_args.Variadic variadic in
      let args, env, res, effs =
        arg_list' ?consider_inlining_effectful_expressions ~dbg env res l
      in
      prim', args, effs, env, res
  in
  let bound_expr = Env.splittable_primitive dbg prim' args in
  bound_expr, env, res, effs
