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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Env = To_cmm_env
module Ece = Effects_and_coeffects
module P = Flambda_primitive

(* Cmm helpers *)
module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

(* Closure offsets *)

let function_slot_offset env function_slot =
  match
    Exported_offsets.function_slot_offset (Env.exported_offsets env)
      function_slot
  with
  | Some res -> res
  | None ->
    Misc.fatal_errorf "Missing offset for function slot %a" Function_slot.print
      function_slot

let value_slot_offset env value_slot =
  match
    Exported_offsets.value_slot_offset (Env.exported_offsets env) value_slot
  with
  | Some res -> res
  | None ->
    Misc.fatal_errorf "Missing offset for value slot %a" Value_slot.print
      value_slot

(* Boxed numbers *)

let primitive_boxed_int_of_boxable_number b =
  match (b : Flambda_kind.Boxable_number.t) with
  | Naked_float -> assert false
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint

let unbox_number ?(dbg = Debuginfo.none) kind arg =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> C.unbox_float dbg arg
  | _ ->
    let primitive_kind = primitive_boxed_int_of_boxable_number kind in
    C.unbox_int dbg primitive_kind arg

let box_number ?(dbg = Debuginfo.none) kind alloc_mode arg =
  let alloc_mode = Alloc_mode.to_lambda alloc_mode in
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> C.box_float dbg alloc_mode arg
  | _ ->
    let primitive_kind = primitive_boxed_int_of_boxable_number kind in
    C.box_int_gen dbg primitive_kind alloc_mode arg

(* Block creation and access. For these functions, [index] is a tagged
   integer. *)

(* Blocks of size 0 (i.e. with an empty list of fields) must be statically
   allocated, else the GC will bug (cf `make_alloc_generic` in cmm_helpers.ml).
   More precisely, blocks of size 0 must have a black header, which means they
   must either be statically allocated, or be pointers to one of the cell of the
   atom_table (see `startup_aux.c`).

   Both `make_alloc` and `make_float_alloc` from `cmm_helpers.ml` already check
   for that, but with an assertion, which do not produce helpful error
   messages. *)
let check_alloc_fields = function
  | [] ->
    Misc.fatal_error
      "Blocks dynamically allocated cannot have size 0 (empty arrays have to \
       be lifted so they can be statically allocated)"
  | _ -> ()

let make_block ?(dbg = Debuginfo.none) kind alloc_mode args =
  check_alloc_fields args;
  match (kind : Flambda_primitive.Block_kind.t) with
  | Values (tag, _) ->
    C.make_alloc
      ~mode:(Alloc_mode.to_lambda alloc_mode)
      dbg (Tag.Scannable.to_int tag) args
  | Naked_floats ->
    C.make_float_alloc
      ~mode:(Alloc_mode.to_lambda alloc_mode)
      dbg
      (Tag.to_int Tag.double_array_tag)
      args

let block_load ?(dbg = Debuginfo.none) (kind : P.Block_access_kind.t)
    (mutability : Mutability.t) block index =
  let mutability = Mutability.to_lambda mutability in
  match kind with
  | Values { field_kind = Any_value; _ } ->
    C.get_field_computed Pointer mutability ~block ~index dbg
  | Values { field_kind = Immediate; _ } ->
    C.get_field_computed Immediate mutability ~block ~index dbg
  | Naked_floats _ -> C.unboxed_float_array_ref block index dbg

let block_set ?(dbg = Debuginfo.none) (kind : P.Block_access_kind.t)
    (init : P.Init_or_assign.t) block index new_value =
  let init_or_assign = P.Init_or_assign.to_lambda init in
  match kind with
  | Values { field_kind = Any_value; _ } ->
    C.setfield_computed Pointer init_or_assign block index new_value dbg
    |> C.return_unit dbg
  | Values { field_kind = Immediate; _ } ->
    C.setfield_computed Immediate init_or_assign block index new_value dbg
    |> C.return_unit dbg
  | Naked_floats _ ->
    C.float_array_set block index new_value dbg |> C.return_unit dbg

(* Array creation and access. *)

let make_array ?(dbg = Debuginfo.none) kind alloc_mode args =
  check_alloc_fields args;
  match (kind : Flambda_primitive.Array_kind.t) with
  | Immediates | Values ->
    C.make_alloc ~mode:(Alloc_mode.to_lambda alloc_mode) dbg 0 args
  | Naked_floats ->
    C.make_float_alloc
      ~mode:(Alloc_mode.to_lambda alloc_mode)
      dbg
      (Tag.to_int Tag.double_array_tag)
      args

let array_length ?(dbg = Debuginfo.none) arr =
  (* [Paddrarray] may be a lie sometimes, but we know for certain that the bit
     width of floats is equal to the machine word width (see flambda2.ml). This
     means that [arraylength] will not use the kind information. *)
  assert (C.wordsize_shift = C.numfloat_shift);
  C.arraylength Paddrarray arr dbg

let array_load ?(dbg = Debuginfo.none) (kind : P.Array_kind.t) arr index =
  match kind with
  | Immediates -> C.int_array_ref arr index dbg
  | Values -> C.addr_array_ref arr index dbg
  | Naked_floats -> C.unboxed_float_array_ref arr index dbg

let addr_array_store init arr index value dbg =
  match (init : P.Init_or_assign.t) with
  | Assignment Heap -> C.addr_array_set arr index value dbg
  | Assignment Local -> C.addr_array_set_local arr index value dbg
  | Initialization -> C.addr_array_initialize arr index value dbg

let array_set ?(dbg = Debuginfo.none) (kind : P.Array_kind.t)
    (init : P.Init_or_assign.t) arr index value =
  match kind with
  | Immediates -> C.return_unit dbg (C.int_array_set arr index value dbg)
  | Values -> C.return_unit dbg (addr_array_store init arr index value dbg)
  | Naked_floats -> C.return_unit dbg (C.float_array_set arr index value dbg)

(* Bigarrays *)

(* CR mshinwell: Add [Flambda_primitive.Bigarray_kind] and move this function
   there *)
let lambda_ba_kind k : Lambda.bigarray_kind =
  match (k : Flambda_primitive.bigarray_kind) with
  | Float32 -> Pbigarray_float32
  | Float64 -> Pbigarray_float64
  | Sint8 -> Pbigarray_sint8
  | Uint8 -> Pbigarray_uint8
  | Sint16 -> Pbigarray_sint16
  | Uint16 -> Pbigarray_uint16
  | Int32 -> Pbigarray_int32
  | Int64 -> Pbigarray_int64
  | Int_width_int -> Pbigarray_caml_int
  | Targetint_width_int -> Pbigarray_native_int
  | Complex32 -> Pbigarray_complex32
  | Complex64 -> Pbigarray_complex64

(* CR mshinwell: Document [offset] including tagging *)
let bigarray_load ?(dbg = Debuginfo.none) kind ~bigarray ~offset =
  let elt_kind = lambda_ba_kind kind in
  let elt_size = C.bigarray_elt_size_in_bytes elt_kind in
  let elt_chunk = C.bigarray_word_kind elt_kind in
  C.bigarray_load ~dbg ~elt_kind ~elt_size ~elt_chunk ~bigarray ~offset

let bigarray_store ?(dbg = Debuginfo.none) kind ~bigarray ~offset ~new_value =
  let elt_kind = lambda_ba_kind kind in
  let elt_size = C.bigarray_elt_size_in_bytes elt_kind in
  let elt_chunk = C.bigarray_word_kind elt_kind in
  C.bigarray_store ~dbg ~elt_kind ~elt_size ~elt_chunk ~bigarray ~offset
    ~new_value

(* String and bytes access. For these functions, [index] is a tagged integer. *)

let string_like_load_aux ~dbg width ptr index =
  match (width : Flambda_primitive.string_accessor_width) with
  | Eight ->
    let index = C.untag_int index dbg in
    C.load ~dbg Byte_unsigned Mutable ~addr:(C.add_int ptr index dbg)
  | Sixteen ->
    let index = C.untag_int index dbg in
    C.unaligned_load_16 ptr index dbg
  | Thirty_two ->
    let index = C.untag_int index dbg in
    C.sign_extend_32 dbg (C.unaligned_load_32 ptr index dbg)
  | Sixty_four ->
    if Target_system.is_32_bit
    then C.unsupported_32_bit ()
    else
      let index = C.untag_int index dbg in
      C.unaligned_load_64 ptr index dbg

let string_like_load ?(dbg = Debuginfo.none) kind width block index =
  match (kind : Flambda_primitive.string_like_value) with
  | String | Bytes -> string_like_load_aux ~dbg width block index
  | Bigstring ->
    let ba_data_addr = C.field_address block 1 dbg in
    let ba_data = C.load ~dbg Word_int Mutable ~addr:ba_data_addr in
    C.bind "ba_data" ba_data (fun ptr ->
        string_like_load_aux ~dbg width ptr index)

(* same as {string_like_load_aux} *)
let bytes_like_set_aux ~dbg _kind width _block ptr idx value =
  match (width : Flambda_primitive.string_accessor_width) with
  | Eight ->
    let idx = C.untag_int idx dbg in
    C.store ~dbg Byte_unsigned Assignment ~addr:(C.add_int ptr idx dbg)
      ~new_value:value
  | Sixteen ->
    let idx = C.untag_int idx dbg in
    C.unaligned_set_16 ptr idx value dbg
  | Thirty_two ->
    let idx = C.untag_int idx dbg in
    C.unaligned_set_32 ptr idx value dbg
  | Sixty_four ->
    if Target_system.is_32_bit
    then C.unsupported_32_bit ()
    else
      let idx = C.untag_int idx dbg in
      C.unaligned_set_64 ptr idx value dbg

let bytes_like_set ?(dbg = Debuginfo.none) kind width block index value =
  match (kind : Flambda_primitive.bytes_like_value) with
  | Bytes ->
    C.return_unit dbg
      (bytes_like_set_aux ~dbg kind width block block index value)
  | Bigstring ->
    let ba_data_addr = C.field_address block 1 dbg in
    let ba_data = C.load ~dbg Word_int Mutable ~addr:ba_data_addr in
    C.return_unit dbg
      (C.bind "ba_data" ba_data (fun ptr ->
           bytes_like_set_aux ~dbg kind width block ptr index value))

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

let primitive_boxed_int_of_standard_int x : Primitive.boxed_integer =
  match (x : Flambda_kind.Standard_int.t) with
  | Naked_int32 -> Pint32
  | Naked_int64 -> Pint64
  | Naked_nativeint -> Pnativeint
  | Tagged_immediate | Naked_immediate -> assert false

let unary_int_arith_primitive _env dbg kind op arg =
  match (kind : Flambda_kind.Standard_int.t), (op : P.unary_int_arith_op) with
  | Tagged_immediate, Neg -> C.negint arg dbg
  | Tagged_immediate, Swap_byte_endianness ->
    (* CR mshinwell for gbury: This could maybe cause a fatal error now? *)
    let untagged = C.untag_int arg dbg in
    let swapped = C.bswap16 untagged dbg in
    C.tag_int swapped dbg
  | Naked_immediate, Swap_byte_endianness -> C.bswap16 arg dbg
  (* Special case for manipulating int64 on 32-bit hosts *)
  | Naked_int64, Neg when Target_system.is_32_bit -> C.unsupported_32_bit ()
  (* General case *)
  | _, Neg -> C.sub_int (C.int ~dbg 0) arg dbg
  (* Byte swap of 32-bits ints on 64-bit arch need a sign-extension *)
  | Naked_int32, Swap_byte_endianness when Target_system.is_64_bit ->
    let primitive_kind = primitive_boxed_int_of_standard_int kind in
    C.sign_extend_32 dbg (C.bbswap primitive_kind arg dbg)
  | _, Swap_byte_endianness ->
    let primitive_kind = primitive_boxed_int_of_standard_int kind in
    C.bbswap primitive_kind arg dbg

let unary_float_arith_primitive _env dbg op arg =
  match (op : P.unary_float_arith_op) with
  | Abs -> C.float_abs ~dbg arg
  | Neg -> C.float_neg ~dbg arg

let arithmetic_conversion dbg src dst arg =
  let open Flambda_kind.Standard_int_or_float in
  match src, dst with
  (* 64-bit on 32-bit host specific cases *)
  | Naked_int64, Tagged_immediate
  | Naked_int64, Naked_int32
  | Naked_int64, (Naked_nativeint | Naked_immediate)
  | Naked_int64, Naked_float
  | Tagged_immediate, Naked_int64
  | Naked_int32, Naked_int64
  | (Naked_nativeint | Naked_immediate), Naked_int64
  | Naked_float, Naked_int64
    when Target_system.is_32_bit ->
    C.unsupported_32_bit ()
  (* Identity on floats *)
  | Naked_float, Naked_float -> None, arg
  (* Conversions to and from tagged ints *)
  | ( (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
      Tagged_immediate ) ->
    None, C.tag_int arg dbg
  | Tagged_immediate, (Naked_int64 | Naked_nativeint | Naked_immediate) ->
    Some (Env.Untag arg), C.untag_int arg dbg
  (* Operations resulting in int32s must take care to sign_extend the res *)
  | Tagged_immediate, Naked_int32 ->
    None, C.sign_extend_32 dbg (C.untag_int arg dbg)
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
  (* Int-Float conversions *)
  | Tagged_immediate, Naked_float ->
    None, C.float_of_int ~dbg (C.untag_int arg dbg)
  | (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint), Naked_float
    ->
    None, C.float_of_int ~dbg arg
  | Naked_float, Tagged_immediate ->
    None, C.tag_int (C.int_of_float ~dbg arg) dbg
  | Naked_float, (Naked_immediate | Naked_int64 | Naked_nativeint) ->
    None, C.int_of_float ~dbg arg
  | Naked_float, Naked_int32 ->
    None, C.sign_extend_32 dbg (C.int_of_float ~dbg arg)

let binary_phys_comparison _env dbg kind op x y =
  match (kind : Flambda_kind.t), (op : P.equality_comparison) with
  (* int64 special case *)
  | (Naked_number Naked_int64, Eq | Naked_number Naked_int64, Neq)
    when Target_system.is_32_bit ->
    C.unsupported_32_bit ()
  (* General case *)
  | _, Eq -> C.eq ~dbg x y
  | _, Neq -> C.neq ~dbg x y

let binary_int_arith_primitive _env dbg kind op x y =
  match (kind : Flambda_kind.Standard_int.t), (op : P.binary_int_arith_op) with
  (* Int64 bits ints on 32-bit archs *)
  | Naked_int64, Add
  | Naked_int64, Sub
  | Naked_int64, Mul
  | Naked_int64, Div
  | Naked_int64, Mod
  | Naked_int64, And
  | Naked_int64, Or
  | Naked_int64, Xor
    when Target_system.is_32_bit ->
    C.unsupported_32_bit ()
  (* Tagged integers *)
  | Tagged_immediate, Add -> C.add_int_caml x y dbg
  | Tagged_immediate, Sub -> C.sub_int_caml x y dbg
  | Tagged_immediate, Mul -> C.mul_int_caml x y dbg
  | Tagged_immediate, Div -> C.div_int_caml Unsafe x y dbg
  | Tagged_immediate, Mod -> C.mod_int_caml Unsafe x y dbg
  | Tagged_immediate, And -> C.and_int_caml x y dbg
  | Tagged_immediate, Or -> C.or_int_caml x y dbg
  | Tagged_immediate, Xor -> C.xor_int_caml x y dbg
  (* Operations on 32-bits integers arguments must return something in the range
     of 32-bits integers, hence the sign_extensions here *)
  | Naked_int32, Add ->
    C.sign_extend_32 dbg (C.add_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, Sub ->
    C.sign_extend_32 dbg (C.sub_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, Mul ->
    C.sign_extend_32 dbg (C.mul_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, Xor ->
    C.sign_extend_32 dbg (C.xor_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, And ->
    C.sign_extend_32 dbg (C.and_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, Or ->
    C.sign_extend_32 dbg (C.or_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  (* Naked ints *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Add -> C.add_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Sub -> C.sub_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Mul -> C.mul_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), And -> C.and_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Or -> C.or_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Xor -> C.xor_int x y dbg
  (* Division and modulo need some extra care *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Div ->
    let bi = primitive_boxed_int_of_standard_int kind in
    C.safe_div_bi Unsafe x y bi dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Mod ->
    let bi = primitive_boxed_int_of_standard_int kind in
    C.safe_mod_bi Unsafe x y bi dbg
  | Naked_int32, Div ->
    let bi = primitive_boxed_int_of_standard_int kind in
    C.sign_extend_32 dbg (C.safe_div_bi Unsafe x y bi dbg)
  | Naked_int32, Mod ->
    let bi = primitive_boxed_int_of_standard_int kind in
    C.sign_extend_32 dbg (C.safe_mod_bi Unsafe x y bi dbg)

let binary_int_shift_primitive _env dbg kind op x y =
  match (kind : Flambda_kind.Standard_int.t), (op : P.int_shift_op) with
  (* Int64 special case *)
  | Naked_int64, Lsl when Target_system.is_32_bit ->
    C.unsupported_32_bit ()
    (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Lsr when Target_system.is_32_bit ->
    C.unsupported_32_bit ()
    (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Asr when Target_system.is_32_bit -> C.unsupported_32_bit ()
  (* caml primitives for these have no native/unboxed version *)
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> C.lsl_int_caml_raw ~dbg x y
  | Tagged_immediate, Lsr -> C.lsr_int_caml_raw ~dbg x y
  | Tagged_immediate, Asr -> C.asr_int_caml_raw ~dbg x y
  (* Operations on 32-bits integers need to ensure their result are within the
     32-bit range, hence the sign_extension. *)
  | Naked_int32, Lsl -> C.sign_extend_32 dbg (C.lsl_int (C.low_32 dbg x) y dbg)
  | Naked_int32, Lsr ->
    let arg = if Target_system.is_64_bit then C.zero_extend_32 dbg x else x in
    C.sign_extend_32 dbg (C.lsr_int arg y dbg)
  | Naked_int32, Asr -> C.sign_extend_32 dbg (C.asr_int x y dbg)
  (* Naked ints *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Lsl -> C.lsl_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Lsr -> C.lsr_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Asr -> C.asr_int x y dbg

let binary_int_comp_primitive _env dbg kind signed cmp x y =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (signed : P.signed_or_unsigned),
      (cmp : P.ordered_comparison) )
  with
  (* XXX arch32 cases need [untag_int] now. *)
  | Naked_int64, Signed, Lt
  | Naked_int64, Signed, Le
  | Naked_int64, Signed, Gt
  | Naked_int64, Signed, Ge
  | Naked_int64, Unsigned, (Lt | Le | Gt | Ge)
    when Target_system.is_32_bit ->
    C.unsupported_32_bit ()
  (* There are no runtime C functions to do that afaict *)
  (* Tagged integers *)
  (* When comparing tagged integers, there is always one number for which the
     last bit is irrelevant.

     For x < y, ignoring the last bit of y will not change the result, as if x
     and y are different (as OCaml integers) then the comparison doesn't need to
     see the last bit, and if they are equal then if the last bit of x is one
     (as it is supposed to be) the result will be false for both values of the
     last bit of y, as expected.

     The same reasoning applies to the other comparisons. *)
  | Tagged_immediate, Signed, Lt -> C.lt ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Signed, Le -> C.le ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Signed, Gt -> C.gt ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Signed, Ge -> C.ge ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Unsigned, Lt -> C.ult ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Unsigned, Le -> C.ule ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Unsigned, Gt -> C.ugt ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Unsigned, Ge -> C.uge ~dbg x (C.ignore_low_bit_int y)
  (* Naked integers. *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Signed, Lt
    ->
    C.lt ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Signed, Le
    ->
    C.le ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Signed, Gt
    ->
    C.gt ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Signed, Ge
    ->
    C.ge ~dbg x y
  | ( (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
      Unsigned,
      Lt ) ->
    C.ult ~dbg x y
  | ( (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
      Unsigned,
      Le ) ->
    C.ule ~dbg x y
  | ( (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
      Unsigned,
      Gt ) ->
    C.ugt ~dbg x y
  | ( (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
      Unsigned,
      Ge ) ->
    C.uge ~dbg x y

let binary_int_comp_primitive_yielding_int _env dbg _kind
    (signed : P.signed_or_unsigned) x y =
  match signed with
  | Signed -> C.mk_compare_ints_untagged dbg x y
  | Unsigned ->
    Misc.fatal_error
      "Translation of [Int_comp] yielding an integer -1, 0 or 1 in unsigned \
       mode is not yet implemented"

let binary_float_arith_primitive _env dbg op x y =
  match (op : P.binary_float_arith_op) with
  | Add -> C.float_add ~dbg x y
  | Sub -> C.float_sub ~dbg x y
  | Mul -> C.float_mul ~dbg x y
  | Div -> C.float_div ~dbg x y

let binary_float_comp_primitive _env dbg op x y =
  match (op : P.comparison) with
  | Eq -> C.float_eq ~dbg x y
  | Neq -> C.float_neq ~dbg x y
  | Lt -> C.float_lt ~dbg x y
  | Gt -> C.float_gt ~dbg x y
  | Le -> C.float_le ~dbg x y
  | Ge -> C.float_ge ~dbg x y

let binary_float_comp_primitive_yielding_int _env dbg x y =
  C.mk_compare_floats_untagged dbg x y

(* Primitives *)

let nullary_primitive _env dbg prim : _ * Cmm.expression =
  match (prim : P.nullary_primitive) with
  | Optimised_out _ -> Misc.fatal_errorf "TODO: phantom let-bindings in to_cmm"
  | Probe_is_enabled { name } -> None, Cop (Cprobe_is_enabled { name }, [], dbg)
  | Begin_region -> None, C.beginregion ~dbg

let unary_primitive env res dbg f arg =
  match (f : P.unary_primitive) with
  | Duplicate_array _ ->
    ( None,
      res,
      C.extcall ~dbg ~alloc:true ~returns:true ~is_c_builtin:false ~ty_args:[]
        "caml_obj_dup" Cmm.typ_val [arg] )
  | Duplicate_block _ ->
    ( None,
      res,
      C.extcall ~dbg ~alloc:true ~returns:true ~is_c_builtin:false ~ty_args:[]
        "caml_obj_dup" Cmm.typ_val [arg] )
  | Is_int -> None, res, C.and_int arg (C.int ~dbg 1) dbg
  | Get_tag -> None, res, C.get_tag arg dbg
  | Array_length -> None, res, array_length ~dbg arg
  | Bigarray_length { dimension } ->
    ( None,
      res,
      C.load ~dbg Word_int Mutable
        ~addr:(C.field_address arg (4 + dimension) dbg) )
  | String_length _ -> None, res, C.string_length arg dbg
  | Int_as_pointer -> None, res, C.int_as_pointer arg dbg
  | Opaque_identity -> None, res, C.opaque arg dbg
  | Int_arith (kind, op) ->
    None, res, unary_int_arith_primitive env dbg kind op arg
  | Float_arith op -> None, res, unary_float_arith_primitive env dbg op arg
  | Num_conv { src; dst } ->
    let extra, expr = arithmetic_conversion dbg src dst arg in
    extra, res, expr
  | Boolean_not -> None, res, C.mk_not dbg arg
  | Reinterpret_int64_as_float ->
    (* CR-someday mshinwell: We should add support for this operation in the
       backend. It isn't the identity as there may need to be a move between
       different register kinds (e.g. integer to XMM registers on x86-64). *)
    ( None,
      res,
      C.extcall ~dbg ~alloc:false ~returns:true ~is_c_builtin:false
        ~ty_args:[C.exttype_of_kind Flambda_kind.naked_int64]
        "caml_int64_float_of_bits_unboxed" Cmm.typ_float [arg] )
  | Unbox_number kind -> None, res, unbox_number ~dbg kind arg
  | Untag_immediate -> Some (Env.Untag arg), res, C.untag_int arg dbg
  | Box_number (kind, alloc_mode) ->
    Some Env.Boxed_number, res, box_number ~dbg kind alloc_mode arg
  | Tag_immediate ->
    (* We could have an [Env.Tag] which would be returned here, but probably
       unnecessary at the moment. *)
    None, res, C.tag_int arg dbg
  | Project_function_slot { move_from = c1; move_to = c2 } -> begin
    match function_slot_offset env c1, function_slot_offset env c2 with
    | ( Live_function_slot { offset = c1_offset; _ },
        Live_function_slot { offset = c2_offset; _ } ) ->
      let diff = c2_offset - c1_offset in
      None, res, C.infix_field_address ~dbg arg diff
    (* one of the ids has been marked as dead, the code should be
       unreachable. *)
    | Dead_function_slot, Live_function_slot _ ->
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
      None, res, expr
  end
  | Project_value_slot { project_from; value_slot } -> begin
    match
      value_slot_offset env value_slot, function_slot_offset env project_from
    with
    (* Normal case: we have offsets for everything *)
    | Live_value_slot { offset }, Live_function_slot { offset = base; _ } ->
      None, res, C.get_field_gen Asttypes.Immutable arg (offset - base) dbg
    (* the value slot and/or function slot has been explicitly removed, the code
       is unreachable *)
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
      None, res, expr
  end
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
  | End_region -> None, res, C.return_unit dbg (C.endregion ~dbg arg)

let binary_primitive env dbg f x y =
  match (f : P.binary_primitive) with
  | Block_load (kind, mut) -> block_load ~dbg kind mut x y
  | Array_load (kind, _mut) -> array_load ~dbg kind x y
  | String_or_bigstring_load (kind, width) ->
    string_like_load ~dbg kind width x y
  | Bigarray_load (_dimensions, kind, _layout) ->
    bigarray_load ~dbg kind ~bigarray:x ~offset:y
  | Phys_equal (kind, op) -> binary_phys_comparison env dbg kind op x y
  | Int_arith (kind, op) -> binary_int_arith_primitive env dbg kind op x y
  | Int_shift (kind, op) -> binary_int_shift_primitive env dbg kind op x y
  | Int_comp (kind, signed, Yielding_bool cmp) ->
    binary_int_comp_primitive env dbg kind signed cmp x y
  | Int_comp (kind, signed, Yielding_int_like_compare_functions) ->
    binary_int_comp_primitive_yielding_int env dbg kind signed x y
  | Float_arith op -> binary_float_arith_primitive env dbg op x y
  | Float_comp (Yielding_bool cmp) ->
    binary_float_comp_primitive env dbg cmp x y
  | Float_comp Yielding_int_like_compare_functions ->
    binary_float_comp_primitive_yielding_int env dbg x y

let ternary_primitive _env dbg f x y z =
  match (f : P.ternary_primitive) with
  | Block_set (block_access, init) -> block_set ~dbg block_access init x y z
  | Array_set (array_kind, init) -> array_set ~dbg array_kind init x y z
  | Bytes_or_bigstring_set (kind, width) -> bytes_like_set ~dbg kind width x y z
  | Bigarray_set (_dimensions, kind, _layout) ->
    bigarray_store ~dbg kind ~bigarray:x ~offset:y ~new_value:z

let variadic_primitive _env dbg f args =
  match (f : P.variadic_primitive) with
  | Make_block (kind, _mut, alloc_mode) -> make_block ~dbg kind alloc_mode args
  | Make_array (kind, _mut, alloc_mode) -> make_array ~dbg kind alloc_mode args

(* CR Gbury: check the order in which the primitive arguments are given to
   [Env.inline_variable]. *)
let prim env res dbg p =
  match (p : P.t) with
  | Nullary prim ->
    let extra, expr = nullary_primitive env dbg prim in
    expr, extra, env, res, Ece.pure
  | Unary (f, x) ->
    let x, env, eff = C.simple ~dbg env res x in
    let extra, res, expr = unary_primitive env res dbg f x in
    expr, extra, env, res, eff
  | Binary (f, x, y) ->
    let x, env, effx = C.simple ~dbg env res x in
    let y, env, effy = C.simple ~dbg env res y in
    let effs = Ece.join effx effy in
    let expr = binary_primitive env dbg f x y in
    expr, None, env, res, effs
  | Ternary (f, x, y, z) ->
    let x, env, effx = C.simple ~dbg env res x in
    let y, env, effy = C.simple ~dbg env res y in
    let z, env, effz = C.simple ~dbg env res z in
    let effs = Ece.join (Ece.join effx effy) effz in
    let expr = ternary_primitive env dbg f x y z in
    expr, None, env, res, effs
  | Variadic (f, l) ->
    let args, env, effs = C.simple_list ~dbg env res l in
    let expr = variadic_primitive env dbg f args in
    expr, None, env, res, effs
