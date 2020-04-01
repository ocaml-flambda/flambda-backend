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

open! Flambda.Import
module Env = To_cmm_env
module Ece = Effects_and_coeffects
module R = To_cmm_result

(* Notes:

   - an int64 on a 32-bit host is represented across two registers, hence most
   operations on them will actually need to call C primitive that can handle
   them.

   - int32 on 64 bits are represented as an int64 in the range of 32-bit
   integers. Thus we insert sign extensions after every operation on 32-bits
   integers that may have a result outside of the range. *)

(* Cmm helpers *)
module C = struct
  include Cmm_helpers
  include To_cmm_helper
end

(* Shortcuts for useful cmm machtypes *)
let typ_int = Cmm.typ_int

let typ_val = Cmm.typ_val

let typ_void = Cmm.typ_void

let typ_float = Cmm.typ_float

let typ_int64 = C.typ_int64

(* CR gbury: {Targetint_32_64.to_int} should raise an error when converting an
   out-of-range integer. *)
let int_of_targetint t =
  let i = Targetint_32_64.to_int t in
  let t' = Targetint_32_64.of_int i in
  if not (Targetint_32_64.equal t t')
  then Misc.fatal_errorf "Cannot translate targetint to caml int";
  i

(* Name expressions *)

let symbol s = Linkage_name.to_string (Symbol.linkage_name s)

let name env name =
  Name.pattern_match name
    ~var:(fun v -> Env.inline_variable env v)
    ~symbol:(fun s ->
      let env =
        Env.check_scope ~allow_deleted:false env (Code_id_or_symbol.Symbol s)
      in
      C.symbol (symbol s), env, Ece.pure)

(* Constants *)

let tag_targetint t = Targetint_32_64.(add (shift_left t 1) one)

let targetint_of_imm i =
  Targetint_31_63.Imm.to_targetint i.Targetint_31_63.value

let const _env cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> C.targetint (targetint_of_imm i)
  | Tagged_immediate i -> C.targetint (tag_targetint (targetint_of_imm i))
  | Naked_float f -> C.float (Numeric_types.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> C.int32 i
  | Naked_int64 i -> C.int64 i
  | Naked_nativeint t -> C.targetint t

let default_of_kind (k : Flambda_kind.t) =
  match k with
  | Value -> C.int 1
  | Naked_number Naked_immediate -> C.int 0
  | Naked_number Naked_float -> C.float 0.
  | Naked_number Naked_int32 -> C.int 0
  | Naked_number Naked_int64 when C.arch32 -> C.unsupported_32_bits ()
  | Naked_number Naked_int64 -> C.int 0
  | Naked_number Naked_nativeint -> C.int 0
  | Fabricated -> Misc.fatal_error "Fabricated_kind have no default value"
  | Rec_info -> Misc.fatal_error "Rec_info has no default value"

(* Function symbol *)

let function_name simple =
  let fail simple =
    Misc.fatal_errorf "Expected a function symbol, instead of@ %a" Simple.print
      simple
  in
  Simple.pattern_match simple
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun _ ~coercion:_ -> fail simple)
        ~symbol:(fun sym ~coercion:_ -> symbol sym))
    ~const:(fun _ -> fail simple)

(* 'Simple' expression *)

let simple env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name env n)
    ~const:(fun c -> const env c, env, Ece.pure)

(* Arithmetic primitives *)

let primitive_boxed_int_of_standard_int x =
  match (x : Flambda_kind.Standard_int.t) with
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint
  | Tagged_immediate -> assert false
  | Naked_immediate -> assert false

let unary_int_arith_primitive _env dbg kind op arg =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (op : Flambda_primitive.unary_int_arith_op) )
  with
  | Tagged_immediate, Neg -> C.negint arg dbg
  | Tagged_immediate, Swap_byte_endianness ->
    (* CR mshinwell for gbury: This could maybe cause a fatal error now? *)
    let untagged = C.untag_int arg dbg in
    let swapped = C.bswap16 untagged dbg in
    C.tag_int swapped dbg
  | Naked_immediate, Swap_byte_endianness -> C.bswap16 arg dbg
  (* Special case for manipulating int64 on 32-bit hosts *)
  | Naked_int64, Neg when C.arch32 -> C.unsupported_32_bits ()
  (* General case (including byte swap for 64-bit on 32-bit archi) *)
  | _, Neg -> C.sub_int (C.int 0) arg dbg
  | _, Swap_byte_endianness ->
    let primitive_kind = primitive_boxed_int_of_standard_int kind in
    C.bbswap primitive_kind arg dbg

let unary_float_arith_primitive _env dbg op arg =
  match (op : Flambda_primitive.unary_float_arith_op) with
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
    when C.arch32 ->
    C.unsupported_32_bits ()
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
  match
    (kind : Flambda_kind.t), (op : Flambda_primitive.equality_comparison)
  with
  (* int64 special case *)
  | (Naked_number Naked_int64, Eq | Naked_number Naked_int64, Neq) when C.arch32
    ->
    C.unsupported_32_bits ()
  (* General case *)
  | _, Eq -> C.eq ~dbg x y
  | _, Neq -> C.neq ~dbg x y

let binary_int_arith_primitive _env dbg kind op x y =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (op : Flambda_primitive.binary_int_arith_op) )
  with
  (* Int64 bits ints on 32-bit archs *)
  | Naked_int64, Add
  | Naked_int64, Sub
  | Naked_int64, Mul
  | Naked_int64, Div
  | Naked_int64, Mod
  | Naked_int64, And
  | Naked_int64, Or
  | Naked_int64, Xor
    when C.arch32 ->
    C.unsupported_32_bits ()
  (* Tagged integers *)
  | Tagged_immediate, Add -> C.add_int_caml x y dbg
  | Tagged_immediate, Sub -> C.sub_int_caml x y dbg
  | Tagged_immediate, Mul -> C.mul_int_caml x y dbg
  | Tagged_immediate, Div -> C.div_int_caml Lambda.Unsafe x y dbg
  | Tagged_immediate, Mod -> C.mod_int_caml Lambda.Unsafe x y dbg
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
    C.sign_extend_32 dbg (C.xor_ ~dbg (C.low_32 dbg x) (C.low_32 dbg y))
  | Naked_int32, And ->
    C.sign_extend_32 dbg (C.and_ ~dbg (C.low_32 dbg x) (C.low_32 dbg y))
  | Naked_int32, Or ->
    C.sign_extend_32 dbg (C.or_ ~dbg (C.low_32 dbg x) (C.low_32 dbg y))
  (* Naked ints *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Add -> C.add_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Sub -> C.sub_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Mul -> C.mul_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), And -> C.and_ ~dbg x y
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Or -> C.or_ ~dbg x y
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Xor -> C.xor_ ~dbg x y
  (* Division and modulo need some extra care *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Div ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.safe_div_bi Lambda.Unsafe x y bi dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Mod ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.safe_mod_bi Lambda.Unsafe x y bi dbg
  | Naked_int32, Div ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.sign_extend_32 dbg (C.safe_div_bi Lambda.Unsafe x y bi dbg)
  | Naked_int32, Mod ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.sign_extend_32 dbg (C.safe_mod_bi Lambda.Unsafe x y bi dbg)

let binary_int_shift_primitive _env dbg kind op x y =
  match
    (kind : Flambda_kind.Standard_int.t), (op : Flambda_primitive.int_shift_op)
  with
  (* Int64 special case *)
  | Naked_int64, Lsl when C.arch32 ->
    C.unsupported_32_bits ()
    (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Lsr when C.arch32 ->
    C.unsupported_32_bits ()
    (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Asr when C.arch32 -> C.unsupported_32_bits ()
  (* caml primitives for these have no native/unboxed version *)
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> C.lsl_int_caml_raw ~dbg x y
  | Tagged_immediate, Lsr -> C.lsr_int_caml_raw ~dbg x y
  | Tagged_immediate, Asr -> C.asr_int_caml_raw ~dbg x y
  (* Operations on 32-bits integers need to ensure their result are within the
     32-bit range, hence the sign_extension. *)
  | Naked_int32, Lsl -> C.sign_extend_32 dbg (C.lsl_int (C.low_32 dbg x) y dbg)
  | Naked_int32, Lsr ->
    let arg = if C.arch64 then C.zero_extend_32 dbg x else x in
    C.sign_extend_32 dbg (C.lsr_int arg y dbg)
  | Naked_int32, Asr -> C.sign_extend_32 dbg (C.asr_int x y dbg)
  (* Naked ints *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Lsl -> C.lsl_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Lsr -> C.lsr_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Asr -> C.asr_int x y dbg

let binary_int_comp_primitive _env dbg kind signed cmp x y =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (signed : Flambda_primitive.signed_or_unsigned),
      (cmp : Flambda_primitive.ordered_comparison) )
  with
  (* XXX arch32 cases need [untag_int] now. *)
  | Naked_int64, Signed, Lt
  | Naked_int64, Signed, Le
  | Naked_int64, Signed, Gt
  | Naked_int64, Signed, Ge
  | Naked_int64, Unsigned, (Lt | Le | Gt | Ge)
    when C.arch32 ->
    C.unsupported_32_bits ()
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
    (signed : Flambda_primitive.signed_or_unsigned) x y =
  match signed with
  | Signed -> C.mk_compare_ints_untagged dbg x y
  | Unsigned ->
    Misc.fatal_error
      "Translation of [Int_comp] yielding an integer -1, 0 or 1 in unsigned \
       mode is not yet implemented"

let binary_float_arith_primitive _env dbg op x y =
  match (op : Flambda_primitive.binary_float_arith_op) with
  | Add -> C.float_add ~dbg x y
  | Sub -> C.float_sub ~dbg x y
  | Mul -> C.float_mul ~dbg x y
  | Div -> C.float_div ~dbg x y

let binary_float_comp_primitive _env dbg op x y =
  match (op : Flambda_primitive.comparison) with
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
  match (prim : Flambda_primitive.nullary_primitive) with
  | Optimised_out _ -> Misc.fatal_errorf "TODO: phantom let-bindings in to_cmm"
  | Probe_is_enabled { name } -> None, Cop (Cprobe_is_enabled { name }, [], dbg)

let unary_primitive env dbg f arg =
  match (f : Flambda_primitive.unary_primitive) with
  | Duplicate_array _ ->
    ( None,
      C.extcall ~alloc:true ~returns:true ~is_c_builtin:false ~ty_args:[]
        "caml_obj_dup" typ_val [arg] )
  | Duplicate_block _ ->
    ( None,
      C.extcall ~alloc:true ~returns:true ~is_c_builtin:false ~ty_args:[]
        "caml_obj_dup" typ_val [arg] )
  | Is_int -> None, C.and_ ~dbg arg (C.int ~dbg 1)
  | Get_tag -> None, C.get_tag arg dbg
  | Array_length array_kind -> None, C.array_length ~dbg array_kind arg
  | Bigarray_length { dimension } ->
    ( None,
      C.load ~dbg Cmm.Word_int Asttypes.Mutable
        (C.field_address arg (4 + dimension) dbg) )
  | String_length _ -> None, C.string_length arg dbg
  | Int_as_pointer -> None, C.int_as_pointer arg dbg
  | Opaque_identity -> None, C.opaque arg dbg
  | Int_arith (kind, op) -> None, unary_int_arith_primitive env dbg kind op arg
  | Float_arith op -> None, unary_float_arith_primitive env dbg op arg
  | Num_conv { src; dst } -> arithmetic_conversion dbg src dst arg
  | Boolean_not -> None, C.mk_not dbg arg
  | Reinterpret_int64_as_float ->
    (* CR-someday mshinwell: We should add support for this operation in the
       backend. It isn't the identity as there may need to be a move between
       different register kinds (e.g. integer to XMM registers on x86-64). *)
    ( None,
      C.extcall ~alloc:false ~returns:true ~is_c_builtin:false
        ~ty_args:[C.exttype_of_kind Flambda_kind.naked_int64]
        "caml_int64_float_of_bits_unboxed" typ_float [arg] )
  | Unbox_number kind ->
    let extra =
      match kind with Untagged_immediate -> Some (Env.Untag arg) | _ -> None
    in
    extra, C.unbox_number ~dbg kind arg
  | Box_number kind -> None, C.box_number ~dbg kind arg
  | Select_closure { move_from = c1; move_to = c2 } -> begin
    match Env.closure_offset env c1, Env.closure_offset env c2 with
    | Some { offset = c1_offset; _ }, Some { offset = c2_offset; _ } ->
      let diff = c2_offset - c1_offset in
      None, C.infix_field_address ~dbg arg diff
    | Some _, None | None, Some _ | None, None -> None, C.unreachable
  end
  | Project_var { project_from; var } -> (
    match Env.env_var_offset env var, Env.closure_offset env project_from with
    | Some { offset }, Some { offset = base; _ } ->
      None, C.get_field_gen Asttypes.Immutable arg (offset - base) dbg
    | Some _, None | None, Some _ | None, None ->
      (* Note that if a closure var is missing from a set of closures
         environment, then [Env.closure_offset] might return [None], even though
         the set of closures has been seen by [To_cmm_closure]. *)
      None, C.unreachable)

let binary_primitive env dbg f x y =
  match (f : Flambda_primitive.binary_primitive) with
  | Block_load (kind, mut) -> C.block_load ~dbg kind mut x y
  | Array_load (kind, _mut) -> C.array_load ~dbg kind x y
  | String_or_bigstring_load (kind, width) ->
    C.string_like_load ~dbg kind width x y
  | Bigarray_load (dimensions, kind, layout) ->
    C.bigarray_load ~dbg dimensions kind layout x y
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
  match (f : Flambda_primitive.ternary_primitive) with
  | Block_set (block_access, init) -> C.block_set ~dbg block_access init x y z
  | Array_set (array_kind, init) -> C.array_set ~dbg array_kind init x y z
  | Bytes_or_bigstring_set (kind, width) ->
    C.bytes_like_set ~dbg kind width x y z
  | Bigarray_set (dimensions, kind, layout) ->
    C.bigarray_store ~dbg dimensions kind layout x y z

let variadic_primitive _env dbg f args =
  match (f : Flambda_primitive.variadic_primitive) with
  | Make_block (kind, _mut) -> C.make_block ~dbg kind args
  | Make_array (kind, _mut) -> C.make_array ~dbg kind args

let arg_list env l =
  let aux (list, env, effs) x =
    let y, env, eff = simple env x in
    y :: list, env, Ece.join eff effs
  in
  let args, env, effs = List.fold_left aux ([], env, Ece.pure) l in
  List.rev args, env, effs

(* CR Gbury: check the order in which the primitive arguments are given to
   [Env.inline_variable]. *)
let prim env dbg p =
  match (p : Flambda_primitive.t) with
  | Nullary prim ->
    let extra, res = nullary_primitive env dbg prim in
    res, extra, env, Ece.pure
  | Unary (f, x) ->
    let x, env, eff = simple env x in
    let extra, res = unary_primitive env dbg f x in
    res, extra, env, eff
  | Binary (f, x, y) ->
    let x, env, effx = simple env x in
    let y, env, effy = simple env y in
    let effs = Ece.join effx effy in
    let res = binary_primitive env dbg f x y in
    res, None, env, effs
  | Ternary (f, x, y, z) ->
    let x, env, effx = simple env x in
    let y, env, effy = simple env y in
    let z, env, effz = simple env z in
    let effs = Ece.join (Ece.join effx effy) effz in
    let res = ternary_primitive env dbg f x y z in
    res, None, env, effs
  | Variadic (f, l) ->
    let args, env, effs = arg_list env l in
    let res = variadic_primitive env dbg f args in
    res, None, env, effs

(* Kinds and types *)

let check_arity arity args = List.compare_lengths arity args = 0

let machtype_of_kind k =
  match (k : Flambda_kind.t) with
  | Value -> typ_val
  | Naked_number Naked_float -> typ_float
  | Naked_number Naked_int64 -> typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) -> typ_int
  | Fabricated | Rec_info -> assert false

let machtype_of_kinded_parameter p =
  Kinded_parameter.kind p |> Flambda_kind.With_subkind.kind |> machtype_of_kind

let machtype_of_return_arity = function
  (* Functions that never return have arity 0. In that case, we use the most
     restrictive machtype to ensure that the return value of the function is not
     used. *)
  | [] -> typ_void
  (* Regular functions with a single return value *)
  | [k] -> machtype_of_kind k
  | _ ->
    (* TODO: update when unboxed tuples are used *)
    Misc.fatal_errorf "Functions are currently limited to a single return value"

let meth_kind k =
  match (k : Call_kind.method_kind) with
  | Self -> (Self : Lambda.meth_kind)
  | Public -> (Public : Lambda.meth_kind)
  | Cached -> (Cached : Lambda.meth_kind)

let apply_returns (e : Apply_expr.t) =
  match Apply_expr.continuation e with
  | Return _ -> true
  | Never_returns -> false

let wrap_extcall_result (l : Flambda_kind.t list) =
  match l with
  (* Int32 need to be sign_extended because it's not clear whether C code that
     returns an int32 returns one that is sign extended or not *)
  | [Naked_number Naked_int32] -> C.sign_extend_32
  (* No need to wrap other return arities.

     Note that extcall of arity 0 are allowed (these are extcalls that never
     return, such as caml_ml_array_bound_error) *)
  | [] | [_] -> fun _dbg cmm -> cmm
  | _ ->
    (* TODO: update when unboxed tuples are used *)
    Misc.fatal_errorf
      "C functions are currently limited to a single return value"

(* Function calls and continuations *)

let var_list env l =
  let flambda_vars = List.map Kinded_parameter.var l in
  let env, cmm_vars = Env.create_variables env flambda_vars in
  let vars =
    List.map2 (fun v v' -> v, machtype_of_kinded_parameter v') cmm_vars l
  in
  env, vars

let split_exn_cont_args k = function
  | (v, _) :: rest -> v, rest
  | [] ->
    Misc.fatal_errorf
      "Exception continuation %a should have at least one argument"
      Continuation.print k

(* effects and co-effects *)

let cont_is_known_to_have_exactly_one_occurrence k (num : _ Or_unknown.t) =
  match num with
  | Unknown -> false
  | Known num -> (
    match (num : Num_occurrences.t) with
    | One -> true
    | More_than_one -> false
    | Zero ->
      Misc.fatal_errorf
        "Found unused let-bound continuation %a, this should not happen"
        Continuation.print k)

type inlining_decision =
  | Skip (* no use, the bound variable can be skipped/ignored *)
  | Inline (* the variable is used once, we can try and inline its use *)
  | Regular
(* the variable is used multiple times, do not try and inline it. *)

let decide_inline_let effs
    ~(num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t)
    var =
  match Variable.Map.find var num_normal_occurrences_of_bound_vars with
  | exception Not_found -> Regular
  | Zero -> begin
    match Env.classify effs with
    | Coeffect | Pure -> Skip
    | Effect ->
      Regular
      (* Could be Inline technically, but it doesn't matter since it can only be
         flushed by the env. *)
  end
  | One -> begin
    match Env.classify effs with
    | Effect when not (Flambda_features.Expert.inline_effects_in_cmm ()) ->
      Regular
    | _ -> Inline
  end
  | More_than_one -> Regular

(* Helpers for translating functions *)

let function_args vars my_closure ~(is_my_closure_used : _ Or_unknown.t) =
  let is_my_closure_used =
    match is_my_closure_used with
    | Unknown -> true
    | Known is_my_closure_used -> is_my_closure_used
  in
  if is_my_closure_used
  then
    let last_arg =
      Kinded_parameter.create my_closure Flambda_kind.With_subkind.any_value
    in
    vars @ [last_arg]
  else vars

let function_flags () =
  if Flambda_features.optimize_for_speed () then [] else [Cmm.Reduce_code_size]

(* Expressions *)

let rec expr env res e =
  match (Expr.descr e : Expr.descr) with
  | Let e' -> let_expr env res e'
  | Let_cont e' -> let_cont env res e'
  | Apply e' -> apply_expr env res e'
  | Apply_cont e' -> apply_cont env res e'
  | Switch e' -> switch env res e'
  | Invalid e' -> invalid env res e'

and let_expr env res t =
  Let.pattern_match' t
    ~f:(fun bindable_let_bound ~num_normal_occurrences_of_bound_vars ~body ->
      let mode = Bindable_let_bound.name_mode bindable_let_bound in
      match Name_mode.descr mode with
      | In_types ->
        Misc.fatal_errorf
          "Binding in terms a variable of mode In_types is forbidden"
      | Phantom -> expr env res body
      | Normal -> (
        let e = Let.defining_expr t in
        match bindable_let_bound, e with
        (* Correct cases *)
        | Singleton v, Simple s ->
          let_expr_simple body env res v ~num_normal_occurrences_of_bound_vars s
        | Singleton v, Prim (p, dbg) ->
          let_expr_prim body env res v ~num_normal_occurrences_of_bound_vars p
            dbg
        | Set_of_closures { closure_vars; _ }, Set_of_closures soc ->
          let_set_of_closures env res body closure_vars
            ~num_normal_occurrences_of_bound_vars soc
        | Symbols { bound_symbols }, Static_consts consts ->
          let_symbol env res bound_symbols consts body
        | Singleton _, Rec_info _ ->
          (* Erase *)
          expr env res body
        (* Error cases *)
        | Singleton _, (Set_of_closures _ | Static_consts _) ->
          Misc.fatal_errorf
            "Singleton binding neither a simple expression nor a primitive \
             application:@ %a"
            Let.print t
        | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
          ->
          Misc.fatal_errorf "Set_of_closures binding a non-Set_of_closures:@ %a"
            Let.print t
        | Symbols _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
          Misc.fatal_errorf "Symbols binding a non-Static const:@ %a" Let.print
            t))

and let_symbol env res bound_symbols consts body =
  let env =
    (* All bound symbols are allowed to appear in each other's definition, so
       they're added to the environment first *)
    (* CR mshinwell: This isn't quite right now, but can be fixed later *)
    Env.add_to_scope env (Bound_symbols.everything_being_defined bound_symbols)
  in
  let env, res, update_opt =
    To_cmm_static.static_consts env res ~params_and_body bound_symbols consts
  in
  match update_opt with
  | None ->
    expr env res body (* trying to preserve tail calls whenever we can *)
  | Some update ->
    let wrap, env = Env.flush_delayed_lets env in
    let body, res = expr env res body in
    wrap (C.sequence update body), res

and let_set_of_closures env res body closure_vars
    ~num_normal_occurrences_of_bound_vars s =
  let fun_decls = Set_of_closures.function_decls s in
  let decls = Function_declarations.funs_in_order fun_decls in
  let elts =
    To_cmm_closure.filter_closure_vars s
      ~used_closure_vars:(Env.used_closure_vars env)
  in
  if Var_within_closure.Map.is_empty elts
  then let_static_set_of_closures env res body closure_vars s
  else
    let_dynamic_set_of_closures env res body closure_vars
      ~num_normal_occurrences_of_bound_vars decls elts

and let_expr_bind ?extra env v ~num_normal_occurrences_of_bound_vars cmm_expr
    effs =
  match decide_inline_let effs ~num_normal_occurrences_of_bound_vars v with
  | Skip -> env
  | Inline -> Env.bind_variable env v ?extra effs true cmm_expr
  | Regular -> Env.bind_variable env v ?extra effs false cmm_expr

and bind_simple (env, res) v ~num_normal_occurrences_of_bound_vars s =
  let cmm_expr, env, effs = simple env s in
  let_expr_bind env v ~num_normal_occurrences_of_bound_vars cmm_expr effs, res

and let_expr_simple body env res v ~num_normal_occurrences_of_bound_vars s =
  let v = Var_in_binding_pos.var v in
  let env, res =
    bind_simple (env, res) v ~num_normal_occurrences_of_bound_vars s
  in
  expr env res body

and let_expr_prim body env res v ~num_normal_occurrences_of_bound_vars p dbg =
  let v = Var_in_binding_pos.var v in
  let cmm_expr, extra, env, effs = prim env dbg p in
  let effs = Ece.join effs (Flambda_primitive.effects_and_coeffects p) in
  let env =
    let_expr_bind ?extra env v ~num_normal_occurrences_of_bound_vars cmm_expr
      effs
  in
  expr env res body

and decide_inline_cont h k ~num_free_occurrences ~is_applied_with_traps =
  (not (Continuation_handler.is_exn_handler h))
  && (not is_applied_with_traps)
  && cont_is_known_to_have_exactly_one_occurrence k num_free_occurrences

and let_cont env res = function
  | Let_cont.Non_recursive
      { handler; num_free_occurrences; is_applied_with_traps } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
        let h = Non_recursive_let_cont_handler.handler handler in
        if decide_inline_cont h k ~num_free_occurrences ~is_applied_with_traps
        then let_cont_inline env res k h body
        else let_cont_jump env res k h body)
  | Let_cont.Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
        assert (not (Continuation_handlers.contains_exn_handler conts));
        let_cont_rec env res conts body)

(* The bound continuation [k] will be inlined. *)
and let_cont_inline env res k h body =
  let params, handler_params_occurrences, handler =
    continuation_handler_split h
  in
  let env =
    Env.add_inline_cont env k params ~handler_params_occurrences handler
  in
  expr env res body

(* Continuations that are not inlined are translated using a jump:

   - exceptions continuations use "dynamic" jumps using the raise/trywith cmm
   mechanism

   - regular continuations use static jumps, through the exit/catch cmm
   mechanism *)
(* CR Gbury: "split" the environment according to which variables the handler
   and the body uses, to allow for inlining to proceed within each
   expression. *)
and let_cont_jump env res k h body =
  let wrap, env = Env.flush_delayed_lets env in
  let vars, arity, handle, res = continuation_handler env res h in
  let id, env = Env.add_jump_cont env (List.map snd vars) k in
  if Continuation_handler.is_exn_handler h
  then
    let body, res = let_cont_exn env res k body vars handle id arity in
    wrap body, res
  else
    let body, res = expr env res body in
    ( wrap (C.ccatch ~rec_flag:false ~body ~handlers:[C.handler id vars handle]),
      res )

(* Exception continuations, translated using delayed trywith blocks.

   Additionally, exn continuations in flambda can have extra args, which are
   passed through the trywith using mutable cmm variables. Thus the exn handler
   must first read the contents of thos extra args (eagerly in order to minmize
   the lifetime of the mutable variables) *)
and let_cont_exn env res k body vars handle id arity =
  let exn_var, extra_params = split_exn_cont_args k vars in
  let env_body, extra_vars = Env.add_exn_handler env k arity in
  let handler = exn_handler handle extra_vars extra_params in
  let body, res = expr env_body res body in
  let trywith =
    C.trywith ~dbg:Debuginfo.none ~kind:(Delayed id) ~body ~exn_var ~handler ()
  in
  wrap_let_cont_exn_body trywith extra_vars, res

(* wrap a exn handler with read of the mutable variables *)
and exn_handler handle extra_vars extra_params =
  List.fold_left2
    (fun handler (v, _) (p, _) -> C.letin p (C.var v) handler)
    handle extra_vars extra_params

(* define and initialize the mutable cmm variables used by an exn extra args *)
and wrap_let_cont_exn_body handler extra_vars =
  List.fold_left
    (fun expr (v, k) ->
      let v = Backend_var.With_provenance.create v in
      C.letin_mut v (machtype_of_kind k) (default_of_kind k) expr)
    handler extra_vars

and let_cont_rec env res conts body =
  (* Flush the env before anything to avoid inlining something inside of a
     recursive cont (aka a loop), as it would increase the number of times the
     computation is performed (even if there is only one syntactic
     occurrence) *)
  let wrap, env = Env.flush_delayed_lets env in
  (* Compute the environment for jump ids *)
  let map = Continuation_handlers.to_map conts in
  let env =
    Continuation.Map.fold
      (fun k h acc -> snd (Env.add_jump_cont acc (continuation_arg_tys h) k))
      map env
  in
  (* Translate each continuation handler *)
  let map, res =
    Continuation.Map.fold
      (fun k h (map, res) ->
        let vars, _arity, handler, res = continuation_handler env res h in
        Continuation.Map.add k (vars, handler) map, res)
      map
      (Continuation.Map.empty, res)
  in
  (* Setup the cmm handlers for the static catch *)
  let handlers =
    Continuation.Map.fold
      (fun k (vars, handle) acc ->
        let id = Env.get_jump_id env k in
        C.handler id vars handle :: acc)
      map []
  in
  let body, res = expr env res body in
  wrap (C.ccatch ~rec_flag:true ~body ~handlers), res

and continuation_handler_split h =
  Continuation_handler.pattern_match' h
    ~f:(fun params ~num_normal_occurrences_of_params ~handler ->
      params, num_normal_occurrences_of_params, handler)

and continuation_arg_tys h =
  let args, _, _ = continuation_handler_split h in
  List.map machtype_of_kinded_parameter args

and continuation_handler env res h =
  let args, _, handler = continuation_handler_split h in
  let arity = Kinded_parameter.List.arity args in
  let env, vars = var_list env args in
  let e, res = expr env res handler in
  vars, arity, e, res

(* Function calls: besides the function calls, there are a few things to do:

   - setup the mutable variables for the exn cont extra args if needed

   - translate the call continuation (either through a jump, or inlining). *)
and apply_expr env res e =
  let call, env, effs = apply_call env e in
  let k_exn = Apply_expr.exn_continuation e in
  let call, env = wrap_call_exn env e call k_exn in
  wrap_cont env res effs call e

(* Bare function calls *)
and apply_call env e =
  let f = Apply_expr.callee e in
  let args = Apply_expr.args e in
  let dbg = Apply_expr.dbg e in
  let effs = Ece.all in
  let fail_if_probe apply =
    match Apply.probe_name apply with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf
        "[Apply] terms with a [probe_name] (i.e. that call a tracing probe) \
         must always be direct applications of an OCaml function:@ %a"
        Apply.print apply
  in
  match Apply_expr.call_kind e with
  (* Effects from arguments are ignored since a function call will always be
     given arbitrary effects and coeffects. *)
  | Call_kind.Function
      (Call_kind.Function_call.Direct { code_id; closure_id = _; return_arity })
    -> (
    let env =
      Env.check_scope ~allow_deleted:false env
        (Code_id_or_symbol.Code_id code_id)
    in
    let info = Env.get_function_info env code_id in
    let params_arity = Exported_code.Calling_convention.params_arity info in
    if not (check_arity params_arity args)
    then Misc.fatal_errorf "Wrong arity for direct call";
    let ty =
      return_arity |> Flambda_arity.With_subkinds.to_arity
      |> machtype_of_return_arity
    in
    let args, env, _ = arg_list env args in
    let args, env =
      if Exported_code.Calling_convention.needs_closure_arg info
      then
        let f, env, _ = simple env f in
        args @ [f], env
      else args, env
    in
    let f_code = symbol (Code_id.code_symbol code_id) in
    match Apply_expr.probe_name e with
    | None -> C.direct_call ~dbg ty (C.symbol f_code) args, env, effs
    | Some name ->
      Cmm.Cop (Cprobe { name; handler_code_sym = f_code }, args, dbg), env, effs
    )
  | Call_kind.Function Call_kind.Function_call.Indirect_unknown_arity ->
    fail_if_probe e;
    let f, env, _ = simple env f in
    let args, env, _ = arg_list env args in
    C.indirect_call ~dbg typ_val f args, env, effs
  | Call_kind.Function
      (Call_kind.Function_call.Indirect_known_arity
        { return_arity; param_arity }) ->
    fail_if_probe e;
    if not (check_arity param_arity args)
    then
      Misc.fatal_errorf
        "To_cmm expects indirect_known_arity calls to be full applications in \
         order to translate it"
    else
      let f, env, _ = simple env f in
      let args, env, _ = arg_list env args in
      let ty =
        return_arity |> Flambda_arity.With_subkinds.to_arity
        |> machtype_of_return_arity
      in
      C.indirect_full_call ~dbg ty f args, env, effs
  | Call_kind.C_call { alloc; return_arity; param_arity; is_c_builtin } ->
    fail_if_probe e;
    let f = function_name f in
    (* CR vlaviron: temporary hack to recover the right symbol *)
    let len = String.length f in
    assert (len >= 9);
    assert (String.sub f 0 9 = ".extern__");
    let f = String.sub f 9 (len - 9) in
    let returns = apply_returns e in
    let args, env, _ = arg_list env args in
    let ty = machtype_of_return_arity return_arity in
    let wrap = wrap_extcall_result return_arity in
    let ty_args = List.map C.exttype_of_kind param_arity in
    ( wrap dbg (C.extcall ~dbg ~alloc ~is_c_builtin ~returns ~ty_args f ty args),
      env,
      effs )
  | Call_kind.Method { kind; obj } ->
    fail_if_probe e;
    let obj, env, _ = simple env obj in
    let meth, env, _ = simple env f in
    let kind = meth_kind kind in
    let args, env, _ = arg_list env args in
    C.send kind meth obj args dbg, env, effs

(* function calls that have an exn continuation with extra arguments must be
   wrapped with assignments for the mutable variables used to pass the extra
   arguments. *)
and wrap_call_exn env e call k_exn =
  let h_exn = Exn_continuation.exn_handler k_exn in
  let mut_vars = Env.get_exn_extra_args env h_exn in
  let extra_args = Exn_continuation.extra_args k_exn in
  if List.compare_lengths extra_args mut_vars = 0
  then
    let aux (call, env) (arg, _k) v =
      let arg, env, _ = simple env arg in
      C.sequence (C.assign v arg) call, env
    in
    List.fold_left2 aux (call, env) extra_args mut_vars
  else
    Misc.fatal_errorf
      "Length of [extra_args] in exception continuation %a@ does not match \
       those in the environment (%a)@ for application expression:@ %a"
      Exn_continuation.print k_exn
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Ident.print)
      mut_vars Apply_expr.print e

(* Wrap a function call to honour its continuation *)
and wrap_cont env res effs call e =
  match Apply_expr.continuation e with
  | Never_returns ->
    let wrap, _ = Env.flush_delayed_lets env in
    wrap call, res
  | Return k when Continuation.equal (Env.return_cont env) k ->
    let wrap, _ = Env.flush_delayed_lets env in
    wrap call, res
  | Return k -> begin
    match Env.get_k env k with
    | Jump { types = []; cont } ->
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.sequence call (C.cexit cont [] [])), res
    | Jump { types = [_]; cont } ->
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.cexit cont [call] []), res
    | Inline
        { handler_params = [];
          handler_body = body;
          handler_params_occurrences = _
        } ->
      let var = Variable.create "*apply_res*" in
      let num_normal_occurrences_of_bound_vars =
        Variable.Map.singleton var Num_occurrences.Zero
      in
      let env =
        let_expr_bind env var ~num_normal_occurrences_of_bound_vars call effs
      in
      expr env res body
    | Inline
        { handler_params = [param];
          handler_body = body;
          handler_params_occurrences
        } ->
      let var = Kinded_parameter.var param in
      let env =
        let_expr_bind env var
          ~num_normal_occurrences_of_bound_vars:handler_params_occurrences call
          effs
      in
      expr env res body
    | Jump _ | Inline _ ->
      (* TODO: add support using unboxed tuples *)
      Misc.fatal_errorf
        "Continuation %a should not handle multiple return values in@\n%a@\n%s"
        Continuation.print k Apply_expr.print e
        "Multi-arguments continuation across function calls are not yet \
         supported"
  end

and apply_cont env res e =
  let k = Apply_cont_expr.continuation e in
  let args = Apply_cont_expr.args e in
  if Env.is_exn_handler env k
  then apply_cont_exn env res e k args
  else if Continuation.equal (Env.return_cont env) k
  then apply_cont_ret env res e k args
  else apply_cont_regular env res e k args

(* Exception Continuations always raise their first argument (which is supposed
   to be an exception). Additionally, they may have extra arguments that are
   passed to the handler via mutables variables (which are expected to be
   spilled on the stack). *)
and apply_cont_exn env res e k = function
  | exn :: extra ->
    let raise_kind =
      match Apply_cont_expr.trap_action e with
      | Some (Pop { raise_kind; _ }) -> C.raise_kind raise_kind
      | _ ->
        Misc.fatal_errorf
          "Apply cont %a calls an exception cont without a Pop trap action"
          Apply_cont.print e
    in
    let exn, env, _ = simple env exn in
    let extra, env, _ = arg_list env extra in
    let mut_vars = Env.get_exn_extra_args env k in
    let wrap, _ = Env.flush_delayed_lets env in
    let expr = C.raise_prim raise_kind exn (Apply_cont_expr.debuginfo e) in
    let expr =
      List.fold_left2
        (fun expr arg v -> C.sequence (C.assign v arg) expr)
        expr extra mut_vars
    in
    wrap expr, res
  | [] ->
    Misc.fatal_errorf "Exception continuation %a has no arguments in@\n%a"
      Continuation.print k Apply_cont.print e

(* A call to the return continuation of the current block simply is the return
   value for the current block being translated. *)
and apply_cont_ret env res e k = function
  | [ret] -> (
    let ret, env, _ = simple env ret in
    let wrap, _ = Env.flush_delayed_lets env in
    match Apply_cont_expr.trap_action e with
    | None -> wrap ret, res
    | Some (Pop _) -> wrap (C.trap_return ret [Cmm.Pop]), res
    | Some (Push _) ->
      Misc.fatal_errorf
        "Continuation %a (return cont) should not be applied with a push trap \
         action"
        Continuation.print k)
  | _ ->
    (* TODO: add support using unboxed tuples *)
    Misc.fatal_errorf
      "Continuation %a (return cont) should be applied to a single argument in@\n\
       %a@\n\
       %s"
      Continuation.print k Apply_cont_expr.print e
      "Multi-arguments continuation across function calls are not yet supported"

and apply_cont_regular env res e k args =
  match Env.get_k env k with
  | Inline { handler_params; handler_body; handler_params_occurrences } ->
    (* CR mshinwell: We should fix this. See comment in apply_cont_expr.ml *)
    if not (Apply_cont_expr.trap_action e = None)
    then
      Misc.fatal_errorf "This [Apply_cont] should not have a trap action:@ %a"
        Apply_cont_expr.print e;
    apply_cont_inline env res e k args handler_body handler_params
      ~handler_params_occurrences
  | Jump { types; cont } -> apply_cont_jump env res e types cont args

(* Inlining a continuation call simply needs to bind the arguments to the
   variables that the continuation's body expects. The delayed lets in the
   environment enables that translation to be tail-rec. *)
and apply_cont_inline env res e k args handler_body handler_params
    ~handler_params_occurrences =
  if List.compare_lengths args handler_params = 0
  then
    let env, res =
      List.fold_left2
        (fun env_and_res param ->
          bind_simple env_and_res
            (Kinded_parameter.var param)
            ~num_normal_occurrences_of_bound_vars:handler_params_occurrences)
        (env, res) handler_params args
    in
    expr env res handler_body
  else
    Misc.fatal_errorf
      "Continuation %a in@\n%a@\nExpected %d arguments but got %a."
      Continuation.print k Apply_cont_expr.print e
      (List.length handler_params)
      Apply_cont_expr.print e

(* Continuation calls need to also translate the associated trap actions. *)
and apply_cont_jump env res e types cont args =
  if List.compare_lengths types args = 0
  then
    let trap_actions = apply_cont_trap_actions env e in
    let args, env, _ = arg_list env args in
    let wrap, _ = Env.flush_delayed_lets env in
    wrap (C.cexit cont args trap_actions), res
  else
    Misc.fatal_errorf "Types (%a) do not match arguments of@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Printcmm.machtype)
      types Apply_cont_expr.print e

and apply_cont_trap_actions env e =
  match Apply_cont_expr.trap_action e with
  | None -> []
  | Some (Pop _) -> [Cmm.Pop]
  | Some (Push { exn_handler }) ->
    let cont = Env.get_jump_id env exn_handler in
    [Cmm.Push cont]

and switch env res s =
  let scrutinee = Switch.scrutinee s in
  let e, env, _ = simple env scrutinee in
  let arms = Switch.arms s in
  (* For binary switches, which can be translated to an if-then-else, it can be
     interesting to *not* untag the scrutinee (particularly for those coming
     from a source if-then-else on booleans) as that way the translation can use
     2 instructions instead of 3.

     However, this is only useful to do if the tagged expression is smaller then
     the untagged one (which is not always true due to arithmetic
     simplifications performed by cmm_helpers).

     Additionally for switches with more than 2 arms, not untagging and lifting
     the switch to perform on tagged integer might be worse (because the
     discriminant of the arms may not be successive anymore, thus preventing the
     use of a table), or simply not worth it given the already high number of
     instructions needed for big switches (but that might be up-to-debate on
     small switches with 3-5 arms). *)
  let scrutinee, tag_discriminant =
    match Targetint_31_63.Map.cardinal arms with
    | 2 -> begin
      match match_var_with_extra_info env scrutinee with
      | None -> e, false
      | Some (Untag e') ->
        let size_e = cmm_arith_size e in
        let size_e' = cmm_arith_size e' in
        if size_e' < size_e then e', true else e, false
    end
    | _ -> e, false
  in
  make_switch ~tag_discriminant env res scrutinee arms

and match_var_with_extra_info env simple : Env.extra_info option =
  Simple.pattern_match simple
    ~const:(fun _ -> None)
    ~name:(fun n ~coercion:_ ->
      Name.pattern_match n
        ~symbol:(fun _ -> None)
        ~var:(fun var -> Env.extra_info env var))

(* Small function to estimate the number of arithmetic instructions in a cmm
   expression. This is currently used to determine whether untagging an
   expression resulted in a smaller expression or not (as can happen because of
   some arithmetic simplifications performed by cmm_helpers.ml) *)
and cmm_arith_size e =
  match (e : Cmm.expression) with
  | Cop
      ( ( Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor
        | Clsl | Clsr | Casr ),
        l,
        _ ) ->
    List.fold_left ( + ) 1 @@ List.map cmm_arith_size l
  | _ -> 0

and prepare_discriminant ~tag d =
  let targetint_d = Targetint_31_63.to_targetint' d in
  let prepared_d = if tag then tag_targetint targetint_d else targetint_d in
  int_of_targetint prepared_d

and make_arm ~tag_discriminant env res (d, action) =
  let d = prepare_discriminant ~tag:tag_discriminant d in
  let cmm_action, res = apply_cont env res action in
  (d, cmm_action), res

(* Create a switch given the env, the scrutinee and its arms, plus some
   optimization/simplification option, for now:

   - whether to tag the discriminant of the arms (this suppose that the
   scrutinee is adequately tagged/untagged) *)
and make_switch ~tag_discriminant env res e arms =
  let wrap, env = Env.flush_delayed_lets env in
  match Targetint_31_63.Map.cardinal arms with
  (* Binary case: if-then-else *)
  | 2 -> (
    let aux = make_arm ~tag_discriminant env in
    let first_arm, res = aux res @@ Targetint_31_63.Map.min_binding arms in
    let second_arm, res = aux res @@ Targetint_31_63.Map.max_binding arms in
    match first_arm, second_arm with
    (* These switchs are actually if-then-elses. On such switches,
       transl_switch_clambda will introduce a let-binding to the scrutinee
       before creating an if-then-else, introducing an indirection that might
       prevent some optimizations performed by selectgen/emit when the condition
       is inlined in the if-then-else. *)
    | (0, else_), (_, then_) | (_, then_), (0, else_) ->
      wrap (C.ite e ~then_ ~else_), res
    (* Similar case to the if/then/else but none of the arms match 0, so we have
       to generate an equality test, and make sure it is inside the condition to
       ensure selectgen and emit can take advantage of it. *)
    | (x, if_x), (_, if_not) ->
      wrap (C.ite (C.eq (C.int x) e) ~then_:if_x ~else_:if_not), res)
  (* General case *)
  | n ->
    (* The transl_switch_clambda expects an index array such that index.(d) is
       the index in [cases] of the expression to execute when [e] matches
       [d]. *)
    let max_d, _ = Targetint_31_63.Map.max_binding arms in
    let m = prepare_discriminant ~tag:tag_discriminant max_d in
    let cases = Array.make (n + 1) C.unreachable in
    let index = Array.make (m + 1) n in
    let _, res =
      Targetint_31_63.Map.fold
        (fun discriminant action (i, res) ->
          let (d, cmm_action), res =
            make_arm ~tag_discriminant env res (discriminant, action)
          in
          cases.(i) <- cmm_action;
          index.(d) <- i;
          i + 1, res)
        arms (0, res)
    in
    wrap (C.transl_switch_clambda Debuginfo.none e index cases), res

and invalid _env res _e = C.unreachable, res

(* Sets of closures with no environment can be turned into statically allocated
   symbols, rather than have to allocate them each time *)
and let_static_set_of_closures env res body closure_vars s =
  (* Generate symbols for the set of closures, and each of the closures *)
  let comp_unit = Compilation_unit.get_current_exn () in
  let cids =
    Function_declarations.funs_in_order (Set_of_closures.function_decls s)
    |> Closure_id.Lmap.keys
  in
  let closure_symbols =
    List.map2
      (fun cid v ->
        let v = Var_in_binding_pos.var v in
        (* rename v just to have a different name for the symbol and the
           variable *)
        let name = Variable.unique_name (Variable.rename v) in
        cid, Symbol.create comp_unit (Linkage_name.create name))
      cids closure_vars
    |> Closure_id.Map.of_list
  in
  (* Statically allocate the set of closures *)
  let env, static_data, updates =
    To_cmm_static.static_set_of_closures env closure_symbols s None
  in
  (* As there is no env vars for the set of closures, there must be no
     updates *)
  begin
    match updates with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf "non-empty updates when lifting set of closures"
  end;
  (* update the result with the new static data *)
  let res = R.archive_data (R.set_data res static_data) in
  (* Bind the variables to the symbols for closure ids. CR gbury: inline the
     variables (require to extend to_cmm_enc to inline pure variables more than
     once). *)
  let env =
    List.fold_left2
      (fun acc cid v ->
        let v = Var_in_binding_pos.var v in
        let sym = symbol (Closure_id.Map.find cid closure_symbols) in
        let sym_cmm = C.symbol sym in
        Env.bind_variable acc v Ece.pure false sym_cmm)
      env cids closure_vars
  in
  (* go on in the body *)
  expr env res body

(* Sets of closures with a non-empty environment are allocated *)
and let_dynamic_set_of_closures env res body closure_vars
    ~num_normal_occurrences_of_bound_vars decls elts =
  (* Create the allocation block for the set of closures *)
  let layout =
    Env.layout env
      (List.map fst (Closure_id.Lmap.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  (* Allocating the closure has at least generative effects *)
  let effs =
    Effects.Only_generative_effects Immutable, Coeffects.No_coeffects
  in
  let decl_map = decls |> Closure_id.Lmap.bindings |> Closure_id.Map.of_list in
  let l, env, effs =
    fill_layout decl_map layout.startenv elts env effs [] 0 layout.slots
  in
  let csoc = C.make_closure_block l in
  (* Create a variable to hold the set of closure *)
  let soc_var = Variable.create "*set_of_closures*" in
  let env = Env.bind_variable env soc_var effs false csoc in
  (* Get from the env the cmm variable that was created and bound to the
     compiled set of closures. *)
  let soc_cmm_var, env, peff = Env.inline_variable env soc_var in
  assert (Env.classify peff = Env.Pure);
  (* Add env bindings for all the closure variables. *)
  let env =
    List.fold_left2
      (fun acc cid v ->
        let v = Var_in_binding_pos.var v in
        let e, effs = get_closure_by_offset env soc_cmm_var cid in
        let_expr_bind acc v ~num_normal_occurrences_of_bound_vars e effs)
      env
      (Closure_id.Lmap.keys decls)
      closure_vars
  in
  (* The set of closures, as well as the individual closures variables are
     correctly set in the env, go on translating the body. *)
  expr env res body

and get_closure_by_offset env set_cmm cid =
  match Env.closure_offset env cid with
  | Some { offset; _ } ->
    C.infix_field_address ~dbg:Debuginfo.none set_cmm offset, Ece.pure
  | None -> Misc.fatal_errorf "No closure offset for %a" Closure_id.print cid

and fill_layout decls startenv elts env effs acc i = function
  | [] -> List.rev acc, env, effs
  | (j, slot) :: r ->
    let acc = fill_up_to j acc i in
    let acc, offset, env, eff = fill_slot decls startenv elts env acc j slot in
    let effs = Ece.join eff effs in
    fill_layout decls startenv elts env effs acc offset r

and fill_slot decls startenv elts env acc offset slot =
  match (slot : To_cmm_closure.layout_slot) with
  | Infix_header ->
    let field = C.alloc_infix_header (offset + 1) Debuginfo.none in
    field :: acc, offset + 1, env, Ece.pure
  | Env_var v ->
    let field, env, eff = simple env (Var_within_closure.Map.find v elts) in
    field :: acc, offset + 1, env, eff
  | Closure (c : Closure_id.t) ->
    let code_id = Closure_id.Map.find c decls in
    (* CR-someday mshinwell: We should probably use the code's [dbg], but it
       would be tricky to get hold of, and this is very unlikely to make any
       difference in practice. *)
    let dbg = Debuginfo.none in
    let code_symbol = Code_id.code_symbol code_id in
    let code_name = Linkage_name.to_string (Symbol.linkage_name code_symbol) in
    let arity = Env.get_func_decl_params_arity env code_id in
    let closure_info = C.closure_info ~arity ~startenv:(startenv - offset) in
    (* We build here the **reverse** list of fields for the closure *)
    if arity = 1 || arity = 0
    then
      let acc =
        C.nativeint ~dbg closure_info :: C.symbol ~dbg code_name :: acc
      in
      acc, offset + 2, env, Ece.pure
    else
      let acc =
        C.symbol ~dbg code_name
        :: C.nativeint ~dbg closure_info
        :: C.symbol ~dbg (C.curry_function_sym arity)
        :: acc
      in
      acc, offset + 3, env, Ece.pure

and fill_up_to j acc i =
  if i > j then Misc.fatal_errorf "Problem while filling up a closure in to_cmm";
  if i = j then acc else fill_up_to j (C.int 1 :: acc) (i + 1)

(* Translate a function declaration. *)

and params_and_body env res fun_name p =
  Function_params_and_body.pattern_match p
    ~f:(fun
         ~return_continuation:k
         k_exn
         vars
         ~body
         ~my_closure
         ~is_my_closure_used
         ~my_depth:_
       ->
      try
        let args = function_args vars my_closure ~is_my_closure_used in
        let k_exn = Exn_continuation.exn_handler k_exn in
        (* Init the env and create a jump id for the ret closure in case a trap
           action is attached to one of tis call *)
        let env = Env.enter_function_def env k k_exn in
        (* translate the arg list and body *)
        let env, fun_args = var_list env args in
        let fun_body, res = expr env res body in
        let fun_flags = function_flags () in
        let fun_dbg = Function_params_and_body.debuginfo p in
        C.fundecl fun_name fun_args fun_body fun_flags fun_dbg, res
      with Misc.Fatal_error as e ->
        Format.eprintf
          "\n\
           %sContext is:%s translating function %s to Cmm with return cont %a, \
           exn cont %a and body:@ %a\n"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          fun_name Continuation.print k Exn_continuation.print k_exn Expr.print
          body;
        raise e)

(* CR gbury: for the future, try and rearrange the generated cmm code to move
   assignments closer to the variable definitions Or better: add traps to the
   env to insert assignemnts after the variable definitions. *)

(* Note about the root symbol: it does not need any particular treatment.
   Concerning gc_roots, it's like any other statically allocated symbol: if it
   has an associated computation, then it will already be included in the list
   of gc_roots; otherwise it does not *have* to be a root. *)

(* Compilation units *)

let unit (middle_end_result : Flambda_middle_end.middle_end_result) =
  let unit = middle_end_result.unit in
  let offsets =
    match middle_end_result.cmx with
    | None -> Exported_offsets.imported_offsets ()
    | Some cmx -> Flambda_cmx_format.exported_offsets cmx
  in
  let functions_info = middle_end_result.all_code in
  Profile.record_call "flambda_to_cmm" (fun () ->
      let offsets =
        To_cmm_closure.compute_offsets offsets functions_info unit
      in
      begin
        match middle_end_result.cmx with
        | None ->
          ()
          (* Either opaque was passed, or there is no need to export offsets *)
        | Some cmx ->
          let cmx = Flambda_cmx_format.with_exported_offsets cmx offsets in
          Compilenv.flambda2_set_export_info cmx
      end;
      let used_closure_vars = Flambda_unit.used_closure_vars unit in
      let dummy_k = Continuation.create () in
      (* The dummy continuation is passed here since we're going to manually
         arrange that the return continuation turns into "return unit". (Module
         initialisers return the unit value). *)
      let env =
        Env.mk offsets functions_info dummy_k
          ~exn_continuation:(Flambda_unit.exn_continuation unit)
          ~used_closure_vars
      in
      let _env, return_cont_params =
        (* Note: the environment would be used if we needed to compile the
           handler, but since it's constant we don't need it *)
        var_list env
          [ Kinded_parameter.create (Variable.create "*ret*")
              Flambda_kind.With_subkind.any_value ]
      in
      let return_cont, env =
        Env.add_jump_cont env
          (List.map snd return_cont_params)
          (Flambda_unit.return_continuation unit)
      in
      let body, res = expr env R.empty (Flambda_unit.body unit) in
      let body =
        let unit_value = C.targetint Targetint_32_64.one in
        C.ccatch ~rec_flag:false ~body
          ~handlers:[C.handler return_cont return_cont_params unit_value]
      in
      let entry =
        let dbg = Debuginfo.none in
        let fun_name = Compilenv.make_symbol (Some "entry") in
        let fun_codegen =
          if Flambda_features.backend_cse_at_toplevel ()
          then [Cmm.Reduce_code_size]
          else [Cmm.Reduce_code_size; Cmm.No_CSE]
        in
        C.cfunction (C.fundecl fun_name [] body fun_codegen dbg)
      in
      let data, gc_roots, functions = R.to_cmm res in
      let cmm_data = C.flush_cmmgen_state () in
      let roots = List.map symbol gc_roots in
      (C.gc_root_table roots :: data) @ cmm_data @ functions @ [entry]
      (* Misc.fatal_error "To be continued" *)
      (* let functions = program_functions offsets used_closure_vars unit in *))
