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

[@@@warning "-fragile-match"]

type t = int

let zero = 0

let equal (a : t) (b : t) = a = b

let ( + ) (a : t) (b : t) : t = a + b

let ( - ) (a : t) (b : t) : t = a - b

let ( <= ) (a : t) (b : t) = a <= b

let arch32 = Targetint_32_64.size = 32 (* are we compiling for a 32-bit arch *)

let arch64 = Targetint_32_64.size = 64
(* are we compiling for a 64-bit arch *)

(* Constants *)
(* CR-soon mshinwell: Investigate revised size numbers. *)

(* Native operations are estimated to be of size 1, this includes:

   - arithmetic operations

   - direct loads (without write barrier) *)

(** Allocation size *)
let alloc_size = 5

(* Call sizes are approximated, using for now the same values as flambda1. This
   estimation includes average cost of spilling registers. Typically, for a
   call, the number of arguments will be added to the size to take into account
   the likely move instructions needed before the call. *)
let direct_call_size = 4

let indirect_call_size = 6

let needs_caml_c_call_extcall_size = 10

let does_not_need_caml_c_call_extcall_size = 4

let array_length_size = 2

(* Helper functions for computing sizes of primitives *)

let unary_int_prim_size kind op =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (op : Flambda_primitive.unary_int_arith_op) )
  with
  | Tagged_immediate, Neg -> 1
  | Tagged_immediate, Swap_byte_endianness ->
    (* CR pchambart: size depends a lot of the architecture. If the backend
       handles it, this is a single arith op. *)
    2 + does_not_need_caml_c_call_extcall_size + 1
  | Naked_immediate, Neg -> 1
  | Naked_immediate, Swap_byte_endianness ->
    does_not_need_caml_c_call_extcall_size + 1
  | Naked_int64, Neg when arch32 -> does_not_need_caml_c_call_extcall_size + 1
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Neg -> 1
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Swap_byte_endianness ->
    does_not_need_caml_c_call_extcall_size + 1

let arith_conversion_size src dst =
  match
    ( (src : Flambda_kind.Standard_int_or_float.t),
      (dst : Flambda_kind.Standard_int_or_float.t) )
  with
  (* 64-bit on 32-bit host specific cases *)
  | Naked_int64, Tagged_immediate
  | Naked_int64, Naked_int32
  | Naked_int64, (Naked_nativeint | Naked_immediate)
  | Naked_int64, Naked_float
    when arch32 ->
    does_not_need_caml_c_call_extcall_size + 1 (* arg *)
  | Tagged_immediate, Naked_int64
  | Naked_int32, Naked_int64
  | (Naked_nativeint | Naked_immediate), Naked_int64
  | Naked_float, Naked_int64
    when arch32 ->
    needs_caml_c_call_extcall_size + 1 (* arg *) + 1 (* unbox *)
  | Naked_float, Naked_float -> 0
  | ( (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
      Tagged_immediate ) ->
    1
  | ( Tagged_immediate,
      (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate) ) ->
    1
  | Tagged_immediate, Tagged_immediate
  | Naked_int32, (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_int64, (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | ( Naked_nativeint,
      (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate) )
  | ( Naked_immediate,
      (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate) ) ->
    0
  | Tagged_immediate, Naked_float -> 1
  | (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint), Naked_float
    ->
    1
  | Naked_float, Tagged_immediate -> 1
  | Naked_float, (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint)
    ->
    1

let unbox_number kind =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float | Naked_vec128 -> 1 (* 1 load *)
  | Naked_int64 when arch32 -> 4 (* 2 Cadda + 2 loads *)
  | Naked_int32 | Naked_int64 | Naked_nativeint -> 2
(* Cadda + load *)

let box_number kind =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float | Naked_vec128 -> alloc_size (* 1 alloc *)
  | Naked_int32 when not arch32 -> 1 + alloc_size (* shift/sextend + alloc *)
  | Naked_int32 | Naked_int64 | Naked_nativeint -> alloc_size
(* alloc *)

let block_load (kind : Flambda_primitive.Block_access_kind.t) =
  match kind with Values _ | Naked_floats _ -> 1

let array_load (kind : Flambda_primitive.Array_kind.t) =
  match kind with
  | Immediates -> 1 (* cadda + load *)
  | Naked_floats | Values -> 1
  | Naked_int32s | Naked_int64s | Naked_nativeints ->
    (* more computation is needed because of the representation using a custom
       block *)
    2

let block_set (kind : Flambda_primitive.Block_access_kind.t)
    (init : Flambda_primitive.Init_or_assign.t) =
  match kind, init with
  | Values _, Assignment Heap ->
    does_not_need_caml_c_call_extcall_size (* caml_modify *)
  | Values _, (Assignment Local | Initialization) -> 1 (* cadda + store *)
  | Naked_floats _, (Assignment _ | Initialization) -> 1

let array_set (kind : Flambda_primitive.Array_set_kind.t) =
  match kind with
  | Values (Assignment Heap) -> does_not_need_caml_c_call_extcall_size
  | Values (Assignment Local | Initialization) -> 1
  | Immediates | Naked_floats -> 1
  | Naked_int32s | Naked_int64s | Naked_nativeints -> 2 (* as above *)

let string_or_bigstring_load kind width =
  let start_address_load =
    match (kind : Flambda_primitive.string_like_value) with
    | String | Bytes -> 0
    | Bigstring -> 2
    (* add, load *)
  in
  let elt_load =
    match (width : Flambda_primitive.string_accessor_width) with
    | Eight -> 3 (* untag, add, load *)
    (* CR gbury: these should actually depend on Arch.allow_unaligned_access,
       but that would add a dependency on the backend which is probably not
       desirable ? *)
    | Sixteen -> 2 (* add, load (allow_unaligned_access) *)
    (* 7 (not allow_unaligned_access) *)
    | Thirty_two -> 2 (* add, load (allow_unaligned_access) *)
    (* 17 (not allow_unaligned_access) *)
    | Sixty_four -> if arch32 then does_not_need_caml_c_call_extcall_size else 2
    (* add, load (allow_unaligned_access) *)
    (* 37 (not allow_unaligned_access) *)
    | One_twenty_eight _ -> 2 (* add, load (alignment handled explicitly) *)
  in
  start_address_load + elt_load

(* This is exactly the same as string/bigstirng loads, since loads and stores
   have the same size *)
let bytes_like_set kind width =
  match (kind : Flambda_primitive.bytes_like_value) with
  | Bytes -> string_or_bigstring_load Bytes width
  | Bigstring -> string_or_bigstring_load Bigstring width

let divmod_bi_check else_branch_size (bi : Flambda_kind.Standard_int.t) =
  (* CR gbury: we should allow check Arch.division_crashed_on_overflow, but
     that's likely a dependency we want to avoid ? *)
  if arch32
     ||
     match bi with
     | Naked_int32 -> false
     | Naked_int64 | Naked_nativeint | Naked_immediate | Tagged_immediate ->
       true
  then 2 + else_branch_size
  else 0

let binary_int_arith_primitive kind op =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (op : Flambda_primitive.binary_int_arith_op) )
  with
  (* Int64 bits ints on 32-bit archs *)
  | (Naked_int64, Add | Naked_int64, Sub | Naked_int64, Mul) when arch32 ->
    does_not_need_caml_c_call_extcall_size + 2
  | (Naked_int64, Div | Naked_int64, Mod) when arch32 ->
    needs_caml_c_call_extcall_size + 2
  | (Naked_int64, And | Naked_int64, Or | Naked_int64, Xor) when arch32 ->
    does_not_need_caml_c_call_extcall_size + 2
  (* Tagged integers *)
  | Tagged_immediate, Add -> 2
  | Tagged_immediate, Sub -> 2
  | Tagged_immediate, Mul -> 4
  | Tagged_immediate, Div -> 4
  | Tagged_immediate, Mod -> 4
  | Tagged_immediate, And -> 1
  | Tagged_immediate, Or -> 1
  | Tagged_immediate, Xor -> 2
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Add
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Sub
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Mul
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), And
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Or
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Xor ->
    1
  (* Division and modulo need some extra care *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Div ->
    divmod_bi_check 1 kind + 1
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Mod ->
    divmod_bi_check 0 kind + 1

let binary_int_shift_primitive kind op =
  match
    (kind : Flambda_kind.Standard_int.t), (op : Flambda_primitive.int_shift_op)
  with
  (* Int64 special case *)
  | (Naked_int64, Lsl | Naked_int64, Lsr | Naked_int64, Asr) when arch32 ->
    does_not_need_caml_c_call_extcall_size + 2
  (* Int32 special case *)
  | Naked_int32, Lsr when arch64 -> 2
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> 3
  | Tagged_immediate, Lsr -> 2
  | Tagged_immediate, Asr -> 2
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lsl
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lsr
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Asr ->
    1

let binary_int_comp_primitive kind cmp =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (cmp : Flambda_primitive.signed_or_unsigned Flambda_primitive.comparison)
    )
  with
  | Naked_int64, Neq
  | Naked_int64, Eq
  | Naked_int64, Lt Signed
  | Naked_int64, Le Signed
  | Naked_int64, Gt Signed
  | Naked_int64, Ge Signed
    when arch32 ->
    needs_caml_c_call_extcall_size + 2
  | ( Naked_int64,
      (Neq | Eq | Lt Unsigned | Le Unsigned | Gt Unsigned | Ge Unsigned) )
    when arch32 ->
    needs_caml_c_call_extcall_size + 2
  (* Tagged integers *)
  | Tagged_immediate, Neq
  | Tagged_immediate, Eq
  | Tagged_immediate, Lt Signed
  | Tagged_immediate, Le Signed
  | Tagged_immediate, Gt Signed
  | Tagged_immediate, Ge Signed
  | Tagged_immediate, Lt Unsigned
  | Tagged_immediate, Le Unsigned
  | Tagged_immediate, Gt Unsigned
  | Tagged_immediate, Ge Unsigned ->
    2
  (* Naked integers. *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Neq
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Eq
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lt Signed
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Le Signed
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Gt Signed
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Ge Signed
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lt Unsigned
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Le Unsigned
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Gt Unsigned
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Ge Unsigned
    ->
    2

let int_comparison_like_compare_functions (kind : Flambda_kind.Standard_int.t)
    (_signedness : Flambda_primitive.signed_or_unsigned) =
  match kind with
  | Tagged_immediate | Naked_immediate | Naked_int32 | Naked_int64
  | Naked_nativeint ->
    4

let binary_float_arith_primitive _op = 2

let binary_float_comp_primitive _op = 2

(* Primitives sizes *)

let nullary_prim_size prim =
  match (prim : Flambda_primitive.nullary_primitive) with
  (* CR gbury: check this *)
  | Invalid _ -> 0
  | Optimised_out _ -> 0
  | Probe_is_enabled { name = _ } -> 4
  | Begin_region -> 1
  | Begin_try_region -> 1
  | Enter_inlined_apply _ -> 0

let unary_prim_size prim =
  match (prim : Flambda_primitive.unary_primitive) with
  | Duplicate_array _ | Duplicate_block _ -> needs_caml_c_call_extcall_size + 1
  | Is_int _ -> 1
  | Get_tag -> 2
  | Array_length array_kind -> (
    match array_kind with
    | Array_kind
        (Immediates | Values | Naked_floats | Naked_int64s | Naked_nativeints)
      ->
      array_length_size
    | Array_kind Naked_int32s ->
      (* There is a dynamic check here to see if the array has an odd or even
         number of elements *)
      array_length_size + 2 (* compare + load *)
    | Float_array_opt_dynamic -> array_length_size + 3 (* a bit approximate *))
  | Bigarray_length _ -> 2 (* cadda + load *)
  | String_length _ -> 5
  | Int_as_pointer _ -> 1
  | Opaque_identity _ -> 0
  | Int_arith (kind, op) -> unary_int_prim_size kind op
  | Float_arith _ -> 2
  | Num_conv { src; dst } -> arith_conversion_size src dst
  | Boolean_not -> 1
  | Reinterpret_int64_as_float -> 0
  | Unbox_number k -> unbox_number k
  | Untag_immediate -> 1 (* 1 shift *)
  | Box_number (k, _alloc_mode) -> box_number k
  | Tag_immediate -> 2 (* 1 shift + add *)
  | Project_function_slot _ -> 1 (* caddv *)
  | Project_value_slot _ -> 1 (* load *)
  | Is_boxed_float -> 4 (* tag load + comparison *)
  | Is_flat_float_array -> 4 (* tag load + comparison *)
  | End_region | End_try_region -> 1
  | Obj_dup -> needs_caml_c_call_extcall_size + 1
  | Get_header -> 2
  | Atomic_load _ -> 1

let binary_prim_size prim =
  match (prim : Flambda_primitive.binary_primitive) with
  | Block_load (kind, _) -> block_load kind
  | Array_load (kind, _mut) -> array_load kind
  | String_or_bigstring_load (kind, width) ->
    string_or_bigstring_load kind width
  | Bigarray_load (_dims, (Complex32 | Complex64), _layout) ->
    5 (* ~ 5 block_loads *) + alloc_size (* complex allocation *)
  | Bigarray_load (_dims, _kind, _layout) -> 2 (* ~ 2 block loads *)
  | Phys_equal _op -> 2
  | Int_arith (kind, op) -> binary_int_arith_primitive kind op
  | Int_shift (kind, op) -> binary_int_shift_primitive kind op
  | Int_comp (kind, Yielding_bool cmp) -> binary_int_comp_primitive kind cmp
  | Int_comp (kind, Yielding_int_like_compare_functions signedness) ->
    int_comparison_like_compare_functions kind signedness
  | Float_arith op -> binary_float_arith_primitive op
  | Float_comp (Yielding_bool cmp) -> binary_float_comp_primitive cmp
  | Float_comp (Yielding_int_like_compare_functions ()) -> 8
  | Bigarray_get_alignment _ -> 3 (* load data + add index + and *)
  | Atomic_exchange | Atomic_fetch_and_add ->
    does_not_need_caml_c_call_extcall_size

let ternary_prim_size prim =
  match (prim : Flambda_primitive.ternary_primitive) with
  | Block_set (block_access, init) -> block_set block_access init
  | Array_set kind -> array_set kind
  | Bytes_or_bigstring_set (kind, width) -> bytes_like_set kind width
  | Bigarray_set (_dims, (Complex32 | Complex64), _layout) ->
    5 (* ~ 3 block_load + 2 block_set *)
  | Bigarray_set (_dims, _kind, _layout) -> 2
  (* ~ 1 block_load + 1 block_set *)
  | Atomic_compare_and_set -> does_not_need_caml_c_call_extcall_size

let variadic_prim_size prim args =
  match (prim : Flambda_primitive.variadic_primitive) with
  | Make_block (_, _mut, _alloc_mode)
  (* CR mshinwell: I think Make_array for a generic array ("Anything") is more
     expensive than the other cases *)
  | Make_array (_, _mut, _alloc_mode) ->
    alloc_size + List.length args

let prim (prim : Flambda_primitive.t) =
  match prim with
  | Nullary p -> nullary_prim_size p
  | Unary (p, _) -> unary_prim_size p
  | Binary (p, _, _) -> binary_prim_size p
  | Ternary (p, _, _, _) -> ternary_prim_size p
  | Variadic (p, args) -> variadic_prim_size p args

let simple simple =
  (* CR pchambart: some large const on ARM might be considered larger *)
  Simple.pattern_match simple ~const:(fun _ -> 1) ~name:(fun _ ~coercion:_ -> 0)

let static_consts _ = 0

let apply apply =
  match Apply_expr.call_kind apply with
  | Function { function_call = Direct _; _ } -> direct_call_size
  (* CR mshinwell: Check / fix these numbers *)
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ } ->
    indirect_call_size
  | Function { function_call = Indirect_known_arity; alloc_mode = _ } ->
    indirect_call_size
  | C_call { needs_caml_c_call = true; _ } -> needs_caml_c_call_extcall_size
  | C_call { needs_caml_c_call = false; _ } ->
    does_not_need_caml_c_call_extcall_size
  | Method _ -> 8
(* from flambda/inlining_cost.ml *)

let apply_cont apply_cont =
  let size =
    match Apply_cont_expr.trap_action apply_cont with
    (* Current rough estimates are from amd64/emit.mlp *)
    | None -> 0
    | Some (Push _) -> 4
    | Some (Pop _) -> 2
  in
  size + 1

let invalid = 0

let switch switch = 0 + (5 * Switch_expr.num_arms switch)

let [@ocamlformat "disable"] print ppf t = Format.fprintf ppf "%d" t

let of_int t = t

let to_int t = t

let evaluate ~args:_ t = float_of_int t
