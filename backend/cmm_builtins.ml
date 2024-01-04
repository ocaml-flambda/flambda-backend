(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmm
open Cmm_helpers
open Arch

let four_args name args =
  match args with
  | [arg1; arg2; arg3; arg4] -> arg1, arg2, arg3, arg4
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 4 arguments for %s" name

let three_args name args =
  match args with
  | [arg1; arg2; arg3] -> arg1, arg2, arg3
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 3 arguments for %s" name

let two_args name args =
  match args with
  | [arg1; arg2] -> arg1, arg2
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 2 arguments for %s" name

let one_arg name args =
  match args with
  | [arg] -> arg
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 1 argument for %s" name

let if_operation_supported op ~f =
  match Proc.operation_supported op with true -> Some (f ()) | false -> None

let if_operation_supported_bi bi op ~f =
  if bi = Primitive.Pint64 && size_int = 4
  then None
  else if_operation_supported op ~f

let int_of_value arg dbg = Cop (Cintofvalue, [arg], dbg)

let value_of_int arg dbg = Cop (Cvalueofint, [arg], dbg)

(* Untagging of a negative value shifts in an extra bit. The following code
   clears the shifted sign bit of an untagged int. This straightline code is
   faster on most targets than conditional code for checking whether the
   argument is negative. *)
let clear_sign_bit arg dbg =
  let mask = Nativeint.lognot (Nativeint.shift_left 1n ((size_int * 8) - 1)) in
  Cop (Cand, [arg; Cconst_natint (mask, dbg)], dbg)

let clz ~arg_is_non_zero bi arg dbg =
  let op = Cclz { arg_is_non_zero } in
  if_operation_supported_bi bi op ~f:(fun () ->
      let res = Cop (op, [make_unsigned_int bi arg dbg], dbg) in
      if bi = Primitive.Pint32 && size_int = 8
      then Cop (Caddi, [res; Cconst_int (-32, dbg)], dbg)
      else res)

let ctz ~arg_is_non_zero bi arg dbg =
  let arg = make_unsigned_int bi arg dbg in
  if bi = Primitive.Pint32 && size_int = 8
  then
    (* regardless of the value of the argument [arg_is_non_zero], always set the
       corresponding field to [true], because we make it non-zero below by
       setting bit 32. *)
    let op = Cctz { arg_is_non_zero = true } in
    if_operation_supported_bi bi op ~f:(fun () ->
        (* Set bit 32 *)
        let mask = Nativeint.shift_left 1n 32 in
        Cop (op, [Cop (Cor, [arg; Cconst_natint (mask, dbg)], dbg)], dbg))
  else
    let op = Cctz { arg_is_non_zero } in
    if_operation_supported_bi bi op ~f:(fun () -> Cop (op, [arg], dbg))

let popcnt bi arg dbg =
  if_operation_supported_bi bi Cpopcnt ~f:(fun () ->
      Cop (Cpopcnt, [make_unsigned_int bi arg dbg], dbg))

let mulhi bi ~signed args dbg =
  let op = Cmulhi { signed } in
  if_operation_supported_bi bi op ~f:(fun () -> Cop (op, args, dbg))

let ext_pointer_load chunk name args dbg =
  let p = int_as_pointer (one_arg name args) dbg in
  Some (Cop (mk_load_mut chunk, [p], dbg))

let ext_pointer_store chunk name args dbg =
  let arg1, arg2 = two_args name args in
  let p = int_as_pointer arg1 dbg in
  Some (return_unit dbg (Cop (Cstore (chunk, Assignment), [p; arg2], dbg)))

let bigstring_prefetch ~is_write locality args dbg =
  let op = Cprefetch { is_write; locality } in
  if_operation_supported op ~f:(fun () ->
      let arg1, arg2 = two_args "bigstring_prefetch" args in
      (* [arg2], the index, is already untagged. *)
      bind "index" arg2 (fun idx ->
          bind "ba" arg1 (fun ba ->
              bind "ba_data"
                (Cop (mk_load_mut Word_int, [field_address ba 1 dbg], dbg))
                (fun ba_data ->
                  (* pointer to element "idx" of "ba" of type (char,
                     int8_unsigned_elt, c_layout) Bigarray.Array1.t is simply
                     offset "idx" from "ba_data" *)
                  return_unit dbg (Cop (op, [add_int ba_data idx dbg], dbg))))))

let prefetch ~is_write locality arg dbg =
  let op = Cprefetch { is_write; locality } in
  if_operation_supported op ~f:(fun () ->
      return_unit dbg (Cop (op, [arg], dbg)))

let prefetch_offset ~is_write locality (arg1, arg2) dbg =
  (* [arg2], the index, is already untagged. *)
  let op = Cprefetch { is_write; locality } in
  if_operation_supported op ~f:(fun () ->
      return_unit dbg (Cop (op, [add_int arg1 arg2 dbg], dbg)))

let ext_pointer_prefetch ~is_write locality arg dbg =
  prefetch ~is_write locality (int_as_pointer arg dbg) dbg

let native_pointer_cas size (arg1, arg2, arg3) dbg =
  let op = Catomic { op = Compare_and_swap; size } in
  if_operation_supported op ~f:(fun () ->
      bind "set_to" arg3 (fun set_to ->
          bind "compare_with" arg2 (fun compare_with ->
              bind "dst" arg1 (fun dst ->
                  tag_int (Cop (op, [compare_with; set_to; dst], dbg)) dbg))))

let ext_pointer_cas size (arg1, arg2, arg3) dbg =
  native_pointer_cas size (int_as_pointer arg1 dbg, arg2, arg3) dbg

let bigstring_cas size (arg1, arg2, arg3, arg4) dbg =
  let op = Catomic { op = Compare_and_swap; size } in
  if_operation_supported op ~f:(fun () ->
      bind "set_to" arg4 (fun set_to ->
          bind "compare_with" arg3 (fun compare_with ->
              bind "idx" arg2 (fun idx ->
                  bind "bs" arg1 (fun bs ->
                      bind "bs_data"
                        (Cop
                           (mk_load_mut Word_int, [field_address bs 1 dbg], dbg))
                        (fun bs_data ->
                          bind "dst" (add_int bs_data idx dbg) (fun dst ->
                              tag_int
                                (Cop (op, [compare_with; set_to; dst], dbg))
                                dbg)))))))

let native_pointer_atomic_add size (arg1, arg2) dbg =
  let op = Catomic { op = Fetch_and_add; size } in
  if_operation_supported op ~f:(fun () ->
      bind "src" arg2 (fun src ->
          bind "dst" arg1 (fun dst -> Cop (op, [src; dst], dbg))))

let native_pointer_atomic_sub size (arg1, arg2) dbg =
  native_pointer_atomic_add size (arg1, neg_int arg2 dbg) dbg

let ext_pointer_atomic_add size (arg1, arg2) dbg =
  native_pointer_atomic_add size (int_as_pointer arg1 dbg, arg2) dbg

let ext_pointer_atomic_sub size (arg1, arg2) dbg =
  native_pointer_atomic_add size (int_as_pointer arg1 dbg, neg_int arg2 dbg) dbg

let bigstring_atomic_add size (arg1, arg2, arg3) dbg =
  let op = Catomic { op = Fetch_and_add; size } in
  if_operation_supported op ~f:(fun () ->
      bind "src" arg3 (fun src ->
          bind "idx" arg2 (fun idx ->
              bind "bs" arg1 (fun bs ->
                  bind "bs_data"
                    (Cop (mk_load_mut Word_int, [field_address bs 1 dbg], dbg))
                    (fun bs_data ->
                      bind "dst" (add_int bs_data idx dbg) (fun dst ->
                          Cop (op, [src; dst], dbg)))))))

let bigstring_atomic_sub size (arg1, arg2, arg3) dbg =
  bigstring_atomic_add size (arg1, arg2, neg_int arg3 dbg) dbg

(* Assumes unboxed float64 *)
let rec const_float_args n args name =
  match n, args with
  | 0, [] -> []
  | n, Cconst_float (f, _) :: args -> f :: const_float_args (n - 1) args name
  | _ -> Misc.fatal_errorf "Invalid float constant arguments for %s" name

(* Assumes untagged int or unboxed int32, always representable by int63 *)
let rec const_int_args n args name =
  match n, args with
  | 0, [] -> []
  | n, Cconst_int (i, _) :: args -> i :: const_int_args (n - 1) args name
  | _ -> Misc.fatal_errorf "Invalid int constant arguments for %s" name

(* Assumes unboxed int64: no tag, comes as Cconst_int when representable by
   int63, otherwise we get Cconst_natint *)
let rec const_int64_args n args name =
  match n, args with
  | 0, [] -> []
  | n, Cconst_int (i, _) :: args ->
    Int64.of_int i :: const_int64_args (n - 1) args name
  | n, Cconst_natint (i, _) :: args ->
    Int64.of_nativeint i :: const_int64_args (n - 1) args name
  | _ -> Misc.fatal_errorf "Invalid int64 constant arguments for %s" name

let int64_of_int8 i =
  (* CR mslater: (SIMD) replace once we have unboxed int8 *)
  if i < 0 || i > 0xff
  then Misc.fatal_errorf "Int8 constant must be in [0x0,0xff]: %016x" i;
  Int64.of_int i

let int64_of_int16 i =
  (* CR mslater: (SIMD) replace once we have unboxed int16 *)
  if i < 0 || i > 0xffff
  then Misc.fatal_errorf "Int16 constant must be in [0x0,0xffff]: %016x" i;
  Int64.of_int i

let int64_of_int32 i =
  if i < Int32.to_int Int32.min_int || i > Int32.to_int Int32.max_int
  then Misc.fatal_errorf "Constant was not an int32: %016x" i;
  Int64.of_int i |> Int64.logand 0xffffffffL

let int64_of_float32 f =
  (* CR mslater: (SIMD) replace once we have unboxed float32 *)
  Int32.bits_of_float f |> Int64.of_int32 |> Int64.logand 0xffffffffL

let pack_int32s i0 i1 = Int64.(logor (shift_left i1 32) i0)

let pack_int16s i0 i1 i2 i3 =
  Int64.(
    logor
      (logor (shift_left i3 48) (shift_left i2 32))
      (logor (shift_left i1 16) i0))

let pack_int8s i0 i1 i2 i3 i4 i5 i6 i7 =
  Int64.(
    logor
      (logor
         (logor (shift_left i7 56) (shift_left i6 48))
         (logor (shift_left i5 40) (shift_left i4 32)))
      (logor
         (logor (shift_left i3 24) (shift_left i2 16))
         (logor (shift_left i1 8) i0)))

let transl_vec128_builtin name args dbg _typ_res =
  match name with
  (* Vector casts (no-ops) *)
  | "caml_vec128_cast" ->
    let op = Cvectorcast Bits128 in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  (* Scalar casts. These leave the top bits of the vector unspecified. *)
  | "caml_float64x2_low_of_float" ->
    let op = Cscalarcast (V128_of_scalar Float64x2) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_float64x2_low_to_float" ->
    let op = Cscalarcast (V128_to_scalar Float64x2) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_float32x4_low_of_float" ->
    (* CR mslater: (SIMD) replace once we have unboxed float32 *)
    let op = Cscalarcast (V128_of_scalar Float32x4) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_float32x4_low_to_float" ->
    (* CR mslater: (SIMD) replace once we have unboxed float32 *)
    let op = Cscalarcast (V128_to_scalar Float32x4) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int64x2_low_of_int64" ->
    let op = Cscalarcast (V128_of_scalar Int64x2) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int64x2_low_to_int64" ->
    let op = Cscalarcast (V128_to_scalar Int64x2) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int32x4_low_of_int32" ->
    let op = Cscalarcast (V128_of_scalar Int32x4) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int32x4_low_to_int32" ->
    let op = Cscalarcast (V128_to_scalar Int32x4) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int16x8_low_of_int" ->
    (* CR mslater: (SIMD) replace once we have unboxed int16 *)
    let op = Cscalarcast (V128_of_scalar Int16x8) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int16x8_low_to_int" ->
    (* CR mslater: (SIMD) replace once we have unboxed int16 *)
    let op = Cscalarcast (V128_to_scalar Int16x8) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int8x16_low_of_int" ->
    (* CR mslater: (SIMD) replace once we have unboxed int8 *)
    let op = Cscalarcast (V128_of_scalar Int8x16) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int8x16_low_to_int" ->
    (* CR mslater: (SIMD) replace once we have unboxed int8 *)
    let op = Cscalarcast (V128_to_scalar Int8x16) in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  (* Constants *)
  | "caml_float32x4_const1" ->
    (* CR mslater: (SIMD) replace once we have unboxed float32 *)
    let f = const_float_args 1 args name |> List.hd in
    let i = int64_of_float32 f in
    let i = pack_int32s i i in
    Some (Cconst_vec128 ({ low = i; high = i }, dbg))
  | "caml_float32x4_const4" ->
    (* CR mslater: (SIMD) replace once we have unboxed float32 *)
    let i0, i1, i2, i3 =
      match const_float_args 4 args name |> List.map int64_of_float32 with
      | [i0; i1; i2; i3] -> i0, i1, i2, i3
      | _ -> assert false
    in
    let low = pack_int32s i0 i1 in
    let high = pack_int32s i2 i3 in
    Some (Cconst_vec128 ({ low; high }, dbg))
  | "caml_float64x2_const1" ->
    let f = const_float_args 1 args name |> List.hd in
    let i = Int64.bits_of_float f in
    Some (Cconst_vec128 ({ low = i; high = i }, dbg))
  | "caml_float64x2_const2" ->
    let low, high =
      match const_float_args 2 args name |> List.map Int64.bits_of_float with
      | [f0; f1] -> f0, f1
      | _ -> assert false
    in
    Some (Cconst_vec128 ({ low; high }, dbg))
  | "caml_int64x2_const1" ->
    let i = const_int64_args 1 args name |> List.hd in
    Some (Cconst_vec128 ({ low = i; high = i }, dbg))
  | "caml_int64x2_const2" ->
    let low, high =
      match const_int64_args 2 args name with
      | [i0; i1] -> i0, i1
      | _ -> assert false
    in
    Some (Cconst_vec128 ({ low; high }, dbg))
  | "caml_int32x4_const1" ->
    let i = const_int_args 1 args name |> List.hd |> int64_of_int32 in
    let i = pack_int32s i i in
    Some (Cconst_vec128 ({ low = i; high = i }, dbg))
  | "caml_int32x4_const4" ->
    let i0, i1, i2, i3 =
      match const_int_args 4 args name |> List.map int64_of_int32 with
      | [i0; i1; i2; i3] -> i0, i1, i2, i3
      | _ -> assert false
    in
    let low = pack_int32s i0 i1 in
    let high = pack_int32s i2 i3 in
    Some (Cconst_vec128 ({ low; high }, dbg))
  | "caml_int16x8_const1" ->
    (* CR mslater: (SIMD) replace once we have unboxed int16 *)
    let i = const_int_args 1 args name |> List.hd |> int64_of_int16 in
    let i = pack_int16s i i i i in
    Some (Cconst_vec128 ({ low = i; high = i }, dbg))
  | "caml_int16x8_const8" ->
    (* CR mslater: (SIMD) replace once we have unboxed int16 *)
    let i0, i1, i2, i3, i4, i5, i6, i7 =
      match const_int_args 8 args name |> List.map int64_of_int16 with
      | [i0; i1; i2; i3; i4; i5; i6; i7] -> i0, i1, i2, i3, i4, i5, i6, i7
      | _ -> assert false
    in
    let low = pack_int16s i0 i1 i2 i3 in
    let high = pack_int16s i4 i5 i6 i7 in
    Some (Cconst_vec128 ({ low; high }, dbg))
  | "caml_int8x16_const1" ->
    (* CR mslater: (SIMD) replace once we have unboxed int8 *)
    let i = const_int_args 1 args name |> List.hd |> int64_of_int8 in
    let i = pack_int8s i i i i i i i i in
    Some (Cconst_vec128 ({ low = i; high = i }, dbg))
  | "caml_int8x16_const16" ->
    (* CR mslater: (SIMD) replace once we have unboxed int8 *)
    let i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15 =
      match const_int_args 16 args name |> List.map int64_of_int8 with
      | [i0; i1; i2; i3; i4; i5; i6; i7; i8; i9; i10; i11; i12; i13; i14; i15]
        ->
        i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15
      | _ -> assert false
    in
    let low = pack_int8s i0 i1 i2 i3 i4 i5 i6 i7 in
    let high = pack_int8s i8 i9 i10 i11 i12 i13 i14 i15 in
    Some (Cconst_vec128 ({ low; high }, dbg))
  | _ -> None

(** [transl_builtin prim args dbg] returns None if the built-in [prim] is not
  supported, otherwise it constructs and returns the corresponding Cmm
  expression.

  The names of builtins below correspond to the native code names associated
  with "external" declarations in the stand-alone library [ocaml_intrinsics].

  For situations such as where the Cmm code below returns e.g. an untagged
  integer, we exploit the generic mechanism on "external" to deal with the
  tagging before the result is returned to the user. *)
let transl_builtin name args dbg typ_res =
  match name with
  | "caml_int_clz_tagged_to_untagged" ->
    (* The tag does not change the number of leading zeros. The advantage of
       keeping the tag is it guarantees that, on x86-64, the input to the BSR
       instruction is nonzero. *)
    let op = Cclz { arg_is_non_zero = true } in
    if_operation_supported op ~f:(fun () -> Cop (op, args, dbg))
  | "caml_int_clz_untagged_to_untagged" ->
    let op = Cclz { arg_is_non_zero = false } in
    if_operation_supported op ~f:(fun () ->
        let arg = clear_sign_bit (one_arg name args) dbg in
        Cop (Caddi, [Cop (op, [arg], dbg); Cconst_int (-1, dbg)], dbg))
  | "caml_int64_clz_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:false Pint64 (one_arg name args) dbg
  | "caml_int32_clz_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:false Pint32 (one_arg name args) dbg
  | "caml_nativeint_clz_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:false Pnativeint (one_arg name args) dbg
  | "caml_int64_clz_nonzero_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:true Pint64 (one_arg name args) dbg
  | "caml_int32_clz_nonzero_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:true Pint32 (one_arg name args) dbg
  | "caml_nativeint_clz_nonzero_unboxed_to_untagged" ->
    clz ~arg_is_non_zero:true Pnativeint (one_arg name args) dbg
  | "caml_int_popcnt_tagged_to_untagged" ->
    if_operation_supported Cpopcnt ~f:(fun () ->
        (* Having the argument tagged saves a shift, but there is one extra
           "set" bit, which is accounted for by the (-1) below. *)
        Cop (Caddi, [Cop (Cpopcnt, args, dbg); Cconst_int (-1, dbg)], dbg))
  | "caml_int_popcnt_untagged_to_untagged" ->
    (* This code is expected to be faster than [popcnt(tagged_x) - 1] when the
       untagged argument is already available from a previous computation. *)
    if_operation_supported Cpopcnt ~f:(fun () ->
        let arg = clear_sign_bit (one_arg name args) dbg in
        Cop (Cpopcnt, [arg], dbg))
  | "caml_int64_popcnt_unboxed_to_untagged" ->
    popcnt Pint64 (one_arg name args) dbg
  | "caml_int32_popcnt_unboxed_to_untagged" ->
    popcnt Pint32 (one_arg name args) dbg
  | "caml_nativeint_popcnt_unboxed_to_untagged" ->
    popcnt Pnativeint (one_arg name args) dbg
  | "caml_int_ctz_untagged_to_untagged" ->
    (* Assuming a 64-bit x86-64 target:

       Setting the top bit of the input for the BSF instruction ensures the
       input is nonzero without affecting the result.

       The expression [x lor (1 lsl 63)] sets the top bit of x. The constant:

       [1 lsl 63]

       can be precomputed statically:

       Cconst_natint ((Nativeint.shift_left 1n 63), dbg)

       However, the encoding of this OR instruction with the large static
       constant is 10 bytes long, on x86-64. Instead, we emit a shift operation,
       whose corresponding instruction is 1 byte shorter. This will not require
       an extra register, unless both the argument and result of the BSF
       instruction are in the same register. *)
    let op = Cctz { arg_is_non_zero = true } in
    if_operation_supported op ~f:(fun () ->
        let c =
          Cop
            ( Clsl,
              [Cconst_int (1, dbg); Cconst_int ((size_int * 8) - 1, dbg)],
              dbg )
        in
        Cop (op, [Cop (Cor, [one_arg name args; c], dbg)], dbg))
  | "caml_int32_ctz_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:false Pint32 (one_arg name args) dbg
  | "caml_int64_ctz_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:false Pint64 (one_arg name args) dbg
  | "caml_nativeint_ctz_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:false Pnativeint (one_arg name args) dbg
  | "caml_int32_ctz_nonzero_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:true Pint32 (one_arg name args) dbg
  | "caml_int64_ctz_nonzero_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:true Pint64 (one_arg name args) dbg
  | "caml_nativeint_ctz_nonzero_unboxed_to_untagged" ->
    ctz ~arg_is_non_zero:true Pnativeint (one_arg name args) dbg
  | "caml_signed_int64_mulh_unboxed" -> mulhi ~signed:true Pint64 args dbg
  | "caml_unsigned_int64_mulh_unboxed" -> mulhi ~signed:false Pint64 args dbg
  | "caml_int32_unsigned_to_int_trunc_unboxed_to_untagged" ->
    Some (zero_extend_32 dbg (one_arg name args))
  | "caml_csel_value" | "caml_csel_int_untagged" | "caml_csel_int64_unboxed"
  | "caml_csel_int32_unboxed" | "caml_csel_nativeint_unboxed" ->
    (* Unboxed float variant of csel intrinsic is not currently supported. It
       can be emitted on arm64 using FCSEL, but there appears to be no
       corresponding instruction on amd64 for xmm registers. *)
    let op = Ccsel typ_res in
    let cond, ifso, ifnot = three_args name args in
    if_operation_supported op ~f:(fun () ->
        let cond = test_bool dbg cond in
        match cond with
        | Cconst_int (0, _) -> ifnot
        | Cconst_int (1, _) -> ifso
        | _ -> Cop (op, [cond; ifso; ifnot], dbg))
  (* Native_pointer: handled as unboxed nativeint *)
  | "caml_ext_pointer_as_native_pointer" ->
    Some (int_as_pointer (one_arg name args) dbg)
  | "caml_native_pointer_of_value" ->
    Some (int_of_value (one_arg name args) dbg)
  | "caml_native_pointer_to_value" ->
    Some (value_of_int (one_arg name args) dbg)
  | "caml_native_pointer_load_immediate"
  | "caml_native_pointer_load_unboxed_nativeint" ->
    Some (Cop (mk_load_mut Word_int, args, dbg))
  | "caml_native_pointer_store_immediate"
  | "caml_native_pointer_store_unboxed_nativeint" ->
    Some (return_unit dbg (Cop (Cstore (Word_int, Assignment), args, dbg)))
  | "caml_native_pointer_load_unboxed_int64" when size_int = 8 ->
    Some (Cop (mk_load_mut Word_int, args, dbg))
  | "caml_native_pointer_store_unboxed_int64" when size_int = 8 ->
    Some (return_unit dbg (Cop (Cstore (Word_int, Assignment), args, dbg)))
  | "caml_native_pointer_load_signed_int32"
  | "caml_native_pointer_load_unboxed_int32" ->
    Some (Cop (mk_load_mut Thirtytwo_signed, args, dbg))
  | "caml_native_pointer_store_signed_int32"
  | "caml_native_pointer_store_unboxed_int32" ->
    Some
      (return_unit dbg (Cop (Cstore (Thirtytwo_signed, Assignment), args, dbg)))
  | "caml_native_pointer_load_unsigned_int32" ->
    Some (Cop (mk_load_mut Thirtytwo_unsigned, args, dbg))
  | "caml_native_pointer_store_unsigned_int32" ->
    Some
      (return_unit dbg
         (Cop (Cstore (Thirtytwo_unsigned, Assignment), args, dbg)))
  | "caml_native_pointer_load_unboxed_float" ->
    Some (Cop (mk_load_mut Double, args, dbg))
  | "caml_native_pointer_store_unboxed_float" ->
    Some (return_unit dbg (Cop (Cstore (Double, Assignment), args, dbg)))
  | "caml_native_pointer_load_unsigned_int8" ->
    Some (Cop (mk_load_mut Byte_unsigned, args, dbg))
  | "caml_native_pointer_load_signed_int8" ->
    Some (Cop (mk_load_mut Byte_signed, args, dbg))
  | "caml_native_pointer_load_unsigned_int16" ->
    Some (Cop (mk_load_mut Sixteen_unsigned, args, dbg))
  | "caml_native_pointer_load_signed_int16" ->
    Some (Cop (mk_load_mut Sixteen_signed, args, dbg))
  | "caml_native_pointer_store_unsigned_int8" ->
    Some (return_unit dbg (Cop (Cstore (Byte_unsigned, Assignment), args, dbg)))
  | "caml_native_pointer_store_signed_int8" ->
    Some (return_unit dbg (Cop (Cstore (Byte_signed, Assignment), args, dbg)))
  | "caml_native_pointer_store_unsigned_int16" ->
    Some
      (return_unit dbg (Cop (Cstore (Sixteen_unsigned, Assignment), args, dbg)))
  | "caml_native_pointer_store_signed_int16" ->
    Some
      (return_unit dbg (Cop (Cstore (Sixteen_signed, Assignment), args, dbg)))
  (* Ext_pointer: handled as tagged int *)
  | "caml_ext_pointer_load_immediate"
  | "caml_ext_pointer_load_unboxed_nativeint" ->
    ext_pointer_load Word_int name args dbg
  | "caml_ext_pointer_store_immediate"
  | "caml_ext_pointer_store_unboxed_nativeint" ->
    ext_pointer_store Word_int name args dbg
  | "caml_ext_pointer_load_unboxed_int64" when size_int = 8 ->
    ext_pointer_load Word_int name args dbg
  | "caml_ext_pointer_store_unboxed_int64" when size_int = 8 ->
    ext_pointer_store Word_int name args dbg
  | "caml_ext_pointer_load_signed_int32" | "caml_ext_pointer_load_unboxed_int32"
    ->
    ext_pointer_load Thirtytwo_signed name args dbg
  | "caml_ext_pointer_store_signed_int32"
  | "caml_ext_pointer_store_unboxed_int32" ->
    ext_pointer_store Thirtytwo_signed name args dbg
  | "caml_ext_pointer_load_unsigned_int32" ->
    ext_pointer_load Thirtytwo_unsigned name args dbg
  | "caml_ext_pointer_store_unsigned_int32" ->
    ext_pointer_store Thirtytwo_unsigned name args dbg
  | "caml_ext_pointer_load_unboxed_float" ->
    ext_pointer_load Double name args dbg
  | "caml_ext_pointer_store_unboxed_float" ->
    ext_pointer_store Double name args dbg
  | "caml_ext_pointer_load_unsigned_int8" ->
    ext_pointer_load Byte_unsigned name args dbg
  | "caml_ext_pointer_load_signed_int8" ->
    ext_pointer_load Byte_signed name args dbg
  | "caml_ext_pointer_load_unsigned_int16" ->
    ext_pointer_load Sixteen_unsigned name args dbg
  | "caml_ext_pointer_load_signed_int16" ->
    ext_pointer_load Sixteen_signed name args dbg
  | "caml_ext_pointer_store_unsigned_int8" ->
    ext_pointer_store Byte_unsigned name args dbg
  | "caml_ext_pointer_store_signed_int8" ->
    ext_pointer_store Byte_signed name args dbg
  | "caml_ext_pointer_store_unsigned_int16" ->
    ext_pointer_store Sixteen_unsigned name args dbg
  | "caml_ext_pointer_store_signed_int16" ->
    ext_pointer_store Sixteen_signed name args dbg
  (* Bigstring prefetch *)
  | "caml_prefetch_write_high_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true High args dbg
  | "caml_prefetch_write_moderate_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true Moderate args dbg
  | "caml_prefetch_write_low_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true Low args dbg
  | "caml_prefetch_write_none_bigstring_untagged" ->
    bigstring_prefetch ~is_write:true Nonlocal args dbg
  | "caml_prefetch_read_none_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false Nonlocal args dbg
  | "caml_prefetch_read_high_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false High args dbg
  | "caml_prefetch_read_moderate_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false Moderate args dbg
  | "caml_prefetch_read_low_bigstring_untagged" ->
    bigstring_prefetch ~is_write:false Low args dbg
  (* Ext_pointer prefetch *)
  | "caml_prefetch_write_high_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true High (one_arg name args) dbg
  | "caml_prefetch_write_moderate_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true Moderate (one_arg name args) dbg
  | "caml_prefetch_write_low_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true Low (one_arg name args) dbg
  | "caml_prefetch_write_none_ext_pointer" ->
    ext_pointer_prefetch ~is_write:true Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_none_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_high_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false High (one_arg name args) dbg
  | "caml_prefetch_read_moderate_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false Moderate (one_arg name args) dbg
  | "caml_prefetch_read_low_ext_pointer" ->
    ext_pointer_prefetch ~is_write:false Low (one_arg name args) dbg
  (* Value and unboxed Native_pointer prefetch *)
  | "caml_prefetch_write_high" ->
    prefetch ~is_write:true High (one_arg name args) dbg
  | "caml_prefetch_write_moderate" ->
    prefetch ~is_write:true Moderate (one_arg name args) dbg
  | "caml_prefetch_write_low" ->
    prefetch ~is_write:true Low (one_arg name args) dbg
  | "caml_prefetch_write_none" ->
    prefetch ~is_write:true Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_none" ->
    prefetch ~is_write:false Nonlocal (one_arg name args) dbg
  | "caml_prefetch_read_high" ->
    prefetch ~is_write:false High (one_arg name args) dbg
  | "caml_prefetch_read_moderate" ->
    prefetch ~is_write:false Moderate (one_arg name args) dbg
  | "caml_prefetch_read_low" ->
    prefetch ~is_write:false Low (one_arg name args) dbg
  (* Prefetch value with offset *)
  | "caml_prefetch_write_high_val_offset_untagged" ->
    prefetch_offset ~is_write:true High (two_args name args) dbg
  | "caml_prefetch_write_moderate_val_offset_untagged" ->
    prefetch_offset ~is_write:true Moderate (two_args name args) dbg
  | "caml_prefetch_write_low_val_offset_untagged" ->
    prefetch_offset ~is_write:true Low (two_args name args) dbg
  | "caml_prefetch_write_none_val_offset_untagged" ->
    prefetch_offset ~is_write:true Nonlocal (two_args name args) dbg
  | "caml_prefetch_read_none_val_offset_untagged" ->
    prefetch_offset ~is_write:false Nonlocal (two_args name args) dbg
  | "caml_prefetch_read_high_val_offset_untagged" ->
    prefetch_offset ~is_write:false High (two_args name args) dbg
  | "caml_prefetch_read_moderate_val_offset_untagged" ->
    prefetch_offset ~is_write:false Moderate (two_args name args) dbg
  | "caml_prefetch_read_low_val_offset_untagged" ->
    prefetch_offset ~is_write:false Low (two_args name args) dbg
  (* Atomics *)
  | "caml_native_pointer_fetch_and_add_nativeint_unboxed"
  | "caml_native_pointer_fetch_and_add_int_untagged" ->
    native_pointer_atomic_add Word (two_args name args) dbg
  | "caml_native_pointer_fetch_and_add_int64_unboxed" when size_int = 8 ->
    native_pointer_atomic_add Sixtyfour (two_args name args) dbg
  | "caml_native_pointer_fetch_and_add_int32_unboxed" ->
    native_pointer_atomic_add Thirtytwo (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_add_nativeint_unboxed"
  | "caml_ext_pointer_fetch_and_add_int_untagged" ->
    ext_pointer_atomic_add Word (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_add_int64_unboxed" when size_int = 8 ->
    ext_pointer_atomic_add Sixtyfour (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_add_int32_unboxed" ->
    ext_pointer_atomic_add Thirtytwo (two_args name args) dbg
  | "caml_bigstring_fetch_and_add_nativeint_unboxed"
  | "caml_bigstring_fetch_and_add_int_untagged" ->
    bigstring_atomic_add Word (three_args name args) dbg
  | "caml_bigstring_fetch_and_add_int64_unboxed" when size_int = 8 ->
    bigstring_atomic_add Sixtyfour (three_args name args) dbg
  | "caml_bigstring_fetch_and_add_int32_unboxed" ->
    bigstring_atomic_add Thirtytwo (three_args name args) dbg
  | "caml_native_pointer_fetch_and_sub_nativeint_unboxed"
  | "caml_native_pointer_fetch_and_sub_int_untagged" ->
    native_pointer_atomic_sub Word (two_args name args) dbg
  | "caml_native_pointer_fetch_and_sub_int64_unboxed" when size_int = 8 ->
    native_pointer_atomic_sub Sixtyfour (two_args name args) dbg
  | "caml_native_pointer_fetch_and_sub_int32_unboxed" ->
    native_pointer_atomic_sub Thirtytwo (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_sub_nativeint_unboxed"
  | "caml_ext_pointer_fetch_and_sub_int_untagged" ->
    ext_pointer_atomic_sub Word (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_sub_int64_unboxed" when size_int = 8 ->
    ext_pointer_atomic_sub Sixtyfour (two_args name args) dbg
  | "caml_ext_pointer_fetch_and_sub_int32_unboxed" ->
    ext_pointer_atomic_sub Thirtytwo (two_args name args) dbg
  | "caml_bigstring_fetch_and_sub_nativeint_unboxed"
  | "caml_bigstring_fetch_and_sub_int_untagged" ->
    bigstring_atomic_sub Word (three_args name args) dbg
  | "caml_bigstring_fetch_and_sub_int64_unboxed" when size_int = 8 ->
    bigstring_atomic_sub Sixtyfour (three_args name args) dbg
  | "caml_bigstring_fetch_and_sub_int32_unboxed" ->
    bigstring_atomic_sub Thirtytwo (three_args name args) dbg
  | "caml_native_pointer_compare_and_swap_int_untagged"
  | "caml_native_pointer_compare_and_swap_nativeint_unboxed" ->
    native_pointer_cas Word (three_args name args) dbg
  | "caml_native_pointer_compare_and_swap_int64_unboxed" when size_int = 8 ->
    native_pointer_cas Sixtyfour (three_args name args) dbg
  | "caml_native_pointer_compare_and_swap_int32_unboxed" ->
    native_pointer_cas Thirtytwo (three_args name args) dbg
  | "caml_ext_pointer_compare_and_swap_int_untagged"
  | "caml_ext_pointer_compare_and_swap_nativeint_unboxed" ->
    ext_pointer_cas Word (three_args name args) dbg
  | "caml_ext_pointer_compare_and_swap_int64_unboxed" when size_int = 8 ->
    ext_pointer_cas Sixtyfour (three_args name args) dbg
  | "caml_ext_pointer_compare_and_swap_int32_unboxed" ->
    ext_pointer_cas Thirtytwo (three_args name args) dbg
  | "caml_bigstring_compare_and_swap_int_untagged"
  | "caml_bigstring_compare_and_swap_nativeint_unboxed" ->
    bigstring_cas Word (four_args name args) dbg
  | "caml_bigstring_compare_and_swap_int64_unboxed" when size_int = 8 ->
    bigstring_cas Sixtyfour (four_args name args) dbg
  | "caml_bigstring_compare_and_swap_int32_unboxed" ->
    bigstring_cas Thirtytwo (four_args name args) dbg
  | _ -> transl_vec128_builtin name args dbg typ_res

let extcall ~dbg ~returns ~alloc ~is_c_builtin ~effects ~coeffects ~ty_args name
    typ_res args =
  if not returns then assert (typ_res = typ_void);
  let default =
    Cop
      ( Cextcall
          { func = name;
            ty = typ_res;
            alloc;
            ty_args;
            returns;
            builtin = is_c_builtin;
            effects;
            coeffects
          },
        args,
        dbg )
  in
  if is_c_builtin
  then
    match transl_builtin name args dbg typ_res with
    | Some op -> op
    | None -> default
  else default
