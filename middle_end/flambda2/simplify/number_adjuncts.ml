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

open! Flambda.Import
module K = Flambda_kind
module T = Flambda2_types
module Float_by_bit_pattern = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64

(* CR vlaviron: I'm not sure what bit width we should use for naked immediates.
   My intuition is that we should not do arbitrary arithmetic operations on
   naked immediates at all, so no option is more correct than the other.

   mshinwell: the byte-swapping semantics is another case where this is weird
   (in addition to the bit width question, see below) -- we have to avoid a sign
   extension because that wouldn't match the expected semantics on tagged
   immediates. Maybe we should check where we are doing arithmetic on naked
   immediates and rework that to operate on tagged values instead, or
   somesuch. *)

(* Regarding int <-> float conversions, xclerc and mshinwell established with
   reasonable certainty that the OCaml backend (at least on x86-64) emits the
   same instructions as C compilers use for compiling the runtime float
   conversion functions. This means that, even though the results of certain of
   these conversions are undefined by the C standard (e.g. when NaN is
   involved), the semantics of the Flambda 2 compile-time evaluation should
   always match that of runtime evaluation by the processor. *)

(* CR mshinwell: We should try to improve the testing story so properties such
   as the int <-> float conversions mentioned above can be machine-checked for
   greater certainty. *)

module type Num_common = sig
  include Container_types.S

  module Pair : sig
    type nonrec t = t * t

    include Container_types.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

  val zero : t

  val one : t

  val minus_one : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t option

  val mod_ : t -> t -> t option

  val to_const : t -> Reg_width_const.t

  val to_immediate : t -> Targetint_31_63.t

  val to_naked_float : t -> Numeric_types.Float_by_bit_pattern.t

  val to_naked_int32 : t -> Numeric_types.Int32.t

  val to_naked_int64 : t -> Numeric_types.Int64.t

  val to_naked_nativeint : t -> Targetint_32_64.t
end

module type Number_kind_common = sig
  module Num : Container_types.S

  val standard_int_or_float_kind : K.Standard_int_or_float.t

  val unboxed_prover :
    Flambda2_types.Typing_env.t ->
    Flambda2_types.t ->
    Num.Set.t Flambda2_types.meet_shortcut

  val this_unboxed : Num.t -> Flambda2_types.t

  val these_unboxed : Num.Set.t -> Flambda2_types.t

  val term_unboxed : Num.t -> Flambda.Named.t
end

module type Number_kind = sig
  module Num : Num_common

  include Number_kind_common with module Num := Num
end

module type Int_number_kind = sig
  module Num : sig
    include Num_common

    val and_ : t -> t -> t

    val or_ : t -> t -> t

    val xor : t -> t -> t

    val shift_left : t -> Targetint_31_63.t -> t

    val shift_right : t -> Targetint_31_63.t -> t

    val shift_right_logical : t -> Targetint_31_63.t -> t

    val swap_byte_endianness : t -> t

    val neg : t -> t

    val compare_unsigned : t -> t -> int
  end

  include Number_kind_common with module Num := Num

  val standard_int_kind : K.Standard_int.t
end

module type Boxable = sig
  module Num : Container_types.S

  val boxable_number_kind : K.Boxable_number.t

  val this_boxed : Num.t -> Alloc_mode.For_types.t -> Flambda2_types.t

  val these_boxed : Num.Set.t -> Alloc_mode.For_types.t -> Flambda2_types.t

  val box : Flambda2_types.t -> Alloc_mode.For_types.t -> Flambda2_types.t
end

module type Boxable_number_kind = sig
  include Number_kind

  include Boxable with module Num := Num
end

module type Boxable_int_number_kind = sig
  include Int_number_kind

  include Boxable with module Num := Num
end

let with_shift shift if_undefined f ~integer_bit_width =
  match Targetint_31_63.to_int_option shift with
  | None ->
    (* As per a similar case in [Simplify_binary_primitive], we are here
       assigning a semantics to an operation which has undefined semantics. *)
    if_undefined
  | Some shift ->
    if shift < 0 || shift >= integer_bit_width then if_undefined else f shift

let compare_unsigned_generic n1 n2 ~compare ~strictly_negative =
  (* CR-someday mshinwell: Use faster implementation and/or implementation in
     the stdlib when available. *)
  (* This relies on the two's complement encoding, take care! *)
  match strictly_negative n1, strictly_negative n2 with
  | true, true -> compare n1 n2
  | true, false -> 1
  | false, true -> -1
  | false, false -> compare n1 n2

module For_tagged_immediates : Int_number_kind = struct
  module Num = struct
    include Targetint_31_63

    let strictly_negative t = t < zero

    let compare_unsigned t1 t2 =
      compare_unsigned_generic t1 t2 ~compare ~strictly_negative

    let div t1 t2 =
      if Targetint_31_63.equal t2 Targetint_31_63.zero
      then None
      else Some (div t1 t2)

    let mod_ t1 t2 =
      if Targetint_31_63.equal t2 Targetint_31_63.zero
      then None
      else Some (mod_ t1 t2)

    (* Note this doesn't say 31 and 63! See the comments on the shift operations
       e.g. [lsl] in stdlib.mli. *)
    let integer_bit_width = if Target_system.is_32_bit () then 32 else 64

    let shift_left t shift =
      with_shift shift zero (fun shift -> shift_left t shift) ~integer_bit_width

    let shift_right t shift =
      with_shift shift zero
        (fun shift -> shift_right t shift)
        ~integer_bit_width

    let shift_right_logical t shift =
      with_shift shift zero
        (fun shift -> shift_right_logical t shift)
        ~integer_bit_width

    let swap_byte_endianness =
      Targetint_31_63.get_least_significant_16_bits_then_byte_swap

    let to_const t = Reg_width_const.tagged_immediate t

    let to_immediate t = t

    let to_naked_float t =
      Float_by_bit_pattern.create (Targetint_31_63.to_float t)

    let to_naked_int32 t = Targetint_31_63.to_int32 t

    let to_naked_int64 t = Targetint_31_63.to_int64 t

    let to_naked_nativeint t = Targetint_31_63.to_targetint t
  end

  let standard_int_or_float_kind : K.Standard_int_or_float.t = Tagged_immediate

  let standard_int_kind : K.Standard_int.t = Tagged_immediate

  let unboxed_prover = T.meet_equals_tagged_immediates

  let this_unboxed = T.this_tagged_immediate

  let these_unboxed = T.these_tagged_immediates

  let term_unboxed imm =
    Named.create_simple (Simple.const (Reg_width_const.tagged_immediate imm))
end

module For_naked_immediates : Int_number_kind = struct
  module Num = struct
    include Targetint_31_63

    let strictly_negative t = t < zero

    let compare_unsigned t1 t2 =
      compare_unsigned_generic t1 t2 ~compare ~strictly_negative

    let div t1 t2 =
      if Targetint_31_63.equal t2 Targetint_31_63.zero
      then None
      else Some (div t1 t2)

    let mod_ t1 t2 =
      if Targetint_31_63.equal t2 Targetint_31_63.zero
      then None
      else Some (mod_ t1 t2)

    let integer_bit_width = if Target_system.is_32_bit () then 31 else 63

    let shift_left t shift =
      with_shift shift zero (fun shift -> shift_left t shift) ~integer_bit_width

    let shift_right t shift =
      with_shift shift zero
        (fun shift -> shift_right t shift)
        ~integer_bit_width

    let shift_right_logical t shift =
      with_shift shift zero
        (fun shift -> shift_right_logical t shift)
        ~integer_bit_width

    let swap_byte_endianness =
      Targetint_31_63.get_least_significant_16_bits_then_byte_swap

    let to_const t = Reg_width_const.naked_immediate t

    let to_immediate t = t

    let to_naked_float t =
      Float_by_bit_pattern.create (Targetint_31_63.to_float t)

    let to_naked_int32 = Targetint_31_63.to_int32

    let to_naked_int64 = Targetint_31_63.to_int64

    let to_naked_nativeint = Targetint_31_63.to_targetint
  end

  let standard_int_or_float_kind : K.Standard_int_or_float.t = Naked_immediate

  let standard_int_kind : K.Standard_int.t = Naked_immediate

  let unboxed_prover = T.meet_naked_immediates

  let this_unboxed = T.this_naked_immediate

  let these_unboxed = T.these_naked_immediates

  let term_unboxed imm =
    Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm))
end

module For_floats : Boxable_number_kind = struct
  module Num = struct
    include Float_by_bit_pattern

    let add = IEEE_semantics.add

    let sub = IEEE_semantics.sub

    let mul = IEEE_semantics.mul

    let div t1 t2 = Some (IEEE_semantics.div t1 t2)

    let mod_ t1 t2 = Some (IEEE_semantics.mod_ t1 t2)

    let to_const t = Reg_width_const.naked_float t

    let to_immediate t = Targetint_31_63.of_float (to_float t)

    let to_naked_float t = t

    let to_naked_int32 t = Int32.of_float (to_float t)

    let to_naked_int64 t = Int64.of_float (to_float t)

    let to_naked_nativeint t = Targetint_32_64.of_float (to_float t)
  end

  let standard_int_or_float_kind : K.Standard_int_or_float.t = Naked_float

  let boxable_number_kind = K.Boxable_number.Naked_float

  let unboxed_prover = T.meet_naked_floats

  let this_unboxed = T.this_naked_float

  let these_unboxed = T.these_naked_floats

  let this_boxed = T.this_boxed_float

  let these_boxed = T.these_boxed_floats

  let box = T.box_float

  let term_unboxed f =
    Named.create_simple (Simple.const (Reg_width_const.naked_float f))
end

module For_int32s : Boxable_int_number_kind = struct
  module Num = struct
    include Int32

    let strictly_negative t = compare t zero < 0

    let compare_unsigned t1 t2 =
      compare_unsigned_generic t1 t2 ~compare ~strictly_negative

    let xor = logxor

    let or_ = logor

    let and_ = logand

    let div t1 t2 = if equal t2 zero then None else Some (div t1 t2)

    let mod_ t1 t2 = if equal t2 zero then None else Some (rem t1 t2)

    let shift_left t shift =
      with_shift shift zero
        (fun shift -> shift_left t shift)
        ~integer_bit_width:32

    let shift_right t shift =
      with_shift shift zero
        (fun shift -> shift_right t shift)
        ~integer_bit_width:32

    let shift_right_logical t shift =
      with_shift shift zero
        (fun shift -> shift_right_logical t shift)
        ~integer_bit_width:32

    let to_const t = Reg_width_const.naked_int32 t

    let to_immediate t = Targetint_31_63.of_int32 t

    let to_naked_float t = Float_by_bit_pattern.create (Int32.to_float t)

    let to_naked_int32 t = t

    let to_naked_int64 t = Int64.of_int32 t

    let to_naked_nativeint t = Targetint_32_64.of_int32 t
  end

  let standard_int_or_float_kind : K.Standard_int_or_float.t = Naked_int32

  let standard_int_kind : K.Standard_int.t = Naked_int32

  let boxable_number_kind = K.Boxable_number.Naked_int32

  let unboxed_prover = T.meet_naked_int32s

  let this_unboxed = T.this_naked_int32

  let these_unboxed = T.these_naked_int32s

  let this_boxed = T.this_boxed_int32

  let these_boxed = T.these_boxed_int32s

  let box = T.box_int32

  let term_unboxed i =
    Named.create_simple (Simple.const (Reg_width_const.naked_int32 i))
end

module For_int64s : Boxable_int_number_kind = struct
  module Num = struct
    include Int64

    let strictly_negative t = compare t zero < 0

    let compare_unsigned t1 t2 =
      compare_unsigned_generic t1 t2 ~compare ~strictly_negative

    let xor = logxor

    let or_ = logor

    let and_ = logand

    let div t1 t2 = if equal t2 zero then None else Some (div t1 t2)

    let mod_ t1 t2 = if equal t2 zero then None else Some (rem t1 t2)

    let shift_left t shift =
      with_shift shift zero
        (fun shift -> shift_left t shift)
        ~integer_bit_width:64

    let shift_right t shift =
      with_shift shift zero
        (fun shift -> shift_right t shift)
        ~integer_bit_width:64

    let shift_right_logical t shift =
      with_shift shift zero
        (fun shift -> shift_right_logical t shift)
        ~integer_bit_width:64

    let to_const t = Reg_width_const.naked_int64 t

    let to_immediate t = Targetint_31_63.of_int64 t

    let to_naked_float t = Float_by_bit_pattern.create (Int64.to_float t)

    let to_naked_int32 t = Int64.to_int32 t

    let to_naked_int64 t = t

    let to_naked_nativeint t = Targetint_32_64.of_int64 t
  end

  let standard_int_or_float_kind : K.Standard_int_or_float.t = Naked_int64

  let standard_int_kind : K.Standard_int.t = Naked_int64

  let boxable_number_kind = K.Boxable_number.Naked_int64

  let unboxed_prover = T.meet_naked_int64s

  let this_unboxed = T.this_naked_int64

  let these_unboxed = T.these_naked_int64s

  let this_boxed = T.this_boxed_int64

  let these_boxed = T.these_boxed_int64s

  let box = T.box_int64

  let term_unboxed i =
    Named.create_simple (Simple.const (Reg_width_const.naked_int64 i))
end

module For_nativeints : Boxable_int_number_kind = struct
  module Num = struct
    include Targetint_32_64

    let strictly_negative t = compare t zero < 0

    let compare_unsigned t1 t2 =
      compare_unsigned_generic t1 t2 ~compare ~strictly_negative

    let xor = logxor

    let or_ = logor

    let and_ = logand

    let div t1 t2 = if equal t2 zero then None else Some (div t1 t2)

    let mod_ t1 t2 = if equal t2 zero then None else Some (rem t1 t2)

    let integer_bit_width = if Target_system.is_32_bit () then 32 else 64

    let shift_left t shift =
      with_shift shift zero (fun shift -> shift_left t shift) ~integer_bit_width

    let shift_right t shift =
      with_shift shift zero
        (fun shift -> shift_right t shift)
        ~integer_bit_width

    let shift_right_logical t shift =
      with_shift shift zero
        (fun shift -> shift_right_logical t shift)
        ~integer_bit_width

    let to_const t = Reg_width_const.naked_nativeint t

    let to_immediate t = Targetint_31_63.of_targetint t

    let to_naked_float t =
      Float_by_bit_pattern.create (Targetint_32_64.to_float t)

    let to_naked_int32 t = Targetint_32_64.to_int32 t

    let to_naked_int64 t = Targetint_32_64.to_int64 t

    let to_naked_nativeint t = t
  end

  let standard_int_or_float_kind : K.Standard_int_or_float.t = Naked_nativeint

  let standard_int_kind : K.Standard_int.t = Naked_nativeint

  let boxable_number_kind = K.Boxable_number.Naked_nativeint

  let unboxed_prover = T.meet_naked_nativeints

  let this_unboxed = T.this_naked_nativeint

  let these_unboxed = T.these_naked_nativeints

  let this_boxed = T.this_boxed_nativeint

  let these_boxed = T.these_boxed_nativeints

  let box = T.box_nativeint

  let term_unboxed i =
    Named.create_simple (Simple.const (Reg_width_const.naked_nativeint i))
end
