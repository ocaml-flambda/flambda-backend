(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Remove uses of polymorphic comparison *)

module K = Flambda_kind

type classification_for_printing =
  | Constructive
  | Destructive
  | Neither

module Block_of_values_field = struct
  type t =
    | Any_value
    | Immediate
    | Boxed_float
    | Boxed_int32
    | Boxed_int64
    | Boxed_nativeint

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Any_value -> Format.fprintf ppf "Any_value"
    | Immediate -> Format.fprintf ppf "Immediate"
    | Boxed_float -> Format.fprintf ppf "Boxed_float"
    | Boxed_int32 -> Format.fprintf ppf "Boxed_int32"
    | Boxed_int64 -> Format.fprintf ppf "Boxed_int64"
    | Boxed_nativeint -> Format.fprintf ppf "Boxed_nativeint"

  let compare = Stdlib.compare
end

module Block_kind = struct
  type t =
    | Values of Tag.Scannable.t * Block_of_values_field.t list
    | Naked_floats
  (* There is no equivalent of the dynamic float array checks (c.f.
     [Array_kind.Float_array_opt_dynamic], below) for blocks; it is known at
     compile time whether they are all-float. *)

  let [@ocamlformat "disable"] print ppf t =
   match t with
   | Values (tag, shape) ->
     Format.fprintf ppf
       "@[<hov 1>(Values@ \
         @[<hov 1>(tag %a)@]@ \
         @[<hov 1>(shape@ @[<hov 1>(%a)@])@])@]"
       Tag.Scannable.print tag
       (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Block_of_values_field.print) shape
   | Naked_floats ->
     Format.pp_print_string ppf "Naked_floats"

  let compare t1 t2 =
    match t1, t2 with
    | Values (tag1, shape1), Values (tag2, shape2) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else Misc.Stdlib.List.compare Block_of_values_field.compare shape1 shape2
    | Naked_floats, Naked_floats -> 0
    | Values _, _ -> -1
    | _, Values _ -> 1

  let element_kind t =
    match t with Values _ -> K.value | Naked_floats -> K.naked_float
end

module Array_kind = struct
  type t =
    | Immediates
    | Values
    | Naked_floats
    | Float_array_opt_dynamic

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Immediates -> Format.pp_print_string ppf "Immediates"
    | Naked_floats -> Format.pp_print_string ppf "Naked_floats"
    | Values -> Format.pp_print_string ppf "Values"
    | Float_array_opt_dynamic ->
      Format.pp_print_string ppf "Float_array_opt_dynamic"

  let compare = Stdlib.compare

  let element_kind_for_set t =
    match t with
    | Immediates | Values | Float_array_opt_dynamic -> K.value
    | Naked_floats -> K.naked_float

  let element_kind_for_creation = element_kind_for_set

  let element_kind_for_load = element_kind_for_set

  let to_lambda t : Lambda.array_kind =
    match t with
    | Immediates -> Pintarray
    | Values -> Paddrarray
    | Naked_floats -> Pfloatarray
    | Float_array_opt_dynamic -> Pgenarray
end

module Duplicate_block_kind = struct
  type t =
    | Values of
        { tag : Tag.Scannable.t;
          length : Targetint_31_63.Imm.t
        }
    | Naked_floats of { length : Targetint_31_63.Imm.t }

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Values { tag; length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Block_of_values \
          @[<hov 1>(tag@ %a)@]@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        Tag.Scannable.print tag
        Targetint_31_63.Imm.print length
    | Naked_floats { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Block_of_naked_floats@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        Targetint_31_63.Imm.print length

  let compare t1 t2 =
    match t1, t2 with
    | ( Values { tag = tag1; length = length1 },
        Values { tag = tag2; length = length2 } ) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0 then c else Targetint_31_63.Imm.compare length1 length2
    | Naked_floats { length = length1 }, Naked_floats { length = length2 } ->
      Targetint_31_63.Imm.compare length1 length2
    | Values _, Naked_floats _ -> -1
    | Naked_floats _, Values _ -> 1
end

module Duplicate_array_kind = struct
  type t =
    | Immediates
    | Values
    | Naked_floats of { length : Targetint_31_63.Imm.t option }
    | Float_array_opt_dynamic

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Immediates -> Format.pp_print_string ppf "Immediates"
    | Values -> Format.pp_print_string ppf "Values"
    | Naked_floats { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_floats@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.Imm.print) length
    | Float_array_opt_dynamic ->
      Format.pp_print_string ppf "Float_array_opt_dynamic"

  let compare t1 t2 =
    match t1, t2 with
    | Immediates, Immediates
    | Values, Values
    | Float_array_opt_dynamic, Float_array_opt_dynamic ->
      0
    | Naked_floats { length = length1 }, Naked_floats { length = length2 } ->
      Option.compare Targetint_31_63.Imm.compare length1 length2
    | Immediates, _ -> -1
    | _, Immediates -> 1
    | Values, _ -> -1
    | _, Values -> 1
    | Naked_floats _, _ -> -1
    | _, Naked_floats _ -> 1
end

module Block_access_field_kind = struct
  type t =
    | Any_value
    | Immediate

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Any_value -> Format.pp_print_string ppf "Any_value"
    | Immediate -> Format.pp_print_string ppf "Immediate"

  let compare = Stdlib.compare
end

module Block_access_kind = struct
  type t =
    | Values of
        { tag : Tag.Scannable.t Or_unknown.t;
          size : Targetint_31_63.Imm.t Or_unknown.t;
          field_kind : Block_access_field_kind.t
        }
    | Naked_floats of { size : Targetint_31_63.Imm.t Or_unknown.t }

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Values { tag; size; field_kind; } ->
      Format.fprintf ppf
        "@[<hov 1>(Values@ \
          @[<hov 1>(tag@ %a)@]@ \
          @[<hov 1>(size@ %a)@]@ \
          @[<hov 1>(field_kind@ %a)@]\
          )@]"
        (Or_unknown.print Tag.Scannable.print) tag
        (Or_unknown.print Targetint_31_63.Imm.print) size
        Block_access_field_kind.print field_kind
    | Naked_floats { size; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_floats@ \
          @[<hov 1>(size@ %a)@]\
          )@]"
        (Or_unknown.print Targetint_31_63.Imm.print) size

  let element_kind_for_load t =
    match t with Values _ -> K.value | Naked_floats _ -> K.naked_float

  let element_kind_for_set = element_kind_for_load

  let compare t1 t2 =
    match t1, t2 with
    | ( Values { tag = tag1; size = size1; field_kind = field_kind1 },
        Values { tag = tag2; size = size2; field_kind = field_kind2 } ) ->
      let c = Or_unknown.compare Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else
        let c = Or_unknown.compare Targetint_31_63.Imm.compare size1 size2 in
        if c <> 0
        then c
        else Block_access_field_kind.compare field_kind1 field_kind2
    | Naked_floats { size = size1 }, Naked_floats { size = size2 } ->
      Or_unknown.compare Targetint_31_63.Imm.compare size1 size2
    | Values _, Naked_floats _ -> -1
    | Naked_floats _, Values _ -> 1
end

type string_or_bytes =
  | String
  | Bytes

module Init_or_assign = struct
  type t =
    | Initialization
    | Assignment

  let [@ocamlformat "disable"] print ppf t =
    let fprintf = Format.fprintf in
    match t with
    | Initialization -> fprintf ppf "Init"
    | Assignment -> fprintf ppf "Assign"

  let compare = Stdlib.compare

  let to_lambda t : Lambda.initialization_or_assignment =
    match t with
    | Initialization -> Heap_initialization
    | Assignment -> Assignment
end

type array_like_operation =
  | Reading
  | Writing

let effects_of_operation operation =
  match operation with
  | Reading -> Effects.No_effects
  | Writing -> Effects.Arbitrary_effects

let reading_from_a_block mutable_or_immutable =
  let effects = effects_of_operation Reading in
  let coeffects =
    match (mutable_or_immutable : Mutability.t) with
    | Immutable | Immutable_unique -> Coeffects.No_coeffects
    | Mutable -> Coeffects.Has_coeffects
  in
  effects, coeffects

let reading_from_an_array (array_kind : Array_kind.t)
    (mutable_or_immutable : Mutability.t) =
  let effects : Effects.t =
    match array_kind with
    | Immediates | Values | Naked_floats -> No_effects
    | Float_array_opt_dynamic ->
      (* See [Un_cps_helpers.array_load] and [Cmm_helpers.float_array_ref]. If
         the array (dynamically) has tag [Double_array_tag], then the read will
         allocate. [Immutable] here means that the returned float itself is
         immutable. *)
      Only_generative_effects Immutable
  in
  let coeffects =
    match mutable_or_immutable with
    | Immutable | Immutable_unique -> Coeffects.No_coeffects
    | Mutable -> Coeffects.Has_coeffects
  in
  effects, coeffects

let reading_from_a_string_or_bigstring mutable_or_immutable =
  reading_from_a_block mutable_or_immutable

let writing_to_a_block =
  let effects = effects_of_operation Writing in
  effects, Coeffects.No_coeffects

let writing_to_an_array = writing_to_a_block

let writing_to_bytes_or_bigstring = writing_to_a_block

(* CR mshinwell: Improve naming *)
let bigarray_kind = K.value

let bigstring_kind = K.value

let block_kind = K.value

let array_kind = K.value

let string_or_bytes_kind = K.value

let block_index_kind = K.value

let array_index_kind = K.value

let string_or_bigstring_index_kind = K.value

let bytes_or_bigstring_index_kind = K.value

type 'op comparison_behaviour =
  | Yielding_bool of 'op
  | Yielding_int_like_compare_functions

type comparison =
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge

let print_comparison ppf c =
  let fprintf = Format.fprintf in
  match c with
  | Neq -> fprintf ppf "<>"
  | Eq -> fprintf ppf "="
  | Lt -> fprintf ppf "<"
  | Le -> fprintf ppf "<="
  | Gt -> fprintf ppf ">"
  | Ge -> fprintf ppf ">="

let print_comparison_and_behaviour ppf behaviour =
  match behaviour with
  | Yielding_bool op -> print_comparison ppf op
  | Yielding_int_like_compare_functions ->
    Format.pp_print_string ppf "<compare>"

type signed_or_unsigned =
  | Signed
  | Unsigned

type ordered_comparison =
  | Lt
  | Gt
  | Le
  | Ge

let print_ordered_comparison ppf signedness c =
  let fprintf = Format.fprintf in
  match signedness with
  | Unsigned -> begin
    match c with
    | Lt -> fprintf ppf "<u"
    | Le -> fprintf ppf "<=u"
    | Gt -> fprintf ppf ">u"
    | Ge -> fprintf ppf ">=u"
  end
  | Signed -> begin
    match c with
    | Lt -> fprintf ppf "<"
    | Le -> fprintf ppf "<="
    | Gt -> fprintf ppf ">"
    | Ge -> fprintf ppf ">="
  end

let print_ordered_comparison_and_behaviour ppf signedness behaviour =
  match behaviour with
  | Yielding_bool op -> print_ordered_comparison ppf signedness op
  | Yielding_int_like_compare_functions ->
    let signedness =
      match signedness with Signed -> "signed" | Unsigned -> "unsigned"
    in
    Format.fprintf ppf "<ordered-%s-compare>" signedness

type equality_comparison =
  | Eq
  | Neq

let print_equality_comparison ppf op =
  match op with
  | Eq -> Format.pp_print_string ppf "Eq"
  | Neq -> Format.pp_print_string ppf "Neq"

type bigarray_kind =
  | Float32
  | Float64
  | Sint8
  | Uint8
  | Sint16
  | Uint16
  | Int32
  | Int64
  | Int_width_int
  | Targetint_width_int
  | Complex32
  | Complex64

let element_kind_of_bigarray_kind k =
  match k with
  | Float32 | Float64 -> K.naked_float
  | Sint8 | Uint8 | Sint16 | Uint16 -> K.naked_immediate
  | Int32 -> K.naked_int32
  | Int64 -> K.naked_int64
  | Int_width_int -> K.naked_immediate
  | Targetint_width_int -> K.naked_nativeint
  | Complex32 | Complex64 ->
    (* See [copy_two_doubles] in bigarray_stubs.c. *)
    K.value

let print_bigarray_kind ppf k =
  let fprintf = Format.fprintf in
  match k with
  | Float32 -> fprintf ppf "Float32"
  | Float64 -> fprintf ppf "Float64"
  | Sint8 -> fprintf ppf "Sint8"
  | Uint8 -> fprintf ppf "Uint8"
  | Sint16 -> fprintf ppf "Sint16"
  | Uint16 -> fprintf ppf "Uint16"
  | Int32 -> fprintf ppf "Int32"
  | Int64 -> fprintf ppf "Int64"
  | Int_width_int -> fprintf ppf "Int_width_int"
  | Targetint_width_int -> fprintf ppf "Targetint_width_int"
  | Complex32 -> fprintf ppf "Complex32"
  | Complex64 -> fprintf ppf "Complex64"

type bigarray_layout =
  | C
  | Fortran

let print_bigarray_layout ppf l =
  let fprintf = Format.fprintf in
  match l with C -> fprintf ppf "C" | Fortran -> fprintf ppf "Fortran"

let reading_from_a_bigarray kind =
  match (kind : bigarray_kind) with
  | Complex32 | Complex64 ->
    Effects.Only_generative_effects Immutable, Coeffects.Has_coeffects
  | Float32 | Float64 | Sint8 | Uint8 | Sint16 | Uint16 | Int32 | Int64
  | Int_width_int | Targetint_width_int ->
    Effects.No_effects, Coeffects.Has_coeffects

(* The bound checks are taken care of outside the array primitive (using an
   explicit test and switch in the flambda code, see
   lambda_to_flambda_primitives.ml). *)
let writing_to_a_bigarray kind =
  match (kind : bigarray_kind) with
  | Float32 | Float64 | Sint8 | Uint8 | Sint16 | Uint16 | Int32 | Int64
  | Int_width_int | Targetint_width_int | Complex32
  | Complex64
    (* Technically, the write of a complex generates read of fields from the
       given complex, but since those reads are immutable, there is no
       observable coeffect. *) ->
    Effects.Arbitrary_effects, Coeffects.No_coeffects

let bigarray_index_kind = K.value

type string_like_value =
  | String
  | Bytes
  | Bigstring

let print_string_like_value ppf s =
  match s with
  | String -> Format.pp_print_string ppf "string"
  | Bytes -> Format.pp_print_string ppf "bytes"
  | Bigstring -> Format.pp_print_string ppf "bigstring"

type bytes_like_value =
  | Bytes
  | Bigstring

let print_bytes_like_value ppf b =
  match b with
  | Bytes -> Format.pp_print_string ppf "bytes"
  | Bigstring -> Format.pp_print_string ppf "bigstring"

type string_accessor_width =
  | Eight
  | Sixteen
  | Thirty_two
  | Sixty_four

let print_string_accessor_width ppf w =
  let fprintf = Format.fprintf in
  match w with
  | Eight -> fprintf ppf "8"
  | Sixteen -> fprintf ppf "16"
  | Thirty_two -> fprintf ppf "32"
  | Sixty_four -> fprintf ppf "64"

let byte_width_of_string_accessor_width width =
  match width with
  | Eight -> 1
  | Sixteen -> 2
  | Thirty_two -> 4
  | Sixty_four -> 8

let kind_of_string_accessor_width width =
  match width with
  | Eight | Sixteen -> K.value
  | Thirty_two -> K.naked_int32
  | Sixty_four -> K.naked_int64

type num_dimensions = int

let print_num_dimensions ppf d = Format.fprintf ppf "%d" d

type unary_int_arith_op =
  | Neg
  | Swap_byte_endianness

let print_unary_int_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Neg -> fprintf ppf "~"
  | Swap_byte_endianness -> fprintf ppf "bswap"

type unary_float_arith_op =
  | Abs
  | Neg

let print_unary_float_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with Abs -> fprintf ppf "abs" | Neg -> fprintf ppf "~"

type arg_kinds =
  | Variadic of K.t list
  | Variadic_all_of_kind of K.t

type result_kind =
  | Singleton of K.t
  | Unit

type nullary_primitive =
  | Optimised_out of K.t
  | Probe_is_enabled of { name : string }

let nullary_primitive_eligible_for_cse = function
  | Optimised_out _ | Probe_is_enabled _ -> false

let compare_nullary_primitive p1 p2 =
  match p1, p2 with
  | Optimised_out k1, Optimised_out k2 -> K.compare k1 k2
  | Probe_is_enabled { name = name1 }, Probe_is_enabled { name = name2 } ->
    String.compare name1 name2
  | Optimised_out _, Probe_is_enabled _ -> -1
  | Probe_is_enabled _, Optimised_out _ -> 1

let equal_nullary_primitive p1 p2 = compare_nullary_primitive p1 p2 = 0

let print_nullary_primitive ppf p =
  match p with
  | Optimised_out _ ->
    Format.fprintf ppf "@<0>%sOptimised_out@<0>%s" (Flambda_colours.elide ())
      (Flambda_colours.normal ())
  | Probe_is_enabled { name } ->
    Format.fprintf ppf "@[<hov 1>(Probe_is_enabled@ %s)@]" name

let result_kind_of_nullary_primitive p : result_kind =
  match p with
  | Optimised_out k -> Singleton k
  | Probe_is_enabled _ -> Singleton K.naked_immediate

let effects_and_coeffects_of_nullary_primitive p =
  match p with
  | Optimised_out _ -> Effects.No_effects, Coeffects.No_coeffects
  | Probe_is_enabled _ ->
    (* This doesn't really have effects, but we want to make sure it never gets
       moved around. *)
    Effects.Arbitrary_effects, Coeffects.Has_coeffects

let nullary_classify_for_printing p =
  match p with Optimised_out _ | Probe_is_enabled _ -> Neither

type unary_primitive =
  | Duplicate_block of
      { kind : Duplicate_block_kind.t;
        source_mutability : Mutability.t;
        destination_mutability : Mutability.t
      }
  | Duplicate_array of
      { kind : Duplicate_array_kind.t;
        source_mutability : Mutability.t;
        destination_mutability : Mutability.t
      }
  | Is_int
  | Get_tag
  | Array_length of Array_kind.t
  | Bigarray_length of { dimension : int }
  | String_length of string_or_bytes
  | Int_as_pointer
  | Opaque_identity
  | Int_arith of Flambda_kind.Standard_int.t * unary_int_arith_op
  | Float_arith of unary_float_arith_op
  | Num_conv of
      { src : Flambda_kind.Standard_int_or_float.t;
        dst : Flambda_kind.Standard_int_or_float.t
      }
  | Boolean_not
  | Reinterpret_int64_as_float
  | Unbox_number of Flambda_kind.Boxable_number.t
  | Box_number of Flambda_kind.Boxable_number.t
  | Select_closure of
      { move_from : Closure_id.t;
        move_to : Closure_id.t
      }
  | Project_var of
      { project_from : Closure_id.t;
        var : Var_within_closure.t
      }

(* Here and below, operations that are genuine projections shouldn't be eligible
   for CSE, since we deal with projections through types. *)
let unary_primitive_eligible_for_cse p ~arg =
  match p with
  | Duplicate_array
      { kind = _;
        source_mutability = Immutable;
        destination_mutability = Immutable
      }
  | Duplicate_block
      { kind = _;
        source_mutability = Immutable;
        destination_mutability = Immutable
      } ->
    true
  | Duplicate_array _ | Duplicate_block _ -> false
  | Is_int | Get_tag -> true
  | Array_length _ -> true
  | Bigarray_length _ -> false
  | String_length _ -> true
  | Int_as_pointer -> true
  | Opaque_identity -> false
  | Int_arith _ -> true
  | Float_arith _ -> Flambda_features.float_const_prop ()
  | Num_conv _ | Boolean_not | Reinterpret_int64_as_float -> true
  | Unbox_number _ -> false
  | Box_number _ ->
    (* Boxing of constants will yield values that can be lifted and if needs be
       deduplicated -- so there's no point in adding CSE variables to hold
       them. *)
    Simple.is_var arg
  | Select_closure _ | Project_var _ -> false

let compare_unary_primitive p1 p2 =
  let unary_primitive_numbering p =
    match p with
    | Duplicate_array _ -> 0
    | Duplicate_block _ -> 1
    | Is_int -> 2
    | Get_tag -> 3
    | Array_length _ -> 4
    | Bigarray_length _ -> 5
    | String_length _ -> 6
    | Int_as_pointer -> 7
    | Opaque_identity -> 8
    | Int_arith _ -> 9
    | Float_arith _ -> 10
    | Num_conv _ -> 11
    | Boolean_not -> 12
    | Reinterpret_int64_as_float -> 13
    | Unbox_number _ -> 14
    | Box_number _ -> 15
    | Select_closure _ -> 16
    | Project_var _ -> 17
  in
  match p1, p2 with
  | ( Duplicate_array
        { kind = kind1;
          source_mutability = source_mutability1;
          destination_mutability = destination_mutability1
        },
      Duplicate_array
        { kind = kind2;
          source_mutability = source_mutability2;
          destination_mutability = destination_mutability2
        } ) ->
    let c = Duplicate_array_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare source_mutability1 source_mutability2 in
      if c <> 0
      then c
      else Stdlib.compare destination_mutability1 destination_mutability2
  | ( Duplicate_block
        { kind = kind1;
          source_mutability = source_mutability1;
          destination_mutability = destination_mutability1
        },
      Duplicate_block
        { kind = kind2;
          source_mutability = source_mutability2;
          destination_mutability = destination_mutability2
        } ) ->
    let c = Duplicate_block_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare source_mutability1 source_mutability2 in
      if c <> 0
      then c
      else Stdlib.compare destination_mutability1 destination_mutability2
  | Is_int, Is_int -> 0
  | Get_tag, Get_tag -> 0
  | String_length kind1, String_length kind2 -> Stdlib.compare kind1 kind2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Num_conv { src = src1; dst = dst1 }, Num_conv { src = src2; dst = dst2 } ->
    let c = K.Standard_int_or_float.compare src1 src2 in
    if c <> 0 then c else K.Standard_int_or_float.compare dst1 dst2
  | Float_arith op1, Float_arith op2 -> Stdlib.compare op1 op2
  | Array_length kind1, Array_length kind2 -> Stdlib.compare kind1 kind2
  | Bigarray_length { dimension = dim1 }, Bigarray_length { dimension = dim2 }
    ->
    Stdlib.compare dim1 dim2
  | Unbox_number kind1, Unbox_number kind2 ->
    K.Boxable_number.compare kind1 kind2
  | Box_number kind1, Box_number kind2 -> K.Boxable_number.compare kind1 kind2
  | ( Select_closure { move_from = move_from1; move_to = move_to1 },
      Select_closure { move_from = move_from2; move_to = move_to2 } ) ->
    let c = Closure_id.compare move_from1 move_from2 in
    if c <> 0 then c else Closure_id.compare move_to1 move_to2
  | ( Project_var { project_from = closure_id1; var = var_within_closure1 },
      Project_var { project_from = closure_id2; var = var_within_closure2 } ) ->
    let c = Closure_id.compare closure_id1 closure_id2 in
    if c <> 0
    then c
    else Var_within_closure.compare var_within_closure1 var_within_closure2
  | ( ( Duplicate_array _ | Duplicate_block _ | Is_int | Get_tag
      | String_length _ | Int_as_pointer | Opaque_identity | Int_arith _
      | Num_conv _ | Boolean_not | Reinterpret_int64_as_float | Float_arith _
      | Array_length _ | Bigarray_length _ | Unbox_number _ | Box_number _
      | Select_closure _ | Project_var _ ),
      _ ) ->
    Stdlib.compare (unary_primitive_numbering p1) (unary_primitive_numbering p2)

let equal_unary_primitive p1 p2 = compare_unary_primitive p1 p2 = 0

let print_unary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Duplicate_block { kind; source_mutability; destination_mutability } ->
    fprintf ppf "@[<hov 1>(Duplicate_block %a (source %a) (dest %a))@]"
      Duplicate_block_kind.print kind Mutability.print source_mutability
      Mutability.print destination_mutability
  | Duplicate_array { kind; source_mutability; destination_mutability } ->
    fprintf ppf "@[<hov 1>(Duplicate_array %a (source %a) (dest %a))@]"
      Duplicate_array_kind.print kind Mutability.print source_mutability
      Mutability.print destination_mutability
  | Is_int -> fprintf ppf "Is_int"
  | Get_tag -> fprintf ppf "Get_tag"
  | String_length _ -> fprintf ppf "String_length"
  | Int_as_pointer -> fprintf ppf "Int_as_pointer"
  | Opaque_identity -> fprintf ppf "Opaque_identity"
  | Int_arith (_k, o) -> print_unary_int_arith_op ppf o
  | Num_conv { src; dst } ->
    fprintf ppf "Num_conv_%a_to_%a"
      Flambda_kind.Standard_int_or_float.print_lowercase src
      Flambda_kind.Standard_int_or_float.print_lowercase dst
  | Boolean_not -> fprintf ppf "Boolean_not"
  | Reinterpret_int64_as_float -> fprintf ppf "Reinterpret_int64_as_float"
  | Float_arith o -> print_unary_float_arith_op ppf o
  | Array_length kind ->
    fprintf ppf "@[<hov 1>(Array_length@ %a)@]" Array_kind.print kind
  | Bigarray_length { dimension } ->
    fprintf ppf "Bigarray_length %a" print_num_dimensions dimension
  | Unbox_number Untagged_immediate -> fprintf ppf "Untag_imm"
  | Unbox_number k ->
    fprintf ppf "Unbox_%a" K.Boxable_number.print_lowercase_short k
  | Box_number Untagged_immediate -> fprintf ppf "Tag_imm"
  | Box_number k ->
    fprintf ppf "Box_%a" K.Boxable_number.print_lowercase_short k
  | Select_closure { move_from; move_to } ->
    Format.fprintf ppf "@[(Select_closure@ (%a \u{2192} %a@<0>%s))@]"
      Closure_id.print move_from Closure_id.print move_to
      (Flambda_colours.prim_destructive ())
  | Project_var { project_from; var = var_within_closure } ->
    Format.fprintf ppf "@[(Project_var@ (%a@ %a@<0>%s))@]" Closure_id.print
      project_from Var_within_closure.print var_within_closure
      (Flambda_colours.prim_destructive ())

let arg_kind_of_unary_primitive p =
  match p with
  | Duplicate_array _ | Duplicate_block _ -> K.value
  | Is_int -> K.value
  | Get_tag -> K.value
  | String_length _ -> K.value
  | Int_as_pointer -> K.value
  | Opaque_identity -> K.value
  | Int_arith (kind, _) -> K.Standard_int.to_kind kind
  | Num_conv { src; dst = _ } -> K.Standard_int_or_float.to_kind src
  | Boolean_not -> K.value
  | Reinterpret_int64_as_float -> K.naked_int64
  | Float_arith _ -> K.naked_float
  | Array_length _ | Bigarray_length _ -> K.value
  | Unbox_number _ -> K.value
  | Box_number kind -> K.Boxable_number.to_kind kind
  | Select_closure _ | Project_var _ -> K.value

let result_kind_of_unary_primitive p : result_kind =
  match p with
  | Duplicate_array _ | Duplicate_block _ -> Singleton K.value
  | Is_int | Get_tag -> Singleton K.naked_immediate
  | String_length _ -> Singleton K.naked_immediate
  | Int_as_pointer ->
    (* This primitive is *only* to be used when the resulting pointer points at
       something which is a valid OCaml value (even if outside of the heap). *)
    Singleton K.value
  | Opaque_identity -> Singleton K.value
  | Int_arith (kind, _) -> Singleton (K.Standard_int.to_kind kind)
  | Num_conv { src = _; dst } -> Singleton (K.Standard_int_or_float.to_kind dst)
  | Boolean_not -> Singleton K.value
  | Reinterpret_int64_as_float -> Singleton K.naked_float
  | Float_arith _ -> Singleton K.naked_float
  | Array_length _ -> Singleton K.value
  | Bigarray_length _ -> Singleton K.naked_immediate
  | Unbox_number kind -> Singleton (K.Boxable_number.to_kind kind)
  | Box_number _ | Select_closure _ -> Singleton K.value
  | Project_var _ -> Singleton K.value

let effects_and_coeffects_of_unary_primitive p =
  match p with
  | Duplicate_array { kind = _; source_mutability; destination_mutability; _ }
  | Duplicate_block { kind = _; source_mutability; destination_mutability; _ }
    -> begin
    match source_mutability with
    | Immutable ->
      (* [Obj.truncate] has now been removed. *)
      ( Effects.Only_generative_effects destination_mutability,
        Coeffects.No_coeffects )
    | Immutable_unique ->
      (* XCR vlaviron: this should never occur, but it's hard to express it
         without duplicating the mutability type mshinwell: Adding a second
         mutability type seems like a good thing to avoid confusion in the
         future. It could maybe be a submodule of [Mutability]. *)
      ( Effects.Only_generative_effects destination_mutability,
        Coeffects.No_coeffects )
    | Mutable ->
      ( Effects.Only_generative_effects destination_mutability,
        Coeffects.Has_coeffects )
  end
  | Is_int -> Effects.No_effects, Coeffects.No_coeffects
  | Get_tag ->
    (* [Obj.truncate] has now been removed. *)
    Effects.No_effects, Coeffects.No_coeffects
  | String_length _ ->
    (* CR mshinwell: check this is right. (Even with safe-string off, I don't
       think changing the length of a string is possible.) *)
    Effects.No_effects, Coeffects.No_coeffects
  | Int_as_pointer -> Effects.No_effects, Coeffects.No_coeffects
  | Opaque_identity -> Effects.Arbitrary_effects, Coeffects.Has_coeffects
  | Int_arith (_, (Neg | Swap_byte_endianness))
  | Num_conv _ | Boolean_not | Reinterpret_int64_as_float
  | Float_arith (Abs | Neg) ->
    Effects.No_effects, Coeffects.No_coeffects
  (* Since Obj.truncate has been deprecated, array_length should have no
     observable effect *)
  | Array_length _ -> Effects.No_effects, Coeffects.No_coeffects
  | Bigarray_length { dimension = _ } ->
    (* This is pretty much a direct access to a field of the bigarray, different
       from reading one of the values actually stored inside the array, hence
       [reading_from_a_block] (i.e. this has the same behaviour as a regular
       Block_load). *)
    reading_from_a_block Mutable
  | Unbox_number _ -> Effects.No_effects, Coeffects.No_coeffects
  | Box_number Untagged_immediate -> Effects.No_effects, Coeffects.No_coeffects
  | Box_number _ ->
    Effects.Only_generative_effects Immutable, Coeffects.No_coeffects
  | Select_closure _ | Project_var _ ->
    Effects.No_effects, Coeffects.No_coeffects

let unary_classify_for_printing p =
  match p with
  | Duplicate_array _ | Duplicate_block _ -> Constructive
  | String_length _ | Get_tag -> Destructive
  | Is_int | Int_as_pointer | Opaque_identity | Int_arith _ | Num_conv _
  | Boolean_not | Reinterpret_int64_as_float | Float_arith _ ->
    Neither
  | Array_length _ | Bigarray_length _ | Unbox_number _ -> Destructive
  | Box_number _ -> Constructive
  | Select_closure _ | Project_var _ -> Destructive

type binary_int_arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor

let print_binary_int_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | Mul -> fprintf ppf "*"
  | Div -> fprintf ppf "/"
  | Mod -> fprintf ppf "mod"
  | And -> fprintf ppf "and"
  | Or -> fprintf ppf "or"
  | Xor -> fprintf ppf "xor"

type int_shift_op =
  | Lsl
  | Lsr
  | Asr

let print_int_shift_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Lsl -> fprintf ppf "lsl"
  | Lsr -> fprintf ppf "lsr"
  | Asr -> fprintf ppf "asr"

type binary_float_arith_op =
  | Add
  | Sub
  | Mul
  | Div

let print_binary_float_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Add -> fprintf ppf "+."
  | Sub -> fprintf ppf "-."
  | Mul -> fprintf ppf "*."
  | Div -> fprintf ppf "/."

type binary_primitive =
  | Block_load of Block_access_kind.t * Mutability.t
  | Array_load of Array_kind.t * Mutability.t
  | String_or_bigstring_load of string_like_value * string_accessor_width
  | Bigarray_load of num_dimensions * bigarray_kind * bigarray_layout
  | Phys_equal of Flambda_kind.t * equality_comparison
  | Int_arith of Flambda_kind.Standard_int.t * binary_int_arith_op
  | Int_shift of Flambda_kind.Standard_int.t * int_shift_op
  | Int_comp of
      Flambda_kind.Standard_int.t
      * signed_or_unsigned
      * ordered_comparison comparison_behaviour
  | Float_arith of binary_float_arith_op
  | Float_comp of comparison comparison_behaviour

let binary_primitive_eligible_for_cse p =
  match p with
  | Array_load _ | Block_load _ -> false
  | String_or_bigstring_load _ -> false (* CR mshinwell: review *)
  | Bigarray_load _ -> false
  | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _ -> true
  | Float_arith _ | Float_comp _ ->
    (* We believe that under the IEEE standard it is correct to CSE
       floating-point comparison operations. However we aren't completely sure
       what the situation is with regard to 80-bit precision floating-point
       support on Intel processors (and indeed whether we make use of that). As
       such, we don't CSE these comparisons unless we would also CSE
       floating-point arithmetic operations. *)
    Flambda_features.float_const_prop ()

let compare_binary_primitive p1 p2 =
  let binary_primitive_numbering p =
    match p with
    | Array_load _ -> 0
    | Block_load _ -> 1
    | String_or_bigstring_load _ -> 2
    | Bigarray_load _ -> 3
    | Phys_equal _ -> 4
    | Int_arith _ -> 5
    | Int_shift _ -> 6
    | Int_comp _ -> 7
    | Float_arith _ -> 8
    | Float_comp _ -> 9
  in
  match p1, p2 with
  | Block_load (kind1, mut1), Block_load (kind2, mut2) ->
    let c = Block_access_kind.compare kind1 kind2 in
    if c <> 0 then c else Mutability.compare mut1 mut2
  | Array_load (kind1, mut1), Array_load (kind2, mut2) ->
    let c = Array_kind.compare kind1 kind2 in
    if c <> 0 then c else Mutability.compare mut1 mut2
  | ( String_or_bigstring_load (string_like1, width1),
      String_or_bigstring_load (string_like2, width2) ) ->
    let c = Stdlib.compare string_like1 string_like2 in
    if c <> 0 then c else Stdlib.compare width1 width2
  | ( Bigarray_load (num_dim1, kind1, layout1),
      Bigarray_load (num_dim2, kind2, layout2) ) ->
    let c = Stdlib.compare num_dim1 num_dim2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare kind1 kind2 in
      if c <> 0 then c else Stdlib.compare layout1 layout2
  | Phys_equal (kind1, comp1), Phys_equal (kind2, comp2) ->
    let c = K.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare comp1 comp2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Int_shift (kind1, op1), Int_shift (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Int_comp (kind1, signedness1, comp1), Int_comp (kind2, signedness2, comp2)
    ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare signedness1 signedness2 in
      if c <> 0 then c else Stdlib.compare comp1 comp2
  | Float_arith op1, Float_arith op2 -> Stdlib.compare op1 op2
  | Float_comp comp1, Float_comp comp2 -> Stdlib.compare comp1 comp2
  | ( ( Block_load _ | Array_load _ | String_or_bigstring_load _
      | Bigarray_load _ | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _
      | Float_arith _ | Float_comp _ ),
      _ ) ->
    Stdlib.compare
      (binary_primitive_numbering p1)
      (binary_primitive_numbering p2)

let equal_binary_primitive p1 p2 = compare_binary_primitive p1 p2 = 0

let print_binary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_load (kind, mut) ->
    fprintf ppf "@[(Block_load@ %a@ %a)@]" Block_access_kind.print kind
      Mutability.print mut
  | Array_load (kind, mut) ->
    fprintf ppf "@[(Array_load@ %a@ %a)@]" Array_kind.print kind
      Mutability.print mut
  | String_or_bigstring_load (string_like, width) ->
    fprintf ppf "@[(String_load %a %a)@]" print_string_like_value string_like
      print_string_accessor_width width
  | Bigarray_load (num_dimensions, kind, layout) ->
    fprintf ppf
      "@[(Bigarray_load (num_dimensions@ %d)@ (kind@ %a)@ (layout@ %a))@]"
      num_dimensions print_bigarray_kind kind print_bigarray_layout layout
  | Phys_equal (kind, op) ->
    Format.fprintf ppf "@[(Phys_equal %a %a)@]" K.print kind
      print_equality_comparison op
  | Int_arith (_k, op) -> print_binary_int_arith_op ppf op
  | Int_shift (_k, op) -> print_int_shift_op ppf op
  | Int_comp (_, signedness, c) ->
    print_ordered_comparison_and_behaviour ppf signedness c
  | Float_arith op -> print_binary_float_arith_op ppf op
  | Float_comp c ->
    print_comparison_and_behaviour ppf c;
    fprintf ppf "."

let args_kind_of_binary_primitive p =
  match p with
  | Block_load _ -> block_kind, block_index_kind
  | Array_load _ -> array_kind, array_index_kind
  | String_or_bigstring_load ((String | Bytes), _) ->
    string_or_bytes_kind, string_or_bigstring_index_kind
  | String_or_bigstring_load (Bigstring, _) ->
    bigstring_kind, string_or_bigstring_index_kind
  | Bigarray_load (_, _, _) -> bigarray_kind, bigarray_index_kind
  | Phys_equal (kind, _) -> kind, kind
  | Int_arith (kind, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Int_shift (kind, _) -> K.Standard_int.to_kind kind, K.naked_immediate
  | Int_comp (kind, _, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Float_arith _ | Float_comp _ -> K.naked_float, K.naked_float

let result_kind_of_binary_primitive p : result_kind =
  match p with
  | Block_load (block_access_kind, _) ->
    Singleton (Block_access_kind.element_kind_for_load block_access_kind)
  | Array_load (kind, _) -> Singleton (Array_kind.element_kind_for_load kind)
  | String_or_bigstring_load (_, (Eight | Sixteen)) ->
    Singleton K.naked_immediate
  | String_or_bigstring_load (_, Thirty_two) -> Singleton K.naked_int32
  | String_or_bigstring_load (_, Sixty_four) -> Singleton K.naked_int64
  | Bigarray_load (_, kind, _) -> Singleton (element_kind_of_bigarray_kind kind)
  | Int_arith (kind, _) | Int_shift (kind, _) ->
    Singleton (K.Standard_int.to_kind kind)
  | Float_arith _ -> Singleton K.naked_float
  | Phys_equal _ | Int_comp _ | Float_comp _ -> Singleton K.naked_immediate

let effects_and_coeffects_of_binary_primitive p =
  match p with
  | Block_load (_, mut) -> reading_from_a_block mut
  | Array_load (kind, mut) -> reading_from_an_array kind mut
  | Bigarray_load (_, kind, _) -> reading_from_a_bigarray kind
  | String_or_bigstring_load (String, _) ->
    reading_from_a_string_or_bigstring Immutable
  | String_or_bigstring_load ((Bytes | Bigstring), _) ->
    reading_from_a_string_or_bigstring Mutable
  | Phys_equal _ -> Effects.No_effects, Coeffects.No_coeffects
  | Int_arith (_kind, (Add | Sub | Mul | Div | Mod | And | Or | Xor)) ->
    Effects.No_effects, Coeffects.No_coeffects
  | Int_shift _ -> Effects.No_effects, Coeffects.No_coeffects
  | Int_comp _ -> Effects.No_effects, Coeffects.No_coeffects
  | Float_arith (Add | Sub | Mul | Div) ->
    Effects.No_effects, Coeffects.No_coeffects
  | Float_comp _ -> Effects.No_effects, Coeffects.No_coeffects

let binary_classify_for_printing p =
  match p with
  | Block_load _ | Array_load _ -> Destructive
  | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _ | Float_arith _
  | Float_comp _ | Bigarray_load _ | String_or_bigstring_load _ ->
    Neither

type ternary_primitive =
  | Block_set of Block_access_kind.t * Init_or_assign.t
  | Array_set of Array_kind.t * Init_or_assign.t
  | Bytes_or_bigstring_set of bytes_like_value * string_accessor_width
  | Bigarray_set of num_dimensions * bigarray_kind * bigarray_layout

let ternary_primitive_eligible_for_cse p =
  match p with
  | Block_set _ | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _ ->
    false

let compare_ternary_primitive p1 p2 =
  let ternary_primitive_numbering p =
    match p with
    | Block_set _ -> 0
    | Array_set _ -> 1
    | Bytes_or_bigstring_set _ -> 2
    | Bigarray_set _ -> 3
  in
  match p1, p2 with
  | Block_set (kind1, init_or_assign1), Block_set (kind2, init_or_assign2) ->
    let c = Block_access_kind.compare kind1 kind2 in
    if c <> 0 then c else Init_or_assign.compare init_or_assign1 init_or_assign2
  | Array_set (kind1, init_or_assign1), Array_set (kind2, init_or_assign2) ->
    let c = Array_kind.compare kind1 kind2 in
    if c <> 0 then c else Init_or_assign.compare init_or_assign1 init_or_assign2
  | ( Bytes_or_bigstring_set (kind1, width1),
      Bytes_or_bigstring_set (kind2, width2) ) ->
    let c = Stdlib.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare width1 width2
  | ( Bigarray_set (num_dims1, kind1, layout1),
      Bigarray_set (num_dims2, kind2, layout2) ) ->
    let c = Stdlib.compare num_dims1 num_dims2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare kind1 kind2 in
      if c <> 0 then c else Stdlib.compare layout1 layout2
  | (Block_set _ | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _), _
    ->
    Stdlib.compare
      (ternary_primitive_numbering p1)
      (ternary_primitive_numbering p2)

let equal_ternary_primitive p1 p2 = compare_ternary_primitive p1 p2 = 0

let print_ternary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_set (kind, init) ->
    fprintf ppf "(Block_set %a %a)" Block_access_kind.print kind
      Init_or_assign.print init
  | Array_set (kind, init) ->
    fprintf ppf "(Array_set %a %a)" Array_kind.print kind Init_or_assign.print
      init
  | Bytes_or_bigstring_set (kind, string_accessor_width) ->
    fprintf ppf "(Bytes_set %a %a)" print_bytes_like_value kind
      print_string_accessor_width string_accessor_width
  | Bigarray_set (num_dimensions, kind, layout) ->
    fprintf ppf
      "@[(Bigarray_set (num_dimensions@ %d)@ (kind@ %a)@ (layout@ %a))@]"
      num_dimensions print_bigarray_kind kind print_bigarray_layout layout

let args_kind_of_ternary_primitive p =
  match p with
  | Block_set (access_kind, _) ->
    ( block_kind,
      block_index_kind,
      Block_access_kind.element_kind_for_set access_kind )
  | Array_set (kind, _) ->
    array_kind, array_index_kind, Array_kind.element_kind_for_set kind
  | Bytes_or_bigstring_set (Bytes, (Eight | Sixteen)) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_immediate
  | Bytes_or_bigstring_set (Bytes, Thirty_two) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_int32
  | Bytes_or_bigstring_set (Bytes, Sixty_four) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_int64
  | Bytes_or_bigstring_set (Bigstring, (Eight | Sixteen)) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_immediate
  | Bytes_or_bigstring_set (Bigstring, Thirty_two) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_int32
  | Bytes_or_bigstring_set (Bigstring, Sixty_four) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_int64
  | Bigarray_set (_, kind, _) ->
    bigarray_kind, bigarray_index_kind, element_kind_of_bigarray_kind kind

let result_kind_of_ternary_primitive p : result_kind =
  match p with
  | Block_set _ | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _ ->
    Unit

let effects_and_coeffects_of_ternary_primitive p =
  match p with
  | Block_set _ -> writing_to_a_block
  | Array_set _ -> writing_to_an_array
  | Bytes_or_bigstring_set _ -> writing_to_bytes_or_bigstring
  | Bigarray_set (_, kind, _) -> writing_to_a_bigarray kind

let ternary_classify_for_printing p =
  match p with
  | Block_set _ | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _ ->
    Neither

type variadic_primitive =
  | Make_block of Block_kind.t * Mutability.t
  | Make_array of Array_kind.t * Mutability.t

let variadic_primitive_eligible_for_cse p ~args =
  match p with
  | Make_block (_, Immutable) | Make_array (_, Immutable) ->
    (* See comment in [unary_primitive_eligible_for_cse], above, on [Box_number]
       case. *)
    List.exists (fun arg -> Simple.is_var arg) args
  | Make_block (_, Immutable_unique) | Make_array (_, Immutable_unique) -> false
  | Make_block (_, Mutable) | Make_array (_, Mutable) -> false

let compare_variadic_primitive p1 p2 =
  match p1, p2 with
  | Make_block (kind1, mut1), Make_block (kind2, mut2) ->
    let c = Block_kind.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare mut1 mut2
  | Make_array (kind1, mut1), Make_array (kind2, mut2) ->
    let c = Array_kind.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare mut1 mut2
  | Make_block _, Make_array _ -> -1
  | Make_array _, Make_block _ -> 1

let equal_variadic_primitive p1 p2 = compare_variadic_primitive p1 p2 = 0

let print_variadic_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Make_block (kind, mut) ->
    fprintf ppf "@[<hov 1>(Make_block@ %a@ %a)@]" Block_kind.print kind
      Mutability.print mut
  | Make_array (kind, mut) ->
    fprintf ppf "@[<hov 1>(Make_array@ %a@ %a)@]" Array_kind.print kind
      Mutability.print mut

let args_kind_of_variadic_primitive p : arg_kinds =
  match p with
  | Make_block (kind, _) -> Variadic_all_of_kind (Block_kind.element_kind kind)
  | Make_array (kind, _) ->
    Variadic_all_of_kind (Array_kind.element_kind_for_creation kind)

let result_kind_of_variadic_primitive p : result_kind =
  match p with Make_block _ | Make_array _ -> Singleton K.value

let effects_and_coeffects_of_variadic_primitive p ~args =
  match p with
  | Make_block (_, mut) | Make_array (_, mut) ->
    if List.length args >= 1
    then Effects.Only_generative_effects mut, Coeffects.No_coeffects
    else
      (* zero-sized blocks and arrays are preallocated ("atoms"). *)
      Effects.No_effects, Coeffects.No_coeffects

let variadic_classify_for_printing p =
  match p with Make_block _ | Make_array _ -> Constructive

type t =
  | Nullary of nullary_primitive
  | Unary of unary_primitive * Simple.t
  | Binary of binary_primitive * Simple.t * Simple.t
  | Ternary of ternary_primitive * Simple.t * Simple.t * Simple.t
  | Variadic of variadic_primitive * Simple.t list

type primitive_application = t

let invariant env t =
  let module E = Invariant_env in
  match t with
  | Nullary (Optimised_out _) | Nullary (Probe_is_enabled _) -> ()
  | Unary (prim, x0) -> (
    let kind0 = arg_kind_of_unary_primitive prim in
    E.check_simple_is_bound_and_of_kind env x0 kind0;
    match prim, x0 with
    | Select_closure { move_from; move_to }, closure ->
      E.check_simple_is_bound_and_of_kind env closure K.value;
      E.add_use_of_closure_id env move_from;
      E.add_use_of_closure_id env move_to
    | Project_var { project_from; var }, closure ->
      E.add_use_of_closure_id env project_from;
      E.add_use_of_var_within_closure env var;
      E.check_simple_is_bound_and_of_kind env closure K.value
    | Duplicate_array _, _
    | Duplicate_block _, _
    | Is_int, _
    | Get_tag, _
    | Array_length _, _
    | Bigarray_length _, _
    | String_length _, _
    | Int_as_pointer, _
    | Opaque_identity, _
    | Int_arith _, _
    | Float_arith _, _
    | Num_conv _, _
    | Boolean_not, _
    | Reinterpret_int64_as_float, _
    | Unbox_number _, _
    | Box_number _, _ ->
      ()
    (* None of these contain names. *))
  | Binary (prim, x0, x1) -> (
    let kind0, kind1 = args_kind_of_binary_primitive prim in
    E.check_simple_is_bound_and_of_kind env x0 kind0;
    E.check_simple_is_bound_and_of_kind env x1 kind1;
    match prim with
    (* None of these currently contain names: this is here so that we are
       reminded to check upon adding a new primitive. *)
    | Block_load _ | Array_load _ | String_or_bigstring_load _ | Bigarray_load _
    | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _ | Float_arith _
    | Float_comp _ ->
      ())
  | Ternary (prim, x0, x1, x2) -> (
    let kind0, kind1, kind2 = args_kind_of_ternary_primitive prim in
    E.check_simple_is_bound_and_of_kind env x0 kind0;
    E.check_simple_is_bound_and_of_kind env x1 kind1;
    E.check_simple_is_bound_and_of_kind env x2 kind2;
    match prim with
    | Block_set _ | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _ ->
      ())
  | Variadic (prim, xs) -> (
    let kinds =
      match args_kind_of_variadic_primitive prim with
      | Variadic kinds -> kinds
      | Variadic_all_of_kind kind ->
        List.init (List.length xs) (fun _index -> kind)
    in
    List.iter2
      (fun var kind -> E.check_simple_is_bound_and_of_kind env var kind)
      xs kinds;
    match prim with Make_block _ | Make_array _ -> ())

let classify_for_printing t =
  match t with
  | Nullary prim -> nullary_classify_for_printing prim
  | Unary (prim, _) -> unary_classify_for_printing prim
  | Binary (prim, _, _) -> binary_classify_for_printing prim
  | Ternary (prim, _, _, _) -> ternary_classify_for_printing prim
  | Variadic (prim, _) -> variadic_classify_for_printing prim

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2
    then 0
    else
      let numbering t =
        match t with
        | Nullary _ -> 0
        | Unary _ -> 1
        | Binary _ -> 2
        | Ternary _ -> 3
        | Variadic _ -> 4
      in
      match t1, t2 with
      | Nullary p, Nullary p' -> compare_nullary_primitive p p'
      | Unary (p, s1), Unary (p', s1') ->
        let c = compare_unary_primitive p p' in
        if c <> 0 then c else Simple.compare s1 s1'
      | Binary (p, s1, s2), Binary (p', s1', s2') ->
        let c = compare_binary_primitive p p' in
        if c <> 0
        then c
        else
          let c = Simple.compare s1 s1' in
          if c <> 0 then c else Simple.compare s2 s2'
      | Ternary (p, s1, s2, s3), Ternary (p', s1', s2', s3') ->
        let c = compare_ternary_primitive p p' in
        if c <> 0
        then c
        else
          let c = Simple.compare s1 s1' in
          if c <> 0
          then c
          else
            let c = Simple.compare s2 s2' in
            if c <> 0 then c else Simple.compare s3 s3'
      | Variadic (p, s), Variadic (p', s') ->
        let c = compare_variadic_primitive p p' in
        if c <> 0 then c else Simple.List.compare s s'
      | (Nullary _ | Unary _ | Binary _ | Ternary _ | Variadic _), _ ->
        Stdlib.compare (numbering t1) (numbering t2)

  let equal t1 t2 = compare t1 t2 = 0

  let hash _t = Misc.fatal_error "Not implemented"

  let [@ocamlformat "disable"] print ppf t =
    let colour =
      match classify_for_printing t with
      | Constructive -> Flambda_colours.prim_constructive ()
      | Destructive -> Flambda_colours.prim_destructive ()
      | Neither -> Flambda_colours.prim_neither ()
    in
    match t with
    | Nullary prim ->
      Format.fprintf ppf "@[<hov 1>@<0>%s%a@<0>%s@]"
        colour
        print_nullary_primitive prim
        (Flambda_colours.normal ())
    | Unary (prim, v0) ->
      Format.fprintf ppf "@[<hov 1>(@<0>%s%a@<0>%s@ %a)@]"
        colour
        print_unary_primitive prim
        (Flambda_colours.normal ())
        Simple.print v0
    | Binary (prim, v0, v1) ->
      Format.fprintf ppf "@[<hov 1>(@<0>%s%a@<0>%s@ %a@ %a)@]"
        colour
        print_binary_primitive prim
        (Flambda_colours.normal ())
        Simple.print v0
        Simple.print v1
    | Ternary (prim, v0, v1, v2) ->
      Format.fprintf ppf "@[<hov 1>(@<0>%s%a@<0>%s@ %a@ %a@ %a)@]"
        colour
        print_ternary_primitive prim
        (Flambda_colours.normal ())
        Simple.print v0
        Simple.print v1
        Simple.print v2
    | Variadic (prim, vs) ->
      Format.fprintf ppf "@[<hov 1>(@<0>%s%a@<0>%s@ %a)@]"
        colour
        print_variadic_primitive prim
        (Flambda_colours.normal ())
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Simple.print) vs

  let output chan t = print (Format.formatter_of_out_channel chan) t
end)

let equal t1 t2 = compare t1 t2 = 0

let free_names t =
  match t with
  | Nullary _ -> Name_occurrences.empty
  | Unary (Project_var { var = clos_var; project_from = _ }, x0) ->
    Name_occurrences.add_closure_var (Simple.free_names x0) clos_var
      Name_mode.normal
  | Unary (_prim, x0) -> Simple.free_names x0
  | Binary (_prim, x0, x1) ->
    Name_occurrences.union_list [Simple.free_names x0; Simple.free_names x1]
  | Ternary (_prim, x0, x1, x2) ->
    Name_occurrences.union_list
      [Simple.free_names x0; Simple.free_names x1; Simple.free_names x2]
  | Variadic (_prim, xs) -> Simple.List.free_names xs

let apply_renaming t perm =
  (* CR mshinwell: add phys-equal checks *)
  let apply simple = Simple.apply_renaming simple perm in
  match t with
  | Nullary _ -> t
  | Unary (prim, x0) -> Unary (prim, apply x0)
  | Binary (prim, x0, x1) -> Binary (prim, apply x0, apply x1)
  | Ternary (prim, x0, x1, x2) -> Ternary (prim, apply x0, apply x1, apply x2)
  | Variadic (prim, xs) -> Variadic (prim, Simple.List.apply_renaming xs perm)

let all_ids_for_export t =
  match t with
  | Nullary _ -> Ids_for_export.empty
  | Unary (_prim, x0) -> Ids_for_export.from_simple x0
  | Binary (_prim, x0, x1) ->
    Ids_for_export.add_simple (Ids_for_export.from_simple x0) x1
  | Ternary (_prim, x0, x1, x2) ->
    Ids_for_export.add_simple
      (Ids_for_export.add_simple (Ids_for_export.from_simple x0) x1)
      x2
  | Variadic (_prim, xs) ->
    List.fold_left Ids_for_export.add_simple Ids_for_export.empty xs

let args t =
  match t with
  | Nullary _ -> []
  | Unary (_, x0) -> [x0]
  | Binary (_, x0, x1) -> [x0; x1]
  | Ternary (_, x0, x1, x2) -> [x0; x1; x2]
  | Variadic (_, xs) -> xs

let result_kind (t : t) =
  match t with
  | Nullary prim -> result_kind_of_nullary_primitive prim
  | Unary (prim, _) -> result_kind_of_unary_primitive prim
  | Binary (prim, _, _) -> result_kind_of_binary_primitive prim
  | Ternary (prim, _, _, _) -> result_kind_of_ternary_primitive prim
  | Variadic (prim, _) -> result_kind_of_variadic_primitive prim

let result_kind' t =
  match result_kind t with Singleton kind -> kind | Unit -> K.value

let result_kind_of_nullary_primitive' t =
  match result_kind_of_nullary_primitive t with
  (* CR mshinwell: factor out this mapping *)
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_unary_primitive' t =
  match result_kind_of_unary_primitive t with
  (* CR mshinwell: factor out this mapping *)
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_binary_primitive' t =
  match result_kind_of_binary_primitive t with
  (* CR mshinwell: factor out this mapping *)
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_ternary_primitive' t =
  match result_kind_of_ternary_primitive t with
  (* CR mshinwell: factor out this mapping *)
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_variadic_primitive' t =
  match result_kind_of_variadic_primitive t with
  (* CR mshinwell: factor out this mapping *)
  | Singleton kind -> kind
  | Unit -> K.value

let effects_and_coeffects (t : t) =
  match t with
  | Nullary prim -> effects_and_coeffects_of_nullary_primitive prim
  | Unary (prim, _) -> effects_and_coeffects_of_unary_primitive prim
  | Binary (prim, _, _) -> effects_and_coeffects_of_binary_primitive prim
  | Ternary (prim, _, _, _) -> effects_and_coeffects_of_ternary_primitive prim
  | Variadic (prim, args) ->
    effects_and_coeffects_of_variadic_primitive prim ~args

let no_effects_or_coeffects t =
  match effects_and_coeffects t with
  | No_effects, No_coeffects -> true
  | _, _ -> false

let at_most_generative_effects t =
  match effects_and_coeffects t with
  | (No_effects | Only_generative_effects _), _ -> true
  | _, _ -> false

let only_generative_effects t =
  match effects_and_coeffects t with
  | Only_generative_effects _, _ -> true
  | _, _ -> false

module Eligible_for_cse = struct
  type t = primitive_application

  let create ?map_arg t =
    (* CR mshinwell: Possible way of handling commutativity: for eligible
       primitives, sort the arguments here *)
    let prim_eligible =
      match t with
      | Nullary prim -> nullary_primitive_eligible_for_cse prim
      | Unary (prim, arg) -> unary_primitive_eligible_for_cse prim ~arg
      | Binary (prim, _, _) -> binary_primitive_eligible_for_cse prim
      | Ternary (prim, _, _, _) -> ternary_primitive_eligible_for_cse prim
      | Variadic (prim, args) -> variadic_primitive_eligible_for_cse prim ~args
    in
    let eligible = prim_eligible && List.exists Simple.is_var (args t) in
    let effects_and_coeffects_ok =
      match effects_and_coeffects t with
      | No_effects, No_coeffects -> true
      | Only_generative_effects Immutable, No_coeffects ->
        (* Allow constructions of immutable blocks to be shared. *)
        true
      | _, _ -> false
    in
    if not ((not eligible) || effects_and_coeffects_ok)
    then Misc.fatal_errorf "Eligible_for_cse.create inconsistency: %a" print t;
    if not eligible
    then None
    else
      match map_arg with
      | None -> Some t
      | Some map_arg ->
        let t =
          match t with
          | Nullary _ -> t
          | Unary (prim, arg) ->
            let arg' = map_arg arg in
            if arg == arg' then t else Unary (prim, arg')
          | Binary (prim, arg1, arg2) ->
            let arg1' = map_arg arg1 in
            let arg2' = map_arg arg2 in
            if arg1 == arg1' && arg2 == arg2'
            then t
            else Binary (prim, arg1', arg2')
          | Ternary (prim, arg1, arg2, arg3) ->
            let arg1' = map_arg arg1 in
            let arg2' = map_arg arg2 in
            let arg3' = map_arg arg3 in
            if arg1 == arg1' && arg2 == arg2' && arg3 == arg3'
            then t
            else Ternary (prim, arg1', arg2', arg3')
          | Variadic (prim, args) ->
            let args' = List.map map_arg args in
            if List.for_all2 ( == ) args args' then t else Variadic (prim, args')
        in
        Some t

  let create_exn prim =
    match create prim with
    | Some t -> t
    | None -> Misc.fatal_errorf "Primitive %a not eligible for CSE" print prim

  let create_is_int ~immediate_or_block =
    Unary (Is_int, Simple.name immediate_or_block)

  let create_get_tag ~block = Unary (Get_tag, Simple.name block)

  let eligible t = match create t with None -> false | Some _ -> true

  let to_primitive t = t

  let fold_args t ~init ~f =
    match t with
    | Nullary _ -> init, t
    | Unary (prim, arg) ->
      let acc, arg = f init arg in
      acc, Unary (prim, arg)
    | Binary (prim, arg1, arg2) ->
      let acc, arg1 = f init arg1 in
      let acc, arg2 = f acc arg2 in
      acc, Binary (prim, arg1, arg2)
    | Ternary (prim, arg1, arg2, arg3) ->
      let acc, arg1 = f init arg1 in
      let acc, arg2 = f acc arg2 in
      let acc, arg3 = f acc arg3 in
      acc, Ternary (prim, arg1, arg2, arg3)
    | Variadic (prim, args) ->
      let acc, args =
        List.fold_left
          (fun (acc, args) arg ->
            let acc, arg = f acc arg in
            acc, arg :: args)
          (init, []) args
      in
      acc, Variadic (prim, List.rev args)

  let filter_map_args t ~f =
    match t with
    | Nullary _ -> Some t
    | Unary (prim, arg) -> begin
      match f arg with
      | None -> None
      | Some arg' -> if arg == arg' then Some t else Some (Unary (prim, arg'))
    end
    | Binary (prim, arg1, arg2) -> begin
      match f arg1 with
      | None -> None
      | Some arg1' -> (
        match f arg2 with
        | None -> None
        | Some arg2' ->
          if arg1 == arg1' && arg2 == arg2'
          then Some t
          else Some (Binary (prim, arg1', arg2')))
    end
    | Ternary (prim, arg1, arg2, arg3) -> begin
      match f arg1 with
      | None -> None
      | Some arg1' -> (
        match f arg2 with
        | None -> None
        | Some arg2' -> (
          match f arg3 with
          | None -> None
          | Some arg3' ->
            if arg1 == arg1' && arg2 == arg2' && arg3 == arg3'
            then Some t
            else Some (Ternary (prim, arg1', arg2', arg3'))))
    end
    | Variadic (prim, args) ->
      let args' = List.filter_map f args in
      if List.compare_lengths args args' = 0
      then
        if List.for_all2 ( == ) args args'
        then Some t
        else Some (Variadic (prim, args'))
      else None

  let free_names = free_names

  let apply_renaming = apply_renaming

  include Container_types.Make (struct
    type nonrec t = t

    let compare = compare

    let equal = equal

    let hash = hash

    let print = print

    let output = output
  end)

  let equal t1 t2 = compare t1 t2 = 0
end

let args t =
  match t with
  | Nullary _ -> []
  | Unary (_, arg) -> [arg]
  | Binary (_, arg1, arg2) -> [arg1; arg2]
  | Ternary (_, arg1, arg2, arg3) -> [arg1; arg2; arg3]
  | Variadic (_, args) -> args

module Without_args = struct
  type t =
    | Nullary of nullary_primitive
    | Unary of unary_primitive
    | Binary of binary_primitive
    | Ternary of ternary_primitive
    | Variadic of variadic_primitive

  let [@ocamlformat "disable"] print ppf (t : t) =
    match t with
    | Nullary prim -> print_nullary_primitive ppf prim
    | Unary prim -> print_unary_primitive ppf prim
    | Binary prim -> print_binary_primitive ppf prim
    | Ternary prim -> print_ternary_primitive ppf prim
    | Variadic prim -> print_variadic_primitive ppf prim
end
