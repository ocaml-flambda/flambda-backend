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

(** "Primitive" operations: those that perform computation but never affect
    control flow.

    Primitives that accept float, int32, int64 or nativeint values always take
    (or return) the unboxed versions.

    No primitive raises an exception. (Bounds checking is handled separately.) *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Block_of_values_field : sig
  type t =
    | Any_value
    | Immediate
    | Boxed_float
    | Boxed_int32
    | Boxed_int64
    | Boxed_nativeint

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit
end

module Block_kind : sig
  type t =
    | Values of Tag.Scannable.t * Block_of_values_field.t list
    | Naked_floats

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Array_kind : sig
  type t =
    | Immediates  (** An array consisting only of immediate values. *)
    | Values
        (** An array consisting of elements of kind [value]. With the float
            array optimisation enabled, such elements must never be [float]s. *)
    | Naked_floats
        (** An array consisting of naked floats, represented using
            [Double_array_tag]. *)
    | Float_array_opt_dynamic
        (** Only used when the float array optimisation is enabled. Arrays of
            this form either consist of elements of kind [value] that are not
            [float]s; or consist entirely of naked floats (represented using
            [Double_array_tag]). *)

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val to_lambda : t -> Lambda.array_kind
end

module Duplicate_block_kind : sig
  type t =
    | Values of { tag : Tag.Scannable.t; length : Targetint_31_63.Imm.t }
    | Naked_floats of { length : Targetint_31_63.Imm.t }

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Duplicate_array_kind : sig
  type t =
    | Immediates
    | Values
    | Naked_floats of { length : Targetint_31_63.Imm.t option }
    | Float_array_opt_dynamic

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Block_access_field_kind : sig
  type t = Any_value | Immediate

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Block_access_kind : sig
  type t =
    | Values of
        { tag : Tag.Scannable.t Or_unknown.t;
          size : Targetint_31_63.Imm.t Or_unknown.t;
          field_kind : Block_access_field_kind.t
        }
    | Naked_floats of { size : Targetint_31_63.Imm.t Or_unknown.t }

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

(* CR-someday mshinwell: We should have unboxed arrays of int32, int64 and
   nativeint. *)

(* CR mshinwell: Some old comments:

   * We should check Pgenarray doesn't occur when the float array optimization
   is disabled.

   * Another note: the "bit test" primitive now needs to be compiled out in
   Lambda_to_flambda. It indexes into a string using a number of bits. (See
   cmmgen.ml) Something that is odd about this primitive is that it does not
   appear to have a bounds check. Maybe it should? *)

type string_or_bytes = String | Bytes

module Init_or_assign : sig
  type t = Initialization | Assignment

  val to_lambda : t -> Lambda.initialization_or_assignment
end

type comparison = Eq | Neq | Lt | Gt | Le | Ge

type ordered_comparison = Lt | Gt | Le | Ge

type equality_comparison = Eq | Neq

type bigarray_kind =
  (* | Unknown *)
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

val element_kind_of_bigarray_kind : bigarray_kind -> Flambda_kind.t

type bigarray_layout = (* Unknown | *) C | Fortran

type string_accessor_width = Eight | Sixteen | Thirty_two | Sixty_four

val kind_of_string_accessor_width : string_accessor_width -> Flambda_kind.t

val byte_width_of_string_accessor_width : string_accessor_width -> int

type string_like_value = String | Bytes | Bigstring

type bytes_like_value = Bytes | Bigstring

type num_dimensions = int

type signed_or_unsigned = Signed | Unsigned

(** Primitive taking exactly zero arguments *)
type nullary_primitive =
  | Optimised_out of Flambda_kind.t
      (** Used for phantom bindings for which there is not enough information
          remaining to build a meaningful value. Can only be used in a phantom
          let-binding. *)
  | Probe_is_enabled of { name : string }
      (** Returns a boolean saying whether the given tracing probe is enabled. *)

(** Untagged binary integer arithmetic operations.

    [Swap_byte_endianness] on a [Tagged_immediate] treats the immediate as
    encoding a 16-bit quantity (described in the least significant 16 bits of
    the immediate after untagging) and exchanges the two halves of the 16-bit
    quantity. The higher-order bits are zeroed. *)
type unary_int_arith_op = Neg | Swap_byte_endianness

(** Naked float unary arithmetic operations. *)
type unary_float_arith_op = Abs | Neg

(** Primitives taking exactly one argument. *)
type unary_primitive =
  | Duplicate_block of
      { kind : Duplicate_block_kind.t;
        source_mutability : Mutability.t;
        destination_mutability : Mutability.t
      }  (** [Duplicate_block] may not be used to change the tag of a block. *)
  | Duplicate_array of
      { kind : Duplicate_array_kind.t;
        source_mutability : Mutability.t;
        destination_mutability : Mutability.t
      }
  | Is_int
  | Get_tag
  | Array_length of Array_kind.t
  | Bigarray_length of { dimension : int }
      (** This primitive is restricted by type-checking to bigarrays that have
          at least the correct number of dimensions. More specifically, they
          come from `%caml_ba_dim_x` primitives (for x=1,2,3), and only exposed
          in the Bigarray.ArrayX modules (incidentally, `dimension` should then
          be one of 1,2,3). *)
  (* CR mshinwell/xclerc: Invariant check: dimension >= 0 *)
  (* CR gbury: Invariant check: 0 < dimension <= 3 *)
  | String_length of string_or_bytes
  (* XCR pchambart: There are 32 and 64 bits swap, that probably need to be
     represented differently mshinwell: I think this should be ok now, please
     check *)
  | Int_as_pointer
  | Opaque_identity
  | Int_arith of Flambda_kind.Standard_int.t * unary_int_arith_op
  | Float_arith of unary_float_arith_op
  | Num_conv of
      { src : Flambda_kind.Standard_int_or_float.t;
        dst : Flambda_kind.Standard_int_or_float.t
      }
  (* CR gbury: check that chains of conversions are not simplified away by
     flambda, particularly those involving an int32. Indeed, a chain of
     conversions int64 -> int32 -> float that would be "simplified" into a int64
     -> float converison would be wrong given that the int64 -> int32 conversion
     needs to introduce a sign-extension, and simplifying it away would remove
     that sign_extension, leading to incorrect overflow behavior *)
  (* CR gbury: add test for this as soon as we can write tests in flambda *)
  | Boolean_not
  (* CR-someday mshinwell: We should maybe change int32.ml and friends to use a
     %-primitive instead of directly calling C stubs for conversions; then we
     could have a single primitive here taking two
     [Flambda_kind.Of_naked_number.t] arguments (one input, one output). *)
  | Reinterpret_int64_as_float
  | Unbox_number of Flambda_kind.Boxable_number.t
  | Box_number of Flambda_kind.Boxable_number.t
  | Select_closure of { move_from : Closure_id.t; move_to : Closure_id.t }
      (** Given the pointer to one closure in some particular set of closures,
          return the pointer to another closure in the same set. *)
  | Project_var of { project_from : Closure_id.t; var : Var_within_closure.t }
      (** Read a value from the environment of a closure. Also specifies the id
          of the closure pointed at in the set of closures given as argument. *)

(** Whether a comparison is to yield a boolean result, as given by a particular
    comparison operator, or whether it is to behave in the manner of "compare"
    functions that yield tagged immediates -1, 0 or 1. *)
type 'op comparison_behaviour =
  | Yielding_bool of 'op
  | Yielding_int_like_compare_functions

(** Binary arithmetic operations on integers. *)
type binary_int_arith_op = Add | Sub | Mul | Div | Mod | And | Or | Xor

(** Shift operations on integers. *)
type int_shift_op = Lsl | Lsr | Asr

(** Naked float binary arithmetic operations. *)
type binary_float_arith_op = Add | Sub | Mul | Div

(** Primitives taking exactly two arguments. *)
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

(** Primitives taking exactly three arguments. *)
type ternary_primitive =
  | Block_set of Block_access_kind.t * Init_or_assign.t
  | Array_set of Array_kind.t * Init_or_assign.t
  | Bytes_or_bigstring_set of bytes_like_value * string_accessor_width
  | Bigarray_set of num_dimensions * bigarray_kind * bigarray_layout

(** Primitives taking zero or more arguments. *)
type variadic_primitive =
  | Make_block of Block_kind.t * Mutability.t
  | Make_array of Array_kind.t * Mutability.t
(* CR mshinwell: Invariant checks -- e.g. that the number of arguments matches
   [num_dimensions] *)

(** The application of a primitive to its arguments. *)
type t =
  | Nullary of nullary_primitive
  | Unary of unary_primitive * Simple.t
  | Binary of binary_primitive * Simple.t * Simple.t
  | Ternary of ternary_primitive * Simple.t * Simple.t * Simple.t
  | Variadic of variadic_primitive * Simple.t list

type primitive_application = t

val invariant : Invariant_env.t -> t -> unit

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val args : t -> Simple.t list

(** Simpler version (e.g. for [Inlining_cost]), where only the actual primitive
    matters, not the arguments. *)
module Without_args : sig
  type t =
    | Nullary of nullary_primitive
    | Unary of unary_primitive
    | Binary of binary_primitive
    | Ternary of ternary_primitive
    | Variadic of variadic_primitive

  val print : Format.formatter -> t -> unit
end

(** A description of the kind of values which a unary primitive expects as its
    arguments. *)
val arg_kind_of_unary_primitive : unary_primitive -> Flambda_kind.t

val args_kind_of_binary_primitive :
  binary_primitive -> Flambda_kind.t * Flambda_kind.t

val args_kind_of_ternary_primitive :
  ternary_primitive -> Flambda_kind.t * Flambda_kind.t * Flambda_kind.t

type arg_kinds =
  | Variadic of Flambda_kind.t list
  | Variadic_all_of_kind of Flambda_kind.t

val args_kind_of_variadic_primitive : variadic_primitive -> arg_kinds

(** A description of the kinds of values (or in the case of [Unit], the actual
    value) which a primitive returns. *)
type result_kind =
  | Singleton of Flambda_kind.t
      (** The primitive returns a single value of the given kind. *)
  | Unit  (** The primitive returns the constant unit value. *)

val result_kind_of_nullary_primitive : nullary_primitive -> result_kind

val result_kind_of_unary_primitive : unary_primitive -> result_kind

val result_kind_of_binary_primitive : binary_primitive -> result_kind

val result_kind_of_ternary_primitive : ternary_primitive -> result_kind

val result_kind_of_variadic_primitive : variadic_primitive -> result_kind

(** Describe the kind of the result of the given primitive. *)
val result_kind : t -> result_kind

(** Like the [result_kind]s, but returns the appropriate [Flambda_kind]. *)
val result_kind_of_nullary_primitive' : nullary_primitive -> Flambda_kind.t

val result_kind_of_unary_primitive' : unary_primitive -> Flambda_kind.t

val result_kind_of_binary_primitive' : binary_primitive -> Flambda_kind.t

val result_kind_of_ternary_primitive' : ternary_primitive -> Flambda_kind.t

val result_kind_of_variadic_primitive' : variadic_primitive -> Flambda_kind.t

val result_kind' : t -> Flambda_kind.t

(** Describe the effects and coeffects that the application of the given
    primitive may have. *)
val effects_and_coeffects : t -> Effects.t * Coeffects.t

(** Returns [true] iff the given primitive has neither effects nor coeffects. *)
val no_effects_or_coeffects : t -> bool

val at_most_generative_effects : t -> bool

(** Returns [true] iff the given primitive has generative effects, and no other
    effects. *)
val only_generative_effects : t -> bool

module Eligible_for_cse : sig
  (** Primitive applications that may be replaced by a variable which is let
      bound to a single instance of such application. Primitives that are
      genuine projections (e.g. [Block_load], etc.) are not eligible, since the
      associated information is propagated through types, not CSE. *)
  type t

  include Contains_names.S with type t := t

  val create :
    ?map_arg:(Simple.t -> Simple.t) -> primitive_application -> t option

  val create_exn : primitive_application -> t

  val create_is_int : immediate_or_block:Name.t -> t

  val create_get_tag : block:Name.t -> t

  val eligible : primitive_application -> bool

  val to_primitive : t -> primitive_application

  val fold_args : t -> init:'a -> f:('a -> Simple.t -> 'a * Simple.t) -> 'a * t

  val filter_map_args : t -> f:(Simple.t -> Simple.t option) -> t option

  (** Total ordering, equality, printing, sets, maps etc. *)
  include Container_types.S with type t := t
end

(** Total ordering, printing, sets, maps etc. *)
include Container_types.S with type t := t

val equal : t -> t -> bool

val equal_nullary_primitive : nullary_primitive -> nullary_primitive -> bool

val equal_unary_primitive : unary_primitive -> unary_primitive -> bool

val equal_binary_primitive : binary_primitive -> binary_primitive -> bool

val equal_ternary_primitive : ternary_primitive -> ternary_primitive -> bool

val equal_variadic_primitive : variadic_primitive -> variadic_primitive -> bool
