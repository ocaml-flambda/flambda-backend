(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Language terms that represent statically-allocated values, bound to
    symbols. *)

(* CR mshinwell: Store coercions so the CR in Simplify_static_const can be
   removed. *)

(** The static structure of a symbol, possibly with holes, ready to be filled
    with values computed at runtime. *)
type t = private
  | Set_of_closures of Set_of_closures.t
  | Block of Tag.Scannable.t * Mutability.t * Field_of_static_block.t list
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Boxed_vec128 of Vector_types.Vec128.Bit_pattern.t Or_variable.t
  | Immutable_float_block of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_int32_array of Int32.t Or_variable.t list
  | Immutable_int64_array of Int64.t Or_variable.t list
  | Immutable_nativeint_array of Targetint_32_64.t Or_variable.t list
  | Immutable_value_array of Field_of_static_block.t list
      (** [Immutable_*_array] constructors always have at least one field. For
        empty arrays, [Empty_array] must be used. *)
  | Empty_array of Empty_array_kind.t
      (** [Empty_array] must specify the kind of the empty array.  Arrays of
      unboxed numbers such as int32 and int64 have a slightly different
      representation (currently using custom blocks) from regular arrays of
      values.  This affects all operations, most importantly the computation of
      the length of the array. *)
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val set_of_closures : Set_of_closures.t -> t

val block : Tag.Scannable.t -> Mutability.t -> Field_of_static_block.t list -> t

val boxed_float : Numeric_types.Float_by_bit_pattern.t Or_variable.t -> t

val boxed_int32 : Int32.t Or_variable.t -> t

val boxed_int64 : Int64.t Or_variable.t -> t

val boxed_nativeint : Targetint_32_64.t Or_variable.t -> t

val boxed_vec128 : Vector_types.Vec128.Bit_pattern.t Or_variable.t -> t

val immutable_float_block :
  Numeric_types.Float_by_bit_pattern.t Or_variable.t list -> t

(** This function can accept empty lists of fields; [Empty_array] will be
    produced. *)
val immutable_float_array :
  Numeric_types.Float_by_bit_pattern.t Or_variable.t list -> t

(** This function can accept empty lists of fields; [Empty_array] will be
    produced. *)
val immutable_int32_array : Int32.t Or_variable.t list -> t

(** This function can accept empty lists of fields; [Empty_array] will be
    produced. *)
val immutable_int64_array : Int64.t Or_variable.t list -> t

(** This function can accept empty lists of fields; [Empty_array] will be
    produced. *)
val immutable_nativeint_array : Targetint_32_64.t Or_variable.t list -> t

(** This function can accept empty lists of fields; [Empty_array] will be
    produced. *)
val immutable_value_array : Field_of_static_block.t list -> t

val empty_array : Empty_array_kind.t -> t

val mutable_string : initial_value:string -> t

val immutable_string : string -> t

val is_block : t -> bool

val is_set_of_closures : t -> bool

val is_fully_static : t -> bool

val can_share : t -> bool

val must_be_set_of_closures : t -> Set_of_closures.t

val match_against_bound_static_pattern :
  t ->
  Bound_static.Pattern.t ->
  set_of_closures:
    (closure_symbols:Symbol.t Function_slot.Lmap.t -> Set_of_closures.t -> 'a) ->
  block_like:(Symbol.t -> t -> 'a) ->
  'a
