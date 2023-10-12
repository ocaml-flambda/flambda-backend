(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Static constants, equipped with free name information, as rebuilt by the
    simplifier. Definitions of the constants themselves are not kept when not
    rebuilding terms, but some of the metadata is. *)

open! Flambda

type t

type rebuilt_static_const = t

val print : Format.formatter -> t -> unit

val create_code :
  Are_rebuilding_terms.t ->
  params_and_body:Rebuilt_expr.Function_params_and_body.t ->
  free_names_of_params_and_body:Name_occurrences.t ->
  (t * Code.t option) Code_metadata.create_type

(* This function should be used when a [Code.t] is already in hand, e.g. from
   the input term to the simplifier, rather than when one needs to be
   constructed. In the latter case, use [create_code] above, so that [Code]
   values are not constructed unnecessarily. *)
val create_code' : Code.t -> t

val create_set_of_closures : Are_rebuilding_terms.t -> Set_of_closures.t -> t

val create_block :
  Are_rebuilding_terms.t ->
  Tag.Scannable.t ->
  Mutability.t ->
  fields:Field_of_static_block.t list ->
  t

val create_boxed_float :
  Are_rebuilding_terms.t ->
  Numeric_types.Float_by_bit_pattern.t Or_variable.t ->
  t

val create_boxed_int32 : Are_rebuilding_terms.t -> Int32.t Or_variable.t -> t

val create_boxed_int64 : Are_rebuilding_terms.t -> Int64.t Or_variable.t -> t

val create_boxed_nativeint :
  Are_rebuilding_terms.t -> Targetint_32_64.t Or_variable.t -> t

val create_boxed_vec128 :
  Are_rebuilding_terms.t -> Vector_types.Vec128.Bit_pattern.t Or_variable.t -> t

val create_immutable_float_block :
  Are_rebuilding_terms.t ->
  Numeric_types.Float_by_bit_pattern.t Or_variable.t list ->
  t

val create_immutable_float_array :
  Are_rebuilding_terms.t ->
  Numeric_types.Float_by_bit_pattern.t Or_variable.t list ->
  t

val create_immutable_int32_array :
  Are_rebuilding_terms.t -> Int32.t Or_variable.t list -> t

val create_immutable_int64_array :
  Are_rebuilding_terms.t -> Int64.t Or_variable.t list -> t

val create_immutable_nativeint_array :
  Are_rebuilding_terms.t -> Targetint_32_64.t Or_variable.t list -> t

val create_immutable_value_array :
  Are_rebuilding_terms.t -> Field_of_static_block.t list -> t

val create_empty_array : Are_rebuilding_terms.t -> t

val create_mutable_string : Are_rebuilding_terms.t -> initial_value:string -> t

val create_immutable_string : Are_rebuilding_terms.t -> string -> t

val map_set_of_closures : t -> f:(Set_of_closures.t -> Set_of_closures.t) -> t

val free_names : t -> Name_occurrences.t

val is_block : t -> bool

val is_set_of_closures : t -> bool

val is_code : t -> bool

val is_fully_static : t -> bool

val make_code_deleted : t -> if_code_id_is_member_of:Code_id.Set.t -> t

(** This will return [None] if terms are not being rebuilt. *)
val to_const : t -> Static_const_or_code.t option

module Group : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t

  val create : rebuilt_static_const list -> t

  val free_names : t -> Name_occurrences.t

  (** This function may only be used when rebuilding terms (a fatal error will
      be produced otherwise). *)
  val to_named : t -> Named.t

  (** This function returns dummy pieces of code for those not rebuilt. Such
      pieces of code will have all of the correct metadata but a body consisting
      solely of an [Invalid] expression. This seems reasonable because inlining
      is always disabled when in not-rebuilding-terms mode. *)
  val pieces_of_code_including_those_not_rebuilt : t -> Code.t Code_id.Map.t

  (** This function ignores [Deleted] code. *)
  val pieces_of_code_for_cmx : t -> Code.t Code_id.Map.t

  val map : t -> f:(rebuilt_static_const -> rebuilt_static_const) -> t

  val fold_left : t -> init:'a -> f:('a -> rebuilt_static_const -> 'a) -> 'a

  (** [map] and [fold_left] should be used in preference, to avoid allocating
      intermediate lists. *)
  val to_list : t -> rebuilt_static_const list

  val add : rebuilt_static_const -> t -> t

  val concat : t -> t -> t
end
