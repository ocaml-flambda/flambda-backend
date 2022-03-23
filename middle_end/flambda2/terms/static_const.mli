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

[@@@ocaml.warning "+a-30-40-41-42"]

(** Language terms that represent statically-allocated values, bound to
    symbols. *)

(** The static structure of a symbol, possibly with holes, ready to be filled
    with values computed at runtime. *)
type t =
  | Set_of_closures of Set_of_closures.t
  | Block of Tag.Scannable.t * Mutability.t * Field_of_static_block.t list
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Immutable_float_block of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Empty_array
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

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
