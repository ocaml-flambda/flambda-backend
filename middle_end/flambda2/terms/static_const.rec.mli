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

[@@@ocaml.warning "+a-30-40-41-42"]

(** Language terms that represent statically-allocated values. *)

module Field_of_block : sig
  (** Inhabitants (of kind [Value]) of fields of statically-allocated blocks. *)
  type t =
    | Symbol of Symbol.t  (** The address of the given symbol. *)
    | Tagged_immediate of Targetint_31_63.t  (** The given tagged immediate. *)
    | Dynamically_computed of Variable.t
        (** The value of the given variable. *)

  include Container_types.S with type t := t

  include Contains_names.S with type t := t
end

(** The static structure of a symbol, possibly with holes, ready to be filled
    with values computed at runtime. *)
type t =
  | Code of Code.t
  | Set_of_closures of Set_of_closures.t
  | Block of Tag.Scannable.t * Mutability.t * Field_of_block.t list
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Immutable_float_block of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

type static_const = t

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val is_fully_static : t -> bool

val can_share : t -> bool

val must_be_set_of_closures : t -> Set_of_closures.t

val to_code : t -> Code.t option

val match_against_bound_symbols_pattern :
  t ->
  Bound_symbols.Pattern.t ->
  code:(Code_id.t -> Code.t -> 'a) ->
  set_of_closures:
    (closure_symbols:Symbol.t Closure_id.Lmap.t -> Set_of_closures.t -> 'a) ->
  block_like:(Symbol.t -> t -> 'a) ->
  'a

(* CR mshinwell: This should probably move to its own file. *)
module Group : sig
  type t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val empty : t

  val create : static_const list -> t

  val print : Format.formatter -> t -> unit

  val to_list : t -> static_const list

  val concat : t -> t -> t

  val map : t -> f:(static_const -> static_const) -> t

  val match_against_bound_symbols :
    t ->
    Bound_symbols.t ->
    init:'a ->
    code:('a -> Code_id.t -> Code.t -> 'a) ->
    set_of_closures:
      ('a ->
      closure_symbols:Symbol.t Closure_id.Lmap.t ->
      Set_of_closures.t ->
      'a) ->
    block_like:('a -> Symbol.t -> static_const -> 'a) ->
    'a

  (** This function ignores [Deleted] code. *)
  val pieces_of_code : t -> Code.t Code_id.Map.t

  (** This function ignores [Deleted] code. *)
  val pieces_of_code' : t -> Code.t list

  val is_fully_static : t -> bool
end
