(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Transformation of types into structures that can be used immediately to
    build terms. *)

type to_lift = private
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        fields : Simple.t list
      }
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t
  | Boxed_int32 of Numeric_types.Int32.t
  | Boxed_int64 of Numeric_types.Int64.t
  | Boxed_nativeint of Targetint_32_64.t
  | Boxed_vec128 of Vector_types.Vec128.Bit_pattern.t
  | Immutable_float_array of
      { fields : Numeric_types.Float_by_bit_pattern.t list }
  | Immutable_int32_array of { fields : Int32.t list }
  | Immutable_int64_array of { fields : Int64.t list }
  | Immutable_nativeint_array of { fields : Targetint_32_64.t list }
  | Immutable_value_array of { fields : Simple.t list }
  | Empty_array

type reification_result = private
  | Lift of to_lift
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

val reify :
  allowed_if_free_vars_defined_in:Typing_env.t ->
  var_is_defined_at_toplevel:(Variable.t -> bool) ->
  var_is_symbol_projection:(Variable.t -> bool) ->
  Typing_env.t ->
  Type_grammar.t ->
  reification_result
