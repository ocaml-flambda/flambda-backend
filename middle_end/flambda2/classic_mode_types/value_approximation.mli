(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Pierre Chambart and Vincent Laviron, OCamlPro               *)
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

(** Approximations used for cross-module inlining in Closure_conversion *)

type 'code t =
  | Value_unknown
  | Value_symbol of Symbol.t
  | Value_const of Reg_width_const.t
  | Closure_approximation of
      { code_id : Code_id.t;
        function_slot : Function_slot.t;
        all_function_slots : Function_slot.Set.t;
        all_value_slots : Value_slot.Set.t;
        code : 'code;
        symbol : Symbol.t option
      }
  | Block_approximation of
      Tag.Scannable.t
      * Flambda_kind.Scannable_block_shape.t
      * 'code t array
      * Alloc_mode.For_types.t

val print : Format.formatter -> 'a t -> unit

val is_unknown : 'a t -> bool

val free_names :
  code_free_names:('code -> Name_occurrences.t) -> 'code t -> Name_occurrences.t
