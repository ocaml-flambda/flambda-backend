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
  | Closure_approximation of Code_id.t * Function_slot.t * 'code
  | Block_approximation of 'code t array * Alloc_mode.t

val is_unknown : 'a t -> bool

val free_names :
  code_free_names:('code -> Name_occurrences.t) -> 'code t -> Name_occurrences.t
