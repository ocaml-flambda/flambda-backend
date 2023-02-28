(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Dataflow analysis.

    This module aims mainly at tracking uses of variables (other things may be
    added later on), with the aim of:

    - removing unused parameters of *recursive* continuations;

    - moving allocations out of the hot path of recursive continuations (e.g.
    the allocation of a float that was unboxed by the simplifier). *)

(** Analyze the uses. *)
val analyze :
  ?speculative:bool ->
  ?print_name:string ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  code_age_relation:Code_age_relation.t ->
  used_value_slots:Name_occurrences.t Or_unknown.t ->
  Flow_types.Acc.t ->
  Flow_types.Flow_result.t
