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
  code_ids_to_never_delete:Code_id.Set.t ->
  specialization_map:Continuation.t Continuation_callsite_map.t ->
  Flow_types.Acc.t ->
  Flow_types.Flow_result.t

(** [true] iff the mutable unboxing pass actually did unbox things *)
val did_perform_mutable_unboxing : Flow_types.Flow_result.t -> bool

(* [true] iff an alias to something useful (e.g. a constant, a symbol with a
   known type) has been added in a loop. More specifically:

   In some cases, the alias analyis can find invariant parameters that are
   aliased to a symbol, and which were not detected during the downwards pass,
   for instance when the parameters are involved in a loop.

   In such cases, it is pertinent to trigger a resimplification of the current
   expression (i.e. function body), so that we can propagate the information
   from that new alias.

   In order to avoid triggering too many resimplifications for no reasons, we
   restrict this process to trigger only when a (non-invariant) parameter of a
   recursive continuation becomes aliased to something that has a non-trivial
   type. *)
val added_useful_alias_in_loop :
  Typing_env.t -> Flow_types.Acc.t -> Flow_types.Flow_result.t -> bool
