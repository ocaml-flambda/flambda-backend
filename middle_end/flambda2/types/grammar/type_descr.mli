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

(** The general description of a type of a particular kind: unknown, bottom,
    equal to a [Simple] (an "alias type") or given by a type head.

    Unknown = "Any value can flow to this point": the top element.

    Bottom = "No value can flow to this point": the least element.

    If you're looking for the full grammar of the Flambda type system please go
    to type_grammar.mli instead. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type 'head t

val print :
  print_head:(Format.formatter -> 'head -> unit) ->
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  Format.formatter ->
  'head t ->
  unit

val create : 'head -> 'head t

val create_equals : Simple.t -> _ t

val bottom : _ t

val unknown : _ t

val is_obviously_bottom : _ t -> bool

val is_obviously_unknown : _ t -> bool

val get_alias_exn :
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  'head t ->
  Simple.t

val apply_coercion :
  apply_coercion_head:('head -> Coercion.t -> 'head Or_bottom.t) ->
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  Coercion.t ->
  'head t ->
  'head t Or_bottom.t

val apply_renaming :
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  'head t ->
  Renaming.t ->
  'head t

val free_names :
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  'head t ->
  Name_occurrences.t

val remove_unused_closure_vars :
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  remove_unused_closure_vars_head:
    ('head -> used_closure_vars:Var_within_closure.Set.t -> 'head) ->
  'head t ->
  used_closure_vars:Var_within_closure.Set.t ->
  'head t

val all_ids_for_export :
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  all_ids_for_export_head:('head -> Ids_for_export.t) ->
  'head t ->
  Ids_for_export.t

module Descr : sig
  type 'head t = private
    | No_alias of 'head
    | Equals of Simple.t

  val print :
    print_head:(Format.formatter -> 'head -> unit) ->
    Format.formatter ->
    'head t ->
    unit

  val apply_renaming :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    'head t ->
    Renaming.t ->
    'head t

  val free_names :
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Name_occurrences.t
end

val descr :
  apply_renaming_head:('head -> Renaming.t -> 'head) ->
  free_names_head:('head -> Name_occurrences.t) ->
  'head t ->
  'head Descr.t Or_unknown_or_bottom.t
