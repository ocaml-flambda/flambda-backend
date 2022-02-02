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

(** Management of cached free names. *)

type 'descr t =
  { descr : 'descr;
    mutable free_names : Name_occurrences.t option
  }

val create : 'descr -> 'descr t

val descr : 'descr t -> 'descr

val print :
  print_descr:(Format.formatter -> 'descr -> unit) ->
  Format.formatter ->
  'descr t ->
  unit

val apply_renaming :
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
  free_names_descr:('descr -> Name_occurrences.t) ->
  'descr t ->
  Renaming.t ->
  'descr t

val free_names :
  free_names_descr:('descr -> Name_occurrences.t) ->
  'descr t ->
  Name_occurrences.t

val remove_unused_closure_vars_and_shortcut_aliases :
  remove_unused_closure_vars_and_shortcut_aliases_descr:
    ('descr ->
    used_closure_vars:Var_within_closure.Set.t ->
    canonicalise:(Simple.t -> Simple.t) ->
    'descr) ->
  'descr t ->
  used_closure_vars:Var_within_closure.Set.t ->
  canonicalise:(Simple.t -> Simple.t) ->
  'descr t
