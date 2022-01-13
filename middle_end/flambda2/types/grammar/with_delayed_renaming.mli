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

(** Management of delayed permutations and cached free names. *)

type 'descr t =
  { mutable descr : 'descr;
    mutable delayed_permutation : Renaming.t;
    mutable free_names : Name_occurrences.t option
  }

val create : 'descr -> 'descr t

val descr :
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
  free_names_descr:('descr -> Name_occurrences.t) ->
  'descr t ->
  'descr

(** [peek_descr] allows access to the underlying description without the current
    permutation being applied. This should only be used when it is certain and
    obvious that the subsequent operations on the returned description do not
    look at any part of the description that involves names. This is a
    performance optimisation. *)
val peek_descr : 'descr t -> 'descr

val print :
  print_descr:(Format.formatter -> 'descr -> unit) ->
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
  free_names_descr:('descr -> Name_occurrences.t) ->
  Format.formatter ->
  'descr t ->
  unit

val apply_renaming : 'descr t -> Renaming.t -> 'descr t

val free_names :
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
  free_names_descr:('descr -> Name_occurrences.t) ->
  'descr t ->
  Name_occurrences.t

val remove_unused_closure_vars :
  apply_renaming_descr:('descr -> Renaming.t -> 'descr) ->
  free_names_descr:('descr -> Name_occurrences.t) ->
  remove_unused_closure_vars_descr:
    ('descr -> used_closure_vars:Var_within_closure.Set.t -> 'descr) ->
  'descr t ->
  used_closure_vars:Var_within_closure.Set.t ->
  'descr t
