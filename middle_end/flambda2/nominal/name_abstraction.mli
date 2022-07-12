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

module type Term = sig
  type t

  val apply_renaming : t -> Renaming.t -> t

  include Contains_ids.S with type t := t
end

type ('bindable, 'term) t

module Make (Bindable : Bindable.S) (Term : Term) : sig
  (** The type [t] is the equivalent of an atom abstraction construction
      "[--]--" in nominal sets. *)
  type nonrec t = (Bindable.t, Term.t) t

  include Contains_ids.S with type t := t

  val create : Bindable.t -> Term.t -> t

  (** Concretion of an abstraction at a fresh name. *)
  val pattern_match : t -> f:(Bindable.t -> Term.t -> 'a) -> 'a

  (** Like [pattern_match], but only freshen if
      [Flambda_features.freshen_when_printing] is enabled. *)
  val pattern_match_for_printing : t -> f:(Bindable.t -> Term.t -> 'a) -> 'a

  (** Concretion of a pair of abstractions at the same fresh name. *)
  val pattern_match_pair :
    t -> t -> f:(Bindable.t -> Term.t -> Term.t -> 'a) -> 'a

  (** Same as [pattern_match]. *)
  val ( let<> ) : t -> (Bindable.t * Term.t -> 'a) -> 'a

  val apply_renaming : t -> Renaming.t -> t
end

(*_ One-off versions of the functions, in the form most convenient to
  [Flambda2_terms.Flambda] *)

val apply_renaming :
  (module Bindable.S with type t = 'bindable) ->
  ('bindable, 'term) t ->
  Renaming.t ->
  apply_renaming_to_term:('term -> Renaming.t -> 'term) ->
  ('bindable, 'term) t

val pattern_match :
  (module Bindable.S with type t = 'bindable) ->
  ('bindable, 'term) t ->
  apply_renaming_to_term:('term -> Renaming.t -> 'term) ->
  f:('bindable -> 'term -> 'a) ->
  'a

val pattern_match_for_printing :
  (module Bindable.S with type t = 'bindable) ->
  ('bindable, 'term) t ->
  apply_renaming_to_term:('term -> Renaming.t -> 'term) ->
  f:('bindable -> 'term -> 'a) ->
  'a

val free_names :
  (module Bindable.S with type t = 'bindable) ->
  ('bindable, 'term) t ->
  free_names_of_term:('term -> Name_occurrences.t) ->
  Name_occurrences.t

val ids_for_export :
  (module Bindable.S with type t = 'bindable) ->
  ('bindable, 'term) t ->
  ids_for_export_of_term:('term -> Ids_for_export.t) ->
  Ids_for_export.t
