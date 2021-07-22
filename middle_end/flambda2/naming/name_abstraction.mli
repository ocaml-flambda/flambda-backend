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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Consider caching the free names of the whole abstraction
   on each abstraction. *)

module type Term = sig
  include Contains_names.S
  include Contains_ids.S with type t := t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end

type printing_style =
  | Normal
  | Brackets
  | Existential

val set_printing_style : printing_style -> unit

val with_printing_style : printing_style -> f:(unit -> unit) -> unit

module type Common = sig
  type t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end

module Make (Bindable : Bindable.S) (Term : Term) : sig
  (** The type [t] is the equivalent of an atom abstraction construction
      "[--]--" in nominal sets. *)

  include Contains_names.S
  include Contains_ids.S with type t := t
  include Common with type t := t

  val create : Bindable.t -> Term.t -> t

  (** Concretion of an abstraction at a fresh name. *)
  val pattern_match : t -> f:(Bindable.t -> Term.t -> 'a) -> 'a

  (** Concretion of an abstraction at a fresh name followed by reconstruction of
      the abstraction. *)
  val pattern_match_map : t -> f:(Term.t -> Term.t) -> t

  (** Like [pattern_match_map] but also provides the fresh name to [f]. *)
  val pattern_match_mapi : t -> f:(Bindable.t -> Term.t -> Term.t) -> t

  (** Concretion of a pair of abstractions at the same fresh name. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(Bindable.t -> Term.t -> Term.t -> 'a)
    -> 'a
end

module Make_list (Bindable : Bindable.S) (Term : Term) : sig
  (** The type [t] is the equivalent of a "generalised abstraction" construction
      with an ordered list containing disjoint elements, represented as a
      separated product, in binding position. *)

  include Contains_names.S
  include Contains_ids.S with type t := t
  include Common with type t := t

  val create : Bindable.t list -> Term.t -> t

  (** Concretion of an abstraction at fresh names. *)
  val pattern_match : t -> f:(Bindable.t list -> Term.t -> 'a) -> 'a

  (** Concretion of an abstraction at fresh names followed by reconstruction of
      the abstraction. *)
  val pattern_match_map : t -> f:(Term.t -> Term.t) -> t

  (** Like [pattern_match_map] but also provides the fresh names to [f]. *)
  val pattern_match_mapi
     : t
    -> f:(Bindable.t list -> Term.t -> Term.t)
    -> t

  (** Concretion of a pair of abstractions at the same fresh [Bindable]s. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(Bindable.t list -> Term.t -> Term.t -> 'a)
    -> 'a
end

module Make_map (Bindable : Bindable.S) (Term : Term) : sig
  (** Like [Make_list], but the names in binding position are specified by
      the keys of a map, and the natural total ordering on such keys. *)

  include Contains_names.S
  include Common with type t := t

  val create : _ Bindable.Map.t -> Term.t -> t

  (** Concretion of an abstraction at fresh names. *)
  val pattern_match : t -> f:(Term.t -> 'a) -> 'a
end
