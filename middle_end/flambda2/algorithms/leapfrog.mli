(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module implements the leapfrog join algorithm from the paper "Leapfrog
    Triejoin: A Simple, Worst-Case Optimal Join Algorithm" by Todd L. Veldhuizen
    (1).

  (1) https://arxiv.org/pdf/1210.0481
*)

(** {2 Iterators} *)

module type Iterator = sig
  (** This module is really an interface for the trie iterators from "Leapfrog
      Triejoin: A Simple, Worst-Case Optimal Join Algorithm" by Todd L.
      Veldhuizen (1)), but because we want {e typed} iterators, we cannot
      implement the [open] and [up] methods from the paper directly.

      Instead, we build one iterator per level of the trie, and use hidden
      references (aka type wormholes) to communicate between the different
      levels of the iterator.

      The [up] function is no longer needed: the iterators at previous depths
      can directly be reused.

      The [open] function is split into two parts, [accept] and [init], which
      interact as follows.

      - [accept] is called on the iterator at depth [d]. This prepares the
        iterator at depth [d + 1] for iteration on the trie currently pointed to
        by the iterator at depth [d].

      - [init] is called on the iterator at depth [d] to start a new
        iteration at depth [d]. Each call to [init] on the iterator at depth [d]
        will iterate on the same set of keys until [accept] is called again on
        the iterator at depth [d - 1].

      There are two special cases:

       - When [accept] on the iterator at depth [d] and [d] is the last depth,
         there is no iterator at depth [d + 1]. Instead, the value associated
         with the current tuple is stored in another hidden reference -- the way
         to access this hidden reference depends on the concrete implementation
         of the [Iterator] signature.

       - There is no iterator at depth [-1], and so there is no way from this
         signature to set a value to iterate on at depth [0]. This is again done
         through explicit setting of yet another hidden reference whose location
         depends on the concrete implementation of the [Iterator] signature.
  *)

  (** ['a t] is the type of iterators with keys of type ['a]. The type of values
      is hidden. *)
  type 'a t

  (** [current it] is the key at the current position of the iterator [it], or
      [None] if the iterator is exhausted. *)
  val current : 'a t -> 'a option

  (** [advance it] advances the iterator to the next key.

      [advance] is a no-op on an exhausted iterator. *)
  val advance : 'a t -> unit

  (** [seek it x] moves the iterator to the least upper bound for [x],
      i.e. the least key [y] such that [y >= x], exhausting the iterator if no
      such key exists.

      {b Note}: Only keys greater than the current position are considered:
      [seek] does nothing if [x] is smaller than the current key. *)
  val seek : 'a t -> 'a -> unit

  (** [init it] resets [it] to the first key of a new iteration. *)
  val init : 'a t -> unit

  (** [accept it] accepts the value associated with the current key, preparing
      the iterator at the next depth for iteration.

      @raise Invalid_argument if [it] is exhausted. *)
  val accept : 'a t -> unit

  (** [equal_key it] is an equality function iterator keys. *)
  val equal_key : 'a t -> 'a -> 'a -> bool

  (** [equal_key it] is a comparison function on iterator keys. *)
  val compare_key : 'a t -> 'a -> 'a -> int
end

module Map (T : Container_types.S_plus_iterator) : sig
  include Iterator

  (** [create cell handler] returns a new imperative iterator that iterates over
      the keys of the map in [cell].

       - When calling [init], the iterator is initialised with the lowest key of
         the map currently in the [cell] reference.

       - Calling [accept] will set the [handler] reference to the current value
         of the iterator. *)
  val create : 'a T.Map.t Channel.receiver -> 'a Channel.sender -> T.t t
end

module Join (Iterator : Iterator) : sig
  include Iterator

  (** [create iterators] returns a new imperative iterator that iterates over
      the join of the keys in [iterators] using the leapfrog join algorithm. The
      keys of the returned iterator are those that are present in {b all} of the
      iterators in [iterators].

      The trie navigation operations [init] and [accept] are delegated to the
      underlying iterators.

      @raise Invalid_argument if [iterators] is empty.
  *)
  val create : 'a Iterator.t list -> 'a t
end
