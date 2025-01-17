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

open Heterogenous_list

type action

val bind_iterator : 'a option ref -> 'a Trie.Iterator.t -> action

val unless : ('t, 'k, 'v) Table.Id.t -> 't ref -> 'k Option_ref.hlist -> action

type actions

val add_action : actions -> action -> unit

module Order : sig
  type t

  val compare : t -> t -> int

  val parameters : t
end

module Level : sig
  type 'a t

  val print : Format.formatter -> 'a t -> unit

  (** Returns a reference to the current value at this level.

      {b Note}: This reference is set to any new value found prior to executing
      the associated actions, if any, and can thus be used in actions for this
      level or levels of later orders. *)
  val use_output : 'a t -> 'a option ref

  (** Actions to execute immediately after a value is found at this level. *)
  val actions : 'a t -> actions

  val add_iterator : 'a t -> 'a Trie.Iterator.t -> unit

  (** Order of this level. Levels will be iterated over in a nested loop of
      ascending order: if level [order b >= order a], then the loop for [b] is
      nested {b inside} the loop for [a]. *)
  val order : 'a t -> Order.t
end

type context

val create_context : unit -> context

val add_new_level : context -> string -> 'a Level.t

val add_iterator : context -> ('t, 'k, 'v) Table.Id.t -> 'k Trie.Iterator.hlist

val add_naive_binder : context -> ('t, 'k, 'v) Table.Id.t -> 't ref

(** Initial actions are always executed when iterating over a cursor, before
    opening the first level. *)
val initial_actions : context -> actions

type 'v t

type 'a cursor = 'a t

val print : Format.formatter -> 'a t -> unit

val create : context -> 'v Option_ref.hlist -> 'v t

val naive_fold :
  'v t -> Table.Map.t -> ('v Constant.hlist -> 'a -> 'a) -> 'a -> 'a

val naive_iter : 'v t -> Table.Map.t -> ('v Constant.hlist -> unit) -> unit

val seminaive_fold :
  'v t ->
  previous:Table.Map.t ->
  diff:Table.Map.t ->
  current:Table.Map.t ->
  ('v Constant.hlist -> 'a -> 'a) ->
  'a ->
  'a

module With_parameters : sig
  type ('p, 'v) t

  val without_parameters : (nil, 'v) t -> 'v cursor

  val create :
    parameters:'p Option_ref.hlist ->
    context ->
    'v Option_ref.hlist ->
    ('p, 'v) t

  val naive_fold :
    ('p, 'v) t ->
    'p Constant.hlist ->
    Table.Map.t ->
    ('v Constant.hlist -> 'a -> 'a) ->
    'a ->
    'a

  val naive_iter :
    ('p, 'v) t ->
    'p Constant.hlist ->
    Table.Map.t ->
    ('v Constant.hlist -> unit) ->
    unit

  val seminaive_fold :
    ('p, 'v) t ->
    'p Constant.hlist ->
    previous:Table.Map.t ->
    diff:Table.Map.t ->
    current:Table.Map.t ->
    ('v Constant.hlist -> 'a -> 'a) ->
    'a ->
    'a
end
