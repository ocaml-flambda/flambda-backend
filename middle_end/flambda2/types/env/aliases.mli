(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Union-find-like structure for keeping track of equivalence classes, used for
    alias resolution in the typing environment, with support for associating
    orderings to aliases of canonical elements.

    The concept of "alias" needs to be broadened where coercions are involved. A
    coercion is a sort of fudge factor---if [x] is equal to [(coerce y c)], then
    [x] and [y] are {i essentially} equal, close enough that we want to think of
    [y] as an alias of [x].

    If the words "fudge factor" sound like we're being imprecise, happily we can
    be precise about our imprecision. Let [x ~ y] mean [x] and [y] are _equal up
    to coercion_, which is to say, there exists a coercion [c] such that [x =
    (coerce y c)]. Coercions form a _groupoid_, meaning they have just the right
    properties to let [~] be an equivalence relation: {| + There is an identity
    coercion, so [x = (coerce x id)], meaning [x ~ x].

    + Coercions can be inverted, so if [x ~ y], meaning [x = (coerce y c)], then
    (writing [c^-1] for the inverse) we have [y = (coerce x c^-1)], meaning [y ~
    x].

    + Coercions can be composed, so if [x ~ y] and [y ~ z], meaning [x = (coerce
    y c_xy)] and [y = (coerce z c_yz)], then (using [>>] as the composition
    operator) we have [x = (coerce z (c_xy >> c_yz))] and [x ~ z]. |} Therefore
    we can safely redefine "alias" to mean [x ~ y] rather than [x = y], and the
    coercions keep track of the precise sense in which [x] and [y] are "equal
    enough." In particular, this module keeps track of aliases in this looser
    sense. *)

type t

val print : Format.formatter -> t -> unit

(** Functions taking [binding_time_resolver] can raise exceptions from that
    resolver. *)

val invariant :
  binding_time_resolver:(Name.t -> Binding_time.With_name_mode.t) ->
  binding_times_and_modes:(_ * Binding_time.With_name_mode.t) Name.Map.t ->
  t ->
  unit

val empty : t

val is_empty : t -> bool

(** The result of calling [add] to state that two [Simple.t]s are now aliases. *)
type add_result = private
  { t : t;  (** The new state of the alias tracker. *)
    canonical_element : Simple.t;
        (** The canonical element of the combined equivalence class. In the type
            environment, this will be the name (if it is a name) that is
            assigned a concrete type. Does not carry a coercion. *)
    alias_of_demoted_element : Simple.t
        (** Whichever argument to [add] had its equivalence class consumed and
            its canonical element demoted to an alias. It is this name that
            needs its type to change to record the new canonical element. Its
            coercion has been adjusted so that it is properly an alias of
            [canonical_element]. *)
  }

(** Add an alias relationship to the tracker. The two simple expressions must be
    different and not both constants. They must both be canonical (as returned
    by [get_canonical_ignoring_name_mode]). If [add t s1 mode1 s2 mode2] returns
    [{ t = t'; canonical_element; alias_of_demoted_element }], then according to
    [t'],

    - [canonical_element] is the canonical element of both [s1] and [s2];

    - [alias_of_demoted_element] is either [s1] or [s2] (possibly with a new
    coercion; see note on [add_result]); and

    - [alias_of_demoted_element] is no longer canonical. *)
val add :
  binding_time_resolver:(Name.t -> Binding_time.With_name_mode.t) ->
  binding_times_and_modes:(_ * Binding_time.With_name_mode.t) Name.Map.t ->
  t ->
  canonical_element1:Simple.t ->
  canonical_element2:Simple.t ->
  add_result Or_bottom.t

(** [get_canonical_element] returns [None] only when the
    [min_order_within_equiv_class] cannot be satisfied. *)
val get_canonical_element_exn :
  binding_time_resolver:(Name.t -> Binding_time.With_name_mode.t) ->
  binding_times_and_modes:(_ * Binding_time.With_name_mode.t) Name.Map.t ->
  t ->
  Simple.t ->
  Name_mode.t ->
  min_name_mode:Name_mode.t ->
  min_binding_time:Binding_time.t ->
  Simple.t

module Alias_set : sig
  (** A set of aliases of one particular [Simple.t]. Each name occurs at most
      once: if an alias carries a coercion, no other alias in the set will have
      the same name. *)
  type t

  val empty : t

  val singleton : Simple.t -> t

  val get_singleton : t -> Simple.t option

  val choose_opt : t -> Simple.t option

  (* CR lmaurer: Could conceivably also do a join on the coercions. *)

  (** Find the aliases that occur in both sets. If a name occurs on both sides
      but under {e syntactically} different coercions, it will be dropped. *)
  val inter : t -> t -> t

  val filter : t -> f:(Simple.t -> bool) -> t

  (** Return the best alias in the set, where constants are better than symbols,
      which are better than variables, and ties are broken (arbitrarily) by
      [Simple.compare]. Returns [None] if the alias set is empty. *)
  val find_best : t -> Simple.t option

  val print : Format.formatter -> t -> unit
end

(** [get_aliases] always returns the supplied element in the result set. *)
val get_aliases : t -> Simple.t -> Alias_set.t

val get_canonical_ignoring_name_mode : t -> Name.t -> Simple.t

val apply_renaming : t -> Renaming.t -> t
