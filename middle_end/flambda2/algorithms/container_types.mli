(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Uniform interface for common data structures over various things.

    {b Warning:} this module is unstable and part of
    {{!Compiler_libs}compiler-libs}. *)

open Container_types_intf

module type Thing_no_hash = Thing_no_hash

module type Thing = Thing

module Pair : functor (A : Thing) (B : Thing) -> Thing with type t = A.t * B.t

module type Set = Set

module Make_set (T : Thing_no_hash) : Set with type elt = T.t

module type Map = Map

module type S = S

module type S_plus_stdlib = S_plus_stdlib

module type Map_plus_iterator = Map_plus_iterator

module type S_plus_iterator = S_plus_iterator

module Make (T : Thing) : S_plus_stdlib with type t := T.t

module Make_pair (T1 : S) (T2 : S) : sig
  include S with type t := T1.t * T2.t

  val create_from_cross_product : T1.Set.t -> T2.Set.t -> Set.t
end

module Shared_set (T : S) : sig
  type t = private T.Set.t

  type elt = T.t

  val create : T.Set.t -> t

  val print : Format.formatter -> t -> unit

  val empty : t

  val is_empty : t -> bool

  val singleton : elt -> t

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val mem : elt -> t -> bool

  (** [diff ss1 ss2] computes the difference of the shared sets [ss1] and [ss2].

      {b Complexity}: The complexity of [diff (union ss1 ss2) ss1] is linear
      in the size of [ss2]. The complexity of [diff (add k ss) ss] is the
      same as that of [mem k (add k ss)]. *)
  val diff : t -> t -> T.Set.t

  (** [diff_set ss s] returns a shared set that is identical to [ss], with the
      keys from [s] removed.

      {b Sharing}: Sharing is guaranteed for sub-trees of [ss] that are
      disjoint from [s]. *)
  val diff_set : t -> T.Set.t -> t

  (** [union ss1 ss2] computes the union of the shared sets [ss1] and [ss2].

      {b Sharing}: Sharing is guaranteed with sub-trees of [ss1] that are
      disjoint from [ss2].

      {b Complexity}: The complexity of [union (diff ss1 ss2) ss1] is linear in
      the size of [ss2]. The complexity of [union (remove k ss) ss] is the same
      as that of [add k (remove k ss)]. *)
  val union : t -> t -> t

  (** [union_set ss s] adds all the keys of set [s] to the shared set [ss].

      {b Sharing}: Sharing is guaranteed with sub-trees of [ss] that are
      disjoint from [s]. *)
  val union_set : t -> T.Set.t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (elt -> unit) -> t -> unit

  (** {2 Unsharing functions}

      The following functions construct a new shared set but do not
      necessarily preserve sharing with their input. They are marked with the
      [_unshare] suffix to make this stand out.
  *)

  val map_unshare : (elt -> elt) -> t -> t
end

module Shared_map (T : S) : sig
  type 'a t = private 'a T.Map.t

  type key = T.t

  val create : 'a T.Map.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val empty : 'a t

  val is_empty : 'a t -> bool

  val singleton : key -> 'a -> 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val find_opt : key -> 'a t -> 'a option

  (** [diff f sm1 sm2] returns a shared map that is identical to [sm1], except
      that bindings in [sm1] with a key [k] that also appears in [sm2] are
      updated according to [f].

      [diff] assumes that [f] always returns [None] when called with physically
      equal arguments, and will never call [f] with physically equal arguments.

      {b Complexity}: The complexity of [diff f (union g sm1 sm2) sm1] is linear
      in the size of [sm2]. The complexity of [diff f (add sm k v) sm] is the
      same as that of [find k (add sm k v)]. *)
  val diff : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a T.Map.t

  (** [diff_map f sm m] returns a shared map that is identical to [sm], except
      that bindings in [sm] with a key [k] that also appear in [m] are updated
      according to [f].

      {b Sharing}: Sharing is guaranteed for sub-trees of [sm] for which all
      call to [f] return the first value unchanged. In particular, sharing is
      guaranteed for all sub-trees of [sm] that are disjoint from [m]. *)
  val diff_map : (key -> 'a -> 'b -> 'a option) -> 'a t -> 'b T.Map.t -> 'a t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val union_map : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a T.Map.t -> 'a t

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val map : ('a -> 'a) -> 'a t -> 'a t

  (** {2 Unsharing functions}

      The following functions construct a new {!Shared_map.t} but do not
      necessarily preserve sharing with their input. They are marked with the
      [_unshare] suffix to make this stand out.
  *)

  val map_unshare : ('a -> 'b) -> 'a t -> 'b t

  val map_keys_unshare : (key -> key) -> 'a t -> 'a t
end
