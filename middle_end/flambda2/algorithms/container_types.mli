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

module type Thing_no_hash = sig
  type t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module Pair : functor (A : Thing) (B : Thing) -> Thing with type t = A.t * B.t

module type Set = sig
  module T : Set.OrderedType

  include Set.S with type elt = T.t

  val print : Format.formatter -> t -> unit

  val to_string : t -> string

  val of_list : elt list -> t

  val map : (elt -> elt) -> t -> t

  val union_list : t list -> t

  val intersection_is_empty : t -> t -> bool

  val get_singleton : t -> elt option
end

module Make_set (T : Thing_no_hash) : Set with module T := T

module type Map = sig
  module T : Map.OrderedType

  include Map.S with type key = T.t

  module Set : Set with module T := T

  val print_debug :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val of_list : (key * 'a) list -> 'a t

  (** [disjoint_union m1 m2] contains all bindings from [m1] and [m2]. If some
      binding is present in both and the associated value is not equal, a
      Fatal_error is raised *)
  val disjoint_union :
    ?eq:('a -> 'a -> bool) ->
    ?print:(Format.formatter -> 'a -> unit) ->
    'a t ->
    'a t ->
    'a t

  val rename : key t -> key -> key

  val map_keys : (key -> key) -> 'a t -> 'a t

  val keys : 'a t -> Set.t

  val data : 'a t -> 'a list

  val of_set : (key -> 'a) -> Set.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val diff_domains : 'a t -> 'a t -> 'a t

  val inter : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val inter_domain_is_non_empty : 'a t -> 'a t -> bool

  val get_singleton : 'a t -> (key * 'a) option

  val replace : key -> ('a -> 'a) -> 'a t -> 'a t

  val map_sharing : ('a -> 'a) -> 'a t -> 'a t
end

module type S = sig
  type t

  module T : Thing with type t = t

  include Thing with type t := T.t

  module Set : Set with module T := T

  module Map : Map with module T := T with module Set = Set
end

module Make (T : Thing) : S with type t := T.t

module Make_pair (T1 : S) (T2 : S) : sig
  include S with type t := T1.t * T2.t

  val create_from_cross_product : T1.Set.t -> T2.Set.t -> Set.t
end
