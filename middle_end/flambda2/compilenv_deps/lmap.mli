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

(** {b Warning:} this module is unstable and part of
    {{!Compiler_libs}compiler-libs}.

    This module supports lists of key-value pairs that aren't intended for fast
    lookup. These are suitable for use in ASTs, where it's uncommon to look up a
    value directly but very common to fold over the list to produce an
    environment that is a true Map. They also preserve the order of the
    elements, which is necessary for performing comparisons up to renaming of
    closure ids and code ids.

    Several operations on Map are provided here for consistency, but they do not
    necessarily provide the same checks that a Map would and in most cases the
    performance is different. Behavior will be the same provided that there are
    never duplicate keys. You can manually invoke [invariant] to check for
    duplicates. *)

(* CR-soon lmaurer: It would be nice to have convenience functions for
   conversion between maps and lmaps; this is complicated by the need to be
   passed the Map.S implementation for the key type. *)

module type Thing = sig
  type t

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  (** The key should not already exist in the map; this is not checked. *)
  val add : key -> 'a -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  (** Unlike [disjoint_union] on maps, the disjointness is not checked. *)
  val disjoint_union : 'a t -> 'a t -> 'a t

  (** The given maps must be pairwise disjoint, which is not checked. *)
  val disjoint_union_many : 'a t list -> 'a t

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val keys : _ t -> key list

  val data : 'a t -> 'a list

  val bindings : 'a t -> (key * 'a) list

  (** Keys in the list must be distinct, which is not checked. *)
  val of_list : (key * 'a) list -> 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val get_singleton : 'a t -> (key * 'a) option

  val get_singleton_exn : 'a t -> key * 'a

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

  val map_sharing : ('a -> 'a) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val to_seq : 'a t -> (key * 'a) Seq.t

  val for_all_with_fixed_arg : (key -> 'a -> 'b -> bool) -> 'a t -> 'b -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  (** Keys in the sequence must be distinct from each other and from keys
      already in the map; neither of these conditions is checked. *)
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

  (** Keys in the sequence must be distinct, which is not checked. *)
  val of_seq : (key * 'a) Seq.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** Check that there are no duplicates in the list, calling
      [Misc.fatal_errorf] if a duplicate is found. *)
  val invariant : 'a t -> unit
end

module Make (T : Thing) : S with type key = T.t
