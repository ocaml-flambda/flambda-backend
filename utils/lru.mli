(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nathanaëlle Courant, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Implementation of an LRU cache. Each slot of the cache is specified by an
    [uncached] type and a [cached] type. The [uncached] type corresponds to the
    information from which the [cached] information is derived. The cache
    ensures that at most [capacity] slots are loaded at each point in time. *)

module type Lru_slot = sig
  type uncached

  type cached

  val load : uncached -> cached

  val unload : uncached -> cached -> unit
end

module type S = sig
  type t

  type slot

  type uncached

  type cached

  val create : capacity:int -> t

  val add_slot : uncached -> cached -> t -> slot

  val load_slot : slot -> t -> cached

  val unload_all : t -> unit
end

module Make (Slot : Lru_slot) :
  S with type uncached = Slot.uncached and type cached = Slot.cached
