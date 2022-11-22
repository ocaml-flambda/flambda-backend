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
  S with type uncached = Slot.uncached and type cached = Slot.cached = struct
  type cached = Slot.cached

  type uncached = Slot.uncached

  type slot =
    { mutable previous : slot;
      mutable next : slot;
      data : uncached option;
      mutable cached_data : cached option
    }

  type t =
    { mutable remaining_slots : int;
      sentinel : slot
    }

  let create ~capacity =
    assert (capacity > 0);
    let rec sentinel =
      { previous = sentinel; next = sentinel; data = None; cached_data = None }
    in
    { remaining_slots = capacity; sentinel }

  let insert_after ~slot ~to_insert =
    let slot2 = slot.next in
    to_insert.previous <- slot;
    to_insert.next <- slot2;
    slot.next <- to_insert;
    slot2.previous <- to_insert

  let extract slot =
    slot.previous.next <- slot.next;
    slot.next.previous <- slot.previous;
    slot.previous <- slot;
    slot.next <- slot

  let load_if_needed slot =
    match slot.cached_data with
    | Some cached_data -> cached_data
    | None -> (
      match slot.data with
      | Some data ->
        let cached_data = Slot.load data in
        slot.cached_data <- Some cached_data;
        cached_data
      | None -> Misc.fatal_error "lru.load_if_needed called on sentinel")

  let unload slot =
    match slot.data, slot.cached_data with
    | Some data, Some cached_data ->
      Slot.unload data cached_data;
      slot.cached_data <- None
    | Some _, None -> ()
    | None, _ -> Misc.fatal_error "lru.unload called on sentinel"

  let unload_one t =
    t.remaining_slots <- t.remaining_slots + 1;
    let slot = t.sentinel.previous in
    extract slot;
    unload slot

  let make_room t = if t.remaining_slots = 0 then unload_one t

  let add_slot uncached cached t =
    make_room t;
    t.remaining_slots <- t.remaining_slots - 1;
    let new_slot =
      { previous = t.sentinel;
        next = t.sentinel.next;
        data = Some uncached;
        cached_data = Some cached
      }
    in
    t.sentinel.next.previous <- new_slot;
    t.sentinel.next <- new_slot;
    new_slot

  let load_slot slot t =
    if slot.next == slot
    then (
      (* This slot is currently outside the cache *)
      make_room t;
      t.remaining_slots <- t.remaining_slots - 1)
    else extract slot;
    insert_after ~slot:t.sentinel ~to_insert:slot;
    load_if_needed slot

  let unload_all t =
    while t.sentinel.previous != t.sentinel do
      unload_one t
    done
end
