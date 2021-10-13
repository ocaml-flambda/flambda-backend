(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Int = Numbers.Int

module Id = struct
  include Int

  let num_empty_bottom_bits = 2

  let mask_selecting_top_bits = -1 lsl num_empty_bottom_bits

  let mask_selecting_bottom_bits = lnot mask_selecting_top_bits

  let flags_size_in_bits = num_empty_bottom_bits

  let flags t = t land mask_selecting_bottom_bits

  let without_flags t = t land mask_selecting_top_bits

  let[@inline always] with_flags t flags =
    if flags < 0 || flags >= 1 lsl (flags_size_in_bits + 1)
    then Misc.fatal_errorf "Flags value 0x%x out of range" flags;
    without_flags t lor flags
end

module Make (E : sig
  type t

  val flags : int

  val print : Format.formatter -> t -> unit

  val hash : t -> int

  val equal : t -> t -> bool
end) =
struct
  module HT = Hashtbl.Make (struct
    type t = int

    (* CR mshinwell: maybe this should be a proper hash function *)
    let hash (t : t) = Hashtbl.hash t

    let equal t1 t2 = t1 == t2
  end)

  let () = assert (E.flags land Id.mask_selecting_top_bits = 0)

  type t = E.t HT.t

  let create () = HT.create 20_000

  exception Can_add of int

  exception Already_added of int

  let add t elt =
    let id = Id.with_flags (E.hash elt) E.flags in
    match HT.find t id with
    | exception Not_found ->
      HT.add t id elt;
      id
    | existing_elt -> (
      if E.equal elt existing_elt
      then id
      else
        try
          let starting_id = id in
          let id = ref (starting_id + 1) in
          (* If there is a collision, we search for another slot, but take care
             not to alter the flags bits. *)
          while !id <> starting_id do
            (* CR mshinwell: performance could be improved *)
            while Id.flags !id <> E.flags do
              incr id
            done;
            match HT.find t !id with
            | exception Not_found -> raise (Can_add !id)
            | existing_elt ->
              if E.equal elt existing_elt
              then raise (Already_added !id)
              else incr id
          done;
          Misc.fatal_errorf "No hash values left for@ %a" E.print elt
        with
        | Can_add id ->
          HT.add t id elt;
          assert (Id.flags id = E.flags);
          id
        | Already_added id ->
          assert (Id.flags id = E.flags);
          id)

  let find t id =
    assert (Id.flags id = E.flags);
    HT.find t id
end
