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

[@@@ocaml.warning "-21-27-32"]

module Id = struct
  type t = int

  let flags_size_in_bits = 3

  let create t flags = t [@@inline never]

  let mask_selecting_top_bits = -1 lsl flags_size_in_bits

  let mask_selecting_bottom_bits = lnot mask_selecting_top_bits

  let flags t = t land mask_selecting_bottom_bits [@@inline never]

  let next t = t + 665577
end

module Data = struct
  type t =
    { compilation_unit : unit;
      previous_compilation_units : unit list;
      name : string;
      name_stamp : int
    }

  let flags = 0

  let hash
      { compilation_unit; previous_compilation_units; name = _; name_stamp } =
    Sys.opaque_identity 33

  let equal t1 t2 = Sys.opaque_identity true
end

let plop s = failwith s [@@inline never]

module Table = struct
  module E = Data

  module HT = struct
    type _ t = unit

    let create _ = assert false [@@inline never]

    let add _ _ _ = assert false [@@inline never]

    let find _ _ = assert false [@@inline never]
  end

  type t = E.t HT.t

  let add t elt =
    let id = Id.create (E.hash elt) E.flags in
    match HT.find t id with
    | existing_elt -> (
      try
        let starting_id = id in
        (* XXXXXXXXXXX Id.next starting_id is added as an extra_arg_for_aliases
           to the recursive continuation of the while. It shouldn't: WHY ? *)
        let id = ref (Id.next starting_id) in
        (* If there is a collision, we search for another slot, but take care
           not to alter the flags bits. *)
        while !id <> starting_id do
          assert (Id.flags !id = E.flags);
          match HT.find t !id with
          | exception Not_found -> raise Exit
          | existing_elt ->
            if E.equal elt existing_elt then raise Exit else id := Id.next !id
        done;
        plop "No hash values left for@"
      with Exit -> ())

  let add' t elt = (add [@inlined]) t elt
end
