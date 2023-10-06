(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Uid = Shape.Uid

type t =
  | Uid of Uid.t
  | Proj of Uid.t * int

let internal_not_actually_unique = Uid Uid.internal_not_actually_unique

let uid u = Uid u

let proj u ~field = Proj (u, field)

module T0 = struct
  type nonrec t = t

  let print ppf t =
    match t with
    | Uid uid -> Format.fprintf ppf "@[<hov 1>(uid@ %a)@]" Uid.print uid
    | Proj (uid, field) ->
      Format.fprintf ppf
        "@[<hov 1>(@[<hov 1>(uid@ %a)@]@ @[<hov 1>(field@ %d)@])@]" Uid.print
        uid field

  let compare t1 t2 =
    match t1, t2 with
    | Uid uid1, Uid uid2 -> Uid.compare uid1 uid2
    | Proj (uid1, field1), Proj (uid2, field2) ->
      let c = Uid.compare uid1 uid2 in
      if c <> 0 then c else Int.compare field1 field2
    | Uid _, Proj _ -> -1
    | Proj _, Uid _ -> 1

  let equal t1 t2 =
    match t1, t2 with
    | Uid uid1, Uid uid2 -> Uid.equal uid1 uid2
    | Proj (uid1, field1), Proj (uid2, field2) ->
      Uid.equal uid1 uid2 && Int.equal field1 field2
    | Uid _, Proj _ | Proj _, Uid _ -> false

  let hash t =
    match t with
    | Uid uid -> Hashtbl.hash (0, Uid.hash uid)
    | Proj (uid, field) -> Hashtbl.hash (1, (Uid.hash uid, field))

  let output _ _ = Misc.fatal_error "Not implemented"
end

include Identifiable.Make (T0)
