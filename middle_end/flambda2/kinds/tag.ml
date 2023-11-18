(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = int

type tag = t

include Container_types.Make (struct
  type nonrec t = t

  let compare = Numeric_types.Int.compare

  let equal = Numeric_types.Int.equal

  let hash = Numeric_types.Int.hash

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@[tag_%d@]" t
end)

let min_tag = 0

let max_tag = 255

let create_exn tag =
  if tag < min_tag || tag > max_tag
  then Misc.fatal_error (Printf.sprintf "Tag.create_exn %d" tag)
  else tag

let create_from_targetint ti =
  let min_tag = Targetint_31_63.of_int min_tag in
  let max_tag = Targetint_31_63.of_int max_tag in
  if Targetint_31_63.compare ti min_tag >= 0
     && Targetint_31_63.compare ti max_tag <= 0
  then Some (Targetint_31_63.to_int ti)
  else None

let to_int t = t

let to_targetint_31_63 t = Targetint_31_63.of_int (to_int t)

let zero = 0

let string_tag = Obj.string_tag

let double_tag = Obj.double_tag

let double_array_tag = Obj.double_array_tag

let abstract_tag = Obj.abstract_tag

let custom_tag = Obj.custom_tag

let infix_tag = Obj.infix_tag

let closure_tag = Obj.closure_tag

let object_tag = Obj.object_tag

let forward_tag = Obj.forward_tag

let lazy_tag = Obj.lazy_tag

module Scannable = struct
  type nonrec t = t

  include Container_types.Make (Numeric_types.Int)

  let create tag =
    if tag < min_tag || tag >= Obj.no_scan_tag then None else Some tag

  let create_exn tag =
    match create tag with
    | Some tag -> tag
    | None ->
      Misc.fatal_error (Printf.sprintf "Tag.Scannable.create_exn %d" tag)

  let to_int t = t

  let to_targetint t = Targetint_32_64.of_int (to_int t)

  let to_tag t = t

  let of_tag tag =
    if tag < min_tag || tag >= Obj.no_scan_tag then None else Some tag

  let zero = 0

  let object_tag = Obj.object_tag
end

module Non_scannable = struct
  type nonrec t = t

  include Container_types.Make (Numeric_types.Int)

  let create tag = if tag < Obj.no_scan_tag then None else Some tag

  let create_exn tag =
    match create tag with
    | Some tag -> tag
    | None ->
      Misc.fatal_error (Printf.sprintf "Tag.Non_scannable.create_exn %d" tag)

  let to_int t = t

  let to_tag t = t

  let of_tag tag =
    if tag < min_tag || tag >= Obj.no_scan_tag then None else Some tag
end

let is_structured_block t =
  match Scannable.create t with None -> false | Some _ -> true

let is_structured_block_but_not_data_constructor t =
  is_structured_block t && t > Obj.last_non_constant_constructor_tag
