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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = int
type tag = t

include Container_types.Make (struct
  type nonrec t = t

  let compare = Numeric_types.Int.compare
  let equal = Numeric_types.Int.equal
  let hash = Numeric_types.Int.hash

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@[tag_%d@]" t

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let min_tag = 0
let max_tag = 255

let create tag =
  if tag < min_tag || tag > max_tag then None
  else Some tag

let create_exn tag =
  if tag < min_tag || tag > max_tag then
    Misc.fatal_error (Printf.sprintf "Tag.create_exn %d" tag)
  else
    tag

let create_from_targetint_imm ti =
  let min_tag = Targetint_31_63.Imm.of_int min_tag in
  let max_tag = Targetint_31_63.Imm.of_int max_tag in
  if Targetint_31_63.Imm.compare ti min_tag >= 0
    && Targetint_31_63.Imm.compare ti max_tag <= 0
  then Some (Targetint_31_63.Imm.to_int ti)
  else None

let create_from_targetint imm =
  create_from_targetint_imm (Targetint_31_63.to_targetint imm)

let to_int t = t
let to_targetint t = Targetint_32_64.of_int (to_int t)
let to_targetint_ocaml t = Targetint_31_63.Imm.of_int (to_int t)
let to_target_imm t = Targetint_31_63.int (to_targetint_ocaml t)

let zero = 0
let string_tag = Obj.string_tag
let double_tag = Obj.double_tag
let double_array_tag = Obj.double_array_tag
let custom_tag = Obj.custom_tag
let infix_tag = Obj.infix_tag
let closure_tag = Obj.closure_tag
let object_tag = Obj.object_tag
let forward_tag = Obj.forward_tag
let lazy_tag = Obj.lazy_tag

let arbitrary = max_int

module Scannable = struct
  type nonrec t = t

  include Container_types.Make (Numeric_types.Int)

  let create tag =
    if tag < min_tag || tag >= Obj.no_scan_tag then None
    else Some tag

  let create_exn tag =
    match create tag with
    | Some tag -> tag
    | None ->
      Misc.fatal_error (Printf.sprintf "Tag.Scannable.create_exn %d" tag)

  let to_int t = t
  let to_targetint t = Targetint_32_64.of_int (to_int t)
  let to_tag t = t

  let of_tag tag =
    if tag < min_tag || tag >= Obj.no_scan_tag then None
    else Some tag

  let zero = 0
  let object_tag = Obj.object_tag
end

let to_scannable_set set =
  Set.fold (fun t result ->
      match Scannable.create t with
      | None -> result
      | Some scannable -> Scannable.Set.add scannable result)
    set
    Scannable.Set.empty

let is_structured_block t =
  match Scannable.create t with
  | None -> false
  | Some _ -> true

module Non_scannable = struct
  type nonrec t = t

  include Container_types.Make (Numeric_types.Int)

  let create tag =
    if tag < Obj.no_scan_tag then None
    else Some tag

  let create_exn tag =
    match create tag with
    | Some tag -> tag
    | None ->
      Misc.fatal_error (Printf.sprintf "Tag.Non_scannable.create_exn %d" tag)

  let to_int t = t
  let to_tag t = t

  let of_tag tag =
    if tag < min_tag || tag >= Obj.no_scan_tag then None
    else Some tag
end

let is_structured_block_but_not_a_variant t =
  is_structured_block t && t > Obj.last_non_constant_constructor_tag

let all_regular_tags =
  let rec compute_all_tags acc n =
    if n > Obj.last_non_constant_constructor_tag
    then acc
    else compute_all_tags (Set.add n acc) (succ n)
  in
  compute_all_tags Set.empty 0
