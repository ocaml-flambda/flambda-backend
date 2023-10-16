(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Mark Shinwell, Jane Street UK Partnership LLP              *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module CU = Compilation_unit

module Intf = struct
  type t =
    | Normal of CU.Name.t * Digest.t (* Unpacked, so compilation unit = name *)
    | Name_only of CU.Name.t (* Same but digest unknown *)
    | Other of CU.Name.t * (CU.t * Digest.t) option
  (* Packed, so the CU isn't just the name *)

  let create cu_name ~crc_with_unit =
    match crc_with_unit with
    | None -> Other (cu_name, None)
    | Some (cu, crc) ->
      (* If there's no pack prefix, the CU is just the name, so we don't need to
         store both. *)
      if CU.Prefix.is_empty (CU.for_pack_prefix cu)
         && CU.Name.equal (CU.name cu) cu_name
      then Normal (cu_name, crc)
      else Other (cu_name, Some (cu, crc))

  let name t =
    match t with Normal (name, _) | Name_only name | Other (name, _) -> name

  let impl t =
    match t with
    | Normal (name, _) ->
      let cu = CU.create CU.Prefix.empty name in
      Some cu
    | Name_only _ -> None
    | Other (_, Some (cu, _)) -> Some cu
    | Other (_, None) -> None

  let crc t =
    match t with
    | Normal (_, crc) -> Some crc
    | Name_only _ | Other (_, None) -> None
    | Other (_, Some (_, crc)) -> Some crc

  let crc_with_unit t =
    match t with
    | Normal (cu, crc) -> Some (CU.create CU.Prefix.empty cu, crc)
    | Name_only _ | Other (_, None) -> None
    | Other (_, some_cu_and_crc) -> some_cu_and_crc

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = Name_only CU.Name.dummy
end

module Impl = struct
  type t =
    | With_crc of CU.t * Digest.t
    | No_crc of CU.t

  let create cu ~crc =
    match crc with Some crc -> With_crc (cu, crc) | None -> No_crc cu

  let cu (With_crc (cu, _) | No_crc cu) = cu

  let name t = CU.name (cu t)

  let crc t = match t with With_crc (_, crc) -> Some crc | No_crc _ -> None

  let dummy = No_crc CU.dummy
end
