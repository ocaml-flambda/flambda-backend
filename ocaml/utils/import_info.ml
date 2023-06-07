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
    | Name_only of CU.Name.t
    | Other of CU.Name.t * CU.t option * Digest.t option

  let create (name : CU.Name.t) cu ~crc =
    match cu, crc with
    | None, None -> Name_only name
    | None, Some crc -> (* Parameter *) Other (name, None, Some crc)
    | Some _, None ->
      Misc.fatal_errorf "@[<hv>CU but no CRC for import:@ %a@]" CU.Name.print
        name
    | Some cu, Some crc ->
      if CU.instance_arguments cu <> []
      then
        Misc.fatal_errorf "@[<hv>Interface import with arguments:@ %a@]"
          CU.print cu;
      if not (CU.Name.equal (CU.name cu) name)
      then
        Misc.fatal_errorf
          "@[<hv>Mismatched import name and compilation unit:@ %a@]"
          CU.Name.print name CU.print cu;
      if CU.is_packed cu
      then Other (name, Some cu, Some crc)
      else Normal (name, crc)

  let name t =
    match t with
    | Normal (name, _) | Name_only name | Other (name, _, _) -> name

  let impl t =
    match t with
    | Normal (name, _) ->
      let cu = CU.create CU.Prefix.empty name in
      Some cu
    | Name_only _ -> None
    | Other (_, cu, _) -> cu

  let crc t =
    match t with
    | Normal (_, crc) -> Some crc
    | Name_only _ -> None
    | Other (_, _, crc) -> crc

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
