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
    | Normal of Import.t * Digest.t (* Unpacked, so compilation unit = import *)
    | Name_only of Import.t
    | Other of Import.t * CU.t option * Digest.t option

  let create (import : Import.t) cu ~crc =
    match cu, crc with
    | None, None -> Name_only import
    | None, Some crc -> (* Parameter *) Other (import, None, Some crc)
    | Some _, None ->
      Misc.fatal_errorf "@[<hv>CU but no CRC for import:@ %a@]" Import.print
        import
    | Some cu, Some crc ->
      if CU.instance_arguments cu <> []
      then
        Misc.fatal_errorf "@[<hv>Interface import with arguments:@ %a@]"
          CU.print cu;
      if not (String.equal (CU.name_as_string cu) (import |> Import.to_string))
      then
        Misc.fatal_errorf
          "@[<hv>Mismatched import name and compilation unit:@ %a@]"
          Import.print import CU.print cu;
      if CU.is_packed cu
      then Other (import, Some cu, Some crc)
      else Normal (import, crc)

  let name t =
    match t with
    | Normal (import, _) | Name_only import | Other (import, _, _) -> import

  let impl t =
    match t with
    | Normal (import, _) ->
      let cu =
        CU.create CU.Prefix.empty
          (import |> Import.to_string |> CU.Name.of_string)
      in
      Some cu
    | Name_only _ -> None
    | Other (_, cu, _) -> cu

  let crc t =
    match t with
    | Normal (_, crc) -> Some crc
    | Name_only _ -> None
    | Other (_, _, crc) -> crc

  let has_name t ~name:name' = Import.equal (name t) name'

  let dummy = Name_only Import.dummy
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
