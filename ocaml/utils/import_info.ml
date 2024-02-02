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
    | Normal of CU.t * Digest.t
    | Normal_no_crc of CU.t
    | Other of CU.Name.t * (CU.t * Digest.t) option

  (* CR xclerc: Maybe introduce Other_no_crc to flatten the option *)

  let create cu_name ~crc_with_unit =
    match crc_with_unit with
    | None -> Other (cu_name, None)
    | Some (cu, crc) ->
      (* For the moment be conservative and only use the [Normal] constructor
         when there is no pack prefix at all. *)
      if CU.Prefix.is_empty (CU.for_pack_prefix cu)
         && CU.Name.equal (CU.name cu) cu_name
      then Normal (cu, crc)
      else Other (cu_name, Some (cu, crc))

  let create_normal cu ~crc =
    match crc with Some crc -> Normal (cu, crc) | None -> Normal_no_crc cu

  let name t =
    match t with
    | Normal (cu, _) | Normal_no_crc cu -> CU.name cu
    | Other (name, _) -> name

  let cu t =
    match t with
    | Normal (cu, _) | Normal_no_crc cu | Other (_, Some (cu, _)) -> cu
    | Other (name, None) ->
      Misc.fatal_errorf
        "Cannot extract [Compilation_unit.t] from [Import_info.t] (for unit \
         %a) that never received it"
        CU.Name.print name

  let crc t =
    match t with
    | Normal (_, crc) -> Some crc
    | Normal_no_crc _ | Other (_, None) -> None
    | Other (_, Some (_, crc)) -> Some crc

  let crc_with_unit t =
    match t with
    | Normal (cu, crc) -> Some (cu, crc)
    | Normal_no_crc _ | Other (_, None) -> None
    | Other (_, some_cu_and_crc) -> some_cu_and_crc

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = Other (CU.Name.dummy, None)
end

module Impl = struct
  type t =
    | Normal of CU.t * Digest.t
    | Normal_no_crc of CU.t
    | Other of CU.Name.t * (CU.t * Digest.t) option

  (* CR xclerc: Maybe introduce Other_no_crc to flatten the option *)

  let create cu_name ~crc_with_unit =
    match crc_with_unit with
    | None -> Other (cu_name, None)
    | Some (cu, crc) ->
      (* For the moment be conservative and only use the [Normal] constructor
         when there is no pack prefix at all. *)
      if CU.Prefix.is_empty (CU.for_pack_prefix cu)
         && CU.Name.equal (CU.name cu) cu_name
      then Normal (cu, crc)
      else Other (cu_name, Some (cu, crc))

  let create_normal cu ~crc =
    match crc with Some crc -> Normal (cu, crc) | None -> Normal_no_crc cu

  let name t =
    match t with
    | Normal (cu, _) | Normal_no_crc cu -> CU.name cu
    | Other (name, _) -> name

  let cu t =
    match t with
    | Normal (cu, _) | Normal_no_crc cu | Other (_, Some (cu, _)) -> cu
    | Other (name, None) ->
      Misc.fatal_errorf
        "Cannot extract [Compilation_unit.t] from [Import_info.t] (for unit \
         %a) that never received it"
        CU.Name.print name

  let crc t =
    match t with
    | Normal (_, crc) -> Some crc
    | Normal_no_crc _ | Other (_, None) -> None
    | Other (_, Some (_, crc)) -> Some crc

  let crc_with_unit t =
    match t with
    | Normal (cu, crc) -> Some (cu, crc)
    | Normal_no_crc _ | Other (_, None) -> None
    | Other (_, some_cu_and_crc) -> some_cu_and_crc

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = Other (CU.Name.dummy, None)
end
