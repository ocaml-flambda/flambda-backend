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

type t =
  | Normal of CU.t * Digest.t
  | Normal_no_crc of CU.t
  | Other of CU.Name.t * CU.t option * Digest.t option

(* CR xclerc: Maybe introduce Other_no_crc to flatten the option *)

let create cu_name ~crc_with_unit =
  match crc_with_unit with
  | None -> Other (cu_name, None, None)
  | Some (cu, crc) ->
    (* For the moment be conservative and only use the [Normal] constructor when
       there is no pack prefix at all. *)
    if CU.Prefix.is_empty (CU.for_pack_prefix cu)
       && CU.Name.equal (CU.name cu) cu_name
    then Normal (cu, crc)
    else Other (cu_name, Some cu, Some crc)

let create_normal cu ~crc =
  match crc with Some crc -> Normal (cu, crc) | None -> Normal_no_crc cu

let name t =
  match t with
  | Normal (cu, _) | Normal_no_crc cu -> CU.name cu
  | Other (name, _, _) -> name

let cu t =
  match t with
  | Normal (cu, _) | Normal_no_crc cu | Other (_, Some cu, _) -> cu
  | Other (name, None, _) ->
    Misc.fatal_errorf
      "Cannot extract [Compilation_unit.t] from [Import_info.t] (for unit %a) \
       that never received it"
      CU.Name.print name

let crc t =
  match t with
  | Normal (_, crc) -> Some crc
  | Normal_no_crc _ | Other (_, _, None) -> None
  | Other (_, _, Some crc) -> Some crc

let has_name t ~name:name' = CU.Name.equal (name t) name'

let dummy = Other (CU.Name.dummy, None, None)

module Intf = struct
  type nonrec t = t

  let create (name : CU.Name.t) cu ~crc =
    match cu, crc with
    | None, None -> Normal_no_crc (CU.create CU.Prefix.empty name)
    | None, Some crc -> (* Parameter *) Other (name, None, Some crc)
    | Some _, None ->
      Misc.fatal_errorf "@[<hv>CU but no CRC for import:@ %a@]" CU.Name.print
        name
    | Some cu, Some crc ->
      if not (CU.Name.equal (CU.name cu) name)
      then
        Misc.fatal_errorf
          "@[<hv>Mismatched import name and compilation unit:@ %a@]"
          CU.Name.print name CU.print cu;
      if CU.is_packed cu
      then Other (name, Some cu, Some crc)
      else Normal (cu, crc)

  let name = name

  let impl t =
    match t with
    | Normal (cu, _) -> Some cu
    | Normal_no_crc cu -> Some cu
    | Other (_, cu, _) -> cu

  let crc = crc

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = dummy
end

module Impl = struct
  type nonrec t = t

  let create cu ~crc =
    match crc with Some crc -> Normal (cu, crc) | None -> Normal_no_crc cu

  (* The fatal error is guaranteed not to be thrown on things created by
     [Impl.create] *)
  let cu = cu

  let name = name

  let crc = crc

  let dummy = dummy
end
