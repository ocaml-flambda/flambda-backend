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
    | Alias of CU.Name.t
    | Parameter of CU.Name.t * Digest.t

  let check_name name cu =
    if not (CU.Name.equal (CU.name cu) name)
    then
      Misc.fatal_errorf
        "@[<hv>Mismatched import name and compilation unit:@ %a != %a@]"
        CU.Name.print name CU.print cu

  let create_normal name cu ~crc =
    check_name name cu;
    Normal (cu, crc)

  let create_alias name = Alias name

  let create_parameter name ~crc = Parameter (name, crc)

  module Nonalias = struct
    module Kind = struct
      type t =
        | Normal of CU.t
        | Parameter
    end

    type t = Kind.t * Digest.t
  end

  let create name nonalias =
    match (nonalias : Nonalias.t option) with
    | None -> create_alias name
    | Some (Normal cu, crc) -> create_normal name cu ~crc
    | Some (Parameter, crc) -> create_parameter name ~crc

  let name t =
    match t with
    | Normal (cu, _) -> CU.name cu
    | Alias name | Parameter (name, _) -> name

  let info t : Nonalias.t option =
    match t with
    | Normal (cu, crc) -> Some (Normal cu, crc)
    | Parameter (_, crc) -> Some (Parameter, crc)
    | Alias _ -> None

  let cu t =
    match t with Normal (cu, _) -> Some cu | Parameter _ | Alias _ -> None

  let crc t =
    match t with
    | Normal (_, crc) | Parameter (_, crc) -> Some crc
    | Alias _ -> None

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = Alias CU.Name.dummy
end

module Impl = struct
  type t =
    | Loaded of CU.t * Digest.t
    | Unloaded of CU.t

  let create_loaded cu ~crc = Loaded (cu, crc)

  let create_unloaded cu = Unloaded cu

  let create cu ~crc =
    match crc with
    | Some crc -> create_loaded cu ~crc
    | None -> create_unloaded cu

  let cu t = match t with Loaded (cu, _) | Unloaded cu -> cu

  let name t = CU.name (cu t)

  let crc t = match t with Loaded (_, crc) -> Some crc | Unloaded _ -> None

  let dummy = Unloaded CU.dummy
end
