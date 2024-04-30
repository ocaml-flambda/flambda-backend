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

type intf =
  | Ordinary of CU.Name.t * CU.t * Digest.t
  | Alias of CU.Name.t
  | Parameter of CU.Name.t * Digest.t

type impl =
  | Loaded of CU.t * Digest.t
  | Unloaded of CU.t

(* CR-soon lmaurer: This combined type should go away soon, since each [t] is
   actually statically known to be either an [intf] or an [impl] (see PR
   #1933) *)
type t =
  | Intf of intf
  | Impl of impl

let create cu_name ~crc_with_unit =
  (* This creates an [Intf] just to be minimally restrictive. Any caller that
     cares should use the [Impl] API. *)
  match crc_with_unit with
  | None -> Intf (Alias cu_name)
  | Some (cu, crc) -> Intf (Ordinary (cu_name, cu, crc))

let create_normal cu ~crc =
  match crc with
  | Some crc -> Impl (Loaded (cu, crc))
  | None -> Impl (Unloaded cu)

let name t =
  match t with
  | Impl (Loaded (cu, _) | Unloaded cu) -> CU.name cu
  | Intf (Ordinary (name, _, _) | Alias name | Parameter (name, _)) -> name

let cu t =
  match t with
  | Intf (Ordinary (_, cu, _)) -> cu
  | Impl (Loaded (cu, _) | Unloaded cu) -> cu
  | Intf (Alias name | Parameter (name, _)) ->
    Misc.fatal_errorf
      "Cannot extract [Compilation_unit.t] from [Import_info.t] (for unit %a) \
       that never received it"
      CU.Name.print name

let crc t =
  match t with
  | Intf (Ordinary (_, _, crc) | Parameter (_, crc)) -> Some crc
  | Intf (Alias _) -> None
  | Impl (Loaded (_, crc)) -> Some crc
  | Impl (Unloaded _) -> None

let has_name t ~name:name' = CU.Name.equal (name t) name'

let dummy = Intf (Alias CU.Name.dummy)

module Intf = struct
  (* Currently this is the same type as [Impl.t] but this will change (see PR
     #1746). *)
  type nonrec t = t

  let create_ordinary name cu ~crc =
    if not (CU.Name.equal (CU.name cu) name)
    then
      Misc.fatal_errorf
        "@[<hv>Mismatched import name and compilation unit:@ %a != %a@]"
        CU.Name.print name CU.print cu;
    Intf (Ordinary (name, cu, crc))

  let create_alias name = Intf (Alias name)

  let create_parameter name ~crc = Intf (Parameter (name, crc))

  module Nonalias = struct
    module Sort = struct
      type t =
        | Ordinary of CU.t
        | Parameter
    end

    type t = Sort.t * Digest.t
  end

  let create name nonalias =
    match (nonalias : Nonalias.t option) with
    | None -> create_alias name
    | Some (Ordinary cu, crc) -> create_ordinary name cu ~crc
    | Some (Parameter, crc) -> create_parameter name ~crc

  let expect_intf t =
    match t with
    | Intf intf -> intf
    | Impl (Loaded (cu, _) | Unloaded cu) ->
      Misc.fatal_errorf "Expected an [Import_info.Impl.t] but found %a" CU.print
        cu

  let name t =
    match expect_intf t with
    | Ordinary (name, _, _) | Alias name | Parameter (name, _) -> name

  let nonalias t : Nonalias.t option =
    match expect_intf t with
    | Ordinary (_, cu, crc) -> Some (Ordinary cu, crc)
    | Parameter (_, crc) -> Some (Parameter, crc)
    | Alias _ -> None

  let crc t =
    match expect_intf t with
    | Ordinary (_, _, crc) | Parameter (_, crc) -> Some crc
    | Alias _ -> None

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = dummy
end

module Impl = struct
  (* Currently this is the same type as [Intf.t] but this will change (see PR
     #1746). *)
  type nonrec t = t

  let create_loaded cu ~crc = Impl (Loaded (cu, crc))

  let create_unloaded cu = Impl (Unloaded cu)

  let create cu ~crc =
    match crc with
    | Some crc -> create_loaded cu ~crc
    | None -> create_unloaded cu

  let expect_impl t =
    match t with
    | Impl impl -> impl
    | Intf (Ordinary (name, _, _) | Alias name | Parameter (name, _)) ->
      Misc.fatal_errorf "Expected an [Import_info.Intf.t] but found %a"
        CU.Name.print name

  let cu t = match expect_impl t with Loaded (cu, _) | Unloaded cu -> cu

  let name t = CU.name (cu t)

  let crc t =
    match expect_impl t with Loaded (_, crc) -> Some crc | Unloaded _ -> None

  let dummy = Impl (Unloaded CU.dummy)
end
