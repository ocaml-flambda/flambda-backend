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
  | Normal of CU.Name.t * CU.t * Digest.t
  | Alias of CU.Name.t
  | Parameter of CU.Name.t * Digest.t

type impl =
  | Loaded of CU.t * Digest.t
  | Unloaded of CU.t

(* CR-soon lmaurer: This combined type should go away soon, since each [t] is
   actually statically known to be either an [intf] or an [impl] (see PR
   #1933) *)
type t =
  | Intf_normal of CU.Name.t * CU.t * Digest.t
  | Intf_alias of CU.Name.t
  | Intf_parameter of CU.Name.t * Digest.t
  | Impl_loaded of CU.t * Digest.t
  | Impl_unloaded of CU.t

let create cu_name ~crc_with_unit =
  (* This creates an [Intf] just to be minimally restrictive. Any caller that
     cares should use the [Impl] API. *)
  match crc_with_unit with
  | None -> Intf_alias cu_name
  | Some (cu, crc) -> Intf_normal (cu_name, cu, crc)

let create_normal cu ~crc =
  match crc with Some crc -> Impl_loaded (cu, crc) | None -> Impl_unloaded cu

let name t =
  match t with
  | Impl_loaded (cu, _) | Impl_unloaded cu -> CU.name cu
  | Intf_normal (name, _, _) | Intf_alias name | Intf_parameter (name, _) ->
    name

let cu t =
  match t with
  | Intf_normal (_, cu, _) -> cu
  | Impl_loaded (cu, _) | Impl_unloaded cu -> cu
  | Intf_alias name | Intf_parameter (name, _) ->
    Misc.fatal_errorf
      "Cannot extract [Compilation_unit.t] from [Import_info.t] (for unit %a) \
       that never received it"
      CU.Name.print name

let crc t =
  match t with
  | Intf_normal (_, _, crc) | Intf_parameter (_, crc) -> Some crc
  | Intf_alias _ -> None
  | Impl_loaded (_, crc) -> Some crc
  | Impl_unloaded _ -> None

let has_name t ~name:name' = CU.Name.equal (name t) name'

let dummy = Intf_alias CU.Name.dummy

module Intf = struct
  (* Currently this is the same type as [Impl.t] but this will change (see PR
     #1746). *)
  type nonrec t = t

  let create_normal name cu ~crc =
    if not (CU.Name.equal (CU.name cu) name)
    then
      Misc.fatal_errorf
        "@[<hv>Mismatched import name and compilation unit:@ %a != %a@]"
        CU.Name.print name CU.print cu;
    Intf_normal (name, cu, crc)

  let create_alias name = Intf_alias name

  let create_parameter name ~crc = Intf_parameter (name, crc)

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

  let expect_intf t =
    match t with
    | Intf_normal (name, cu, crc) -> Normal (name, cu, crc)
    | Intf_alias name -> Alias name
    | Intf_parameter (name, crc) -> Parameter (name, crc)
    | Impl_loaded (cu, _) | Impl_unloaded cu ->
      Misc.fatal_errorf "Expected an [Import_info.Impl.t] but found %a" CU.print
        cu

  let name t =
    match expect_intf t with
    | Normal (name, _, _) | Alias name | Parameter (name, _) -> name

  let info t : Nonalias.t option =
    match expect_intf t with
    | Normal (_, cu, crc) -> Some (Normal cu, crc)
    | Parameter (_, crc) -> Some (Parameter, crc)
    | Alias _ -> None

  let crc t =
    match expect_intf t with
    | Normal (_, _, crc) | Parameter (_, crc) -> Some crc
    | Alias _ -> None

  let has_name t ~name:name' = CU.Name.equal (name t) name'

  let dummy = dummy
end

module Impl = struct
  (* Currently this is the same type as [Intf.t] but this will change (see PR
     #1746). *)
  type nonrec t = t

  let create_loaded cu ~crc = Impl_loaded (cu, crc)

  let create_unloaded cu = Impl_unloaded cu

  let create cu ~crc =
    match crc with
    | Some crc -> create_loaded cu ~crc
    | None -> create_unloaded cu

  let expect_impl t =
    match t with
    | Impl_loaded (cu, crc) -> Loaded (cu, crc)
    | Impl_unloaded cu -> Unloaded cu
    | Intf_normal (name, _, _) | Intf_alias name | Intf_parameter (name, _) ->
      Misc.fatal_errorf "Expected an [Import_info.Intf.t] but found %a"
        CU.Name.print name

  let cu t = match expect_impl t with Loaded (cu, _) | Unloaded cu -> cu

  let name t = CU.name (cu t)

  let crc t =
    match expect_impl t with Loaded (_, crc) -> Some crc | Unloaded _ -> None

  let dummy = Impl_unloaded CU.dummy
end
