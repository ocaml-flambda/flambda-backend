(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Code_present of Code.t
  | Metadata_only of Code_metadata.t

let print ppf t =
  match t with
  | Code_present code ->
    Format.fprintf ppf "@[<hov 1>(Code_present@ (@[<hov 1>(code@ %a)@]))@]"
      Code.print code
  | Metadata_only code_metadata ->
    Format.fprintf ppf "@[<hov 1>(Metadata_only@ (code_metadata@ %a))@]"
      Code_metadata.print code_metadata

let create code = Code_present code

let merge code_id t1 t2 =
  match t1, t2 with
  | Metadata_only cm1, Metadata_only cm2 ->
    if Code_metadata.equal cm1 cm2
    then Some t1
    else
      Misc.fatal_errorf
        "Code id %a is imported with different code metadata (%a and %a)"
        Code_id.print code_id Code_metadata.print cm1 Code_metadata.print cm2
  | Code_present _, Code_present _ ->
    Misc.fatal_errorf "Cannot merge two definitions for code id %a"
      Code_id.print code_id
  | Metadata_only cm_imported, (Code_present code_present as t)
  | (Code_present code_present as t), Metadata_only cm_imported ->
    let cm_present = Code.code_metadata code_present in
    if Code_metadata.equal cm_present cm_imported
    then Some t
    else
      Misc.fatal_errorf
        "Code_id %a is present with code metadata@ %abut imported with code \
         metadata@ %a"
        Code_id.print code_id Code_metadata.print cm_present Code_metadata.print
        cm_imported

let free_names t =
  match t with
  | Code_present code -> Code.free_names code
  | Metadata_only code_metadata -> Code_metadata.free_names code_metadata

let apply_renaming t renaming =
  match t with
  | Code_present code ->
    let code = Code.apply_renaming code renaming in
    Code_present code
  | Metadata_only code_metadata ->
    let code_metadata = Code_metadata.apply_renaming code_metadata renaming in
    Metadata_only code_metadata

let all_ids_for_export t =
  match t with
  | Code_present code -> Code.all_ids_for_export code
  | Metadata_only code_metadata ->
    Code_metadata.all_ids_for_export code_metadata

let remember_only_metadata t =
  match t with
  | Code_present code -> Metadata_only (Code.code_metadata code)
  | Metadata_only _ -> t

let code_metadata t =
  match t with
  | Code_present code -> Code.code_metadata code
  | Metadata_only code_metadata -> code_metadata

let iter_code t ~f =
  match t with Code_present code -> f code | Metadata_only _ -> ()

let code_present t =
  match t with Code_present _ -> true | Metadata_only _ -> false
