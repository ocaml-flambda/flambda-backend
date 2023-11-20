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

module File_sections = Flambda_backend_utils.File_sections

type code_status =
  | Loaded of Code.t
  | Not_loaded of
      { sections : File_sections.t;
        index : int;
        metadata : Code_metadata.t;
        delayed_renaming : Renaming.t
      }

type t =
  | Code_present of { mutable code_status : code_status }
  | Metadata_only of Code_metadata.t

type code_present =
  | Present of { index : int }
  | Absent

type raw =
  { metadata : Code_metadata.t;
    code_present : code_present
  }

module View = struct
  type t =
    | Code_present of Code.t
    | Metadata_only of Code_metadata.t
end

let view t =
  match t with
  | Code_present { code_status = Loaded code } -> View.Code_present code
  | Code_present ({ code_status = Not_loaded not_loaded } as c) ->
    let params_and_body, free_names_of_params_and_body =
      Obj.obj (File_sections.get not_loaded.sections not_loaded.index)
    in
    let params_and_body =
      Flambda.Function_params_and_body.apply_renaming params_and_body
        not_loaded.delayed_renaming
    in
    let free_names_of_params_and_body =
      Name_occurrences.apply_renaming free_names_of_params_and_body
        not_loaded.delayed_renaming
    in
    let code =
      Code.create_with_metadata ~params_and_body ~free_names_of_params_and_body
        ~code_metadata:not_loaded.metadata
    in
    c.code_status <- Loaded code;
    View.Code_present code
  | Metadata_only metadata -> View.Metadata_only metadata

let get_code t =
  match view t with
  | Code_present code -> code
  | Metadata_only metadata ->
    Misc.fatal_errorf
      "Code_or_metadata.get_code called but only metadata is available:@ %a"
      Code_metadata.print metadata

let print ppf t =
  match t with
  | Code_present { code_status = Loaded code } ->
    Format.fprintf ppf "@[<hov 1>(Code_present@ (@[<hov 1>(code@ %a)@]))@]"
      Code.print code
  | Code_present { code_status = Not_loaded not_loaded } ->
    Format.fprintf ppf
      "@[<hov 1>(Present@ (@[<hov 1>(code@ Not_loaded)@]@[<hov 1>(metadata@ \
       %a)@]))@]"
      Code_metadata.print not_loaded.metadata
  | Metadata_only code_metadata ->
    Format.fprintf ppf "@[<hov 1>(Metadata_only@ (code_metadata@ %a))@]"
      Code_metadata.print code_metadata

let code_status_metadata = function
  | Loaded code -> Code.code_metadata code
  | Not_loaded not_loaded -> not_loaded.metadata

let create code = Code_present { code_status = Loaded code }

let from_raw ~sections ~in_current_dir raw =
  match raw.code_present with
  | Absent ->
    let metadata =
      Code_metadata.adjust_for_current_dir raw.metadata in_current_dir
    in
    Metadata_only metadata
  | Present { index } ->
    let metadata =
      Code_metadata.adjust_for_current_dir raw.metadata in_current_dir
    in
    Code_present
      { code_status =
          Not_loaded
            { sections; index; metadata; delayed_renaming = Renaming.empty }
      }

let to_raw ~add_section t : raw =
  match view t with
  | Code_present code ->
    { metadata = Code.code_metadata code;
      code_present =
        Present
          { index =
              add_section
                (Obj.repr
                   ( Code.params_and_body code,
                     Code.free_names_of_params_and_body code ))
          }
    }
  | Metadata_only metadata -> { metadata; code_present = Absent }

let create_metadata_only metadata = Metadata_only metadata

let merge code_id t1 t2 =
  match t1, t2 with
  | Metadata_only cm1, Metadata_only cm2 ->
    if Code_metadata.approx_equal cm1 cm2
    then Some t1
    else
      Misc.fatal_errorf
        "Code id %a is imported with different code metadata (%a and %a)"
        Code_id.print code_id Code_metadata.print cm1 Code_metadata.print cm2
  | Code_present _, Code_present _ ->
    Misc.fatal_errorf "Cannot merge two definitions for code id %a"
      Code_id.print code_id
  | Metadata_only cm_imported, (Code_present { code_status } as t)
  | (Code_present { code_status } as t), Metadata_only cm_imported ->
    let cm_present = code_status_metadata code_status in
    if Code_metadata.approx_equal cm_present cm_imported
    then Some t
    else
      Misc.fatal_errorf
        "Code_id %a is present with code metadata@ %abut imported with code \
         metadata@ %a"
        Code_id.print code_id Code_metadata.print cm_present Code_metadata.print
        cm_imported

let free_names t =
  match view t with
  | Code_present code -> Code.free_names code
  | Metadata_only code_metadata -> Code_metadata.free_names code_metadata

let apply_renaming t renaming =
  match t with
  | Metadata_only code_metadata ->
    let code_metadata' = Code_metadata.apply_renaming code_metadata renaming in
    if code_metadata == code_metadata' then t else Metadata_only code_metadata'
  | Code_present { code_status = Loaded code } ->
    let code' = Code.apply_renaming code renaming in
    if code == code' then t else Code_present { code_status = Loaded code' }
  | Code_present { code_status = Not_loaded not_loaded } ->
    let metadata' = Code_metadata.apply_renaming not_loaded.metadata renaming in
    let delayed_renaming' =
      Renaming.compose ~second:renaming ~first:not_loaded.delayed_renaming
    in
    if metadata' == not_loaded.metadata
       && delayed_renaming' == not_loaded.delayed_renaming
    then t
    else
      Code_present
        { code_status =
            Not_loaded
              { not_loaded with
                metadata = metadata';
                delayed_renaming = delayed_renaming'
              }
        }

let ids_for_export t =
  match view t with
  | Code_present code -> Code.ids_for_export code
  | Metadata_only code_metadata -> Code_metadata.ids_for_export code_metadata

let remember_only_metadata t =
  match t with
  | Code_present { code_status } ->
    Metadata_only (code_status_metadata code_status)
  | Metadata_only _ -> t

let code_metadata t =
  match t with
  | Code_present { code_status } -> code_status_metadata code_status
  | Metadata_only code_metadata -> code_metadata

let iter_code t ~f =
  match view t with Code_present code -> f code | Metadata_only _ -> ()

let map_result_types t ~f =
  (* CR ncourant: we could probably do this without loading the code if it is
     not needed, but it doesn't seem necessary as this function seems to only be
     called before output *)
  match view t with
  | Code_present code ->
    Code_present { code_status = Loaded (Code.map_result_types code ~f) }
  | Metadata_only code_metadata ->
    Metadata_only (Code_metadata.map_result_types code_metadata ~f)

let code_present t =
  match t with Code_present _ -> true | Metadata_only _ -> false

let map_raw_index map_index t =
  match t.code_present with
  | Absent -> t
  | Present { index } ->
    { t with code_present = Present { index = map_index index } }
