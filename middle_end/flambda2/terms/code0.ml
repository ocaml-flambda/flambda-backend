(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'function_params_and_body t =
  { params_and_body : 'function_params_and_body;
    free_names_of_params_and_body : Name_occurrences.t;
    code_metadata : Code_metadata.t
  }

let code_metadata t = t.code_metadata

module Metadata_view = struct
  type nonrec 'function_params_and_body t = 'function_params_and_body t

  let metadata t = t.code_metadata
end

include Code_metadata.Code_metadata_accessors [@inlined hint] (Metadata_view)

let params_and_body t = t.params_and_body

let check_free_names_of_params_and_body ~print_function_params_and_body code_id
    ~params_and_body ~free_names_of_params_and_body =
  if not
       (Name_occurrences.no_continuations free_names_of_params_and_body
       && Name_occurrences.no_variables free_names_of_params_and_body)
  then
    Misc.fatal_errorf
      "Incorrect free names:@ %a@ for creation of code:@ %a@ =@ %a"
      Name_occurrences.print free_names_of_params_and_body Code_id.print code_id
      print_function_params_and_body params_and_body

let create_with_metadata ~print_function_params_and_body ~params_and_body
    ~free_names_of_params_and_body ~code_metadata =
  check_free_names_of_params_and_body ~print_function_params_and_body
    (Code_metadata.code_id code_metadata)
    ~params_and_body ~free_names_of_params_and_body;
  { params_and_body; free_names_of_params_and_body; code_metadata }

let create ~print_function_params_and_body ~params_and_body
    ~free_names_of_params_and_body =
  Code_metadata.createk (fun code_metadata ->
      create_with_metadata ~print_function_params_and_body ~params_and_body
        ~free_names_of_params_and_body ~code_metadata)

let with_code_id code_id t =
  { t with code_metadata = Code_metadata.with_code_id code_id t.code_metadata }

let with_params_and_body ~print_function_params_and_body ~params_and_body
    ~free_names_of_params_and_body ~cost_metrics t =
  check_free_names_of_params_and_body ~print_function_params_and_body
    (code_id t) ~params_and_body ~free_names_of_params_and_body;
  let code_metadata =
    Code_metadata.with_cost_metrics cost_metrics t.code_metadata
  in
  { params_and_body; code_metadata; free_names_of_params_and_body }

let with_newer_version_of newer_version_of t =
  { t with
    code_metadata =
      Code_metadata.with_newer_version_of newer_version_of t.code_metadata
  }

let print ~print_function_params_and_body ppf
    { params_and_body; code_metadata; free_names_of_params_and_body = _ } =
  Format.fprintf ppf "@[<hov 1>(@[<hov 1>(code_metadata@ %a)@]@ %a)@]"
    Code_metadata.print code_metadata print_function_params_and_body
    params_and_body

let compare t1 t2 = Code_id.compare (code_id t1) (code_id t2)

let free_names
    { params_and_body = _; free_names_of_params_and_body; code_metadata } =
  Name_occurrences.union
    (Code_metadata.free_names code_metadata)
    free_names_of_params_and_body

let apply_renaming ~apply_renaming_function_params_and_body
    ({ params_and_body; free_names_of_params_and_body; code_metadata } as t)
    renaming =
  let code_metadata' = Code_metadata.apply_renaming code_metadata renaming in
  let params_and_body' =
    apply_renaming_function_params_and_body params_and_body renaming
  in
  let free_names_of_params_and_body' =
    (* See note in [ids_for_export], below. *)
    Name_occurrences.apply_renaming free_names_of_params_and_body renaming
  in
  if params_and_body == params_and_body'
     && code_metadata == code_metadata'
     && free_names_of_params_and_body == free_names_of_params_and_body'
  then t
  else
    { params_and_body = params_and_body';
      free_names_of_params_and_body = free_names_of_params_and_body';
      code_metadata = code_metadata'
    }

let ids_for_export ~ids_for_export_function_params_and_body
    { params_and_body; free_names_of_params_and_body; code_metadata } =
  let params_and_body_ids =
    ids_for_export_function_params_and_body params_and_body
  in
  let free_names_of_params_and_body_ids =
    Name_occurrences.ids_for_export free_names_of_params_and_body
  in
  (* [free_names_of_params_and_body] is allowed to be an over-approximation, so
     we must count it. *)
  Ids_for_export.union_list
    [ Code_metadata.ids_for_export code_metadata;
      params_and_body_ids;
      free_names_of_params_and_body_ids ]

let map_result_types ({ code_metadata; _ } as t) ~f =
  { t with code_metadata = Code_metadata.map_result_types code_metadata ~f }

let free_names_of_params_and_body t = t.free_names_of_params_and_body
