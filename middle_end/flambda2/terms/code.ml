(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Flambda.Function_params_and_body.t Code0.t

let code_metadata = Code0.code_metadata

let params_and_body = Code0.params_and_body

module Metadata_view = struct
  type nonrec 'a t = t

  let metadata = code_metadata
end

include Code_metadata.Code_metadata_accessors [@inlined hint] (Metadata_view)

let create_with_metadata =
  Code0.create_with_metadata
    ~print_function_params_and_body:Flambda.Function_params_and_body.print

let create =
  Code0.create
    ~print_function_params_and_body:Flambda.Function_params_and_body.print

let with_code_id = Code0.with_code_id

let with_params_and_body =
  Code0.with_params_and_body
    ~print_function_params_and_body:Flambda.Function_params_and_body.print

let with_newer_version_of = Code0.with_newer_version_of

let free_names = Code0.free_names

let apply_renaming =
  Code0.apply_renaming
    ~apply_renaming_function_params_and_body:
      Flambda.Function_params_and_body.apply_renaming

let print =
  Code0.print
    ~print_function_params_and_body:Flambda.Function_params_and_body.print

let ids_for_export =
  Code0.ids_for_export
    ~ids_for_export_function_params_and_body:
      Flambda.Function_params_and_body.ids_for_export

let map_result_types = Code0.map_result_types

let free_names_of_params_and_body = Code0.free_names_of_params_and_body
