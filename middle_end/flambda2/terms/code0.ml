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

[@@@ocaml.warning "+a-30-40-41-42"]

type 'function_params_and_body t =
  { params_and_body : 'function_params_and_body;
    free_names_of_params_and_body : Name_occurrences.t;
    code_metadata : Code_metadata.t
  }

let code_metadata t = t.code_metadata

let code_id t = Code_metadata.code_id t.code_metadata

let params_and_body t = t.params_and_body

let newer_version_of t = Code_metadata.newer_version_of t.code_metadata

let params_arity t = Code_metadata.params_arity t.code_metadata

let num_leading_heap_params t =
  Code_metadata.num_leading_heap_params t.code_metadata

let num_trailing_local_params t =
  Code_metadata.num_trailing_local_params t.code_metadata

let result_arity t = Code_metadata.result_arity t.code_metadata

let result_types t = Code_metadata.result_types t.code_metadata

let stub t = Code_metadata.stub t.code_metadata

let inline t = Code_metadata.inline t.code_metadata

let is_a_functor t = Code_metadata.is_a_functor t.code_metadata

let recursive t = Code_metadata.recursive t.code_metadata

let cost_metrics t = Code_metadata.cost_metrics t.code_metadata

let inlining_arguments t = Code_metadata.inlining_arguments t.code_metadata

let dbg t = Code_metadata.dbg t.code_metadata

let is_tupled t = Code_metadata.is_tupled t.code_metadata

let is_my_closure_used t = Code_metadata.is_my_closure_used t.code_metadata

let inlining_decision t = Code_metadata.inlining_decision t.code_metadata

let contains_no_escaping_local_allocs t =
  Code_metadata.contains_no_escaping_local_allocs t.code_metadata

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

let create ~print_function_params_and_body code_id ~params_and_body
    ~free_names_of_params_and_body ~newer_version_of ~params_arity
    ~num_trailing_local_params ~result_arity ~result_types
    ~contains_no_escaping_local_allocs ~stub ~(inline : Inline_attribute.t)
    ~is_a_functor ~recursive ~cost_metrics ~inlining_arguments ~dbg ~is_tupled
    ~is_my_closure_used ~inlining_decision =
  begin
    match stub, inline with
    | true, (Available_inline | Never_inline | Default_inline)
    | ( false,
        ( Never_inline | Default_inline | Always_inline | Available_inline
        | Unroll _ ) ) ->
      ()
    | true, (Always_inline | Unroll _) ->
      Misc.fatal_error
        "Stubs may not be annotated as [Always_inline] or [Unroll]"
  end;
  check_free_names_of_params_and_body ~print_function_params_and_body code_id
    ~params_and_body ~free_names_of_params_and_body;
  let code_metadata =
    Code_metadata.create code_id ~newer_version_of ~params_arity
      ~num_trailing_local_params ~result_arity ~result_types
      ~contains_no_escaping_local_allocs ~stub ~inline ~is_a_functor ~recursive
      ~cost_metrics ~inlining_arguments ~dbg ~is_tupled ~is_my_closure_used
      ~inlining_decision
  in
  { params_and_body; free_names_of_params_and_body; code_metadata }

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
  if params_and_body == params_and_body' && code_metadata == code_metadata'
  then t
  else
    let free_names_of_params_and_body' =
      Name_occurrences.apply_renaming free_names_of_params_and_body renaming
    in
    { params_and_body = params_and_body';
      free_names_of_params_and_body = free_names_of_params_and_body';
      code_metadata = code_metadata'
    }

let all_ids_for_export ~all_ids_for_export_function_params_and_body
    { params_and_body; free_names_of_params_and_body = _; code_metadata } =
  let params_and_body_ids =
    all_ids_for_export_function_params_and_body params_and_body
  in
  Ids_for_export.union
    (Code_metadata.all_ids_for_export code_metadata)
    params_and_body_ids

let map_result_types ({ code_metadata; _ } as t) ~f =
  { t with code_metadata = Code_metadata.map_result_types code_metadata ~f }
