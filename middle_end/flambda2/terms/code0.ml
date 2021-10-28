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

module Params_and_body_state = struct
  type 'function_params_and_body t =
    | Inlinable of 'function_params_and_body
    | Non_inlinable
    | Cannot_be_called

  let inlinable params_and_body = Inlinable params_and_body

  (* This function is deliberately not exposed in the .mli to make it clear that
     the transition from inlinable to non-inlinable only happens when
     [make_non_inlinable] (see below) is called. *)
  let non_inlinable = Non_inlinable

  let cannot_be_called = Cannot_be_called

  let map t ~f =
    match t with
    | Inlinable params_and_body -> Inlinable (f params_and_body)
    | Non_inlinable -> Non_inlinable
    | Cannot_be_called -> Cannot_be_called

  let print print_params_and_body ppf t =
    match t with
    | Inlinable params_and_body ->
      Format.fprintf ppf "@[<hov 1>(Inlinable@ %a)@]" print_params_and_body
        params_and_body
    | Non_inlinable -> Format.fprintf ppf "Non_inlinable"
    | Cannot_be_called -> Format.fprintf ppf "Cannot_be_called"
end

type 'function_params_and_body t =
  { params_and_body : 'function_params_and_body Params_and_body_state.t;
    free_names_of_params_and_body : Name_occurrences.t;
    code_metadata : Code_metadata.t
  }

let code_metadata t = t.code_metadata

let code_id t = Code_metadata.code_id t.code_metadata

let params_and_body t = t.params_and_body

let newer_version_of t = Code_metadata.newer_version_of t.code_metadata

let params_arity t = Code_metadata.params_arity t.code_metadata

let result_arity t = Code_metadata.result_arity t.code_metadata

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

let check_params_and_body ~print_function_params_and_body code_id
    (params_and_body : _ Params_and_body_state.t) =
  let free_names_of_params_and_body =
    match params_and_body with
    | Cannot_be_called | Non_inlinable -> Name_occurrences.empty
    | Inlinable (params_and_body, free_names_of_params_and_body) ->
      if not
           (Name_occurrences.no_continuations free_names_of_params_and_body
           && Name_occurrences.no_variables free_names_of_params_and_body)
      then
        Misc.fatal_errorf
          "Incorrect free names:@ %a@ for creation of code:@ %a@ =@ %a"
          Name_occurrences.print free_names_of_params_and_body Code_id.print
          code_id print_function_params_and_body params_and_body;
      free_names_of_params_and_body
  in
  let params_and_body : _ Params_and_body_state.t =
    match params_and_body with
    | Cannot_be_called -> Cannot_be_called
    | Non_inlinable -> Non_inlinable
    | Inlinable (params_and_body, _) -> Inlinable params_and_body
  in
  params_and_body, free_names_of_params_and_body

let create ~print_function_params_and_body code_id
    ~(params_and_body : _ Params_and_body_state.t) ~newer_version_of
    ~params_arity ~result_arity ~stub ~(inline : Inline_attribute.t)
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
  let params_and_body, free_names_of_params_and_body =
    check_params_and_body ~print_function_params_and_body code_id
      params_and_body
  in
  let code_metadata =
    Code_metadata.create code_id ~newer_version_of ~params_arity ~result_arity
      ~stub ~inline ~is_a_functor ~recursive ~cost_metrics ~inlining_arguments
      ~dbg ~is_tupled ~is_my_closure_used ~inlining_decision
  in
  { params_and_body; free_names_of_params_and_body; code_metadata }

let make_non_inlinable t =
  match t.params_and_body with
  | Inlinable _ ->
    { t with
      params_and_body = Params_and_body_state.non_inlinable;
      free_names_of_params_and_body = Name_occurrences.empty
    }
  | Non_inlinable -> t
  | Cannot_be_called ->
    Misc.fatal_errorf
      "A piece of code in [Cannot_be_called] state cannot be transitioned to \
       [Non_inlinable] state"

let with_code_id code_id t =
  { t with code_metadata = Code_metadata.with_code_id code_id t.code_metadata }

let with_params_and_body ~print_function_params_and_body params_and_body
    ~cost_metrics t =
  let params_and_body, free_names_of_params_and_body =
    check_params_and_body ~print_function_params_and_body (code_id t)
      params_and_body
  in
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
    Code_metadata.print code_metadata
    (Params_and_body_state.print print_function_params_and_body)
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
    match params_and_body with
    | Cannot_be_called -> Params_and_body_state.cannot_be_called
    | Non_inlinable -> Params_and_body_state.non_inlinable
    | Inlinable params_and_body_inner ->
      let params_and_body_inner' =
        apply_renaming_function_params_and_body params_and_body_inner renaming
      in
      if params_and_body_inner == params_and_body_inner'
      then params_and_body
      else Params_and_body_state.inlinable params_and_body_inner'
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
    { params_and_body; free_names_of_params_and_body; code_metadata } =
  let params_and_body_ids =
    match params_and_body with
    | Cannot_be_called -> Ids_for_export.empty
    | Inlinable params_and_body ->
      all_ids_for_export_function_params_and_body params_and_body
    | Non_inlinable ->
      (* Usually the ids for export collected from the [params_and_body] in the
         inlinable case are used to rename [free_names_of_params_and_body] upon
         import. Since we don't know what those ids for export are in the
         non-inlinable case, we double-check that the
         [free_names_of_params_and_body] field has been correctly cleared. *)
      assert (Name_occurrences.is_empty free_names_of_params_and_body);
      Ids_for_export.empty
  in
  Ids_for_export.union
    (Code_metadata.all_ids_for_export code_metadata)
    params_and_body_ids

let make_not_callable t =
  { t with
    params_and_body = Cannot_be_called;
    free_names_of_params_and_body = Name_occurrences.empty
  }

let is_non_callable t =
  match t.params_and_body with
  | Cannot_be_called -> true
  | Inlinable _ | Non_inlinable -> false
