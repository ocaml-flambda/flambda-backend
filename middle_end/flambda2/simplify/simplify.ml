(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type simplify_result =
  { cmx : Flambda_cmx_format.t option;
    unit : Flambda_unit.t;
    all_code : Exported_code.t;
    exported_offsets : Exported_offsets.t
  }

let run ~cmx_loader ~round unit =
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let module_symbol = FU.module_symbol unit in
  let resolver = Flambda_cmx.load_cmx_file_contents cmx_loader in
  let get_imported_names = Flambda_cmx.get_imported_names cmx_loader in
  let get_imported_code = Flambda_cmx.get_imported_code cmx_loader in
  let denv =
    DE.create ~round ~resolver ~get_imported_names ~get_imported_code
      ~float_const_prop:(Flambda_features.float_const_prop ())
      ~unit_toplevel_return_continuation:return_continuation
      ~unit_toplevel_exn_continuation:exn_continuation
  in
  let return_cont_scope = DE.get_continuation_scope denv in
  let denv = DE.increment_continuation_scope denv in
  let exn_cont_scope = DE.get_continuation_scope denv in
  let denv = DE.increment_continuation_scope denv in
  (* CR gbury: only compute closure offsets if this is the last round. (same
     remark for the cmx contents) *)
  let dacc = DA.create denv Continuation_uses_env.empty in
  let body, uacc =
    Simplify_expr.simplify_toplevel dacc (FU.body unit) ~return_continuation
      ~return_arity:
        (Flambda_arity.With_subkinds.create [K.With_subkind.any_value])
      ~exn_continuation ~return_cont_scope ~exn_cont_scope
  in
  let body = Rebuilt_expr.to_expr body (UA.are_rebuilding_terms uacc) in
  let name_occurrences = UA.name_occurrences uacc in
  Name_occurrences.fold_names name_occurrences ~init:() ~f:(fun () name ->
      Name.pattern_match name
        ~var:(fun var ->
          Misc.fatal_errorf
            "Variable %a not expected to be free in whole-compilation-unit \
             term:@ %a"
            Variable.print var Expr.print body)
        ~symbol:(fun _symbol -> ()));
  let final_typing_env =
    let cont_uses_env = DA.continuation_uses_env (UA.creation_dacc uacc) in
    match
      Continuation_uses_env.get_typing_env_no_more_than_one_use cont_uses_env
        return_continuation
    with
    | Some typing_env -> typing_env
    | None -> TE.create ~resolver ~get_imported_names
  in
  let all_code =
    Exported_code.merge (UA.all_code uacc)
      (Exported_code.mark_as_imported (get_imported_code ()))
  in
  let name_occurrences = UA.name_occurrences uacc in
  let function_slots_in_normal_projections =
    Name_occurrences.function_slots_in_normal_projections name_occurrences
  in
  let value_slots_in_normal_projections =
    Name_occurrences.value_slots_in_normal_projections name_occurrences
  in
  let all_function_slots =
    Name_occurrences.all_function_slots name_occurrences
  in
  let all_value_slots = Name_occurrences.all_value_slots name_occurrences in
  let used_value_slots, exported_offsets =
    match UA.slot_offsets uacc with
    | Unknown ->
      Misc.fatal_error "Slot offsets must be computed and cannot be unknown"
    | Known slot_offsets -> (
      let used_slots : Slot_offsets.used_slots Or_unknown.t =
        Known
          { function_slots_in_normal_projections;
            all_function_slots;
            value_slots_in_normal_projections;
            all_value_slots
          }
      in
      let get_code_metadata code_id =
        Exported_code.find_exn all_code code_id
        |> Code_or_metadata.code_metadata
      in
      match
        Slot_offsets.finalize_offsets slot_offsets ~get_code_metadata
          ~used_slots
      with
      | Known used_value_slots, offsets -> used_value_slots, offsets
      | Unknown, _ ->
        (* could be an assert false *)
        Misc.fatal_error
          "Slot_offsets should not have returned Unknown when given a Known \
           used_names.")
  in
  let cmx =
    Flambda_cmx.prepare_cmx_file_contents ~final_typing_env ~module_symbol
      ~used_value_slots ~exported_offsets all_code
  in
  let unit =
    FU.create ~return_continuation ~exn_continuation ~module_symbol ~body
      ~used_value_slots:(Known used_value_slots)
  in
  { cmx; unit; all_code; exported_offsets }
