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

open! Simplify_import

type simplify_result =
  { cmx : Flambda_cmx_format.t option;
    unit : Flambda_unit.t;
    all_code : Exported_code.t;
    exported_offsets : Exported_offsets.t;
    reachable_names : Name_occurrences.t
  }

let run ~cmx_loader ~round unit =
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let toplevel_my_region = FU.toplevel_my_region unit in
  let module_symbol = FU.module_symbol unit in
  let resolver = Flambda_cmx.load_cmx_file_contents cmx_loader in
  let get_imported_names = Flambda_cmx.get_imported_names cmx_loader in
  let get_imported_code = Flambda_cmx.get_imported_code cmx_loader in
  let denv =
    DE.create ~round ~resolver ~get_imported_names ~get_imported_code
      ~propagating_float_consts:(Flambda_features.float_const_prop ())
      ~unit_toplevel_return_continuation:return_continuation
      ~unit_toplevel_exn_continuation:exn_continuation ~toplevel_my_region
  in
  (* CR gbury: only compute closure offsets if this is the last round. (same
     remark for the cmx contents) *)
  let dacc = DA.create denv Continuation_uses_env.empty in
  let body, uacc =
    Simplify_expr.simplify_toplevel dacc (FU.body unit) ~return_continuation
      ~return_arity:(Flambda_arity.create_singletons [K.With_subkind.any_value])
      ~exn_continuation
  in
  let body = Rebuilt_expr.to_expr body (UA.are_rebuilding_terms uacc) in
  let name_occurrences = UA.name_occurrences uacc in
  NO.fold_names name_occurrences ~init:() ~f:(fun () name ->
      Name.pattern_match name
        ~var:(fun var ->
          if not (Variable.equal var toplevel_my_region)
          then
            Misc.fatal_errorf
              "Variable %a not expected to be free in whole-compilation-unit \
               term:@ %a"
              Variable.print var Expr.print body
          else Misc.fatal_errorf "toplevel_my_region is free")
        ~symbol:(fun _symbol -> ()));
  if not (LCS.is_empty (UA.lifted_constants uacc))
  then
    Misc.fatal_errorf "Lifted constants accumulator should be empty:@ %a"
      UA.print uacc;
  let final_typing_env =
    let cont_uses_env = DA.continuation_uses_env (UA.creation_dacc uacc) in
    Continuation_uses_env.get_typing_env_no_more_than_one_use cont_uses_env
      return_continuation
  in
  let all_code =
    Exported_code.merge (UA.all_code uacc)
      (Exported_code.mark_as_imported (get_imported_code ()))
  in
  let name_occurrences = UA.name_occurrences uacc in
  let function_slots_in_normal_projections =
    NO.function_slots_in_normal_projections name_occurrences
  in
  let value_slots_in_normal_projections =
    NO.value_slots_in_normal_projections name_occurrences
  in
  let all_function_slots = NO.all_function_slots name_occurrences in
  let all_value_slots = NO.all_value_slots name_occurrences in
  let ({ used_value_slots; exported_offsets } : Slot_offsets.result) =
    match UA.slot_offsets uacc with
    | Unknown ->
      Misc.fatal_error "Slot offsets must be computed and cannot be unknown"
    | Known slot_offsets ->
      let used_slots : Slot_offsets.used_slots =
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
      Slot_offsets.finalize_offsets slot_offsets ~get_code_metadata ~used_slots
  in
  let reachable_names, cmx =
    Flambda_cmx.prepare_cmx_file_contents ~final_typing_env ~module_symbol
      ~used_value_slots ~exported_offsets all_code
  in
  let unit =
    FU.create ~return_continuation ~exn_continuation ~toplevel_my_region
      ~module_symbol ~body ~used_value_slots:(Known used_value_slots)
  in
  { cmx; unit; all_code; exported_offsets; reachable_names }
