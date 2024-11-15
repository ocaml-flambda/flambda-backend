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
  { free_names : Name_occurrences.t;
    final_typing_env : Typing_env.t option;
    all_code : Exported_code.t;
    slot_offsets : Slot_offsets.t;
    unit : Flambda_unit.t
  }

let run ~cmx_loader ~round ~code_slot_offsets unit =
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let toplevel_my_region = FU.toplevel_my_region unit in
  let toplevel_my_ghost_region = FU.toplevel_my_ghost_region unit in
  let module_symbol = FU.module_symbol unit in
  let resolver = Flambda_cmx.load_cmx_file_contents cmx_loader in
  let get_imported_names = Flambda_cmx.get_imported_names cmx_loader in
  let get_imported_code = Flambda_cmx.get_imported_code cmx_loader in
  let denv =
    DE.create ~round ~resolver ~get_imported_names ~get_imported_code
      ~propagating_float_consts:(Flambda_features.float_const_prop ())
      ~unit_toplevel_return_continuation:return_continuation
      ~unit_toplevel_exn_continuation:exn_continuation ~toplevel_my_region
      ~toplevel_my_ghost_region
  in
  (* CR gbury: only compute closure offsets if this is the last round. (same
     remark for the cmx contents) *)
  let dacc = DA.create denv code_slot_offsets Continuation_uses_env.empty in
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
              Variable.print var Expr.print body)
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
  let slot_offsets =
    match UA.slot_offsets uacc with
    | Unknown ->
      Misc.fatal_error "Slot offsets must be computed and cannot be unknown"
    | Known slot_offsets -> slot_offsets
  in
  let unit =
    FU.create ~return_continuation ~exn_continuation ~toplevel_my_region
      ~toplevel_my_ghost_region ~module_symbol ~body ~used_value_slots:Unknown
  in
  { unit;
    free_names = name_occurrences;
    final_typing_env;
    all_code;
    slot_offsets
  }
