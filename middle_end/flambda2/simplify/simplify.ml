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

let all_predefined_exception_symbols ~symbol_for_global =
  Predef.all_predef_exns
  |> List.map (fun ident ->
         symbol_for_global
           ?comp_unit:(Some (Compilation_unit.predefined_exception ()))
           ident)
  |> Symbol.Set.of_list

let predefined_exception_typing_env ~symbol_for_global ~resolver
    ~get_imported_names =
  let comp_unit = Compilation_unit.get_current_exn () in
  Compilation_unit.set_current (Compilation_unit.predefined_exception ());
  let typing_env =
    Symbol.Set.fold
      (fun sym typing_env ->
        TE.add_definition typing_env (Bound_name.symbol sym) K.value)
      (all_predefined_exception_symbols ~symbol_for_global)
      (TE.create ~resolver ~get_imported_names)
  in
  Compilation_unit.set_current comp_unit;
  typing_env

let run ~symbol_for_global ~get_global_info ~round unit =
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let module_symbol = FU.module_symbol unit in
  let imported_names = ref Name.Set.empty in
  let imported_code = ref Exported_code.empty in
  let imported_units = ref Compilation_unit.Map.empty in
  let resolver comp_unit =
    Flambda_cmx.load_cmx_file_contents ~get_global_info comp_unit
      ~imported_names ~imported_code ~imported_units
  in
  let get_imported_names () = !imported_names in
  let get_imported_code () = !imported_code in
  let predefined_exception_typing_env =
    predefined_exception_typing_env ~symbol_for_global ~resolver
      ~get_imported_names
  in
  imported_units
    := Compilation_unit.Map.add
         (Compilation_unit.predefined_exception ())
         (Some predefined_exception_typing_env) !imported_units;
  imported_names
    := Name.Set.union !imported_names
         (TE.name_domain predefined_exception_typing_env);
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
  let dacc =
    DA.create denv Continuation_uses_env.empty ~compute_closure_offsets:true
  in
  let body, uacc =
    Simplify_expr.simplify_toplevel dacc (FU.body unit) ~return_continuation
      ~return_arity:[K.With_subkind.any_value] ~exn_continuation
      ~return_cont_scope ~exn_cont_scope
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
    Continuation_uses_env.get_typing_env_no_more_than_one_use cont_uses_env
      return_continuation
  in
  let all_code =
    Exported_code.merge (UA.all_code uacc)
      (Exported_code.mark_as_imported !imported_code)
  in
  let name_occurrences = UA.name_occurrences uacc in
  let closure_ids_normal =
    Name_occurrences.closure_ids_normal name_occurrences
  in
  let closure_vars_normal =
    Name_occurrences.closure_vars_normal name_occurrences
  in
  let closure_ids_in_types =
    Name_occurrences.closure_ids_in_types name_occurrences
  in
  let closure_vars_in_types =
    Name_occurrences.closure_vars_in_types name_occurrences
  in
  let used_closure_vars, exported_offsets =
    match UA.closure_offsets uacc with
    | Unknown ->
      Misc.fatal_error "Closure offsets must be computed and cannot be unknown"
    | Known closure_offsets -> (
      let used_names : Closure_offsets.used_names Or_unknown.t =
        Known
          { closure_vars_normal;
            closure_ids_normal;
            closure_vars_in_types;
            closure_ids_in_types
          }
      in
      match Closure_offsets.finalize_offsets closure_offsets ~used_names with
      | Known used_closure_vars, offsets -> used_closure_vars, offsets
      | Unknown, _ ->
        (* could be an assert false *)
        Misc.fatal_error
          "Closure offsets should not have returned Unknown when given a known \
           used_names.")
  in
  let cmx =
    Flambda_cmx.prepare_cmx_file_contents ~final_typing_env ~module_symbol
      ~used_closure_vars ~exported_offsets all_code
  in
  let unit =
    FU.create ~return_continuation ~exn_continuation ~module_symbol ~body
      ~used_closure_vars:(Known used_closure_vars)
  in
  { cmx; unit; all_code; exported_offsets }
