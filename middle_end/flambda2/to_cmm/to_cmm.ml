(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Env = To_cmm_env
module R = To_cmm_result

(* Notes:

   - an int64 on a 32-bit host is represented across two registers, hence most
   operations on them will actually need to call C primitive that can handle
   them.

   - int32 on 64 bits are represented as an int64 in the range of 32-bit
   integers. Thus we insert sign extensions after every operation on 32-bits
   integers that may have a result outside of the range. *)

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

(* Get constant definitions from Cmmgen_state. (The To_cmm translation uses
   functions from Cmm_helpers which populate some mutable state in
   Cmmgen_state.) *)

let flush_cmm_helpers_state res =
  let aux name cst (res, acc) =
    match (cst : Cmmgen_state.constant) with
    | Const_table (global, l) ->
      let res, sym = R.raw_symbol res ~global name in
      res, C.cdata (C.define_symbol sym @ l) :: acc
  in
  (* reset the structured constants, just in case *)
  Cmmgen_state.clear_local_structured_constants ();
  match Cmmgen_state.get_and_clear_data_items () with
  | [] ->
    let cst_map = Cmmgen_state.get_and_clear_constants () in
    Misc.Stdlib.String.Map.fold aux cst_map (res, [])
  | _ ->
    Misc.fatal_errorf
      "There shouldn't be any data items in Cmmgen_state during Flambda 2 to \
       Cmm translation"

(* Note about the root (module block) symbol: it does not need any particular
   treatment. Specifically concerning its treatment as a GC root, it's like any
   other statically allocated symbol: if it has an associated computation, then
   it will already be included in the list of GC roots; otherwise it does not
   *have* to be a root. *)

let unit0 ~offsets ~all_code ~reachable_names ~code_ids_kept_for_zero_alloc
    flambda_unit =
  (* If someone wants to add 32-bit support in the future there will be a
     (merged) PR on ocaml-flambda/flambda-backend which can be used as a guide:
     https://github.com/ocaml-flambda/flambda-backend/pull/685 *)
  if Target_system.is_32_bit ()
  then
    Misc.fatal_error
      "Flambda 2 to Cmm conversion does not support 32-bit targets";
  let dummy_k = Continuation.create () in
  (* The dummy continuation is passed here since we're going to manually arrange
     that the return continuation turns into "return unit". (Module initialisers
     return the unit value). *)
  let env =
    Env.create offsets all_code ~return_continuation:dummy_k
      ~trans_prim:To_cmm_primitive.trans_prim
      ~exn_continuation:(Flambda_unit.exn_continuation flambda_unit)
      ~code_ids_kept_for_zero_alloc
  in
  let _env, return_cont_params =
    (* The environment is dropped because the handler for the dummy continuation
       (which just returns unit) doesn't use any of the parameters. *)
    C.continuation_bound_parameters env
      (Bound_parameters.create
         [ Bound_parameter.create (Variable.create "*ret*")
             Flambda_kind.With_subkind.any_value ])
  in
  let return_cont, env =
    Env.add_jump_cont env
      (Flambda_unit.return_continuation flambda_unit)
      ~param_types:(List.map snd return_cont_params)
  in
  (* See comment in [To_cmm_set_of_closures] about binding [my_region] *)
  let env, toplevel_region_var =
    Env.create_bound_parameter env
      (Flambda_unit.toplevel_my_region flambda_unit)
  in
  let r =
    R.create ~reachable_names
      ~module_symbol:(Flambda_unit.module_symbol flambda_unit)
  in
  let body, body_free_vars, res =
    To_cmm_expr.expr env r (Flambda_unit.body flambda_unit)
  in
  let free_vars =
    To_cmm_shared.remove_var_with_provenance body_free_vars toplevel_region_var
  in
  if not (Backend_var.Set.is_empty free_vars)
  then
    Misc.fatal_errorf
      "Unbound free_vars in module init code when translating to cmm: %a"
      Backend_var.Set.print free_vars;
  (* CR mshinwell: This should at least be given a source file location. *)
  let dbg = Debuginfo.none in
  let body =
    let unit_value = C.targetint ~dbg Targetint_32_64.one in
    C.create_ccatch ~rec_flag:false ~body
      ~handlers:
        [ C.handler ~dbg return_cont
            (C.remove_skipped_params return_cont_params)
            unit_value false ]
  in
  let body =
    if !Clflags.afl_instrument
    then Afl_instrument.instrument_initialiser body (fun () -> dbg)
    else body
  in
  let entry_name = Cmm_helpers.make_symbol "entry" in
  let res, entry_sym = R.raw_symbol res ~global:Global entry_name in
  let entry =
    let fun_codegen =
      let fun_codegen = [Cmm.Reduce_code_size; Cmm.Use_linscan_regalloc] in
      if Flambda_features.backend_cse_at_toplevel ()
      then fun_codegen
      else Cmm.No_CSE :: fun_codegen
    in
    C.cfunction
      (C.fundecl entry_sym [] body fun_codegen dbg Default_poll
         ~only_kept_for_zero_alloc:false)
  in
  let { R.data_items; gc_roots; functions } = R.to_cmm res in
  let _res, cmm_helpers_data = flush_cmm_helpers_state res in
  let gc_root_data = C.gc_root_table gc_roots in
  (gc_root_data :: data_items) @ cmm_helpers_data @ functions @ [entry]

let unit ~offsets ~all_code ~reachable_names ~code_ids_kept_for_zero_alloc
    flambda_unit =
  Profile.record_call "flambda_to_cmm" (fun () ->
      unit0 ~offsets ~all_code ~reachable_names ~code_ids_kept_for_zero_alloc
        flambda_unit)
