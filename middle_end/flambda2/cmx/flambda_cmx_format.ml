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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Contents of middle-end-specific portion of .cmx files when using Flambda. *)

type table_data =
  { symbols : (Symbol.t * Symbol.exported) list;
    variables : (Variable.t * Variable.exported) list;
    simples : (Simple.t * Simple.exported) list;
    consts : (Reg_width_const.t * Reg_width_const.exported) list;
    code_ids : (Code_id.t * Code_id.exported) list;
    continuations : (Continuation.t * Continuation.exported) list
  }

type t0 =
  { original_compilation_unit : Compilation_unit.t;
    final_typing_env : Flambda2_types.Typing_env.Serializable.t;
    all_code : Exported_code.t;
    exported_offsets : Exported_offsets.t;
    used_value_slots : Value_slot.Set.t;
    table_data : table_data option
  }

type t = t0 list

let create ~final_typing_env ~all_code ~exported_offsets ~used_value_slots =
  [ { original_compilation_unit = Compilation_unit.get_current_exn ();
      final_typing_env;
      all_code;
      exported_offsets;
      used_value_slots;
      table_data = None
    } ]

let prepare_for_serialization0 t =
  let typing_env_exported_ids =
    Flambda2_types.Typing_env.Serializable.all_ids_for_export t.final_typing_env
  in
  let all_code_exported_ids = Exported_code.all_ids_for_export t.all_code in
  let exported_offsets_ids =
    Exported_offsets.all_ids_for_export t.exported_offsets
  in
  let exported_ids =
    Ids_for_export.union typing_env_exported_ids
      (Ids_for_export.union all_code_exported_ids exported_offsets_ids)
  in
  let symbols =
    Symbol.Set.fold
      (fun symbol symbols -> (symbol, Symbol.export symbol) :: symbols)
      exported_ids.symbols []
  in
  let variables =
    Variable.Set.fold
      (fun variable variables ->
        (variable, Variable.export variable) :: variables)
      exported_ids.variables []
  in
  let simples =
    Simple.Set.fold
      (fun simple simples -> (simple, Simple.export simple) :: simples)
      exported_ids.simples []
  in
  let consts =
    Reg_width_const.Set.fold
      (fun const consts -> (const, Reg_width_const.export const) :: consts)
      exported_ids.consts []
  in
  let code_ids =
    Code_id.Set.fold
      (fun code_id code_ids -> (code_id, Code_id.export code_id) :: code_ids)
      exported_ids.code_ids []
  in
  let continuations =
    Continuation.Set.fold
      (fun continuation continuations ->
        (continuation, Continuation.export continuation) :: continuations)
      exported_ids.continuations []
  in
  let table_data =
    { symbols; variables; simples; consts; code_ids; continuations }
  in
  { t with table_data = Some table_data }

let prepare_for_serialization t = List.map prepare_for_serialization0 t

module Make_importer (S : sig
  type t

  type exported

  val import : exported -> t

  val map_compilation_unit :
    (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported

  include Container_types.S with type t := t
end) : sig
  val import : (S.t * S.exported) list -> S.t S.Map.t

  val update_for_pack :
    pack_units:Compilation_unit.Set.t ->
    pack:Compilation_unit.t ->
    (S.t * S.exported) list ->
    (S.t * S.exported) list
end = struct
  let import from_table_data =
    (* The returned map gives the hash collisions. *)
    List.fold_left
      (fun import_map (key, exported) ->
        let new_key = S.import exported in
        if key == new_key then import_map else S.Map.add key new_key import_map)
      S.Map.empty from_table_data

  let update_for_pack ~pack_units ~pack from_table_data =
    let update_cu unit =
      if Compilation_unit.Set.mem unit pack_units then pack else unit
    in
    List.map
      (fun (symbol, exported) ->
        let exported = S.map_compilation_unit update_cu exported in
        symbol, exported)
      from_table_data
end
[@@inline always]

module Symbol_importer = Make_importer (Symbol)
module Variable_importer = Make_importer (Variable)
module Simple_importer = Make_importer (Simple)
module Const_importer = Make_importer (Reg_width_const)
module Code_id_importer = Make_importer (Code_id)
module Continuation_importer = Make_importer (Continuation)

let check_was_prepared_for_serialization t =
  match t.table_data with
  | Some table_data -> table_data
  | None ->
    Misc.fatal_errorf
      "[Flambda_cmx_format.t] value for %a was not prepared for serialization"
      Compilation_unit.print t.original_compilation_unit

let import_typing_env_and_code0 t =
  let table_data = check_was_prepared_for_serialization t in
  let symbols = Symbol_importer.import table_data.symbols in
  let variables = Variable_importer.import table_data.variables in
  let simples = Simple_importer.import table_data.simples in
  let consts = Const_importer.import table_data.consts in
  let code_ids = Code_id_importer.import table_data.code_ids in
  let continuations = Continuation_importer.import table_data.continuations in
  let used_value_slots = t.used_value_slots in
  let original_compilation_unit = t.original_compilation_unit in
  let renaming =
    Renaming.create_import_map ~symbols ~variables ~simples ~consts ~code_ids
      ~continuations ~used_value_slots ~original_compilation_unit
  in
  let typing_env =
    Flambda2_types.Typing_env.Serializable.apply_renaming t.final_typing_env
      renaming
  in
  let all_code = Exported_code.apply_renaming code_ids renaming t.all_code in
  let exported_offsets =
    Exported_offsets.apply_renaming t.exported_offsets renaming
  in
  typing_env, all_code, exported_offsets

let import_typing_env_and_code t =
  match t with
  | [] -> Misc.fatal_error "Flambda cmx info should never be empty"
  | [t0] -> import_typing_env_and_code0 t0
  | t0 :: rem ->
    List.fold_left
      (fun (typing_env, code, exported_offsets) t0 ->
        let typing_env0, code0, exported_offsets0 =
          import_typing_env_and_code0 t0
        in
        let typing_env =
          Flambda2_types.Typing_env.Serializable.merge typing_env typing_env0
        in
        let code = Exported_code.merge code code0 in
        let exported_offsets =
          Exported_offsets.merge exported_offsets exported_offsets0
        in
        typing_env, code, exported_offsets)
      (import_typing_env_and_code0 t0)
      rem

let functions_info t =
  List.fold_left
    (fun code t0 -> Exported_code.merge code t0.all_code)
    Exported_code.empty t

let with_exported_offsets t exported_offsets =
  match t with
  | [t0] -> [{ t0 with exported_offsets }]
  | [] | _ :: _ :: _ ->
    Misc.fatal_error "Cannot set exported offsets on multiple units"

let update_for_pack0 ~pack_units ~pack t =
  let table_data = check_was_prepared_for_serialization t in
  let symbols =
    Symbol_importer.update_for_pack ~pack_units ~pack table_data.symbols
  in
  let variables =
    Variable_importer.update_for_pack ~pack_units ~pack table_data.variables
  in
  let simples =
    Simple_importer.update_for_pack ~pack_units ~pack table_data.simples
  in
  let consts =
    Const_importer.update_for_pack ~pack_units ~pack table_data.consts
  in
  let code_ids =
    Code_id_importer.update_for_pack ~pack_units ~pack table_data.code_ids
  in
  let continuations =
    Continuation_importer.update_for_pack ~pack_units ~pack
      table_data.continuations
  in
  let table_data =
    { symbols; variables; simples; consts; code_ids; continuations }
  in
  { t with table_data = Some table_data }

let update_for_pack ~pack_units ~pack t_opt =
  match t_opt with
  | None -> None
  | Some t -> Some (List.map (update_for_pack0 ~pack_units ~pack) t)

let merge t1_opt t2_opt =
  match t1_opt, t2_opt with
  | None, None -> None
  | Some _, None | None, Some _ ->
    (* CR vlaviron: turn this into a proper user error *)
    Misc.fatal_error
      "Some pack units do not have their export info set.\n\
       Flambda doesn't support packing opaque and normal units together."
  | Some t1, Some t2 -> Some (t1 @ t2)

let print0 ppf t =
  Format.fprintf ppf "@[<hov>Original unit:@ %a@]@;" Compilation_unit.print
    t.original_compilation_unit;
  Compilation_unit.set_current t.original_compilation_unit;
  let typing_env, code, exported_offsets = import_typing_env_and_code0 t in
  Format.fprintf ppf "@[<hov>Typing env:@ %a@]@;"
    Flambda2_types.Typing_env.Serializable.print typing_env;
  Format.fprintf ppf "@[<hov>Code:@ %a@]@;" Exported_code.print code;
  Format.fprintf ppf "@[<hov>Offsets:@ %a@]@;" Exported_offsets.print
    exported_offsets

let [@ocamlformat "disable"] print ppf t =
  let rec print_rest ppf = function
    | [] -> ()
    | t0 :: t ->
      Format.fprintf ppf "@ (%a)"
        print0 t0;
      print_rest ppf t
  in
  match t with
  | [] -> assert false
  | [ t0 ] -> print0 ppf t0
  | t0 :: t ->
    Format.fprintf ppf "Packed units:@ @[<v>(%a)%a@]"
      print0 t0 print_rest t
