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
    all_code : Exported_code.raw;
    exported_offsets : Exported_offsets.t;
    used_value_slots : Value_slot.Set.t;
    table_data : table_data
  }

type t = t0 list

type current_sections =
  {
    mutable sections_rev : Obj.t list;
    mutable num_sections : int
  }

let add_section cs section =
  let n = cs.num_sections in
  cs.sections_rev <- (Obj.repr section) :: cs.sections_rev;
  cs.num_sections <- n + 1;
  n

let create ~final_typing_env ~all_code ~exported_offsets ~used_value_slots =
  let typing_env_exported_ids =
    Flambda2_types.Typing_env.Serializable.ids_for_export final_typing_env
  in
  let all_code_exported_ids = Exported_code.ids_for_export all_code in
  let exported_ids =
    Ids_for_export.union typing_env_exported_ids all_code_exported_ids
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
  let sections = { sections_rev = []; num_sections = 0 } in
  let all_code = Exported_code.to_raw ~add_section:(add_section sections) all_code in
  [ { original_compilation_unit = Compilation_unit.get_current_exn ();
      final_typing_env;
      all_code;
      exported_offsets;
      used_value_slots;
      table_data
    } ], Array.of_list (List.rev sections.sections_rev)

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

let import_typing_env_and_code0 ~compilation_unit t =
  let symbols = Symbol_importer.import t.table_data.symbols in
  let variables = Variable_importer.import t.table_data.variables in
  let simples = Simple_importer.import t.table_data.simples in
  let consts = Const_importer.import t.table_data.consts in
  let code_ids = Code_id_importer.import t.table_data.code_ids in
  let continuations = Continuation_importer.import t.table_data.continuations in
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
  let all_code = Exported_code.from_raw ~compilation_unit t.all_code in
  let all_code = Exported_code.apply_renaming code_ids renaming all_code in
  typing_env, all_code

let import_typing_env_and_code ~compilation_unit t =
  match t with
  | [] -> Misc.fatal_error "Flambda cmx info should never be empty"
  | [t0] -> import_typing_env_and_code0 ~compilation_unit t0
  | t0 :: rem ->
    List.fold_left
      (fun (typing_env, code) t0 ->
        let typing_env0, code0 = import_typing_env_and_code0 ~compilation_unit t0 in
        let typing_env =
          Flambda2_types.Typing_env.Serializable.merge typing_env typing_env0
        in
        let code = Exported_code.merge code code0 in
        typing_env, code)
      (import_typing_env_and_code0 ~compilation_unit t0)
      rem

let exported_offsets t =
  List.fold_left
    (fun offsets t0 -> Exported_offsets.merge offsets t0.exported_offsets)
    Exported_offsets.empty t

(*
let functions_info t =
  List.fold_left
    (fun code t0 -> Exported_code.merge code t0.all_code)
    Exported_code.empty t
*)

let with_exported_offsets t exported_offsets =
  match t with
  | [t0] -> [{ t0 with exported_offsets }]
  | [] | _ :: _ :: _ ->
    Misc.fatal_error "Cannot set exported offsets on multiple units"

let update_for_pack0 ~pack_units ~pack t =
  let symbols =
    Symbol_importer.update_for_pack ~pack_units ~pack t.table_data.symbols
  in
  let variables =
    Variable_importer.update_for_pack ~pack_units ~pack t.table_data.variables
  in
  let simples =
    Simple_importer.update_for_pack ~pack_units ~pack t.table_data.simples
  in
  let consts =
    Const_importer.update_for_pack ~pack_units ~pack t.table_data.consts
  in
  let code_ids =
    Code_id_importer.update_for_pack ~pack_units ~pack t.table_data.code_ids
  in
  let continuations =
    Continuation_importer.update_for_pack ~pack_units ~pack
      t.table_data.continuations
  in
  let table_data =
    { symbols; variables; simples; consts; code_ids; continuations }
  in
  { t with table_data }

let update_for_pack ~pack_units ~pack (t_opt, sections) =
  match t_opt with
  | None -> None, sections
  | Some t -> Some (List.map (update_for_pack0 ~pack_units ~pack) t), sections

let merge (t1_opt, sections1) (t2_opt, sections2) =
  match t1_opt, t2_opt with
  | None, None -> None, [||]
  | Some _, None | None, Some _ ->
    (* CR vlaviron: turn this into a proper user error *)
    Misc.fatal_error
      "Some pack units do not have their export info set.\n\
       Flambda doesn't support packing opaque and normal units together."
  | Some t1, Some t2 ->
    let t2 = List.map (fun t0 -> {
      t0 with all_code = Exported_code.map_raw_index (fun x -> x + Array.length sections1) t0.all_code
    }) t2 in
    Some (t1 @ t2), Array.append sections1 sections2

let print0 ~compilation_unit ppf t =
  Format.fprintf ppf "@[<hov>Original unit:@ %a@]@;" Compilation_unit.print
    t.original_compilation_unit;
  Compilation_unit.set_current t.original_compilation_unit;
  let typing_env, code = import_typing_env_and_code0 ~compilation_unit t in
  Format.fprintf ppf "@[<hov>Typing env:@ %a@]@;"
    Flambda2_types.Typing_env.Serializable.print typing_env;
  Format.fprintf ppf "@[<hov>Code:@ %a@]@;" Exported_code.print code;
  Format.fprintf ppf "@[<hov>Offsets:@ %a@]@;" Exported_offsets.print
    t.exported_offsets

let [@ocamlformat "disable"] print ~compilation_unit ppf t =
  let rec print_rest ppf = function
    | [] -> ()
    | t0 :: t ->
      Format.fprintf ppf "@ (%a)"
        (print0 ~compilation_unit) t0;
      print_rest ppf t
  in
  match t with
  | [] -> assert false
  | [ t0 ] -> print0 ~compilation_unit ppf t0
  | t0 :: t ->
    Format.fprintf ppf "Packed units:@ @[<v>(%a)%a@]"
      (print0 ~compilation_unit) t0 print_rest t
