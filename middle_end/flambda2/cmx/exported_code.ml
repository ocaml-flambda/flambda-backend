(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module C = Code

type raw = Code_or_metadata.raw Code_id.Map.t

type t = Code_or_metadata.t Code_id.Map.t

let print ppf t = Code_id.Map.print Code_or_metadata.print ppf t

let empty = Code_id.Map.empty

let free_names t =
  Code_id.Map.fold
    (fun _code_id code acc ->
      Name_occurrences.union acc (Code_or_metadata.free_names code))
    t Name_occurrences.empty

let add_code ~keep_code code_map t =
  Code_id.Map.mapi
    (fun code_id code ->
      if not (Code_id.equal code_id (Code.code_id code))
      then
        Misc.fatal_errorf "Code ID key in map disagrees with [Code]:@ %a"
          (Code_id.Map.print Code.print)
          code_map;
      let code_or_metadata = Code_or_metadata.create code in
      if (not (keep_code code_id))
         && Function_decl_inlining_decision_type.cannot_be_inlined
              (C.inlining_decision code)
      then Code_or_metadata.remember_only_metadata code_or_metadata
      else code_or_metadata)
    code_map
  |> Code_id.Map.disjoint_union t

let mark_as_imported t =
  Code_id.Map.map_sharing Code_or_metadata.remember_only_metadata t

let merge t1 t2 = Code_id.Map.union Code_or_metadata.merge t1 t2

let mem code_id t = Code_id.Map.mem code_id t

let find_exn t code_id = Code_id.Map.find code_id t

let find t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    (* In some cases a code ID is created, the corresponding closure stored into
       another closure, but the variable bound to the closure ends up never
       being used and so the initial code ID and closure are removed, but the
       type of the second closure still describes the first closure and
       eventually points to its code ID. Ideally the type should be patched to
       remove such references before computing reachability, but for now this is
       done during import instead so we can end up with missing code IDs during
       the reachability computation, and have to assume that it fits the above
       case.

       The other situation where this returns [None] is when we are looking
       during the export reachability computation for a piece of deleted code.
       This can happen because the code ID might have been encountered via a
       "newer version of" field during reachability. *)
    None
  | code_or_metadata -> Some code_or_metadata

let remove_unreachable ~reachable_names t =
  Code_id.Map.filter
    (fun code_id _code_or_metadata ->
      Name_occurrences.mem_code_id reachable_names code_id)
    t

let remove_unused_value_slots_from_result_types_and_shortcut_aliases
    ~used_value_slots ~canonicalise t =
  Code_id.Map.map
    (fun code_or_metadata ->
      Code_or_metadata.map_result_types code_or_metadata ~f:(fun result_ty ->
          Flambda2_types.remove_unused_value_slots_and_shortcut_aliases
            result_ty ~used_value_slots ~canonicalise))
    t

let ids_for_export t =
  Code_id.Map.fold
    (fun code_id code_or_metadata all_ids ->
      Ids_for_export.union
        (Ids_for_export.add_code_id all_ids code_id)
        (Code_or_metadata.ids_for_export code_or_metadata))
    t Ids_for_export.empty

let apply_renaming code_id_map renaming t =
  if Renaming.is_identity renaming && Code_id.Map.is_empty code_id_map
  then t
  else
    Code_id.Map.fold
      (fun code_id code_or_metadata all_code ->
        let code_id =
          match Code_id.Map.find code_id code_id_map with
          | exception Not_found -> code_id
          | code_id -> code_id
        in
        let code_or_metadata =
          Code_or_metadata.apply_renaming code_or_metadata renaming
        in
        Code_id.Map.add code_id code_or_metadata all_code)
      t Code_id.Map.empty

let iter_code t ~f =
  Code_id.Map.iter
    (fun _code_id code_or_metadata ->
      Code_or_metadata.iter_code code_or_metadata ~f)
    t

let from_raw ~sections ~in_current_dir t =
  Code_id.Map.map (Code_or_metadata.from_raw ~sections ~in_current_dir) t

let to_raw ~add_section t =
  Code_id.Map.map (Code_or_metadata.to_raw ~add_section) t

let map_raw_index map_index t =
  Code_id.Map.map (Code_or_metadata.map_raw_index map_index) t
