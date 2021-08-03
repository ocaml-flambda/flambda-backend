(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(* If a value of type [t] maps [id1] to [id2], it means that [id1] is a
   newer version of [id2].  The relation forms a partial order.
   These relations are expected to be small in the majority of cases. *)
type t = Code_id.t Code_id.Map.t

let print ppf t = Code_id.Map.print Code_id.print ppf t

let empty = Code_id.Map.empty

(* CR mshinwell: There should be a well-formedness check during [add], otherwise
   the functions below may not work correctly. *)

let add t ~newer ~older = Code_id.Map.add newer older t

let rec all_ids_up_to_root t ~resolver id =
  match Code_id.Map.find id t with
  | exception Not_found ->
    let comp_unit = Code_id.get_compilation_unit id in
    if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
    then
      Code_id.Set.empty
    else
      begin match resolver comp_unit with
      | exception _ ->
        Misc.fatal_errorf "Exception in resolver@ Backtrace is: %s"
          (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
      | None -> Code_id.Set.empty
      | Some t ->
        (* Inlining the base case, so that we do not recursively loop in case
           of a code_id that is not bound in the map *)
        begin match Code_id.Map.find id t with
        | exception Not_found -> Code_id.Set.empty
        | older -> Code_id.Set.add older (all_ids_up_to_root t ~resolver older)
        end
      end
  | older -> Code_id.Set.add older (all_ids_up_to_root t ~resolver older)

let num_ids_up_to_root t ~resolver id =
  Code_id.Set.cardinal (all_ids_up_to_root t ~resolver id)

(* CR mshinwell: There are no doubt better implementations than the below. *)

(* CR mshinwell: We need a fatal error now if a code ID isn't in [t]. *)

let meet t ~resolver id1 id2 : _ Or_bottom.t =
  (* Whichever of [id1] and [id2] is newer (or the same as the other one),
     in the case where they are comparable; otherwise bottom. *)
  if Code_id.equal id1 id2 then Ok id1
  else
    let id1_to_root = all_ids_up_to_root t ~resolver id1 in
    let id2_to_root = all_ids_up_to_root t ~resolver id2 in
    if Code_id.Set.mem id1 id2_to_root then Ok id2
    else if Code_id.Set.mem id2 id1_to_root then Ok id1
    else Bottom

let join ~target_t ~resolver t1 t2 id1 id2 : _ Or_unknown.t =
  (* Lowest ("newest") common ancestor, if such exists. *)
  if Code_id.equal id1 id2 then Known id1
  else
    let id1_to_root = all_ids_up_to_root ~resolver t1 id1 in
    let id2_to_root = all_ids_up_to_root ~resolver t2 id2 in
    let shared_ids =
      Code_id.Set.inter (Code_id.Map.keys target_t)
        (Code_id.Set.inter id1_to_root id2_to_root)
    in
    if Code_id.Set.is_empty shared_ids then
      Unknown
    else
      let newest_shared_id, _ =
        shared_ids
        |> Code_id.Set.elements
        |> List.map (fun id -> id, num_ids_up_to_root target_t ~resolver id)
        |> List.sort (fun (_, len1) (_, len2) -> - (Int.compare len1 len2))
        |> List.hd
      in
      Known newest_shared_id

type at_most_one_newer =
  | No_newer_version
  | Exactly_one_newer_version of Code_id.t
  | More_than_one_newer_version

let has_at_most_one_newer_version t id =
  let newer_to_id =
    Code_id.Map.filter (fun _newer older -> Code_id.equal older id) t
  in
  if Code_id.Map.is_empty newer_to_id then No_newer_version
  else
    match Code_id.Map.get_singleton newer_to_id with
    | Some (newer, id') ->
      assert (Code_id.equal id id');
      Exactly_one_newer_version newer
    | None -> More_than_one_newer_version

let newer_versions_form_linear_chain t id ~all_code_ids_still_existing =
  if not (Code_id.Set.mem id all_code_ids_still_existing) then true
  else
    match has_at_most_one_newer_version t id with
    | No_newer_version -> true
    | Exactly_one_newer_version _ ->
      (* It doesn't matter if the chain continues linearly then branches
         after a subsequent code ID; any join won't get back as far as
         the current code ID.  It's important to return [true] in this
         case to avoid unsimplified code being used during simplification,
         since it does not contain correct free name information. *)
      true
    | More_than_one_newer_version -> false

let newer_versions_form_linear_chain' t id
      ~all_free_names_still_existing =
  if (not (Name_occurrences.mem_code_id all_free_names_still_existing id))
    && (not (Name_occurrences.mem_newer_version_of_code_id
      all_free_names_still_existing id))
  then true
  else
    match has_at_most_one_newer_version t id with
    | No_newer_version -> true
    | Exactly_one_newer_version _ -> true
    | More_than_one_newer_version -> false

let union t1 t2 =
  Code_id.Map.disjoint_union ~eq:Code_id.equal t1 t2

let all_code_ids_for_export t =
  Code_id.Map.fold (fun key v acc ->
      Code_id.Set.add key (Code_id.Set.add v acc))
    t
    Code_id.Set.empty

let apply_renaming t renaming =
  let rename_code_id = Renaming.apply_code_id renaming in
  Code_id.Map.fold (fun key v acc ->
      Code_id.Map.add (rename_code_id key) (rename_code_id v) acc)
    t
    Code_id.Map.empty
