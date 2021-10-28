(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = Code

type t0 =
  | Present of { code : C.t }
  | Imported of { code_metadata : Code_metadata.t }

type t = t0 Code_id.Map.t

let print0 ppf t0 =
  match t0 with
  | Present { code } ->
    Format.fprintf ppf "@[<hov 1>(Present@ (@[<hov 1>(code@ %a)@]))@]" C.print
      code
  | Imported { code_metadata } ->
    Format.fprintf ppf "@[<hov 1>(Imported@ (code_metadata@ %a))@]"
      Code_metadata.print code_metadata

let print ppf t = Code_id.Map.print print0 ppf t

let empty = Code_id.Map.empty

let add_code code t =
  Code_id.Map.filter_map
    (fun _code_id code ->
      match C.params_and_body code with
      | Inlinable _ ->
        let code =
          if Function_decl_inlining_decision_type.cannot_be_inlined
               (C.inlining_decision code)
          then C.make_non_inlinable code
          else code
        in
        Some (Present { code })
      | Non_inlinable -> Some (Present { code })
      | Cannot_be_called -> None)
    code
  |> Code_id.Map.disjoint_union t

let mark_as_imported t =
  let forget_params_and_body t0 =
    match t0 with
    | Imported _ -> t0
    | Present { code } -> Imported { code_metadata = C.code_metadata code }
  in
  Code_id.Map.map forget_params_and_body t

let merge t1 t2 =
  let merge_one code_id t01 t02 =
    match t01, t02 with
    | Imported { code_metadata = cc1 }, Imported { code_metadata = cc2 } ->
      if Code_metadata.equal cc1 cc2
      then Some t01
      else
        Misc.fatal_errorf
          "Code id %a is imported with different code metadata (%a and %a)"
          Code_id.print code_id Code_metadata.print cc1 Code_metadata.print cc2
    | Present _, Present _ ->
      Misc.fatal_errorf "Cannot merge two definitions for code id %a"
        Code_id.print code_id
    | ( Imported { code_metadata = cc_imported },
        (Present { code = code_present } as t0) )
    | ( (Present { code = code_present } as t0),
        Imported { code_metadata = cc_imported } ) ->
      let cc_present = C.code_metadata code_present in
      if Code_metadata.equal cc_present cc_imported
      then Some t0
      else
        Misc.fatal_errorf
          "Code_id %a is present with code metadata@ %abut imported with code \
           metadata@ %a"
          Code_id.print code_id Code_metadata.print cc_present
          Code_metadata.print cc_imported
  in
  Code_id.Map.union merge_one t1 t2

let mem code_id t = Code_id.Map.mem code_id t

let find_code t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    Misc.fatal_errorf "Code ID %a not bound" Code_id.print code_id
  | Present { code } -> Some code
  | Imported _ -> None

let find_code_if_not_imported t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    (* In some cases a code ID is created, the corresponding closure stored into
       another closure, but the corresponding closure variable ends up never
       being used and so the initial code ID and closure are removed, but the
       type of the second closure still mentions its closure variable and its
       contents (eventually pointing to the code ID). Ideally the type should be
       patched to remove the unused closure variable before computing
       reachability, but for now this is done during import instead so we can
       end up with missing code IDs during the reachability computation, and
       have to assume that it fits the above case. *)
    None
  | Present { code } -> Some code
  | Imported _ -> None

let find_code_metadata t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    Misc.fatal_errorf "Code ID %a not bound" Code_id.print code_id
  | Present { code } -> Code.code_metadata code
  | Imported { code_metadata } -> code_metadata

let remove_unreachable t ~reachable_names =
  Code_id.Map.filter
    (fun code_id _exported_code ->
      Name_occurrences.mem_code_id reachable_names code_id)
    t

let all_ids_for_export t =
  Code_id.Map.fold
    (fun code_id code_data all_ids ->
      let all_ids = Ids_for_export.add_code_id all_ids code_id in
      match code_data with
      | Present { code } ->
        Ids_for_export.union all_ids (C.all_ids_for_export code)
      | Imported { code_metadata = _ } -> all_ids)
    t Ids_for_export.empty

let apply_renaming code_id_map renaming t =
  if Renaming.is_empty renaming && Code_id.Map.is_empty code_id_map
  then t
  else
    Code_id.Map.fold
      (fun code_id code_data all_code ->
        let code_id =
          match Code_id.Map.find code_id code_id_map with
          | exception Not_found -> code_id
          | code_id -> code_id
        in
        let code_data =
          match code_data with
          | Present { code } ->
            let code = C.apply_renaming code renaming in
            Present { code }
          | Imported { code_metadata } -> Imported { code_metadata }
        in
        Code_id.Map.add code_id code_data all_code)
      t Code_id.Map.empty

let iter t f =
  Code_id.Map.iter
    (fun id -> function Present { code; _ } -> f id code | _ -> ())
    t
