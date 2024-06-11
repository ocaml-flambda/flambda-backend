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

type code_id_in_function_declaration =
  | Deleted of { function_slot_size : int }
  | Code_id of Code_id.t

type t =
  { funs : code_id_in_function_declaration Function_slot.Map.t;
    in_order : code_id_in_function_declaration Function_slot.Lmap.t
  }

let empty =
  { funs = Function_slot.Map.empty; in_order = Function_slot.Lmap.empty }

let is_empty { funs; _ } = Function_slot.Map.is_empty funs

let create in_order =
  { funs = Function_slot.Map.of_list (Function_slot.Lmap.bindings in_order);
    in_order
  }

let funs t = t.funs

let funs_in_order t = t.in_order

let find ({ funs; _ } : t) function_slot =
  Function_slot.Map.find function_slot funs

let [@ocamlformat "disable"] print ppf { in_order; _ } =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Function_slot.Lmap.print
       (fun ppf -> function Deleted _ -> Format.fprintf ppf "[deleted]" | Code_id code_id -> Format.fprintf ppf "%a" Code_id.print code_id))
    in_order

let free_names { funs; _ } =
  Function_slot.Map.fold
    (fun function_slot code_id syms ->
      let syms =
        Name_occurrences.add_function_slot_in_declaration syms function_slot
          Name_mode.normal
      in
      match code_id with
      | Deleted _ -> syms
      | Code_id code_id ->
        Name_occurrences.add_code_id syms code_id Name_mode.normal)
    funs Name_occurrences.empty

(* Note: the call to {create} at the end already takes into account the
   permutation applied to the function_declarations in {in_order}, so there is
   no need to apply_renaming the func_decls in the {funs} field. *)
let apply_renaming ({ in_order; _ } as t) renaming =
  let in_order' =
    Function_slot.Lmap.map_sharing
      (fun t ->
        match t with
        | Deleted _ -> t
        | Code_id code_id ->
          let code_id' = Renaming.apply_code_id renaming code_id in
          if code_id == code_id' then t else Code_id code_id')
      in_order
  in
  if in_order == in_order' then t else create in_order'

let ids_for_export { funs; _ } =
  Function_slot.Map.fold
    (fun _function_slot code_id ids ->
      match code_id with
      | Deleted _ -> ids
      | Code_id code_id -> Ids_for_export.add_code_id ids code_id)
    funs Ids_for_export.empty

let compare { funs = funs1; _ } { funs = funs2; _ } =
  Function_slot.Map.compare
    (fun code_id1 code_id2 ->
      match code_id1, code_id2 with
      | ( Deleted { function_slot_size = size1 },
          Deleted { function_slot_size = size2 } ) ->
        Int.compare size1 size2
      | Deleted _, Code_id _ -> -1
      | Code_id _, Deleted _ -> 1
      | Code_id code_id1, Code_id code_id2 -> Code_id.compare code_id1 code_id2)
    funs1 funs2

let filter t ~f =
  let funs = Function_slot.Map.filter f t.funs in
  let in_order = Function_slot.Lmap.filter f t.in_order in
  { funs; in_order }

let binds_function_slot t function_slot =
  Function_slot.Map.mem function_slot t.funs
