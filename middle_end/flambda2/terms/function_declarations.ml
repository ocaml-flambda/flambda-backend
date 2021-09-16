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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  { funs : Code_id.t Closure_id.Map.t;
    in_order : Code_id.t Closure_id.Lmap.t
  }

let empty = { funs = Closure_id.Map.empty; in_order = Closure_id.Lmap.empty }

let is_empty { funs; _ } = Closure_id.Map.is_empty funs

let create in_order =
  { funs = Closure_id.Map.of_list (Closure_id.Lmap.bindings in_order);
    in_order
  }

let funs t = t.funs

let funs_in_order t = t.in_order

let find ({ funs; _ } : t) closure_id = Closure_id.Map.find closure_id funs

let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf { in_order; _ } =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Closure_id.Lmap.print Code_id.print)
    in_order

let [@ocamlformat "disable"] print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let free_names { funs; _ } =
  Closure_id.Map.fold
    (fun _closure_id code_id syms ->
      Name_occurrences.add_code_id syms code_id Name_mode.normal)
    funs Name_occurrences.empty

(* Note: the call to {create} at the end already takes into account the
   permutation applied to the function_declarations in {in_order}, so there is
   no need to apply_renaming the func_decls in the {funs} field. *)
let apply_renaming ({ in_order; _ } as t) perm =
  let in_order' =
    Closure_id.Lmap.map_sharing
      (fun code_id -> Renaming.apply_code_id perm code_id)
      in_order
  in
  if in_order == in_order' then t else create in_order'

let all_ids_for_export { funs; _ } =
  Closure_id.Map.fold
    (fun _closure_id code_id ids -> Ids_for_export.add_code_id ids code_id)
    funs Ids_for_export.empty

let compare { funs = funs1; _ } { funs = funs2; _ } =
  Closure_id.Map.compare Code_id.compare funs1 funs2

let filter t ~f =
  let funs = Closure_id.Map.filter f t.funs in
  let in_order = Closure_id.Lmap.filter f t.in_order in
  { funs; in_order }

let binds_closure_id t closure_id = Closure_id.Map.mem closure_id t.funs
