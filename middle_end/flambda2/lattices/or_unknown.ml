(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t =
  | Known of 'a
  | Unknown

let known x = Known x

let print f ppf t =
  let colour = Flambda_colours.top_or_bottom_type in
  match t with
  | Known contents -> Format.fprintf ppf "%a" f contents
  | Unknown ->
    if Flambda_features.unicode ()
    then Format.fprintf ppf "%t@<1>\u{22a4}%t" colour Flambda_colours.pop
    else Format.fprintf ppf "%tT%t" colour Flambda_colours.pop

let compare compare_contents t1 t2 =
  match t1, t2 with
  | Unknown, Unknown -> 0
  | Known contents1, Known contents2 -> compare_contents contents1 contents2
  | Unknown, Known _ -> -1
  | Known _, Unknown -> 1

let equal equal_contents t1 t2 =
  match t1, t2 with
  | Unknown, Unknown -> true
  | Known contents1, Known contents2 -> equal_contents contents1 contents2
  | Unknown, Known _ | Known _, Unknown -> false

let bind t ~f = match t with Known contents -> f contents | Unknown -> Unknown

let map t ~f =
  match t with Known contents -> Known (f contents) | Unknown -> Unknown

let map_sharing t ~f =
  match t with
  | Known contents ->
    let contents' = f contents in
    if contents == contents' then t else Known contents'
  | Unknown -> Unknown

let free_names free_names_contents t =
  match t with
  | Known contents -> free_names_contents contents
  | Unknown -> Name_occurrences.empty

let ids_for_export ids_for_export_contents t =
  match t with
  | Known contents -> ids_for_export_contents contents
  | Unknown -> Ids_for_export.empty

let apply_renaming t renaming rename_contents =
  match t with
  | Known contents -> Known (rename_contents contents renaming)
  | Unknown -> Unknown

module Lift (I : Container_types.S) = struct
  type nonrec t = I.t t

  include Container_types.Make (struct
    type nonrec t = t

    let print ppf t = print I.print ppf t

    let compare t1 t2 = compare I.compare t1 t2

    let equal t1 t2 = equal I.equal t1 t2

    let hash t =
      match t with
      | Unknown -> Hashtbl.hash 0
      | Known i -> Hashtbl.hash (1, I.hash i)
  end)
end

module Let_syntax = struct
  let ( let>* ) x f = bind x ~f

  let ( let>+ ) x f = map x ~f

  let ( let>+$ ) x f = map_sharing x ~f
end
