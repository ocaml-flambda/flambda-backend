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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  | Ok of 'a
  | Bottom

let [@ocamlformat "disable"] print f ppf t =
  match t with
  | Ok contents -> Format.fprintf ppf "@[(Ok %a)@]" f contents
  | Bottom -> Format.pp_print_string ppf "Bottom"

let both t1 t2 ~f =
  match t1, t2 with
  | Ok contents1, Ok contents2 -> Ok (f contents1 contents2)
  | Bottom, _ | _, Bottom -> Bottom

let map t ~f = match t with Ok contents -> Ok (f contents) | Bottom -> Bottom

let value_map t ~bottom ~f =
  match t with Ok contents -> f contents | Bottom -> bottom

let all ts =
  let contents =
    List.filter_map
      (fun t -> match t with Ok contents -> Some contents | Bottom -> None)
      ts
  in
  if List.compare_lengths ts contents <> 0 then Bottom else Ok contents

let bind t ~f = match t with Bottom -> Bottom | Ok contents -> f contents

module Let_syntax = struct
  let ( let<* ) x f = bind x ~f

  let ( let<+ ) x f = map x ~f
end
