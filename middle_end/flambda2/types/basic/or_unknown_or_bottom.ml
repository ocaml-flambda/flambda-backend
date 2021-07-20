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
  | Unknown
  | Ok of 'a
  | Bottom

let print f ppf t =
  match t with
  | Unknown -> Format.pp_print_string ppf "Unknown"
  | Ok contents -> Format.fprintf ppf "@[(Ok %a)@]" f contents
  | Bottom -> Format.pp_print_string ppf "Bottom"

let equal eq_contents t1 t2 =
  match t1, t2 with
  | Unknown, Unknown -> true
  | Ok contents1, Ok contents2 -> eq_contents contents1 contents2
  | Bottom, Bottom -> true
  | (Unknown | Ok _ | Bottom), _ -> false

let map t ~f =
  match t with
  | Unknown -> Unknown
  | Bottom -> Bottom
  | Ok contents -> Ok (f contents)

let map_sharing t ~f =
  match t with
  | Unknown | Bottom -> t
  | Ok contents ->
    let contents' = f contents in
    if contents == contents' then t
    else Ok contents'

let of_or_unknown (unk : _ Or_unknown.t) : _ t =
  match unk with
  | Known contents -> Ok contents
  | Unknown -> Unknown
