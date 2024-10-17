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
  | Unknown
  | Ok of 'a
  | Bottom

let print f ppf t =
  let colour = Flambda_colours.top_or_bottom_type in
  match t with
  | Unknown ->
    if Flambda_features.unicode ()
    then Format.fprintf ppf "%t@<1>\u{22a4}%t" colour Flambda_colours.pop
    else Format.fprintf ppf "%tT%t" colour Flambda_colours.pop
  | Bottom ->
    if Flambda_features.unicode ()
    then Format.fprintf ppf "%t@<1>\u{22a5}%t" colour Flambda_colours.pop
    else Format.fprintf ppf "%t_|_%t" colour Flambda_colours.pop
  | Ok contents -> Format.fprintf ppf "@[(%a)@]" f contents

let equal eq_contents t1 t2 =
  match t1, t2 with
  | Unknown, Unknown -> true
  | Ok contents1, Ok contents2 -> eq_contents contents1 contents2
  | Bottom, Bottom -> true
  | (Unknown | Ok _ | Bottom), _ -> false

let bind t ~f =
  match t with
  | Unknown -> Unknown
  | Bottom -> Bottom
  | Ok contents -> f contents

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
    if contents == contents' then t else Ok contents'

module Let_syntax = struct
  let ( let<>* ) x f = bind x ~f

  let ( let<>+ ) x f = map x ~f
end
