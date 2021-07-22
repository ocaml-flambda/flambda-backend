(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type string_contents =
  | Contents of string
  | Unknown_or_mutable

type t = {
  contents : string_contents;
  size : Targetint_31_63.Imm.t;
}

let create ~contents ~size =
  { contents;
    size;
  }

let contents t = t.contents
let size t = t.size

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    let c =
      match t1.contents, t2.contents with
      | Contents s1, Contents s2 -> String.compare s1 s2
      | Unknown_or_mutable, Unknown_or_mutable -> 0
      | Contents _, Unknown_or_mutable -> -1
      | Unknown_or_mutable, Contents _ -> 1
    in
    if c <> 0 then c
    else Stdlib.compare t1.size t2.size

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash t = Hashtbl.hash t

  let print ppf { contents; size; } =
    match contents with
    | Unknown_or_mutable ->
      Format.fprintf ppf "(size %a)" Targetint_31_63.Imm.print size
    | Contents s ->
      let s, dots =
        let max_size = Targetint_31_63.Imm.ten in
        let long = Targetint_31_63.Imm.compare size max_size > 0 in
        if long then String.sub s 0 8, "..."
        else s, ""
      in
      Format.fprintf ppf "(size %a) (contents \"%S\"%s)"
        Targetint_31_63.Imm.print size
        s dots

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)
