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

type t = unit

include Container_types.Make (struct
  type nonrec t = t

  let compare () () = 0

  let equal () () = true

  let hash () = 0

  let [@ocamlformat "disable"] print ppf () = Format.pp_print_string ppf "()"
end)

let free_names _ = Name_occurrences.empty

let apply_renaming () _ = ()

let subset () () = true
