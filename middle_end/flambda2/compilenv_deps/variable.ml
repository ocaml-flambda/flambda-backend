(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

include Reg_width_things.Variable

let create_with_same_name_as_ident ?user_visible ident : t =
  create ?user_visible (Ident.name ident)

let rename ?append t =
  let name =
    match append with
    | None -> (name t)
    | Some s -> (name t) ^ s
  in
  let user_visible = if user_visible t then Some () else None in
  create ?user_visible name

let raw_name = name
let raw_name_stamp = name_stamp

let unique_name t =
  (name t) ^ (string_of_int (name_stamp t))

let print_list ppf ts =
  let pp_sep ppf () = Format.fprintf ppf "@ " in
  Format.pp_print_list ~pp_sep print ppf ts

let debug_when_stamp_matches t ~stamp ~f =
  if (name_stamp t) = stamp then f ()

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no var>"
  | Some t -> print ppf t

let compare_lists l1 l2 =
  Misc.Stdlib.List.compare compare l1 l2

module List = struct
  type nonrec t = t list

  let rename ?append t =
    List.map (fun var -> rename ?append var) t
end
