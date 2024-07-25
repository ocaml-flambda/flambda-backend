(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type tail_attribute = Lambda.tail_attribute

type t = Lambda.position_and_tail_attribute

let print ppf (t : t) =
  let pattr ppf (tail_attribute : tail_attribute) =
    let str =
      match tail_attribute with
      | Explicit_tail -> "Explicit_tail"
      | Hint_tail -> "Hint_tail"
      | Explicit_non_tail -> "Explicit_non_tail"
      | Default_tail -> "Default_tail"
    in
    Format.pp_print_string ppf str
  in
  match t with
  | Unknown_position -> Format.pp_print_string ppf "Unknown_position"
  | Tail_position attr -> Format.fprintf ppf "Tail_position (%a)" pattr attr
  | Not_tail_position attr ->
    Format.fprintf ppf "Not_tail_position (%a)" pattr attr
