(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
type desc =
  { name : string;
    enabled_at_init : bool
  }

type t = desc option

let from_lambda : Lambda.probe -> t = function
  | None -> None
  | Some { name : string; enabled_at_init : bool } ->
    Some { name; enabled_at_init }

let print ppf = function
  | None -> Format.pp_print_string ppf "()"
  | Some { name; enabled_at_init } ->
    Format.pp_print_string ppf name;
    if enabled_at_init then Format.pp_print_string ppf "enabled_at_init"
