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

type t =
  | Explicit_tail (* [@tail] *)
  | Hint_tail (* [@tail hint] *)
  | Explicit_non_tail (* [@nontail] *)
  | Default_tail (* No [@tail] or [@nontail] attribute *)

let from_lambda (tail : Lambda.tail_attribute) : t =
  match tail with
  | Explicit_tail -> Explicit_tail
  | Hint_tail -> Hint_tail
  | Explicit_non_tail -> Explicit_non_tail
  | Default_tail -> Default_tail

let to_lambda (tail : t) : Lambda.tail_attribute =
  match tail with
  | Explicit_tail -> Explicit_tail
  | Hint_tail -> Hint_tail
  | Explicit_non_tail -> Explicit_non_tail
  | Default_tail -> Default_tail

let print ppf t =
  let str =
    match t with
    | Explicit_tail -> "Explicit_tail"
    | Hint_tail -> "Hint_tail"
    | Explicit_non_tail -> "Explicit_non_tail"
    | Default_tail -> "Default_tail"
  in
  Format.pp_print_string ppf str
