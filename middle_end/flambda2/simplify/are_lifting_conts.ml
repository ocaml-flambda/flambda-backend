(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Not_lifting
  | Analyzing of
      { continuation : Continuation.t;
        is_exn_handler : bool;
        uses : Continuation_uses.t
      }
  | Lifting_out_of of { continuation : Continuation.t }

let no_lifting : t = Not_lifting

let think_about_lifting_out_of ~is_exn_handler continuation uses =
  Analyzing { continuation; uses; is_exn_handler }

let lift_continuations_out_of continuation : t = Lifting_out_of { continuation }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Not_lifting ->
    Format.fprintf ppf "Not_lifting"
  | Lifting_out_of { continuation; } ->
    Format.fprintf ppf "@[<hov>(lifting_out_of@ \
        @[<hov 1>(continuation@ %a)@]\
      )@]"
      Continuation.print continuation
  | Analyzing { continuation; is_exn_handler; uses; } ->
    Format.fprintf ppf "@[<hov>(analysing@ \
        @[<hov 1>(continuation@ %a)@]@ \
        @[<hov 1>(is_exn_handler@ %b)@]@ \
        @[<hov 1>(uses@ %a)@]\
      )@]"
      Continuation.print continuation
      is_exn_handler
      Continuation_uses.print uses
