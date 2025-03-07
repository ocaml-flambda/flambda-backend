(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Not_rebuilding
  | Rebuilding_partially
  | Rebuilding_everything

let [@ocamlformat "disable"] print ppf = function
  | Not_rebuilding -> Format.fprintf ppf "%b" false
  | Rebuilding_partially -> Format.fprintf ppf "partial"
  | Rebuilding_everything -> Format.fprintf ppf "%b" true

let not_rebuilding_terms = function
  | Not_rebuilding -> true
  | Rebuilding_partially | Rebuilding_everything -> false

let are_rebuilding_partially = function
  | Not_rebuilding | Rebuilding_everything -> false
  | Rebuilding_partially -> true

let rebuild_nothing = Not_rebuilding

let rebuild_everything = Rebuilding_everything

let partial_rebuilding = Rebuilding_partially
