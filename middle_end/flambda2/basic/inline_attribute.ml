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

type t =
  | Always_inline
  | Hint_inline
  | Never_inline
  | Unroll of int
  | Default_inline

let print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Always_inline -> fprintf ppf "Always_inline"
  | Hint_inline -> fprintf ppf "Hint_inline"
  | Never_inline -> fprintf ppf "Never_inline"
  | Unroll n -> fprintf ppf "@[(Unroll %d)@]" n
  | Default_inline -> fprintf ppf "Default_inline"

let equal t1 t2 =
  match t1, t2 with
  | Always_inline, Always_inline
  | Hint_inline, Hint_inline
  | Never_inline, Never_inline
  | Default_inline, Default_inline -> true
  | Unroll n1, Unroll n2 -> n1 = n2
  | _, _ -> false

let is_default t =
  match t with
  | Default_inline -> true
  | _ -> false
