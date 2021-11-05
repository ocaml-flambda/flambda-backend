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

(** Function declaration (not call site) inlining annotations. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Always_inline
  | Available_inline
  | Never_inline
  | Unroll of int
  | Default_inline

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val is_default : t -> bool

val number_of_unrolls : t -> int
