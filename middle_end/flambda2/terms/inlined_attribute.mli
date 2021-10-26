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

(** Call site (not function declaration) inlining annotations. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Always_inlined
  | Hint_inlined
  | Never_inlined
  | Unroll of int
  | Default_inlined

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val is_default : t -> bool
